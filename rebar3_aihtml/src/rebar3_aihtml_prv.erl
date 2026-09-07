%%%-------------------------------------------------------------------
%%% @doc The `rebar3 mustache' provider, and the engine-neutral pipeline both
%%% providers share.
%%%
%%% Scan views, decide what is stale, compile it, clean up what is left over.
%%%
%%% Staleness is decided ONLY by the build stamp
%%% ai_mustache_compiler:source_hash/2 produces, read back out of the
%%% -mustache_source attribute of the previously generated .erl. Never by
%%% mtime: a git checkout or a CI cache restore rewrites mtimes wholesale and
%%% would either rebuild everything or, worse, nothing. There is no cache file
%%% -- the generated module is its own manifest. The single place mtime is
%%% consulted is rebar3_aihtml_stale, for reasons documented there.
%%% @end
%%%-------------------------------------------------------------------
-module(rebar3_aihtml_prv).

-behaviour(provider).

-include("rebar3_aihtml.hrl").

-export([init/1, do/1, format_error/1]).
-export([require_core/1, run/3]).

-define(PROVIDER, mustache).

%%%===================================================================
%%% Provider
%%%===================================================================

init(State) ->
    P = providers:create(
          [{name,       ?PROVIDER},
           {module,     ?MODULE},
           {namespace,  default},
           {bare,       true},
           {deps,       [app_discovery, install_deps]},
           {example,    "rebar3 mustache"},
           {opts,       [{force, $f, "force", boolean,
                          "Ignore build stamps and recompile every template"},
                         %% `migrate' is a sub-command rather than a provider
                         %% in the mustache namespace: rebar3 resolves the bare
                         %% default-namespace command `mustache' before it
                         %% considers a namespace of the same name, so
                         %% `rebar3 mustache migrate' would otherwise just run
                         %% the compiler with a stray argument. These two flags
                         %% belong to that sub-command and are declared here so
                         %% that getopt accepts them on the way in.
                         {write, $w, "write", boolean,
                          "migrate sub-command: write the rewritten templates "
                          "back instead of printing a diff"},
                         {verbose, $v, "verbose", boolean,
                          "migrate sub-command: explain every rewrite"}]},
           {short_desc, "Compile mustache templates to Erlang modules"},
           {desc,       "Scan the views directory of every project app and "
                        "compile each .mustache template into an .erl module "
                        "in out_dir. Templates whose recorded build stamp "
                        "still matches are skipped."}]),
    {ok, rebar_state:add_provider(State, P)}.

do(State) ->
    case rebar_state:command_args(State) of
        ["migrate" | _] -> rebar3_aihtml_migrate:do(State);
        _ ->
            {Args, _} = rebar_state:command_parsed_args(State),
            Force = proplists:get_value(force, Args, false),
            run(State, ai_mustache_engine, Force)
    end.

%% @doc The whole pipeline for one engine.
%%
%% Also the entry point for a caller that is not the command line, and the one
%% `rebar3 jinja' uses -- there is exactly one implementation of scan,
%% validate, compile, collect.
run(State, Engine, Force) ->
    try
        ok = require_core(State),
        %% Two phases on purpose. Everything is scanned and validated before
        %% anything is written, so a name collision cannot leave out_dir in a
        %% state that depends on which template happened to be compiled first.
        Collected = collect(State, Engine),
        State1 = validate(Collected, State),
        Totals = lists:foldl(fun(C, Acc) -> emit_app(C, Force, Acc) end,
                             {0, 0, 0}, Collected),
        report(Engine, Totals),
        {ok, State1}
    catch
        throw:{rebar3_aihtml, Reason} ->
            {error, {?MODULE, Reason}}
    end.

%% @doc Make the compile core reachable, or explain why it is not.
%%
%% Exported because rebar3_aihtml_migrate needs the same guarantee.
%% The compile core has to be reachable from the rebar3 VM, not merely from
%% the app being built. Normally the plugin carries its own copy (see
%% rebar3_aihtml:init/1 and the plugin's rebar.config); when it has been
%% repackaged without it, fall back to the aihtml the project already depends
%% on -- it has to be there anyway, since the generated modules call
%% ai_mustache_rt at run time. Failing that, say so in terms a user can act on
%% rather than dying on an undef halfway down the pipeline.
require_core(State) ->
    case code:ensure_loaded(ai_mustache_compiler) of
        {module, _} -> ok;
        {error, _}  -> borrow_core(State)
    end.

borrow_core(State) ->
    Apps = rebar_state:all_deps(State) ++ rebar_state:project_apps(State),
    Declared = [rebar_app_info:ebin_dir(A)
                || A <- Apps, rebar_app_info:name(A) =:= <<"aihtml">>],
    %% all_deps/1 is empty when the provider is invoked bare and nothing has
    %% resolved dependencies yet, so also look where a build would have put
    %% aihtml, whether it arrived as a dependency, a checkout or a plugin.
    Base = rebar_dir:base_dir(State),
    Globbed = lists:append(
                [filelib:wildcard(filename:join([Base, Kind, "aihtml", "ebin"]))
                 || Kind <- ["lib", "checkouts", "plugins"]]),
    lists:foreach(fun(D) ->
                          filelib:is_dir(D) andalso code:add_pathz(D)
                  end, Declared ++ Globbed),
    case code:ensure_loaded(ai_mustache_compiler) of
        {module, _} -> ok;
        {error, R}  -> throw({rebar3_aihtml, {no_core, R}})
    end.

%%%===================================================================
%%% Phase 1: collect
%%%===================================================================

collect(State, Engine) ->
    lists:foldr(
      fun(App, Acc) ->
              case rebar3_aihtml_opts:for_app(App, State, Engine) of
                  skip ->
                      %% Most apps have no templates; saying so on every run
                      %% would be noise.
                      Acc;
                  {ok, Opts} ->
                      [{App, Opts, rebar3_aihtml_scan:templates(Opts)} | Acc]
              end
      end, [], rebar_state:project_apps(State)).

%%%===================================================================
%%% Phase 2: validate
%%%===================================================================

%% Returns the state with this engine's module table folded in, so that the
%% second provider to run can see the first one's output when it checks for
%% collisions. Erlang module names are global; the check has to be too, or
%% two engines could each generate `view_index' -- from the same app, even --
%% and neither would notice.
validate(Collected, State) ->
    Errors = lists:append([app_errors(Opts, Tpls) || {_, Opts, Tpls} <- Collected]),
    State1 = record_modules(Collected, State),
    ok = cross_app_warnings(Collected, State1),
    case Errors of
        [] -> State1;
        _  -> throw({rebar3_aihtml, {errors, Errors}})
    end.

-define(MODULE_TABLE, {aihtml, modules}).

record_modules(Collected, State) ->
    %% Keyed by app AND engine: two engines in one app are two sources, and
    %% keying by app alone would let the second overwrite the first and hide
    %% exactly the clash this table exists to find.
    Mine = [{{Opts#mopts.app_name, tag(Opts)}, Tpls}
            || {_, Opts, Tpls} <- Collected],
    Others = [E || E <- rebar_state:get(State, ?MODULE_TABLE, []),
                   not lists:keymember(element(1, E), 1, Mine)],
    rebar_state:set(State, ?MODULE_TABLE, Others ++ Mine).

tag(#mopts{engine = Engine}) -> list_to_atom(Engine:banner_tag()).

app_errors(Opts, Tpls) ->
    Names = [{T, rebar3_aihtml_name:validate(T#tpl.module, T#tpl.rel_path, Opts)}
             || T <- Tpls],
    [E || {_, {error, E}} <- Names]
        ++ rebar3_aihtml_check:collisions(Tpls, Opts)
        ++ rebar3_aihtml_check:handwritten(Tpls, Opts).

cross_app_warnings(Collected, State) ->
    Wae = lists:any(fun({_, O, _}) -> O#mopts.wae end, Collected),
    PerApp = rebar_state:get(State, ?MODULE_TABLE, []),
    case rebar3_aihtml_check:cross_app(PerApp) of
        [] -> ok;
        Dups ->
            Lines = [io_lib:format("~s is generated by ~p", [M, Apps])
                     || {M, Apps} <- Dups],
            Msg = lists:flatten(
                    ["module name(s) generated more than once; give them "
                     "different prefixes: " |
                     lists:join("; ", Lines)]),
            case Wae of
                true  -> throw({rebar3_aihtml, {bad_opts, Msg}});
                false -> rebar_api:warn("aihtml: ~s", [Msg]), ok
            end
    end.

%%%===================================================================
%%% Phase 3: compile
%%%===================================================================

emit_app({_App, Opts, Tpls}, Force, {C0, S0, K0}) ->
    Available = sets:from_list([N || #tpl{name = N} <- Tpls], [{version, 2}]),
    %% A partial that disappeared leaves its callers with an unchanged stamp,
    %% so they would be skipped and the dangling cross-module call would only
    %% show up at run time. Recompiling them makes the compiler report it with
    %% a template line number instead. This has to happen before the skip
    %% decision, which is why it is not folded into the loop below.
    Forced = rebar3_aihtml_check:stale_partials(Tpls, Opts),
    {Compiled, Skipped, Errors} =
        lists:foldl(fun(T, Acc) ->
                            one(T, Opts, Available, Force, Forced, Acc)
                    end, {0, 0, []}, Tpls),
    case Errors of
        [] -> ok;
        _  -> throw({rebar3_aihtml, {errors, lists:reverse(Errors)}})
    end,
    Cleaned = rebar3_aihtml_gc:sweep(Tpls, Opts),
    {Touched, StaleErrs} = rebar3_aihtml_stale:touch(Opts, []),
    case StaleErrs of
        [] -> ok;
        _  -> throw({rebar3_aihtml, {errors, lists:reverse(StaleErrs)}})
    end,
    Touched > 0 andalso
        rebar_api:debug("aihtml: touched ~b module(s) with a stale template "
                        "attribute", [Touched]),
    {C0 + Compiled, S0 + Skipped, K0 + Cleaned}.

one(T = #tpl{module = Mod, rel_path = Rel, out_path = Out, stamp = Stamp},
    Opts = #mopts{engine = Engine}, Available, Force, Forced, {C, S, Errs}) ->
    case needs_build(Force, sets:is_element(Mod, Forced), Out, Stamp, Engine) of
        false ->
            rebar_api:debug("aihtml: ~ts is up to date", [Rel]),
            {C, S + 1, Errs};
        true ->
            rebar_api:debug("aihtml: compiling ~ts -> ~ts", [Rel, Out]),
            case rebar3_aihtml_emit:render(T, Opts, Available) of
                {error, {File, Line, Reason}} ->
                    {C, S, [{File, Line, Reason} | Errs]};
                {errors, Es} ->
                    {C, S, lists:reverse(Es) ++ Errs};
                {ok, Bin} ->
                    case rebar3_aihtml_emit:write(Out, Bin) of
                        {error, R}  -> throw({rebar3_aihtml, R});
                        %% Byte identical output is not rewritten, so --force
                        %% does not churn mtimes and trigger a pointless
                        %% rebar3 recompile of every generated module.
                        unchanged   -> {C + 1, S, Errs};
                        written     -> {C + 1, S, Errs}
                    end
            end
    end.

needs_build(true, _Forced, _Out, _Stamp, _Engine)  -> true;
needs_build(_Force, true, _Out, _Stamp, _Engine)   -> true;
needs_build(_Force, _Forced, Out, Stamp, Engine)   ->
    %% A generated file that cannot be read back -- truncated, hand edited,
    %% written by an older version -- counts as stale rather than as an error.
    %% Healing beats failing for a file the plugin owns outright.
    rebar3_aihtml_scan:stamp_of(Out, Engine) =/= {ok, Stamp}.

report(_Engine, {0, 0, 0}) ->
    ok;
report(Engine, {C, S, K}) ->
    rebar_api:info("~s: compiled ~b, skipped ~b, cleaned ~b",
                   [Engine:banner_tag(), C, S, K]).

%%%===================================================================
%%% Diagnostics
%%%===================================================================

%% Errors come out in erlc's shape, `path:line: message', so an editor can
%% jump to them. Every error found in a run is reported, not just the first:
%% otherwise fixing N broken templates takes N runs.
format_error({errors, Errors}) ->
    lists:flatten(
      lists:join("\n", ["template compilation failed:" |
                        [diagnostic(E) || E <- Errors]]));
format_error({bad_opts, Msg}) ->
    lists:flatten(io_lib:format("~ts", [Msg]));
format_error({bad_opt, Key, Value}) ->
    lists:flatten(io_lib:format("bad value for option ~p: ~p",
                                [Key, Value]));
format_error({bad_prefix, P}) ->
    lists:flatten(io_lib:format(
                    "prefix ~p is not a legal start of an "
                    "Erlang atom (want [a-z][a-zA-Z0-9_]* or \"\")", [P]));
format_error({ext_not_loadable, M, R}) ->
    lists:flatten(io_lib:format(
                    "extension module ~p could not be loaded "
                    "(~p); it has to be on the code path of the rebar3 VM, "
                    "not only of the target app", [M, R]));
format_error({no_core, R}) ->
    lists:flatten(io_lib:format(
                    "the aihtml compile core is not on the code path (~p). "
                    "The plugin normally carries its own copy; failing that it "
                    "borrows the aihtml your project depends on, which needs "
                    "to have been built at least once -- try `rebar3 compile\' "
                    "first, and check that aihtml is in your deps", [R]));
format_error({write_failed, Path, Posix}) ->
    lists:flatten(io_lib:format("cannot write ~ts: ~p", [Path, Posix]));
format_error(Other) ->
    lists:flatten(io_lib:format("~p", [Other])).

diagnostic({File, Line, Reason}) ->
    lists:flatten(io_lib:format("~ts:~b: ~ts", [File, Line, reason(Reason)])).

reason({unclosed_tag, []}) ->
    "unclosed tag";
reason({unclosed_tag, Keys}) ->
    %% Which of #/^/+/- opened the block is not carried in the reason, so
    %% the message names the key without claiming a marker it does not know.
    io_lib:format("unclosed section tag for ~ts", [keys(Keys)]);
reason({mismatched_close, [], Got}) ->
    io_lib:format("closing tag {{/~ts}} has no matching opening tag", [keys(Got)]);
reason({mismatched_close, Want, Got}) ->
    io_lib:format("mismatched closing tag: expected {{/~ts}}, got {{/~ts}}",
                  [keys(Want), keys(Got)]);
reason({partial_not_found, Name}) ->
    io_lib:format("partial not found: {{> ~ts}}", [Name]);
reason(partial_in_inline_template) ->
    "an inline template cannot use partials: there is no views directory to "
    "resolve them against";
reason({unknown_marker, C}) ->
    io_lib:format("unknown tag marker ~ts; register it from an extension "
                  "module's markers/0 or escape it", [[C]]);
reason({invalid_delimiter, Bin}) ->
    io_lib:format("invalid delimiter tag {{=~ts=}}", [Bin]);
reason({ext_crashed, Mod, C, Term}) ->
    io_lib:format("extension ~p crashed handling marker ~ts: ~p", [Mod, [C], Term]);
reason({unexpected_remote_calls, Mods}) ->
    io_lib:format("generated code would call ~p, which is a compiler bug; "
                  "please report it", [Mods]);
reason({codegen_failed, Term}) ->
    io_lib:format("code generation failed: ~p", [Term]);
reason({partial_cycle_depth, Mods}) ->
    io_lib:format("partial expansion went too deep through ~p", [Mods]);
reason({module_conflict, Mod, Others}) ->
    io_lib:format("module name conflict: ~s is also generated from ~ts",
                  [Mod, lists:join(", ", Others)]);
reason({handwritten_conflict, Mod, File}) ->
    io_lib:format("module name conflict: ~s is already defined by ~ts",
                  [Mod, File]);
reason({illegal_module_name, Mod, Prefix}) ->
    io_lib:format("template name yields ~ts, which is not a bare Erlang atom; "
                  "rename the template (prefix is ~ts). Characters are not "
                  "escaped silently because that would create a second, "
                  "invisible source of module name collisions",
                  [Mod, Prefix]);
reason({read_failed, Posix}) ->
    io_lib:format("cannot read template: ~p", [Posix]);
reason({template_not_found, Path}) ->
    io_lib:format("-mustache_template refers to ~ts, which does not exist",
                  [Path]);
reason(Other) ->
    io_lib:format("~p", [Other]).

keys(Keys) -> lists:join(".", [atom_to_list(K) || K <- Keys]).
