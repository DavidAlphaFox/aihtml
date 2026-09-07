%%%-------------------------------------------------------------------
%%% @doc The `rebar3 mustache migrate' provider.
%%%
%%% Mechanically rewrites v0.3.x flat-context templates into standard mustache
%%% context stack form. Prints a diff by default; --write commits it.
%%%
%%% This is an assistant, not an authority. Anything it cannot decide -- a
%%% sibling section's key, a partial, a lambda, a dotted section key -- is
%%% left exactly as it was and listed for a human, and the run still succeeds.
%%% @end
%%%-------------------------------------------------------------------
-module(rebar3_aihtml_migrate).

-behaviour(provider).

-include("rebar3_aihtml.hrl").

-export([init/1, do/1, format_error/1]).

init(State) ->
    P = providers:create(
          [{name,       migrate},
           {module,     ?MODULE},
           {namespace,  mustache},
           {bare,       true},
           {deps,       [app_discovery, install_deps]},
           {example,    "rebar3 mustache migrate"},
           {opts,       [{write,   $w, "write",   boolean,
                          "Write the rewritten templates back (default: print "
                          "a diff only)"},
                         {verbose, $v, "verbose", boolean,
                          "Explain every rewrite"}]},
           {short_desc, "Rewrite v0.3.x flat-context templates to standard "
                        "mustache semantics"},
           {desc,       "Inside the body of {{#x}}, references written as "
                        "{{x.y}} lose the redundant prefix. Edits are made in "
                        "place in the source text, so comments, whitespace and "
                        "custom delimiters are preserved verbatim."}]),
    {ok, rebar_state:add_provider(State, P)}.

do(State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    Write = proplists:get_value(write, Args, false),
    Verbose = proplists:get_value(verbose, Args, false),
    Write andalso
        rebar_api:warn("mustache migrate: --write rewrites your templates in "
                       "place and keeps no backup; make sure the working tree "
                       "is committed", []),
    try
        ok = rebar3_aihtml_prv:require_core(State),
        Totals = lists:foldl(
                   fun(App, Acc) -> app(App, State, Write, Verbose, Acc) end,
                   {0, 0, 0}, rebar_state:project_apps(State)),
        summary(Totals),
        {ok, State}
    catch
        throw:{rebar3_aihtml, Reason} -> {error, {?MODULE, Reason}}
    end.

format_error(Reason) -> rebar3_aihtml_prv:format_error(Reason).

%%%===================================================================
%%% Per app
%%%===================================================================

app(AppInfo, State, Write, Verbose, Acc) ->
    case rebar3_aihtml_opts:for_app(AppInfo, State) of
        skip       -> Acc;
        {ok, Opts} ->
            %% Exactly the scan the compile provider does, so migrate can never
            %% see a different set of files than the thing that compiles them.
            Tpls = rebar3_aihtml_scan:templates(Opts),
            lists:foldl(fun(T, A) -> one(T, Opts, Write, Verbose, A) end,
                        Acc, Tpls)
    end.

one(#tpl{abs_path = Abs, rel_path = Rel, source = Body, module = Mod},
    #mopts{compiler_opts = CO}, Write, Verbose, {Files, Rewrites, Manual}) ->
    Opts = CO#{module => Mod, source => unicode:characters_to_binary(Rel)},
    case rebar3_aihtml_rewrite:file(Body, Opts) of
        {error, {File, Line, Reason}} ->
            throw({rebar3_aihtml, {errors, [{File, Line, Reason}]}});
        {Body, 0, []} ->
            {Files, Rewrites, Manual};
        {New, N, Todo} ->
            report(Rel, N, Todo, Verbose),
            emit(Abs, Rel, Body, New, Write),
            {Files + case New =:= Body of true -> 0; false -> 1 end,
             Rewrites + N, Manual + length(Todo)}
    end.

report(_Rel, 0, [], _Verbose) ->
    ok;
report(Rel, N, Todo, Verbose) ->
    io:format("~ts~n", [Rel]),
    N > 0 andalso io:format("  rewritten: ~b~n", [N]),
    case Todo of
        [] -> ok;
        _  ->
            io:format("  manual review needed:~n", []),
            [io:format("    line ~b: ~ts~n", [L, T]) || {L, T} <- Todo],
            ok
    end,
    _ = Verbose,
    ok.

emit(_Abs, _Rel, Body, Body, _Write) ->
    ok;
emit(Abs, Rel, Old, New, false) ->
    _ = Abs,
    io:format("~ts", [rebar3_aihtml_diff:unified(Rel, Old, New)]),
    ok;
emit(Abs, _Rel, _Old, New, true) ->
    %% Same atomic write as the compile path: a sibling .tmp then a rename, so
    %% an interrupted run never leaves a half written template behind.
    case rebar3_aihtml_emit:write(Abs, New) of
        {error, R} -> throw({rebar3_aihtml, R});
        _          -> ok
    end.

summary({0, 0, 0}) ->
    rebar_api:info("mustache migrate: nothing to change", []);
summary({Files, Rewrites, Manual}) ->
    io:format("~n~b file(s), ~b rewrite(s), ~b item(s) need manual review~n",
              [Files, Rewrites, Manual]),
    %% A non-empty manual list is not a failure: migrate is a helper and
    %% failing the build over work it deliberately declined to do would be
    %% wrong. It is loud instead.
    Manual > 0 andalso
        rebar_api:warn("mustache migrate: ~b item(s) were left alone and need "
                       "a human; see the report above", [Manual]),
    ok.
