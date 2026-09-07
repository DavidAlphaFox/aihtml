%%%-------------------------------------------------------------------
%%% @doc The extension behaviour: user-defined mustache markers.
%%%
%%% An extension module claims one or more marker characters and, at compile
%%% time, turns every tag introduced by one of them into an Erlang abstract
%%% expression. `{{@ hello}}' with `$@' claimed by `my_i18n' becomes whatever
%%% `my_i18n:compile_tag($@, [hello], [], Opts)' returns, spliced straight into
%%% the iolist the template compiles to.
%%%
%%% == Declaring versus assembling ==
%%%
%%% These are two different mechanisms and only one of them switches the
%%% extension on:
%%%
%%% <dl>
%%%   <dt>`-mustache_tag(my_i18n).' + the parse_transform</dt>
%%%   <dd><b>Declaration and validation only.</b> A parse_transform sees one
%%%       module at a time, so a declaration in `view_index.erl' says nothing
%%%       about how `views/index.mustache' is compiled by the rebar3 plugin.
%%%       Its value is that a typo, a missing callback or a reserved marker
%%%       becomes a compile error instead of a puzzling runtime failure. The
%%%       one exception: forms (b) and (c) compile inside the very module that
%%%       carries the attribute, so a `-mustache_tag' there <em>is</em> used to
%%%       assemble the extension for those two.</dd>
%%%   <dt>`{mustache_opts, [{extensions, [my_i18n]}]}'</dt>
%%%   <dd><b>Assembly.</b> This is what tells the compiler to enable the
%%%       extension while compiling `.mustache' files.</dd>
%%% </dl>
%%%
%%% == The Opts contract ==
%%%
%%% `compile_tag/4' receives the compiler's options map. The fields an
%%% extension may rely on:
%%%
%%% <table>
%%%   <tr><td>`module'</td><td>the module being generated</td></tr>
%%%   <tr><td>`source'</td><td>template path, for the extension's own
%%%                            diagnostics</td></tr>
%%%   <tr><td>`stack_var'</td><td>name of the context-stack variable in the
%%%                            generated code (default `S')</td></tr>
%%%   <tr><td>`indent_var'</td><td>name of the indent variable (default
%%%                            `I')</td></tr>
%%%   <tr><td>`loc'</td><td>`{Line, Col}' of the tag, when the caller
%%%                            supplies it</td></tr>
%%%   <tr><td>`ext_opts'</td><td>`#{Module => term()}' of user
%%%                            configuration</td></tr>
%%% </table>
%%%
%%% Do not hard-code `{var, 0, 'S'}'. Use {@link stack_expr/1} and the other
%%% builders below: the stack variable is renamed when a template is expanded
%%% inline, and a hard-coded name then refers to something that is not bound.
%%%
%%% == Escaping ==
%%%
%%% <b>The value an extension returns is spliced into the iolist verbatim. It
%%% is NOT escaped.</b> This matches `{{*f}}' lambdas, and it means the
%%% extension author owns the XSS question for every byte the extension emits.
%%% Wrap untrusted data in {@link escape_expr/2}.
%%%
%%% == Runtime fallback ==
%%%
%%% Extension tags cannot work on the interpreted fallback path of
%%% `ai_mustache:inline/2'. `compile_tag/4' produces an abstract expression,
%%% which only means something at compile time. An inline template that uses a
%%% custom marker therefore requires
%%% `-compile({parse_transform, ai_mustache_transform})'; without it the call
%%% fails with an `{unknown_marker, Char}' error at runtime.
%%%
%%% See designs/06-parse-transform.md and tasks/T21.md.
%%% @end
%%%-------------------------------------------------------------------
-module(ai_mustache_ext).

-include("ai_mustache.hrl").

%%%===================================================================
%%% Behaviour
%%%===================================================================

%% Which marker characters this module claims.
-callback markers() -> [char()].

%% Compile one extension tag into an Erlang abstract expression.
%%   Marker : the character that introduced the tag, e.g. $@
%%   Keys   : the dotted path inside the tag, e.g. [greeting, title]
%%   Body   : compiled body expressions for a block marker, [] for an inline one
%%   Opts   : the compiler options map, see the contract above
%% The expression must evaluate to iodata(). It is not escaped.
-callback compile_tag(Marker :: char(), Keys :: [atom()],
                      Body :: [erl_parse:abstract_expr()],
                      Opts :: map()) -> erl_parse:abstract_expr().

%% Optional: markers that open a block and need a {{/x}} to close. Every
%% character listed here must also appear in markers/0.
-callback block_markers() -> [char()].

-optional_callbacks([block_markers/0]).

%%%===================================================================
%%% API
%%%===================================================================

-export([builtin_markers/0, is_reserved/1, is_valid_marker/1]).
-export([describe/1, describe/2, validate/1, validate/2, is_block/2]).
-export([registry/1, registry/2, spec_module/1, spec_markers/1]).
-export([format_error/1]).

%% Builders for extension authors.
-export([anno/1, stack_var/1, indent_var/1, stack_expr/1, indent_expr/1]).
-export([lookup_expr/2, escape_expr/2, to_binary_expr/2, ext_opt/3]).

-type spec() :: module()
              | {char(), module()}
              | {char(), {module(), atom()}}.

-type info() :: #{markers := [char()], block_markers := [char()]}.

-type reason() ::
        {ext_module_not_found, module()}
      | {not_an_ext_module, module()}
      | {ext_missing_callback, module(), {atom(), arity()}}
      | {ext_no_markers, module()}
      | {invalid_marker, char(), module()}
      | {marker_reserved, char(), module()}
      | {marker_conflict, char(), [module()]}
      | {marker_not_declared, char(), module(), [char()]}
      | {block_marker_not_declared, char(), module()}.

-type vopts() :: #{search_dirs => [file:filename_all()]}.

-export_type([spec/0, info/0, reason/0, vopts/0]).

%%%===================================================================
%%% Reserved characters
%%%===================================================================

%% @doc The markers aihtml claims for itself: `# ^ / > ! = & { } + - *'.
%%
%% The set is defined once, in include/ai_mustache.hrl. Everything that needs
%% it -- the plugin, the parse_transform, this module -- reads it from here so
%% that adding a builtin tag cannot leave a second copy behind.
-spec builtin_markers() -> [char()].
builtin_markers() -> ?AI_MUSTACHE_BUILTIN_MARKERS.

-spec is_reserved(char()) -> boolean().
is_reserved(C) -> lists:member(C, ?AI_MUSTACHE_BUILTIN_MARKERS).

%% @doc Can this character be used as a marker at all?
%%
%% The scanner splits a tag by looking at its first character, so a marker has
%% to be printable ASCII punctuation. Whitespace, letters, digits and the brace
%% characters are all out: whitespace and letters would swallow ordinary
%% `{{name}}' interpolations, and braces collide with the delimiters.
-spec is_valid_marker(term()) -> boolean().
is_valid_marker(C) when is_integer(C), C >= 33, C =< 126 ->
    not lists:member(C, [${, $}, $_]) andalso
        not (C >= $0 andalso C =< $9) andalso
        not (C >= $a andalso C =< $z) andalso
        not (C >= $A andalso C =< $Z);
is_valid_marker(_) ->
    false.

%%%===================================================================
%%% Validation
%%%===================================================================

%% @doc Validate one extension module and report the markers it claims.
-spec describe(module()) -> {ok, info()} | {error, reason()}.
describe(Mod) -> describe(Mod, #{}).

%% @doc As describe/1, but able to fall back to reading the module's source.
%%
%% `search_dirs' lists directories to look for `Mod.erl' in when the module has
%% no beam yet. That happens routinely: an extension and the module that
%% declares it can sit in the same application, and rebar3 does not promise to
%% compile them in any particular order.
-spec describe(module(), vopts()) -> {ok, info()} | {error, reason()}.
describe(Mod, VOpts) when is_atom(Mod) ->
    case raw_info(Mod, VOpts) of
        {error, _} = E -> E;
        {ok, Raw}      -> check(Mod, Raw)
    end.

-spec validate(module()) -> ok | {error, reason()}.
validate(Mod) -> validate(Mod, #{}).

-spec validate(module(), vopts()) -> ok | {error, reason()}.
validate(Mod, VOpts) ->
    case describe(Mod, VOpts) of
        {ok, _}        -> ok;
        {error, _} = E -> E
    end.

%% @doc Is Marker a block marker of Mod? Modules without block_markers/0 have
%% none, so every marker they claim is inline.
-spec is_block(module(), char()) -> boolean().
is_block(Mod, Marker) ->
    _ = code:ensure_loaded(Mod),
    case erlang:function_exported(Mod, block_markers, 0) of
        true  -> lists:member(Marker, Mod:block_markers());
        false -> false
    end.

%% Gather behaviours, exports and the two marker lists, from the loaded module
%% if possible and from its source otherwise.
-spec raw_info(module(), vopts()) -> {ok, map()} | {error, reason()}.
raw_info(Mod, VOpts) ->
    case code:ensure_loaded(Mod) of
        {module, Mod} -> {ok, loaded_info(Mod)};
        _             -> source_info(Mod, VOpts)
    end.

-spec loaded_info(module()) -> map().
loaded_info(Mod) ->
    Attrs = Mod:module_info(attributes),
    Behaviours = lists:append(proplists:get_all_values(behaviour, Attrs)
                              ++ proplists:get_all_values(behavior, Attrs)),
    Exports = Mod:module_info(exports),
    #{behaviours   => Behaviours,
      exports      => Exports,
      markers      => call_if_exported(Mod, markers, Exports),
      block_markers => call_if_exported(Mod, block_markers, Exports)}.

-spec call_if_exported(module(), atom(), [{atom(), arity()}]) -> term().
call_if_exported(Mod, Fun, Exports) ->
    case lists:member({Fun, 0}, Exports) of
        false -> undefined;
        true  -> try Mod:Fun() catch _:_ -> undefined end
    end.

%% Read Mod.erl and answer the same questions statically. markers/0 in a real
%% extension is a constant clause, so its body is readable with
%% erl_parse:normalise/1; anything more elaborate reads back as `undefined' and
%% simply skips the marker checks rather than failing the build.
-spec source_info(module(), vopts()) -> {ok, map()} | {error, reason()}.
source_info(Mod, VOpts) ->
    Dirs = maps:get(search_dirs, VOpts, []),
    case find_source(Mod, Dirs) of
        error     -> {error, {ext_module_not_found, Mod}};
        {ok, File} ->
            case epp_dodger:quick_parse_file(File) of
                {ok, Forms} -> {ok, scan_forms(Forms)};
                _           -> {error, {ext_module_not_found, Mod}}
            end
    end.

-spec find_source(module(), [file:filename_all()]) ->
          {ok, file:filename_all()} | error.
find_source(Mod, Dirs) ->
    Base = atom_to_list(Mod) ++ ".erl",
    All = Dirs ++ code_path_src_dirs(),
    case [F || D <- All, F <- [filename:join(D, Base)], filelib:is_regular(F)] of
        [F | _] -> {ok, F};
        []      -> error
    end.

%% ebin directories on the code path have a sibling src/ in the usual OTP
%% layout, which is where an as-yet-uncompiled extension lives.
-spec code_path_src_dirs() -> [file:filename_all()].
code_path_src_dirs() ->
    [filename:join(filename:dirname(D), "src")
     || D <- code:get_path(), filename:basename(D) =:= "ebin"].

-spec scan_forms([term()]) -> map().
scan_forms(Forms) ->
    lists:foldl(fun scan_form/2,
                #{behaviours => [], exports => [],
                  markers => undefined, block_markers => undefined},
                Forms).

-spec scan_form(term(), map()) -> map().
scan_form({attribute, _, B, V}, Acc) when B =:= behaviour; B =:= behavior ->
    Acc#{behaviours := maps:get(behaviours, Acc) ++ lists:flatten([V])};
scan_form({attribute, _, export, L}, Acc) when is_list(L) ->
    Acc#{exports := maps:get(exports, Acc) ++ L};
scan_form({function, _, Name, 0, [{clause, _, [], [], [Body]}]}, Acc)
  when Name =:= markers; Name =:= block_markers ->
    Value = try erl_parse:normalise(Body) catch _:_ -> undefined end,
    Acc#{Name := Value};
scan_form(_, Acc) ->
    Acc.

-spec check(module(), map()) -> {ok, info()} | {error, reason()}.
check(Mod, Raw) ->
    #{behaviours := Bs, exports := Es} = Raw,
    case lists:member(?MODULE, Bs) of
        false -> {error, {not_an_ext_module, Mod}};
        true  -> check_exports(Mod, Es, Raw)
    end.

-spec check_exports(module(), [{atom(), arity()}], map()) ->
          {ok, info()} | {error, reason()}.
check_exports(Mod, Exports, Raw) ->
    Missing = [MFA || {F, A} = MFA <- [{markers, 0}, {compile_tag, 4}],
                      not lists:member({F, A}, Exports)],
    case Missing of
        [MFA | _] -> {error, {ext_missing_callback, Mod, MFA}};
        []        -> check_markers(Mod, Raw)
    end.

-spec check_markers(module(), map()) -> {ok, info()} | {error, reason()}.
check_markers(Mod, Raw) ->
    case maps:get(markers, Raw) of
        undefined ->
            %% Source-only validation of a non-constant markers/0. The module
            %% is well formed; nothing more can be said without running it.
            {ok, #{markers => [], block_markers => []}};
        Markers when is_list(Markers), Markers =/= [] ->
            case bad_marker(Mod, Markers) of
                {error, _} = E -> E;
                ok             -> check_blocks(Mod, Markers, Raw)
            end;
        _ ->
            {error, {ext_no_markers, Mod}}
    end.

-spec bad_marker(module(), [char()]) -> ok | {error, reason()}.
bad_marker(_Mod, []) ->
    ok;
bad_marker(Mod, [C | Rest]) ->
    case {is_valid_marker(C), is_reserved(C)} of
        {false, _}   -> {error, {invalid_marker, C, Mod}};
        {true, true} -> {error, {marker_reserved, C, Mod}};
        _            -> bad_marker(Mod, Rest)
    end.

-spec check_blocks(module(), [char()], map()) -> {ok, info()} | {error, reason()}.
check_blocks(Mod, Markers, Raw) ->
    Blocks = case maps:get(block_markers, Raw) of
                 undefined       -> [];
                 L when is_list(L) -> L;
                 _               -> bad
             end,
    case Blocks of
        bad -> {error, {block_marker_not_declared, 0, Mod}};
        _   ->
            case [C || C <- Blocks, not lists:member(C, Markers)] of
                [C | _] -> {error, {block_marker_not_declared, C, Mod}};
                []      -> {ok, #{markers => Markers, block_markers => Blocks}}
            end
    end.

%%%===================================================================
%%% Registry
%%%===================================================================

%% @doc Build the marker dispatch table for a list of extension specs.
%%
%% A spec is a bare module (claim everything its markers/0 returns), or
%% `{Marker, Module}' / `{Marker, {Module, Fun}}' to claim a single character.
%% Every module is validated and every marker checked for collisions, so this
%% is the single place that decides whether a set of extensions is usable.
%%
%% The input is sorted first and conflicts are reported with their module lists
%% sorted, so the same configuration always yields the same message.
-spec registry([spec()]) -> {ok, #{char() => module()}} | {error, reason()}.
registry(Specs) -> registry(Specs, #{}).

-spec registry([spec()], vopts()) ->
          {ok, #{char() => module()}} | {error, reason()}.
registry(Specs, VOpts) ->
    case claims(lists:usort(Specs), VOpts, []) of
        {error, _} = E -> E;
        {ok, Claims}   -> resolve_claims(Claims)
    end.

%% One {Marker, Module} pair per character each spec claims.
-spec claims([spec()], vopts(), [{char(), module()}]) ->
          {ok, [{char(), module()}]} | {error, reason()}.
claims([], _VOpts, Acc) ->
    {ok, lists:reverse(Acc)};
claims([Spec | Rest], VOpts, Acc) ->
    Mod = spec_module(Spec),
    case describe(Mod, VOpts) of
        {error, _} = E -> E;
        {ok, #{markers := Declared}} ->
            case spec_markers(Spec) of
                all ->
                    claims(Rest, VOpts, [{C, Mod} || C <- Declared] ++ Acc);
                [C] ->
                    case lists:member(C, Declared) orelse Declared =:= [] of
                        false -> {error, {marker_not_declared, C, Mod, Declared}};
                        true  -> claims(Rest, VOpts, [{C, Mod} | Acc])
                    end
            end
    end.

-spec resolve_claims([{char(), module()}]) ->
          {ok, #{char() => module()}} | {error, reason()}.
resolve_claims(Claims) ->
    Markers = lists:usort([C || {C, _} <- Claims]),
    Conflicts = [{C, Mods} || C <- Markers,
                              Mods <- [lists:usort([M || {C1, M} <- Claims,
                                                         C1 =:= C])],
                              length(Mods) > 1],
    case Conflicts of
        [{C, Mods} | _] -> {error, {marker_conflict, C, Mods}};
        []              -> {ok, maps:from_list(Claims)}
    end.

%% @doc The module named by an extension spec.
-spec spec_module(spec()) -> module().
spec_module(Mod) when is_atom(Mod)             -> Mod;
spec_module({_C, Mod}) when is_atom(Mod)       -> Mod;
spec_module({_C, {Mod, _F}}) when is_atom(Mod) -> Mod.

%% @doc `all' if the spec claims every marker of its module, otherwise the one
%% character it names.
-spec spec_markers(spec()) -> all | [char()].
spec_markers(Mod) when is_atom(Mod) -> all;
spec_markers({C, _})                -> [C].

%%%===================================================================
%%% Diagnostics
%%%===================================================================

%% @doc Human-readable text for every reason this module produces.
%%
%% The parse_transform delegates to this so that an error reported while
%% compiling a module and the same error reported by the plugin read
%% identically.
-spec format_error(reason() | term()) -> string().
format_error({ext_module_not_found, Mod}) ->
    io_lib:format(
      "extension module ~w could not be loaded and no ~s.erl was found on the "
      "search path. If it lives in the same application as this module, note "
      "that the compile order is not fixed: move it to its own application or "
      "list it in {erl_first_files, [...]} so it is compiled first.",
      [Mod, Mod]);
format_error({not_an_ext_module, Mod}) ->
    io_lib:format(
      "~w is not a mustache extension: it does not declare "
      "-behaviour(ai_mustache_ext).", [Mod]);
format_error({ext_missing_callback, Mod, {F, A}}) ->
    io_lib:format(
      "extension module ~w does not export the required callback ~w/~w.",
      [Mod, F, A]);
format_error({ext_no_markers, Mod}) ->
    io_lib:format(
      "~w:markers/0 must return a non-empty list of characters; an extension "
      "that claims no marker can never be reached.", [Mod]);
format_error({invalid_marker, C, Mod}) ->
    io_lib:format(
      "~ts is not usable as a mustache marker (claimed by ~w). A marker must "
      "be printable ASCII punctuation other than { and }; whitespace, letters "
      "and digits would swallow ordinary {{name}} tags.",
      [quote_char(C), Mod]);
format_error({marker_reserved, C, Mod}) ->
    io_lib:format(
      "marker ~ts (claimed by ~w) is reserved by aihtml. The builtin markers "
      "are ~ts; pick a character outside that set.",
      [quote_char(C), Mod, marker_list(builtin_markers())]);
format_error({marker_conflict, C, Mods}) ->
    io_lib:format(
      "marker ~ts is claimed by more than one extension: ~ts. Remove it from "
      "all but one of their markers/0.",
      [quote_char(C), string:join([lists:flatten(io_lib:format("~w", [M]))
                                   || M <- Mods], ", ")]);
format_error({marker_not_declared, C, Mod, Declared}) ->
    io_lib:format(
      "marker ~ts is not claimed by ~w; its markers/0 returns ~ts.",
      [quote_char(C), Mod, marker_list(Declared)]);
format_error({block_marker_not_declared, C, Mod}) ->
    io_lib:format(
      "~w:block_markers/0 lists ~ts, which its markers/0 does not return. "
      "Every block marker must also be a marker.",
      [Mod, quote_char(C)]);
format_error(Other) ->
    io_lib:format("~p", [Other]).

-spec quote_char(term()) -> string().
quote_char(C) when is_integer(C), C >= 32, C =< 126 -> [$', C, $'];
quote_char(C) -> lists:flatten(io_lib:format("~p", [C])).

-spec marker_list([char()]) -> string().
marker_list([]) -> "[]";
marker_list(Cs) -> string:join([quote_char(C) || C <- Cs], " ").

%%%===================================================================
%%% Builders for extension authors
%%%===================================================================

%% @doc The annotation to hang generated nodes off, taken from `loc'.
-spec anno(map()) -> erl_anno:anno().
anno(Opts) ->
    case maps:get(loc, Opts, undefined) of
        {L, _C} when is_integer(L) -> erl_anno:new(L);
        L when is_integer(L)       -> erl_anno:new(L);
        _                          -> erl_anno:new(0)
    end.

%% @doc Name of the context-stack variable in the generated code.
-spec stack_var(map()) -> atom().
stack_var(Opts) -> maps:get(stack_var, Opts, 'S').

%% @doc Name of the indent variable in the generated code.
-spec indent_var(map()) -> atom().
indent_var(Opts) -> maps:get(indent_var, Opts, 'I').

%% @doc `{var, Anno, StackVar}'. Use this instead of writing `{var, 0, 'S'}'.
-spec stack_expr(map()) -> erl_parse:abstract_expr().
stack_expr(Opts) -> {var, anno(Opts), stack_var(Opts)}.

-spec indent_expr(map()) -> erl_parse:abstract_expr().
indent_expr(Opts) -> {var, anno(Opts), indent_var(Opts)}.

%% @doc `ai_mustache_rt:lookup(Keys, S)' -- the value a key path resolves to.
-spec lookup_expr([atom()], map()) -> erl_parse:abstract_expr().
lookup_expr(Keys, Opts) ->
    L = anno(Opts),
    rt_call(L, lookup, [key_list(L, Keys), stack_expr(Opts)]).

%% @doc `ai_mustache_rt:escape(Expr)' -- HTML-escape a value.
-spec escape_expr(erl_parse:abstract_expr(), map()) -> erl_parse:abstract_expr().
escape_expr(Expr, Opts) -> rt_call(anno(Opts), escape, [Expr]).

%% @doc `ai_mustache_rt:to_binary(Expr)' -- render a value without escaping.
-spec to_binary_expr(erl_parse:abstract_expr(), map()) ->
          erl_parse:abstract_expr().
to_binary_expr(Expr, Opts) -> rt_call(anno(Opts), to_binary, [Expr]).

%% @doc This extension's own configuration, from `{ext_opts, #{Mod => Term}}'.
-spec ext_opt(module(), map(), term()) -> term().
ext_opt(Mod, Opts, Default) ->
    maps:get(Mod, maps:get(ext_opts, Opts, #{}), Default).

-spec rt_call(erl_anno:anno(), atom(), [erl_parse:abstract_expr()]) ->
          erl_parse:abstract_expr().
rt_call(L, F, Args) ->
    {call, L, {remote, L, {atom, L, ai_mustache_rt}, {atom, L, F}}, Args}.

-spec key_list(erl_anno:anno(), [atom()]) -> erl_parse:abstract_expr().
key_list(L, [])      -> {nil, L};
key_list(L, [K | T]) -> {cons, L, {atom, L, K}, key_list(L, T)}.
