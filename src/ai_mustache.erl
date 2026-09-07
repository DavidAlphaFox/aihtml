%%%-------------------------------------------------------------------
%%% @doc Public entry points.
%%%
%%% Templates normally reach here already compiled into modules by the rebar3
%%% plugin, in which case render/2 is a single call into the generated module:
%%% no lookup table, no process, no ets. render_string/2,3 compiles a template
%%% on the fly and exists for tests and for the runtime fallback of
%%% inline/2 when the parse_transform has not been applied.
%%% @end
%%%-------------------------------------------------------------------
-module(ai_mustache).

-include("ai_mustache.hrl").

-export([render/2, render_iolist/2]).
-export([render_string/2, render_string/3]).
-export([inline/2]).

-define(INLINE_PREFIX, "aimi_").

%%%===================================================================
%%% Compiled templates
%%%===================================================================

%% @doc Render a template that the build has already compiled into a module.
%%
%% This is a direct call into the generated module. The module name is
%% supplied by the caller, never derived from user input, so there is no
%% atom-table exposure here (contrast the old loader, which ran
%% binary_to_atom/2 on whatever template name it was handed).
-spec render(module(), term()) -> binary().
render(Mod, Ctx) -> Mod:render(Ctx).

%% @doc As render/2 but without the final iolist_to_binary/1.
%%
%% Hand this straight to cowboy as a response body and the copy is skipped
%% entirely.
-spec render_iolist(module(), term()) -> iolist().
render_iolist(Mod, Ctx) -> Mod:render_iolist(Ctx).

%%%===================================================================
%%% Ad-hoc templates
%%%===================================================================

-spec render_string(binary(), term()) -> binary().
render_string(Template, Ctx) -> render_string(Template, Ctx, #{}).

%% @doc Compile and render a template given as a string.
%%
%% `partials' maps a partial name to its template text, so no views directory
%% is involved. Compiled modules are named after a digest of the whole
%% template set and are reused on later calls, which is what keeps the spec
%% suite from recompiling the same template 136 times.
-spec render_string(binary(), term(), map()) -> binary().
render_string(Template, Ctx, Opts) ->
    case compile_set(Template, Opts) of
        {ok, Mod}      -> Mod:render(Ctx);
        {error, _} = E -> erlang:error({ai_mustache, E})
    end.

%% @doc Render an inline template.
%%
%% With the ai_mustache_transform parse_transform applied and a literal first
%% argument this call is replaced at compile time by the expanded code and
%% never runs. Without it, the same template is compiled here and cached, so
%% behaviour is identical and only the cost differs.
%%
%% "Identical" includes the failures. An inline template has no views
%% directory, so {{> x}} is rejected here just as the transform rejects it at
%% compile time -- otherwise a partial would be a build error with the
%% transform and silently render as empty without it, and the two paths would
%% disagree exactly where it is hardest to notice.
-spec inline(binary(), term()) -> binary().
inline(Template, Ctx) ->
    case ai_mustache_parser:parse(Template, #{source => <<"inline">>}) of
        {error, _} = E ->
            erlang:error({ai_mustache, E});
        {ok, Ast} ->
            case has_partial(Ast) of
                true ->
                    erlang:error({ai_mustache,
                                  {error, {<<"inline">>, 1,
                                           partial_in_inline_template}}});
                false ->
                    render_string(Template, Ctx, #{})
            end
    end.

-spec has_partial([ai_mustache_node()]) -> boolean().
has_partial([])                            -> false;
has_partial([{partial, _, _, _} | _])      -> true;
has_partial([{section, _, _, B} | T])      -> has_partial(B) orelse has_partial(T);
has_partial([{inverted, _, _, B} | T])     -> has_partial(B) orelse has_partial(T);
has_partial([{has, _, _, B, _} | T])       -> has_partial(B) orelse has_partial(T);
has_partial([{ext, _, _, _, B} | T])       -> has_partial(B) orelse has_partial(T);
has_partial([_ | T])                       -> has_partial(T).

%%%===================================================================
%%% On-the-fly compilation
%%%===================================================================

-spec compile_set(binary(), map()) -> {ok, module()} | ai_mustache_error().
compile_set(Template, Opts) ->
    Partials = maps:get(partials, Opts, #{}),
    Prefix = prefix_for(Template, Partials, Opts),
    Main = binary_to_atom(<<Prefix/binary, "main">>, utf8),
    case is_loaded(Main) of
        true  -> {ok, Main};
        false -> compile_all(Template, Partials, Prefix, Main, Opts)
    end.

%% One prefix per template set. Every module in the set shares it, so a
%% partial reference inside the main template resolves to the module compiled
%% from that set's own partial rather than to some other test's.
-spec prefix_for(binary(), map(), map()) -> binary().
prefix_for(Template, Partials, Opts) ->
    Digest = erlang:md5(term_to_binary({Template, lists:sort(maps:to_list(Partials)),
                                        maps:get(extensions, Opts, []),
                                        ?AI_MUSTACHE_VSN})),
    Hex = binary:encode_hex(Digest, lowercase),
    <<?INLINE_PREFIX, Hex/binary, "_">>.

-spec compile_all(binary(), map(), binary(), module(), map()) ->
          {ok, module()} | ai_mustache_error().
compile_all(Template, Partials, Prefix, Main, Opts) ->
    Base = maps:merge(Opts, #{prefix => Prefix}),
    Units = [{Main, <<"main">>, Template}
             | [{ai_mustache_ast:module_name(N, Base), N, T}
                || {N, T} <- maps:to_list(Partials)]],
    case parse_units(Units, Base, []) of
        {error, _} = E -> E;
        {ok, Parsed} ->
            ok = stub_missing(Parsed, Base),
            case build(Parsed, Base) of
                {error, _} = E -> E;
                ok             -> {ok, Main}
            end
    end.

-spec parse_units([{module(), binary(), binary()}], map(),
                  [{module(), binary(), [ai_mustache_node()]}]) ->
          {ok, [{module(), binary(), [ai_mustache_node()]}]} | ai_mustache_error().
parse_units([], _Base, Acc) ->
    {ok, lists:reverse(Acc)};
parse_units([{Mod, Name, Text} | Rest], Base, Acc) ->
    Opts = Base#{module => Mod, source => <<Name/binary, ".mustache">>},
    case ai_mustache_parser:parse(Text, Opts) of
        {error, _} = E -> E;
        {ok, Ast}      -> parse_units(Rest, Base, [{Mod, Name, Ast} | Acc])
    end.

%% The spec requires a partial that cannot be found to render as the empty
%% string. Compiling partials into cross-module calls would instead raise undef
%% at render time, so any referenced module the caller did not supply gets an
%% empty stub. Putting the check here rather than in the generated code keeps
%% the compiled path free of per-call existence tests; the plugin takes the
%% other branch and reports a missing partial as a build error.
-spec stub_missing([{module(), binary(), [ai_mustache_node()]}], map()) -> ok.
stub_missing(Parsed, Base) ->
    Have = [M || {M, _, _} <- Parsed],
    Want = lists:usort(lists:append([refs(Ast, Base) || {_, _, Ast} <- Parsed])),
    lists:foreach(fun(M) -> stub(M) end, [M || M <- Want, not lists:member(M, Have)]).

-spec refs([ai_mustache_node()], map()) -> [module()].
refs(Nodes, Base) -> refs(Nodes, Base, []).

refs([], _Base, Acc) ->
    Acc;
refs([{partial, _, Name, _} | Rest], Base, Acc) when is_binary(Name) ->
    refs(Rest, Base, [ai_mustache_ast:module_name(Name, Base) | Acc]);
refs([{section, _, _, B} | Rest], Base, Acc) ->
    refs(Rest, Base, refs(B, Base, Acc));
refs([{inverted, _, _, B} | Rest], Base, Acc) ->
    refs(Rest, Base, refs(B, Base, Acc));
refs([{has, _, _, B, _} | Rest], Base, Acc) ->
    refs(Rest, Base, refs(B, Base, Acc));
refs([{ext, _, _, _, B} | Rest], Base, Acc) ->
    refs(Rest, Base, refs(B, Base, Acc));
refs([_ | Rest], Base, Acc) ->
    refs(Rest, Base, Acc).

-spec stub(module()) -> ok.
stub(Mod) ->
    case is_loaded(Mod) of
        true  -> ok;
        false ->
            L = erl_anno:new(0),
            Forms = [{attribute, L, module, Mod},
                     {attribute, L, export, [{render, 1}, {render_iolist, 1},
                                             {render_stack, 1}, {render_stack, 2},
                                             {partials, 0}]},
                     {function, L, render, 1,
                      [{clause, L, [{var, L, '_Ctx'}], [], [{bin, L, []}]}]},
                     {function, L, render_iolist, 1,
                      [{clause, L, [{var, L, '_Ctx'}], [], [{nil, L}]}]},
                     {function, L, render_stack, 1,
                      [{clause, L, [{var, L, '_S'}], [], [{nil, L}]}]},
                     {function, L, render_stack, 2,
                      [{clause, L, [{var, L, '_S'}, {var, L, '_I'}], [], [{nil, L}]}]},
                     {function, L, partials, 0,
                      [{clause, L, [], [], [{nil, L}]}]}],
            {ok, Mod, Bin} = compile:forms(Forms, [return_errors, binary]),
            _ = code:purge(Mod),
            {module, Mod} = code:load_binary(Mod, "missing_partial", Bin),
            ok
    end.

-spec build([{module(), binary(), [ai_mustache_node()]}], map()) ->
          ok | ai_mustache_error().
build([], _Base) ->
    ok;
build([{Mod, Name, Ast} | Rest], Base) ->
    Opts = Base#{module => Mod, source => <<Name/binary, ".mustache">>},
    case ai_mustache_compiler:forms(Ast, Opts) of
        {error, _} = E -> E;
        {ok, Forms, _Deps} ->
            case load(Mod, Forms, Opts) of
                ok             -> build(Rest, Base);
                {error, _} = E -> E
            end
    end.

-spec load(module(), [erl_parse:abstract_form()], map()) -> ok | ai_mustache_error().
load(Mod, Forms, Opts) ->
    case compile:forms(Forms, [return_errors, binary]) of
        {ok, Mod, Bin} ->
            _ = code:purge(Mod),
            {module, Mod} = code:load_binary(Mod, source_list(Opts), Bin),
            ok;
        {error, Errors, _Warnings} ->
            {error, {source(Opts), 1, {codegen_failed, Errors}}}
    end.

-spec is_loaded(module()) -> boolean().
is_loaded(Mod) ->
    case code:is_loaded(Mod) of
        false -> false;
        _     -> true
    end.

-spec source(map()) -> binary().
source(#{source := S}) when is_binary(S) -> S;
source(_)                                -> <<"nofile">>.

-spec source_list(map()) -> string().
source_list(Opts) -> unicode:characters_to_list(source(Opts)).
