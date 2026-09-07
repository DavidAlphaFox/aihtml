%%%-------------------------------------------------------------------
%%% @doc Public entry points for the jinja engine.
%%%
%%% Templates normally reach here already compiled into modules by the rebar3
%%% plugin, in which case render/2 is a single call into the generated module:
%%% no lookup table, no process, no ets. render_string/2,3 compiles on the fly
%%% and exists for tests and for the run-time fallback of inline/2 when the
%%% parse_transform has not been applied.
%%% @end
%%%-------------------------------------------------------------------
-module(ai_jinja).

-include("ai_jinja.hrl").

-export([render/2, render_iolist/2]).
-export([render_string/2, render_string/3]).
-export([inline/2]).

-define(INLINE_PREFIX, "aij_").

%%%===================================================================
%%% Compiled templates
%%%===================================================================

%% @doc Render a template the build has already compiled into a module.
%%
%% The module name comes from the caller and is never derived from user input,
%% so there is no atom-table exposure here.
-spec render(module(), term()) -> binary().
render(Mod, Ctx) -> Mod:render(Ctx).

-spec render_iolist(module(), term()) -> iolist().
render_iolist(Mod, Ctx) -> Mod:render_iolist(Ctx).

%%%===================================================================
%%% Ad-hoc templates
%%%===================================================================

-spec render_string(binary(), term()) -> binary().
render_string(Template, Ctx) -> render_string(Template, Ctx, #{}).

%% @doc Compile and render a template given as a string.
%%
%% `templates' maps a name to its source, so `{% extends %}' and
%% `{% include %}' work with no views directory. The generated modules are
%% named after a digest of the whole set and reused on later calls, which is
%% what keeps a 500 case suite from recompiling the same template every time.
-spec render_string(binary(), term(), map()) -> binary().
render_string(Template, Ctx, Opts) ->
    case compile_set(Template, Opts) of
        {ok, Mod}      -> Mod:render(Ctx);
        {error, _} = E -> erlang:error({ai_jinja, E})
    end.

%% @doc Render an inline template.
%%
%% With ai_jinja_transform applied and a literal first argument, this call is
%% replaced at compile time and never runs. Without it the same template is
%% compiled here, so behaviour is identical and only the cost differs --
%% including the failures: the statements an inline template cannot carry are
%% rejected on both paths by the same predicate
%% (ai_jinja_compiler:inline_forbidden/1).
-spec inline(binary(), term()) -> binary().
inline(Template, Ctx) ->
    Opts = #{source => <<"inline">>},
    case ai_jinja_parser:parse(Template, Opts) of
        {error, _} = E ->
            erlang:error({ai_jinja, E});
        {ok, Nodes} ->
            case forbidden_in(Nodes) of
                {yes, Kind} ->
                    erlang:error({ai_jinja,
                                  {error, {<<"inline">>, 1,
                                           {target_in_inline_template, Kind}}}});
                no ->
                    render_string(Template, Ctx, #{})
            end
    end.

forbidden_in([]) -> no;
forbidden_in([Node | Rest]) ->
    Kind = element(1, Node),
    case ai_jinja_compiler:inline_forbidden(Kind) of
        true  -> {yes, Kind};
        false ->
            case forbidden_bodies(ai_jinja_ast:bodies(Node)) of
                no          -> forbidden_in(Rest);
                {yes, _} = Y -> Y
            end
    end.

forbidden_bodies([]) -> no;
forbidden_bodies([B | Rest]) ->
    case forbidden_in(B) of
        no           -> forbidden_bodies(Rest);
        {yes, _} = Y -> Y
    end.

%%%===================================================================
%%% On-the-fly compilation
%%%===================================================================

compile_set(Template, Opts) ->
    Templates = maps:get(templates, Opts, #{}),
    Prefix = prefix_for(Template, Templates, Opts),
    Main = binary_to_atom(<<Prefix/binary, "main">>, utf8),
    case is_loaded(Main) of
        true  -> {ok, Main};
        false -> compile_all(Template, Templates, Prefix, Main, Opts)
    end.

%% One prefix per template set, so that two sets that both define `base.j2'
%% cannot collide.
prefix_for(Template, Templates, Opts) ->
    Digest = erlang:md5(term_to_binary(
                          {Template, lists:sort(maps:to_list(Templates)),
                           maps:get(extensions, Opts, []),
                           ai_jinja_compiler:normalize_opts(Opts),
                           ?AI_JINJA_VSN})),
    <<?INLINE_PREFIX, (binary:encode_hex(Digest, lowercase))/binary, "_">>.

is_loaded(Mod) ->
    erlang:function_exported(Mod, render, 1)
        orelse code:ensure_loaded(Mod) =:= {module, Mod}.

compile_all(Template, Templates, Prefix, Main, Opts) ->
    Base = maps:merge(Opts, #{prefix => Prefix,
                              templates => Templates,
                              origin => string}),
    Units = [{Main, <<"main">>, Template}
             | [{ai_jinja_ast:module_name(N, Base), N, T}
                || {N, T} <- maps:to_list(Templates)]],
    case parse_units(Units, Base, []) of
        {error, _} = E -> E;
        {ok, Parsed} ->
            %% Every unit is parsed before any is compiled, so that each one
            %% knows what the others export and `{% from "x" import y %}' can
            %% be checked at build time rather than failing with an undef.
            Macros = maps:from_list([{M, macro_names(N)} || {M, _, N} <- Parsed]),
            case build(Parsed, Base#{macros => Macros}) of
                {error, _} = E -> E;
                ok             -> {ok, Main}
            end
    end.

parse_units([], _Base, Acc) ->
    {ok, lists:reverse(Acc)};
parse_units([{Mod, Name, Source} | Rest], Base, Acc) ->
    Opts = unit_opts(Mod, Name, Source, Base),
    case ai_jinja_parser:parse(Source, Opts) of
        {error, _} = E -> E;
        {ok, Nodes}    -> parse_units(Rest, Base, [{Mod, Opts, Nodes} | Acc])
    end.

macro_names(Nodes) ->
    lists:foldl(fun({macro, _, N, _, _}, Acc) -> [N | Acc];
                   (Node, Acc) ->
                        lists:foldl(fun(B, A) -> macro_names(B) ++ A end, Acc,
                                    ai_jinja_ast:bodies(Node))
                end, [], Nodes).

build(Units, Base) ->
    Compiled = [compile_unit(U, Base) || U <- Units],
    case [E || {error, _} = E <- Compiled] of
        [E | _] -> E;
        []      -> load_all(Compiled)
    end.

unit_opts(Mod, Name, Source, Base) ->
    Base#{module => Mod, source => Name,
          stamp => ai_jinja_compiler:source_hash(Source, Base)}.

compile_unit({Mod, Opts0, Nodes}, Base) ->
    Opts = maps:merge(Opts0, maps:with([macros], Base)),
    case ai_jinja_compiler:forms(Nodes, Opts) of
        {error, _} = E     -> E;
        {ok, Forms, _Deps} -> {Mod, Forms}
    end.

load_all(Compiled) ->
    lists:foldl(fun({Mod, Forms}, ok) -> load_one(Mod, Forms);
                   (_, {error, _} = E) -> E
                end, ok, Compiled).

load_one(Mod, Forms) ->
    case compile:forms(Forms, [return_errors, binary]) of
        {ok, Mod, Bin} ->
            case code:load_binary(Mod, atom_to_list(Mod), Bin) of
                {module, Mod} -> ok;
                {error, R}    -> {error, {<<"inline">>, 1, {codegen_failed, R}}}
            end;
        Other ->
            {error, {<<"inline">>, 1, {codegen_failed, Other}}}
    end.
