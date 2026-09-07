%%%-------------------------------------------------------------------
%%% @doc Jinja AST -> Erlang abstract forms.
%%%
%%% The only implementation of the translation; the rebar3 plugin and the
%%% parse_transform both go through it (architecture invariant 7).
%%%
%%% == A body is a fold, not a map ==
%%%
%%% `{% set %}' changes what the statements AFTER it can see, so compiling a
%%% body means threading a scope variable through it. Each node takes the
%%% current scope variable and returns statements, output expressions, and the
%%% scope variable in force afterwards. Almost every node returns the one it
%%% was given, so the generated code contains no threading at all unless the
%%% template actually assigns something.
%%%
%%% == Inheritance keeps the increment ==
%%%
%%% A module names only its DIRECT parent; the block table is merged at run
%%% time by all_blocks/0 walking one link up the chain. Flattening it at
%%% compile time would be faster by one small maps:merge per render and would
%%% force a rebuild of every descendant whenever a base template gained a
%%% block (architecture invariant 9).
%%% @end
%%%-------------------------------------------------------------------
-module(ai_jinja_compiler).

-include("ai_jinja.hrl").

-export([forms/2, compile_inline/3, source_hash/2, normalize_opts/1]).
-export([inline_forbidden/1]).

-import(ai_html_forms,
        [a/2, v/2, n/2, cl/4, fn/4, spec/4, t/2, param/3,
         bin/2, empty_bin/1, mklist/2, abstract/2,
         rem_call/4, loc_call/3, anno/1, remotes/2]).

-define(SV, 'V').        % scope variable base name
-define(IV, 'I').        % indent (reserved; see designs/11 section 6)
-define(BV, 'B').        % block override table
-define(TAG, ai_jinja).

-record(cs, {opts      :: map(),
             file      :: binary(),
             line_map  :: boolean(),
             escape    :: boolean(),
             counter = 0 :: non_neg_integer(),
             aux     = [] :: [tuple()],
             exports = [] :: [{atom(), arity()}],
             registry  :: map(),
             parent    :: module() | undefined,
             blocks  = [] :: [{atom(), atom()}],
             macros  = [] :: [{atom(), [atom()]}],
             imports = #{} :: #{atom() => module() | {module(), atom()}},
             ext_remotes = [] :: [module()],
             %% Appended to every generated variable name. Empty in a module,
             %% where the whole namespace is ours; distinctive in an inline
             %% expansion, which has to live inside somebody else's function
             %% without colliding with their variables.
             vsuffix = "" :: string(),
             block     :: atom() | undefined,       % block being compiled
             %% Innermost loop context: the variables folded_for/10 bound,
             %% plus `rec' when the loop is recursive. `#{}' as a type would
             %% mean the EMPTY map, which is not what is stored here.
             loop      :: undefined | map(),
             inline  = false :: boolean()}).

-opaque state() :: #cs{}.
-export_type([state/0]).

-define(THROW(Loc, Reason), throw({ai_jinja_compile, Loc, Reason})).

%%%===================================================================
%%% API
%%%===================================================================

-spec forms([ai_jinja_node()], map()) ->
          {ok, [erl_parse:abstract_form()], [module()]} | ai_jinja_error().
forms(Nodes0, Opts) ->
    case ai_jinja_ast:postprocess(Nodes0, Opts) of
        {error, _} = E -> E;
        {ok, Nodes, Deps} ->
            case new_state(Opts) of
                {error, Reason} ->
                    {error, {ai_html_text:source(Opts), 1, Reason}};
                {ok, St0} ->
                    try
                        St1 = collect(Nodes, St0),
                        module_forms(Nodes, Deps, St1)
                    catch
                        throw:{ai_jinja_compile, Loc, Reason} ->
                            {error, {ai_html_text:source(Opts), line_of(Loc), Reason}}
                    end
            end
    end.

%% @doc Compile a template into one expression, for the parse_transform.
-spec compile_inline([ai_jinja_node()], erl_parse:abstract_expr(), map()) ->
          {ok, erl_parse:abstract_expr()} | ai_jinja_error().
compile_inline(Nodes0, CtxExpr, Opts) ->
    Nodes = ai_jinja_ast:merge_text(ai_jinja_ast:fold_constants(
                                      ai_jinja_ast:merge_text(Nodes0))),
    case new_state(Opts) of
        {error, Reason} ->
            {error, {ai_html_text:source(Opts), 1, Reason}};
        {ok, St0} ->
            try
                ok = reject_inline(Nodes),
                ok = check_inline_macros(Nodes),
                St1 = collect(Nodes, St0#cs{inline = true,
                                            vsuffix = "__aihtml"}),
                L = anno(0),
                SV = scope_var(0, St1),
                {Stmts, Items, _V, St2} = compile_body(Nodes, SV, St1),
                Prelude = [{match, L, v(L, SV), rt(L, new_scope, [CtxExpr])}],
                Block = {block, L, lists:reverse(St2#cs.aux) ++ Prelude
                         ++ Stmts ++ [mklist(L, Items)]},
                {ok, rem_call(L, erlang, iolist_to_binary, [Block])}
            catch
                throw:{ai_jinja_compile, Loc, Reason} ->
                    {error, {ai_html_text:source(Opts), line_of(Loc), Reason}}
            end
    end.

%% @doc Statements an inline template cannot carry, and why.
-spec inline_forbidden(atom()) -> boolean().
inline_forbidden(K) ->
    lists:member(K, [include, extends, import, from, block]).

%% @doc The build stamp. The ONLY place it is computed.
-spec source_hash(binary(), map() | [{atom(), term()}]) -> binary().
source_hash(Body, Opts) ->
    erlang:md5(term_to_binary({Body, normalize_opts(Opts), ?AI_JINJA_VSN},
                              [deterministic])).

%% Only what can change the generated code, sorted so the result does not
%% depend on map iteration order. `suffix' feeds only the existence check and
%% `views_abs' is an absolute path that would make the build unreproducible,
%% so neither is included.
-spec normalize_opts(map() | [{atom(), term()}]) -> [{atom(), term()}].
normalize_opts(Opts0) ->
    Opts = ai_html_text:opts(as_map(Opts0)),
    lists:sort([{K, maps:get(K, Opts)}
                || K <- [prefix, views, extensions, escape, trim_blocks,
                         lstrip_blocks, keep_trailing_newline,
                         strict_undefined, line_map],
                   maps:is_key(K, Opts)]).

%% The self-description records the options as the sorted list normalize_opts/1
%% produces, so reading them back finds a list rather than a map.
as_map(L) when is_list(L) -> maps:from_list(L);
as_map(M) when is_map(M)  -> M.

%%%===================================================================
%%% State
%%%===================================================================

new_state(Opts0) ->
    Opts = ai_html_text:opts(Opts0),
    case ai_jinja_ext:registry(maps:get(extensions, Opts, [])) of
        {error, _} = E -> E;
        {ok, Reg} ->
            {ok, #cs{opts     = Opts,
                     file     = ai_html_text:source(Opts),
                     line_map = maps:get(line_map, Opts, true),
                     escape   = maps:get(escape, Opts, true),
                     registry = Reg}}
    end.

%% Everything the body needs to know about itself before any of it is
%% compiled: which macros exist (a macro may be called before it is defined),
%% which names are imported, which blocks there are, and whether a parent
%% template takes over.
collect(Nodes, St) ->
    lists:foldl(fun collect_node/2, St, Nodes).

collect_node({macro, _, Name, Params, Body}, St) ->
    collect(Body, St#cs{macros = [{Name, [P || {P, _} <- Params]} | St#cs.macros]});
collect_node({extends, _, Mod}, St) ->
    St#cs{parent = Mod};
collect_node({import, _, Mod, As, WithCtx}, St) ->
    St#cs{imports = (St#cs.imports)#{As => {module, Mod, WithCtx}}};
collect_node({from, Loc, Mod, Names, WithCtx}, St) ->
    %% When the caller told us what each target exports -- the plugin and
    %% ai_jinja:render_string/3 both do -- a typo here is a build error rather
    %% than an undef at render time.
    case maps:get(macros, St#cs.opts, undefined) of
        #{Mod := Exported} ->
            [?THROW(Loc, {macro_not_found, N, Mod})
             || {N, _} <- Names, not lists:member(N, Exported)];
        _ ->
            ok
    end,
    lists:foldl(fun({N, As}, S) ->
                        S#cs{imports =
                                 (S#cs.imports)#{As => {macro, Mod, N, WithCtx}}}
                end, St, Names);
collect_node({block, _, Name, _, _, Body}, St) ->
    St1 = St#cs{blocks = [{Name, block_fun(Name)} | St#cs.blocks]},
    collect(Body, St1);
collect_node(Node, St) ->
    lists:foldl(fun(B, S) -> collect(B, S) end, St, ai_jinja_ast:bodies(Node)).

block_fun(Name) -> list_to_atom("block_" ++ atom_to_list(Name)).
macro_fun(Name) -> list_to_atom("macro_" ++ atom_to_list(Name)).

%%%===================================================================
%%% Module assembly
%%%===================================================================

module_forms(Nodes, Deps, St0) ->
    Mod = maps:get(module, St0#cs.opts),
    L = anno(0),
    {RenderWith, St} =
        case St0#cs.parent of
            undefined ->
                {Stmts, Items, _V, S} = compile_body(Nodes, ?SV, St0),
                {block_of(L, Stmts, mklist(L, Items)), S};
            Parent ->
                %% Output stops at the {% extends %}: whatever precedes it is
                %% emitted, whatever follows contributes only its bindings.
                %% That is the reference implementation's behaviour and it is
                %% the reason the split happens here rather than in the parser.
                {Before, After} = split_at_extends(Nodes),
                {S1, I1, V1, SA} = compile_body(Before, ?SV, St0),
                {S2, _I2, V2, SB} = compile_body(After, V1, SA),
                Delegate = rem_call(L, Parent, render_with,
                                    [v(L, V2), v(L, ?IV), v(L, ?BV)]),
                {block_of(L, S1 ++ S2, mklist(L, I1 ++ [Delegate])), SB}
        end,
    Blocks = lists:usort(St#cs.blocks),
    Exports = [{render, 1}, {render_iolist, 1}, {render_scope, 2},
               {render_with, 3}, {blocks, 0}, {all_blocks, 0}, {partials, 0}]
        ++ lists:usort(St#cs.exports),
    Forms =
        [{attribute, L, module, Mod},
         {attribute, L, jinja_source, source_attr(St)},
         {attribute, L, export, Exports},
         spec(L, render, [t(L, term)], t(L, binary)),
         fn(L, render, [v(L, 'Ctx')],
            [rem_call(L, erlang, iolist_to_binary,
                      [loc_call(L, render_iolist, [v(L, 'Ctx')])])]),
         spec(L, render_iolist, [t(L, term)], t(L, iolist)),
         fn(L, render_iolist, [v(L, 'Ctx')],
            [loc_call(L, render_with, [rt(L, new_scope, [v(L, 'Ctx')]),
                                       empty_bin(L),
                                       loc_call(L, all_blocks, [])])]),
         spec(L, render_scope, [t(L, list), t(L, binary)], t(L, iolist)),
         fn(L, render_scope, [v(L, ?SV), v(L, ?IV)],
            [loc_call(L, render_with, [v(L, ?SV), v(L, ?IV),
                                       loc_call(L, all_blocks, [])])]),
         spec(L, blocks, [], t(L, map)),
         fn(L, blocks, [], [blocks_map(L, Mod, Blocks)]),
         spec(L, all_blocks, [], t(L, map)),
         fn(L, all_blocks, [], [all_blocks_expr(L, St#cs.parent)]),
         spec(L, partials, [], {type, L, list, [t(L, module)]}),
         fn(L, partials, [], [mklist(L, [a(L, D) || D <- Deps])]),
         spec(L, render_with, [t(L, list), t(L, binary), t(L, map)], t(L, iolist)),
         fn(L, render_with,
            [param(L, ?SV, RenderWith), param(L, ?IV, RenderWith),
             param(L, ?BV, RenderWith)],
            [RenderWith])]
        ++ lists:reverse(St#cs.aux),
    Allowed = [erlang, lists, maps, code, ai_jinja_rt, ai_jinja_filters,
               ai_jinja_tests, ai_html_escape, Mod]
        ++ Deps ++ parent_list(St#cs.parent) ++ St#cs.ext_remotes,
    case check_remotes(Forms, Allowed, St) of
        ok             -> {ok, Forms, Deps};
        {error, _} = E -> E
    end.

%% Everything up to and including the {% extends %}, then the rest.
split_at_extends(Nodes) ->
    case lists:splitwith(fun(N) -> element(1, N) =/= extends end, Nodes) of
        {Before, [Ext | After]} -> {Before ++ [Ext], After};
        {Before, []}            -> {Before, []}
    end.

parent_list(undefined) -> [];
parent_list(P)         -> [P].

blocks_map(L, Mod, Blocks) ->
    {map, L, [{map_field_assoc, L, a(L, Name),
               {'fun', L, {function, a(L, Mod), a(L, Fun), n(L, 3)}}}
              || {Name, Fun} <- Blocks]}.

all_blocks_expr(L, undefined) ->
    loc_call(L, blocks, []);
all_blocks_expr(L, Parent) ->
    rem_call(L, maps, merge, [rem_call(L, Parent, all_blocks, []),
                              loc_call(L, blocks, [])]).

source_attr(#cs{opts = Opts}) ->
    Attr = #{path  => ai_html_text:source(Opts),
             stamp => maps:get(stamp, Opts, <<>>),
             mtime => maps:get(mtime, Opts, 0),
             vsn   => ?AI_JINJA_VSN,
             opts  => normalize_opts(Opts)},
    case maps:get(origin, Opts, file) of
        file   -> Attr;
        Origin -> Attr#{origin => Origin}
    end.

%% Generated modules may only call the runtime, the filter and test libraries,
%% other generated modules, and whatever a registered extension brought with
%% it. A violation is a bug in this module, so it is checked rather than
%% assumed (architecture invariant 8).
check_remotes(Forms, Allowed, St) ->
    case [M || M <- remotes(Forms, []), not lists:member(M, Allowed)] of
        []  -> ok;
        Bad -> {error, {St#cs.file, 1, {unexpected_remote_calls, lists:usort(Bad)}}}
    end.

%%%===================================================================
%%% Bodies
%%%===================================================================

%% Returns {Statements, OutputExpressions, ScopeVarAfter, State}.
%%
%% Statements are hoisted in order to the front of the enclosing block. That
%% is safe because a straight-line body executes all of them unconditionally
%% anyway, and it is what lets the output stay a plain list literal.
compile_body(Nodes, VIn, St) -> compile_body(Nodes, VIn, St, [], []).

compile_body([], V, St, Stmts, Items) ->
    {lists:reverse(Stmts), lists:reverse(Items), V, St};
compile_body([Node | Rest], V, St, Stmts, Items) ->
    {S1, I1, V1, St1} = compile_node(Node, V, St),
    compile_body(Rest, V1, St1,
                 lists:reverse(S1) ++ Stmts, lists:reverse(I1) ++ Items).

%% A body as one expression evaluating to an iolist. Used everywhere the
%% scope must not escape: loop bodies, blocks, macros, with.
body_expr(Nodes, VIn, St) ->
    {Stmts, Items, _V, St1} = compile_body(Nodes, VIn, St),
    L = anno(0),
    {block_of(L, Stmts, mklist(L, Items)), St1}.

%% Wrap statements and a result into one expression, underscoring any binding
%% the rest never reads.
%%
%% A {% set %} in the last branch of an {% if %} still has to publish a scope
%% variable, even when nothing follows the block to read it. Leaving it named
%% would fail the generated module under warnings_as_errors, which is exactly
%% the configuration a real build uses.
block_of(_L, [], Tail) ->
    Tail;
block_of(L, Stmts, Tail) ->
    {block, L, prune_bindings(Stmts, [Tail]) ++ [Tail]}.

prune_bindings(Stmts, Tail) -> prune_bindings(Stmts, Tail, []).

prune_bindings([], _Tail, Acc) ->
    lists:reverse(Acc);
prune_bindings([{match, L, Pat, Expr} | Rest], Tail, Acc) ->
    prune_bindings(Rest, Tail,
                   [{match, L, underscore_unused(Pat, Rest ++ Tail), Expr} | Acc]);
prune_bindings([S | Rest], Tail, Acc) ->
    prune_bindings(Rest, Tail, [S | Acc]).

underscore_unused({var, L, V}, Following) ->
    case hd(atom_to_list(V)) =/= $_ andalso not ai_html_forms:uses_var(Following, V) of
        true  -> {var, L, list_to_atom([$_ | atom_to_list(V)])};
        false -> {var, L, V}
    end;
underscore_unused({tuple, L, Es}, Following) ->
    {tuple, L, [underscore_unused(E, Following) || E <- Es]};
underscore_unused(Pat, _Following) ->
    Pat.

%%%===================================================================
%%% Nodes
%%%===================================================================

compile_node({text, Loc, Bin}, V, St) ->
    {[], [bin(line(Loc, St), Bin)], V, St};

compile_node({output, Loc, E}, V, St) ->
    L = line(Loc, St),
    {Expr, St1} = expr(E, V, St),
    {[], [emit(L, Expr, St)], V, St1};

compile_node({do, Loc, E}, V, St) ->
    L = line(Loc, St),
    {Expr, St1} = expr(E, V, St),
    {[{match, L, v(L, '_'), Expr}], [], V, St1};

compile_node({set, Loc, Name, E}, V, St) when is_atom(Name) ->
    L = line(Loc, St),
    {Expr, St1} = expr(E, V, St),
    {VNew, St2} = next_scope(St1),
    {[{match, L, v(L, VNew), rt(L, bind, [v(L, V), a(L, Name), Expr])}],
     [], VNew, St2};
compile_node({set, Loc, {attr, _, _}, _E}, _V, St) ->
    ?THROW(Loc, {namespace_assignment_unsupported, Loc}),
    {[], [], none, St};

compile_node({set_block, Loc, Name, Body, Filter}, V, St) when is_atom(Name) ->
    L = line(Loc, St),
    {BodyExpr, St1} = body_expr(Body, V, St),
    %% The body has already been rendered and escaped; marking it safe is what
    %% stops `{% set x %}a & b{% endset %}{{ x }}' printing `&amp;amp;'.
    Safe = {tuple, L, [a(L, safe), BodyExpr]},
    {Value, St2} = apply_body_filter(Filter, Safe, V, St1, L),
    {VNew, St3} = next_scope(St2),
    {[{match, L, v(L, VNew), rt(L, bind, [v(L, V), a(L, Name), Value])}],
     [], VNew, St3};
compile_node({set_block, Loc, {attr, _, _}, _B, _F}, _V, St) ->
    ?THROW(Loc, {namespace_assignment_unsupported, Loc}),
    {[], [], none, St};

compile_node({'if', Loc, Branches, Else}, V, St) ->
    compile_if(Loc, Branches, Else, V, St);

compile_node({'for', _, _, _, _, _, _, _} = Node, V, St) ->
    {Expr, St1} = compile_for(Node, V, St),
    {[], [Expr], V, St1};

compile_node({with, Loc, Bindings, Body}, V, St) ->
    L = line(Loc, St),
    {Fields, St1} = lists:mapfoldl(
                      fun({N, E}, S) ->
                              {Expr, S1} = expr(E, V, S),
                              {{map_field_assoc, L, a(L, N), Expr}, S1}
                      end, St, Bindings),
    {VInner, St2} = next_scope(St1),
    {BodyExpr, St3} = body_expr(Body, VInner, St2),
    Block = {block, L, [{match, L, v(L, VInner),
                         rt(L, push, [v(L, V), {map, L, Fields}])},
                        BodyExpr]},
    {[], [Block], V, St3};

compile_node({filter, Loc, Chain, Body}, V, St) ->
    L = line(Loc, St),
    {BodyExpr, St1} = body_expr(Body, V, St),
    Safe = {tuple, L, [a(L, safe), BodyExpr]},
    {Expr, St2} = apply_body_filter(Chain, Safe, V, St1, L),
    {[], [emit(L, Expr, St2)], V, St2};

compile_node({include, Loc, Mod, Ignore, WithCtx}, V, St) ->
    L = line(Loc, St),
    reject_in_inline(include, Loc, St),
    Scope = case WithCtx of
                true  -> v(L, V);
                false -> rt(L, new_scope, [a(L, undefined)])
            end,
    Call = rem_call(L, Mod, render_scope, [Scope, v(L, ?IV)]),
    Expr = case Ignore of
               false -> Call;
               true  ->
                   %% `ignore missing' is the one place a target is allowed
                   %% not to exist, so it is checked at run time. ensure_loaded
                   %% first, or a module that simply has not been loaded yet
                   %% would look missing.
                   {block, L,
                    [{match, L, v(L, '_'),
                      rem_call(L, code, ensure_loaded, [a(L, Mod)])},
                     {'case', L,
                      rem_call(L, erlang, function_exported,
                               [a(L, Mod), a(L, render_scope), n(L, 2)]),
                      [cl(L, [a(L, true)], [], [Call]),
                       cl(L, [a(L, false)], [], [{nil, L}])]}]}
           end,
    {[], [Expr], V, St};

compile_node({extends, _Loc, _Mod}, V, St) ->
    {[], [], V, St};

compile_node({import, _Loc, _Mod, _As, _With}, V, St) ->
    {[], [], V, St};
compile_node({from, _Loc, _Mod, _Names, _With}, V, St) ->
    {[], [], V, St};

compile_node({block, Loc, Name, Scoped, Required, Body}, V, St) ->
    L = line(Loc, St),
    reject_in_inline(block, Loc, St),
    St1 = emit_block(Name, Loc, Required, Body, St),
    Fun = block_fun(Name),
    Default = {'fun', L, {function, a(L, self_module(St)), a(L, Fun), n(L, 3)}},
    Lookup = rem_call(L, maps, get, [a(L, Name), v(L, ?BV), Default]),
    %% Without `scoped' a block does not see the loop or with variables around
    %% its call site -- only the template level scope. That is what makes
    %% `{% for i in ... %}{% block item %}' render nothing for `i' in an
    %% overriding child unless the block was declared scoped.
    Scope = case Scoped of
                true  -> v(L, V);
                false -> rt(L, globals, [v(L, V)])
            end,
    {[], [{call, L, Lookup, [Scope, v(L, ?IV), v(L, ?BV)]}], V, St1};

compile_node({macro, Loc, Name, Params, Body}, V, St) ->
    {[], [], V, emit_macro(Name, Loc, Params, Body, St)};

compile_node({call, Loc, Params, Target, Body}, V, St) ->
    compile_call_block(Loc, Params, Target, Body, V, St).

%%%===================================================================
%%% if
%%%===================================================================

compile_if(Loc, Branches, Else, V, St0) ->
    L = line(Loc, St0),
    {Compiled, St1} =
        lists:mapfoldl(
          fun({Cond, Body}, S) ->
                  {CondExpr, S1} = expr(Cond, V, S),
                  {Stmts, Items, VOut, S2} = compile_body(Body, V, S1),
                  {{CondExpr, Stmts, Items, VOut}, S2}
          end, St0, Branches),
    {EStmts, EItems, EVOut, St2} = compile_body(Else, V, St1),
    Advances = lists:any(fun({_, _, _, VO}) -> VO =/= V end, Compiled)
        orelse EVOut =/= V,
    case Advances of
        false ->
            Expr = if_expr(L, Compiled, {EStmts, EItems, EVOut},
                           fun plain_branch/3),
            {[], [Expr], V, St2};
        true ->
            %% An {% if %} is not a scope, so a {% set %} in a branch has to
            %% survive it. Both arms therefore return {Output, Scope} and the
            %% case is matched, which is why this shape only appears when a
            %% branch really does assign.
            {VNew, St3} = next_scope(St2),
            {OutVar, St4} = next_temp(St3),
            Expr = if_expr(L, Compiled, {EStmts, EItems, EVOut},
                           fun tuple_branch/3),
            Match = {match, L, {tuple, L, [v(L, OutVar), v(L, VNew)]}, Expr},
            {[Match], [v(L, OutVar)], VNew, St4}
    end.

plain_branch(L, {Stmts, Items}, _VOut) ->
    block_of(L, Stmts, mklist(L, Items)).

tuple_branch(L, {Stmts, Items}, VOut) ->
    block_of(L, Stmts, {tuple, L, [mklist(L, Items), v(L, VOut)]}).

if_expr(L, [{Cond, Stmts, Items, VOut} | Rest], {EStmts, EItems, EVOut}, Wrap) ->
    Then = Wrap(L, {Stmts, Items}, VOut),
    ElseExpr = case Rest of
                   [] -> Wrap(L, {EStmts, EItems}, EVOut);
                   _  -> if_expr(L, Rest, {EStmts, EItems, EVOut}, Wrap)
               end,
    {'case', L, rt(L, truthy, [Cond]),
     [cl(L, [a(L, true)], [], [Then]),
      cl(L, [a(L, false)], [], [ElseExpr])]}.

%%%===================================================================
%%% for
%%%===================================================================

compile_for({'for', Loc, Targets, Iter, Filter, Recursive, Body, Else}, V, St0) ->
    L = line(Loc, St0),
    {IterExpr, St1} = expr(Iter, V, St0),
    {ItemsVar, St2} = next_temp(St1),
    {ElemVar, St3}  = next_temp(St2),
    NeedsLoop = uses_loop(Body),
    NeedsChanged = uses_changed(Body),
    Pattern = target_pattern(L, Targets, ElemVar),
    %% The filter has to see the loop variable, so it is applied in an inner
    %% comprehension whose own scope frame binds it.
    {FilterExpr, St4} =
        case Filter of
            undefined -> {undefined, St3};
            _ ->
                {VF, StF0} = next_scope(St3),
                {FE, StF1} = expr(Filter, VF, StF0),
                {{VF, FE}, StF1}
        end,
    Source = filtered_source(L, IterExpr, Pattern, ElemVar, Targets, FilterExpr, V),
    {LoopExpr, St5} =
        case {NeedsLoop, Recursive, NeedsChanged} of
            {false, false, false} ->
                simple_for(L, Pattern, ItemsVar, Targets, ElemVar, Body, V, St4);
            {_, false, _} ->
                folded_for(L, Pattern, ItemsVar, Targets, ElemVar, Body, V,
                           NeedsChanged, undefined, St4);
            {_, true, _} ->
                recursive_for(L, Pattern, ItemsVar, Targets, ElemVar, Body, V,
                              NeedsChanged, St4)
        end,
    {WithElse, St6} =
        case Else of
            [] -> {LoopExpr, St5};
            _ ->
                {ElseExpr, StE} = body_expr(Else, V, St5),
                {{'case', L, v(L, ItemsVar),
                  [cl(L, [{nil, L}], [], [ElseExpr]),
                   cl(L, [v(L, '_')], [], [LoopExpr])]}, StE}
        end,
    Expr = {block, L, [{match, L, v(L, ItemsVar), Source}, WithElse]},
    {Expr, St6}.

filtered_source(L, IterExpr, _Pattern, _ElemVar, _Targets, undefined, _V) ->
    rt(L, to_list, [IterExpr]);
filtered_source(L, IterExpr, Pattern, ElemVar, Targets, {VF, FE}, V) ->
    Frame = target_frame(L, Targets, ElemVar),
    {lc, L, v(L, ElemVar),
     [{generate, L, Pattern, rt(L, to_list, [IterExpr])},
      {block, L, [{match, L, v(L, VF), rt(L, push, [v(L, V), Frame])},
                  rt(L, truthy, [FE])]}]}.

%% A plain loop with no `loop' variable is a list comprehension, which is
%% both the smallest code and the fastest thing the emulator can run.
simple_for(L, Pattern, ItemsVar, Targets, ElemVar, Body, V, St) ->
    {VInner, St1} = next_scope(St),
    {BodyExpr, St2} = body_expr(Body, VInner, St1),
    Frame = target_frame(L, Targets, ElemVar),
    {{lc, L,
      {block, L, [{match, L, v(L, VInner), rt(L, push, [v(L, V), Frame])},
                  BodyExpr]},
      [{generate, L, Pattern, v(L, ItemsVar)}]}, St2}.

%% Anything that needs `loop' needs the index and the total, so the
%% comprehension becomes a fold.
folded_for(L, Pattern, ItemsVar, Targets, ElemVar, Body, V, NeedsChanged,
           DepthVar, St) ->
    {VInner, St1} = next_scope(St),
    {NVar, St2}   = next_temp(St1),
    {AccVar, St3} = next_temp(St2),
    {LoopVar, St4} = next_temp(St3),
    {PrevScope, St5} = case NeedsChanged of
                           true  -> next_scope(St4);
                           false -> {undefined, St4}
                       end,
    %% `rec' comes from recursive_for/9 and must survive: it is what
    %% `{{ loop(children) }}' inside the body compiles against.
    Rec = case St#cs.loop of
              #{rec := R} -> #{rec => R};
              _           -> #{}
          end,
    LoopCtx = maps:merge(#{scope => VInner, prev_scope => PrevScope, n => NVar,
                           items => ItemsVar, targets => Targets,
                           elem => ElemVar, depth => DepthVar}, Rec),
    {BodyExpr, St6} = body_expr(Body, VInner, St5#cs{loop = LoopCtx}),
    St7 = St6#cs{loop = St#cs.loop},
    Depth = case DepthVar of
                undefined -> n(L, 0);
                _         -> v(L, DepthVar)
            end,
    Frame0 = target_frame(L, Targets, ElemVar),
    Bind = rt(L, push, [v(L, V),
                        map_put(L, Frame0, loop, v(L, LoopVar))]),
    PrevBind = case NeedsChanged of
                   false -> [];
                   true ->
                       PrevFrame = target_frame_from(
                                     L, Targets,
                                     rt(L, subscript, [v(L, ItemsVar),
                                                       {op, L, '-', v(L, NVar), n(L, 2)}])),
                       [{match, L, v(L, PrevScope),
                         rt(L, push, [v(L, V), PrevFrame])}]
               end,
    FunBody =
        [{match, L, v(L, LoopVar),
          rt(L, loop, [v(L, NVar), rem_call(L, erlang, length, [v(L, ItemsVar)]),
                       v(L, ItemsVar), Depth, v(L, ElemVar)])},
         {match, L, v(L, VInner), Bind}]
        ++ PrevBind
        ++ [{tuple, L, [{op, L, '+', v(L, NVar), n(L, 1)},
                        {cons, L, BodyExpr, v(L, AccVar)}]}],
    Fold = rem_call(L, lists, foldl,
                    [{'fun', L, {clauses,
                                 [cl(L, [Pattern,
                                         {tuple, L, [v(L, NVar), v(L, AccVar)]}],
                                     [], FunBody)]}},
                     {tuple, L, [n(L, 1), {nil, L}]},
                     v(L, ItemsVar)]),
    {ResVar, St8} = next_temp(St7),
    {{block, L, [{match, L, {tuple, L, [v(L, '_'), v(L, ResVar)]}, Fold},
                 rem_call(L, lists, reverse, [v(L, ResVar)])]}, St8}.

%% `{% for ... recursive %}' with `{{ loop(children) }}' in the body becomes a
%% named fun so the body can call back into it, carrying the depth.
recursive_for(L, Pattern, OuterItems, Targets, ElemVar, Body, V, NeedsChanged, St) ->
    {RecVar, St1}   = next_temp(St),
    {DepthVar, St2} = next_temp(St1),
    {ArgVar, St3}   = next_temp(St2),
    %% A fresh items variable: the outer one is already bound to the initial
    %% sequence, and matching against it inside the fun would compare rather
    %% than bind.
    {ItemsVar, St3b} = next_temp(St3),
    St4 = St3b#cs{loop = maps:put(rec, {RecVar, DepthVar},
                                  case St3b#cs.loop of
                                      undefined -> #{};
                                      M -> M
                                  end)},
    {Inner, St5} = folded_for(L, Pattern, ItemsVar, Targets, ElemVar, Body, V,
                              NeedsChanged, DepthVar, St4),
    St6 = St5#cs{loop = St#cs.loop},
    Clause = cl(L, [v(L, ArgVar), v(L, DepthVar)], [],
                [{block, L, [{match, L, v(L, ItemsVar), rt(L, to_list, [v(L, ArgVar)])},
                             Inner]}]),
    Fun = {named_fun, L, RecVar, [Clause]},
    {{block, L, [{match, L, v(L, RecVar), Fun},
                 {call, L, v(L, RecVar), [v(L, OuterItems), n(L, 0)]}]}, St6}.

target_pattern(L, [_One], ElemVar) -> v(L, ElemVar);
target_pattern(L, Targets, ElemVar) ->
    _ = Targets,
    v(L, ElemVar).

target_frame(L, Targets, ElemVar) -> target_frame_from(L, Targets, v(L, ElemVar)).

%% One target binds the element; several destructure it, which is what
%% `{% for k, v in m.items() %}' needs.
target_frame_from(L, [One], Expr) ->
    {map, L, [{map_field_assoc, L, a(L, One), Expr}]};
target_frame_from(L, Targets, Expr) ->
    {map, L, [{map_field_assoc, L, a(L, T),
               rt(L, subscript, [Expr, n(L, I)])}
              || {I, T} <- lists:zip(lists:seq(0, length(Targets) - 1), Targets)]}.

map_put(L, {map, L2, Fields}, Key, Expr) ->
    {map, L2, Fields ++ [{map_field_assoc, L, a(L, Key), Expr}]}.

uses_loop(Body) -> mentions(Body, loop).
uses_changed(Body) -> mentions_call(Body, loop, changed).

%%%===================================================================
%%% Blocks and macros
%%%===================================================================

emit_block(Name, Loc, Required, Body, St) ->
    L = line(Loc, St),
    Fun = block_fun(Name),
    St1 = St,
    {BodyExpr, St2} =
        case Required of
            true ->
                {rem_call(L, erlang, error,
                          [{tuple, L, [a(L, ?TAG),
                                       {tuple, L, [a(L, required_block_not_provided),
                                                   a(L, Name)]}]}]), St1};
            false ->
                body_expr(Body, ?SV, St1#cs{block = Name})
        end,
    St3 = St2#cs{block = St#cs.block},
    Def = fn(L, Fun,
             [param(L, ?SV, BodyExpr), param(L, ?IV, BodyExpr),
              param(L, ?BV, BodyExpr)],
             [BodyExpr]),
    St3#cs{aux = [Def | St3#cs.aux], exports = [{Fun, 3} | St3#cs.exports]}.

%% A macro compiles to a module-level function that binds its own parameters,
%% because the caller may be in another module and cannot know them.
emit_macro(Name, Loc, Params, Body, St) ->
    L = line(Loc, St),
    Fun = macro_fun(Name),
    {ArgsVar, St1} = next_temp(St),
    {PosVar, St2}  = next_temp(St1),
    V0 = scope_var(0, St2),
    {VInner, St3} = next_scope(St2),
    Names = [P || {P, _} <- Params],
    {Fields, St4} =
        lists:mapfoldl(
          fun({{P, Default}, I}, S) ->
                  {DefExpr, S1} = case Default of
                                      undefined -> {a(L, undefined), S};
                                      _         -> expr(Default, V0, S)
                                  end,
                  {{map_field_assoc, L, a(L, P),
                    rt(L, arg, [v(L, ArgsVar), v(L, PosVar), n(L, I), a(L, P),
                                DefExpr])}, S1}
          end, St3, lists:zip(Params, lists:seq(0, length(Params) - 1))),
    Extra = [{map_field_assoc, L, a(L, varargs),
              rt(L, varargs, [v(L, PosVar), n(L, length(Params))])},
             {map_field_assoc, L, a(L, kwargs),
              rt(L, kwargs, [v(L, ArgsVar), ai_html_forms:atoms(L, Names)])},
             {map_field_assoc, L, a(L, caller),
              rem_call(L, maps, get, [a(L, caller), v(L, ArgsVar), a(L, undefined)])}],
    {Body0, St5} = body_expr(Body, VInner, St4),
    %% A macro's output has already been escaped by its own body, so it is
    %% marked safe: `{{ m() }}' must not escape it a second time, and it must
    %% not be rendered as the iolist it literally is.
    BodyExpr = {tuple, L, [a(L, safe), Body0]},
    Def = fn(L, Fun, [v(L, ArgsVar), v(L, V0)],
             [{block, L,
               [{match, L, v(L, PosVar),
                 rem_call(L, maps, get, [a(L, '$positional'), v(L, ArgsVar), {nil, L}])},
                {match, L, v(L, VInner),
                 rt(L, push, [v(L, V0), {map, L, Fields ++ Extra}])},
                BodyExpr]}]),
    case St5#cs.inline of
        false -> St5#cs{aux = [Def | St5#cs.aux],
                        exports = [{Fun, 2} | St5#cs.exports]};
        true  -> St5#cs{aux = [inline_fun(L, Fun, Def, St5) | St5#cs.aux]}
    end.

%% Inside an inline expansion there is no module to add a function to, so a
%% macro becomes a bound anonymous fun instead.
inline_fun(L, Fun, {function, _, _, _, Clauses}, St) ->
    {match, L, v(L, inline_var(Fun, St)), {'fun', L, {clauses, Clauses}}}.

inline_var(Fun, #cs{vsuffix = Sfx}) ->
    list_to_atom([$M | atom_to_list(Fun)] ++ "__" ++ Sfx).

%% {% call %} hands the block body to the macro as `caller'.
compile_call_block(Loc, Params, Target, Body, V, St) ->
    L = line(Loc, St),
    {ArgVar, St1} = next_temp(St),
    {VInner, St2} = next_scope(St1),
    {BodyExpr, St3} = body_expr(Body, VInner, St2),
    Frame = {map, L, [{map_field_assoc, L, a(L, P),
                       rt(L, subscript, [v(L, ArgVar), n(L, I)])}
                      || {{P, _}, I} <- lists:zip(Params,
                                                  lists:seq(0, length(Params) - 1))]},
    %% Same reasoning as a macro body: `{{ caller() }}' is interpolated, so
    %% what comes back has to be marked safe.
    CallerFun = {'fun', L, {clauses,
                            [cl(L, [param(L, ArgVar, Frame)], [],
                                [{block, L,
                                  [{match, L, v(L, VInner),
                                    rt(L, push, [v(L, V), Frame])},
                                   {tuple, L, [a(L, safe), BodyExpr]}]}])]}},
    {Expr, St4} = call_expr(Target, V, St3, [{caller, CallerFun}]),
    {[], [emit(L, Expr, St4)], V, St4}.

%%%===================================================================
%%% Expressions
%%%===================================================================

expr({lit, Loc, V}, _Scope, St) ->
    {abstract(line(Loc, St), V), St};
expr({name, Loc, N}, Scope, St) ->
    L = line(Loc, St),
    Fun = case ai_jinja_rt:is_builtin(N) of
              true  -> resolve_or_builtin;   % `range' is a value as well as a call
              false -> resolve
          end,
    {rt(L, Fun, [a(L, N), v(L, Scope)]), St};
expr({attr, Loc, E, K}, Scope, St) ->
    L = line(Loc, St),
    {Expr, St1} = expr(E, Scope, St),
    {rt(L, attr, [Expr, a(L, K)]), St1};
expr({sub, Loc, E, I}, Scope, St) ->
    L = line(Loc, St),
    {EE, St1} = expr(E, Scope, St),
    {IE, St2} = expr(I, Scope, St1),
    {rt(L, subscript, [EE, IE]), St2};
expr({slice, Loc, E, A, B, C}, Scope, St) ->
    L = line(Loc, St),
    {EE, St1} = expr(E, Scope, St),
    {AE, St2} = maybe_expr(A, Scope, St1, L),
    {BE, St3} = maybe_expr(B, Scope, St2, L),
    {CE, St4} = maybe_expr(C, Scope, St3, L),
    {rt(L, slice, [EE, AE, BE, CE]), St4};
expr({binop, Loc, Op, A, B}, Scope, St) ->
    L = line(Loc, St),
    {AE, St1} = expr(A, Scope, St),
    {BE, St2} = expr(B, Scope, St1),
    {binop_call(L, Op, AE, BE), St2};
expr({unop, Loc, 'not', E}, Scope, St) ->
    L = line(Loc, St),
    {EE, St1} = expr(E, Scope, St),
    {{op, L, 'not', rt(L, truthy, [EE])}, St1};
expr({unop, Loc, '-', E}, Scope, St) ->
    L = line(Loc, St),
    {EE, St1} = expr(E, Scope, St),
    {rt(L, sub, [n(L, 0), EE]), St1};
expr({unop, _Loc, '+', E}, Scope, St) ->
    expr(E, Scope, St);
expr({'and', Loc, A, B}, Scope, St) ->
    short_circuit(Loc, A, B, false, Scope, St);
expr({'or', Loc, A, B}, Scope, St) ->
    short_circuit(Loc, A, B, true, Scope, St);
expr({'cond', Loc, C, T, E}, Scope, St) ->
    L = line(Loc, St),
    {CE, St1} = expr(C, Scope, St),
    {TE, St2} = expr(T, Scope, St1),
    {EE, St3} = case E of
                    undefined -> {a(L, undefined), St2};
                    _         -> expr(E, Scope, St2)
                end,
    {{'case', L, rt(L, truthy, [CE]),
      [cl(L, [a(L, true)], [], [TE]),
       cl(L, [a(L, false)], [], [EE])]}, St3};
expr({filter, Loc, Name, E, Args}, Scope, St) ->
    L = line(Loc, St),
    {EE, St1} = expr(E, Scope, St),
    {M, F} = filter_mf(Name, Loc, St1),
    {ArgsExpr, St2} = args_map(Name, Args, filter_params(St1), Scope, St1, L),
    {rem_call(L, M, F, [EE, ArgsExpr]), record_remote(M, St2)};
expr({test, Loc, Name, E, Args, Neg}, Scope, St) ->
    L = line(Loc, St),
    {EE, St1} = expr(E, Scope, St),
    {M, F} = test_mf(Name, Loc, St1),
    {ArgsExpr, St2} = args_map(Name, Args, test_params(St1), Scope, St1, L),
    Call = rem_call(L, M, F, [EE, ArgsExpr]),
    {case Neg of true -> {op, L, 'not', Call}; false -> Call end,
     record_remote(M, St2)};
expr({call, Loc, F, Args}, Scope, St) ->
    call_expr({call, Loc, F, Args}, Scope, St, []);
expr({tuple, Loc, Es}, Scope, St) ->
    L = line(Loc, St),
    {Exprs, St1} = exprs(Es, Scope, St),
    {{tuple, L, Exprs}, St1};
expr({list, Loc, Es}, Scope, St) ->
    L = line(Loc, St),
    {Exprs, St1} = exprs(Es, Scope, St),
    {mklist(L, Exprs), St1};
expr({map, Loc, KVs}, Scope, St) ->
    L = line(Loc, St),
    {Fields, St1} =
        lists:mapfoldl(fun({K, V}, S) ->
                               {KE, S1} = map_key_expr(K, Scope, S, L),
                               {VE, S2} = expr(V, Scope, S1),
                               {{map_field_assoc, L, KE, VE}, S2}
                       end, St, KVs),
    {{map, L, Fields}, St1}.

exprs(Es, Scope, St) ->
    lists:mapfoldl(fun(E, S) -> expr(E, Scope, S) end, St, Es).

maybe_expr(undefined, _Scope, St, L) -> {a(L, undefined), St};
maybe_expr(E, Scope, St, _L)          -> expr(E, Scope, St).

%% A literal string key becomes an atom at compile time, matching the engine's
%% map representation without a conversion on every lookup.
map_key_expr({lit, Loc, K}, _Scope, St, _L) when is_binary(K) ->
    {a(line(Loc, St), binary_to_atom(K, utf8)), St};
map_key_expr(K, Scope, St, _L) ->
    expr(K, Scope, St).

binop_call(L, Op, A, B) ->
    Fun = case Op of
              '+' -> add; '-' -> sub; '*' -> mul; '/' -> divide;
              '//' -> floordiv; '%' -> mod; '**' -> pow; '~' -> concat;
              '==' -> eq; '!=' -> ne; '<' -> lt; '<=' -> le;
              '>' -> gt; '>=' -> ge;
              'in' -> contains; 'not in' -> contains
          end,
    Call = case Op of
               'in'     -> rt(L, contains, [B, A]);
               'not in' -> rt(L, contains, [B, A]);
               _        -> rt(L, Fun, [A, B])
           end,
    case Op of
        'not in' -> {op, L, 'not', Call};
        _        -> Call
    end.

%% The left operand is bound before being tested so that it is evaluated once:
%% `{{ f(x) or g(x) }}' must not call f twice.
short_circuit(Loc, A, B, OnTrue, Scope, St) ->
    L = line(Loc, St),
    {AE, St1} = expr(A, Scope, St),
    {BE, St2} = expr(B, Scope, St1),
    {Tmp, St3} = next_temp(St2),
    Bound = {match, L, v(L, Tmp), AE},
    {Then, Else} = case OnTrue of
                       true  -> {v(L, Tmp), BE};   % or
                       false -> {BE, v(L, Tmp)}    % and
                   end,
    {{block, L,
      [{'case', L, rt(L, truthy, [Bound]),
        [cl(L, [a(L, true)], [], [Then]),
         cl(L, [a(L, false)], [], [Else])]}]}, St3}.

%%%===================================================================
%%% Calls
%%%===================================================================

call_expr({call, Loc, {name, _, super}, _Args}, _Scope, St, _Extra) ->
    super_call(Loc, St);
call_expr({call, Loc, {name, _, caller}, {Pos, _, _, _}}, Scope, St, _Extra) ->
    L = line(Loc, St),
    {Exprs, St1} = exprs(Pos, Scope, St),
    {{call, L, rt(L, resolve, [a(L, caller), v(L, Scope)]), [mklist(L, Exprs)]}, St1};
call_expr({call, Loc, {name, _, loop}, {Pos, _, _, _}}, Scope, St, _Extra) ->
    recursive_call(Loc, Pos, Scope, St);
call_expr({call, Loc, {attr, _, {name, _, loop}, cycle}, {Pos, _, _, _}}, Scope, St, _E) ->
    L = line(Loc, St),
    {Exprs, St1} = exprs(Pos, Scope, St),
    Cycle = rt(L, attr, [rt(L, resolve, [a(L, loop), v(L, Scope)]), a(L, cycle)]),
    {{call, L, Cycle, [mklist(L, Exprs)]}, St1};
call_expr({call, Loc, {attr, _, {name, _, loop}, changed}, {Pos, _, _, _}}, Scope, St, _E) ->
    changed_call(Loc, Pos, Scope, St);
call_expr({call, Loc, {attr, _, E, K}, Args}, Scope, St, Extra)
  when K =:= items; K =:= keys; K =:= values ->
    L = line(Loc, St),
    case {Args, Extra} of
        {{[], [], undefined, undefined}, []} ->
            {EE, St1} = expr(E, Scope, St),
            {rt(L, K, [EE]), St1};
        _ ->
            generic_call(Loc, {attr, Loc, E, K}, Args, Scope, St, Extra)
    end;
call_expr({call, Loc, {attr, _, {name, _, Alias}, Macro}, Args}, Scope, St, Extra) ->
    case maps:get(Alias, St#cs.imports, undefined) of
        {module, Mod, WithCtx} ->
            macro_call(Loc, {remote, Mod, macro_fun(Macro)}, Args, Scope, St,
                       Extra, WithCtx);
        _ ->
            generic_call(Loc, {attr, Loc, {name, Loc, Alias}, Macro}, Args, Scope,
                         St, Extra)
    end;
call_expr({call, Loc, {name, _, N}, Args}, Scope, St, Extra) ->
    case resolve_callee(N, St) of
        {imported, {Mod, Fun}, WithCtx} ->
            macro_call(Loc, {remote, Mod, Fun}, Args, Scope, St, Extra, WithCtx);
        {macro, _Params} ->
            macro_call(Loc, {local, macro_fun(N)}, Args, Scope, St, Extra, true);
        builtin ->
            builtin_call(Loc, N, Args, Scope, St);
        none ->
            generic_call(Loc, {name, Loc, N}, Args, Scope, St, Extra)
    end;
call_expr({call, Loc, F, Args}, Scope, St, Extra) ->
    generic_call(Loc, F, Args, Scope, St, Extra).

resolve_callee(N, St) ->
    case maps:get(N, St#cs.imports, undefined) of
        {macro, Mod, Macro, WithCtx} ->
            {imported, {Mod, macro_fun(Macro)}, WithCtx};
        undefined ->
            case lists:keyfind(N, 1, St#cs.macros) of
                {N, Params} -> {macro, Params};
                false ->
                    case ai_jinja_rt:is_builtin(N) of
                        true  -> builtin;
                        false -> none
                    end
            end;
        _Module -> none
    end.

%% A macro call is a direct call, local or remote: no lookup table, no apply.
%% The callee binds its own parameters (see emit_macro/5), which is what makes
%% a cross-module macro call possible at all.
macro_call(Loc, Target, {Pos, Kw, _, _}, Scope, St, Extra, WithCtx) ->
    L = line(Loc, St),
    {PosExprs, St1} = exprs(Pos, Scope, St),
    {KwFields, St2} =
        lists:mapfoldl(fun({N, E}, S) ->
                               {EE, S1} = expr(E, Scope, S),
                               {{map_field_assoc, L, a(L, N), EE}, S1}
                       end, St1, Kw),
    ExtraFields = [{map_field_assoc, L, a(L, K), Val} || {K, Val} <- Extra],
    Args = {map, L, [{map_field_assoc, L, a(L, '$positional'), mklist(L, PosExprs)}]
            ++ KwFields ++ ExtraFields},
    %% A macro sees the template-level scope, never the caller's locals -- and
    %% a macro imported `without context' (the default for {% import %}) sees
    %% no context at all.
    ScopeArg = case WithCtx of
                   true  -> rt(L, globals, [v(L, Scope)]);
                   false -> rt(L, new_scope, [a(L, undefined)])
               end,
    case Target of
        {local, Fun}   -> {{call, L, local_callee(L, Fun, St2), [Args, ScopeArg]}, St2};
        {remote, M, F} -> {rem_call(L, M, F, [Args, ScopeArg]), St2}
    end.

%% In an inline expansion a macro is a bound fun, so the call site names a
%% variable; in a module it names the function.
local_callee(L, Fun, #cs{inline = false}) -> a(L, Fun);
local_callee(L, Fun, #cs{inline = true} = St) -> v(L, inline_var(Fun, St)).

builtin_call(Loc, N, {Pos, Kw, _, _}, Scope, St) ->
    L = line(Loc, St),
    {PosExprs, St1} = exprs(Pos, Scope, St),
    {KwPairs, St2} =
        lists:mapfoldl(fun({K, E}, S) ->
                               {EE, S1} = expr(E, Scope, S),
                               {{tuple, L, [a(L, K), EE]}, S1}
                       end, St1, Kw),
    {rt(L, builtin, [a(L, N), mklist(L, PosExprs), mklist(L, KwPairs)]), St2}.

generic_call(Loc, F, {Pos, Kw, _, _}, Scope, St, _Extra) ->
    L = line(Loc, St),
    {FE, St1} = expr(F, Scope, St),
    {PosExprs, St2} = exprs(Pos, Scope, St1),
    {KwPairs, St3} =
        lists:mapfoldl(fun({K, E}, S) ->
                               {EE, S1} = expr(E, Scope, S),
                               {{tuple, L, [a(L, K), EE]}, S1}
                       end, St2, Kw),
    {rt(L, call, [FE, mklist(L, PosExprs), mklist(L, KwPairs)]), St3}.

super_call(Loc, #cs{block = undefined}) ->
    ?THROW(Loc, {super_outside_block, Loc});
super_call(Loc, #cs{parent = undefined}) ->
    ?THROW(Loc, {super_outside_block, Loc});
super_call(Loc, #cs{block = Name, parent = Parent} = St) ->
    L = line(Loc, St),
    %% The parent block returns rendered output, so `{{ super() }}' must not
    %% escape it again -- nor render it as the iolist it literally is.
    {{tuple, L, [a(L, safe),
                 rem_call(L, Parent, block_fun(Name),
                          [v(L, ?SV), v(L, ?IV), v(L, ?BV)])]}, St}.

recursive_call(Loc, Pos, Scope, #cs{loop = Loop} = St) ->
    L = line(Loc, St),
    case Loop of
        #{rec := {RecVar, DepthVar}} ->
            {Exprs, St1} = exprs(Pos, Scope, St),
            Arg = case Exprs of [E | _] -> E; [] -> {nil, L} end,
            %% Already-rendered output, so it must not be escaped or printed
            %% as the iolist it is.
            {{tuple, L, [a(L, safe),
                         {call, L, v(L, RecVar),
                          [Arg, {op, L, '+', v(L, DepthVar), n(L, 1)}]}]}, St1};
        _ ->
            ?THROW(Loc, {not_callable, loop})
    end.

%% loop.changed(E) means "did E differ from its value last time round". The
%% previous value cannot be produced at run time, because E is an arbitrary
%% expression -- so it is compiled a second time against a scope in which the
%% loop variable is bound to the previous item.
changed_call(Loc, Pos, Scope, #cs{loop = Loop} = St) ->
    L = line(Loc, St),
    case {Loop, Pos} of
        {#{prev_scope := Prev, n := NVar}, [E]} when Prev =/= undefined ->
            {Now, St1}  = expr(E, Scope, St),
            {Before, St2} = expr(E, Prev, St1),
            First = {op, L, '=:=', v(L, NVar), n(L, 1)},
            {rt(L, changed, [Now, Before, First]), St2};
        _ ->
            ?THROW(Loc, {not_callable, changed})
    end.

%%%===================================================================
%%% Filters and tests
%%%===================================================================

filter_mf(Name, Loc, #cs{registry = #{filters := T}}) ->
    case T of
        #{Name := MF} -> MF;
        _             -> ?THROW(Loc, {unknown_filter, Name})
    end.

test_mf(Name, Loc, #cs{registry = #{tests := T}}) ->
    case T of
        #{Name := MF} -> MF;
        _             -> ?THROW(Loc, {unknown_test, Name})
    end.

filter_params(#cs{registry = #{params := P}}) -> P.
test_params(_St) -> ai_jinja_tests:params().

%% Positional arguments are matched to parameter names at COMPILE time, so the
%% generated code builds one map literal and calls the filter directly.
args_map(Name, {Pos, Kw, _, _}, Params, Scope, St, L) ->
    Names = maps:get(Name, Params, []),
    {PosExprs, St1} = exprs(Pos, Scope, St),
    {Bound, Extra} = zip_params(Names, PosExprs),
    {KwFields, St2} =
        lists:mapfoldl(fun({K, E}, S) ->
                               {EE, S1} = expr(E, Scope, S),
                               {{map_field_assoc, L, a(L, K), EE}, S1}
                       end, St1, Kw),
    Fields = [{map_field_assoc, L, a(L, N), E} || {N, E} <- Bound]
        ++ KwFields
        ++ [{map_field_assoc, L, a(L, '$positional'), mklist(L, Extra)}
            || Extra =/= []],
    {{map, L, Fields}, St2}.

zip_params(Names, Exprs) -> zip_params(Names, Exprs, []).
zip_params([], Rest, Acc)          -> {lists:reverse(Acc), Rest};
zip_params(_Names, [], Acc)        -> {lists:reverse(Acc), []};
zip_params([N | Ns], [E | Es], Acc) -> zip_params(Ns, Es, [{N, E} | Acc]).

%% A {% filter %} block and a filtered {% set %} block both apply a chain to
%% an already-rendered body, which the parser represented as a chain over a
%% placeholder name. Substituting the body for that placeholder is all that is
%% left to do here.
apply_body_filter(undefined, BodyExpr, _Scope, St, _L) ->
    {BodyExpr, St};
apply_body_filter(Chain, BodyExpr, Scope, St, _L) ->
    Placeholder = ai_jinja_parser:body_placeholder(),
    {Expr, St1} = expr(Chain, Scope, St),
    {substitute_placeholder(Expr, Placeholder, BodyExpr), St1}.

substitute_placeholder({call, _, {remote, _, {atom, _, ai_jinja_rt},
                                 {atom, _, resolve}},
                        [{atom, _, Name}, _]}, Name, Replacement) ->
    Replacement;
substitute_placeholder(T, Name, Replacement) when is_tuple(T) ->
    list_to_tuple(substitute_placeholder(tuple_to_list(T), Name, Replacement));
substitute_placeholder([H | T], Name, Replacement) ->
    [substitute_placeholder(H, Name, Replacement)
     | substitute_placeholder(T, Name, Replacement)];
substitute_placeholder(Other, _Name, _Replacement) ->
    Other.

%%%===================================================================
%%% Inline restrictions
%%%===================================================================

reject_inline(Nodes) ->
    lists:foreach(fun reject_inline_node/1, Nodes),
    ok.

reject_inline_node(Node) ->
    K = element(1, Node),
    case inline_forbidden(K) of
        true  -> ?THROW(element(2, Node), {target_in_inline_template, K});
        false -> lists:foreach(fun reject_inline/1, ai_jinja_ast:bodies(Node))
    end.

%% Inside an expansion a macro is a bound fun, and a bound fun cannot refer to
%% itself or to one bound after it. Mutual (and self) recursion is therefore
%% impossible here and is reported up front rather than as an unbound variable
%% from the Erlang compiler.
check_inline_macros(Nodes) ->
    Macros = lists:reverse(collect_macros(Nodes, [])),
    Names = [N || {N, _} <- Macros],
    Bad = [{N, Called}
           || {Index, {N, Body}} <- lists:zip(lists:seq(1, length(Macros)), Macros),
              Called <- called_macros(Body, Names),
              index_of(Called, Names) >= Index],
    case Bad of
        [] -> ok;
        _  -> ?THROW({1, 1}, {mutual_macro_in_inline,
                              lists:usort([N || {N, _} <- Bad]
                                          ++ [C || {_, C} <- Bad])})
    end.

collect_macros(Nodes, Acc) ->
    lists:foldl(fun({macro, _, N, _, B}, A) -> collect_macros(B, [{N, B} | A]);
                   (Node, A) ->
                        lists:foldl(fun(B, A2) -> collect_macros(B, A2) end, A,
                                    ai_jinja_ast:bodies(Node))
                end, Acc, Nodes).

called_macros(Body, Names) ->
    lists:usort([N || N <- Names, mentions_call_name(Body, N)]).

mentions_call_name(Term, Name) when is_tuple(Term) ->
    case Term of
        {call, _, {name, _, Name}, _} -> true;
        _ -> mentions_call_name(tuple_to_list(Term), Name)
    end;
mentions_call_name([H | T], Name) ->
    mentions_call_name(H, Name) orelse mentions_call_name(T, Name);
mentions_call_name(_, _) -> false.

index_of(X, L) -> index_of(X, L, 1).
index_of(X, [X | _], N) -> N;
index_of(X, [_ | T], N) -> index_of(X, T, N + 1);
index_of(_X, [], _N)    -> 0.

reject_in_inline(_K, _Loc, #cs{inline = false}) -> ok;
reject_in_inline(K, Loc, #cs{inline = true})    ->
    ?THROW(Loc, {target_in_inline_template, K}).

%%%===================================================================
%%% Helpers
%%%===================================================================

rt(L, F, Args) -> rem_call(L, ai_jinja_rt, F, Args).

%% Turn a value into output. Anything that can produce a `{safe, _}' marker --
%% an interpolation, a {% filter %} block, a {% call %} block -- has to go
%% through here, or the marker tuple reaches iolist_to_binary/1 as data.
emit(L, Expr, #cs{escape = true})  -> rt(L, escape, [Expr]);
emit(L, Expr, #cs{escape = false}) -> rt(L, to_binary, [Expr]).

self_module(#cs{opts = Opts}) -> maps:get(module, Opts, undefined).

record_remote(M, #cs{ext_remotes = R} = St) ->
    case lists:member(M, R) of
        true  -> St;
        false -> St#cs{ext_remotes = [M | R]}
    end.

%% Generated variable names. Index 0 is the bare base, so an ordinary module
%% keeps the readable `V', `V1', `V2'.
scope_var(0, #cs{vsuffix = Sfx}) -> list_to_atom([$V | Sfx]);
scope_var(N, #cs{vsuffix = Sfx}) -> list_to_atom([$V | integer_to_list(N)] ++ Sfx).

next_scope(#cs{counter = C} = St) ->
    {scope_var(C + 1, St), St#cs{counter = C + 1}}.

next_temp(#cs{counter = C, vsuffix = Sfx} = St) ->
    {list_to_atom("T" ++ integer_to_list(C + 1) ++ "__" ++ Sfx),
     St#cs{counter = C + 1}}.

line(_Loc, #cs{line_map = false}) -> anno(0);
line({L, _C}, _St)                -> anno(L);
line(_Other, _St)                 -> anno(0).

line_of({L, _}) -> L;
line_of(L) when is_integer(L) -> L;
line_of(_) -> 1.

%% Does any expression in this body mention the name?
mentions(Term, Name) when is_tuple(Term) ->
    case Term of
        {name, _, Name} -> true;
        _ -> mentions(tuple_to_list(Term), Name)
    end;
mentions([H | T], Name) -> mentions(H, Name) orelse mentions(T, Name);
mentions(_, _)          -> false.

mentions_call(Term, Obj, Field) when is_tuple(Term) ->
    case Term of
        {call, _, {attr, _, {name, _, Obj}, Field}, _} -> true;
        _ -> mentions_call(tuple_to_list(Term), Obj, Field)
    end;
mentions_call([H | T], Obj, Field) ->
    mentions_call(H, Obj, Field) orelse mentions_call(T, Obj, Field);
mentions_call(_, _, _) -> false.
