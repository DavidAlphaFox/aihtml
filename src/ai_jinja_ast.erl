%%%-------------------------------------------------------------------
%%% @doc AST post-processing passes for the jinja engine.
%%%
%%% Four pure passes in a fixed order:
%%%
%%%   merge_text -> fold_constants -> merge_text -> resolve_targets
%%%
%%% merge_text runs twice on purpose: eliminating a dead `{% if false %}' can
%%% leave two literals adjacent that were not adjacent before, and merging
%%% them is what puts one binary in the module's literal pool instead of two.
%%%
%%% resolve_targets goes last because it is the only pass that can fail;
%%% running it first would throw away the other passes' work on the way to
%%% the same error.
%%%
%%% Constant folding evaluates through ai_jinja_rt, never through a second
%%% implementation. If the two disagreed, `{{ 1 + 2 }}' and `{{ a + b }}'
%%% would answer differently for the same values.
%%% @end
%%%-------------------------------------------------------------------
-module(ai_jinja_ast).

-include("ai_jinja.hrl").

-export([postprocess/2, merge_text/1, fold_constants/1, resolve_targets/2]).
-export([module_name/2, default_prefix/0, map_body/2, bodies/1]).
-export([fold_expr/1]).

-type nodes() :: [ai_jinja_node()].

%% Every builtin filter is a pure function of its arguments except this one,
%% so it is the only one the folder must leave alone.
-define(IMPURE_FILTERS, [random]).

%%%===================================================================
%%% API
%%%===================================================================

-spec postprocess(nodes(), map()) ->
          {ok, nodes(), [module()]} | ai_jinja_error().
postprocess(Nodes0, Opts0) ->
    Opts = ai_html_text:opts(Opts0),
    Nodes = merge_text(fold_constants(merge_text(Nodes0))),
    case resolve_targets(Nodes, Opts) of
        {error, _} = E    -> E;
        {ok, Out, Deps}   -> {ok, Out, Deps}
    end.

-spec default_prefix() -> binary().
default_prefix() -> ?AI_JINJA_DEFAULT_PREFIX.

%% @doc Template name -> generated module name.
%%
%% Unlike a mustache partial, a jinja template is referred to WITH its
%% extension (`{% extends "layout/base.j2" %}'), so the suffix is stripped
%% before the name is flattened.
%%
%%   <<"index.j2">>          -> j2_index
%%   <<"layout/base.j2">>    -> j2_layout_base
-spec module_name(binary(), map()) -> module().
module_name(Name, Opts) ->
    Prefix = maps:get(prefix, Opts, default_prefix()),
    Suffix = maps:get(suffix, Opts, ?AI_JINJA_DEFAULT_SUFFIX),
    Base = strip_suffix(Name, Suffix),
    Flat = << <<(norm_char(C))>> || <<C>> <= Base >>,
    binary_to_atom(<<Prefix/binary, Flat/binary>>, utf8).

strip_suffix(Name, Suffix) ->
    NS = byte_size(Suffix),
    case byte_size(Name) > NS andalso binary:part(Name, byte_size(Name) - NS, NS) =:= Suffix of
        true  -> binary:part(Name, 0, byte_size(Name) - NS);
        false -> Name
    end.

norm_char($/) -> $_;
norm_char($-) -> $_;
norm_char($.) -> $_;
norm_char(C)  -> C.

%%%===================================================================
%%% Traversal
%%%===================================================================

%% The single place that knows which nodes carry bodies. A new node type only
%% has to be added here and in bodies/1.
-spec map_body(fun((nodes()) -> nodes()), ai_jinja_node()) -> ai_jinja_node().
map_body(F, {'if', L, Branches, Else}) ->
    {'if', L, [{C, F(B)} || {C, B} <- Branches], F(Else)};
map_body(F, {'for', L, T, I, Filt, R, B, E}) ->
    {'for', L, T, I, Filt, R, F(B), F(E)};
map_body(F, {set_block, L, N, B, Filt}) -> {set_block, L, N, F(B), Filt};
map_body(F, {with, L, Bs, B})           -> {with, L, Bs, F(B)};
map_body(F, {filter, L, E, B})          -> {filter, L, E, F(B)};
map_body(F, {block, L, N, S, R, B})     -> {block, L, N, S, R, F(B)};
map_body(F, {macro, L, N, P, B})        -> {macro, L, N, P, F(B)};
map_body(F, {call, L, P, E, B})         -> {call, L, P, E, F(B)};
map_body(_F, Node)                      -> Node.

-spec bodies(ai_jinja_node()) -> [nodes()].
bodies({'if', _, Branches, Else})    -> [B || {_, B} <- Branches] ++ [Else];
bodies({'for', _, _, _, _, _, B, E}) -> [B, E];
bodies({set_block, _, _, B, _})      -> [B];
bodies({with, _, _, B})              -> [B];
bodies({filter, _, _, B})            -> [B];
bodies({block, _, _, _, _, B})       -> [B];
bodies({macro, _, _, _, B})          -> [B];
bodies({call, _, _, _, B})           -> [B];
bodies(_)                            -> [].

%%%===================================================================
%%% Pass 1: merge adjacent text
%%%===================================================================

-spec merge_text(nodes()) -> nodes().
merge_text(Nodes) -> merge(Nodes, [], []).

merge([], Pending, Acc) ->
    lists:reverse(flush(Pending, Acc));
merge([{text, Loc, Bin} | Rest], Pending, Acc) ->
    merge(Rest, [{Loc, Bin} | Pending], Acc);
merge([Node | Rest], Pending, Acc) ->
    merge(Rest, [], [map_body(fun merge_text/1, Node) | flush(Pending, Acc)]).

flush([], Acc) -> Acc;
flush(Pending, Acc) ->
    [{Loc, _} | _] = Rev = lists:reverse(Pending),
    [{text, Loc, iolist_to_binary([B || {_, B} <- Rev])} | Acc].

%%%===================================================================
%%% Pass 2: constant folding and dead branches
%%%===================================================================

-spec fold_constants(nodes()) -> nodes().
fold_constants(Nodes) -> lists:append([fold_node(N) || N <- Nodes]).

fold_node({output, L, E}) ->
    [{output, L, fold_expr(E)}];
fold_node({do, L, E}) ->
    [{do, L, fold_expr(E)}];
fold_node({'if', L, Branches0, Else0}) ->
    Branches = [{fold_expr(C), fold_constants(B)} || {C, B} <- Branches0],
    Else = fold_constants(Else0),
    prune_branches(L, Branches, Else, []);
fold_node({'for', L, T, I, Filt, R, B, E}) ->
    [{'for', L, T, fold_expr(I), fold_maybe(Filt), R,
      fold_constants(B), fold_constants(E)}];
fold_node({set, L, N, E}) ->
    [{set, L, N, fold_expr(E)}];
fold_node({with, L, Bs, B}) ->
    [{with, L, [{N, fold_expr(E)} || {N, E} <- Bs], fold_constants(B)}];
fold_node(Node) ->
    [map_body(fun fold_constants/1, Node)].

fold_maybe(undefined) -> undefined;
fold_maybe(E)         -> fold_expr(E).

%% A branch whose condition is a literal is decided here: a false one is
%% dropped outright, and a true one ends the chain, since nothing after it can
%% ever run.
prune_branches(L, [], Else, []) ->
    _ = L, Else;
prune_branches(L, [], Else, Kept) ->
    [{'if', L, lists:reverse(Kept), Else}];
prune_branches(L, [{{lit, _, V}, Body} | Rest], Else, Kept) ->
    case ai_jinja_rt:truthy(V) of
        true when Kept =:= [] -> Body;
        true                  -> [{'if', L, lists:reverse(Kept), Body}];
        false                 -> prune_branches(L, Rest, Else, Kept)
    end;
prune_branches(L, [Branch | Rest], Else, Kept) ->
    prune_branches(L, Rest, Else, [Branch | Kept]).

%%%===================================================================
%%% Expression folding
%%%===================================================================

-spec fold_expr(ai_jinja_expr()) -> ai_jinja_expr().
fold_expr({binop, L, Op, A0, B0}) ->
    A = fold_expr(A0), B = fold_expr(B0),
    case {A, B} of
        {{lit, _, VA}, {lit, _, VB}} -> try_lit(L, fun() -> ai_jinja_rt:binop(Op, VA, VB) end,
                                                {binop, L, Op, A, B});
        _ -> {binop, L, Op, A, B}
    end;
fold_expr({unop, L, 'not', E0}) ->
    case fold_expr(E0) of
        {lit, _, V} -> {lit, L, not ai_jinja_rt:truthy(V)};
        E           -> {unop, L, 'not', E}
    end;
fold_expr({unop, L, Op, E0}) ->
    case fold_expr(E0) of
        {lit, _, V} when is_number(V) ->
            {lit, L, case Op of '-' -> -V; '+' -> V end};
        E -> {unop, L, Op, E}
    end;
fold_expr({'and', L, A0, B0}) ->
    A = fold_expr(A0), B = fold_expr(B0),
    case A of
        {lit, _, V} -> case ai_jinja_rt:truthy(V) of true -> B; false -> A end;
        _           -> {'and', L, A, B}
    end;
fold_expr({'or', L, A0, B0}) ->
    A = fold_expr(A0), B = fold_expr(B0),
    case A of
        {lit, _, V} -> case ai_jinja_rt:truthy(V) of true -> A; false -> B end;
        _           -> {'or', L, A, B}
    end;
fold_expr({'cond', L, C0, T0, E0}) ->
    C = fold_expr(C0), T = fold_expr(T0),
    E = case E0 of undefined -> undefined; _ -> fold_expr(E0) end,
    case C of
        {lit, _, V} ->
            case {ai_jinja_rt:truthy(V), E} of
                {true, _}          -> T;
                {false, undefined} -> {lit, L, undefined};
                {false, _}         -> E
            end;
        _ -> {'cond', L, C, T, E}
    end;
fold_expr({filter, L, Name, E0, Args0}) ->
    E = fold_expr(E0),
    Args = fold_args(Args0),
    case foldable_filter(Name, E, Args) of
        true  -> try_lit(L, fun() -> eval_filter(Name, E, Args) end,
                         {filter, L, Name, E, Args});
        false -> {filter, L, Name, E, Args}
    end;
fold_expr({test, L, Name, E0, Args0, Neg}) ->
    {test, L, Name, fold_expr(E0), fold_args(Args0), Neg};
fold_expr({call, L, F0, Args0}) ->
    {call, L, fold_expr(F0), fold_args(Args0)};
fold_expr({attr, L, E0, K}) ->
    {attr, L, fold_expr(E0), K};
fold_expr({sub, L, E0, I0}) ->
    {sub, L, fold_expr(E0), fold_expr(I0)};
fold_expr({slice, L, E0, A0, B0, C0}) ->
    {slice, L, fold_expr(E0), fold_maybe(A0), fold_maybe(B0), fold_maybe(C0)};
fold_expr({list, L, Es0}) ->
    Es = [fold_expr(E) || E <- Es0],
    case all_lit(Es) of
        true  -> {lit, L, [V || {lit, _, V} <- Es]};
        false -> {list, L, Es}
    end;
fold_expr({tuple, L, Es0}) ->
    Es = [fold_expr(E) || E <- Es0],
    case all_lit(Es) of
        true  -> {lit, L, list_to_tuple([V || {lit, _, V} <- Es])};
        false -> {tuple, L, Es}
    end;
fold_expr({map, L, KVs0}) ->
    KVs = [{fold_expr(K), fold_expr(V)} || {K, V} <- KVs0],
    Flat = lists:append([[K, V] || {K, V} <- KVs]),
    case all_lit(Flat) of
        true  -> {lit, L, maps:from_list([{map_key(K), V}
                                          || {{lit, _, K}, {lit, _, V}} <- KVs])};
        false -> {map, L, KVs}
    end;
fold_expr(E) ->
    E.

%% A literal map written in a template has string keys; the engine's maps are
%% keyed by atom, so the key is converted once, here, rather than on every
%% lookup at run time.
map_key(K) when is_binary(K) -> binary_to_atom(K, utf8);
map_key(K)                   -> K.

fold_args({Pos, Kw, S, D}) ->
    {[fold_expr(E) || E <- Pos],
     [{N, fold_expr(E)} || {N, E} <- Kw],
     fold_maybe(S), fold_maybe(D)}.

all_lit(Es) -> lists:all(fun({lit, _, _}) -> true; (_) -> false end, Es).

foldable_filter(Name, {lit, _, _}, {Pos, Kw, undefined, undefined}) ->
    not lists:member(Name, ?IMPURE_FILTERS)
        andalso all_lit(Pos) andalso all_lit([E || {_, E} <- Kw])
        andalso ai_jinja_ext:lookup(filter, Name) =/= error;
foldable_filter(_Name, _E, _Args) ->
    false.

eval_filter(Name, {lit, _, V}, {Pos, Kw, _, _}) ->
    {ok, {M, F}} = ai_jinja_ext:lookup(filter, Name),
    Params = maps:get(Name, ai_jinja_filters:params(), []),
    M:F(V, ai_jinja_args:build(Params, [X || {lit, _, X} <- Pos],
                               [{N, X} || {N, {lit, _, X}} <- Kw])).

%% Folding must never turn a run-time error into a build failure: a template
%% is allowed to contain `{{ 1 / 0 }}' inside a branch that never runs.
try_lit(L, Fun, Fallback) ->
    try {lit, L, Fun()}
    catch _:_ -> Fallback end.

%%%===================================================================
%%% Pass 3: resolve include/extends/import/from targets
%%%===================================================================

-spec resolve_targets(nodes(), map()) ->
          {ok, nodes(), [module()]} | ai_jinja_error().
resolve_targets(Nodes, Opts) ->
    try
        {Out, Deps} = resolve(Nodes, Opts, []),
        {ok, Out, lists:usort(Deps)}
    catch
        throw:{ai_jinja_ast, Loc, Reason} ->
            {error, {ai_html_text:source(Opts), element(1, Loc), Reason}}
    end.

resolve(Nodes, Opts, Deps) ->
    lists:foldr(fun(N, {Acc, D}) ->
                        {N1, D1} = resolve_node(N, Opts, D),
                        {[N1 | Acc], D1}
                end, {[], Deps}, Nodes).

resolve_node({include, L, T, Ign, With}, Opts, Deps) ->
    {Mod, Deps1} = target(T, L, Opts, Deps, Ign),
    {{include, L, Mod, Ign, With}, Deps1};
resolve_node({extends, L, T}, Opts, Deps) ->
    {Mod, Deps1} = target(T, L, Opts, Deps, false),
    case Mod =:= maps:get(module, Opts, undefined) of
        true  -> throw({ai_jinja_ast, L, {extends_cycle, [Mod]}});
        false -> {{extends, L, Mod}, Deps1}
    end;
resolve_node({import, L, T, As, With}, Opts, Deps) ->
    {Mod, Deps1} = target(T, L, Opts, Deps, false),
    {{import, L, Mod, As, With}, Deps1};
resolve_node({from, L, T, Names, With}, Opts, Deps) ->
    {Mod, Deps1} = target(T, L, Opts, Deps, false),
    {{from, L, Mod, Names, With}, Deps1};
resolve_node(Node, Opts, Deps) ->
    resolve_bodies(Node, Opts, Deps).

resolve_bodies(Node, Opts, Deps) ->
    %% map_body/2 cannot thread the dependency list, so the bodies are
    %% resolved first and then substituted back in order.
    Bs = bodies(Node),
    {Resolved, Deps1} =
        lists:foldl(fun(B, {Acc, D}) ->
                            {B1, D1} = resolve(B, Opts, D),
                            {[B1 | Acc], D1}
                    end, {[], Deps}, Bs),
    Ordered = lists:reverse(Resolved),
    {substitute(Node, Ordered), Deps1}.

substitute(Node, Ordered) ->
    {Out, []} = do_substitute(Node, Ordered),
    Out.

do_substitute({'if', L, Branches, _Else}, Bodies) ->
    {BranchBodies, [Else | Rest]} = lists:split(length(Branches), Bodies),
    {{'if', L, lists:zipwith(fun({C, _}, B) -> {C, B} end, Branches, BranchBodies),
      Else}, Rest};
do_substitute({'for', L, T, I, F, R, _, _}, [B, E | Rest]) ->
    {{'for', L, T, I, F, R, B, E}, Rest};
do_substitute({set_block, L, N, _, F}, [B | Rest]) -> {{set_block, L, N, B, F}, Rest};
do_substitute({with, L, Bs, _}, [B | Rest])        -> {{with, L, Bs, B}, Rest};
do_substitute({filter, L, E, _}, [B | Rest])       -> {{filter, L, E, B}, Rest};
do_substitute({block, L, N, S, R, _}, [B | Rest])  -> {{block, L, N, S, R, B}, Rest};
do_substitute({macro, L, N, P, _}, [B | Rest])     -> {{macro, L, N, P, B}, Rest};
do_substitute({call, L, P, E, _}, [B | Rest])      -> {{call, L, P, E, B}, Rest};
do_substitute(Node, Bodies)                        -> {Node, Bodies}.

%% A literal name becomes a module; anything else is a dynamic target, which
%% this engine does not support (deviation J11). The alternative would be
%% binary_to_atom/2 on template data or a run-time registry, and both are ruled
%% out by the architecture.
target(Name, L, Opts, Deps, IgnoreMissing) when is_binary(Name) ->
    Mod = module_name(Name, Opts),
    case IgnoreMissing orelse exists(Name, Opts) of
        true  -> {Mod, [Mod | Deps]};
        false -> throw({ai_jinja_ast, L, {template_not_found, Name}})
    end;
target(_Expr, L, _Opts, _Deps, _Ign) ->
    throw({ai_jinja_ast, L, {dynamic_target_unsupported, L}}).

%% Existence is only checked when the caller said where templates live. An
%% ad-hoc set passes them inline; a build passes a views root; the inline
%% template path passes neither and rejects targets outright.
exists(Name, #{templates := T}) ->
    maps:is_key(Name, T);
exists(Name, Opts) ->
    case maps:get(views_abs, Opts, maps:get(views, Opts, undefined)) of
        undefined -> true;
        Views     -> filelib:is_regular(filename:join(Views, Name))
    end.
