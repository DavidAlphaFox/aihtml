%%%-------------------------------------------------------------------
%%% @doc Statement parser: scanner tokens -> jinja AST.
%%%
%%% Bodies are parsed recursively, each call knowing which keywords end it:
%%% `{% if %}' parses its body with the terminator set `[elif, else, endif]'
%%% and dispatches on whichever one it hit. That keeps the block stack in the
%%% Erlang call stack, where the nesting already lives, instead of in a
%%% separate structure that has to be kept in step with it.
%%%
%%% Every diagnostic in designs/09-jinja-syntax.md section 4.4 is produced
%%% here: a mismatched or missing end, an orphan `{% else %}', a duplicate
%%% block name, a statement this engine does not implement.
%%%
%%% Warnings are NOT returned from parse/2 -- that signature belongs to
%%% ai_html_engine. They are computed on the finished AST by warnings/1, which
%%% the plugin and the parse_transform call.
%%% @end
%%%-------------------------------------------------------------------
-module(ai_jinja_parser).

-include("ai_jinja.hrl").

-export([parse/2, warnings/1]).
-export([body_placeholder/0]).

-type nodes() :: [ai_jinja_node()].
-type loc()   :: ai_html_loc().

%% The operand a {% filter %} block's chain is applied to. The compiler
%% substitutes the rendered body for it. Using a name the lexer can produce
%% but a template author cannot write (it would have to be spelled literally)
%% keeps the filter chain an ordinary expression, so `{% filter a|b(1) %}'
%% needs no special parsing at all.
-define(BODY_VAR, '__filter_body__').

-define(THROW(Loc, Reason), throw({ai_jinja_parse, Loc, Reason})).

%%%===================================================================
%%% API
%%%===================================================================

-spec parse(unicode:chardata(), map()) -> {ok, nodes()} | ai_jinja_error().
parse(Body, Opts) ->
    case ai_jinja_scanner:scan(Body, Opts) of
        {error, _} = E -> E;
        {ok, Toks} ->
            try
                {Nodes, Rest} = body(Toks, [], root, {1, 1}),
                [] = Rest,
                ok = check_duplicate_blocks(Nodes),
                {ok, Nodes}
            catch
                throw:{ai_jinja_parse, {L, _C}, Reason} ->
                    {error, {ai_html_text:source(Opts), L, Reason}}
            end
    end.

-spec body_placeholder() -> atom().
body_placeholder() -> ?BODY_VAR.

%%%===================================================================
%%% Bodies
%%%===================================================================

%% Parse until one of `Stops' or the end of input. Returns the nodes plus the
%% remaining tokens, whose head is the terminating statement (or nothing).
%%
%% The FIRST entry of Stops must be the real closing keyword; the others are
%% mid-block clauses such as `elif'. Both diagnostics depend on that order --
%% an unexpected end reports what was expected, and running out of input
%% reports what was left open.
-spec body([ai_jinja_token()], [atom()], atom(), loc()) ->
          {nodes(), [ai_jinja_token()]}.
body(Toks, Stops, Owner, OpenLoc) -> body(Toks, Stops, Owner, OpenLoc, []).

body([], [], _Owner, _OpenLoc, Acc) ->
    {lists:reverse(Acc), []};
body([], Stops, Owner, OpenLoc, _Acc) ->
    _ = Stops,
    ?THROW(OpenLoc, {unclosed_block, Owner, OpenLoc});
body([{stmt, Loc, Kw, _} | _] = Toks, Stops, Owner, OpenLoc, Acc) ->
    case lists:member(Kw, Stops) of
        true  -> {lists:reverse(Acc), Toks};
        false ->
            {node, Node, Rest} = statement(Kw, Loc, Toks, Stops),
            body(Rest, Stops, Owner, OpenLoc, [Node | Acc])
    end;
body([{text, Loc, Bin} | Rest], Stops, Owner, OpenLoc, Acc) ->
    body(Rest, Stops, Owner, OpenLoc, [{text, Loc, Bin} | Acc]);
body([{expr, Loc, ETs} | Rest], Stops, Owner, OpenLoc, Acc) ->
    body(Rest, Stops, Owner, OpenLoc, [{output, Loc, expr_all(ETs, Loc)} | Acc]).

%%%===================================================================
%%% Statement dispatch
%%%===================================================================

statement(Kw, Loc, [{stmt, _, _, Args} | Rest], Stops) ->
    case Kw of
        'if'    -> if_stmt(Loc, Args, Rest);
        'for'   -> for_stmt(Loc, Args, Rest);
        set     -> set_stmt(Loc, Args, Rest);
        with    -> with_stmt(Loc, Args, Rest);
        filter  -> filter_stmt(Loc, Args, Rest);
        include -> include_stmt(Loc, Args, Rest);
        do      -> {node, {do, Loc, expr_all(Args, Loc)}, Rest};
        extends -> extends_stmt(Loc, Args, Rest);
        block   -> block_stmt(Loc, Args, Rest);
        macro   -> macro_stmt(Loc, Args, Rest);
        call    -> call_stmt(Loc, Args, Rest);
        import  -> import_stmt(Loc, Args, Rest);
        from    -> from_stmt(Loc, Args, Rest);
        _       -> unknown(Kw, Loc, Stops)
    end.

%% A terminator that nobody is waiting for. The two shapes get different
%% reasons because they mean different mistakes: `{% endfor %}' inside an
%% `{% if %}' is a mismatched end, while a bare `{% else %}' at the top level
%% is an orphan clause.
unknown(Kw, Loc, Stops) ->
    case {is_clause(Kw), is_end(Kw), Stops} of
        %% An `{% else %}' where none is expected is an orphan clause whatever
        %% block we are inside; only a closing keyword can be a MISmatch.
        {true, _, _}        -> ?THROW(Loc, {orphan_clause, Kw});
        {_, true, []}       -> ?THROW(Loc, {orphan_clause, Kw});
        {_, true, [S | _]}  -> ?THROW(Loc, {mismatched_end, S, Kw});
        {_, _, _}           -> ?THROW(Loc, {unknown_statement, Kw})
    end.

is_clause(K) -> lists:member(K, ['else', 'elif']).

is_end(K) ->
    lists:member(K, [endif, endfor, endset, endwith, endfilter,
                     endblock, endmacro, endcall, endraw]).

%%%===================================================================
%%% if
%%%===================================================================

if_stmt(Loc, Args, Rest) ->
    {Branches, Else, Rest1} = if_branches(Loc, Args, Rest, []),
    {node, {'if', Loc, Branches, Else}, Rest1}.

if_branches(Loc, Args, Toks, Acc) ->
    Cond = expr_all(Args, Loc),
    {Body, Rest} = body(Toks, [endif, 'elif', 'else'], 'if', Loc),
    Acc1 = [{Cond, Body} | Acc],
    case Rest of
        [{stmt, L2, 'elif', A2} | R2] ->
            if_branches(L2, A2, R2, Acc1);
        [{stmt, _, 'else', _} | R2] ->
            {ElseBody, R3} = body(R2, [endif], 'if', Loc),
            {lists:reverse(Acc1), ElseBody, expect_end(R3, endif, Loc)};
        [{stmt, _, endif, _} | R2] ->
            {lists:reverse(Acc1), [], R2};
        [] ->
            ?THROW(Loc, {unclosed_block, 'if', Loc})
    end.

expect_end([{stmt, _, Kw, _} | R], Kw, _Loc) -> R;
expect_end([{stmt, L, Got, _} | _], Kw, _Loc) -> ?THROW(L, {mismatched_end, Kw, Got});
expect_end([], Kw, Loc)                       -> ?THROW(Loc, {unclosed_block, Kw, Loc}).

%%%===================================================================
%%% for
%%%===================================================================

for_stmt(Loc, Args, Toks) ->
    {Targets, R1} = for_targets(Args, Loc, []),
    {IterToks, R2} = split_for_tail(R1),
    Iter = expr_all(IterToks, Loc),
    {Filter, Recursive} = for_modifiers(R2, Loc),
    {Body, Rest} = body(Toks, [endfor, 'else'], 'for', Loc),
    case Rest of
        [{stmt, _, 'else', _} | R3] ->
            {ElseBody, R4} = body(R3, [endfor], 'for', Loc),
            {node, {'for', Loc, Targets, Iter, Filter, Recursive, Body, ElseBody},
             expect_end(R4, endfor, Loc)};
        _ ->
            {node, {'for', Loc, Targets, Iter, Filter, Recursive, Body, []},
             expect_end(Rest, endfor, Loc)}
    end.

%% Flat name lists only. Nested destructuring is a deliberate omission
%% (deviation J15) and is rejected here rather than half-supported.
for_targets([{name, _, N}, {op, _, ','} | R], Loc, Acc) ->
    for_targets(R, Loc, [N | Acc]);
for_targets([{name, _, N}, {kw, _, 'in'} | R], _Loc, Acc) ->
    {lists:reverse([N | Acc]), R};
for_targets([T | _], _Loc, _Acc) ->
    ?THROW(element(2, T), {unexpected_token, token_text(T)});
for_targets([], Loc, _Acc) ->
    ?THROW(Loc, {unexpected_token, <<"end of statement">>}).

%% The iterable runs to the `if' or `recursive' modifier, if any. Both are
%% found at bracket depth zero so that `{% for x in f(a if b else c) %}'
%% is not cut in half.
split_for_tail(Toks) ->
    case find_top(Toks, fun({kw, _, 'if'}) -> true;
                           ({name, _, recursive}) -> true;
                           (_) -> false
                        end) of
        none    -> {Toks, []};
        {N, _T} -> lists:split(N, Toks)
    end.

for_modifiers([], _Loc) ->
    {undefined, false};
for_modifiers([{name, _, recursive}], _Loc) ->
    {undefined, true};
for_modifiers([{kw, L, 'if'} | R], _Loc) ->
    case lists:reverse(R) of
        [{name, _, recursive} | RevRest] ->
            {expr_all(lists:reverse(RevRest), L), true};
        _ ->
            {expr_all(R, L), false}
    end;
for_modifiers([T | _], _Loc) ->
    ?THROW(element(2, T), {unexpected_token, token_text(T)}).

%%%===================================================================
%%% set / with / filter / do
%%%===================================================================

set_stmt(Loc, Args, Toks) ->
    case find_top(Args, fun({op, _, '='}) -> true; (_) -> false end) of
        {N, _} ->
            {Lhs, [_Eq | Rhs]} = lists:split(N, Args),
            {node, {set, Loc, set_target(Lhs, Loc), expr_all(Rhs, Loc)}, Toks};
        none ->
            %% Block form, optionally with a filter chain: {% set x | upper %}
            {Name, Filter} = set_block_head(Args, Loc),
            {Body, Rest} = body(Toks, [endset], set, Loc),
            {node, {set_block, Loc, Name, Body, Filter},
             expect_end(Rest, endset, Loc)}
    end.

set_target(Toks, Loc) ->
    case expr_all(Toks, Loc) of
        {name, _, N}       -> N;
        {attr, _, E, K}    -> {attr, E, K};
        Other              -> ?THROW(loc_of(Other, Loc),
                                     {unexpected_token, <<"assignment target">>})
    end.

set_block_head([{name, _, N}], _Loc) ->
    {N, undefined};
set_block_head([{name, _, N}, {op, L, '|'} | Rest], _Loc) ->
    {N, filter_chain_expr(Rest, L)};
set_block_head([T | _], _Loc) ->
    ?THROW(element(2, T), {unexpected_token, token_text(T)});
set_block_head([], Loc) ->
    ?THROW(Loc, {unexpected_token, <<"end of statement">>}).

with_stmt(Loc, Args, Toks) ->
    Bindings = with_bindings(Args, Loc),
    {Body, Rest} = body(Toks, [endwith], with, Loc),
    {node, {with, Loc, Bindings, Body}, expect_end(Rest, endwith, Loc)}.

with_bindings([], _Loc) -> [];
with_bindings(Args, Loc) ->
    [binding(Part, Loc) || Part <- split_top(Args, fun({op, _, ','}) -> true;
                                                      (_) -> false end)].

binding(Toks, Loc) ->
    case find_top(Toks, fun({op, _, '='}) -> true; (_) -> false end) of
        {N, _} ->
            {[{name, _, Name}], [_ | Rhs]} = {lists:sublist(Toks, N), lists:nthtail(N, Toks)},
            {Name, expr_all(Rhs, Loc)};
        none ->
            ?THROW(Loc, {unexpected_token, <<"expected name = expression">>})
    end.

filter_stmt(Loc, Args, Toks) ->
    Chain = filter_chain_expr(Args, Loc),
    {Body, Rest} = body(Toks, [endfilter], filter, Loc),
    {node, {filter, Loc, Chain, Body}, expect_end(Rest, endfilter, Loc)}.

%% Build `__filter_body__ | <chain>' and parse it as one expression, so a
%% chain with arguments needs no parsing code of its own.
filter_chain_expr(Toks, Loc) ->
    expr_all([{name, Loc, ?BODY_VAR}, {op, Loc, '|'} | Toks], Loc).

%%%===================================================================
%%% include / extends / import / from
%%%===================================================================

include_stmt(Loc, Args, Toks) ->
    {Target, R1} = target(Args, Loc),
    {Ignore, R2} = flag(R1, [ignore, missing]),
    %% include passes the context by default; import and from do not. That
    %% asymmetry is the reference implementation's, and it is the sort of
    %% default that is easier to get right once here than to remember.
    {With, R3}   = context_flag(R2, true),
    ok = nothing_left(R3),
    {node, {include, Loc, Target, Ignore, With}, Toks}.

extends_stmt(Loc, Args, Toks) ->
    {Target, R1} = target(Args, Loc),
    ok = nothing_left(R1),
    {node, {extends, Loc, Target}, Toks}.

import_stmt(Loc, Args, Toks) ->
    {Target, R1} = target(Args, Loc),
    case R1 of
        [{name, _, as}, {name, _, Alias} | R2] ->
            {With, R3} = context_flag(R2, false),
            ok = nothing_left(R3),
            {node, {import, Loc, Target, Alias, With}, Toks};
        _ ->
            ?THROW(Loc, {unexpected_token, <<"expected: as <name>">>})
    end.

from_stmt(Loc, Args, Toks) ->
    {Target, R1} = target(Args, Loc),
    case R1 of
        [{name, _, import} | R2] ->
            {Names, R3} = import_names(R2, Loc, []),
            {With, R4} = context_flag(R3, false),
            ok = nothing_left(R4),
            {node, {from, Loc, Target, Names, With}, Toks};
        _ ->
            ?THROW(Loc, {unexpected_token, <<"expected: import <name>">>})
    end.

import_names([{name, _, N}, {name, _, as}, {name, _, A} | R], Loc, Acc) ->
    import_names_sep(R, Loc, [{N, A} | Acc]);
import_names([{name, _, N} | R], Loc, Acc) ->
    import_names_sep(R, Loc, [{N, N} | Acc]);
import_names([T | _], _Loc, _Acc) ->
    ?THROW(element(2, T), {unexpected_token, token_text(T)});
import_names([], Loc, _Acc) ->
    ?THROW(Loc, {unexpected_token, <<"expected a macro name">>}).

import_names_sep([{op, _, ','} | R], Loc, Acc) -> import_names(R, Loc, Acc);
import_names_sep(R, _Loc, Acc)                 -> {lists:reverse(Acc), R}.

%% A literal path becomes a binary the AST pass resolves to a module. Anything
%% else is a dynamic target, kept as an expression so the rejection in
%% ai_jinja_ast has something to match on (deviation J11).
target([{str, _, Path} | R], _Loc) ->
    {Path, R};
target(Toks, Loc) ->
    case find_top(Toks, fun({name, _, N}) ->
                                lists:member(N, [ignore, as, import, with, without]);
                           (_) -> false
                        end) of
        none    -> {expr_all(Toks, Loc), []};
        {0, _}  -> ?THROW(Loc, {unexpected_token, <<"expected a template name">>});
        {N, _}  -> {Head, Tail} = lists:split(N, Toks),
                   {expr_all(Head, Loc), Tail}
    end.

flag([{name, _, A}, {name, _, B} | R], [A, B]) -> {true, R};
flag(R, _)                                     -> {false, R}.

context_flag([{name, _, with}, {name, _, context} | R], _D)    -> {true, R};
context_flag([{name, _, without}, {name, _, context} | R], _D) -> {false, R};
context_flag(R, Default)                                       -> {Default, R}.

nothing_left([])      -> ok;
nothing_left([T | _]) -> ?THROW(element(2, T), {unexpected_token, token_text(T)}).

%%%===================================================================
%%% block / macro / call
%%%===================================================================

block_stmt(Loc, Args, Toks) ->
    {Name, R1} = block_name(Args, Loc),
    {Scoped, R2}   = word(R1, scoped),
    {Required, R3} = word(R2, required),
    ok = nothing_left(R3),
    {Body, Rest} = body(Toks, [endblock], block, Loc),
    Rest1 = close_block(Rest, Name, Loc),
    {node, {block, Loc, Name, Scoped, Required, Body}, Rest1}.

block_name([{name, _, N} | R], _Loc) -> {N, R};
block_name([T | _], _Loc)            -> ?THROW(element(2, T), {unexpected_token, token_text(T)});
block_name([], Loc)                  -> ?THROW(Loc, {unexpected_token, <<"expected a block name">>}).

%% `{% endblock title %}' must name the block it closes, when it names one at
%% all: a mismatch is nearly always a copy-paste error in a long template.
close_block([{stmt, L, endblock, [{name, _, Other}]} | _R], Name, _Loc)
  when Other =/= Name ->
    ?THROW(L, {block_name_mismatch, Name, Other});
close_block([{stmt, _, endblock, _} | R], _Name, _Loc) ->
    R;
close_block(Rest, _Name, Loc) ->
    expect_end(Rest, endblock, Loc).

word([{name, _, W} | R], W) -> {true, R};
word(R, _W)                 -> {false, R}.

macro_stmt(Loc, Args, Toks) ->
    {Name, Params} = macro_head(Args, Loc),
    {Body, Rest} = body(Toks, [endmacro], macro, Loc),
    {node, {macro, Loc, Name, Params, Body}, expect_end(Rest, endmacro, Loc)}.

macro_head([{name, _, Name} | R], Loc) ->
    {Name, params(R, Loc)};
macro_head([T | _], _Loc) ->
    ?THROW(element(2, T), {unexpected_token, token_text(T)});
macro_head([], Loc) ->
    ?THROW(Loc, {unexpected_token, <<"expected a macro name">>}).

%% `(a, b=1)'. Defaults are expressions and are evaluated at the call site
%% rather than at definition time; that difference is in the deviation list.
params([], _Loc) -> [];
params([{open, _, $(}, {close, _, $)}], _Loc) -> [];
params([{open, _, $(} | R], Loc) ->
    Inner = strip_close(R, Loc),
    [param(P, Loc) || P <- split_top(Inner, fun({op, _, ','}) -> true; (_) -> false end)];
params([T | _], _Loc) ->
    ?THROW(element(2, T), {unexpected_token, token_text(T)}).

strip_close(Toks, Loc) ->
    case lists:reverse(Toks) of
        [{close, _, $)} | Rev] -> lists:reverse(Rev);
        _ -> ?THROW(Loc, {unexpected_token, <<"expected )">>})
    end.

param([{name, _, N}], _Loc) ->
    {N, undefined};
param([{name, _, N}, {op, L, '='} | Rest], _Loc) ->
    {N, expr_all(Rest, L)};
param([T | _], _Loc) ->
    ?THROW(element(2, T), {unexpected_token, token_text(T)});
param([], Loc) ->
    ?THROW(Loc, {unexpected_token, <<"expected a parameter name">>}).

%% `{% call(x) macro(...) %}' -- the optional parameter list belongs to the
%% caller block, not to the macro being called.
call_stmt(Loc, Args, Toks) ->
    {Params, Rest0} = call_params(Args, Loc),
    Target = expr_all(Rest0, Loc),
    {Body, Rest} = body(Toks, [endcall], call, Loc),
    {node, {call, Loc, Params, Target, Body}, expect_end(Rest, endcall, Loc)}.

call_params([{open, _, $(} | _] = Toks, Loc) ->
    case find_top_close(Toks) of
        none -> ?THROW(Loc, {unexpected_token, <<"expected )">>});
        N ->
            {Head, Tail} = lists:split(N + 1, Toks),
            {params(Head, Loc), Tail}
    end;
call_params(Toks, _Loc) ->
    {[], Toks}.

%%%===================================================================
%%% Duplicate blocks
%%%===================================================================

check_duplicate_blocks(Nodes) ->
    check_dups(collect_blocks(Nodes, []), #{}).

check_dups([], _Seen) -> ok;
check_dups([{Name, Loc} | Rest], Seen) ->
    case Seen of
        #{Name := First} -> ?THROW(Loc, {duplicate_block, Name, First});
        _                -> check_dups(Rest, Seen#{Name => Loc})
    end.

collect_blocks(Nodes, Acc) ->
    lists:foldl(fun(N, A) -> collect_block(N, A) end, Acc, Nodes).

collect_block({block, Loc, Name, _, _, Body}, Acc) ->
    collect_blocks(Body, [{Name, Loc} | Acc]);
collect_block(Node, Acc) ->
    lists:foldl(fun(B, A) -> collect_blocks(B, A) end, Acc, bodies(Node)).

%% The single place that knows which nodes carry a body, used by warnings/1
%% too. ai_jinja_ast has its own copy for the same reason ai_mustache_ast does:
%% the passes there also need to rebuild the node, not just read it.
bodies({'if', _, Branches, Else})        -> [B || {_, B} <- Branches] ++ [Else];
bodies({'for', _, _, _, _, _, B, E})     -> [B, E];
bodies({set_block, _, _, B, _})          -> [B];
bodies({with, _, _, B})                  -> [B];
bodies({filter, _, _, B})                -> [B];
bodies({block, _, _, _, _, B})           -> [B];
bodies({macro, _, _, _, B})              -> [B];
bodies({call, _, _, _, B})               -> [B];
bodies(_)                                -> [].

%%%===================================================================
%%% Warnings
%%%===================================================================

%% @doc Non-fatal observations about a parsed template.
%%
%% `{% extends %}' anywhere but first is legal -- the reference implementation
%% allows it and only the first one takes effect -- so it warns rather than
%% failing; rejecting it would turn working templates into build errors.
-spec warnings(nodes()) -> [{loc(), term()}].
warnings(Nodes) ->
    case lists:keyfind(extends, 1, Nodes) of
        false -> shadow_warnings(Nodes, []);
        {extends, Loc, _} ->
            Pos = [N || N <- Nodes, element(1, N) =:= extends],
            First = hd(Nodes) =:= hd(Pos),
            W0 = [{Loc, extends_not_first} || not First],
            W1 = [{element(2, N), {content_after_extends, element(1, N)}}
                  || N <- Nodes, not carried_by_extends(N)],
            W0 ++ W1 ++ shadow_warnings(Nodes, [])
    end.

%% With a parent template in play, only these contribute anything; everything
%% else at the top level is discarded when rendering.
carried_by_extends({extends, _, _})       -> true;
carried_by_extends({block, _, _, _, _, _}) -> true;
carried_by_extends({macro, _, _, _, _})   -> true;
carried_by_extends({import, _, _, _, _})  -> true;
carried_by_extends({from, _, _, _, _})    -> true;
carried_by_extends({set, _, _, _})        -> true;
carried_by_extends({set_block, _, _, _, _}) -> true;
carried_by_extends({text, _, Bin})        -> is_blank(Bin);
carried_by_extends(_)                     -> false.

is_blank(Bin) ->
    binary:replace(Bin, [<<" ">>, <<"\t">>, <<"\r">>, <<"\n">>], <<>>, [global]) =:= <<>>.

%% Binding over a name the engine provides is legal but nearly always a typo.
shadow_warnings(Nodes, Acc) ->
    lists:foldl(fun shadow_warning/2, Acc, Nodes).

shadow_warning({'for', Loc, Targets, _, _, _, B, E}, Acc) ->
    Shadowed = [{Loc, {shadows_reserved, T}}
                || T <- Targets, lists:member(T, ?AI_JINJA_RESERVED)],
    shadow_warnings(E, shadow_warnings(B, Shadowed ++ Acc));
shadow_warning({set, Loc, Name, _}, Acc) when is_atom(Name) ->
    case lists:member(Name, ?AI_JINJA_RESERVED) of
        true  -> [{Loc, {shadows_reserved, Name}} | Acc];
        false -> Acc
    end;
shadow_warning(Node, Acc) ->
    lists:foldl(fun(B, A) -> shadow_warnings(B, A) end, Acc, bodies(Node)).

%%%===================================================================
%%% Token helpers
%%%===================================================================

%% The expression parser always reports a position of its own, so the
%% statement's is not needed as a fallback -- it is kept in the signature only
%% because every caller has one to hand and a future clause might.
expr_all(Toks, _Loc) ->
    case ai_jinja_expr:parse_all(Toks) of
        {ok, E}            -> E;
        {error, L, Reason} -> ?THROW(L, Reason)
    end.

%% Every expression node carries its own position; the fallback is for a
%% caller that has something other than a node in hand.
loc_of(Node, _Fallback) when is_tuple(Node), tuple_size(Node) >= 2 ->
    element(2, Node);
loc_of(_Other, Fallback) ->
    Fallback.

%% Find the first token at bracket depth zero for which Pred holds.
find_top(Toks, Pred) -> find_top(Toks, Pred, 0, 0).
find_top([], _Pred, _D, _N) -> none;
find_top([{open, _, _} | R], Pred, D, N)  -> find_top(R, Pred, D + 1, N + 1);
find_top([{close, _, _} | R], Pred, D, N) -> find_top(R, Pred, D - 1, N + 1);
find_top([T | R], Pred, 0, N) ->
    case Pred(T) of
        true  -> {N, T};
        false -> find_top(R, Pred, 0, N + 1)
    end;
find_top([_ | R], Pred, D, N) -> find_top(R, Pred, D, N + 1).

%% Index of the `)' that closes a leading `('.
find_top_close([{open, _, $(} | R]) -> find_close(R, 0, 1);
find_top_close(_)                   -> none.
find_close([], _N, _D)              -> none;
find_close([{open, _, _} | R], N, D)  -> find_close(R, N + 1, D + 1);
find_close([{close, _, _} | _R], N, 1) -> N + 1;
find_close([{close, _, _} | R], N, D) -> find_close(R, N + 1, D - 1);
find_close([_ | R], N, D)             -> find_close(R, N + 1, D).

%% Split on a separator at bracket depth zero.
split_top([], _Pred) -> [];
split_top(Toks, Pred) ->
    case find_top(Toks, Pred) of
        none    -> [Toks];
        {N, _T} ->
            {Head, [_Sep | Tail]} = lists:split(N, Toks),
            [Head | split_top(Tail, Pred)]
    end.

token_text({name, _, N})  -> atom_to_binary(N, utf8);
token_text({kw, _, K})    -> atom_to_binary(K, utf8);
token_text({op, _, O})    -> atom_to_binary(O, utf8);
token_text({int, _, V})   -> integer_to_binary(V);
token_text({float, _, V}) -> float_to_binary(V, [short]);
token_text({str, _, V})   -> <<$", V/binary, $">>;
token_text({open, _, C})  -> <<C>>;
token_text({close, _, C}) -> <<C>>.
