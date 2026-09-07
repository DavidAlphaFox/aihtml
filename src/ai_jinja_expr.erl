%%%-------------------------------------------------------------------
%%% @doc Expression parser for the jinja engine.
%%%
%%% Recursive descent, one function per precedence level, in the order the
%%% reference implementation uses (designs/09-jinja-syntax.md section 3.3):
%%%
%%%   condexpr -> or -> and -> not -> compare -> math1(+ -) -> concat(~)
%%%            -> math2(* / // %) -> pow(**) -> unary -> postfix -> primary
%%%
%%% Three things here are counter-intuitive and each has a fixture:
%%%
%%%   * `**' is LEFT associative, unlike Python: `2**3**2' is 64, not 512.
%%%   * filters and `is' tests bind at the unary level, tighter than every
%%%     arithmetic operator: `1 + -2|abs' is 1 + abs(-2).
%%%   * `-2 ** 2' is `(-2) ** 2' = 4: the unary minus is parsed first and
%%%     the power loop then folds it in from the left.
%%%
%%% Chained comparison is the one place we deliberately diverge: `1 < x < 3'
%%% is rejected rather than silently parsed as `(1 < x) < 3', which would give
%%% a confidently wrong answer (deviation J3).
%%% @end
%%%-------------------------------------------------------------------
-module(ai_jinja_expr).

-include("ai_jinja.hrl").

-export([parse/1, parse_all/1, parse_list/1, parse_args/1]).
-export([loc_of/1, expect_end/1]).

-type toks() :: [ai_jinja_expr_token()].
-type expr() :: ai_jinja_expr().
-type loc()  :: ai_html_loc().

-define(THROW(Loc, Reason), throw({ai_jinja_expr, Loc, Reason})).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc One expression, plus whatever tokens follow it.
%%
%% The statement parser needs the remainder: `{% for x in xs if C %}' holds
%% three expressions in one tag.
-spec parse(toks()) -> {ok, expr(), toks()} | {error, loc(), ai_jinja_reason()}.
parse(Toks) ->
    try condexpr(Toks) of
        {E, Rest} -> {ok, E, Rest}
    catch
        throw:{ai_jinja_expr, Loc, Reason} -> {error, Loc, Reason}
    end.

%% @doc One expression that must consume every token.
-spec parse_all(toks()) -> {ok, expr()} | {error, loc(), ai_jinja_reason()}.
parse_all(Toks) ->
    case parse(Toks) of
        {ok, E, []}          -> {ok, E};
        {ok, _E, [T | _]}    -> {error, loc_of(T), {unexpected_token, text_of(T)}};
        {error, _, _} = E    -> E
    end.

%% @doc A comma-separated list of expressions.
-spec parse_list(toks()) -> {ok, [expr()], toks()} | {error, loc(), ai_jinja_reason()}.
parse_list(Toks) ->
    try list_of(Toks, []) of
        {Es, Rest} -> {ok, Es, Rest}
    catch
        throw:{ai_jinja_expr, Loc, Reason} -> {error, Loc, Reason}
    end.

%% @doc A parenthesised argument list, starting at the `('.
-spec parse_args(toks()) ->
          {ok, ai_jinja_args(), toks()} | {error, loc(), ai_jinja_reason()}.
parse_args(Toks) ->
    try args(Toks) of
        {A, Rest} -> {ok, A, Rest}
    catch
        throw:{ai_jinja_expr, Loc, Reason} -> {error, Loc, Reason}
    end.

%% @doc Assert nothing is left over.
-spec expect_end(toks()) -> ok | {error, loc(), ai_jinja_reason()}.
expect_end([])      -> ok;
expect_end([T | _]) -> {error, loc_of(T), {unexpected_token, text_of(T)}}.

-spec loc_of(tuple()) -> loc().
loc_of(T) -> element(2, T).

%%%===================================================================
%%% Precedence ladder
%%%===================================================================

%% X if C else Y -- the only right-associative level.
condexpr(Toks) ->
    {E, Rest} = or_expr(Toks),
    case Rest of
        [{kw, L, 'if'} | R1] ->
            {C, R2} = or_expr(R1),
            case R2 of
                [{kw, _, 'else'} | R3] ->
                    {Else, R4} = condexpr(R3),
                    {{'cond', L, C, E, Else}, R4};
                _ ->
                    {{'cond', L, C, E, undefined}, R2}
            end;
        _ ->
            {E, Rest}
    end.

or_expr(Toks) ->
    {E, Rest} = and_expr(Toks),
    or_loop(E, Rest).
or_loop(E, [{kw, L, 'or'} | R]) ->
    {E2, R1} = and_expr(R),
    or_loop({'or', L, E, E2}, R1);
or_loop(E, Rest) -> {E, Rest}.

and_expr(Toks) ->
    {E, Rest} = not_expr(Toks),
    and_loop(E, Rest).
and_loop(E, [{kw, L, 'and'} | R]) ->
    {E2, R1} = not_expr(R),
    and_loop({'and', L, E, E2}, R1);
and_loop(E, Rest) -> {E, Rest}.

not_expr([{kw, L, 'not'} | R]) ->
    {E, R1} = not_expr(R),
    {{unop, L, 'not', E}, R1};
not_expr(Toks) ->
    compare(Toks).

%% Exactly one comparison operator. A second one is the chained form, which
%% the reference implementation supports and we refuse outright rather than
%% answer wrongly.
compare(Toks) ->
    {E, Rest} = math1(Toks),
    case compop(Rest) of
        none ->
            {E, Rest};
        {Op, L, R1} ->
            {E2, R2} = math1(R1),
            Node = {binop, L, Op, E, E2},
            case compop(R2) of
                none      -> {Node, R2};
                {_, L2, _} -> ?THROW(L2, {chained_comparison, L2})
            end
    end.

compop([{op, L, Op} | R]) when Op =:= '=='; Op =:= '!='; Op =:= '<';
                               Op =:= '>'; Op =:= '<='; Op =:= '>=' ->
    {Op, L, R};
compop([{kw, L, 'in'} | R]) ->
    {'in', L, R};
compop([{kw, L, 'not'}, {kw, _, 'in'} | R]) ->
    {'not in', L, R};
compop(_) ->
    none.

math1(Toks) ->
    {E, Rest} = concat(Toks),
    math1_loop(E, Rest).
math1_loop(E, [{op, L, Op} | R]) when Op =:= '+'; Op =:= '-' ->
    {E2, R1} = concat(R),
    math1_loop({binop, L, Op, E, E2}, R1);
math1_loop(E, Rest) -> {E, Rest}.

%% `~' sits between the additive and multiplicative levels, so `2 * 3 ~ 4'
%% is `(2 * 3) ~ 4'.
concat(Toks) ->
    {E, Rest} = math2(Toks),
    concat_loop(E, Rest).
concat_loop(E, [{op, L, '~'} | R]) ->
    {E2, R1} = math2(R),
    concat_loop({binop, L, '~', E, E2}, R1);
concat_loop(E, Rest) -> {E, Rest}.

math2(Toks) ->
    {E, Rest} = power(Toks),
    math2_loop(E, Rest).
math2_loop(E, [{op, L, Op} | R]) when Op =:= '*'; Op =:= '/'; Op =:= '//'; Op =:= '%' ->
    {E2, R1} = power(R),
    math2_loop({binop, L, Op, E, E2}, R1);
math2_loop(E, Rest) -> {E, Rest}.

%% Left-folded, which is what makes `**' left associative here.
power(Toks) ->
    {E, Rest} = unary(Toks, with_filter),
    power_loop(E, Rest).
power_loop(E, [{op, L, '**'} | R]) ->
    {E2, R1} = unary(R, with_filter),
    power_loop({binop, L, '**', E, E2}, R1);
power_loop(E, Rest) -> {E, Rest}.

%% A leading sign parses its operand WITHOUT a filter chain, so that the
%% filter attaches to the negated value: `-a|abs' is `(-a)|abs'.
unary([{op, L, Op} | R], Mode) when Op =:= '-'; Op =:= '+' ->
    {E, R1} = unary(R, no_filter),
    finish_unary({unop, L, Op, E}, R1, Mode);
unary(Toks, Mode) ->
    {E, Rest} = primary(Toks),
    finish_unary(E, Rest, Mode).

finish_unary(E, Rest, Mode) ->
    {E1, R1} = postfix(E, Rest),
    case Mode of
        with_filter -> filter_chain(E1, R1);
        no_filter   -> {E1, R1}
    end.

%%%===================================================================
%%% Postfix and the filter chain
%%%===================================================================

postfix(E, [{op, L, '.'}, {name, _, N} | R]) ->
    postfix({attr, L, E, N}, R);
%% `d.items' would otherwise fail: several of the names we special-case are
%% keywords to the lexer.
postfix(E, [{op, L, '.'}, {kw, _, K} | R]) ->
    postfix({attr, L, E, K}, R);
postfix(E, [{op, L, '.'}, {int, _, N} | R]) ->
    postfix({sub, L, E, {lit, L, N}}, R);
postfix(E, [{open, L, $[} | R]) ->
    {Node, R1} = subscript(E, L, R),
    postfix(Node, R1);
postfix(E, [{open, L, $(} | _] = Toks) ->
    {A, R1} = args(Toks),
    postfix({call, L, E, A}, R1);
postfix(E, Rest) ->
    {E, Rest}.

filter_chain(E, [{op, _, '|'}, {name, L, F} | R]) ->
    {A, R1} = maybe_args(R),
    filter_chain({filter, L, F, E, A}, R1);
filter_chain(_E, [{op, L, '|'} | _]) ->
    ?THROW(L, {unexpected_token, <<"|">>});
filter_chain(E, [{kw, _, 'is'}, {kw, L, 'not'} | R]) ->
    {Node, R1} = test_of(E, L, R, true),
    filter_chain(Node, R1);
filter_chain(E, [{kw, L, 'is'} | R]) ->
    {Node, R1} = test_of(E, L, R, false),
    filter_chain(Node, R1);
filter_chain(E, Rest) ->
    {E, Rest}.

%% The one place the grammar switches namespace: after `is' comes a test name,
%% not an expression.
test_of(E, _L, [{name, L, T} | R], Neg) ->
    {A, R1} = test_args(R),
    {{test, L, T, E, A, Neg}, R1};
%% A test may be named by a word the lexer treats as a keyword -- `is in(...)',
%% `is none' -- so any keyword is accepted here. The namespace switch after
%% `is' is what makes that unambiguous.
test_of(E, _L, [{kw, L, K} | R], Neg) ->
    {A, R1} = test_args(R),
    {{test, L, K, E, A, Neg}, R1};
test_of(_E, L, Toks, _Neg) ->
    ?THROW(L, {unexpected_token, text_of(hd_or_eof(Toks))}).

maybe_args([{open, _, $(} | _] = Toks) -> args(Toks);
maybe_args(Toks)                       -> {no_args(), Toks}.

no_args() -> {[], [], undefined, undefined}.

%% A test may take a single argument without parentheses: `x is sameas true',
%% `x is divisibleby 3'. Only a primary with its postfix chain, so that
%% `x is odd and y' still parses the way it reads.
test_args([{open, _, $(} | _] = Toks) ->
    args(Toks);
test_args([T | _] = Toks) when element(1, T) =:= name;
                               element(1, T) =:= str;
                               element(1, T) =:= int;
                               element(1, T) =:= float;
                               element(1, T) =:= open ->
    {E, R} = unary(Toks, no_filter),
    {{[E], [], undefined, undefined}, R};
test_args([{kw, _, K} | _] = Toks) when K =:= 'true'; K =:= 'false'; K =:= 'none' ->
    {E, R} = unary(Toks, no_filter),
    {{[E], [], undefined, undefined}, R};
test_args(Toks) ->
    {no_args(), Toks}.

%%%===================================================================
%%% Primary
%%%===================================================================

primary([{int, L, V} | R])   -> {{lit, L, V}, R};
primary([{float, L, V} | R]) -> {{lit, L, V}, R};
primary([{str, L, V} | R])   -> adjacent_strings(L, V, R);
primary([{kw, L, 'true'} | R])  -> {{lit, L, true}, R};
primary([{kw, L, 'false'} | R]) -> {{lit, L, false}, R};
primary([{kw, L, 'none'} | R])  -> {{lit, L, undefined}, R};
primary([{name, L, N} | R])  -> {{name, L, N}, R};
primary([{open, L, $(} | R]) -> paren(L, R);
primary([{open, L, $[} | R]) -> list_lit(L, R, []);
primary([{open, L, ${} | R]) -> map_lit(L, R, []);
primary(Toks) ->
    T = hd_or_eof(Toks),
    ?THROW(loc_or_zero(T), {unexpected_token, text_of(T)}).

%% "a" "b" is "ab", as in Python.
adjacent_strings(L, V, [{str, _, V2} | R]) ->
    adjacent_strings(L, <<V/binary, V2/binary>>, R);
adjacent_strings(L, V, R) ->
    {{lit, L, V}, R}.

%% `(a)' is `a'; only a comma makes a tuple.
paren(L, R) ->
    case R of
        [{close, _, $)} | R1] -> {{tuple, L, []}, R1};
        _ ->
            {E, R1} = condexpr(R),
            case R1 of
                [{close, _, $)} | R2] -> {E, R2};
                [{op, _, ','} | _]    -> tuple_rest(L, [E], R1);
                _                     -> ?THROW(close_loc(L, R1), {unexpected_token, <<"(">>})
            end
    end.

tuple_rest(L, Acc, [{op, _, ','}, {close, _, $)} | R]) ->
    {{tuple, L, lists:reverse(Acc)}, R};
tuple_rest(L, Acc, [{op, _, ','} | R]) ->
    {E, R1} = condexpr(R),
    tuple_rest(L, [E | Acc], R1);
tuple_rest(L, Acc, [{close, _, $)} | R]) ->
    {{tuple, L, lists:reverse(Acc)}, R};
tuple_rest(L, _Acc, Toks) ->
    ?THROW(close_loc(L, Toks), {unexpected_token, text_of(hd_or_eof(Toks))}).

list_lit(L, [{close, _, $]} | R], Acc) ->
    {{list, L, lists:reverse(Acc)}, R};
list_lit(L, Toks, Acc) ->
    {E, R1} = condexpr(Toks),
    case R1 of
        [{op, _, ','} | R2] -> list_lit(L, R2, [E | Acc]);
        [{close, _, $]} | R2] -> {{list, L, lists:reverse([E | Acc])}, R2};
        _ -> ?THROW(close_loc(L, R1), {unexpected_token, text_of(hd_or_eof(R1))})
    end.

map_lit(L, [{close, _, $}} | R], Acc) ->
    {{map, L, lists:reverse(Acc)}, R};
map_lit(L, Toks, Acc) ->
    {K, R1} = condexpr(Toks),
    case R1 of
        [{op, _, ':'} | R2] ->
            {V, R3} = condexpr(R2),
            case R3 of
                [{op, _, ','} | R4]   -> map_lit(L, R4, [{K, V} | Acc]);
                [{close, _, $}} | R4] -> {{map, L, lists:reverse([{K, V} | Acc])}, R4};
                _ -> ?THROW(close_loc(L, R3), {unexpected_token, text_of(hd_or_eof(R3))})
            end;
        _ ->
            ?THROW(close_loc(L, R1), {unexpected_token, text_of(hd_or_eof(R1))})
    end.

%%%===================================================================
%%% Subscripts and slices
%%%===================================================================

subscript(E, L, Toks) ->
    case slice_parts(Toks, L) of
        {index, Idx, R}          -> {{sub, L, E, Idx}, R};
        {slice, {A, B, C}, R}    -> {{slice, L, E, A, B, C}, R}
    end.

%% `a[1]', `a[1:]', `a[:2]', `a[::2]' -- every component optional.
slice_parts(Toks, L) ->
    {First, R1} = slice_component(Toks),
    case R1 of
        [{close, _, $]} | R2] when First =/= undefined ->
            {index, First, R2};
        [{close, _, $]} | _] ->
            ?THROW(close_loc(L, R1), {unexpected_token, <<"[">>});
        [{op, _, ':'} | R2] ->
            {Second, R3} = slice_component(R2),
            case R3 of
                [{close, _, $]} | R4] ->
                    {slice, {First, Second, undefined}, R4};
                [{op, _, ':'} | R4] ->
                    {Third, R5} = slice_component(R4),
                    case R5 of
                        [{close, _, $]} | R6] ->
                            {slice, {First, Second, Third}, R6};
                        _ ->
                            ?THROW(close_loc(L, R5), {unexpected_token, text_of(hd_or_eof(R5))})
                    end;
                _ ->
                    ?THROW(close_loc(L, R3), {unexpected_token, text_of(hd_or_eof(R3))})
            end;
        _ ->
            ?THROW(close_loc(L, R1), {unexpected_token, text_of(hd_or_eof(R1))})
    end.

slice_component([{close, _, $]} | _] = Toks) -> {undefined, Toks};
slice_component([{op, _, ':'} | _] = Toks)   -> {undefined, Toks};
slice_component(Toks)                        -> condexpr(Toks).

%%%===================================================================
%%% Argument lists
%%%===================================================================

args([{open, L, $(} | R]) ->
    arg_loop(R, L, [], [], undefined, undefined);
args(Toks) ->
    ?THROW(loc_or_zero(hd_or_eof(Toks)), {unexpected_token, text_of(hd_or_eof(Toks))}).

arg_loop([{close, _, $)} | R], _L, Pos, Kw, S, D) ->
    {{lists:reverse(Pos), lists:reverse(Kw), S, D}, R};
arg_loop([{op, _, '**'} | R], L, Pos, Kw, S, _D) ->
    {E, R1} = condexpr(R),
    arg_sep(R1, L, Pos, Kw, S, E);
arg_loop([{op, _, '*'} | R], L, Pos, Kw, _S, D) ->
    {E, R1} = condexpr(R),
    arg_sep(R1, L, Pos, Kw, E, D);
arg_loop([{name, _, N}, {op, _, '='} | R], L, Pos, Kw, S, D) ->
    {E, R1} = condexpr(R),
    arg_sep(R1, L, Pos, [{N, E} | Kw], S, D);
arg_loop(Toks, L, Pos, Kw, S, D) ->
    %% A positional argument after a keyword one is a Python error and would
    %% otherwise be silently reordered here.
    Kw =:= [] orelse ?THROW(loc_or_zero(hd_or_eof(Toks)),
                            {unexpected_token, text_of(hd_or_eof(Toks))}),
    {E, R1} = condexpr(Toks),
    arg_sep(R1, L, [E | Pos], Kw, S, D).

arg_sep([{op, _, ','} | R], L, Pos, Kw, S, D) -> arg_loop(R, L, Pos, Kw, S, D);
arg_sep([{close, _, $)} | R], _L, Pos, Kw, S, D) ->
    {{lists:reverse(Pos), lists:reverse(Kw), S, D}, R};
arg_sep(Toks, L, _Pos, _Kw, _S, _D) ->
    ?THROW(close_loc(L, Toks), {unexpected_token, text_of(hd_or_eof(Toks))}).

%%%===================================================================
%%% Comma-separated expressions, for statements
%%%===================================================================

list_of(Toks, Acc) ->
    {E, R} = condexpr(Toks),
    case R of
        [{op, _, ','} | R1] -> list_of(R1, [E | Acc]);
        _                   -> {lists:reverse([E | Acc]), R}
    end.

%%%===================================================================
%%% Diagnostics
%%%===================================================================

hd_or_eof([T | _]) -> T;
hd_or_eof([])      -> eof.

loc_or_zero(eof) -> {1, 1};
loc_or_zero(T)   -> element(2, T).

close_loc(L, [])      -> L;
close_loc(_L, [T | _]) -> element(2, T).

%% A readable rendering of a token, for the diagnostic only.
text_of(eof)             -> <<"end of expression">>;
text_of({name, _, N})    -> atom_to_binary(N, utf8);
text_of({kw, _, K})      -> atom_to_binary(K, utf8);
text_of({op, _, O})      -> atom_to_binary(O, utf8);
text_of({int, _, V})     -> integer_to_binary(V);
text_of({float, _, V})   -> float_to_binary(V, [short]);
text_of({str, _, V})     -> <<$", V/binary, $">>;
text_of({open, _, C})    -> <<C>>;
text_of({close, _, C})   -> <<C>>.
