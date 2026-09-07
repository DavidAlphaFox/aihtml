%%%-------------------------------------------------------------------
%%% Tests for the expression parser.
%%%
%%% The three counter-intuitive rules of the grammar each get a direct
%%% assertion on the tree, because a rendering fixture can only show that the
%%% ANSWER is right, not that the parse was.
%%%-------------------------------------------------------------------
-module(ai_jinja_expr_tests).

-include_lib("eunit/include/eunit.hrl").

p(Bin) ->
    {ok, T, _, _, _} = ai_jinja_lexer:tokens(<<Bin/binary, " }}">>, {1, 1}, expr_end),
    ai_jinja_expr:parse_all(T).

ok_(Bin) -> {ok, E} = p(Bin), E.

%%%===================================================================
%%% Associativity and binding
%%%===================================================================

%% Unlike Python, `**' folds from the left here, because that is what the
%% reference implementation does.
power_is_left_associative_test() ->
    ?assertMatch({binop, _, '**',
                  {binop, _, '**', {lit, _, 2}, {lit, _, 3}},
                  {lit, _, 2}},
                 ok_(<<"2 ** 3 ** 2">>)).

%% A filter binds tighter than any arithmetic operator...
filter_binds_tighter_than_arithmetic_test() ->
    ?assertMatch({binop, _, '+', {lit, _, 1}, {filter, _, abs, _, _}},
                 ok_(<<"1 + -2|abs">>)),
    ?assertMatch({binop, _, '**', {lit, _, 2}, {filter, _, abs, _, _}},
                 ok_(<<"2 ** 3|abs">>)).

%% ... but a leading sign is inside it, so the filter sees the negated value.
sign_is_inside_the_filter_test() ->
    ?assertMatch({filter, _, abs, {unop, _, '-', {name, _, a}}, _},
                 ok_(<<"-a|abs">>)).

concat_sits_between_additive_and_multiplicative_test() ->
    ?assertMatch({binop, _, '~', {binop, _, '*', _, _}, {lit, _, 4}},
                 ok_(<<"2 * 3 ~ 4">>)),
    ?assertMatch({binop, _, '~', {binop, _, '~', _, _}, _},
                 ok_(<<"\"a\" ~ \"b\" ~ \"c\"">>)).

precedence_ladder_test() ->
    ?assertMatch({binop, _, '+', _, {binop, _, '*', _, _}}, ok_(<<"2 + 3 * 4">>)),
    ?assertMatch({binop, _, '*', {binop, _, '+', _, _}, _}, ok_(<<"(2 + 3) * 4">>)),
    ?assertMatch({'or', _, _, {'and', _, _, _}}, ok_(<<"a or b and c">>)),
    ?assertMatch({'and', _, {unop, _, 'not', _}, _}, ok_(<<"not a and b">>)).

%%%===================================================================
%%% Deviation J3
%%%===================================================================

%% The reference implementation chains comparisons; we refuse rather than
%% answer `(1 < x) < 3', which would be confidently wrong.
chained_comparison_is_refused_test() ->
    ?assertMatch({error, _, {chained_comparison, _}}, p(<<"1 < x < 3">>)),
    ?assertMatch({error, _, {chained_comparison, _}}, p(<<"a == b == c">>)).

single_comparison_is_fine_test() ->
    ?assertMatch({binop, _, '<', _, _}, ok_(<<"1 < x">>)),
    ?assertMatch({binop, _, 'in', _, _}, ok_(<<"1 in xs">>)),
    ?assertMatch({binop, _, 'not in', _, _}, ok_(<<"1 not in xs">>)).

%%%===================================================================
%%% Tests, filters, calls
%%%===================================================================

is_switches_namespace_test() ->
    ?assertMatch({test, _, odd, {name, _, x}, _, false}, ok_(<<"x is odd">>)),
    ?assertMatch({test, _, none, {name, _, x}, _, true}, ok_(<<"x is not none">>)),
    ?assertMatch({test, _, divisibleby, _, {[{lit, _, 3}], [], _, _}, false},
                 ok_(<<"x is divisibleby(3)">>)).

%% A test may take one bare argument: `x is sameas true'.
test_takes_a_bare_argument_test() ->
    ?assertMatch({test, _, sameas, _, {[{lit, _, true}], [], _, _}, false},
                 ok_(<<"x is sameas true">>)),
    %% ... but only one, and not at the cost of the surrounding expression.
    ?assertMatch({'and', _, {test, _, odd, _, {[], [], _, _}, false}, _},
                 ok_(<<"x is odd and y">>)).

postfix_chain_test() ->
    ?assertMatch({call, _, {sub, _, {attr, _, {name, _, a}, b}, {lit, _, 0}},
                  {[{lit, _, 1}], [{k, {lit, _, 2}}], undefined, undefined}},
                 ok_(<<"a.b[0](1, k=2)">>)).

filter_chain_keeps_order_test() ->
    ?assertMatch({filter, _, g, {filter, _, f, {name, _, x}, _}, _},
                 ok_(<<"x|f|g">>)).

%%%===================================================================
%%% Literals
%%%===================================================================

containers_test() ->
    ?assertMatch({list, _, [{lit, _, 1}, {lit, _, 2}]}, ok_(<<"[1, 2,]">>)),
    ?assertMatch({list, _, []}, ok_(<<"[]">>)),
    ?assertMatch({map, _, [{{lit, _, <<"a">>}, {lit, _, 1}}]}, ok_(<<"{\"a\": 1}">>)),
    ?assertMatch({tuple, _, [{lit, _, 1}, {lit, _, 2}]}, ok_(<<"(1, 2)">>)).

%% `(a)' is grouping; only a comma makes a tuple.
parentheses_are_not_a_tuple_test() ->
    ?assertMatch({lit, _, 1}, ok_(<<"(1)">>)),
    ?assertMatch({tuple, _, [{lit, _, 1}]}, ok_(<<"(1,)">>)).

adjacent_strings_join_test() ->
    ?assertMatch({lit, _, <<"ab">>}, ok_(<<"\"a\" \"b\"">>)).

slices_test() ->
    ?assertMatch({slice, _, _, {lit, _, 1}, {lit, _, 2}, {lit, _, 3}},
                 ok_(<<"l[1:2:3]">>)),
    ?assertMatch({slice, _, _, undefined, {lit, _, 2}, undefined}, ok_(<<"l[:2]">>)),
    ?assertMatch({sub, _, _, {lit, _, 0}}, ok_(<<"l[0]">>)).

conditional_test() ->
    ?assertMatch({'cond', _, {name, _, b}, {name, _, a}, {name, _, c}},
                 ok_(<<"a if b else c">>)),
    ?assertMatch({'cond', _, _, _, undefined}, ok_(<<"a if b">>)).

%%%===================================================================
%%% Diagnostics
%%%===================================================================

errors_test() ->
    ?assertMatch({error, _, {unexpected_token, _}}, p(<<"1 2">>)),
    ?assertMatch({error, _, {unexpected_token, _}}, p(<<"+">>)),
    %% A keyword argument may not be followed by a positional one.
    ?assertMatch({error, _, {unexpected_token, _}}, p(<<"f(k=1, 2)">>)).

%% `{{ a|f1|nosuch|f3 }}' has to point at nosuch, not at the start of the tag.
filter_position_is_the_filter_name_test() ->
    {filter, Loc, nosuch, _, _} = strip_outer(ok_(<<"a|f1|nosuch|f3">>)),
    ?assertMatch({1, 6}, Loc).

strip_outer({filter, _, f3, Inner, _}) -> Inner;
strip_outer(Other) -> Other.
