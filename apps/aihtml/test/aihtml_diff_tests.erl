%% Tests for aihtml_diff.
-module(aihtml_diff_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_diff.hrl").

-define(M, aihtml_diff).

r(Html) -> aihtml_html:render_binary(Html).

has(Needle, Hay) ->
    case binary:match(Hay, Needle) of
        nomatch -> ?debugFmt("~s not in~n~s", [Needle, Hay]), false;
        _ -> true
    end.

has_quiet(Needle, Hay) -> binary:match(Hay, Needle) =/= nomatch.

%%%===================================================================
%%% diff
%%%===================================================================

rows(Old, New) ->
    [{T, X, O, N} || #{type := T, text := X, old_no := O, new_no := N} <- ?M:line_rows(Old, New)].

line_rows_test() ->
    ?assertEqual([], rows(<<>>, <<>>)),
    ?assertEqual([{ctx, <<"a">>, 1, 1}], rows(<<"a\n">>, <<"a">>)),
    ?assertEqual([{add, <<"a">>, undefined, 1}, {add, <<"b">>, undefined, 2}],
                 rows(<<>>, <<"a\nb\n">>)),
    ?assertEqual([{ctx, <<"a">>, 1, 1}, {del, <<"b">>, 2, undefined}, {del, <<"c">>, 3, undefined},
                  {add, <<"X">>, undefined, 2}, {ctx, <<"d">>, 4, 3}, {add, <<"e">>, undefined, 4}],
                 rows("a\nb\nc\nd", "a\nX\nd\ne")),
    %% an empty line in the middle is a real line; CRLF compares like LF
    ?assertEqual([{ctx, <<"a">>, 1, 1}, {ctx, <<>>, 2, 2}, {ctx, <<"b">>, 3, 3}],
                 rows(<<"a\r\n\r\nb">>, <<"a\n\nb\n">>)),
    ?assertEqual([{del, <<"中"/utf8>>, 1, undefined}, {add, <<"文"/utf8>>, undefined, 1}],
                 rows(<<"中"/utf8>>, <<"文"/utf8>>)).

%% The edit script is minimal and rebuilds both sides.
myers_property_test() ->
    rand:seed(exsss, {1, 2, 3}),
    [begin
         A = [rand:uniform(4) || _ <- lists:seq(1, rand:uniform(12) - 1)],
         B = [rand:uniform(4) || _ <- lists:seq(1, rand:uniform(12) - 1)],
         Rows = ?M:line_rows(join(A), join(B)),
         Old = [T || #{type := Ty, text := T} <- Rows, Ty =/= add],
         New = [T || #{type := Ty, text := T} <- Rows, Ty =/= del],
         ?assertEqual({A, B}, {[binary_to_integer(X) || X <- Old],
                               [binary_to_integer(X) || X <- New]}),
         ?assertEqual(lcs(A, B), length([x || #{type := ctx} <- Rows]))
     end || _ <- lists:seq(1, 300)].

join(L) -> iolist_to_binary(lists:join(<<"\n">>, [integer_to_binary(X) || X <- L])).

lcs(A, B) ->
    {_, Last} = lists:foldl(
                  fun(X, {_, Prev}) ->
                          Row = lists:foldl(
                                  fun({J, Y}, Acc) ->
                                          Left = hd(Acc),
                                          V = case X =:= Y of
                                                  true -> lists:nth(J, Prev) + 1;
                                                  false -> max(Left, lists:nth(J + 1, Prev))
                                              end,
                                          [V | Acc]
                                  end, [0], lists:zip(lists:seq(1, length(B)), B)),
                          {x, lists:reverse(Row)}
                  end, {x, lists:duplicate(length(B) + 1, 0)}, A),
    lists:last(Last).

split_rows_test() ->
    Rows = ?M:line_rows(<<"a\nb\nc\nd">>, <<"a\nB\nd\ne">>),
    Pairs = [{side(L, old_no), side(R, new_no)} || {L, R} <- ?M:split_rows(Rows)],
    ?assertEqual([{{ctx, 1}, {ctx, 1}}, {{del, 2}, {add, 2}}, {{del, 3}, none},
                  {{ctx, 4}, {ctx, 3}}, {none, {add, 4}}], Pairs).

side(undefined, _) -> none;
side(#{type := T} = Row, K) -> {T, maps:get(K, Row)}.

word_parts_test() ->
    ?assertEqual([#{type => ctx, value => <<"the ">>}, #{type => del, value => <<"quick">>},
                  #{type => add, value => <<"slow">>}, #{type => ctx, value => <<" brown fox">>}],
                 ?M:word_parts(<<"the quick brown fox">>, <<"the slow brown fox">>)),
    %% CJK compares per character
    ?assertEqual([#{type => ctx, value => <<"今天"/utf8>>}, #{type => del, value => <<"晴"/utf8>>},
                  #{type => add, value => <<"雨"/utf8>>}],
                 ?M:word_parts(<<"今天晴"/utf8>>, <<"今天雨"/utf8>>)),
    ?assertEqual([], ?M:word_parts(<<>>, <<>>)).

diff_unified_test() ->
    H = r(?M:diff(<<"a\n\n<b>">>, <<"a\n\nc">>, [line_numbers, stats, <<"max-h-64">>], [{id, d}])),
    ?assert(has(<<"<div class=\"ah-diff max-h-64\" data-mode=\"line\" data-view=\"unified\" id=\"d\">">>, H)),
    ?assert(has(<<"<div class=\"ah-diff__stats\"><span class=\"ah-diff__stat\" data-type=\"add\">+1</span>"
                  "<span class=\"ah-diff__stat\" data-type=\"del\">-1</span></div>">>, H)),
    ?assert(has(<<"<div class=\"ah-diff__row\" data-type=\"ctx\"><span class=\"ah-diff__lineno\" aria-hidden=\"true\">1</span>"
                  "<span class=\"ah-diff__lineno\" aria-hidden=\"true\">1</span>"
                  "<span class=\"ah-diff__marker\" aria-hidden=\"true\"> </span>"
                  "<span class=\"ah-diff__text\">a</span></div>">>, H)),
    ?assert(has(<<"<span class=\"ah-diff__text\"> </span>"/utf8>>, H)),
    ?assert(has(<<"<div class=\"ah-diff__row\" data-type=\"del\"><span class=\"ah-diff__lineno\" aria-hidden=\"true\">3</span>"
                  "<span class=\"ah-diff__lineno\" aria-hidden=\"true\"></span>"
                  "<span class=\"ah-diff__marker\" aria-hidden=\"true\">-</span>"
                  "<span class=\"ah-diff__text\">&lt;b&gt;</span>">>, H)),
    %% without line numbers
    H2 = r(?M:diff(<<"a">>, <<"b">>, [], [])),
    ?assertNot(has_quiet(<<"lineno">>, H2)),
    ?assertNot(has_quiet(<<"stats">>, H2)),
    ?assert(has(<<"<div class=\"ah-diff__row\" data-type=\"add\"><span class=\"ah-diff__marker\" aria-hidden=\"true\">+</span>">>, H2)).

diff_split_word_test() ->
    H = r(?M:diff(<<"a\nb">>, <<"a\nc\nd">>, [split], [])),
    ?assert(has(<<"data-view=\"split\"><div class=\"ah-diff__split\">">>, H)),
    ?assert(has(<<"<div class=\"ah-diff__pair\"><div class=\"ah-diff__side\" data-side=\"old\" data-type=\"del\">"
                  "<span class=\"ah-diff__lineno\" aria-hidden=\"true\">2</span>"
                  "<span class=\"ah-diff__text\">b</span></div>"
                  "<div class=\"ah-diff__side\" data-side=\"new\" data-type=\"add\">">>, H)),
    ?assert(has(<<"<div class=\"ah-diff__side\" data-side=\"old\" data-type=\"empty\">"
                  "<span class=\"ah-diff__lineno\" aria-hidden=\"true\"></span>"/utf8>>, H)),
    %% word mode is always one column, and has no stats bar
    W = r(?M:diff(<<"one two">>, <<"one 2">>, [word, split, stats], [])),
    ?assert(has(<<"data-mode=\"word\" data-view=\"unified\"><div class=\"ah-diff__words\">"
                  "<span class=\"ah-diff__word\" data-type=\"ctx\">one </span>"
                  "<span class=\"ah-diff__word\" data-type=\"del\">two</span>"
                  "<span class=\"ah-diff__word\" data-type=\"add\">2</span></div>">>, W)),
    ?assertError({aihtml, {conflicting_modifiers, diff, view, [split, unified]}},
                 ?M:diff(<<>>, <<>>, [split, unified], [])).

%%%===================================================================
%%% Catalog
%%%===================================================================

catalog_test() ->
    [#{name := diff}] = ?M:catalog(),
    [begin
         #{flags := Fl, options := Op, option_docs := Docs, methods := Ms} = E =
             aihtml_catalog:entry(?M, N),
         ?assertEqual(lists:sort(Fl ++ Op), lists:sort(maps:keys(Docs))),
         [_ | _] = aihtml_catalog:classes(E, Fl),
         [?assert(is_binary(D)) || #{doc := D} <- Ms]
     end || #{name := N} <- ?M:catalog()],
    %% diff modifiers write no classes of their own
    ?assertEqual([<<"ah-diff">>], aihtml_catalog:classes(aihtml_catalog:entry(?M, diff),
                                                         [word, split, stats, line_numbers])).

catalog_docs_test() ->
    [begin
         ?assert(byte_size(maps:get(doc, E)) > 0),
         [?assert(byte_size(maps:get(K, maps:get(option_docs, E))) > 0)
          || K <- maps:get(options, E, []) ++ maps:get(flags, E, [])]
     end || E <- ?M:catalog()].

%%%===================================================================
%%% element record (designs/05-records.md)
%%%===================================================================

record_equals_builder_test() ->
    ?assertEqual(r(?M:diff(<<"a">>, <<"b">>, [split, stats], [])),
                 r(#ah_diff{old = <<"a">>, new = <<"b">>, view = split, stats = true})).

builder_fills_fields_test() ->
    ?assertMatch(#ah_diff{old = <<"o">>, new = <<"n">>, mode = word, view = unified,
                          line_numbers = true}, ?M:diff(<<"o">>, <<"n">>, [word, line_numbers], [])).

postback_test() ->
    ?assertError({aihtml, {no_postback_event, ah_diff}}, r(#ah_diff{postback = x})).

field_validation_test() ->
    ?assertError({aihtml, {bad_modifier, diff, view, both, _}}, r(#ah_diff{view = both})),
    ?assertError({aihtml, {modifier_in_css, diff, split}}, r(#ah_diff{css = [split]})).

records_match_catalog_test() ->
    Base = [module, id, css, attrs, postback, delegate],
    [begin
         Tag = list_to_atom("ah_" ++ atom_to_list(N)),
         Fields = ?M:fields(Tag),
         ?assertEqual(Base, lists:sublist(Fields, 6)),
         Defaults = maps:from_list(lists:zip(Fields, tl(tuple_to_list(default(Tag))))),
         [?assertEqual({N, G, case D of none -> undefined; _ -> D end},
                       {N, G, maps:get(G, Defaults)})
          || {G, {_, D}} <- maps:to_list(maps:get(groups, E, #{}))],
         [?assertEqual({N, F, false}, {N, F, maps:get(F, Defaults)})
          || F <- maps:get(flags, E, [])],
         [?assert(lists:member(O, Fields)) || O <- maps:get(options, E, [])],
         ?assertEqual(?M, maps:get(module, Defaults))
     end || #{name := N} = E <- ?M:catalog()].

default(ah_diff) -> #ah_diff{}.
