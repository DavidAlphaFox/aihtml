%% Tests for aihtml_data_tree. The module is also the fake action module
%% of the lazy tree round trip.
-module(aihtml_data_tree_tests).
-behaviour(aihtml_action).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_data_tree.hrl").

-export([action/4]).

-define(M, aihtml_data_tree).

r(Html) -> aihtml_html:render_binary(Html).

has(Needle, Hay) ->
    case binary:match(Hay, Needle) of
        nomatch -> ?debugFmt("~s not in~n~s", [Needle, Hay]), false;
        _ -> true
    end.

count(Needle, Hay) -> length(binary:matches(Hay, Needle)).

%%%===================================================================
%%% tree
%%%===================================================================

files() ->
    [#{label => <<"Docs">>, value => docs, icon => <<"D">>,
       items => [{work, <<"Work">>, [<<"q1">>, <<"q2">>]},
                 #{label => <<"Personal">>, disabled => true}]},
     {dl, <<"Downloads">>},
     #{label => <<"Remote">>, value => remote, lazy => true}].

tree_structure_test() ->
    H = r(?M:tree(files(), undefined, [<<"w-64">>], [{id, t}, {aria_label, <<"Files">>}])),
    ?assert(has(<<"<div class=\"ah-tree w-64\" id=\"t\" role=\"tree\" data-ah=\"tree\" "
                  "data-ah-value=\"\" data-toggle-mode=\"click\" data-animation=\"slide\" "
                  "aria-label=\"Files\">">>, H)),
    ?assert(has(<<"<ul class=\"ah-tree-list\" role=\"presentation\">">>, H)),
    %% top level: first enabled node takes the tab stop
    ?assert(has(<<"<li class=\"ah-tree-item\" id=\"t-0\" role=\"treeitem\" aria-level=\"1\" "
                  "data-tree-id=\"0\" data-value=\"docs\" aria-expanded=\"false\" tabindex=\"0\">">>, H)),
    ?assert(has(<<"<span class=\"ah-tree-icon\" aria-hidden=\"true\">D</span>">>, H)),
    ?assert(has(<<"<ul class=\"ah-tree-list\" role=\"group\" id=\"t-0-g\" style=\"display:none;\">">>, H)),
    ?assert(has(<<"id=\"t-0-0-1\" role=\"treeitem\" aria-level=\"3\" data-tree-id=\"0-0-1\" "
                  "data-value=\"q2\" tabindex=\"-1\"">>, H)),
    ?assert(has(<<"<div class=\"ah-tree-row ah-tree-row-disabled\">">>, H)),
    ?assert(has(<<"aria-disabled=\"true\"">>, H)),
    ?assert(has(<<"<li class=\"ah-tree-item ah-tree-item-leaf\" id=\"t-1\"">>, H)),
    ?assert(has(<<"<span class=\"ah-tree-toggle ah-tree-toggle-leaf\" aria-hidden=\"true\">"/utf8>>, H)),
    %% a lazy node: expandable, empty group, names the tree
    ?assert(has(<<"data-value=\"remote\" aria-expanded=\"false\" tabindex=\"-1\" data-lazy=\"true\" "
                  "data-tree=\"t\" data-level=\"1\"">>, H)),
    ?assert(has(<<"id=\"t-2-g\" style=\"display:none;\"></ul>">>, H)),
    ?assertNot(has_quiet(<<"data-load">>, H)).

tree_selection_test() ->
    H = r(?M:tree(files(), <<"q2">>, [], [{id, t}, {name, doc}])),
    ?assert(has(<<"data-ah-value=\"q2\"">>, H)),
    %% the ancestors of the selected node are open
    ?assert(has(<<"data-value=\"docs\" aria-expanded=\"true\" tabindex=\"-1\"">>, H)),
    ?assert(has(<<"data-value=\"work\" aria-expanded=\"true\"">>, H)),
    ?assertNot(has_quiet(<<"id=\"t-0-g\" style">>, H)),
    ?assert(has(<<"data-value=\"q2\" aria-selected=\"true\" tabindex=\"0\"">>, H)),
    ?assert(has(<<"<div class=\"ah-tree-row ah-tree-row-selected\">">>, H)),
    ?assertEqual(1, count(<<"tabindex=\"0\"">>, H)),
    ?assert(has(<<"<input type=\"hidden\" name=\"doc\" value=\"q2\" data-ah-input>">>, H)),
    %% an unknown value selects nothing
    H2 = r(?M:tree(files(), nope, [], [{id, t}])),
    ?assert(has(<<"data-ah-value=\"\"">>, H2)),
    ?assertNot(has_quiet(<<"aria-selected">>, H2)).

tree_options_test() ->
    H = r(?M:tree([a, 1, "s"], undefined, [disabled],
                  [{toggle_mode, dblclick}, {animation, none}, {load, {?MODULE, children, #{}}}])),
    ?assert(has(<<"class=\"ah-tree ah-tree-disabled\"">>, H)),
    ?assert(has(<<"aria-disabled=\"true\" data-ah=\"tree\"">>, H)),
    ?assert(has(<<"data-toggle-mode=\"dblclick\" data-animation=\"none\" data-load=\"">>, H)),
    {match, [Token]} = re:run(H, <<"data-load=\"([^\"]+)\"">>, [{capture, all_but_first, binary}]),
    ?assertEqual({ok, {?MODULE, children, #{}}}, aihtml_action:verify(Token)),
    ?assert(has(<<"data-value=\"1\"">>, H)),
    ?assert(has(<<"data-value=\"s\"">>, H)),
    %% expanded without children is a leaf; expanded with children is open
    H2 = r(?M:tree([#{label => <<"x">>, expanded => true},
                    #{label => <<"y">>, expanded => true, items => [z]}], undefined, [], [{id, e}])),
    ?assert(has(<<"data-value=\"x\" tabindex=\"0\"">>, H2)),
    ?assert(has(<<"data-value=\"y\" aria-expanded=\"true\"">>, H2)),
    ?assert(has(<<"<span class=\"ah-tree-toggle ah-tree-toggle-open\"">>, H2)).

tree_escaping_test() ->
    H = r(?M:tree([{<<"a\"b">>, <<"<i>x</i>">>}], <<"a\"b">>, [], [{id, t}])),
    ?assert(has(<<"data-value=\"a&quot;b\"">>, H)),
    ?assert(has(<<"&lt;i&gt;x&lt;/i&gt;">>, H)).

generated_id_test() ->
    H = r(#ah_tree{items = [a]}),
    {match, [Id]} = re:run(H, <<"^<div class=\"ah-tree\" id=\"(ah-t[0-9]+)\"">>,
                           [{capture, all_but_first, binary}]),
    ?assert(has(<<"id=\"", Id/binary, "-0\"">>, H)),
    ?assertNotEqual(r(#ah_tree{}), r(#ah_tree{})).

%%%===================================================================
%%% Lazy nodes: render, fire, answer with set_children
%%%===================================================================

action(children, #{source := Source}, #{data := #{<<"value">> := V}} = Ev, Ctx) ->
    ?M:set_children(Ctx, Ev, maps:get(V, Source, [])).

lazy_round_trip_test() ->
    Ref = {?MODULE, children, #{source => #{<<"remote">> => [<<"r1">>, #{label => <<"R2">>,
                                                                        lazy => true}]}}},
    H = r(?M:tree(files(), undefined, [], [{id, <<"tr">>}, {load, Ref}])),
    {match, [Token]} = re:run(H, <<"data-load=\"([^\"]+)\"">>, [{capture, all_but_first, binary}]),
    {ok, Ref} = aihtml_action:verify(Token),
    %% the browser sends the node's id and data-* attributes
    Event = #{<<"type">> => <<"ah:load">>, <<"id">> => <<"tr-2">>, <<"value">> => null,
              <<"data">> => #{<<"value">> => <<"remote">>, <<"tree">> => <<"tr">>,
                              <<"treeId">> => <<"2">>, <<"level">> => <<"1">>,
                              <<"lazy">> => <<"true">>}},
    Self = self(),
    ok = aihtml_action:execute(Ref, Event, #{emit => fun(E) -> Self ! {ev, E} end}),
    [#{<<"value">> := [Html, Call]}] = [E || #{<<"type">> := <<"CUSTOM">>} = E <- collect()],
    #{op := html, swap := morph_inner, id := <<"tr-2-g">>, html := Kids} = Html,
    ?assert(has(<<"<li class=\"ah-tree-item ah-tree-item-leaf\" id=\"tr-2-0\" role=\"treeitem\" "
                  "aria-level=\"2\" data-tree-id=\"2-0\" data-value=\"r1\" tabindex=\"-1\">">>, Kids)),
    %% a nested lazy node names the tree and its own level
    ?assert(has(<<"id=\"tr-2-1\" role=\"treeitem\" aria-level=\"2\" data-tree-id=\"2-1\" "
                  "data-value=\"R2\" aria-expanded=\"false\" tabindex=\"-1\" data-lazy=\"true\" "
                  "data-tree=\"tr\" data-level=\"2\"">>, Kids)),
    ?assertEqual(#{op => call, id => <<"tr">>, method => <<"childrenLoaded">>,
                   args => [<<"tr-2">>]}, Call),
    %% the same markup as a first render of those children would have
    Full = r(?M:tree([#{label => <<"Remote">>, value => remote,
                        items => [<<"r1">>, #{label => <<"R2">>, lazy => true}]}],
                     undefined, [], [{id, <<"tr">>}])),
    ?assert(has(binary:replace(binary:replace(Kids, <<"tr-2">>, <<"tr-0">>, [global]),
                               <<"data-tree-id=\"2-">>, <<"data-tree-id=\"0-">>, [global]),
                Full)),
    _ = iolist_to_binary(json:encode([Html, Call])).

set_children_empty_test() ->
    Ops = aihtml_action:render_ops(
            fun(Ctx) ->
                    ?M:set_children(Ctx, #{id => <<"t-0-3">>,
                                           data => #{<<"tree">> => <<"t">>, <<"level">> => <<"2">>,
                                                     <<"treeId">> => <<"0-3">>}}, [])
            end),
    ?assertMatch([#{op := html, id := <<"t-0-3-g">>, html := <<>>},
                  #{op := call, method := <<"childrenLoaded">>, args := [<<"t-0-3">>]}], Ops).

collect() ->
    receive {ev, E} -> [E | collect()]
    after 0 -> []
    end.

has_quiet(Needle, Hay) -> binary:match(Hay, Needle) =/= nomatch.

%%%===================================================================
%%% nav_tree
%%%===================================================================

nav() ->
    [#{group => <<"OVERVIEW">>,
       items => [#{label => <<"Dashboard">>, icon => {safe, <<"<svg></svg>">>}, route => <<"dashboard">>},
                 {<<"Analytics">>, <<"analytics">>}]},
     #{group => <<"MANAGEMENT">>,
       items => [#{label => <<"User">>,
                   items => [{<<"Profile">>, <<"user/profile">>},
                             #{label => <<"Deep">>, items => [{<<"Cards">>, <<"user/cards">>}]}]},
                 #{label => <<"Docs">>, href => <<"https://example.com">>}]},
     {<<"Loose">>, <<"loose">>}].

nav_tree_test() ->
    H = r(?M:nav_tree(nav(), <<"user/cards">>, [<<"w-64">>], [{id, nt}])),
    ?assert(has(<<"<nav class=\"ah-nav-tree w-64\" data-ah=\"nav-tree\" data-ah-value=\"user/cards\" id=\"nt\">">>, H)),
    ?assert(has(<<"<div class=\"ah-nav-tree__group\"><div class=\"ah-nav-tree__group-label\">OVERVIEW</div>">>, H)),
    ?assert(has(<<"<a class=\"ah-nav-tree__item\" href=\"#/dashboard\" data-route=\"dashboard\">"
                  "<span class=\"ah-nav-tree__icon\" aria-hidden=\"true\"><svg></svg></span>"
                  "<span class=\"ah-nav-tree__label\">Dashboard</span></a>">>, H)),
    %% the nodes around the active link are open
    ?assertEqual(2, count(<<"<details class=\"ah-nav-tree__node\" open>">>, H)),
    ?assertEqual(2, count(<<"ah-nav-tree__item--parent ah-is-open">>, H)),
    ?assert(has(<<"<a class=\"ah-nav-tree__item ah-is-active\" href=\"#/user/cards\" "
                  "data-route=\"user/cards\" aria-current=\"page\">">>, H)),
    ?assertEqual(1, count(<<"ah-is-active">>, H)),
    ?assert(has(<<"<div class=\"ah-nav-tree__children\"><div class=\"ah-nav-tree__children-inner\">">>, H)),
    ?assert(has(<<"<a class=\"ah-nav-tree__item\" href=\"https://example.com\">">>, H)),
    %% a loose item gets a group without a heading
    ?assert(has(<<"<div class=\"ah-nav-tree__group\"><a class=\"ah-nav-tree__item\" href=\"#/loose\"">>, H)),
    %% another route: nothing active, nodes closed; route prefix
    H2 = r(?M:nav_tree(nav(), undefined, [], [{route_prefix, <<"/app/">>}])),
    ?assertNot(has_quiet(<<" open>">>, H2)),
    ?assertNot(has_quiet(<<"ah-is-">>, H2)),
    ?assert(has(<<"data-ah-value=\"\"">>, H2)),
    ?assert(has(<<"href=\"/app/analytics\"">>, H2)),
    ?assertError({aihtml, {bad_nav_tree_item, 42}}, r(?M:nav_tree([42], undefined, [], []))).

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
%%% heatmap_calendar
%%%===================================================================

heatmap_test() ->
    Data = #{<<"2026-09-01">> => 1, {2026, 9, 2} => 4, "2026-09-03" => 7, <<"2026-09-04">> => 2.5},
    H = r(?M:heatmap_calendar(Data, [], [{months, 1}, {end_date, <<"2026-09-29">>}, {id, hm}])),
    ?assert(has(<<"<div class=\"ah-heatmap-calendar\" data-ah=\"heatmap-calendar\" "
                  "data-tip=\"{value} · {date}\" id=\"hm\">"/utf8>>, H)),
    %% 2026-08-29 is a Saturday: the grid starts on Sunday 2026-08-23 and
    %% ends with the week of the 29th of September (a Tuesday): 6 weeks
    ?assertEqual(6, count(<<"class=\"ah-heatmap-calendar__week\"">>, H)),
    ?assertEqual(42, count(<<"class=\"ah-heatmap-calendar__cell\"">>, H)),
    {match, [First]} = re:run(H, <<"data-date=\"([0-9-]+)\"">>, [{capture, all_but_first, binary}]),
    ?assertEqual(<<"2026-08-23">>, First),
    ?assert(has(<<"data-date=\"2026-10-03\"">>, H)),
    ?assert(has(<<"data-level=\"1\" data-date=\"2026-09-01\" data-value=\"1\"">>, H)),
    ?assert(has(<<"data-level=\"3\" data-date=\"2026-09-02\" data-value=\"4\"">>, H)),
    ?assert(has(<<"data-level=\"4\" data-date=\"2026-09-03\" data-value=\"7\"">>, H)),
    ?assert(has(<<"data-level=\"2\" data-date=\"2026-09-04\" data-value=\"2.5\"">>, H)),
    ?assert(has(<<"data-level=\"0\" data-date=\"2026-09-05\" data-value=\"0\"">>, H)),
    %% month labels by the Wednesday of each week: Aug 1 week, Sep 5 weeks
    ?assert(has(<<"<span class=\"ah-heatmap-calendar__month\" style=\"width:15px;\">Aug</span>"
                  "<span class=\"ah-heatmap-calendar__month\" style=\"width:75px;\">Sep</span>">>, H)),
    ?assert(has(<<"<span class=\"ah-heatmap-calendar__weekday\">Mon</span>">>, H)),
    ?assertEqual(5, count(<<"ah-heatmap-calendar__legend-cell">>, H)),
    ?assert(has(<<"<span>Less</span>">>, H)),
    ?assert(has(<<"<div class=\"ah-heatmap-calendar__tooltip\" data-visible=\"false\" role=\"tooltip\"></div>">>, H)).

heatmap_options_test() ->
    Months = [integer_to_binary(M) || M <- lists:seq(1, 12)],
    H = r(?M:heatmap_calendar([{{2026, 3, 1}, 9}], [],
                              [{months, 2}, {end_date, {2026, 3, 31}}, {thresholds, [0, 10]},
                               {legend, false}, {month_labels, Months},
                               {weekday_labels, [<<"S">>, <<"M">>, <<"T">>, <<"W">>, <<"T">>,
                                                 <<"F">>, <<"S">>]},
                               {tooltip, <<"{date}: {value}">>}])),
    ?assertNot(has_quiet(<<"legend">>, H)),
    ?assert(has(<<"data-level=\"1\" data-date=\"2026-03-01\"">>, H)),
    ?assert(has(<<">1</span><span class=\"ah-heatmap-calendar__month\"">>, H)),
    ?assert(has(<<"<span class=\"ah-heatmap-calendar__weekday\">S</span>">>, H)),
    ?assert(has(<<"data-tip=\"{date}: {value}\"">>, H)),
    %% 31 March minus 2 months: 31 January (no overflow into February)
    ?assert(has(<<"data-date=\"2026-01-25\"">>, H)),
    ?assertNot(has_quiet(<<"data-date=\"2026-01-24\"">>, H)),
    %% without end_date the grid ends in the current week
    Today = iso(date()),
    ?assert(has(<<"data-date=\"", Today/binary, "\"">>, r(?M:heatmap_calendar(#{}, [], [])))).

iso({Y, M, D}) -> iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0B", [Y, M, D])).

%%%===================================================================
%%% Catalog
%%%===================================================================

catalog_test() ->
    [#{name := tree}, #{name := nav_tree, category := layout}, #{name := diff},
     #{name := heatmap_calendar}] = ?M:catalog(),
    ?assertEqual([{set_children, 3}], ?M:facade_extras()),
    [?assert(erlang:function_exported(?M, F, A)) || {F, A} <- ?M:facade_extras()],
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
%%% element records (designs/05-records.md)
%%%===================================================================

record_equals_builder_test() ->
    Load = {?MODULE, children, #{}},
    ?assertEqual(r(?M:tree(files(), q1, [disabled, <<"w-64">>],
                           [{id, t}, {name, n}, {toggle_mode, dblclick}, {animation, none},
                            {load, Load}, {title, <<"t">>}])),
                 r(#ah_tree{items = files(), value = q1, disabled = true, css = [<<"w-64">>],
                            id = t, name = n, toggle_mode = dblclick, animation = none,
                            load = Load, attrs = [{title, <<"t">>}]})),
    ?assertEqual(r(?M:nav_tree(nav(), <<"loose">>, [], [{route_prefix, <<"/">>}, {id, n}])),
                 r(#ah_nav_tree{items = nav(), value = <<"loose">>, route_prefix = <<"/">>, id = n})),
    ?assertEqual(r(?M:diff(<<"a">>, <<"b">>, [split, stats], [])),
                 r(#ah_diff{old = <<"a">>, new = <<"b">>, view = split, stats = true})),
    ?assertEqual(r(?M:heatmap_calendar(#{}, [], [{months, 3}, {end_date, {2026, 9, 29}},
                                                 {legend, false}])),
                 r(#ah_heatmap_calendar{months = 3, end_date = {2026, 9, 29}, legend = false})).

builder_fills_fields_test() ->
    T = ?M:tree([a], a, [disabled, <<"x">>], [{name, n}, {toggle_mode, dblclick}, {role, x}]),
    ?assertMatch(#ah_tree{items = [a], value = a, disabled = true, name = n,
                          toggle_mode = dblclick, animation = slide, css = [<<"x">>],
                          attrs = [{role, x}]}, T),
    ?assertMatch(#ah_diff{old = <<"o">>, new = <<"n">>, mode = word, view = unified,
                          line_numbers = true}, ?M:diff(<<"o">>, <<"n">>, [word, line_numbers], [])),
    ?assertMatch(#ah_heatmap_calendar{data = #{}, months = 6, tooltip = <<"t">>},
                 ?M:heatmap_calendar(#{}, [], [{months, 6}, {tooltip, <<"t">>}])),
    ?assertError({aihtml, {record_only_field, ah_tree, postback}},
                 ?M:tree([], undefined, [], [{postback, pick}])).

postback_test() ->
    Token = fun(Html) ->
                    {match, [Ev, Tok]} = re:run(r(Html), <<"data-ah-on=\"([a-z:]+):([^\"]+)\"">>,
                                                [{capture, all_but_first, binary}]),
                    {ok, Ref} = aihtml_action:unsign(Tok),
                    {Ev, Ref}
            end,
    ?assertEqual({<<"change">>, {?MODULE, picked, #{id => 7}}},
                 Token(#ah_tree{items = [a], postback = {picked, #{id => 7}}})),
    ?assertEqual({<<"change">>, {other, go, #{}}},
                 Token(#ah_nav_tree{postback = go, delegate = other})),
    ?assertEqual({<<"ah:select">>, {?MODULE, day, #{}}},
                 Token(#ah_heatmap_calendar{end_date = {2026, 1, 1}, postback = day})),
    ?assertError({aihtml, {no_postback_event, ah_diff}}, r(#ah_diff{postback = x})).

field_validation_test() ->
    ?assertError({aihtml, {bad_option, toggle_mode, triple}}, r(#ah_tree{toggle_mode = triple})),
    ?assertError({aihtml, {bad_option, animation, fade}}, r(#ah_tree{animation = fade})),
    ?assertError({aihtml, {bad_option, load, nope}}, r(#ah_tree{load = nope})),
    ?assertError({aihtml, {bad_option, lazy, yes}}, r(#ah_tree{items = [#{label => a, lazy => yes}]})),
    ?assertError({aihtml, {bad_tree_item, {1, 2, 3}}}, r(#ah_tree{items = [{1, 2, 3}]})),
    ?assertError({aihtml, {tree_item_needs_value, _}},
                 r(#ah_tree{items = [#{label => {safe, <<"<b>x</b>">>}}]})),
    ?assertError({aihtml, {bad_flag, tree, disabled, yes}}, r(#ah_tree{disabled = yes})),
    ?assertError({aihtml, {bad_modifier, diff, view, both, _}}, r(#ah_diff{view = both})),
    ?assertError({aihtml, {bad_option, months, 0}},
                 r(#ah_heatmap_calendar{months = 0})),
    ?assertError({aihtml, {bad_option, thresholds, [3, 1]}},
                 r(#ah_heatmap_calendar{thresholds = [3, 1]})),
    ?assertError({aihtml, {bad_option, month_labels, [a]}},
                 r(#ah_heatmap_calendar{month_labels = [a]})),
    ?assertError({aihtml, {bad_option, legend, true}}, r(#ah_heatmap_calendar{legend = true})),
    ?assertError({aihtml, {bad_date, <<"2026-02-30">>}},
                 r(#ah_heatmap_calendar{end_date = <<"2026-02-30">>})),
    ?assertError({aihtml, {bad_heatmap_value, _, x}},
                 r(#ah_heatmap_calendar{end_date = {2026, 1, 1}, data = #{{2026, 1, 1} => x}})),
    ?assertError({aihtml, {modifier_in_css, diff, split}}, r(#ah_diff{css = [split]})),
    ?assertError({aihtml, {unknown_modifier, tree, big, _}}, ?M:tree([], undefined, [big], [])).

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

default(ah_tree) -> #ah_tree{};
default(ah_nav_tree) -> #ah_nav_tree{};
default(ah_diff) -> #ah_diff{};
default(ah_heatmap_calendar) -> #ah_heatmap_calendar{}.
