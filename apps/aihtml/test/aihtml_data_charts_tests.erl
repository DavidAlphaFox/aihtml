%% Tests for aihtml_data_charts: the markup, the echarts options built on
%% the server, the data island, the records and the catalog.
-module(aihtml_data_charts_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_data_charts.hrl").

-define(M, aihtml_data_charts).

r(Html) -> aihtml_html:render_binary(Html).

has(Needle, Hay) ->
    case binary:match(Hay, Needle) of
        nomatch -> ?debugFmt("~s not in~n~s", [Needle, Hay]), false;
        _ -> true
    end.

%% The option in a rendered chart's data island, decoded.
island(Html) ->
    {match, [Json]} = re:run(r(Html), <<"<script class=\"ah-chart-data\" type=\"application/json\">"
                                        "(.*?)</script>">>,
                             [{capture, all_but_first, binary}, dotall]),
    json:decode(Json).

%%%===================================================================
%%% chart
%%%===================================================================

chart_markup_test() ->
    H = r(?M:chart(#{series => [#{type => line, data => [1, 2]}]}, [<<"mt-2">>],
                   [{id, c}, {height, 300}, {aria_label, <<"Sales">>}])),
    ?assert(has(<<"<div class=\"ah-chart mt-2\" role=\"img\" data-ah=\"chart\" "
                  "style=\"height:300px;\" id=\"c\" aria-label=\"Sales\">"
                  "<script class=\"ah-chart-data\" type=\"application/json\">">>, H)),
    ?assertEqual(#{<<"series">> => [#{<<"type">> => <<"line">>, <<"data">> => [1, 2]}]},
                 island(?M:chart(#{series => [#{type => line, data => [1, 2]}]}, [], []))).

chart_flags_test() ->
    H = r(?M:chart(#{}, [loading, disabled], [{width, <<"50%">>}, {height, 200},
                                               {renderer, svg}])),
    ?assert(has(<<"class=\"ah-chart ah-chart-disabled\"">>, H)),
    ?assert(has(<<"data-ah-renderer=\"svg\" data-ah-loading=\"true\" aria-disabled=\"true\" "
                  "style=\"width:50%;height:200px;\"">>, H)),
    ?assert(has(<<"<div class=\"ah-chart-overlay\"></div></div>">>, H)).

island_escaping_test() ->
    Opt = #{title => #{text => <<"</script><!-- x">>}},
    H = r(?M:chart(Opt, [], [])),
    ?assertEqual(nomatch, binary:match(H, <<"</script><!--">>)),
    ?assertEqual(1, length(binary:matches(H, <<"</script>">>))),
    %% and the data still round-trips
    ?assertEqual(#{<<"title">> => #{<<"text">> => <<"</script><!-- x">>}}, island(?M:chart(Opt, [], []))).

%%%===================================================================
%%% Convenience charts: the options they build
%%%===================================================================

area_option_test() ->
    O = island(?M:area_chart([{<<"A">>, [1, 2, 3]}, #{name => b, data => [3, null, 1],
                                                      color => <<"--ah-color-info">>}],
                             [stack], [{categories, [x, y, z]}, {title, <<"T">>},
                                       {y_name, <<"n">>}])),
    #{<<"xAxis">> := #{<<"type">> := <<"category">>, <<"data">> := [<<"x">>, <<"y">>, <<"z">>],
                       <<"boundaryGap">> := false},
      <<"yAxis">> := #{<<"type">> := <<"value">>, <<"name">> := <<"n">>},
      <<"series">> := [S1, S2], <<"title">> := #{<<"text">> := <<"T">>},
      <<"legend">> := #{<<"data">> := [<<"A">>, <<"b">>], <<"bottom">> := 0},
      <<"tooltip">> := #{<<"trigger">> := <<"axis">>}, <<"grid">> := #{<<"top">> := 64}} = O,
    #{<<"type">> := <<"line">>, <<"smooth">> := true, <<"stack">> := <<"total">>,
      <<"areaStyle">> := #{<<"opacity">> := 0.15}} = S1,
    #{<<"data">> := [3, null, 1], <<"color">> := <<"--ah-color-info">>} = S2,
    ?assertNot(maps:is_key(<<"color">>, O)),
    %% line, straight, no legend / tooltip, default categories
    O2 = island(?M:area_chart([{a, [1, 2]}], [line, straight],
                              [{legend, none}, {tooltip, false}, {colors, [<<"#f00">>]}])),
    [S] = maps:get(<<"series">>, O2),
    ?assertEqual(false, maps:get(<<"smooth">>, S)),
    ?assertNot(maps:is_key(<<"areaStyle">>, S)),
    ?assertNot(maps:is_key(<<"legend">>, O2)),
    ?assertNot(maps:is_key(<<"tooltip">>, O2)),
    ?assertEqual([<<"#f00">>], maps:get(<<"color">>, O2)),
    ?assertEqual([<<"1">>, <<"2">>], maps:get(<<"data">>, maps:get(<<"xAxis">>, O2))).

bar_option_test() ->
    O = island(?M:bar_chart([{a, [1, 2]}, {b, [3, 4]}], [horizontal, stack],
                            [{categories, [p, q]}, {bar_width, 20}, {grid, false},
                             {legend, right}])),
    #{<<"xAxis">> := #{<<"type">> := <<"value">>,
                       <<"splitLine">> := #{<<"show">> := false}},
      <<"yAxis">> := #{<<"type">> := <<"category">>, <<"data">> := [<<"p">>, <<"q">>]},
      <<"legend">> := #{<<"orient">> := <<"vertical">>, <<"right">> := 10},
      <<"grid">> := #{<<"right">> := 120},
      <<"series">> := [B1, B2]} = O,
    %% stacked: only the outer segment is rounded
    ?assertMatch(#{<<"type">> := <<"bar">>, <<"stack">> := <<"total">>, <<"barWidth">> := 20,
                   <<"itemStyle">> := #{<<"borderRadius">> := 0}}, B1),
    ?assertMatch(#{<<"itemStyle">> := #{<<"borderRadius">> := [0, 4, 4, 0]}}, B2),
    [V] = maps:get(<<"series">>, island(?M:bar_chart([{a, [1]}], [], []))),
    ?assertMatch(#{<<"itemStyle">> := #{<<"borderRadius">> := [4, 4, 0, 0]},
                   <<"barMaxWidth">> := 40}, V).

donut_option_test() ->
    O = island(?M:donut_chart([{a, 1}, #{name => b, value => 2.5, color => <<"red">>}], [],
                              [{title, t}])),
    #{<<"series">> := [#{<<"type">> := <<"pie">>, <<"radius">> := [<<"50%">>, <<"70%">>],
                         <<"center">> := [<<"40%">>, <<"55%">>],
                         <<"data">> := [#{<<"name">> := <<"a">>, <<"value">> := 1},
                                        #{<<"name">> := <<"b">>, <<"value">> := 2.5,
                                          <<"itemStyle">> := #{<<"color">> := <<"red">>}}],
                         <<"itemStyle">> := #{<<"borderColor">> := <<"--ah-color-bg-paper">>},
                         <<"label">> := #{<<"show">> := true}}],
      <<"legend">> := #{<<"orient">> := <<"vertical">>, <<"data">> := [<<"a">>, <<"b">>]},
      <<"tooltip">> := #{<<"trigger">> := <<"item">>}} = O,
    O2 = island(?M:donut_chart([{a, 1}], [pie], [{labels, false}, {legend, bottom},
                                                 {center, {10, 20}}])),
    #{<<"series">> := [#{<<"radius">> := [0, <<"70%">>], <<"center">> := [10, 20],
                         <<"label">> := #{<<"show">> := false}}],
      <<"legend">> := #{<<"bottom">> := 0}} = O2.

radar_option_test() ->
    O = island(?M:radar_chart([{a, [1, 2, 3]}], [circle],
                              [{indicators, [{x, 5}, #{name => y, max => 10, min => 1}, {z, 5}]},
                               {split_number, 5}, {area_opacity, 0.5}])),
    #{<<"radar">> := #{<<"shape">> := <<"circle">>, <<"splitNumber">> := 5,
                       <<"indicator">> := [#{<<"name">> := <<"x">>, <<"max">> := 5},
                                           #{<<"name">> := <<"y">>, <<"max">> := 10, <<"min">> := 1},
                                           _]},
      <<"series">> := [#{<<"type">> := <<"radar">>, <<"areaStyle">> := #{<<"opacity">> := 0.5},
                         <<"data">> := [#{<<"name">> := <<"a">>, <<"value">> := [1, 2, 3]}]}],
      <<"legend">> := #{<<"icon">> := <<"circle">>}} = O,
    ?assertMatch(#{<<"radar">> := #{<<"shape">> := <<"polygon">>}},
                 island(?M:radar_chart([], [], []))).

%%%===================================================================
%%% relation_graph
%%%===================================================================

graph() ->
    #{categories => [<<"people">>, #{name => <<"teams">>, color => <<"#123456">>}],
      nodes => [#{id => a, label => <<"Ann">>, category => <<"people">>, root => true},
                {b, <<"Bob">>},
                #{id => t, category => 1}],
      edges => [{a, b}, #{source => a, target => t, label => <<"leads">>, kind => dashed}]}.

graph_markup_test() ->
    H = r(?M:relation_graph(graph(), [directed],
                            [{id, g}, {selected, a}, {focus, b},
                             {details, #{a => <<"<Ann>">>, b => <<"Bob">>}}])),
    ?assert(has(<<"<div class=\"ah-relation-graph\" role=\"group\" "
                  "aria-roledescription=\"relation graph\" tabindex=\"0\" "
                  "data-ah=\"relation-graph\" data-ah-value=\"a\" data-layout=\"force\" "
                  "data-ah-focus=\"b\" style=\"height:420px;\" id=\"g\">">>, H)),
    ?assert(has(<<"<div class=\"ah-relation-graph__canvas ah-chart\" data-ah=\"chart\" "
                  "aria-hidden=\"true\" style=\"height:100%;\">">>, H)),
    ?assert(has(<<"data-act=\"fit\" title=\"Fit view\" aria-label=\"Fit view\"">>, H)),
    ?assert(has(<<"data-act=\"refresh\"">>, H)),
    ?assert(has(<<"<div class=\"ah-relation-graph__detail\" data-visible=\"true\">">>, H)),
    ?assert(has(<<"<div class=\"ah-relation-graph__detail-item\" data-node=\"a\">&lt;Ann&gt;</div>"
                  "<div class=\"ah-relation-graph__detail-item\" data-node=\"b\" hidden>Bob</div>">>, H)),
    ?assert(has(<<"<div class=\"ah-relation-graph__live\" aria-live=\"polite\" "
                  "aria-atomic=\"true\"></div>">>, H)),
    ?assertEqual(nomatch, binary:match(H, <<"ah-relation-graph__state">>)),
    %% no selection: the card is hidden
    H2 = r(?M:relation_graph(graph(), [], [{toolbar, false}])),
    ?assert(has(<<"data-ah-value=\"\" data-layout=\"force\" style">>, H2)),
    ?assert(has(<<"class=\"ah-relation-graph__detail\" data-visible=\"false\"">>, H2)),
    ?assertEqual(nomatch, binary:match(H2, <<"toolbar">>)).

graph_states_test() ->
    ?assert(has(<<"<div class=\"ah-relation-graph__state\" data-kind=\"loading\">">>,
                r(?M:relation_graph(graph(), [loading], [])))),
    ?assert(has(<<"data-kind=\"error\" role=\"alert\"><span>boom</span>">>,
                r(?M:relation_graph(graph(), [], [{error, <<"boom">>}])))),
    ?assert(has(<<"data-kind=\"empty\"><span>Nothing</span>">>,
                r(?M:relation_graph({[], []}, [], [{empty_text, <<"Nothing">>}])))).

graph_option_test() ->
    O = island_of_graph(?M:relation_graph(graph(), [directed, round_rect], [])),
    #{<<"color">> := [<<"--ah-color-primary">>, <<"#123456">>],
      <<"legend">> := [#{<<"data">> := [<<"people">>, <<"teams">>]}],
      <<"series">> := [S]} = O,
    #{<<"type">> := <<"graph">>, <<"layout">> := <<"force">>, <<"roam">> := true,
      <<"edgeSymbol">> := [<<"none">>, <<"arrow">>],
      <<"edgeLabel">> := #{<<"show">> := true},
      <<"categories">> := [#{<<"name">> := <<"people">>}, #{<<"name">> := <<"teams">>}],
      <<"data">> := [A, B, T], <<"links">> := [L1, L2]} = S,
    #{<<"id">> := <<"a">>, <<"name">> := <<"Ann">>, <<"category">> := 0,
      <<"symbol">> := <<"roundRect">>, <<"symbolSize">> := [63, 32]} = A,
    #{<<"id">> := <<"b">>, <<"name">> := <<"Bob">>, <<"symbolSize">> := [63, 27]} = B,
    ?assertNot(maps:is_key(<<"category">>, B)),
    #{<<"name">> := <<"t">>, <<"category">> := 1} = T,
    #{<<"source">> := <<"a">>, <<"target">> := <<"b">>, <<"value">> := <<>>,
      <<"lineStyle">> := #{<<"type">> := <<"solid">>, <<"width">> := 1.8}} = L1,
    #{<<"value">> := <<"leads">>, <<"lineStyle">> := #{<<"type">> := <<"dashed">>}} = L2,
    %% circular / fixed layouts; edge labels off
    [C] = maps:get(<<"series">>, island_of_graph(?M:relation_graph(graph(), [circular],
                                                                   [{edge_labels, false},
                                                                    {roam, false}]))),
    #{<<"layout">> := <<"circular">>, <<"roam">> := false,
      <<"edgeLabel">> := #{<<"show">> := false}} = C,
    ?assertNot(maps:is_key(<<"force">>, C)),
    [F] = maps:get(<<"series">>, island_of_graph(?M:relation_graph(
                                                   #{nodes => [#{id => p, x => 1, y => 2}]},
                                                   [fixed], []))),
    #{<<"layout">> := <<"none">>, <<"data">> := [#{<<"x">> := 1, <<"y">> := 2}]} = F.

tree_option_test() ->
    %% from edges; a cycle is cut; two roots get an invisible root
    G = #{nodes => [a, b, c, d], edges => [{a, b}, {b, c}, {c, b}]},
    [S] = maps:get(<<"series">>, island_of_graph(?M:relation_graph(G, [tree, bt], []))),
    #{<<"type">> := <<"tree">>, <<"orient">> := <<"BT">>,
      <<"data">> := [#{<<"id">> := <<"__root__">>,
                       <<"children">> := [#{<<"id">> := <<"a">>,
                                            <<"children">> := [#{<<"id">> := <<"b">>,
                                                                 <<"children">> := [C]}]},
                                          #{<<"id">> := <<"d">>} = D]}],
      <<"label">> := #{<<"position">> := <<"top">>}} = S,
    ?assertEqual(#{<<"id">> => <<"c">>, <<"name">> => <<"c">>}, C),
    ?assertNot(maps:is_key(<<"children">>, D)),
    %% from parents; one root
    P = #{nodes => [#{id => r}, #{id => k, parent => r, collapsed => true}]},
    [S2] = maps:get(<<"series">>, island_of_graph(?M:relation_graph(P, [tree], []))),
    #{<<"orient">> := <<"LR">>,
      <<"data">> := [#{<<"id">> := <<"r">>,
                       <<"children">> := [#{<<"id">> := <<"k">>, <<"collapsed">> := true}]}]} = S2.

island_of_graph(G) -> island(G).

%%%===================================================================
%%% chart_option / chart_update
%%%===================================================================

chart_option_test() ->
    ?assertEqual(#{a => 1}, ?M:chart_option(#ah_chart{option = #{a => 1}})),
    #{series := [#{type := bar}]} = ?M:chart_option(#ah_bar_chart{series = [{a, [1]}]}),
    #{series := [#{type := graph}]} = ?M:chart_option(#ah_relation_graph{graph = {[a], []}}),
    ?assertError({aihtml, {not_a_chart, x}}, ?M:chart_option(x)).

chart_update_test() ->
    Ops = aihtml_action:render_ops(
            fun(Ctx) ->
                    ?M:chart_update(Ctx, {id, <<"c">>}, #ah_area_chart{series = [{a, [1]}]}),
                    ?M:chart_update(Ctx, {id, <<"g">>}, #ah_relation_graph{graph = {[a], []}}),
                    ?M:chart_update(Ctx, <<"#x">>, #{series => [#{data => [2]}]})
            end),
    Text = iolist_to_binary(io_lib:format("~p", [Ops])),
    ?assert(has(<<"setOption">>, Text)),
    [#{op := call, id := <<"c">>, method := <<"setOption">>, args := [#{series := _}, false]},
     #{op := call, id := <<"g">>, args := [#{series := [#{type := graph}]}, true]},
     #{op := call, sel := <<"#x">>, args := [#{series := [#{data := [2]}]}, false]}] = Ops.

%%%===================================================================
%%% Catalog
%%%===================================================================

catalog_test() ->
    [#{name := chart}, #{name := area_chart}, #{name := bar_chart}, #{name := donut_chart},
     #{name := radar_chart}, #{name := relation_graph}] = ?M:catalog(),
    ?assertEqual([{chart_option, 1}, {chart_update, 3}], ?M:facade_extras()),
    [?assert(erlang:function_exported(?M, F, A)) || {F, A} <- ?M:facade_extras()],
    [begin
         #{flags := Fl, options := Op, option_docs := Docs, methods := Ms} = E =
             aihtml_catalog:entry(?M, N),
         ?assertEqual(lists:sort(Fl ++ Op), lists:sort(maps:keys(Docs))),
         [_ | _] = aihtml_catalog:classes(E, Fl),
         [?assert(is_binary(D)) || #{doc := D} <- Ms]
     end || #{name := N} <- ?M:catalog()],
    ?assertEqual([<<"ah-chart">>], aihtml_catalog:classes(aihtml_catalog:entry(?M, area_chart),
                                                          [line, stack, loading])),
    ?assertEqual([<<"ah-relation-graph">>],
                 aihtml_catalog:classes(aihtml_catalog:entry(?M, relation_graph),
                                        [tree, tb, square, directed])).

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
    Opt = #{series => []},
    ?assertEqual(r(?M:chart(Opt, [loading, <<"x">>], [{id, c}, {height, 100}, {title, <<"t">>}])),
                 r(#ah_chart{option = Opt, loading = true, css = [<<"x">>], id = c, height = 100,
                             attrs = [{title, <<"t">>}]})),
    ?assertEqual(r(?M:area_chart([{a, [1]}], [line], [{categories, [x]}, {legend, top}])),
                 r(#ah_area_chart{series = [{a, [1]}], line = true, categories = [x],
                                  legend = top})),
    ?assertEqual(r(?M:bar_chart([{a, [1]}], [horizontal], [{bar_width, 9}])),
                 r(#ah_bar_chart{series = [{a, [1]}], horizontal = true, bar_width = 9})),
    ?assertEqual(r(?M:donut_chart([{a, 1}], [pie], [{labels, false}])),
                 r(#ah_donut_chart{items = [{a, 1}], pie = true, labels = false})),
    ?assertEqual(r(?M:radar_chart([{a, [1]}], [circle], [{indicators, [{x, 2}]}])),
                 r(#ah_radar_chart{series = [{a, [1]}], shape = circle, indicators = [{x, 2}]})),
    ?assertEqual(r(?M:relation_graph({[a], []}, [tree, rl, directed], [{selected, a}])),
                 r(#ah_relation_graph{graph = {[a], []}, layout = tree, orient = rl,
                                      directed = true, selected = a})).

builder_fills_fields_test() ->
    ?assertMatch(#ah_bar_chart{series = [], stack = true, horizontal = false, title = <<"t">>,
                               css = [<<"x">>], attrs = [{role, x}]},
                 ?M:bar_chart([], [stack, <<"x">>], [{title, <<"t">>}, {role, x}])),
    ?assertMatch(#ah_relation_graph{layout = circular, node_shape = square, height = 300},
                 ?M:relation_graph({[], []}, [circular, square], [{height, 300}])),
    ?assertError({aihtml, {record_only_field, ah_chart, postback}},
                 ?M:chart(#{}, [], [{postback, x}])).

postback_test() ->
    Token = fun(Html) ->
                    {match, [Ev, Tok]} = re:run(r(Html), <<"data-ah-on=\"([a-z:-]+):([^\"]+)\"">>,
                                                [{capture, all_but_first, binary}]),
                    {ok, Ref} = aihtml_action:unsign(Tok),
                    {Ev, Ref}
            end,
    ?assertEqual({<<"ah:chart-click">>, {?MODULE, picked, #{id => 7}}},
                 Token(#ah_bar_chart{postback = {picked, #{id => 7}}})),
    ?assertEqual({<<"ah:chart-click">>, {other, go, #{}}},
                 Token(#ah_chart{postback = go, delegate = other})),
    ?assertEqual({<<"ah:select">>, {?MODULE, node, #{}}},
                 Token(#ah_relation_graph{postback = node})).

field_validation_test() ->
    ?assertError({aihtml, {bad_option, option, x}}, r(#ah_chart{option = x})),
    ?assertError({aihtml, {bad_option, option, _}}, r(#ah_chart{option = #{a => {1, 2}}})),
    ?assertError({aihtml, {bad_option, renderer, webgl}}, r(#ah_chart{renderer = webgl})),
    ?assertError({aihtml, {bad_option, height, 0}}, r(#ah_chart{height = 0})),
    ?assertError({aihtml, {bad_option, width, <<"1px;color:red">>}},
                 r(#ah_chart{width = <<"1px;color:red">>})),
    ?assertError({aihtml, {bad_flag, chart, loading, yes}}, r(#ah_chart{loading = yes})),
    ?assertError({aihtml, {bad_series, {a, [x]}}}, r(#ah_area_chart{series = [{a, [x]}]})),
    ?assertError({aihtml, {bad_option, legend, middle}}, r(#ah_area_chart{legend = middle})),
    ?assertError({aihtml, {bad_option, grid, 1}}, r(#ah_bar_chart{grid = 1})),
    ?assertError({aihtml, {bad_option, colors, [red]}}, r(#ah_bar_chart{colors = [red]})),
    ?assertError({aihtml, {bad_option, bar_width, wide}}, r(#ah_bar_chart{bar_width = wide})),
    ?assertError({aihtml, {bad_item, {a, b}}}, r(#ah_donut_chart{items = [{a, b}]})),
    ?assertError({aihtml, {bad_option, radius, 5}}, r(#ah_donut_chart{radius = 5})),
    ?assertError({aihtml, {bad_indicator, {x, y}}}, r(#ah_radar_chart{indicators = [{x, y}]})),
    ?assertError({aihtml, {bad_option, area_opacity, 2}}, r(#ah_radar_chart{area_opacity = 2})),
    ?assertError({aihtml, {bad_modifier, radar_chart, shape, star, _}},
                 r(#ah_radar_chart{shape = star})),
    ?assertError({aihtml, {bad_graph, _}}, r(#ah_relation_graph{graph = #{edges => []}})),
    ?assertError({aihtml, {unknown_category, zz}},
                 r(#ah_relation_graph{graph = #{nodes => [#{id => a, category => zz}]}})),
    ?assertError({aihtml, {bad_edge, _}},
                 r(#ah_relation_graph{graph = #{nodes => [a], edges => [#{source => a,
                                                                          target => a,
                                                                          kind => wavy}]}})),
    ?assertError({aihtml, {bad_option, edge_labels, sometimes}},
                 r(#ah_relation_graph{edge_labels = sometimes})),
    ?assertError({aihtml, {bad_modifier, relation_graph, layout, grid, _}},
                 r(#ah_relation_graph{layout = grid})),
    ?assertError({aihtml, {unknown_modifier, bar_chart, big, _}}, ?M:bar_chart([], [big], [])).

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

default(ah_chart) -> #ah_chart{};
default(ah_area_chart) -> #ah_area_chart{};
default(ah_bar_chart) -> #ah_bar_chart{};
default(ah_donut_chart) -> #ah_donut_chart{};
default(ah_radar_chart) -> #ah_radar_chart{};
default(ah_relation_graph) -> #ah_relation_graph{}.
