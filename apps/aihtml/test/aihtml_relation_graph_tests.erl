%% Tests for aihtml_relation_graph: the markup, the echarts options built
%% on the server, the record and the catalog.
-module(aihtml_relation_graph_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_relation_graph.hrl").

-define(M, aihtml_relation_graph).

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
%%% Catalog
%%%===================================================================

catalog_test() ->
    [#{name := relation_graph}] = ?M:catalog(),
    [begin
         #{flags := Fl, options := Op, option_docs := Docs, methods := Ms} = E =
             aihtml_catalog:entry(?M, N),
         ?assertEqual(lists:sort(Fl ++ Op), lists:sort(maps:keys(Docs))),
         [_ | _] = aihtml_catalog:classes(E, Fl),
         [?assert(is_binary(D)) || #{doc := D} <- Ms]
     end || #{name := N} <- ?M:catalog()],
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
    ?assertEqual(r(?M:relation_graph({[a], []}, [tree, rl, directed], [{selected, a}])),
                 r(#ah_relation_graph{graph = {[a], []}, layout = tree, orient = rl,
                                      directed = true, selected = a})).

builder_fills_fields_test() ->
    ?assertMatch(#ah_relation_graph{layout = circular, node_shape = square, height = 300},
                 ?M:relation_graph({[], []}, [circular, square], [{height, 300}])).

postback_test() ->
    Token = fun(Html) ->
                    {match, [Ev, Tok]} = re:run(r(Html), <<"data-ah-on=\"([a-z:-]+):([^\"]+)\"">>,
                                                [{capture, all_but_first, binary}]),
                    {ok, Ref} = aihtml_action:unsign(Tok),
                    {Ev, Ref}
            end,
    ?assertEqual({<<"ah:select">>, {?MODULE, node, #{}}},
                 Token(#ah_relation_graph{postback = node})).

field_validation_test() ->
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
                 r(#ah_relation_graph{layout = grid})).

records_match_catalog_test() ->
    Base = [module, id, css, attrs, postback, delegate],
    [begin
         Tag = list_to_atom("ah_" ++ atom_to_list(N)),
         Fields = ?M:fields(Tag),
         ?assertEqual(Base, lists:sublist(Fields, 6)),
         Defaults = maps:from_list(lists:zip(Fields, tl(tuple_to_list(#ah_relation_graph{})))),
         [?assertEqual({N, G, case D of none -> undefined; _ -> D end},
                       {N, G, maps:get(G, Defaults)})
          || {G, {_, D}} <- maps:to_list(maps:get(groups, E, #{}))],
         [?assertEqual({N, F, false}, {N, F, maps:get(F, Defaults)})
          || F <- maps:get(flags, E, [])],
         [?assert(lists:member(O, Fields)) || O <- maps:get(options, E, [])],
         ?assertEqual(?M, maps:get(module, Defaults))
     end || #{name := N} = E <- ?M:catalog()].
