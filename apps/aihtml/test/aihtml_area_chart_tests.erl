%% Tests for aihtml_area_chart: the echarts option built on the server, the
%% record and the catalog.
-module(aihtml_area_chart_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_area_chart.hrl").

-define(M, aihtml_area_chart).

r(Html) -> aihtml_html:render_binary(Html).

%% The option in a rendered chart's data island, decoded.
island(Html) ->
    {match, [Json]} = re:run(r(Html), <<"<script class=\"ah-chart-data\" type=\"application/json\">"
                                        "(.*?)</script>">>,
                             [{capture, all_but_first, binary}, dotall]),
    json:decode(Json).

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

%%%===================================================================
%%% Catalog
%%%===================================================================

catalog_test() ->
    [#{name := area_chart}] = ?M:catalog(),
    [begin
         #{flags := Fl, options := Op, option_docs := Docs, methods := Ms} = E =
             aihtml_catalog:entry(?M, N),
         ?assertEqual(lists:sort(Fl ++ Op), lists:sort(maps:keys(Docs))),
         [_ | _] = aihtml_catalog:classes(E, Fl),
         [?assert(is_binary(D)) || #{doc := D} <- Ms]
     end || #{name := N} <- ?M:catalog()],
    ?assertEqual([<<"ah-chart">>], aihtml_catalog:classes(aihtml_catalog:entry(?M, area_chart),
                                                          [line, stack, loading])).

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
    ?assertEqual(r(?M:area_chart([{a, [1]}], [line], [{categories, [x]}, {legend, top}])),
                 r(#ah_area_chart{series = [{a, [1]}], line = true, categories = [x],
                                  legend = top})).

field_validation_test() ->
    ?assertError({aihtml, {bad_series, {a, [x]}}}, r(#ah_area_chart{series = [{a, [x]}]})),
    ?assertError({aihtml, {bad_option, legend, middle}}, r(#ah_area_chart{legend = middle})).

records_match_catalog_test() ->
    Base = [module, id, css, attrs, postback, delegate],
    [begin
         Tag = list_to_atom("ah_" ++ atom_to_list(N)),
         Fields = ?M:fields(Tag),
         ?assertEqual(Base, lists:sublist(Fields, 6)),
         Defaults = maps:from_list(lists:zip(Fields, tl(tuple_to_list(#ah_area_chart{})))),
         [?assertEqual({N, G, case D of none -> undefined; _ -> D end},
                       {N, G, maps:get(G, Defaults)})
          || {G, {_, D}} <- maps:to_list(maps:get(groups, E, #{}))],
         [?assertEqual({N, F, false}, {N, F, maps:get(F, Defaults)})
          || F <- maps:get(flags, E, [])],
         [?assert(lists:member(O, Fields)) || O <- maps:get(options, E, [])],
         ?assertEqual(?M, maps:get(module, Defaults))
     end || #{name := N} = E <- ?M:catalog()].
