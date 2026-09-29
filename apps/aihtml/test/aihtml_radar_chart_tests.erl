%% Tests for aihtml_radar_chart: the echarts option built on the server,
%% the record and the catalog.
-module(aihtml_radar_chart_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_radar_chart.hrl").

-define(M, aihtml_radar_chart).

r(Html) -> aihtml_html:render_binary(Html).

%% The option in a rendered chart's data island, decoded.
island(Html) ->
    {match, [Json]} = re:run(r(Html), <<"<script class=\"ah-chart-data\" type=\"application/json\">"
                                        "(.*?)</script>">>,
                             [{capture, all_but_first, binary}, dotall]),
    json:decode(Json).

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
%%% Catalog
%%%===================================================================

catalog_test() ->
    [#{name := radar_chart}] = ?M:catalog(),
    [begin
         #{flags := Fl, options := Op, option_docs := Docs, methods := Ms} = E =
             aihtml_catalog:entry(?M, N),
         ?assertEqual(lists:sort(Fl ++ Op), lists:sort(maps:keys(Docs))),
         [_ | _] = aihtml_catalog:classes(E, Fl),
         [?assert(is_binary(D)) || #{doc := D} <- Ms]
     end || #{name := N} <- ?M:catalog()].

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
    ?assertEqual(r(?M:radar_chart([{a, [1]}], [circle], [{indicators, [{x, 2}]}])),
                 r(#ah_radar_chart{series = [{a, [1]}], shape = circle, indicators = [{x, 2}]})).

field_validation_test() ->
    ?assertError({aihtml, {bad_indicator, {x, y}}}, r(#ah_radar_chart{indicators = [{x, y}]})),
    ?assertError({aihtml, {bad_option, area_opacity, 2}}, r(#ah_radar_chart{area_opacity = 2})),
    ?assertError({aihtml, {bad_modifier, radar_chart, shape, star, _}},
                 r(#ah_radar_chart{shape = star})).

records_match_catalog_test() ->
    Base = [module, id, css, attrs, postback, delegate],
    [begin
         Tag = list_to_atom("ah_" ++ atom_to_list(N)),
         Fields = ?M:fields(Tag),
         ?assertEqual(Base, lists:sublist(Fields, 6)),
         Defaults = maps:from_list(lists:zip(Fields, tl(tuple_to_list(#ah_radar_chart{})))),
         [?assertEqual({N, G, case D of none -> undefined; _ -> D end},
                       {N, G, maps:get(G, Defaults)})
          || {G, {_, D}} <- maps:to_list(maps:get(groups, E, #{}))],
         [?assertEqual({N, F, false}, {N, F, maps:get(F, Defaults)})
          || F <- maps:get(flags, E, [])],
         [?assert(lists:member(O, Fields)) || O <- maps:get(options, E, [])],
         ?assertEqual(?M, maps:get(module, Defaults))
     end || #{name := N} = E <- ?M:catalog()].
