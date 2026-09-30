%% Tests for aihtml_donut_chart: the echarts option built on the server,
%% the record and the catalog.
-module(aihtml_donut_chart_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_donut_chart.hrl").

-define(M, aihtml_donut_chart).

r(Html) -> aihtml_html:render_binary(Html).

%% The option in a rendered chart's data island, decoded.
island(Html) ->
    {match, [Json]} = re:run(r(Html), <<"<script class=\"ah-chart-data\" type=\"application/json\">"
                                        "(.*?)</script>">>,
                             [{capture, all_but_first, binary}, dotall]),
    json:decode(Json).

donut_option_test() ->
    O = island(?M:ah_donut_chart([{a, 1}, #{name => b, value => 2.5, color => <<"red">>}], [],
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
    O2 = island(?M:ah_donut_chart([{a, 1}], [pie], [{labels, false}, {legend, bottom},
                                                    {center, {10, 20}}])),
    #{<<"series">> := [#{<<"radius">> := [0, <<"70%">>], <<"center">> := [10, 20],
                         <<"label">> := #{<<"show">> := false}}],
      <<"legend">> := #{<<"bottom">> := 0}} = O2.

%% The slices as a visually hidden table, the title as its caption.
donut_text_test() ->
    H = r(?M:ah_donut_chart([{a, 1}, {b, 2}], [], [{id, d}, {title, <<"Share">>}])),
    ?assertMatch({_, _}, binary:match(H, <<"aria-describedby=\"d-data\"">>)),
    ?assertMatch({_, _}, binary:match(H, <<
        "<div class=\"ah-chart-text ah-sr-only\" id=\"d-data\"><table><caption>Share</caption>"
        "<thead><tr><th scope=\"col\">Name</th><th scope=\"col\">Value</th>"
        "<th scope=\"col\">Share</th></tr></thead><tbody>"
        "<tr><th scope=\"row\">a</th><td>1</td><td>33.3%</td></tr>"
        "<tr><th scope=\"row\">b</th><td>2</td><td>66.7%</td></tr></tbody></table></div>">>)).

%%%===================================================================
%%% Catalog
%%%===================================================================

catalog_test() ->
    [#{name := donut_chart}] = ?M:catalog(),
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
    ?assertEqual(r(?M:ah_donut_chart([{a, 1}], [pie], [{id, c}, {labels, false}])),
                 r(#ah_donut_chart{items = [{a, 1}], pie = true, labels = false, id = c})).

field_validation_test() ->
    ?assertError({aihtml, {bad_item, {a, b}}}, r(#ah_donut_chart{items = [{a, b}]})),
    ?assertError({aihtml, {bad_option, radius, 5}}, r(#ah_donut_chart{radius = 5})).

records_match_catalog_test() ->
    Base = [module, id, css, attrs, postback, delegate],
    [begin
         Tag = list_to_atom("ah_" ++ atom_to_list(N)),
         Fields = ?M:fields(Tag),
         ?assertEqual(Base, lists:sublist(Fields, 6)),
         Defaults = maps:from_list(lists:zip(Fields, tl(tuple_to_list(#ah_donut_chart{})))),
         [?assertEqual({N, G, case D of none -> undefined; _ -> D end},
                       {N, G, maps:get(G, Defaults)})
          || {G, {_, D}} <- maps:to_list(maps:get(groups, E, #{}))],
         [?assertEqual({N, F, false}, {N, F, maps:get(F, Defaults)})
          || F <- maps:get(flags, E, [])],
         [?assert(lists:member(O, Fields)) || O <- maps:get(options, E, [])],
         ?assertEqual(?M, maps:get(module, Defaults))
     end || #{name := N} = E <- ?M:catalog()].
