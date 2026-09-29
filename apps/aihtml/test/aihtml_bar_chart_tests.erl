%% Tests for aihtml_bar_chart: the echarts option built on the server, the
%% record and the catalog.
-module(aihtml_bar_chart_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_bar_chart.hrl").

-define(M, aihtml_bar_chart).

r(Html) -> aihtml_html:render_binary(Html).

%% The option in a rendered chart's data island, decoded.
island(Html) ->
    {match, [Json]} = re:run(r(Html), <<"<script class=\"ah-chart-data\" type=\"application/json\">"
                                        "(.*?)</script>">>,
                             [{capture, all_but_first, binary}, dotall]),
    json:decode(Json).

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

%%%===================================================================
%%% Catalog
%%%===================================================================

catalog_test() ->
    [#{name := bar_chart}] = ?M:catalog(),
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
    ?assertEqual(r(?M:bar_chart([{a, [1]}], [horizontal], [{id, c}, {bar_width, 9}])),
                 r(#ah_bar_chart{series = [{a, [1]}], horizontal = true, bar_width = 9, id = c})).

builder_fills_fields_test() ->
    ?assertMatch(#ah_bar_chart{series = [], stack = true, horizontal = false, title = <<"t">>,
                               css = [<<"x">>], attrs = [{role, x}]},
                 ?M:bar_chart([], [stack, <<"x">>], [{title, <<"t">>}, {role, x}])).

postback_test() ->
    Token = fun(Html) ->
                    {match, [Ev, Tok]} = re:run(r(Html), <<"data-ah-on=\"([a-z:-]+):([^\"]+)\"">>,
                                                [{capture, all_but_first, binary}]),
                    {ok, Ref} = aihtml_action:unsign(Tok),
                    {Ev, Ref}
            end,
    ?assertEqual({<<"ah:chart-click">>, {?MODULE, picked, #{id => 7}}},
                 Token(#ah_bar_chart{postback = {picked, #{id => 7}}})).

field_validation_test() ->
    ?assertError({aihtml, {bad_option, grid, 1}}, r(#ah_bar_chart{grid = 1})),
    ?assertError({aihtml, {bad_option, colors, [red]}}, r(#ah_bar_chart{colors = [red]})),
    ?assertError({aihtml, {bad_option, bar_width, wide}}, r(#ah_bar_chart{bar_width = wide})),
    ?assertError({aihtml, {unknown_modifier, bar_chart, big, _}}, ?M:bar_chart([], [big], [])).

records_match_catalog_test() ->
    Base = [module, id, css, attrs, postback, delegate],
    [begin
         Tag = list_to_atom("ah_" ++ atom_to_list(N)),
         Fields = ?M:fields(Tag),
         ?assertEqual(Base, lists:sublist(Fields, 6)),
         Defaults = maps:from_list(lists:zip(Fields, tl(tuple_to_list(#ah_bar_chart{})))),
         [?assertEqual({N, G, case D of none -> undefined; _ -> D end},
                       {N, G, maps:get(G, Defaults)})
          || {G, {_, D}} <- maps:to_list(maps:get(groups, E, #{}))],
         [?assertEqual({N, F, false}, {N, F, maps:get(F, Defaults)})
          || F <- maps:get(flags, E, [])],
         [?assert(lists:member(O, Fields)) || O <- maps:get(options, E, [])],
         ?assertEqual(?M, maps:get(module, Defaults))
     end || #{name := N} = E <- ?M:catalog()].
