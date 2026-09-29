-module(aihtml_kpi_card_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_kpi_card.hrl").

-define(D, aihtml_kpi_card).

r(Html) -> aihtml_html:render_binary(Html).

has(Needle, Html) ->
    binary:match(r(Html), Needle) =/= nomatch.

%%%===================================================================
%%% Catalog
%%%===================================================================

catalog_names_are_exported_test() ->
    [#{name := kpi_card, category := C}] = ?D:catalog(),
    ?assert(erlang:function_exported(?D, kpi_card, 3)),
    ?assert(lists:member(C, [form, layout, overlay, data, media, text])).

api_docs_cover_options_and_flags_test() ->
    [#{option_docs := OD, methods := Ms} = E] = ?D:catalog(),
    Docs = maps:keys(OD),
    [?assert(lists:member(K, Docs)) || K <- maps:get(options, E, [])],
    [?assert(is_binary(D)) || D <- maps:values(OD)],
    [?assertMatch(#{name := _, args := _, doc := _}, M) || M <- Ms].

records_match_catalog_test() ->
    [#{name := N} = E] = ?D:catalog(),
    Fields = ?D:fields(ah_kpi_card),
    ?assertEqual([module, id, css, attrs, postback, delegate], lists:sublist(Fields, 6)),
    Defaults = maps:from_list(lists:zip(Fields, tl(tuple_to_list(#ah_kpi_card{})))),
    [?assertEqual({N, G, case D of none -> undefined; _ -> D end},
                  {N, G, maps:get(G, Defaults)})
     || {G, {_, D}} <- maps:to_list(maps:get(groups, E, #{}))],
    [?assertEqual({N, F, false}, {N, F, maps:get(F, Defaults)})
     || F <- maps:get(flags, E, [])],
    [?assert(lists:member(O, Fields)) || O <- maps:get(options, E, [])],
    % the record renders with its defaults
    ?assert(is_binary(r(#ah_kpi_card{}))),
    ?assertEqual(?D, maps:get(module, Defaults)).

%%%===================================================================
%%% Rendering
%%%===================================================================

kpi_card_test() ->
    H = ?D:kpi_card(<<"1<2">>, [success], [{title, <<"Users">>}, {trend, 5}, {icon, users},
                                           {trend_label, <<"vs">>}]),
    ?assert(has(<<"class=\"ah-kpi-card ah-kpi-card--success ah-kpi-card-trend-up\"">>, H)),
    ?assert(has(<<"<span class=\"ah-kpi-card-trend-value\">+5.0%</span>">>, H)),
    ?assert(has(<<"<div class=\"ah-kpi-card-value\">1&lt;2</div>">>, H)),
    ?assert(has(<<"ah-kpi-card-icon-wrapper">>, H)),
    ?assert(has(<<"ah-kpi-card-trend-label\">vs<">>, H)),
    D = ?D:kpi_card(<<"1">>, [disabled], [{trend, -3.25}]),
    ?assert(has(<<"class=\"ah-kpi-card ah-kpi-card-disabled ah-kpi-card-trend-down\"">>, D)),
    ?assert(has(<<">-3.3%<">>, D) orelse has(<<">-3.2%<">>, D)),
    ?assertNot(has(<<"ah-kpi-card-trend">>, ?D:kpi_card(<<"1">>, [], []))),
    ?assertError({aihtml, {unknown_icon, rocket}}, r(?D:kpi_card(<<"1">>, [], [{icon, rocket}]))).

%%%===================================================================
%%% element record (designs/05-records.md)
%%%===================================================================

record_equals_builder_test() ->
    ?assertEqual(r(?D:kpi_card(<<"845">>, [warning, <<"p-2">>],
                               [{title, <<"Installs">>}, {trend, -3.5}, {icon, install},
                                {data_x, 1}])),
                 r(#ah_kpi_card{value = <<"845">>, color = warning, css = [<<"p-2">>],
                                title = <<"Installs">>, trend = -3.5, icon = install,
                                attrs = [{data_x, 1}]})).

postback_test() ->
    ?assertError({aihtml, {no_postback_event, ah_kpi_card}}, r(#ah_kpi_card{postback = p})).

field_validation_test() ->
    ?assertError({aihtml, {unknown_icon, rocket}}, r(#ah_kpi_card{icon = rocket})),
    %% kpi_card's colour group has no default and may stay undefined
    ?assert(has(<<"class=\"ah-kpi-card\"">>, #ah_kpi_card{value = <<"1">>})).
