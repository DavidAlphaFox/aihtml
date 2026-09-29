-module(aihtml_statistic_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_statistic.hrl").

-define(D, aihtml_statistic).

r(Html) -> aihtml_html:render_binary(Html).

has(Needle, Html) ->
    binary:match(r(Html), Needle) =/= nomatch.

%%%===================================================================
%%% Catalog
%%%===================================================================

catalog_names_are_exported_test() ->
    [#{name := statistic, category := C}] = ?D:catalog(),
    ?assert(erlang:function_exported(?D, statistic, 3)),
    ?assert(lists:member(C, [form, layout, overlay, data, media, text])).

api_docs_cover_options_and_flags_test() ->
    [#{option_docs := OD, methods := Ms} = E] = ?D:catalog(),
    Docs = maps:keys(OD),
    [?assert(lists:member(K, Docs)) || K <- maps:get(options, E, [])],
    [?assert(is_binary(D)) || D <- maps:values(OD)],
    [?assertMatch(#{name := _, args := _, doc := _}, M) || M <- Ms].

records_match_catalog_test() ->
    [#{name := N} = E] = ?D:catalog(),
    Fields = ?D:fields(ah_statistic),
    ?assertEqual([module, id, css, attrs, postback, delegate], lists:sublist(Fields, 6)),
    Defaults = maps:from_list(lists:zip(Fields, tl(tuple_to_list(#ah_statistic{})))),
    [?assertEqual({N, G, case D of none -> undefined; _ -> D end},
                  {N, G, maps:get(G, Defaults)})
     || {G, {_, D}} <- maps:to_list(maps:get(groups, E, #{}))],
    [?assertEqual({N, F, false}, {N, F, maps:get(F, Defaults)})
     || F <- maps:get(flags, E, [])],
    [?assert(lists:member(O, Fields)) || O <- maps:get(options, E, [])],
    % the record renders with its defaults
    ?assert(is_binary(r(#ah_statistic{}))),
    ?assertEqual(?D, maps:get(module, Defaults)).

%%%===================================================================
%%% Rendering
%%%===================================================================

statistic_format_test() ->
    N = fun(V, O) ->
                {match, [S]} = re:run(r(?D:statistic(V, [], O)),
                                      "ah-statistic__number\">([^<]*)<",
                                      [{capture, all_but_first, binary}, unicode]),
                S
        end,
    ?assertEqual(<<"1,284,500">>, N(1284500, [])),
    ?assertEqual(<<"1284500">>, N(1284500, [{group_separator, false}])),
    ?assertEqual(<<"-1,234.50">>, N(-1234.5, [{precision, 2}])),
    ?assertEqual(<<"999">>, N(999, [])),
    ?assertEqual(<<"1,000">>, N(1000.0, [])),
    ?assertEqual(<<"3">>, N(2.6, [{precision, 0}])),
    ?assertEqual(<<"n/a">>, N(<<"n/a">>, [])).

statistic_markup_test() ->
    H = ?D:statistic(5, [success], [{title, <<"T">>}, {prefix, <<"$">>}, {suffix, <<"%">>},
                                    {delta, -1.5}, {precision, 1}]),
    ?assert(has(<<"data-color=\"success\" data-loading=\"false\"">>, H)),
    ?assert(has(<<"<span class=\"ah-statistic__prefix\">$</span>">>, H)),
    ?assert(has(<<"data-direction=\"down\"">>, H)),
    ?assert(has(<<"<span>1.5</span>">>, H)),
    ?assert(has(<<"data-direction=\"flat\"">>, ?D:statistic(1, [], [{delta, 0}]))),
    ?assertNot(has(<<"ah-statistic__delta">>, ?D:statistic(1, [], []))),
    ?assert(has(<<"data-loading=\"true\"">>, ?D:statistic(1, [loading], []))).

%%%===================================================================
%%% element record (designs/05-records.md)
%%%===================================================================

builder_fills_fields_test() ->
    ?assertMatch(#ah_statistic{value = 5, color = default, precision = 1, loading = true},
                 ?D:statistic(5, [loading], [{precision, 1}])).

postback_test() ->
    ?assertError({aihtml, {no_postback_event, ah_statistic}}, r(#ah_statistic{postback = p})).
