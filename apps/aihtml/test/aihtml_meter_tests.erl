-module(aihtml_meter_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_meter.hrl").

-define(D, aihtml_meter).

r(Html) -> aihtml_html:render_binary(Html).

has(Needle, Html) ->
    binary:match(r(Html), Needle) =/= nomatch.

%%%===================================================================
%%% Catalog
%%%===================================================================

catalog_names_are_exported_test() ->
    [#{name := meter, category := C}] = ?D:catalog(),
    ?assert(erlang:function_exported(?D, ah_meter, 3)),
    ?assert(lists:member(C, [form, layout, overlay, data, media, text])).

api_docs_cover_options_and_flags_test() ->
    [#{option_docs := OD, methods := Ms} = E] = ?D:catalog(),
    Docs = maps:keys(OD),
    [?assert(lists:member(K, Docs)) || K <- maps:get(options, E, [])],
    [?assert(is_binary(D)) || D <- maps:values(OD)],
    [?assertMatch(#{name := _, args := _, doc := _}, M) || M <- Ms].

records_match_catalog_test() ->
    [#{name := N} = E] = ?D:catalog(),
    Fields = ?D:fields(ah_meter),
    ?assertEqual([module, id, css, attrs, postback, delegate], lists:sublist(Fields, 6)),
    Defaults = maps:from_list(lists:zip(Fields, tl(tuple_to_list(#ah_meter{})))),
    [?assertEqual({N, G, case D of none -> undefined; _ -> D end},
                  {N, G, maps:get(G, Defaults)})
     || {G, {_, D}} <- maps:to_list(maps:get(groups, E, #{}))],
    [?assertEqual({N, F, false}, {N, F, maps:get(F, Defaults)})
     || F <- maps:get(flags, E, [])],
    [?assert(lists:member(O, Fields)) || O <- maps:get(options, E, [])],
    % the record renders with its defaults
    ?assert(is_binary(r(#ah_meter{}))),
    ?assertEqual(?D, maps:get(module, Defaults)).

%%%===================================================================
%%% Rendering
%%%===================================================================

meter_state_test() ->
    St = fun(V, O) ->
                 {match, [S]} = re:run(r(?D:ah_meter(V, [], O)), "data-state=\"([a-z]+)\"",
                                       [{capture, all_but_first, binary}]),
                 S
         end,
    T = [{low, 25}, {high, 75}],
    ?assertEqual(<<"low">>, St(10, T)),
    ?assertEqual(<<"optimum">>, St(50, T)),
    ?assertEqual(<<"high">>, St(90, T)),
    ?assertEqual(<<"optimum">>, St(90, [{optimum, 90} | T])),
    ?assertEqual(<<"low">>, St(10, [{optimum, 90} | T])),
    ?assertEqual(<<"low">>, St(90, [{optimum, 5} | T])),
    ?assertEqual(<<"optimum">>, St(10, [{optimum, 5} | T])).

meter_markup_test() ->
    H = ?D:ah_meter(30, [lg], [{min, 20}, {max, 40}, {label, <<"CPU&">>}, {show_value, true},
                                {helper_text, <<"h">>}]),
    ?assert(has(<<"class=\"ah-meter\" data-size=\"lg\"">>, H)),
    ?assert(has(<<"<span class=\"ah-meter__label\">CPU&amp;</span><span class=\"ah-meter__value\">30</span>">>, H)),
    ?assert(has(<<"role=\"meter\" aria-valuenow=\"30\" aria-valuemin=\"20\" aria-valuemax=\"40\"">>, H)),
    ?assert(has(<<"style=\"width: 50%;\"">>, H)),
    ?assert(has(<<"<div class=\"ah-meter__helper\">h</div>">>, H)),
    ?assertNot(has(<<"ah-meter__head">>, ?D:ah_meter(1, [], []))).

%%%===================================================================
%%% element record (designs/05-records.md)
%%%===================================================================

postback_test() ->
    ?assertError({aihtml, {no_postback_event, ah_meter}}, r(#ah_meter{postback = p})).
