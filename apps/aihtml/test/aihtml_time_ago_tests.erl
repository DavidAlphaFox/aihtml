-module(aihtml_time_ago_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_time_ago.hrl").

-define(D, aihtml_time_ago).

r(Html) -> aihtml_html:render_binary(Html).

has(Needle, Html) ->
    binary:match(r(Html), Needle) =/= nomatch.

%%%===================================================================
%%% Catalog
%%%===================================================================

catalog_names_are_exported_test() ->
    [#{name := time_ago, category := C}] = ?D:catalog(),
    ?assert(erlang:function_exported(?D, time_ago, 3)),
    ?assert(lists:member(C, [form, layout, overlay, data, media, text])).

api_docs_cover_options_and_flags_test() ->
    [#{option_docs := OD, methods := Ms} = E] = ?D:catalog(),
    Docs = maps:keys(OD),
    [?assert(lists:member(K, Docs)) || K <- maps:get(options, E, [])],
    [?assert(is_binary(D)) || D <- maps:values(OD)],
    [?assertMatch(#{name := _, args := _, doc := _}, M) || M <- Ms].

records_match_catalog_test() ->
    [#{name := N} = E] = ?D:catalog(),
    Fields = ?D:fields(ah_time_ago),
    ?assertEqual([module, id, css, attrs, postback, delegate], lists:sublist(Fields, 6)),
    Defaults = maps:from_list(lists:zip(Fields, tl(tuple_to_list(#ah_time_ago{})))),
    [?assertEqual({N, G, case D of none -> undefined; _ -> D end},
                  {N, G, maps:get(G, Defaults)})
     || {G, {_, D}} <- maps:to_list(maps:get(groups, E, #{}))],
    [?assertEqual({N, F, false}, {N, F, maps:get(F, Defaults)})
     || F <- maps:get(flags, E, [])],
    [?assert(lists:member(O, Fields)) || O <- maps:get(options, E, [])],
    ?assertEqual(?D, maps:get(module, Defaults)).

%%%===================================================================
%%% Rendering
%%%===================================================================

time_ago_format_test() ->
    Now = 1790000000,
    T = fun(Ago) -> ?D:time_ago(Now - Ago, [], [{now, Now}]) end,
    ?assert(has(<<">just now</time>">>, T(59))),
    ?assert(has(<<">1m ago</time>">>, T(60))),
    ?assert(has(<<">59m ago</time>">>, T(3599))),
    ?assert(has(<<">2h ago</time>">>, T(7200))),
    ?assert(has(<<">29d ago</time>">>, T(29 * 86400))),
    ?assert(has(<<">1mo ago</time>">>, T(30 * 86400))),
    ?assert(has(<<">just now</time>">>, T(-500))).

time_ago_markup_test() ->
    Now = calendar:datetime_to_gregorian_seconds({{2026, 7, 22}, {8, 5, 0}}) - 62167219200,
    H = ?D:time_ago({{2026, 7, 22}, {8, 0, 0}}, [], [{now, Now}]),
    ?assertEqual(<<"<time class=\"ah-time-ago\" datetime=\"2026-07-22T08:00:00Z\" "
                   "title=\"2026-07-22 08:00:00 UTC\" data-ah=\"time-ago\" data-ah-title=\"true\">"
                   "5m ago</time>">>, r(H)),
    ?assert(has(<<"datetime=\"2026-07-22T08:00:00Z\"">>,
                ?D:time_ago(<<"2026-07-22T08:00:00Z">>, [], [{now, Now}]))),
    L = ?D:time_ago(Now - 120, [], [{now, Now}, {live, false}, {title, false},
                                    {labels, #{minutes => <<"<{n}> min">>}}]),
    ?assert(has(<<"data-ah-live=\"false\" data-ah-label-minutes=\"&lt;{n}&gt; min\">&lt;2&gt; min</time>">>, L)),
    ?assertNot(has(<<"title=">>, L)),
    ?assertError({aihtml, {bad_timestamp, _}}, r(?D:time_ago(<<"yesterday">>, [], []))).

%%%===================================================================
%%% element record (designs/05-records.md)
%%%===================================================================

postback_test() ->
    ?assertError({aihtml, {no_postback_event, ah_time_ago}}, r(#ah_time_ago{timestamp = 0, postback = p})).
