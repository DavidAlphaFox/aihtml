-module(aihtml_tag_cloud_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_tag_cloud.hrl").

-define(D, aihtml_tag_cloud).

r(Html) -> aihtml_html:render_binary(Html).

has(Needle, Html) ->
    binary:match(r(Html), Needle) =/= nomatch.

%%%===================================================================
%%% Catalog
%%%===================================================================

catalog_names_are_exported_test() ->
    [#{name := tag_cloud, category := C}] = ?D:catalog(),
    ?assert(erlang:function_exported(?D, ah_tag_cloud, 3)),
    ?assert(lists:member(C, [form, layout, overlay, data, media, text])).

api_docs_cover_options_and_flags_test() ->
    [#{option_docs := OD, methods := Ms} = E] = ?D:catalog(),
    Docs = maps:keys(OD),
    [?assert(lists:member(K, Docs)) || K <- maps:get(options, E, [])],
    [?assert(is_binary(D)) || D <- maps:values(OD)],
    [?assertMatch(#{name := _, args := _, doc := _}, M) || M <- Ms].

records_match_catalog_test() ->
    [#{name := N} = E] = ?D:catalog(),
    Fields = ?D:fields(ah_tag_cloud),
    ?assertEqual([module, id, css, attrs, postback, delegate], lists:sublist(Fields, 6)),
    Defaults = maps:from_list(lists:zip(Fields, tl(tuple_to_list(#ah_tag_cloud{})))),
    [?assertEqual({N, G, case D of none -> undefined; _ -> D end},
                  {N, G, maps:get(G, Defaults)})
     || {G, {_, D}} <- maps:to_list(maps:get(groups, E, #{}))],
    [?assertEqual({N, F, false}, {N, F, maps:get(F, Defaults)})
     || F <- maps:get(flags, E, [])],
    [?assert(lists:member(O, Fields)) || O <- maps:get(options, E, [])],
    % the record renders with its defaults
    ?assert(is_binary(r(#ah_tag_cloud{}))),
    ?assertEqual(?D, maps:get(module, Defaults)).

%%%===================================================================
%%% Rendering
%%%===================================================================

tag_cloud_weights_test() ->
    Tags = [#{label => <<"a">>, value => 10}, #{label => <<"b">>, value => 20},
            {<<"c">>, 30, <<"/c?x=1&y">>}],
    H = ?D:ah_tag_cloud(Tags, [], []),
    ?assert(has(<<"style=\"font-size: 10px;\"">>, H)),
    ?assert(has(<<"style=\"font-size: 17px;\"">>, H)),
    ?assert(has(<<"style=\"font-size: 24px;\" href=\"/c?x=1&amp;y\"">>, H)),
    ?assert(has(<<"<ul class=\"ah-tagcloud\">">>, H)),
    ?assert(has(<<"<div class=\"ah-tagcloud\" data-ah=\"tag-cloud\">">>, H)),
    One = ?D:ah_tag_cloud([{<<"only">>, 5}], [], [{min_font_size, 1}, {max_font_size, 2},
                                                 {font_size_unit, 'rem'}]),
    ?assert(has(<<"font-size: 1.5rem;">>, One)).

tag_cloud_options_test() ->
    Tags = [{<<"b x">>, 20}, {<<"a">>, 10}, {<<"c">>, 30}],
    G = ?D:ah_tag_cloud(Tags, [], [{min_color, <<"#000000">>}, {max_color, <<"#ffffff">>}]),
    ?assert(has(<<"color: rgb(128,128,128);\"">>, G)),
    ?assert(has(<<"color: rgb(0,0,0);\"">>, G)),
    S = r(?D:ah_tag_cloud(Tags, [], [{sort_by, value}, {sort_order, descending},
                                     {text_case, title_case}, {display_value, true}])),
    {P1, _} = binary:match(S, <<">C (30)<">>),
    {P2, _} = binary:match(S, <<">B X (20)<">>),
    ?assert(P1 < P2),
    L = ?D:ah_tag_cloud(Tags, [], [{display_limit, 2}, {take_top_weighted, true}]),
    ?assertNot(has(<<">a<">>, L)),
    ?assert(has(<<">b x<">>, L)),
    F = ?D:ah_tag_cloud(Tags, [], [{min_value, 15}, {max_value, 25}]),
    ?assertNot(has(<<">c<">>, F)),
    ?assertError({aihtml, {bad_color, _}},
                 r(?D:ah_tag_cloud(Tags, [], [{min_color, <<"red">>}, {max_color, <<"#fff000">>}]))),
    ?assertError({aihtml, {bad_unit, _}}, r(?D:ah_tag_cloud(Tags, [], [{font_size_unit, <<"px;x">>}]))).

tag_cloud_escaping_test() ->
    H = ?D:ah_tag_cloud([{<<"<script>">>, 1}], [disabled], []),
    ?assert(has(<<"&lt;script&gt;">>, H)),
    ?assertNot(has(<<"<script>">>, H)),
    ?assert(has(<<"class=\"ah-tagcloud ah-tagcloud-disabled\"">>, H)).

%%%===================================================================
%%% element record (designs/05-records.md)
%%%===================================================================

record_equals_builder_test() ->
    ?assertEqual(r(?D:ah_tag_cloud([{<<"a">>, 1}, {<<"b">>, 2}], [], [{sort_by, value},
                                                                     {sort_order, descending}])),
                 r(#ah_tag_cloud{items = [{<<"a">>, 1}, {<<"b">>, 2}], sort_by = value,
                                 sort_order = descending})).

postback_test() ->
    Token = fun(Html) ->
                    {match, [Ev, Tok]} = re:run(r(Html), <<"data-ah-on=\"((?:ah:)?[a-z-]+):([^\":]+)\"">>,
                                                [{capture, all_but_first, binary}]),
                    {ok, Ref} = aihtml_action:unsign(Tok),
                    {Ev, Ref}
            end,
    ?assertMatch({<<"ah:tag-click">>, _}, Token(#ah_tag_cloud{postback = p})).
