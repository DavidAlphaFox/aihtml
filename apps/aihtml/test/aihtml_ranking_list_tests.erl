-module(aihtml_ranking_list_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_ranking_list.hrl").

-define(D, aihtml_ranking_list).

r(Html) -> aihtml_html:render_binary(Html).

has(Needle, Html) ->
    binary:match(r(Html), Needle) =/= nomatch.

%%%===================================================================
%%% Catalog
%%%===================================================================

catalog_names_are_exported_test() ->
    [#{name := ranking_list, category := C}] = ?D:catalog(),
    ?assert(erlang:function_exported(?D, ranking_list, 3)),
    ?assert(lists:member(C, [form, layout, overlay, data, media, text])).

api_docs_cover_options_and_flags_test() ->
    [#{option_docs := OD, methods := Ms} = E] = ?D:catalog(),
    Docs = maps:keys(OD),
    [?assert(lists:member(K, Docs)) || K <- maps:get(options, E, [])],
    [?assert(is_binary(D)) || D <- maps:values(OD)],
    [?assertMatch(#{name := _, args := _, doc := _}, M) || M <- Ms].

records_match_catalog_test() ->
    [#{name := N} = E] = ?D:catalog(),
    Fields = ?D:fields(ah_ranking_list),
    ?assertEqual([module, id, css, attrs, postback, delegate], lists:sublist(Fields, 6)),
    Defaults = maps:from_list(lists:zip(Fields, tl(tuple_to_list(#ah_ranking_list{})))),
    [?assertEqual({N, G, case D of none -> undefined; _ -> D end},
                  {N, G, maps:get(G, Defaults)})
     || {G, {_, D}} <- maps:to_list(maps:get(groups, E, #{}))],
    [?assertEqual({N, F, false}, {N, F, maps:get(F, Defaults)})
     || F <- maps:get(flags, E, [])],
    [?assert(lists:member(O, Fields)) || O <- maps:get(options, E, [])],
    % the record renders with its defaults
    ?assert(is_binary(r(#ah_ranking_list{}))),
    ?assertEqual(?D, maps:get(module, Defaults)).

%%%===================================================================
%%% Rendering
%%%===================================================================

ranking_list_test() ->
    Items = [#{name => <<"<DE>">>, code => de, value => 10, tag => <<"Free">>, sub_value => <<"s">>},
             #{name => <<"US">>, code => <<"US">>, value => 9, tag => <<"Beta">>},
             #{name => <<"X">>, value => 1, rank => 7}],
    H = ?D:ranking_list(Items, [dense, clickable], [{title, <<"Top">>},
                                                    {tag_colors, #{<<"Beta">> => error}}]),
    ?assert(has(<<"class=\"ah-ranking-list ah-ranking-list--dense\"">>, H)),
    ?assert(has(<<"<span class=\"ah-ranking-list__rank\" data-rank=\"1\">1</span>">>, H)),
    ?assert(has(<<"data-rank=\"7\">7<">>, H)),
    ?assert(has(<<"🇩🇪"/utf8>>, H)),
    ?assert(has(<<"🇺🇸"/utf8>>, H)),
    ?assert(has(<<"&lt;DE&gt;">>, H)),
    ?assert(has(<<"ah-ranking-list__tag ah-ranking-list__tag--success\">Free">>, H)),
    ?assert(has(<<"ah-ranking-list__tag ah-ranking-list__tag--error\">Beta">>, H)),
    ?assert(has(<<"ah-ranking-list__item ah-ranking-list__item--clickable\" data-idx=\"0\" role=\"button\"">>, H)),
    M = ?D:ranking_list(Items, [], [{max_items, 1}, {show_rank, false}, {flag_style, none}]),
    ?assertNot(has(<<"US">>, M)),
    ?assertNot(has(<<"ah-ranking-list__rank">>, M)),
    ?assertNot(has(<<"ah-ranking-list__flag">>, M)).

%%%===================================================================
%%% element record (designs/05-records.md)
%%%===================================================================

postback_test() ->
    Token = fun(Html) ->
                    {match, [Ev, Tok]} = re:run(r(Html), <<"data-ah-on=\"((?:ah:)?[a-z-]+):([^\":]+)\"">>,
                                                [{capture, all_but_first, binary}]),
                    {ok, Ref} = aihtml_action:unsign(Tok),
                    {Ev, Ref}
            end,
    ?assertMatch({<<"ah:item-click">>, _}, Token(#ah_ranking_list{clickable = true, postback = p})).

field_validation_test() ->
    ?assertError({aihtml, {bad_flag_style, round}},
                 r(#ah_ranking_list{items = [#{name => <<"a">>, code => de}],
                                    flag_style = round})).
