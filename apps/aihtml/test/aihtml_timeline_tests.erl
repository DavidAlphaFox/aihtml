-module(aihtml_timeline_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_timeline.hrl").

-define(D, aihtml_timeline).

r(Html) -> aihtml_html:render_binary(Html).

has(Needle, Html) ->
    binary:match(r(Html), Needle) =/= nomatch.

%%%===================================================================
%%% Catalog
%%%===================================================================

catalog_names_are_exported_test() ->
    [#{name := timeline, category := C}] = ?D:catalog(),
    ?assert(erlang:function_exported(?D, ah_timeline, 3)),
    ?assert(lists:member(C, [form, layout, overlay, data, media, text])).

api_docs_cover_options_and_flags_test() ->
    [#{option_docs := OD, methods := Ms} = E] = ?D:catalog(),
    Docs = maps:keys(OD),
    [?assert(lists:member(K, Docs)) || K <- maps:get(options, E, [])],
    [?assert(is_binary(D)) || D <- maps:values(OD)],
    [?assertMatch(#{name := _, args := _, doc := _}, M) || M <- Ms].

records_match_catalog_test() ->
    [#{name := N} = E] = ?D:catalog(),
    Fields = ?D:fields(ah_timeline),
    ?assertEqual([module, id, css, attrs, postback, delegate], lists:sublist(Fields, 6)),
    Defaults = maps:from_list(lists:zip(Fields, tl(tuple_to_list(#ah_timeline{})))),
    [?assertEqual({N, G, case D of none -> undefined; _ -> D end},
                  {N, G, maps:get(G, Defaults)})
     || {G, {_, D}} <- maps:to_list(maps:get(groups, E, #{}))],
    [?assertEqual({N, F, false}, {N, F, maps:get(F, Defaults)})
     || F <- maps:get(flags, E, [])],
    [?assert(lists:member(O, Fields)) || O <- maps:get(options, E, [])],
    % the record renders with its defaults
    ?assert(is_binary(r(#ah_timeline{}))),
    ?assertEqual(?D, maps:get(module, Defaults)).

%%%===================================================================
%%% Rendering
%%%===================================================================

timeline_test() ->
    Items = [#{date => <<"d0">>, title => <<"<t0>">>, description => <<"x">>},
             #{date => <<"d1">>, title => <<"t1">>, dot => success, expanded => true,
               description => <<"y">>},
             #{date => <<"d2">>, title => <<"t2">>}],
    H = ?D:ah_timeline(Items, [], []),
    B = r(H),
    ?assert(has(<<"class=\"ah-timeline ah-timeline-position-both ah-collapsible\"">>, H)),
    %% both: item 0 on the far side (date near), item 1 near
    ?assertMatch({_, _}, binary:match(B, <<"<div class=\"ah-timeline-near-cell\"><div class=\"ah-timeline-date\">d0</div>">>)),
    ?assertMatch({_, _}, binary:match(B, <<"<div class=\"ah-timeline-far-cell\"><div class=\"ah-timeline-date\">d1</div>">>)),
    ?assert(has(<<"&lt;t0&gt;">>, H)),
    ?assert(has(<<"ah-timeline-dot ah-timeline-dot-success">>, H)),
    ?assert(has(<<"class=\"ah-timeline-item ah-timeline-item-expanded\" ah-collapsible role=\"button\" "
                  "tabindex=\"0\" aria-expanded=\"true\"">>, H)),
    %% no description, nothing to expand
    ?assert(has(<<"<div class=\"ah-timeline-item\"><div class=\"ah-timeline-item-pointer\">">>, H)),
    N = ?D:ah_timeline(Items, [near, horizontal], [{collapsible, false}]),
    ?assert(has(<<"class=\"ah-timeline ah-timeline-position-near ah-timeline-horizontal\"">>, N)),
    ?assertNot(has(<<"ah-collapsible">>, N)),
    ?assertError({aihtml, {bad_dot, pink}}, r(?D:ah_timeline([#{dot => pink}], [], []))).

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
    ?assertMatch({<<"ah:toggle">>, _}, Token(#ah_timeline{postback = p})).
