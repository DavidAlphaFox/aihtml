-module(aihtml_badge_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_badge.hrl").

-define(D, aihtml_badge).

r(Html) -> aihtml_html:render_binary(Html).

has(Needle, Html) ->
    binary:match(r(Html), Needle) =/= nomatch.

%%%===================================================================
%%% Catalog
%%%===================================================================

catalog_names_are_exported_test() ->
    [#{name := badge, category := C}] = ?D:catalog(),
    ?assert(erlang:function_exported(?D, badge, 3)),
    ?assert(lists:member(C, [form, layout, overlay, data, media, text])).

api_docs_cover_options_and_flags_test() ->
    [#{option_docs := OD, methods := Ms} = E] = ?D:catalog(),
    Docs = maps:keys(OD),
    [?assert(lists:member(K, Docs)) || K <- maps:get(options, E, [])],
    [?assert(is_binary(D)) || D <- maps:values(OD)],
    [?assertMatch(#{name := _, args := _, doc := _}, M) || M <- Ms].

records_match_catalog_test() ->
    [#{name := N} = E] = ?D:catalog(),
    Fields = ?D:fields(ah_badge),
    ?assertEqual([module, id, css, attrs, postback, delegate], lists:sublist(Fields, 6)),
    Defaults = maps:from_list(lists:zip(Fields, tl(tuple_to_list(#ah_badge{})))),
    [?assertEqual({N, G, case D of none -> undefined; _ -> D end},
                  {N, G, maps:get(G, Defaults)})
     || {G, {_, D}} <- maps:to_list(maps:get(groups, E, #{}))],
    [?assertEqual({N, F, false}, {N, F, maps:get(F, Defaults)})
     || F <- maps:get(flags, E, [])],
    [?assert(lists:member(O, Fields)) || O <- maps:get(options, E, [])],
    % the record renders with its defaults
    ?assert(is_binary(r(#ah_badge{}))),
    ?assertEqual(?D, maps:get(module, Defaults)).

%%%===================================================================
%%% Rendering
%%%===================================================================

badge_count_max_zero_test() ->
    ?assert(has(<<"data-invisible=\"false\" aria-hidden=\"true\">99+</span>">>,
                ?D:badge(<<"x">>, [], [{count, 120}]))),
    ?assert(has(<<">9+</span>">>, ?D:badge(<<"x">>, [], [{count, 12}, {max, 9}]))),
    ?assert(has(<<"data-invisible=\"true\"">>, ?D:badge(<<"x">>, [], [{count, 0}]))),
    ?assert(has(<<"data-invisible=\"false\"">>, ?D:badge(<<"x">>, [show_zero], [{count, 0}]))),
    ?assert(has(<<"data-invisible=\"true\"">>, ?D:badge(<<"x">>, [invisible], []))).

badge_dot_anchor_and_standalone_test() ->
    H = ?D:badge(<<"anchor">>, [online, circular, bottom, left], [{count, 5}]),
    ?assert(has(<<"data-overlap=\"circular\" data-anchor-vertical=\"bottom\" data-anchor-horizontal=\"left\"">>, H)),
    ?assert(has(<<"data-variant=\"online\" data-color=\"primary\" data-dot=\"true\"">>, H)),
    ?assertNot(has(<<">5<">>, H)),
    S = ?D:badge(undefined, [error], [{count, <<"<b>">>}]),
    ?assert(has(<<"ah-badge-root ah-badge-root--standalone">>, S)),
    ?assert(has(<<"data-invisible=\"false\">&lt;b&gt;</span>">>, S)).

%%%===================================================================
%%% element record (designs/05-records.md)
%%%===================================================================

postback_test() ->
    ?assertError({aihtml, {no_postback_event, ah_badge}}, r(#ah_badge{postback = p})).

field_validation_test() ->
    ?assertError({aihtml, {bad_modifier, badge, variant, square, _}},
                 r(#ah_badge{variant = square, count = 1})).
