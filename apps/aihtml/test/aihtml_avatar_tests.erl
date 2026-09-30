-module(aihtml_avatar_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_avatar.hrl").

-define(D, aihtml_avatar).

r(Html) -> aihtml_html:render_binary(Html).

has(Needle, Html) ->
    binary:match(r(Html), Needle) =/= nomatch.

%%%===================================================================
%%% Catalog
%%%===================================================================

catalog_names_are_exported_test() ->
    [#{name := avatar, category := C}] = ?D:catalog(),
    ?assert(erlang:function_exported(?D, ah_avatar, 3)),
    ?assert(lists:member(C, [form, layout, overlay, data, media, text])).

api_docs_cover_options_and_flags_test() ->
    [#{option_docs := OD, methods := Ms} = E] = ?D:catalog(),
    Docs = maps:keys(OD),
    [?assert(lists:member(K, Docs)) || K <- maps:get(options, E, [])],
    [?assert(is_binary(D)) || D <- maps:values(OD)],
    [?assertMatch(#{name := _, args := _, doc := _}, M) || M <- Ms].

records_match_catalog_test() ->
    [#{name := N} = E] = ?D:catalog(),
    Fields = ?D:fields(ah_avatar),
    ?assertEqual([module, id, css, attrs, postback, delegate], lists:sublist(Fields, 6)),
    Defaults = maps:from_list(lists:zip(Fields, tl(tuple_to_list(#ah_avatar{})))),
    [?assertEqual({N, G, case D of none -> undefined; _ -> D end},
                  {N, G, maps:get(G, Defaults)})
     || {G, {_, D}} <- maps:to_list(maps:get(groups, E, #{}))],
    [?assertEqual({N, F, false}, {N, F, maps:get(F, Defaults)})
     || F <- maps:get(flags, E, [])],
    [?assert(lists:member(O, Fields)) || O <- maps:get(options, E, [])],
    % the record renders with its defaults
    ?assert(is_binary(r(#ah_avatar{}))),
    ?assertEqual(?D, maps:get(module, Defaults)).

%%%===================================================================
%%% Rendering
%%%===================================================================

avatar_defaults_and_modifiers_test() ->
    H = ?D:ah_avatar(<<"JD">>, [lg, square, success], []),
    ?assert(has(<<"class=\"ah-avatar\" data-size=\"lg\" data-shape=\"square\" data-color=\"success\"">>, H)),
    ?assert(has(<<"<span class=\"ah-avatar__fallback\" aria-hidden=\"true\">JD</span>">>, H)),
    ?assert(has(<<"data-size=\"md\" data-shape=\"circle\" data-color=\"primary\"">>,
                ?D:ah_avatar(<<"x">>, [], []))),
    ?assert(has(<<">?</span>">>, ?D:ah_avatar(undefined, [], []))).

avatar_image_test() ->
    H = ?D:ah_avatar(<<"A">>, [], [{src, <<"/a.png?x=1&y=\"2\"">>}, {alt, <<"Ann">>}]),
    ?assert(has(<<"<img class=\"ah-avatar__image\" src=\"/a.png?x=1&amp;y=&quot;2&quot;\" alt=\"Ann\">">>, H)),
    ?assertNot(has(<<"role=\"img\"">>, H)),
    ?assert(has(<<"role=\"img\" aria-label=\"Ann\"">>, ?D:ah_avatar(<<"A">>, [], [{alt, <<"Ann">>}]))).

%%%===================================================================
%%% element record (designs/05-records.md)
%%%===================================================================

unknown_modifier_fails_test() ->
    ?assertError({aihtml, {unknown_modifier, avatar, huge, _}}, ?D:ah_avatar(<<"A">>, [huge], [])).

postback_test() ->
    ?assertError({aihtml, {no_postback_event, ah_avatar}}, r(#ah_avatar{postback = p})).

field_validation_test() ->
    ?assertError({aihtml, {bad_modifier, avatar, size, huge, _}}, r(#ah_avatar{size = huge})).
