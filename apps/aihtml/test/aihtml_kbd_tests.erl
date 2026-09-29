-module(aihtml_kbd_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_kbd.hrl").

-define(D, aihtml_kbd).

r(Html) -> aihtml_html:render_binary(Html).

has(Needle, Html) ->
    binary:match(r(Html), Needle) =/= nomatch.

%%%===================================================================
%%% Catalog
%%%===================================================================

catalog_names_are_exported_test() ->
    [#{name := kbd, category := C}] = ?D:catalog(),
    ?assert(erlang:function_exported(?D, kbd, 3)),
    ?assert(lists:member(C, [form, layout, overlay, data, media, text])).

api_docs_cover_options_and_flags_test() ->
    [#{option_docs := OD, methods := Ms} = E] = ?D:catalog(),
    Docs = maps:keys(OD),
    [?assert(lists:member(K, Docs)) || K <- maps:get(options, E, [])],
    [?assert(is_binary(D)) || D <- maps:values(OD)],
    [?assertMatch(#{name := _, args := _, doc := _}, M) || M <- Ms].

records_match_catalog_test() ->
    [#{name := N} = E] = ?D:catalog(),
    Fields = ?D:fields(ah_kbd),
    ?assertEqual([module, id, css, attrs, postback, delegate], lists:sublist(Fields, 6)),
    Defaults = maps:from_list(lists:zip(Fields, tl(tuple_to_list(#ah_kbd{})))),
    [?assertEqual({N, G, case D of none -> undefined; _ -> D end},
                  {N, G, maps:get(G, Defaults)})
     || {G, {_, D}} <- maps:to_list(maps:get(groups, E, #{}))],
    [?assertEqual({N, F, false}, {N, F, maps:get(F, Defaults)})
     || F <- maps:get(flags, E, [])],
    [?assert(lists:member(O, Fields)) || O <- maps:get(options, E, [])],
    % the record renders with its defaults
    ?assert(is_binary(r(#ah_kbd{}))),
    ?assertEqual(?D, maps:get(module, Defaults)).

%%%===================================================================
%%% Rendering
%%%===================================================================

kbd_test() ->
    ?assertEqual(<<"<kbd class=\"ah-kbd\" data-size=\"lg\">&lt;Esc&gt;</kbd>">>,
                 r(?D:kbd(<<"<Esc>">>, [lg], []))),
    ?assertEqual(<<"<kbd class=\"ah-kbd\" data-size=\"md\">Tab</kbd>">>, r(?D:kbd("Tab", [], []))),
    C = ?D:kbd([<<"Ctrl">>, <<"K">>], [], []),
    ?assert(has(<<"<kbd class=\"ah-kbd-combo\" data-size=\"md\"><kbd class=\"ah-kbd\" data-size=\"md\">Ctrl</kbd>"
                  "<span class=\"ah-kbd-combo__sep\" aria-hidden=\"true\">+</span>">>, C)).

%%%===================================================================
%%% element record (designs/05-records.md)
%%%===================================================================

literal_classes_are_appended_test() ->
    ?assert(has(<<"class=\"ah-kbd mx-1\"">>, ?D:kbd(<<"K">>, [<<"mx-1">>], []))).

postback_test() ->
    ?assertError({aihtml, {no_postback_event, ah_kbd}}, r(#ah_kbd{postback = p})).
