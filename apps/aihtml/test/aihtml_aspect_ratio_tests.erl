-module(aihtml_aspect_ratio_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_aspect_ratio.hrl").

-define(D, aihtml_aspect_ratio).

r(Html) -> aihtml_html:render_binary(Html).

has(Needle, Html) ->
    binary:match(r(Html), Needle) =/= nomatch.

%%%===================================================================
%%% Catalog
%%%===================================================================

catalog_names_are_exported_test() ->
    [#{name := aspect_ratio, category := C}] = ?D:catalog(),
    ?assert(erlang:function_exported(?D, ah_aspect_ratio, 3)),
    ?assert(lists:member(C, [form, layout, overlay, data, media, text])).

api_docs_cover_options_and_flags_test() ->
    [#{option_docs := OD, methods := Ms} = E] = ?D:catalog(),
    Docs = maps:keys(OD),
    [?assert(lists:member(K, Docs)) || K <- maps:get(options, E, [])],
    [?assert(is_binary(D)) || D <- maps:values(OD)],
    [?assertMatch(#{name := _, args := _, doc := _}, M) || M <- Ms].

records_match_catalog_test() ->
    [#{name := N} = E] = ?D:catalog(),
    Fields = ?D:fields(ah_aspect_ratio),
    ?assertEqual([module, id, css, attrs, postback, delegate], lists:sublist(Fields, 6)),
    Defaults = maps:from_list(lists:zip(Fields, tl(tuple_to_list(#ah_aspect_ratio{})))),
    [?assertEqual({N, G, case D of none -> undefined; _ -> D end},
                  {N, G, maps:get(G, Defaults)})
     || {G, {_, D}} <- maps:to_list(maps:get(groups, E, #{}))],
    [?assertEqual({N, F, false}, {N, F, maps:get(F, Defaults)})
     || F <- maps:get(flags, E, [])],
    [?assert(lists:member(O, Fields)) || O <- maps:get(options, E, [])],
    % the record renders with its defaults
    ?assert(is_binary(r(#ah_aspect_ratio{}))),
    ?assertEqual(?D, maps:get(module, Defaults)).

%%%===================================================================
%%% Rendering
%%%===================================================================

aspect_ratio_test() ->
    ?assert(has(<<"style=\"aspect-ratio: 16 / 9;\"">>, ?D:ah_aspect_ratio([], [], []))),
    ?assert(has(<<"style=\"aspect-ratio: 4 / 3; max-width: 10px\"">>,
                ?D:ah_aspect_ratio([], [], [{ratio, <<"4:3">>}, {style, <<"max-width: 10px">>}]))),
    ?assert(has(<<"aspect-ratio: 1.5;">>, ?D:ah_aspect_ratio([], [], [{ratio, 1.5}]))),
    ?assert(has(<<"aspect-ratio: 21 / 9;">>, ?D:ah_aspect_ratio([], [], [{ratio, {21, 9}}]))),
    ?assertError({aihtml, {bad_ratio, _}},
                 r(?D:ah_aspect_ratio([], [], [{ratio, <<"1;background:red">>}]))).

%%%===================================================================
%%% element record (designs/05-records.md)
%%%===================================================================

record_equals_builder_test() ->
    ?assertEqual(r(?D:ah_aspect_ratio([], [], [{ratio, <<"4:3">>}, {style, <<"max-width: 10px">>}])),
                 r(#ah_aspect_ratio{ratio = <<"4:3">>, style = <<"max-width: 10px">>})).

builder_fills_fields_test() ->
    %% a binary style key stays an attribute but is still merged
    ?assert(has(<<"style=\"aspect-ratio: 16 / 9; a: b\"">>,
                ?D:ah_aspect_ratio([], [], [{<<"style">>, <<"a: b">>}]))).

postback_test() ->
    ?assertError({aihtml, {no_postback_event, ah_aspect_ratio}}, r(#ah_aspect_ratio{postback = p})).

field_validation_test() ->
    ?assertError({aihtml, {bad_ratio, _}}, r(#ah_aspect_ratio{ratio = -1})).
