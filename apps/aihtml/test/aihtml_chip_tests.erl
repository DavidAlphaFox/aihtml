-module(aihtml_chip_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_chip.hrl").

-define(D, aihtml_chip).

r(Html) -> aihtml_html:render_binary(Html).

has(Needle, Html) ->
    binary:match(r(Html), Needle) =/= nomatch.

%%%===================================================================
%%% Catalog
%%%===================================================================

catalog_names_are_exported_test() ->
    [#{name := chip, category := C}] = ?D:catalog(),
    ?assert(erlang:function_exported(?D, ah_chip, 3)),
    ?assert(lists:member(C, [form, layout, overlay, data, media, text])).

api_docs_cover_options_and_flags_test() ->
    [#{option_docs := OD, methods := Ms} = E] = ?D:catalog(),
    Docs = maps:keys(OD),
    [?assert(lists:member(K, Docs)) || K <- maps:get(options, E, [])],
    [?assert(is_binary(D)) || D <- maps:values(OD)],
    [?assertMatch(#{name := _, args := _, doc := _}, M) || M <- Ms].

records_match_catalog_test() ->
    [#{name := N} = E] = ?D:catalog(),
    Fields = ?D:fields(ah_chip),
    ?assertEqual([module, id, css, attrs, postback, delegate], lists:sublist(Fields, 6)),
    Defaults = maps:from_list(lists:zip(Fields, tl(tuple_to_list(#ah_chip{})))),
    [?assertEqual({N, G, case D of none -> undefined; _ -> D end},
                  {N, G, maps:get(G, Defaults)})
     || {G, {_, D}} <- maps:to_list(maps:get(groups, E, #{}))],
    [?assertEqual({N, F, false}, {N, F, maps:get(F, Defaults)})
     || F <- maps:get(flags, E, [])],
    [?assert(lists:member(O, Fields)) || O <- maps:get(options, E, [])],
    % the record renders with its defaults
    ?assert(is_binary(r(#ah_chip{}))),
    ?assertEqual(?D, maps:get(module, Defaults)).

%%%===================================================================
%%% Rendering
%%%===================================================================

chip_test() ->
    H = ?D:ah_chip(<<"<Tag>">>, [removable, outlined, info, small], [{avatar, <<"JD">>}]),
    ?assert(has(<<"data-variant=\"outlined\" data-color=\"info\" data-size=\"small\" "
                  "data-disabled=\"false\" data-clickable=\"false\" data-ah=\"chip\" "
                  "data-ah-value=\"&lt;Tag&gt;\"">>, H)),
    ?assert(has(<<"<span class=\"ah-chip__avatar\">JD</span><span class=\"ah-chip__label\">&lt;Tag&gt;</span>">>, H)),
    ?assert(has(<<"<button class=\"ah-chip__delete\" type=\"button\"">>, H)),
    C = ?D:ah_chip(<<"c">>, [clickable], [{value, 7}]),
    ?assert(has(<<"data-clickable=\"true\" data-ah=\"chip\" data-ah-value=\"7\" role=\"button\" tabindex=\"0\"">>, C)),
    ?assertNot(has(<<"ah-chip__delete">>, C)),
    ?assert(has(<<"data-disabled=\"true\"">>, ?D:ah_chip(<<"d">>, [disabled], []))).

%%%===================================================================
%%% element record (designs/05-records.md)
%%%===================================================================

conflicting_modifiers_fail_test() ->
    ?assertError({aihtml, {conflicting_modifiers, chip, variant, _}},
                 ?D:ah_chip(<<"x">>, [filled, soft], [])).

record_equals_builder_test() ->
    ?assertEqual(r(?D:ah_chip(<<"Erlang">>, [removable, outlined, info], [{value, erlang}, {id, c1}])),
                 r(#ah_chip{body = <<"Erlang">>, variant = outlined, color = info,
                            removable = true, value = erlang, id = c1})).

builder_fills_fields_test() ->
    ?assertError({aihtml, {record_only_field, ah_chip, postback}},
                 ?D:ah_chip(<<"x">>, [], [{postback, go}])).

postback_test() ->
    Token = fun(Html) ->
                    {match, [Ev, Tok]} = re:run(r(Html), <<"data-ah-on=\"((?:ah:)?[a-z-]+):([^\":]+)\"">>,
                                                [{capture, all_but_first, binary}]),
                    {ok, Ref} = aihtml_action:unsign(Tok),
                    {Ev, Ref}
            end,
    %% chip: click, or change for a removable chip that is not clickable
    ?assertEqual({<<"click">>, {?MODULE, pick, #{id => 1}}},
                 Token(#ah_chip{body = <<"a">>, clickable = true, removable = true,
                                postback = {pick, #{id => 1}}})),
    ?assertMatch({<<"change">>, {?MODULE, drop, #{}}},
                 Token(#ah_chip{body = <<"a">>, removable = true, postback = drop})).

field_validation_test() ->
    ?assertError({aihtml, {bad_flag, chip, removable, yes}}, r(#ah_chip{removable = yes})).
