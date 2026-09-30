-module(aihtml_alert_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_alert.hrl").

-define(D, aihtml_alert).

r(Html) -> aihtml_html:render_binary(Html).

has(Needle, Html) ->
    binary:match(r(Html), Needle) =/= nomatch.

%%%===================================================================
%%% Catalog
%%%===================================================================

catalog_names_are_exported_test() ->
    [#{name := alert, category := C}] = ?D:catalog(),
    ?assert(erlang:function_exported(?D, ah_alert, 3)),
    ?assert(lists:member(C, [form, layout, overlay, data, media, text])).

api_docs_cover_options_and_flags_test() ->
    [#{option_docs := OD, methods := Ms} = E] = ?D:catalog(),
    Docs = maps:keys(OD),
    [?assert(lists:member(K, Docs)) || K <- maps:get(options, E, [])],
    [?assert(is_binary(D)) || D <- maps:values(OD)],
    [?assertMatch(#{name := _, args := _, doc := _}, M) || M <- Ms].

records_match_catalog_test() ->
    [#{name := N} = E] = ?D:catalog(),
    Fields = ?D:fields(ah_alert),
    ?assertEqual([module, id, css, attrs, postback, delegate], lists:sublist(Fields, 6)),
    Defaults = maps:from_list(lists:zip(Fields, tl(tuple_to_list(#ah_alert{})))),
    [?assertEqual({N, G, case D of none -> undefined; _ -> D end},
                  {N, G, maps:get(G, Defaults)})
     || {G, {_, D}} <- maps:to_list(maps:get(groups, E, #{}))],
    [?assertEqual({N, F, false}, {N, F, maps:get(F, Defaults)})
     || F <- maps:get(flags, E, [])],
    [?assert(lists:member(O, Fields)) || O <- maps:get(options, E, [])],
    % the record renders with its defaults
    ?assert(is_binary(r(#ah_alert{}))),
    ?assertEqual(?D, maps:get(module, Defaults)).

%%%===================================================================
%%% Rendering
%%%===================================================================

alert_test() ->
    H = ?D:ah_alert(<<"<msg>">>, [], []),
    ?assert(has(<<"<div class=\"ah-alert ah-alert-info\" role=\"alert\" data-ah=\"alert\">">>, H)),
    ?assert(has(<<"<div class=\"ah-alert-body\">&lt;msg&gt;</div>">>, H)),
    ?assert(has(<<"<svg">>, H)),
    ?assertNot(has(<<"ah-alert-close">>, H)),
    D = ?D:ah_alert(<<"m">>, [error, dismissible, <<"mt-2">>], [{title, <<"T&">>}, {icon, false}, {id, a1}]),
    ?assert(has(<<"class=\"ah-alert ah-alert-error ah-alert-dismissible mt-2\"">>, D)),
    ?assert(has(<<"<div class=\"ah-alert-title\">T&amp;</div>">>, D)),
    ?assert(has(<<"<button class=\"ah-alert-close\" type=\"button\" aria-label=\"Close\">">>, D)),
    ?assert(has(<<"id=\"a1\"">>, D)),
    ?assertNot(has(<<"<svg">>, D)).

%%%===================================================================
%%% element record (designs/05-records.md)
%%%===================================================================

unknown_modifier_fails_test() ->
    ?assertError({aihtml, {unknown_modifier, alert, danger, _}}, ?D:ah_alert(<<"x">>, [danger], [])).

builder_fills_fields_test() ->
    A = ?D:ah_alert(<<"m">>, [error, dismissible, <<"mt-2">>],
                    [{title, <<"T">>}, {icon, false}, {id, a1}, {aria_live, polite}]),
    ?assertMatch(#ah_alert{body = <<"m">>, variant = error, dismissible = true,
                           title = <<"T">>, icon = false, id = a1, css = [<<"mt-2">>],
                           attrs = [{aria_live, polite}]}, A).

postback_test() ->
    Token = fun(Html) ->
                    {match, [Ev, Tok]} = re:run(r(Html), <<"data-ah-on=\"((?:ah:)?[a-z-]+):([^\":]+)\"">>,
                                                [{capture, all_but_first, binary}]),
                    {ok, Ref} = aihtml_action:unsign(Tok),
                    {Ev, Ref}
            end,
    ?assertMatch({<<"ah:dismiss">>, {other_mod, gone, 1}},
                 Token(#ah_alert{body = <<"x">>, postback = {gone, 1}, delegate = other_mod})).

field_validation_test() ->
    ?assertError({aihtml, {modifier_in_css, alert, error}}, r(#ah_alert{css = [error]})).
