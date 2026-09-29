-module(aihtml_progressbar_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_progressbar.hrl").

-define(D, aihtml_progressbar).

r(Html) -> aihtml_html:render_binary(Html).

has(Needle, Html) ->
    binary:match(r(Html), Needle) =/= nomatch.

%%%===================================================================
%%% Catalog
%%%===================================================================

catalog_names_are_exported_test() ->
    [#{name := progressbar, category := C}] = ?D:catalog(),
    ?assert(erlang:function_exported(?D, progressbar, 3)),
    ?assert(lists:member(C, [form, layout, overlay, data, media, text])).

api_docs_cover_options_and_flags_test() ->
    [#{option_docs := OD, methods := Ms} = E] = ?D:catalog(),
    Docs = maps:keys(OD),
    [?assert(lists:member(K, Docs)) || K <- maps:get(options, E, [])],
    [?assert(is_binary(D)) || D <- maps:values(OD)],
    [?assertMatch(#{name := _, args := _, doc := _}, M) || M <- Ms].

records_match_catalog_test() ->
    [#{name := N} = E] = ?D:catalog(),
    Fields = ?D:fields(ah_progressbar),
    ?assertEqual([module, id, css, attrs, postback, delegate], lists:sublist(Fields, 6)),
    Defaults = maps:from_list(lists:zip(Fields, tl(tuple_to_list(#ah_progressbar{})))),
    [?assertEqual({N, G, case D of none -> undefined; _ -> D end},
                  {N, G, maps:get(G, Defaults)})
     || {G, {_, D}} <- maps:to_list(maps:get(groups, E, #{}))],
    [?assertEqual({N, F, false}, {N, F, maps:get(F, Defaults)})
     || F <- maps:get(flags, E, [])],
    [?assert(lists:member(O, Fields)) || O <- maps:get(options, E, [])],
    % the record renders with its defaults
    ?assert(is_binary(r(#ah_progressbar{}))),
    ?assertEqual(?D, maps:get(module, Defaults)).

%%%===================================================================
%%% Rendering
%%%===================================================================

progressbar_test() ->
    H = ?D:progressbar(150, [show_text], [{min, 50}, {max, 250}]),
    ?assert(has(<<"class=\"ah-progressbar ah-progressbar-horizontal\" role=\"progressbar\" "
                  "aria-valuemin=\"50\" aria-valuemax=\"250\" aria-valuenow=\"150\" "
                  "aria-valuetext=\"50%\"">>, H)),
    ?assert(has(<<"<div class=\"ah-progressbar-value\" style=\"width: 50%;\">">>, H)),
    ?assert(has(<<"<span class=\"ah-progressbar-text\">50%</span>">>, H)),
    ?assert(has(<<"style=\"width: 100%;\"">>, ?D:progressbar(999, [], []))),
    ?assert(has(<<"style=\"display: none;\">0%</span>">>, ?D:progressbar(-5, [], []))).

progressbar_variants_test() ->
    V = ?D:progressbar(30, [vertical, reverse, success, striped, animated, disabled], []),
    ?assert(has(<<"class=\"ah-progressbar ah-progressbar-success ah-progressbar-reverse "
                  "ah-progressbar-vertical ah-progressbar-animated ah-progressbar-disabled "
                  "ah-progressbar-striped\"">>, V)),
    ?assert(has(<<"<div class=\"ah-progressbar-value-vertical\" style=\"height: 30%;\">">>, V)),
    ?assert(has(<<"aria-orientation=\"vertical\"">>, V)),
    I = ?D:progressbar(undefined, [indeterminate], []),
    ?assert(has(<<"ah-progressbar-indeterminate">>, I)),
    ?assert(has(<<"aria-busy=\"true\"">>, I)),
    ?assertNot(has(<<"aria-valuenow">>, I)),
    ?assert(has(<<"<div class=\"ah-progressbar-value\"></div>">>, I)).

progressbar_ranges_and_text_test() ->
    H = ?D:progressbar(50, [], [{color_ranges, [{30, success}, {80, <<"#ff0000">>}]},
                                {text, <<"<half>">>}]),
    ?assert(has(<<"data-range-index=\"0\" data-ah-stop=\"30\" style=\"background-color: "
                  "var(--ah-color-success); z-index: 2; width: 30%;\"">>, H)),
    ?assert(has(<<"data-range-index=\"1\" data-ah-stop=\"80\" style=\"background-color: "
                  "#ff0000; z-index: 1; width: 50%;\"">>, H)),
    ?assert(has(<<"&lt;half&gt;</span>">>, H)),
    ?assert(has(<<"data-ah-text=\"custom\"">>, H)),
    ?assertError({aihtml, {bad_color, _}},
                 r(?D:progressbar(1, [], [{color_ranges, [{5, <<"red;}x{">>}]}]))).

%%%===================================================================
%%% element record (designs/05-records.md)
%%%===================================================================

conflicting_modifiers_fail_test() ->
    ?assertError({aihtml, {conflicting_modifiers, progressbar, orientation, _}},
                 ?D:progressbar(1, [horizontal, vertical], [])).

record_equals_builder_test() ->
    ?assertEqual(r(?D:progressbar(9, [show_text, striped], [{max, 10}, {text, <<"9 of 10">>}])),
                 r(#ah_progressbar{value = 9, show_text = true, striped = true, max = 10,
                                   text = <<"9 of 10">>})).

postback_test() ->
    Token = fun(Html) ->
                    {match, [Ev, Tok]} = re:run(r(Html), <<"data-ah-on=\"((?:ah:)?[a-z-]+):([^\":]+)\"">>,
                                                [{capture, all_but_first, binary}]),
                    {ok, Ref} = aihtml_action:unsign(Tok),
                    {Ev, Ref}
            end,
    ?assertMatch({<<"change">>, _}, Token(#ah_progressbar{value = 5, postback = p})).

field_validation_test() ->
    ?assertError({aihtml, {bad_modifier, progressbar, layout, sideways, _}},
                 r(#ah_progressbar{value = 1, layout = sideways})).
