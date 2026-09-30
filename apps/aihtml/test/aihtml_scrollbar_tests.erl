-module(aihtml_scrollbar_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_scrollbar.hrl").

-define(M, aihtml_scrollbar).

r(Html) -> aihtml_html:render_binary(Html).

has(Needle, Hay) ->
    case binary:match(Hay, Needle) of
        nomatch -> ?debugFmt("~s not in~n~s", [Needle, Hay]), false;
        _ -> true
    end.

count(Needle, Hay) -> length(binary:matches(Hay, Needle)).

%%%===================================================================
%%% catalog
%%%===================================================================

catalog_names_test() ->
    ?assertEqual([scrollbar], [N || #{name := N} <- ?M:catalog()]).

catalog_entries_are_valid_test() ->
    [begin
         E = aihtml_catalog:entry(?M, N),
         ?assertMatch(#{category := layout, root := <<"ah-", _/binary>>}, E),
         ?assert(is_list(aihtml_catalog:classes(E, [])))
     end || #{name := N} <- ?M:catalog()].

catalog_docs_test() ->
    [begin
         ?assertMatch(#{option_docs := #{}, methods := [_ | _]}, E),
         Documented = maps:keys(maps:get(option_docs, E)),
         Opts = maps:get(options, E, []) ++ maps:get(flags, E, []) ++
             lists:append([Ms || {Ms, _} <- maps:values(maps:get(groups, E, #{}))]),
         ?assertEqual([], Opts -- Documented)
     end || E <- ?M:catalog()].

unknown_modifier_fails_test() ->
    ?assertError({aihtml, {conflicting_modifiers, scrollbar, orientation, _}},
                 ?M:ah_scrollbar([], [vertical, horizontal], [])).

%%%===================================================================
%%% scrollbar
%%%===================================================================

scrollbar_standalone_test() ->
    H = r(?M:ah_scrollbar([], [vertical],
                          [{id, sb}, {value, 1200}, {max, 500}, {step, 5}, {name, off},
                           {height, 300}, {label, <<"Offset">>}, {show_buttons, false}])),
    ?assert(has(<<"<div class=\"ah-scrollbar-host ah-scrollbar-host-vertical\" id=\"sb\" "
                  "data-ah=\"scrollbar\" data-ah-value=\"500\" role=\"scrollbar\" "
                  "aria-orientation=\"vertical\" aria-valuemin=\"0\" aria-valuemax=\"500\" "
                  "aria-valuenow=\"500\" aria-label=\"Offset\" tabindex=\"0\"">>, H)),
    ?assert(has(<<"data-min=\"0\" data-max=\"500\" data-step=\"5\" data-large-step=\"50\" "
                  "data-thumb-min=\"10\" data-buttons=\"false\" style=\"height:300px;\"">>, H)),
    ?assert(has(<<"<div class=\"ah-scrollbar ah-scrollbar-vertical\" aria-hidden=\"true\">"
                  "<div class=\"ah-scrollbar-btn-up\"></div><div class=\"ah-scrollbar-track-up\">"
                  "</div><div class=\"ah-scrollbar-thumb\"></div><div class=\"ah-scrollbar-track-down\">"
                  "</div><div class=\"ah-scrollbar-btn-down\"></div></div>">>, H)),
    ?assert(has(<<"<input type=\"hidden\" name=\"off\" value=\"500\">">>, H)),
    D = r(?M:ah_scrollbar([], [disabled], [{min, 0.5}, {max, 2.5}, {value, 1.25}])),
    ?assert(has(<<"class=\"ah-scrollbar-host ah-scrollbar-disabled\"">>, D)),
    ?assert(has(<<"data-ah-value=\"1.25\"">>, D)),
    ?assert(has(<<"aria-disabled=\"true\" tabindex=\"-1\"">>, D)).

scrollbar_area_test() ->
    H = r(?M:ah_scrollbar(aihtml_html:el(p, <<"x">>, [], []), [<<"border">>],
                          [{id, sa}, {height, 200}, {label, <<"Log">>}])),
    ?assert(has(<<"<div class=\"ah-scrollbar-host border ah-scrollbar-area\" id=\"sa\" "
                  "data-ah=\"scrollbar\" data-area">>, H)),
    ?assert(has(<<"<div class=\"ah-scrollbar-viewport\" id=\"sa-viewport\" tabindex=\"0\" "
                  "role=\"region\" aria-label=\"Log\"><div class=\"ah-scrollbar-content\">"
                  "<p>x</p></div></div>">>, H)),
    ?assertEqual(1, count(<<"ah-scrollbar ah-scrollbar-vertical">>, H)),
    ?assertEqual(1, count(<<"ah-scrollbar ah-scrollbar-horizontal">>, H)),
    ?assert(has(<<"ah-scrollbar-corner">>, H)),
    ?assertNot(has_quiet(<<"data-ah-value">>, H)),
    %% a scroll area has no value, so no postback event
    ?assertError({aihtml, {no_postback_event, ah_scrollbar}},
                 r(#ah_scrollbar{body = <<"x">>, postback = go})).

%%%===================================================================
%%% element records (designs/05-records.md)
%%%===================================================================

record_equals_builder_test() ->
    ?assertEqual(r(?M:ah_scrollbar([], [vertical], [{id, b}, {value, 5}, {max, 50}, {step, 1}])),
                 r(#ah_scrollbar{orientation = vertical, id = b, value = 5, max = 50,
                                 step = 1})).

builder_fills_fields_test() ->
    B = ?M:ah_scrollbar([], [vertical], [{large_step, 7}, {width, 20}]),
    ?assertMatch(#ah_scrollbar{orientation = vertical, large_step = 7, width = 20}, B).

postback_test() ->
    Token = fun(Html) ->
                    {match, [T]} = re:run(r(Html), <<"data-ah-on=\"([a-z]+:[^\"]+)\"">>,
                                          [{capture, all_but_first, binary}]),
                    [Ev, Tok] = binary:split(T, <<":">>),
                    {ok, Ref} = aihtml_action:unsign(Tok),
                    {Ev, Ref}
            end,
    ?assertEqual({<<"change">>, {other, moved, #{}}},
                 Token(#ah_scrollbar{postback = moved, delegate = other})).

field_validation_test() ->
    ?assertError({aihtml, {bad_option, max, -5}}, r(#ah_scrollbar{max = -5})),
    ?assertError({aihtml, {bad_option, step, big}}, r(#ah_scrollbar{step = big})),
    ?assertError({aihtml, {bad_modifier, scrollbar, orientation, diagonal, _}},
                 r(#ah_scrollbar{orientation = diagonal})),
    ?assertError({aihtml, {bad_flag, scrollbar, disabled, 1}}, r(#ah_scrollbar{disabled = 1})).

records_match_catalog_test() ->
    Base = [module, id, css, attrs, postback, delegate],
    [begin
         Tag = list_to_atom("ah_" ++ atom_to_list(N)),
         Fields = ?M:fields(Tag),
         ?assertEqual(Base, lists:sublist(Fields, 6)),
         Defaults = maps:from_list(lists:zip(Fields, tl(tuple_to_list(default(Tag))))),
         [?assertEqual({N, G, case D of none -> undefined; _ -> D end},
                       {N, G, maps:get(G, Defaults)})
          || {G, {_, D}} <- maps:to_list(maps:get(groups, E, #{}))],
         [?assertEqual({N, F, false}, {N, F, maps:get(F, Defaults)})
          || F <- maps:get(flags, E, [])],
         [?assert(lists:member(O, Fields)) || O <- maps:get(options, E, [])],
         ?assertEqual(?M, maps:get(module, Defaults))
     end || #{name := N} = E <- ?M:catalog()].

default(ah_scrollbar) -> #ah_scrollbar{}.

has_quiet(Needle, Hay) -> binary:match(Hay, Needle) =/= nomatch.
