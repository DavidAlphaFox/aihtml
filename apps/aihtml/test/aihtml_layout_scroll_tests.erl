-module(aihtml_layout_scroll_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_layout_scroll.hrl").

-define(M, aihtml_layout_scroll).

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
    ?assertEqual([scrollview, scrollbar, responsive_panel],
                 [N || #{name := N} <- ?M:catalog()]).

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
    ?assertError({aihtml, {unknown_modifier, scrollview, big, _}},
                 ?M:scrollview([], [big], [])),
    ?assertError({aihtml, {conflicting_modifiers, scrollbar, orientation, _}},
                 ?M:scrollbar([], [vertical, horizontal], [])).

%%%===================================================================
%%% scrollview
%%%===================================================================

scrollview_structure_test() ->
    H = r(?M:scrollview([<<"a">>, <<"b">>, <<"c">>], [<<"rounded">>],
                        [{id, sv}, {current_page, 1}, {name, page}, {height, 200},
                         {title, <<"t">>}])),
    ?assert(has(<<"<div class=\"ah-scrollview rounded\" id=\"sv\" data-ah=\"scrollview\" "
                  "data-ah-value=\"1\"">>, H)),
    ?assert(has(<<"role=\"region\" aria-roledescription=\"carousel\" aria-label=\"Carousel\" "
                  "tabindex=\"0\" style=\"height:200px;\"">>, H)),
    ?assert(has(<<"<div class=\"ah-scrollview-wrapper\" id=\"sv-pages\" "
                  "style=\"margin-left:-100%;\" aria-live=\"polite\">">>, H)),
    ?assertEqual(3, count(<<"class=\"ah-scrollview-page\"">>, H)),
    ?assertEqual(2, count(<<"aria-hidden=\"true\" inert>">>, H)),
    ?assert(has(<<"aria-label=\"2 / 3\">b</div>">>, H)),
    ?assertEqual(3, count(<<"<span class=\"ah-scrollview-button">>, H)),
    ?assert(has(<<"ah-scrollview-button ah-scrollview-button-active\" role=\"button\" "
                  "tabindex=\"-1\" aria-label=\"Page 2\" aria-current=\"true\"">>, H)),
    ?assert(has(<<"<input type=\"hidden\" name=\"page\" value=\"1\">">>, H)),
    ?assert(has(<<"title=\"t\"">>, H)),
    ?assertNot(has_quiet(<<"data-slide-show">>, H)).

scrollview_options_test() ->
    H = r(?M:scrollview([<<"a">>, <<"b">>], [disabled],
                        [{current_page, 9}, {show_buttons, false}, {slide_show, true},
                         {slide_duration, 2000}, {animation_duration, 500},
                         {move_threshold, 0.25}, {bounce, false}, {label, <<"Promo">>}])),
    ?assert(has(<<"class=\"ah-scrollview ah-scrollview-disabled\"">>, H)),
    ?assert(has(<<"data-ah-value=\"1\"">>, H)),                  % clamped
    ?assert(has(<<"style=\"margin-left:-100%;transition-duration:500ms;\"">>, H)),
    ?assert(has(<<"style=\"display:none\"">>, H)),
    ?assert(has(<<"tabindex=\"-1\" aria-disabled=\"true\"">>, H)),
    ?assert(has(<<"aria-label=\"Promo\"">>, H)),
    ?assert(has(<<"data-slide-show data-slide-duration=\"2000\" data-duration=\"500\" "
                  "data-threshold=\"0.25\" data-bounce=\"false\"">>, H)),
    %% no aria-live while the pages turn by themselves
    ?assertNot(has_quiet(<<"aria-live">>, H)),
    ?assert(has(<<"data-ah-value=\"0\"">>, r(?M:scrollview([], [], [])))).

%%%===================================================================
%%% scrollbar
%%%===================================================================

scrollbar_standalone_test() ->
    H = r(?M:scrollbar([], [vertical],
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
    D = r(?M:scrollbar([], [disabled], [{min, 0.5}, {max, 2.5}, {value, 1.25}])),
    ?assert(has(<<"class=\"ah-scrollbar-host ah-scrollbar-disabled\"">>, D)),
    ?assert(has(<<"data-ah-value=\"1.25\"">>, D)),
    ?assert(has(<<"aria-disabled=\"true\" tabindex=\"-1\"">>, D)).

scrollbar_area_test() ->
    H = r(?M:scrollbar(aihtml_html:el(p, <<"x">>, [], []), [<<"border">>],
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
%%% responsive_panel
%%%===================================================================

responsive_panel_test() ->
    H = r(?M:responsive_panel(<<"nav">>, [],
                              [{id, rp}, {breakpoint, 600}, {collapse_width, 240},
                               {height, 300}, {animation, slide}, {auto_close, false},
                               {toggle_button, <<"#menu">>}, {toggle_size, 36},
                               {toggle_label, <<"Menu">>}])),
    ?assert(has(<<"<div class=\"ah-responsive-panel\" id=\"rp\" data-ah=\"responsive-panel\" "
                  "data-breakpoint=\"600\" data-collapse-width=\"240px\" data-animation=\"slide\" "
                  "data-show-duration=\"200\" data-hide-duration=\"200\" data-auto-close=\"false\" "
                  "data-toggle-button=\"#menu\">">>, H)),
    ?assert(has(<<"<div class=\"ah-responsive-panel-toggle\" role=\"button\" tabindex=\"0\" "
                  "title=\"Menu\" aria-label=\"Menu\" aria-expanded=\"false\" "
                  "aria-controls=\"rp-content\" style=\"width:36px;height:36px;\">☰</div>"/utf8>>, H)),
    ?assert(has(<<"<div class=\"ah-responsive-panel-content\" id=\"rp-content\" "
                  "style=\"height:300px;\">nav</div>">>, H)),
    D = r(?M:responsive_panel([], [disabled], [{toggle_content, <<"=">>}])),
    ?assert(has(<<"class=\"ah-responsive-panel ah-responsive-panel-disabled\" id=\"ah-responsive-panel-">>, D)),
    ?assert(has(<<"aria-disabled=\"true\"">>, D)),
    ?assert(has(<<">=</div>">>, D)).

responsive_panel_load_test() ->
    H = r(#ah_responsive_panel{id = rp, load = {?MODULE, load, #{n => 1}}}),
    {match, [Tok]} = re:run(H, <<"id=\"rp-content\" data-ah-on=\"ah:load:([^\"]+)\"">>,
                            [{capture, all_but_first, binary}]),
    ?assertEqual({ok, {?MODULE, load, #{n => 1}}}, aihtml_action:unsign(Tok)).

%%%===================================================================
%%% element records (designs/05-records.md)
%%%===================================================================

record_equals_builder_test() ->
    ?assertEqual(r(?M:scrollview([<<"a">>, <<"b">>], [disabled, <<"x">>],
                                 [{id, s}, {name, n}, {current_page, 1}, {height, 90},
                                  {bounce, false}, {title, <<"t">>}])),
                 r(#ah_scrollview{body = [<<"a">>, <<"b">>], disabled = true, css = [<<"x">>],
                                  id = s, name = n, current_page = 1, height = 90,
                                  bounce = false, attrs = [{title, <<"t">>}]})),
    ?assertEqual(r(?M:scrollbar([], [vertical], [{id, b}, {value, 5}, {max, 50}, {step, 1}])),
                 r(#ah_scrollbar{orientation = vertical, id = b, value = 5, max = 50,
                                 step = 1})),
    ?assertEqual(r(?M:responsive_panel(<<"c">>, [], [{id, p}, {breakpoint, 10},
                                                    {animation, none}])),
                 r(#ah_responsive_panel{body = <<"c">>, id = p, breakpoint = 10,
                                        animation = none})).

builder_fills_fields_test() ->
    S = ?M:scrollview([<<"a">>], [disabled], [{slide_show, true}, {title, <<"t">>}]),
    ?assertMatch(#ah_scrollview{body = [<<"a">>], disabled = true, slide_show = true,
                                attrs = [{title, <<"t">>}]}, S),
    B = ?M:scrollbar([], [vertical], [{large_step, 7}, {width, 20}]),
    ?assertMatch(#ah_scrollbar{orientation = vertical, large_step = 7, width = 20}, B),
    ?assertError({aihtml, {record_only_field, ah_scrollview, postback}},
                 ?M:scrollview([], [], [{postback, go}])).

generated_id_test() ->
    H = r(#ah_scrollview{body = [<<"a">>]}),
    {match, [Id]} = re:run(H, <<"^<div class=\"ah-scrollview\" id=\"(ah-scrollview-[0-9]+)\"">>,
                           [{capture, all_but_first, binary}]),
    ?assert(has(<<"id=\"", Id/binary, "-pages\"">>, H)),
    R = #ah_responsive_panel{},
    ?assertNotEqual(r(R), r(R)).

postback_test() ->
    Token = fun(Html) ->
                    {match, [T]} = re:run(r(Html), <<"data-ah-on=\"([a-z]+:[^\"]+)\"">>,
                                          [{capture, all_but_first, binary}]),
                    [Ev, Tok] = binary:split(T, <<":">>),
                    {ok, Ref} = aihtml_action:unsign(Tok),
                    {Ev, Ref}
            end,
    ?assertEqual({<<"change">>, {?MODULE, paged, #{id => 7}}},
                 Token(#ah_scrollview{postback = {paged, #{id => 7}}})),
    ?assertEqual({<<"change">>, {other, moved, #{}}},
                 Token(#ah_scrollbar{postback = moved, delegate = other})),
    ?assertError({aihtml, {no_postback_event, ah_responsive_panel}},
                 r(#ah_responsive_panel{postback = go})).

field_validation_test() ->
    ?assertError({aihtml, {bad_option, current_page, -1}}, r(#ah_scrollview{current_page = -1})),
    ?assertError({aihtml, {bad_option, move_threshold, 2}}, r(#ah_scrollview{move_threshold = 2})),
    ?assertError({aihtml, {bad_option, slide_show, yes}}, r(#ah_scrollview{slide_show = yes})),
    ?assertError({aihtml, {bad_option, body, <<"x">>}}, r(#ah_scrollview{body = <<"x">>})),
    ?assertError({aihtml, {bad_option, max, -5}}, r(#ah_scrollbar{max = -5})),
    ?assertError({aihtml, {bad_option, step, big}}, r(#ah_scrollbar{step = big})),
    ?assertError({aihtml, {bad_modifier, scrollbar, orientation, diagonal, _}},
                 r(#ah_scrollbar{orientation = diagonal})),
    ?assertError({aihtml, {bad_flag, scrollbar, disabled, 1}}, r(#ah_scrollbar{disabled = 1})),
    ?assertError({aihtml, {bad_option, animation, zoom}},
                 r(#ah_responsive_panel{animation = zoom})),
    ?assertError({aihtml, {bad_option, load, fun_ref}}, r(#ah_responsive_panel{load = fun_ref})),
    ?assertError({aihtml, {bad_length, 1.5}}, r(#ah_responsive_panel{height = 1.5})).

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

default(ah_scrollview) -> #ah_scrollview{};
default(ah_scrollbar) -> #ah_scrollbar{};
default(ah_responsive_panel) -> #ah_responsive_panel{}.

has_quiet(Needle, Hay) -> binary:match(Hay, Needle) =/= nomatch.
