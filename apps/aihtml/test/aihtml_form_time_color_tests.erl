-module(aihtml_form_time_color_tests).

-include_lib("eunit/include/eunit.hrl").

-define(M, aihtml_form_time_color).

r(Html) -> aihtml_html:render_binary(Html).

has(Bin, Part) -> binary:match(Bin, Part) =/= nomatch.

count(Bin, Part) -> length(binary:matches(Bin, Part)).

bad(Fun) ->
    try Fun(), no_error
    catch error:{aihtml, E} -> element(1, E)
    end.

%%%-------------------------------------------------------------------
%%% timepicker values
%%%-------------------------------------------------------------------

normalize_time_test_() ->
    [?_assertEqual({14, 30}, ?M:normalize_time(<<"14:30">>)),
     ?_assertEqual({9, 5}, ?M:normalize_time(<<"9:05">>)),
     ?_assertEqual({23, 59}, ?M:normalize_time(<<"23:59:59">>)),
     ?_assertEqual({0, 0}, ?M:normalize_time(" 00:00 ")),
     ?_assertEqual({7, 15}, ?M:normalize_time({7, 15})),
     ?_assertEqual({7, 15}, ?M:normalize_time({7, 15, 30})),
     ?_assertEqual(undefined, ?M:normalize_time(undefined)),
     ?_assertEqual(undefined, ?M:normalize_time(<<>>)),
     ?_assertError({aihtml, {bad_value, timepicker, <<"24:00">>}}, ?M:normalize_time(<<"24:00">>)),
     ?_assertError({aihtml, {bad_value, timepicker, <<"12:60">>}}, ?M:normalize_time(<<"12:60">>)),
     ?_assertError({aihtml, {bad_value, timepicker, <<"2pm">>}}, ?M:normalize_time(<<"2pm">>)),
     ?_assertError({aihtml, {bad_value, timepicker, {25, 0}}}, ?M:normalize_time({25, 0})),
     ?_assertError({aihtml, {bad_value, timepicker, {1, 2, 60}}}, ?M:normalize_time({1, 2, 60})),
     ?_assertError({aihtml, {bad_value, timepicker, 1430}}, ?M:normalize_time(1430))].

%%%-------------------------------------------------------------------
%%% timepicker rendering
%%%-------------------------------------------------------------------

timepicker_popup_test() ->
    H = r(?M:timepicker(<<"14:30">>, [<<"w-48">>], [{name, start}, {id, <<"t1">>}])),
    ?assert(has(H, <<"class=\"ah-timepicker-field w-48\"">>)),
    ?assert(has(H, <<"data-ah=\"timepicker\"">>)),
    ?assert(has(H, <<"data-ah-value=\"14:30\"">>)),
    ?assert(has(H, <<"id=\"t1\"">>)),
    ?assert(has(H, <<"<input type=\"hidden\" name=\"start\" value=\"14:30\">">>)),
    %% name only on the hidden input
    ?assertEqual(1, count(H, <<"name=">>)),
    ?assert(has(H, <<"value=\"2:30 PM\"">>)),
    ?assert(has(H, <<"class=\"ah-timepicker-popup\" role=\"dialog\"">>)),
    ?assert(has(H, <<" hidden>">>)),
    %% 12h header: hour 2, PM active
    ?assert(has(H, <<"aria-label=\"Hours\">2</span>">>)),
    ?assert(has(H, <<"ah-timepicker-header-pm ah-timepicker-header-pm-active">>)),
    %% twelve numbers, 2 selected, hand at 2 o'clock
    ?assertEqual(12, count(H, <<"<text ">>)),
    ?assert(has(H, <<"ah-timepicker-number ah-timepicker-number-selected\" x=\"220.93\" "
                     "y=\"77.5\" data-val=\"2\"">>)),
    ?assert(has(H, <<"x2=\"220.93\" y2=\"77.5\"">>)).

timepicker_24h_test() ->
    H = r(?M:timepicker({21, 0}, [inline], [{format, '24h'}, {minute_step, 15}])),
    ?assert(has(H, <<"class=\"ah-timepicker-field ah-timepicker-field-inline\"">>)),
    ?assertNot(has(H, <<"ah-timepicker-popup">>)),
    ?assertNot(has(H, <<"ah-timepicker-input">>)),
    ?assert(has(H, <<"data-format=\"24h\"">>)),
    ?assert(has(H, <<"data-step=\"15\"">>)),
    ?assertNot(has(H, <<"header-period">>)),
    ?assertEqual(24, count(H, <<"<text ">>)),
    ?assertEqual(12, count(H, <<"ah-timepicker-number-inner">>)),
    %% 21 is on the inner ring at 9 o'clock: (130 - 70, 130)
    ?assert(has(H, <<"x2=\"60\" y2=\"130\"">>)),
    ?assert(has(H, <<"number-selected\" x=\"60\" y=\"130\" data-val=\"21\">21</text>">>)),
    ?assert(has(H, <<"<input type=\"hidden\" value=\"21:00\">">>)).

timepicker_empty_test() ->
    H = r(?M:timepicker(undefined, [clearable], [{placeholder, <<"<Pick>">>}])),
    ?assert(has(H, <<"data-ah-value=\"\"">>)),
    ?assert(has(H, <<"placeholder=\"&lt;Pick&gt;\"">>)),
    ?assert(has(H, <<"ah-timepicker-field-clearable">>)),
    %% nothing to clear yet
    ?assert(has(H, <<"aria-label=\"Clear\" hidden>">>)),
    H2 = r(?M:timepicker(undefined, [], [{format, '24h'}])),
    ?assert(has(H2, <<"placeholder=\"--:--\"">>)),
    ?assertNot(has(H2, <<"ah-timepicker-clear">>)).

timepicker_modifiers_test() ->
    H = r(?M:timepicker(<<"00:10">>, [inline, landscape, disabled], [])),
    ?assert(has(H, <<"ah-timepicker-field-landscape">>)),
    ?assert(has(H, <<"class=\"ah-timepicker ah-timepicker-landscape ah-timepicker-disabled\"">>)),
    ?assert(has(H, <<"aria-disabled=\"true\"">>)),
    %% midnight is 12 AM; a disabled header is out of the tab order
    ?assert(has(H, <<"tabindex=\"-1\" aria-pressed=\"true\" aria-label=\"Hours\" "
                     "aria-disabled=\"true\">12</span>">>)),
    ?assertEqual(0, count(H, <<"tabindex=\"0\" aria-pressed">>)),
    ?assert(has(H, <<"ah-timepicker-header-am ah-timepicker-header-am-active">>)),
    Hd = r(?M:timepicker(<<"07:05">>, [disabled], [])),
    ?assert(has(Hd, <<"<input class=\"ah-timepicker-input\" type=\"text\" value=\"7:05 AM\"">>)),
    ?assert(has(Hd, <<"aria-expanded=\"false\" disabled>">>)),
    ?assertEqual(unknown_modifier, bad(fun() -> ?M:timepicker(undefined, [huge], []) end)),
    ?assertEqual(conflicting_modifiers,
                 bad(fun() -> ?M:timepicker(undefined, [portrait, landscape], []) end)).

timepicker_range_test() ->
    H = r(?M:timepicker(<<"14:00">>, [], [{min, <<"09:00">>}, {max, {17, 30}}])),
    ?assert(has(H, <<"data-min=\"09:00\" data-max=\"17:30\"">>)),
    %% PM: 6..11 are after 17:30 and disabled; 12 (noon) .. 5 are allowed
    Disabled = [V || {match, [V]} <- [re:run(T, <<"data-val=\"([0-9]+)\"">>,
                                                 [{capture, all_but_first, binary}])
                                      || T <- binary:split(H, <<"<text ">>, [global]),
                                         has(T, <<"number-disabled">>)]],
    ?assertEqual([<<"6">>, <<"7">>, <<"8">>, <<"9">>, <<"10">>, <<"11">>], Disabled).

timepicker_options_test() ->
    ?assertEqual(bad_option, bad(fun() -> ?M:timepicker(undefined, [], [{format, '36h'}]) end)),
    ?assertEqual(bad_option, bad(fun() -> ?M:timepicker(undefined, [], [{minute_step, 0}]) end)),
    ?assertEqual(bad_option, bad(fun() -> ?M:timepicker(undefined, [], [{min, <<"9">>}]) end)),
    ?assertEqual(bad_value, bad(fun() -> ?M:timepicker(<<"nope">>, [], []) end)),
    H = r(?M:timepicker(<<"10:00">>, [], [{auto_switch, false}])),
    ?assert(has(H, <<"data-auto-switch=\"false\"">>)).

timepicker_escaping_test() ->
    H = r(?M:timepicker(<<"10:00">>, [inline],
                        [{footer, <<"<b>&">>}, {title, <<"\"x\"">>},
                         {data, #{note => <<"<i>">>}}])),
    ?assert(has(H, <<"&lt;b&gt;&amp;">>)),
    ?assert(has(H, <<"title=\"&quot;x&quot;\"">>)),
    ?assert(has(H, <<"data-note=\"&lt;i&gt;\"">>)),
    ?assertNot(has(H, <<"<b>">>)).

%%%-------------------------------------------------------------------
%%% colorpicker values
%%%-------------------------------------------------------------------

normalize_color_test_() ->
    [?_assertEqual({255, 0, 0, 255}, ?M:normalize_color(<<"#FF0000">>, false)),
     ?_assertEqual({255, 0, 0, 255}, ?M:normalize_color(<<"ff0000">>, false)),
     ?_assertEqual({170, 187, 204, 255}, ?M:normalize_color(<<"#abc">>, false)),
     ?_assertEqual({1, 2, 3, 255}, ?M:normalize_color({1, 2, 3}, false)),
     ?_assertEqual({1, 2, 3, 255}, ?M:normalize_color("#010203", false)),
     ?_assertEqual({34, 197, 94, 128}, ?M:normalize_color(<<"#22c55e80">>, true)),
     ?_assertEqual({170, 187, 204, 221}, ?M:normalize_color(<<"#abcd">>, true)),
     ?_assertEqual({1, 2, 3, 4}, ?M:normalize_color({1, 2, 3, 4}, true)),
     ?_assertEqual(undefined, ?M:normalize_color(undefined, false)),
     ?_assertEqual(undefined, ?M:normalize_color(<<>>, true)),
     ?_assertError({aihtml, {bad_value, colorpicker, <<"#22c55e80">>}},
                   ?M:normalize_color(<<"#22c55e80">>, false)),
     ?_assertError({aihtml, {bad_value, colorpicker, {1, 2, 3, 4}}},
                   ?M:normalize_color({1, 2, 3, 4}, false)),
     ?_assertError({aihtml, {bad_value, colorpicker, <<"#ggg">>}},
                   ?M:normalize_color(<<"#ggg">>, false)),
     ?_assertError({aihtml, {bad_value, colorpicker, <<"#12345">>}},
                   ?M:normalize_color(<<"#12345">>, true)),
     ?_assertError({aihtml, {bad_value, colorpicker, {256, 0, 0}}},
                   ?M:normalize_color({256, 0, 0}, false)),
     ?_assertError({aihtml, {bad_value, colorpicker, red}},
                   ?M:normalize_color(red, false))].

%%%-------------------------------------------------------------------
%%% colorpicker rendering
%%%-------------------------------------------------------------------

colorpicker_popup_test() ->
    H = r(?M:colorpicker(<<"#3B82F6">>, [<<"m-2">>],
                         [{name, brand}, {id, <<"c1">>},
                          {swatches, [<<"#3b82f6">>, <<"#EF4444">>]}])),
    ?assert(has(H, <<"class=\"ah-colorpicker-field m-2\"">>)),
    ?assert(has(H, <<"data-ah=\"colorpicker\" data-ah-value=\"#3b82f6\"">>)),
    ?assert(has(H, <<"<input type=\"hidden\" name=\"brand\" value=\"#3b82f6\">">>)),
    ?assertEqual(1, count(H, <<"name=">>)),
    ?assert(has(H, <<"ah-colorpicker-trigger-text\">#3b82f6</span>">>)),
    ?assert(has(H, <<"class=\"ah-colorpicker-popup\" role=\"dialog\"">>)),
    %% #3b82f6 is hsv(217, 76, 96)
    ?assert(has(H, <<"background-color: hsl(217, 100%, 50%)">>)),
    ?assert(has(H, <<"left: 76%; top: 4%">>)),
    ?assert(has(H, <<"ah-colorpicker-map-pointer-light">>)),
    ?assert(has(H, <<"value=\"3b82f6\"">>)),
    ?assert(has(H, <<"ah-colorpicker-r-input\" value=\"59\"">>)),
    %% swatches, the current one pressed
    ?assertEqual(2, count(H, <<"class=\"ah-colorpicker-swatch\"">>)),
    ?assert(has(H, <<"data-color=\"#3b82f6\" aria-label=\"#3b82f6\" title=\"#3b82f6\" "
                     "aria-pressed=\"true\"">>)),
    ?assert(has(H, <<"data-color=\"#ef4444\" aria-label=\"#ef4444\" title=\"#ef4444\" "
                     "aria-pressed=\"false\"">>)),
    ?assertNot(has(H, <<"ah-colorpicker-alpha">>)),
    ?assertNot(has(H, <<"ah-colorpicker-transparent">>)).

colorpicker_alpha_test() ->
    H = r(?M:colorpicker(<<"#22C55E80">>, [inline, alpha, clearable], [])),
    ?assert(has(H, <<"data-ah-value=\"#22c55e80\"">>)),
    ?assert(has(H, <<"data-alpha=\"true\"">>)),
    ?assert(has(H, <<"class=\"ah-colorpicker-field ah-colorpicker-field-clearable "
                     "ah-colorpicker-field-inline\"">>)),
    ?assert(has(H, <<"ah-colorpicker-bar ah-colorpicker-alpha">>)),
    ?assert(has(H, <<"aria-valuenow=\"50\"">>)),
    ?assert(has(H, <<"ah-colorpicker-a-input\" value=\"50\"">>)),
    ?assert(has(H, <<"value=\"22c55e80\"">>)),
    ?assert(has(H, <<"maxlength=\"8\"">>)),
    ?assert(has(H, <<"<div class=\"ah-colorpicker-transparent\"><a href=\"#\" "
                     "role=\"button\">Clear</a></div>">>)),
    %% opaque alpha is written as 6 digits
    H2 = r(?M:colorpicker({1, 2, 3, 255}, [alpha], [])),
    ?assert(has(H2, <<"data-ah-value=\"#010203\"">>)).

colorpicker_empty_test() ->
    H = r(?M:colorpicker(undefined, [], [{placeholder, <<"<none>">>}])),
    ?assert(has(H, <<"data-ah-value=\"\"">>)),
    ?assert(has(H, <<"ah-colorpicker-trigger-swatch ah-colorpicker-trigger-empty">>)),
    ?assert(has(H, <<"trigger-text\">&lt;none&gt;</span>">>)),
    ?assert(has(H, <<"data-placeholder=\"&lt;none&gt;\"">>)),
    %% the panel starts at sigil's default red
    ?assert(has(H, <<"hsl(0, 100%, 50%)">>)).

colorpicker_modifiers_test() ->
    H = r(?M:colorpicker(<<"#0ea5e9">>, [inline, no_inputs],
                         [{width, 200}, {height, <<"8rem">>}])),
    ?assertNot(has(H, <<"ah-colorpicker-inputs">>)),
    ?assert(has(H, <<"style=\"width: 200px\"">>)),
    ?assert(has(H, <<"; height: 8rem\"">>)),
    Hp = r(?M:colorpicker(<<"#0ea5e9">>, [inline, no_preview], [])),
    ?assertNot(has(Hp, <<"ah-colorpicker-preview">>)),
    ?assert(has(Hp, <<"ah-colorpicker-hex-input">>)),
    Hd = r(?M:colorpicker(<<"#f97316">>, [disabled], [])),
    ?assert(has(Hd, <<"ah-colorpicker-field-disabled">>)),
    ?assert(has(Hd, <<"class=\"ah-colorpicker ah-colorpicker-disabled\"">>)),
    ?assert(has(Hd, <<"aria-expanded=\"false\" disabled">>)),
    %% white gets the dark pointer
    Hw = r(?M:colorpicker(<<"#fff">>, [inline], [])),
    ?assert(has(Hw, <<"ah-colorpicker-map-pointer-dark">>)),
    ?assertEqual(unknown_modifier, bad(fun() -> ?M:colorpicker(undefined, [primary], []) end)),
    ?assertEqual(bad_value, bad(fun() -> ?M:colorpicker(<<"#12345678">>, [], []) end)),
    ?assertEqual(bad_value,
                 bad(fun() -> ?M:colorpicker(undefined, [], [{swatches, [<<"x">>]}]) end)).

colorpicker_escaping_test() ->
    H = r(?M:colorpicker(<<"#000">>, [clearable],
                         [{clear_label, <<"<x>">>}, {aria_label, <<"a\"b">>}])),
    ?assert(has(H, <<"<a href=\"#\" role=\"button\">&lt;x&gt;</a>">>)),
    ?assert(has(H, <<"aria-label=\"a&quot;b\"">>)).

%%%-------------------------------------------------------------------
%%% catalog and examples
%%%-------------------------------------------------------------------

catalog_test() ->
    [T, C] = ?M:catalog(),
    ?assertMatch(#{name := timepicker, behavior := <<"timepicker">>}, T),
    ?assertMatch(#{name := colorpicker, behavior := <<"colorpicker">>}, C),
    [?assert(erlang:function_exported(?M, N, 3)) || #{name := N} <- [T, C]].

examples_render_test() ->
    Ex = ?M:examples(),
    ?assertEqual([timepicker, colorpicker], [N || {N, _, _} <- Ex]),
    [?assert(is_binary(r(Html))) || {_, _, Html} <- Ex].
