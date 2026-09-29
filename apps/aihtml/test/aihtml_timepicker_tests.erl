%% Tests for aihtml_timepicker.
-module(aihtml_timepicker_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_timepicker.hrl").

-export([action/4]).

-define(M, aihtml_timepicker).

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
    ?assertEqual(bad_option, bad(fun() -> r(?M:timepicker(undefined, [], [{format, '36h'}])) end)),
    ?assertEqual(bad_option, bad(fun() -> r(?M:timepicker(undefined, [], [{minute_step, 0}])) end)),
    ?assertEqual(bad_option, bad(fun() -> r(?M:timepicker(undefined, [], [{min, <<"9">>}])) end)),
    ?assertEqual(bad_value, bad(fun() -> r(?M:timepicker(<<"nope">>, [], [])) end)),
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
%%% catalog
%%%-------------------------------------------------------------------

catalog_test() ->
    [T] = ?M:catalog(),
    ?assertMatch(#{name := timepicker, behavior := <<"timepicker">>}, T),
    [?assert(erlang:function_exported(?M, N, 3)) || #{name := N} <- [T]],
    %% every option and flag is documented
    [?assertEqual([], (Opts ++ Flags) -- maps:keys(Docs))
     || #{options := Opts, flags := Flags, option_docs := Docs} <- [T]],
    [?assertNotEqual([], Ms) || #{methods := Ms} <- [T]].

%%%-------------------------------------------------------------------
%%% element records (designs/05-records.md)
%%%-------------------------------------------------------------------

-spec action(atom(), term(), map(), term()) -> ok.
action(_, _, _, _) -> ok.

record_equals_builder_test() ->
    ?assertEqual(r(?M:timepicker(<<"14:30">>, [landscape, clearable, <<"w-48">>],
                                 [{name, start}, {id, t1}, {format, '24h'},
                                  {minute_step, 15}, {min, <<"09:00">>},
                                  {auto_switch, false}, {title, <<"t">>}])),
                 r(#ah_timepicker{value = <<"14:30">>, view = landscape, clearable = true,
                                  css = [<<"w-48">>], name = start, id = t1,
                                  format = '24h', minute_step = 15, min = <<"09:00">>,
                                  auto_switch = false, attrs = [{title, <<"t">>}]})),
    ?assertEqual(r(?M:timepicker(undefined, [inline, disabled], [{footer, <<"f">>}])),
                 r(#ah_timepicker{inline = true, disabled = true, footer = <<"f">>})).

builder_fills_fields_test() ->
    T = ?M:timepicker({9, 5}, [portrait, inline, <<"x">>],
                      [{name, at}, {format, '24h'}, {placeholder, <<"p">>},
                       {title, <<"t">>}]),
    ?assertMatch(#ah_timepicker{value = {9, 5}, view = portrait, inline = true,
                                disabled = false, name = at, format = '24h',
                                minute_step = 5, auto_switch = true,
                                placeholder = <<"p">>, css = [<<"x">>],
                                attrs = [{title, <<"t">>}]}, T).

postback_test() ->
    Token = fun(Html) ->
                    {match, [Tk]} = re:run(r(Html), <<"data-ah-on=\"([a-z]+:[^\"]+)\"">>,
                                           [{capture, all_but_first, binary}]),
                    [Ev, Tok] = binary:split(Tk, <<":">>),
                    {ok, Ref} = aihtml_action:unsign(Tok),
                    {Ev, Ref}
            end,
    ?assertEqual({<<"change">>, {?MODULE, pick_time, #{id => 7}}},
                 Token(#ah_timepicker{postback = {pick_time, #{id => 7}}})).

field_validation_test() ->
    ?assertError({aihtml, {bad_modifier, timepicker, view, sideways, _}},
                 r(#ah_timepicker{view = sideways})),
    ?assertError({aihtml, {bad_flag, timepicker, inline, yes}},
                 r(#ah_timepicker{inline = yes})),
    ?assertError({aihtml, {bad_value, timepicker, <<"25:00">>}},
                 r(#ah_timepicker{value = <<"25:00">>})),
    ?assertError({aihtml, {bad_option, timepicker, format, '36h'}},
                 r(#ah_timepicker{format = '36h'})),
    ?assertError({aihtml, {bad_option, timepicker, minute_step, 45}},
                 r(#ah_timepicker{minute_step = 45})),
    ?assertError({aihtml, {bad_option, timepicker, max, <<"9">>}},
                 r(#ah_timepicker{max = <<"9">>})),
    %% the view group has no default
    ?assert(has(r(#ah_timepicker{}), <<"class=\"ah-timepicker-field\"">>)).

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

default(ah_timepicker) -> #ah_timepicker{}.
