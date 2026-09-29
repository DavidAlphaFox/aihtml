-module(aihtml_form_choice_tests).

-include_lib("eunit/include/eunit.hrl").

-define(M, aihtml_form_choice).

r(H) -> aihtml_html:render_binary(H).

has(Sub, Bin) -> binary:match(Bin, Sub) =/= nomatch.

count(Sub, Bin) -> length(binary:matches(Bin, Sub)).

%% --- checkbox ------------------------------------------------------

checkbox_markup_test() ->
    H = r(?M:checkbox(<<"Accept">>, yes, [], [{name, terms}, {checked, true}, {id, <<"t">>}])),
    ?assertMatch(<<"<label class=\"ah-checkbox ah-checkbox-checked\" data-ah=\"checkbox\">", _/binary>>, H),
    %% Attrs land on the native input
    ?assert(has(<<"<input class=\"ah-choice-input\" type=\"checkbox\" value=\"yes\" name=\"terms\" checked id=\"t\">">>, H)),
    ?assert(has(<<"ah-checkbox-check ah-checkbox-check-checked">>, H)),
    ?assert(has(<<"<span class=\"ah-checkbox-label\">Accept</span>">>, H)).

checkbox_states_test() ->
    I = r(?M:checkbox(<<"x">>, undefined, [], [{indeterminate, true}])),
    ?assert(has(<<"ah-checkbox-indeterminate">>, I)),
    ?assert(has(<<"ah-checkbox-check-indeterminate">>, I)),
    ?assertNot(has(<<"indeterminate=">>, I)),         % an option, not an attribute
    D = r(?M:checkbox(<<"x">>, undefined, [], [{disabled, true}, {three_states, true}, {locked, true}])),
    ?assert(has(<<"ah-checkbox-disabled">>, D)),
    ?assert(has(<<" disabled">>, D)),
    ?assert(has(<<"data-ah-three-states">>, D)),
    ?assert(has(<<"data-ah-locked">>, D)),
    ?assertNot(has(<<"value=">>, D)),
    B = r(?M:checkbox([], undefined, [], [{box_size, 24}])),
    ?assert(has(<<"style=\"width:24px;height:24px;\"">>, B)),
    ?assertNot(has(<<"ah-checkbox-label">>, B)).

checkbox_escaping_test() ->
    H = r(?M:checkbox(<<"<b>&">>, <<"a\"b">>, [<<"mt-2">>], [{title, <<"<x>">>}])),
    ?assert(has(<<"&lt;b&gt;&amp;">>, H)),
    ?assert(has(<<"value=\"a&quot;b\"">>, H)),
    ?assert(has(<<"title=\"&lt;x&gt;\"">>, H)),
    ?assert(has(<<"class=\"ah-checkbox mt-2\"">>, H)).

checkbox_modifiers_test() ->
    ?assert(has(<<"ah-checkbox ah-checkbox-lg">>, r(?M:checkbox(<<"x">>, undefined, [lg], [])))),
    ?assertError({aihtml, {unknown_modifier, checkbox, huge, _}},
                 ?M:checkbox(<<"x">>, undefined, [huge], [])),
    ?assertError({aihtml, {conflicting_modifiers, checkbox, size, _}},
                 ?M:checkbox(<<"x">>, undefined, [sm, lg], [])).

checkbox_action_on_input_test() ->
    A = [{<<"data-ah-on">>, {actions, [{<<"change">>, <<"TOKEN">>, #{}}]}}],
    H = r(?M:checkbox(<<"x">>, undefined, [], [A])),
    ?assert(has(<<"type=\"checkbox\" data-ah-on=\"change:TOKEN\">">>, H)).

%% --- radiobutton and switch ---------------------------------------

radiobutton_test() ->
    H = r(?M:radiobutton(<<"Email">>, email, [sm], [{name, c}, {checked, true}])),
    ?assertMatch(<<"<label class=\"ah-radiobutton ah-radiobutton-sm ah-radiobutton-checked\" data-ah=\"radiobutton\">", _/binary>>, H),
    ?assert(has(<<"type=\"radio\" value=\"email\" name=\"c\" checked">>, H)),
    ?assert(has(<<"ah-radiobutton-check ah-radiobutton-check-checked">>, H)),
    ?assertError({aihtml, {unknown_modifier, radiobutton, primary, _}},
                 ?M:radiobutton(<<"x">>, x, [primary], [])).

switch_test() ->
    H = r(?M:switch_button(<<"Wi-Fi">>, undefined, [lg], [{name, wifi}, {checked, true},
                                                          {on_label, <<"<on>">>}])),
    ?assertMatch(<<"<label class=\"ah-switch ah-switch-lg ah-switch-on\" data-ah=\"switch-button\">", _/binary>>, H),
    ?assert(has(<<"type=\"checkbox\" name=\"wifi\" checked role=\"switch\">">>, H)),
    ?assert(has(<<"ah-switch-label ah-switch-label-on\">&lt;on&gt;</span>">>, H)),
    ?assertNot(has(<<"ah-switch-label-off">>, H)),
    ?assert(has(<<"<span class=\"ah-switch-text\">Wi-Fi</span>">>, H)),
    C = r(?M:switch_button([], undefined, [], [{width, 80}, {height, 32}])),
    ?assert(has(<<"width:80px;height:32px;--sw-travel:-48px;">>, C)),
    ?assert(has(<<"width:28px;height:28px;">>, C)).

%% --- groups --------------------------------------------------------

items() -> [{a, <<"A">>}, {b, <<"B & b">>}, {c, <<"C">>, #{disabled => true, class => <<"x">>}}].

checkbox_group_test() ->
    A = [{<<"data-ah-on">>, {actions, [{<<"change">>, <<"TOK">>, #{}}]}}],
    H = r(?M:checkbox_group(items(), [a, <<"c">>], [horizontal],
                            [{name, f}, {id, <<"g">>}, A])),
    ?assertMatch(<<"<div class=\"ah-checkbox-group ah-checkbox-group-horizontal\" data-ah=\"checkbox-group\" role=\"group\" data-ah-value=\"a,c\" data-label-position=\"after\" id=\"g\" data-ah-on=\"change:TOK\">", _/binary>>, H),
    %% name goes to every input, the action only to the root
    ?assertEqual(3, count(<<"name=\"f\"">>, H)),
    ?assertEqual(1, count(<<"data-ah-on">>, H)),
    ?assertEqual(2, count(<<" checked">>, H)),
    ?assert(has(<<"B &amp; b">>, H)),
    ?assert(has(<<"ah-checkbox-group-item x ah-checkbox-group-item-disabled\" data-value=\"c\" data-index=\"2\" data-ah-item-disabled">>, H)),
    ?assertEqual(1, count(<<" disabled">>, H)).

checkbox_group_disabled_before_test() ->
    H = r(?M:checkbox_group(items(), [], [label_before, sm], [{disabled, true}])),
    ?assert(has(<<"ah-checkbox-group ah-checkbox-group-vertical ah-checkbox-group-disabled\"">>, H)),
    ?assert(has(<<"data-label-position=\"before\"">>, H)),
    ?assert(has(<<"aria-disabled=\"true\"">>, H)),
    ?assertEqual(3, count(<<" disabled">>, H)),
    ?assert(has(<<"<span class=\"ah-checkbox-group-label\">A</span><span class=\"ah-checkbox ah-checkbox-sm ah-checkbox-disabled\">">>, H)),
    ?assertNot(has(<<"ah-checkbox-group-sm">>, H)),
    ?assertError({aihtml, {conflicting_modifiers, checkbox_group, layout, _}},
                 ?M:checkbox_group(items(), [], [vertical, horizontal], [])).

radiobutton_group_test() ->
    H = r(?M:radiobutton_group(items(), b, [], [{name, p}])),
    ?assert(has(<<"role=\"radiogroup\" data-ah-value=\"b\"">>, H)),
    ?assert(has(<<"data-ah=\"radiobutton-group\"">>, H)),
    ?assertEqual(3, count(<<"type=\"radio\"">>, H)),
    ?assert(has(<<"value=\"b\" name=\"p\" checked">>, H)),
    ?assert(has(<<"ah-radiobutton ah-radiobutton-checked">>, H)),
    %% an unknown value selects nothing
    ?assert(has(<<"data-ah-value=\"\"">>, r(?M:radiobutton_group(items(), zzz, [], [])))).

radio_cards_test() ->
    Items = [{free, <<"Free">>, #{description => <<"<i>trial</i>">>}},
             {pro, <<"Pro">>, #{icon => <<"P">>}},
             {team, <<"Team">>, [{disabled, true}]}],
    H = r(?M:radio_cards(Items, pro, [], [{name, plan}, {columns, 2}, {align, start}])),
    ?assertMatch(<<"<div class=\"ah-radio-cards\" data-ah=\"radio-cards\" role=\"radiogroup\" data-ah-value=\"pro\" data-columns=\"2\" data-align=\"start\" data-disabled=\"false\">", _/binary>>, H),
    ?assert(has(<<"&lt;i&gt;trial&lt;/i&gt;">>, H)),
    ?assert(has(<<"<span class=\"ah-radio-cards__icon\" aria-hidden=\"true\">P</span>">>, H)),
    ?assert(has(<<"data-value=\"pro\" data-index=\"1\" data-selected=\"true\"">>, H)),
    ?assert(has(<<"data-value=\"team\" data-index=\"2\" data-selected=\"false\" data-disabled=\"true\"">>, H)),
    ?assertEqual(3, count(<<"name=\"plan\"">>, H)),
    ?assertError({aihtml, {bad_option, radio_cards, columns, <<"4">>}},
                 ?M:radio_cards(Items, pro, [], [{columns, 4}])),
    ?assertError({aihtml, {unknown_modifier, radio_cards, big, _}},
                 ?M:radio_cards(Items, pro, [big], [])).

%% --- rating --------------------------------------------------------

rating_test() ->
    H = r(?M:rating_group(5, 2.5, [lg, error], [{name, stars}, {precision, 0.5}, {id, <<"r">>}])),
    ?assertMatch(<<"<div class=\"ah-rating\" data-ah=\"rating\" role=\"radiogroup\" data-ah-value=\"2.5\" data-ah-max=\"5\" data-size=\"lg\" data-color=\"error\" data-precision=\"0.5\" data-readonly=\"false\" data-disabled=\"false\" data-allow-clear=\"true\" id=\"r\">", _/binary>>, H),
    ?assertEqual(5, count(<<"class=\"ah-rating__star\"">>, H)),
    ?assertEqual(2, count(<<"aria-checked=\"true\"">>, H)),
    ?assert(has(<<"style=\"width:50%;\"">>, H)),
    ?assertEqual(2, count(<<"style=\"width:100%;\"">>, H)),
    ?assert(has(<<"<input type=\"hidden\" name=\"stars\" value=\"2.5\">">>, H)),
    ?assert(has(<<"aria-label=\"3 / 5\"">>, H)).

rating_defaults_test() ->
    H = r(?M:rating_group(3, undefined, [], [{readonly, true}, {allow_clear, false}])),
    ?assert(has(<<"data-ah-value=\"0\"">>, H)),
    ?assert(has(<<"data-size=\"md\" data-color=\"warning\" data-precision=\"1\" data-readonly=\"true\"">>, H)),
    ?assert(has(<<"data-allow-clear=\"false\"">>, H)),
    ?assert(has(<<"aria-readonly=\"true\"">>, H)),
    ?assertEqual(3, count(<<"tabindex=\"-1\"">>, H)),
    ?assertNot(has(<<"type=\"hidden\"">>, H)),
    ?assertNot(has(<<" readonly">>, H)),
    D = r(?M:rating_group(2, 1.0, [], [{disabled, true}])),
    ?assert(has(<<"data-ah-value=\"1\"">>, D)),
    ?assertEqual(2, count(<<" disabled>">>, D)),
    ?assertError({aihtml, {conflicting_modifiers, rating_group, size, _}},
                 ?M:rating_group(5, 1, [sm, lg], [])),
    ?assertError({aihtml, {bad_option, rating_group, precision, 0.25}},
                 ?M:rating_group(5, 1, [], [{precision, 0.25}])),
    ?assertError({aihtml, {bad_max, rating_group, 0}}, ?M:rating_group(0, 1, [], [])).

%% --- catalog --------------------------------------------------------

catalog_test() ->
    Names = [N || #{name := N} <- ?M:catalog()],
    ?assertEqual([checkbox, radiobutton, switch_button, checkbox_group,
                  radiobutton_group, radio_cards, rating_group], Names),
    [begin
         ?assert(erlang:function_exported(?M, N, 4)),
         ?assertMatch(#{category := form, behavior := B} when is_binary(B), E)
     end || #{name := N} = E <- ?M:catalog()].

api_docs_test() ->
    [begin
         Documented = maps:keys(maps:get(option_docs, E)),
         Mods = lists:append([Ms || {Ms, _} <- maps:values(maps:get(groups, E, #{}))]),
         Keys = maps:get(options, E, []) ++ maps:get(flags, E, []) ++ Mods,
         ?assertEqual({N, []}, {N, Keys -- Documented}),
         ?assertMatch([_ | _], maps:get(methods, E))
     end || #{name := N} = E <- ?M:catalog()].

render_all_test() ->
    %% every component renders with a plain call
    Items = [{a, <<"A">>}, {b, <<"B">>}],
    [?assert(is_binary(r(H))) || H <- [?M:checkbox(<<"x">>, undefined, [], []),
                                       ?M:radiobutton(<<"x">>, a, [], []),
                                       ?M:switch_button(<<"x">>, undefined, [], []),
                                       ?M:checkbox_group(Items, [a], [], []),
                                       ?M:radiobutton_group(Items, a, [], []),
                                       ?M:radio_cards(Items, a, [], []),
                                       ?M:rating_group(5, 3, [], [])]].
