-module(aihtml_form_buttons_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_form_buttons.hrl").

-behaviour(aihtml_element).
-export([render/1, action/4]).

-define(M, aihtml_form_buttons).

r(H) -> aihtml_html:render_binary(H).

has(Sub, Bin) -> binary:match(Bin, Sub) =/= nomatch.

-define(has(Sub, Bin), ?assert(has(Sub, Bin))).
-define(hasnt(Sub, Bin), ?assertNot(has(Sub, Bin))).

%%% button

button_default_test() ->
    ?assertEqual(<<"<button class=\"ah-btn ah-btn-primary\" type=\"button\" value=\"save\">Save</button>">>,
                 r(?M:button(<<"Save">>, save, [], []))).

button_no_value_test() ->
    ?hasnt(<<"value=">>, r(?M:button(<<"Go">>, undefined, [], []))).

button_variants_and_sizes_test() ->
    H = r(?M:button(<<"x">>, undefined, [outlined, lg, round, <<"mt-2">>], [])),
    ?has(<<"class=\"ah-btn ah-btn-lg ah-btn-outlined ah-btn-round mt-2\"">>, H),
    ?has(<<"class=\"ah-btn ah-btn-primary\"">>, r(?M:button(<<"x">>, undefined, [md], []))),
    ?has(<<"ah-btn-sm">>, r(?M:button(<<"x">>, undefined, [sm], []))).

button_type_override_test() ->
    H = r(?M:button(<<"x">>, undefined, [], [{type, submit}])),
    ?has(<<"type=\"submit\"">>, H),
    ?hasnt(<<"type=\"button\"">>, H).

button_disabled_test() ->
    H = r(?M:button(<<"x">>, undefined, [], [{disabled, true}])),
    ?has(<<"ah-btn-disabled">>, H),
    ?has(<<" disabled>">>, H),
    ?hasnt(<<"ah-btn-disabled">>, r(?M:button(<<"x">>, undefined, [], [{disabled, false}]))).

button_escaping_test() ->
    H = r(?M:button(<<"<b>&\"">>, <<"a\"b">>, [], [{title, <<"<t>">>}])),
    ?has(<<"&lt;b&gt;&amp;&quot;</button>">>, H),
    ?has(<<"value=\"a&quot;b\"">>, H),
    ?has(<<"title=\"&lt;t&gt;\"">>, H),
    ?hasnt(<<"<b>">>, H).

button_icon_test() ->
    H = r(?M:button(<<"Go">>, undefined, [], [{icon, <<"*">>}, {icon_position, right}])),
    ?has(<<"ah-btn-img-right">>, H),
    ?has(<<"<span class=\"ah-btn-text\">Go</span><span class=\"ah-btn-img\" aria-hidden=\"true\">*</span>">>, H),
    ?hasnt(<<"icon">>, binary:replace(H, <<"ah-btn-img">>, <<>>, [global])),
    I = r(?M:button(<<"Go">>, undefined, [], [{img, <<"/a.png">>}])),
    ?has(<<"<img class=\"ah-btn-img\" src=\"/a.png\" width=\"16\" height=\"16\" alt=\"\">">>, I),
    %% field values are checked when rendering
    ?assertError({aihtml, {bad_option, icon_position, middle}},
                 r(?M:button(<<"Go">>, undefined, [], [{icon, <<"*">>}, {icon_position, middle}]))).

button_modifier_validation_test() ->
    ?assertError({aihtml, {unknown_modifier, button, huge, _}},
                 ?M:button(<<"x">>, undefined, [huge], [])),
    ?assertError({aihtml, {conflicting_modifiers, button, variant, _}},
                 ?M:button(<<"x">>, undefined, [primary, error], [])),
    ?assertError({aihtml, {conflicting_modifiers, button, size, _}},
                 ?M:button(<<"x">>, undefined, [sm, lg], [])).

%%% link_button

link_button_test() ->
    H = r(?M:link_button(<<"Docs & more">>, <<"/docs?a=1&b=2">>, [outlined], [{target, <<"_blank">>}])),
    ?assertEqual(<<"<a class=\"ah-btn ah-btn-outlined ah-link-btn\" role=\"link\" "
                   "href=\"/docs?a=1&amp;b=2\" target=\"_blank\">Docs &amp; more</a>">>, H).

link_button_disabled_test() ->
    H = r(?M:link_button(<<"x">>, <<"/x">>, [], [{disabled, true}])),
    ?hasnt(<<"href">>, H),
    ?hasnt(<<" disabled">>, H),
    ?has(<<"aria-disabled=\"true\"">>, H),
    ?has(<<"tabindex=\"-1\"">>, H),
    ?has(<<"ah-btn-disabled">>, H).

%%% toggle_button

toggle_button_test() ->
    Off = r(?M:toggle_button(<<"B">>, false, [default], [])),
    ?has(<<"data-ah=\"toggle-button\"">>, Off),
    ?has(<<"data-ah-value=\"false\"">>, Off),
    ?has(<<"aria-pressed=\"false\"">>, Off),
    ?has(<<"value=\"false\"">>, Off),
    ?hasnt(<<"ah-btn-toggled">>, Off),
    On = r(?M:toggle_button(<<"B">>, true, [], [])),
    ?has(<<"ah-btn-toggled">>, On),
    ?has(<<"data-ah-value=\"true\"">>, On).

toggle_button_hidden_input_test() ->
    H = r(?M:toggle_button(<<"B">>, true, [], [{name, bold}, {id, t}])),
    ?has(<<"<input type=\"hidden\" name=\"bold\" value=\"true\" data-ah-input>">>, H),
    %% the name is on the hidden input only
    ?assertEqual(1, length(binary:matches(H, <<"name=">>))),
    ?has(<<"id=\"t\"">>, H),
    ?assertError(function_clause, ?M:toggle_button(<<"B">>, yes, [], [])).

toggle_button_on_change_test() ->
    Attrs = [{<<"data-ah-on">>, {actions, [{<<"change">>, <<"TOKEN">>, #{}}]}}],
    H = r(?M:toggle_button(<<"B">>, false, [], Attrs)),
    ?has(<<"data-ah-on=\"change:TOKEN\"">>, H).

%%% button_group

-define(ITEMS, [{list, <<"List">>}, {grid, <<"Grid">>}, {board, <<"Board">>}]).

button_group_default_test() ->
    H = r(?M:button_group([<<"A">>, {b, <<"B">>, [{title, <<"t">>}]}], undefined, [], [])),
    ?has(<<"class=\"ah-btn-group ah-btn-group-horizontal ah-btn-group-rounded\"">>, H),
    ?has(<<"role=\"group\"">>, H),
    ?has(<<"data-ah=\"button-group\"">>, H),
    ?hasnt(<<"data-ah-value">>, H),
    ?has(<<"ah-btn-group-btn ah-btn-group-btn-first\" type=\"button\" value=\"A\"">>, H),
    ?has(<<"ah-btn-group-btn ah-btn-group-btn-last\" type=\"button\" value=\"b\" data-value=\"b\" title=\"t\"">>, H).

button_group_radio_test() ->
    H = r(?M:button_group(?ITEMS, grid, [radio], [{name, view}])),
    ?has(<<"role=\"radiogroup\"">>, H),
    ?has(<<"ah-btn-group-radio">>, H),
    ?has(<<"data-ah-value=\"grid\"">>, H),
    ?has(<<"<input type=\"hidden\" name=\"view\" value=\"grid\" data-ah-input>">>, H),
    ?has(<<"ah-btn-group-btn-selected\" type=\"button\" value=\"grid\" data-value=\"grid\" "
           "tabindex=\"0\" role=\"radio\" aria-checked=\"true\"">>, H),
    ?has(<<"value=\"list\" data-value=\"list\" tabindex=\"-1\" role=\"radio\" aria-checked=\"false\"">>, H),
    %% no selection: the first enabled button takes focus
    N = r(?M:button_group([{a, <<"A">>, [{disabled, true}]}, {b, <<"B">>}], undefined, [radio], [])),
    ?has(<<"data-ah-value=\"\"">>, N),
    ?has(<<"value=\"b\" data-value=\"b\" tabindex=\"0\"">>, N),
    ?has(<<"ah-btn-group-btn-disabled\" type=\"button\" value=\"a\" data-value=\"a\" disabled">>, N).

button_group_checkbox_test() ->
    H = r(?M:button_group(?ITEMS, [list, board], [checkbox], [])),
    ?has(<<"data-ah-value=\"list,board\"">>, H),
    ?has(<<"value=\"list\" data-value=\"list\" aria-pressed=\"true\"">>, H),
    ?has(<<"value=\"grid\" data-value=\"grid\" aria-pressed=\"false\"">>, H),
    ?assertEqual(r(?M:button_group(?ITEMS, [list, board], [checkbox], [])),
                 r(?M:button_group(?ITEMS, <<"list,board">>, [checkbox], []))).

button_group_modifiers_test() ->
    H = r(?M:button_group(?ITEMS, list, [radio, vertical, square, outlined], [])),
    ?has(<<"class=\"ah-btn-group ah-btn-group-outlined ah-btn-group-radio ah-btn-group-vertical\"">>, H),
    ?assertError({aihtml, {conflicting_modifiers, button_group, mode, _}},
                 ?M:button_group(?ITEMS, list, [radio, checkbox], [])),
    ?assertError({aihtml, {unknown_modifier, button_group, primary, _}},
                 ?M:button_group(?ITEMS, list, [primary], [])).

button_group_disabled_test() ->
    H = r(?M:button_group(?ITEMS, list, [radio], [{disabled, true}])),
    ?has(<<"ah-btn-group-disabled">>, H),
    ?has(<<"aria-disabled=\"true\"">>, H),
    ?assertEqual(3, length(binary:matches(H, <<" disabled">>))).

button_group_escaping_test() ->
    H = r(?M:button_group([{<<"a\"b">>, <<"<i>">>}], <<"a\"b">>, [radio], [])),
    ?has(<<"data-value=\"a&quot;b\"">>, H),
    ?has(<<"data-ah-value=\"a&quot;b\"">>, H),
    ?has(<<"&lt;i&gt;">>, H).

%%% segmented_control

segmented_control_test() ->
    H = r(?M:segmented_control(?ITEMS, grid, [lg, full_width], [{name, layout}, {id, s}])),
    ?has(<<"<div class=\"ah-segmented-control\" role=\"tablist\" data-size=\"lg\" "
           "data-full-width=\"true\" data-disabled=\"false\" data-ah=\"segmented-control\" "
           "data-ah-value=\"grid\" id=\"s\">">>, H),
    ?has(<<"<button class=\"ah-segmented-control__item\" type=\"button\" role=\"tab\" "
           "aria-selected=\"true\" data-value=\"grid\" data-state=\"active\" "
           "data-disabled=\"false\" tabindex=\"0\">Grid</button>">>, H),
    ?has(<<"data-state=\"inactive\"">>, H),
    ?has(<<"<input type=\"hidden\" name=\"layout\" value=\"grid\" data-ah-input>">>, H),
    ?has(<<"data-size=\"md\"">>, r(?M:segmented_control(?ITEMS, grid, [], []))).

segmented_control_disabled_test() ->
    H = r(?M:segmented_control([{a, <<"A">>}, {b, <<"B">>, [{disabled, true}]}], a, [], [])),
    ?has(<<"data-value=\"b\" data-state=\"inactive\" data-disabled=\"true\" tabindex=\"-1\" disabled">>, H),
    D = r(?M:segmented_control(?ITEMS, list, [], [{disabled, true}])),
    ?has(<<"data-disabled=\"true\" aria-disabled=\"true\"">>, D),
    ?assertError({aihtml, {unknown_modifier, segmented_control, xl, _}},
                 ?M:segmented_control(?ITEMS, list, [xl], [])).

%%% dropdown_button

-define(MENU, [{draft, <<"Draft">>}, divider, {copy, <<"Copy & paste">>, [{disabled, true}]}]).

dropdown_button_test() ->
    H = r(?M:dropdown_button(<<"Actions">>, ?MENU, [primary, sm, rounded], [{name, act}, {id, d}])),
    ?has(<<"<div class=\"ah-dropdown-btn ah-dropdown-btn-sm ah-dropdown-btn-primary "
           "ah-dropdown-btn-rounded\" data-ah=\"dropdown-button\" data-ah-value=\"\" id=\"d\">">>, H),
    ?has(<<"<button class=\"ah-dropdown-btn-wrapper\" type=\"button\" aria-haspopup=\"menu\" "
           "aria-expanded=\"false\"><div class=\"ah-dropdown-btn-content\">Actions</div>">>, H),
    ?has(<<"<div class=\"ah-dropdown-btn-popup\" role=\"menu\" hidden>">>, H),
    ?has(<<"<button class=\"ah-dropdown-btn-item\" type=\"button\" role=\"menuitem\" "
           "tabindex=\"-1\" data-value=\"draft\"><span>Draft</span></button>">>, H),
    ?has(<<"<div class=\"ah-dropdown-btn-divider\" role=\"separator\"></div>">>, H),
    ?has(<<"data-value=\"copy\" disabled><span>Copy &amp; paste</span>">>, H),
    ?has(<<"<input type=\"hidden\" name=\"act\" value=\"\" data-ah-input>">>, H).

dropdown_button_value_test() ->
    H = r(?M:dropdown_button(<<"A">>, ?MENU, [], [{value, draft}, {auto_open, true}])),
    ?has(<<"data-ah-value=\"draft\"">>, H),
    ?has(<<"ah-dropdown-btn-item selected\"">>, H),
    ?has(<<"ah-dropdown-btn-auto-open">>, H),
    ?hasnt(<<"auto-open=">>, H),
    ?hasnt(<<"auto_open">>, H).

dropdown_button_disabled_test() ->
    H = r(?M:dropdown_button(<<"A">>, ?MENU, [], [{disabled, true}])),
    ?has(<<"ah-dropdown-btn-disabled">>, H),
    ?has(<<"aria-disabled=\"true\"">>, H),
    ?has(<<"aria-expanded=\"false\" disabled>">>, H),
    ?assertError({aihtml, {unknown_modifier, dropdown_button, info, _}},
                 ?M:dropdown_button(<<"A">>, ?MENU, [info], [])).

dropdown_button_escaping_test() ->
    H = r(?M:dropdown_button(<<"<x>">>, [{<<"\"v">>, <<"<y>">>}], [], [])),
    ?has(<<"&lt;x&gt;">>, H),
    ?has(<<"&lt;y&gt;">>, H),
    ?has(<<"data-value=\"&quot;v\"">>, H).

%%% split_button

split_button_test() ->
    H = r(?M:split_button(<<"Save">>, [{a, <<"A">>, [{icon, <<"+">>}]}, divider,
                                        {b, <<"B">>, [{disabled, true}]}],
                          [success, lg], [{menu_align, start}, {id, s}])),
    ?has(<<"<div class=\"ah-split-button\" data-variant=\"success\" data-size=\"lg\" "
           "data-disabled=\"false\" data-menu-align=\"start\" data-open=\"false\" "
           "data-ah=\"split-button\" data-ah-value=\"\" id=\"s\">">>, H),
    ?has(<<"<button class=\"ah-split-button__main ah-btn ah-btn-success\" type=\"button\">Save</button>">>, H),
    ?has(<<"class=\"ah-split-button__arrow ah-btn ah-btn-success\" type=\"button\" "
           "aria-haspopup=\"menu\" aria-expanded=\"false\" aria-label=\"Open menu\">">>, H),
    ?has(<<"<div class=\"ah-split-button__menu\" role=\"menu\">">>, H),
    ?has(<<"data-value=\"a\" data-disabled=\"false\"><span class=\"ah-split-button__item-icon\" "
           "aria-hidden=\"true\">+</span><span>A</span>">>, H),
    ?hasnt(<<"icon=">>, H),
    ?has(<<"data-value=\"b\" data-disabled=\"true\" disabled>">>, H),
    ?has(<<"<div class=\"ah-split-button__divider\" role=\"separator\"></div>">>, H).

split_button_defaults_test() ->
    H = r(?M:split_button(<<"Go">>, [], [], [{name, n}, {value, x}])),
    ?has(<<"data-variant=\"primary\" data-size=\"md\"">>, H),
    ?has(<<"data-menu-align=\"end\"">>, H),
    ?has(<<"data-ah-value=\"x\"">>, H),
    ?has(<<"<input type=\"hidden\" name=\"n\" value=\"x\" data-ah-input>">>, H).

split_button_disabled_test() ->
    H = r(?M:split_button(<<"Go">>, [], [], [{disabled, true}])),
    ?has(<<"data-disabled=\"true\"">>, H),
    ?assertEqual(2, length(binary:matches(H, <<" disabled">>))),
    ?assertError({aihtml, {bad_option, menu_align, middle}},
                 r(?M:split_button(<<"Go">>, [], [], [{menu_align, middle}]))),
    ?assertError({aihtml, {conflicting_modifiers, split_button, variant, _}},
                 ?M:split_button(<<"Go">>, [], [primary, error], [])).

%%% catalog (demos live in aihtml_example)

catalog_test() ->
    Cat = ?M:catalog(),
    Names = [N || #{name := N} <- Cat],
    ?assertEqual([button, link_button, toggle_button, button_group,
                  segmented_control, dropdown_button, split_button], Names),
    [begin
         ?assert(erlang:function_exported(?M, N, 4)),
         #{category := form, signature := S, root := <<"ah-", _/binary>>} = E,
         ?assert(is_binary(S))
     end || #{name := N} = E <- Cat],
    %% every behaviour named in the catalog is rendered by its component
    Behaviors = [B || #{behavior := B} <- Cat],
    ?assertEqual(5, length(Behaviors)).

catalog_docs_test() ->
    [begin
         Docs = maps:get(option_docs, E, #{}),
         ?assertEqual(lists:sort(maps:get(options, E, []) ++ maps:get(flags, E, [])),
                      lists:sort(maps:keys(Docs))),
         [?assert(is_binary(D) andalso D =/= <<>>) || D <- maps:values(Docs)],
         Ms = maps:get(methods, E),
         [#{name := _, args := <<"(", _/binary>>, doc := _} = X || X <- Ms],
         ?assertEqual(maps:get(behavior, E, none) =/= none, Ms =/= [])
     end || E <- ?M:catalog()].

%%% element records (designs/05-records.md)

%% A test element that renders through this module.
-record(test_badge, {?AH_BASE(?MODULE), text = <<>>}).

-spec render(tuple()) -> aihtml_html:html().
render(#test_badge{text = T} = R) ->
    aihtml_html:el(span, T, [<<"badge">> | R#test_badge.css],
                   aihtml_element:root_attrs(R, none));
%% wraps the default rendering of a button
render(#ah_button{} = B) ->
    aihtml_html:el(span, ?M:render(B#ah_button{module = ?M}), [<<"wrap">>], []).

-spec action(atom(), term(), map(), term()) -> ok.
action(_, _, _, _) -> ok.

record_equals_builder_test() ->
    ?assertEqual(r(?M:button(<<"Save">>, save, [outlined, lg, round, <<"mt-2">>],
                             [{disabled, true}, {id, s}, {title, <<"t">>}])),
                 r(#ah_button{body = <<"Save">>, value = save, variant = outlined, size = lg,
                              round = true, css = [<<"mt-2">>], disabled = true, id = s,
                              attrs = [{title, <<"t">>}]})),
    ?assertEqual(r(?M:button_group(?ITEMS, [list], [checkbox, vertical], [{name, v}])),
                 r(#ah_button_group{items = ?ITEMS, value = [list], mode = checkbox,
                                    orientation = vertical, name = v})),
    ?assertEqual(r(?M:dropdown_button(<<"A">>, ?MENU, [sm], [{value, draft}, {auto_open, true}])),
                 r(#ah_dropdown_button{body = <<"A">>, items = ?MENU, size = sm,
                                       value = draft, auto_open = true})).

builder_fills_fields_test() ->
    B = ?M:toggle_button(<<"B">>, true, [secondary, <<"x">>],
                         [{name, bold}, {disabled, true}, {icon, <<"*">>}, {title, <<"t">>},
                          on(change)]),
    ?assertMatch(#ah_toggle_button{value = true, variant = secondary, size = md,
                                   name = bold, disabled = true, icon = <<"*">>,
                                   css = [<<"x">>]}, B),
    [{title, <<"t">>}, {<<"data-ah-on">>, _} | _] = B#ah_toggle_button.attrs,
    ?assertError({aihtml, {record_only_field, ah_button, postback}},
                 ?M:button(<<"x">>, undefined, [], [{postback, save}])).

postback_test() ->
    Token = fun(Html) ->
                    {match, [T]} = re:run(r(Html), <<"data-ah-on=\"([a-z]+:[^\"]+)\"">>,
                                          [{capture, all_but_first, binary}]),
                    [Ev, Tok] = binary:split(T, <<":">>),
                    {ok, Ref} = aihtml_action:unsign(Tok),
                    {Ev, Ref}
            end,
    ?assertEqual({<<"click">>, {?MODULE, save, #{id => 7}}},
                 Token(#ah_button{body = <<"Save">>, postback = {save, #{id => 7}}})),
    ?assertEqual({<<"change">>, {?MODULE, pick, #{}}},
                 Token(#ah_segmented_control{items = ?ITEMS, postback = pick})),
    ?assertEqual({<<"click">>, {other_mod, go, 1}},
                 Token(#ah_split_button{body = <<"Go">>, postback = {go, 1},
                                        delegate = other_mod})),
    %% button_group: click in default mode, change in radio mode
    ?assertMatch({<<"click">>, _}, Token(#ah_button_group{items = ?ITEMS, postback = a})),
    ?assertMatch({<<"change">>, _}, Token(#ah_button_group{items = ?ITEMS, mode = radio,
                                                           postback = a})),
    H = r(#ah_button{postback = {save, #{}, #{debounce => 300, confirm => <<"Sure?">>}}}),
    ?has(<<"data-ah-confirm=\"Sure?\"">>, H),
    ?assertMatch({match, _}, re:run(H, <<"data-ah-on=\"click:[^\":]+:300\"">>)),
    ?assertError({aihtml, {bad_postback, ah_button, "save"}},
                 r(#ah_button{postback = "save"})).

field_validation_test() ->
    ?assertError({aihtml, {bad_modifier, button, variant, huge, _}},
                 r(#ah_button{variant = huge})),
    ?assertError({aihtml, {bad_modifier, button, size, primary, _}},
                 r(#ah_button{size = primary})),
    ?assertError({aihtml, {bad_flag, button, round, yes}}, r(#ah_button{round = yes})),
    ?assertError({aihtml, {modifier_in_css, button, primary}},
                 r(#ah_button{css = [primary]})),
    %% a group without a default may stay undefined
    ?has(<<"class=\"ah-dropdown-btn\"">>,
         r(#ah_dropdown_button{})).

custom_module_test() ->
    %% an element of another module, nested in a component
    H = r(#ah_button{body = #test_badge{text = <<"<3">>, css = [<<"ml-1">>], id = b}}),
    ?has(<<"<span class=\"badge ml-1\" id=\"b\">&lt;3</span></button>">>, H),
    ?assertError({aihtml, {no_postback_event, test_badge}},
                 r(#test_badge{postback = x})),
    %% one button rendered by another module
    ?assertEqual(<<"<span class=\"wrap\">", (r(#ah_button{body = <<"x">>}))/binary, "</span>">>,
                 r(#ah_button{module = ?MODULE, body = <<"x">>})),
    ?assertError({aihtml, {no_render, no_such_module, ah_button}},
                 r(#ah_button{module = no_such_module})).

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

default(ah_button) -> #ah_button{};
default(ah_link_button) -> #ah_link_button{};
default(ah_toggle_button) -> #ah_toggle_button{};
default(ah_button_group) -> #ah_button_group{};
default(ah_segmented_control) -> #ah_segmented_control{};
default(ah_dropdown_button) -> #ah_dropdown_button{};
default(ah_split_button) -> #ah_split_button{}.

on(Event) -> aihtml:on(Event, {?MODULE, x, #{}}).
