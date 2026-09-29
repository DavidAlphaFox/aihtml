-module(aihtml_form_select_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_form_select.hrl").

-export([action/4]).

-define(M, aihtml_form_select).

r(Html) -> aihtml_html:render_binary(Html).

has(Bin, Sub) -> binary:match(Bin, Sub) =/= nomatch.

%%% dropdownlist

dropdownlist_markup_test() ->
    H = r(?M:dropdownlist([{a, <<"Alpha">>}, b], b, [primary, <<"w-40">>],
                          [{name, pick}, {id, <<"dd">>}])),
    ?assert(has(H, <<"class=\"ah-dropdownlist ah-dropdownlist-primary w-40\"">>)),
    ?assert(has(H, <<"role=\"combobox\"">>)),
    ?assert(has(H, <<"aria-expanded=\"false\"">>)),
    ?assert(has(H, <<"data-ah=\"dropdownlist\"">>)),
    ?assert(has(H, <<"data-ah-value=\"b\"">>)),
    ?assert(has(H, <<"<span class=\"ah-dropdownlist-content\">b</span>">>)),
    ?assert(has(H, <<"<input type=\"hidden\" name=\"pick\" value=\"b\">">>)),
    ?assert(has(H, <<"ah-listbox-item ah-listbox-item-selected\" role=\"option\" data-idx=\"1\" data-value=\"b\" aria-selected=\"true\"">>)),
    ?assert(has(H, <<"ah-dropdownlist-arrow">>)),
    ?assertNot(has(H, <<" name=\"pick\" role">>)).

dropdownlist_placeholder_test() ->
    H = r(?M:dropdownlist([a], undefined, [], [{placeholder, <<"Pick">>}])),
    ?assert(has(H, <<"ah-dropdownlist-content-placeholder\">Pick</span>">>)),
    ?assert(has(H, <<"data-ah-value=\"\"">>)),
    ?assertNot(has(H, <<"type=\"hidden\"">>)).

dropdownlist_groups_filter_disabled_test() ->
    H = r(?M:dropdownlist([{group, <<"G">>, [{x, <<"X">>, #{disabled => true}}, y]}], y,
                          [simple, disabled], [{filterable, true}, {dropdown_height, 120}])),
    ?assert(has(H, <<"ah-dropdownlist-simple">>)),
    ?assert(has(H, <<"ah-dropdownlist-disabled">>)),
    ?assert(has(H, <<"tabindex=\"-1\"">>)),
    ?assert(has(H, <<"aria-disabled=\"true\"">>)),
    ?assertNot(has(H, <<"ah-dropdownlist-arrow">>)),
    ?assert(has(H, <<"<li class=\"ah-listbox-group\" role=\"presentation\">G</li>">>)),
    ?assert(has(H, <<"ah-listbox-item-disabled">>)),
    ?assert(has(H, <<"ah-listbox-filter-input">>)),
    ?assert(has(H, <<"max-height:120px">>)).

dropdownlist_escapes_test() ->
    H = r(?M:dropdownlist([{<<"<v>">>, <<"<b>">>}], <<"<v>">>, [], [])),
    ?assertNot(has(H, <<"<b>">>)),
    ?assert(has(H, <<"&lt;b&gt;">>)).

dropdownlist_bad_modifier_test() ->
    ?assertError({aihtml, {unknown_modifier, dropdownlist, huge, _}},
                 ?M:dropdownlist([a], a, [huge], [])).

%%% select

select_test() ->
    H = r(?M:select([{a, <<"A">>}, {group, <<"G">>, [b, c]}], b, [sm],
                    [{name, s}, {placeholder, <<"Choose">>}])),
    ?assert(has(H, <<"<span class=\"ah-select ah-select-sm\">">>)),
    ?assert(has(H, <<"<select class=\"ah-select-control\" name=\"s\">">>)),
    ?assert(has(H, <<"<option value=\"\">Choose</option>">>)),
    ?assert(has(H, <<"<optgroup label=\"G\">">>)),
    ?assert(has(H, <<"<option value=\"b\" selected>b</option>">>)),
    ?assertNot(has(H, <<"placeholder">>)).

select_multiple_test() ->
    H = r(?M:select([a, b, c], [a, c], [], [{multiple, true}])),
    ?assert(has(H, <<"<option value=\"a\" selected>">>)),
    ?assert(has(H, <<"<option value=\"b\">">>)),
    ?assert(has(H, <<"<option value=\"c\" selected>">>)),
    ?assert(has(H, <<" multiple>">>)).

%%% slider

slider_single_test() ->
    H = r(?M:slider({0, 100}, 25, [], [{name, v}])),
    ?assert(has(H, <<"class=\"ah-slider ah-slider-horizontal ah-slider-buttons-hidden ah-slider-ticks-hidden\"">>)),
    ?assert(has(H, <<"role=\"slider\" tabindex=\"0\" aria-valuenow=\"25\"">>)),
    ?assert(has(H, <<"data-ah-value=\"25\"">>)),
    ?assert(has(H, <<"data-ah-step=\"1\"">>)),
    ?assert(has(H, <<"left:calc((100% - 18px) * 0.25)">>)),
    ?assert(has(H, <<"<input type=\"hidden\" name=\"v\" value=\"25\">">>)).

slider_clamps_test() ->
    ?assert(has(r(?M:slider({0, 10}, 42, [], [])), <<"data-ah-value=\"10\"">>)),
    ?assert(has(r(?M:slider({0, 10}, undefined, [], [])), <<"data-ah-value=\"0\"">>)).

slider_range_test() ->
    H = r(?M:slider({0, 100, 5}, {80, 20}, [success], [{min_range, 10}])),
    ?assert(has(H, <<"data-ah-value=\"20,80\"">>)),
    ?assert(has(H, <<"role=\"group\"">>)),
    ?assert(has(H, <<"ah-slider-range-slider">>)),
    ?assert(has(H, <<"ah-slider-thumb-start\" style=\"left:calc((100% - 18px) * 0.2)\" role=\"slider\"">>)),
    ?assert(has(H, <<"data-ah-min-range=\"10\"">>)),
    ?assert(has(H, <<"ah-slider-success">>)).

slider_ticks_buttons_vertical_test() ->
    H = r(?M:slider({0, 1, 0.1}, 0.5, [vertical, buttons, tooltip],
                    [{ticks, 0.5}, {ticks_position, both}])),
    ?assert(has(H, <<"ah-slider-vertical">>)),
    ?assert(has(H, <<"ah-slider-button-prev">>)),
    ?assertNot(has(H, <<"ah-slider-buttons-hidden">>)),
    ?assert(has(H, <<"ah-slider-tooltip">>)),
    ?assert(has(H, <<"ah-slider-ticks ah-slider-ticks-top">>)),
    ?assert(has(H, <<"ah-slider-ticks ah-slider-ticks-bottom">>)),
    ?assert(has(H, <<">0.5</div>">>)),
    ?assert(has(H, <<"top:calc((100% - 18px) * 0.5)">>)),
    ?assert(has(H, <<"data-ah-step=\"0.1\"">>)).

slider_bad_range_test() ->
    ?assertError({aihtml, {bad_slider_range, {5, 1, 1}}}, ?M:slider({5, 1}, 2, [], [])).

%%% field

field_test() ->
    H = r(?M:field(<<"Name">>, aihtml_html:void(input, [<<"ah-input">>], [{id, n}]), [top],
                   [{for, n}, {required, true}, {help, <<"Help">>}, {id, row}])),
    ?assert(has(H, <<"<div class=\"ah-form-row ah-form-row-top\" id=\"row\">">>)),
    ?assert(has(H, <<"<label class=\"ah-form-label\" for=\"n\">">>)),
    ?assert(has(H, <<"ah-form-required">>)),
    ?assert(has(H, <<"<div class=\"ah-form-help\">Help</div>">>)),
    ?assert(has(H, <<"ah-form-field">>)).

field_error_test() ->
    H = r(?M:field(<<"E">>, <<"x">>, [], [{error, <<"Bad">>}, {label_width, 90}])),
    ?assert(has(H, <<"ah-form-row ah-form-row-invalid">>)),
    ?assert(has(H, <<"ah-form-error ah-validator-error-label\" role=\"alert\">Bad">>)),
    ?assert(has(H, <<"width:90px;min-width:90px">>)).

%%% form_layout

form_layout_test() ->
    Fields = [{text, <<"Intro">>},
              {<<"A">>, <<"ctl-a">>},
              #{label => <<"B">>, key => b, control => fun(V) -> [<<"val=">>, V] end},
              {columns, [{<<"C">>, <<"c">>}, {<<"D">>, <<"d">>}]},
              blank,
              #{label => <<"H">>, control => <<"h">>, hidden => true, label_position => top}],
    H = r(?M:form_layout(Fields, #{b => <<"bee">>}, [bordered, bg],
                         [{label_width, 100}, {id, f}])),
    ?assert(has(H, <<"<form class=\"ah-form ah-form-bg ah-form-bordered\" style=\"padding:10px\" id=\"f\">">>)),
    ?assert(has(H, <<"ah-form-label-text\">Intro">>)),
    ?assert(has(H, <<"val=bee">>)),
    ?assert(has(H, <<"data-ah-key=\"b\"">>)),
    ?assert(has(H, <<"ah-form-row ah-form-columns">>)),
    ?assert(has(H, <<"<div class=\"ah-form-col\">">>)),
    ?assert(has(H, <<"ah-form-row-blank\" style=\"height:16px\"">>)),
    ?assert(has(H, <<"ah-form-row ah-form-row-top\" hidden>">>)),
    ?assert(has(H, <<"width:100px">>)).

form_layout_div_padding_test() ->
    H = r(?M:form_layout([], #{}, [], [{tag, 'div'}, {padding, {1, 2, 3, 4}},
                                       {label_position, top}])),
    ?assert(has(H, <<"<div class=\"ah-form\" style=\"padding:1px 2px 3px 4px\">">>)).

form_layout_bad_field_test() ->
    ?assertError({aihtml, {bad_form_field, 42}}, r(?M:form_layout([42], #{}, [], []))).

%%% validate

validate_test() ->
    A = aihtml_html:attrs(?M:validate([required, {min_length, 3}, {email, <<"Mail!">>},
                                       {{range, 1, 9}, <<"1-9">>}, {pattern, <<"[a-z]+">>},
                                       {hint, label}, {on, [blur, input]}])),
    {_, Json} = lists:keyfind(<<"data-ah-validate">>, 1, A),
    Rules = json:decode(Json),
    ?assertEqual([#{<<"rule">> => <<"required">>},
                  #{<<"rule">> => <<"min_length">>, <<"args">> => [3]},
                  #{<<"rule">> => <<"email">>, <<"msg">> => <<"Mail!">>},
                  #{<<"rule">> => <<"range">>, <<"args">> => [1, 9], <<"msg">> => <<"1-9">>},
                  #{<<"rule">> => <<"pattern">>, <<"args">> => [<<"[a-z]+">>]}], Rules),
    ?assertEqual({<<"data-ah-validate-on">>, <<"blur input">>},
                 lists:keyfind(<<"data-ah-validate-on">>, 1, A)),
    ?assertEqual({<<"data-ah-validate-hint">>, <<"label">>},
                 lists:keyfind(<<"data-ah-validate-hint">>, 1, A)),
    ?assertEqual({<<"aria-required">>, <<"true">>}, lists:keyfind(<<"aria-required">>, 1, A)).

validate_no_required_test() ->
    A = aihtml_html:attrs(?M:validate([email])),
    ?assertEqual(false, lists:keyfind(<<"aria-required">>, 1, A)),
    ?assertEqual(false, lists:keyfind(<<"data-ah-validate-hint">>, 1, A)).

validate_errors_test() ->
    ?assertError({aihtml, {unknown_rule, bogus}}, ?M:validate([bogus])),
    ?assertError({aihtml, {bad_rule_argument, min_length, x}}, ?M:validate([{min_length, x}])),
    ?assertError({aihtml, {bad_validate_option, hint, big}}, ?M:validate([{hint, big}])).

validate_on_native_input_test() ->
    H = r(aihtml_html:void(input, [], [{name, e}, ?M:validate([required])])),
    ?assert(has(H, <<"data-ah-validate=\"[{&quot;rule&quot;:&quot;required&quot;}]\"">>)).

%%% catalog

catalog_test() ->
    Names = [N || #{name := N} <- ?M:catalog()],
    ?assertEqual([dropdownlist, select, slider, field, form_layout], Names),
    [begin
         ?assert(is_binary(maps:get(signature, E))),
         ?assert(erlang:function_exported(?M, N, 4))
     end || #{name := N} = E <- ?M:catalog()].

catalog_docs_test() ->
    [begin
         Docs = maps:get(option_docs, E),
         [?assert(maps:is_key(O, Docs)) || O <- maps:get(options, E, [])],
         ?assert(is_list(maps:get(methods, E)))
     end || E <- ?M:catalog()].

%%% element records (designs/05-records.md)

-define(FRUITS, [{apple, <<"Apple">>}, {group, <<"G">>, [b, {c, <<"C">>, #{disabled => true}}]}]).

-spec action(atom(), term(), map(), term()) -> ok.
action(_, _, _, _) -> ok.

record_equals_builder_test() ->
    ?assertEqual(r(?M:dropdownlist(?FRUITS, b, [success, simple, <<"w-40">>],
                                   [{name, pick}, {id, dd}, {filterable, true},
                                    {title, <<"t">>}])),
                 r(#ah_dropdownlist{items = ?FRUITS, value = b, template = success,
                                    simple = true, css = [<<"w-40">>], name = pick, id = dd,
                                    filterable = true, attrs = [{title, <<"t">>}]})),
    ?assertEqual(r(?M:select(?FRUITS, [apple, b], [lg, block],
                             [{multiple, true}, {size, 4}, {name, s}, {id, sel}])),
                 r(#ah_select{items = ?FRUITS, value = [apple, b], size = lg, block = true,
                              id = sel, attrs = [{multiple, true}, {size, 4}, {name, s}]})),
    ?assertEqual(r(?M:slider({0, 10, 2}, {2, 8}, [vertical, buttons],
                             [{ticks, 2}, {min_range, 2}, {name, r}])),
                 r(#ah_slider{range = {0, 10, 2}, value = {2, 8}, orientation = vertical,
                              buttons = true, ticks = 2, min_range = 2, name = r})),
    ?assertEqual(r(?M:slider({0, 10}, 3, [], [])), r(#ah_slider{range = {0, 10}, value = 3})),
    ?assertEqual(r(?M:field(<<"L">>, <<"ctl">>, [top],
                            [{for, x}, {error, <<"E">>}, {required, true}, {id, row}])),
                 r(#ah_field{label = <<"L">>, body = <<"ctl">>, label_position = top, for = x,
                             error = <<"E">>, required = true, id = row})),
    Fields = [{<<"A">>, <<"a">>}, #{label => <<"B">>, key => b, control => fun(V) -> V end}],
    ?assertEqual(r(?M:form_layout(Fields, #{b => <<"bee">>}, [bordered],
                                  [{label_width, 80}, {padding, 4}, {action, <<"#x">>}])),
                 r(#ah_form_layout{fields = Fields, values = #{b => <<"bee">>}, bordered = true,
                                   label_width = 80, padding = 4,
                                   attrs = [{action, <<"#x">>}]})).

builder_fills_fields_test() ->
    D = ?M:dropdownlist([a], a, [danger, disabled, <<"x">>],
                        [{name, n}, {placeholder, <<"P">>}, {dropdown_height, 90},
                         {id, i}, {title, <<"t">>}]),
    ?assertMatch(#ah_dropdownlist{template = danger, disabled = true, simple = false,
                                  name = n, placeholder = <<"P">>, dropdown_height = 90,
                                  id = i, css = [<<"x">>], attrs = [{title, <<"t">>}]}, D),
    %% {size, N} stays an HTML attribute of the select
    S = ?M:select([a], a, [sm], [{size, 3}, {name, s}]),
    ?assertMatch(#ah_select{size = sm, attrs = [{<<"size">>, 3}, {name, s}]}, S),
    ?assert(has(r(S), <<"<select class=\"ah-select-control\" size=\"3\" name=\"s\">">>)),
    ?assertMatch(#ah_slider{range = {0, 5, 1}, value = 2, template = info, tooltip = true,
                            ticks = 1, ticks_position = both},
                 ?M:slider({0, 5}, 2, [info, tooltip], [{ticks, 1}, {ticks_position, both}])),
    ?assertMatch(#ah_field{label = <<"L">>, body = <<"c">>, help = <<"h">>, label_width = 60},
                 ?M:field(<<"L">>, <<"c">>, [], [{help, <<"h">>}, {label_width, 60}])),
    ?assertMatch(#ah_form_layout{fields = [], tag = 'div', label_position = top, bg = true},
                 ?M:form_layout([], #{}, [bg], [{tag, 'div'}, {label_position, top}])),
    ?assertError({aihtml, {record_only_field, ah_slider, postback}},
                 ?M:slider({0, 1}, 0, [], [{postback, x}])).

field_can_wrap_records_test() ->
    %% a control given as a record is rendered in place
    Dd = #ah_dropdownlist{items = [a, b], value = a, id = pick},
    H = r(#ah_field{label = <<"Pick">>, body = Dd, for = pick}),
    ?assert(has(H, <<"<label class=\"ah-form-label\" for=\"pick\">">>)),
    ?assert(has(H, <<"<div><div class=\"ah-dropdownlist\" role=\"combobox\"">>)),
    H2 = r(?M:form_layout([#{label => <<"S">>, key => s,
                             control => fun(V) -> #ah_slider{value = V} end}],
                          #{s => 30}, [], [])),
    ?assert(has(H2, <<"data-ah-value=\"30\"">>)).

postback_test() ->
    Token = fun(Html) ->
                    {match, [T]} = re:run(r(Html), <<"data-ah-on=\"([a-z]+:[^\"]+)\"">>,
                                          [{capture, all_but_first, binary}]),
                    [Ev, Tok] = binary:split(T, <<":">>),
                    {ok, Ref} = aihtml_action:unsign(Tok),
                    {Ev, Ref}
            end,
    ?assertEqual({<<"change">>, {?MODULE, pick, #{}}},
                 Token(#ah_dropdownlist{items = [a], postback = pick})),
    ?assertEqual({<<"change">>, {?MODULE, pick, 1}},
                 Token(#ah_select{items = [a], postback = {pick, 1}})),
    ?assertEqual({<<"change">>, {other_mod, vol, #{}}},
                 Token(#ah_slider{value = 3, postback = vol, delegate = other_mod})),
    ?assertEqual({<<"submit">>, {?MODULE, save, #{id => 7}}},
                 Token(#ah_form_layout{postback = {save, #{id => 7}}})),
    %% select binds its postback on the <select>, not the wrapper
    ?assert(has(r(#ah_select{items = [a], postback = pick}),
                <<"<select class=\"ah-select-control\" data-ah-on=">>)),
    ?assertError({aihtml, {no_postback_event, ah_field}},
                 r(#ah_field{postback = x})),
    ?assertError({aihtml, {no_postback_event, ah_form_layout}},
                 r(#ah_form_layout{tag = 'div', postback = x})).

field_validation_test() ->
    ?assertError({aihtml, {bad_modifier, dropdownlist, template, info, _}},
                 r(#ah_dropdownlist{template = info})),
    ?assertError({aihtml, {bad_flag, dropdownlist, simple, yes}},
                 r(#ah_dropdownlist{simple = yes})),
    ?assertError({aihtml, {bad_modifier, select, size, md, _}}, r(#ah_select{size = md})),
    ?assertError({aihtml, {bad_modifier, slider, orientation, up, _}},
                 r(#ah_slider{orientation = up})),
    ?assertError({aihtml, {bad_slider_range, {5, 1, 1}}}, r(#ah_slider{range = {5, 1}})),
    ?assertError({aihtml, {bad_slider_range, {0, 1, 0}}}, r(#ah_slider{range = {0, 1, 0}})),
    ?assertError({aihtml, {bad_option, ticks_position, left}},
                 r(#ah_slider{ticks = 1, ticks_position = left})),
    ?assertError({aihtml, {bad_modifier, field, label_position, middle, _}},
                 r(#ah_field{label_position = middle})),
    ?assertError({aihtml, {bad_flag, form_layout, bordered, 1}},
                 r(#ah_form_layout{bordered = 1})),
    ?assertError({aihtml, {modifier_in_css, select, lg}}, r(#ah_select{css = [lg]})),
    ?assertError({aihtml, {bad_item, {1, 2, 3, 4}}},
                 r(#ah_dropdownlist{items = [{1, 2, 3, 4}]})),
    %% groups without a default may stay undefined
    ?assert(has(r(#ah_select{}), <<"<span class=\"ah-select\">">>)),
    ?assert(has(r(#ah_dropdownlist{}), <<"class=\"ah-dropdownlist\"">>)).

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

default(ah_dropdownlist) -> #ah_dropdownlist{};
default(ah_select) -> #ah_select{};
default(ah_slider) -> #ah_slider{};
default(ah_field) -> #ah_field{};
default(ah_form_layout) -> #ah_form_layout{}.
