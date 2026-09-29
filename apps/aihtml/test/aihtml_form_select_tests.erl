-module(aihtml_form_select_tests).

-include_lib("eunit/include/eunit.hrl").

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
    ?assertError({aihtml, {bad_form_field, 42}}, ?M:form_layout([42], #{}, [], [])).

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
