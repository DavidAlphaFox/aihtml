%% Tests for aihtml_form_entry.
-module(aihtml_form_entry_tests).
-behaviour(aihtml_action).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_form_entry.hrl").

-export([action/4]).

-define(M, aihtml_form_entry).

-spec action(atom(), term(), map(), term()) -> ok.
action(_, _, _, _) -> ok.

r(Html) -> aihtml_html:render_binary(Html).

has(Needle, Hay) ->
    case binary:match(Hay, Needle) of
        nomatch -> ?debugFmt("~s not in~n~s", [Needle, Hay]), false;
        _ -> true
    end.

%%%===================================================================
%%% masked_input
%%%===================================================================

masked_input_test() ->
    H = r(?M:masked_input(<<"5551234">>, [<<"w-48">>],
                          [{mask, <<"(999) 999-9999">>}, {name, phone}, {id, m},
                           {title, <<"t">>}])),
    ?assert(has(<<"<div class=\"ah-masked-input-group w-48\" data-ah=\"masked-input\" "
                  "data-ah-value=\"5551234\" data-ah-mask=\"(999) 999-9999\" "
                  "data-ah-prompt=\"_\" id=\"m\" title=\"t\">">>, H)),
    ?assert(has(<<"value=\"(555) 123-4___\"">>, H)),
    ?assert(has(<<"inputmode=\"numeric\"">>, H)),
    ?assert(has(<<"<input type=\"hidden\" name=\"phone\" value=\"5551234\">">>, H)).

masked_fill_test() ->
    Show = fun(V, Mask) ->
                   H = r(?M:masked_input(V, [], [{mask, Mask}])),
                   {match, [D]} = re:run(H, <<"<input class=\"ah-masked-input\"[^>]* value=\"([^\"]*)\"">>, [{capture, all_but_first, binary}]),
                   {match, [Val]} = re:run(H, <<"data-ah-value=\"([^\"]*)\"">>,
                                           [{capture, all_but_first, binary}]),
                   {D, Val}
           end,
    %% literals in the value are taken where they stand, misfits skipped
    ?assertEqual({<<"(555) 123-4567">>, <<"5551234567">>}, Show(<<"(555) 123-4567">>, <<"(999) 999-9999">>)),
    ?assertEqual({<<"12/31/2026">>, <<"12312026">>}, Show(<<"12x31y2026">>, <<"99/99/9999">>)),
    ?assertEqual({<<"AB_-____">>, <<"AB">>}, Show(<<"A1B">>, <<"LLL-9999">>)),
    ?assertEqual({<<"1f:__">>, <<"1f">>}, Show(<<"1fG">>, <<"[0-9A-F][0-9A-F]:99">>)),
    ?assertEqual({<<"_____">>, <<>>}, Show(undefined, <<"99999">>)),
    %% no inputmode for a mask with letters
    ?assertNot(has_quiet(<<"inputmode">>, r(?M:masked_input(<<>>, [], [{mask, <<"LL">>}])))).

has_quiet(Needle, Hay) -> binary:match(Hay, Needle) =/= nomatch.

masked_options_test() ->
    H = r(?M:masked_input(<<"41111">>, [floating_label, square, readonly],
                          [{mask, <<"9999 9999">>}, {prompt_char, <<"*">>},
                           {include_literals, true}, {placeholder, <<"Card">>}, {name, c}])),
    ?assert(has(<<"class=\"ah-masked-input-group ah-masked-input-readonly "
                  "ah-masked-input-no-rounded\"">>, H)),
    ?assert(has(<<"data-ah-value=\"4111 1***\"">>, H)),
    ?assert(has(<<"data-ah-literals">>, H)),
    ?assert(has(<<"<label class=\"ah-masked-input-label ah-masked-input-label-float\">Card</label>">>, H)),
    ?assert(has(<<"placeholder=\"\"">>, H)),
    ?assert(has(<<" readonly">>, H)),
    %% include_literals with nothing typed: empty value
    ?assert(has(<<"data-ah-value=\"\"">>,
                r(?M:masked_input(undefined, [], [{include_literals, true}])))),
    D = r(?M:masked_input(<<>>, [disabled], [{placeholder, <<"Zip">>}])),
    ?assert(has(<<"ah-masked-input-disabled">>, D)),
    ?assert(has(<<"aria-label=\"Zip\"">>, D)),
    ?assertError({aihtml, {bad_option, prompt_char, <<"__">>}},
                 r(?M:masked_input(<<>>, [], [{prompt_char, <<"__">>}]))),
    ?assertError({aihtml, {bad_option, mask, _}}, r(?M:masked_input(<<>>, [], [{mask, <<"[0-9">>}]))).

%%%===================================================================
%%% formatted_input
%%%===================================================================

formatted_input_test() ->
    H = r(?M:formatted_input(255, [], [{id, f}, {radix, 16}, {min, 0}, {max, <<"1000">>},
                                       {name, n}, {upper_case, true}])),
    ?assert(has(<<"<div class=\"ah-fmt-input-group\" id=\"f\" data-ah=\"formatted-input\" "
                  "data-ah-value=\"255\" data-ah-radix=\"16\" data-ah-min=\"0\" "
                  "data-ah-max=\"1000\" data-ah-step=\"1\" data-ah-upper>">>, H)),
    ?assert(has(<<"value=\"FF\" role=\"spinbutton\" aria-valuenow=\"255\"">>, H)),
    ?assert(has(<<"<span class=\"ah-fmt-spin-up\">">>, H)),
    ?assert(has(<<"aria-controls=\"f-radix\"">>, H)),
    ?assert(has(<<"class=\"ah-fmt-popup-item ah-fmt-popup-item-active\" role=\"option\" "
                  "id=\"f-radix-16\" aria-selected=\"true\" data-radix=\"16\"">>, H)),
    ?assert(has(<<"<input type=\"hidden\" name=\"n\" value=\"255\">">>, H)).

formatted_values_test() ->
    Shown = fun(V, Attrs) ->
                    H = r(?M:formatted_input(V, [], Attrs)),
                    {match, [D, Dec]} = re:run(H, <<"class=\"ah-fmt-input\"[^>]* value=\"([^\"]*)\" "
                                                     "role=\"spinbutton\" aria-valuenow=\"([^\"]*)\"">>,
                                               [{capture, all_but_first, binary}]),
                    {D, Dec}
            end,
    ?assertEqual({<<"-ff">>, <<"-255">>}, Shown(-255, [{radix, 16}])),
    ?assertEqual({<<"101">>, <<"5">>}, Shown(<<"5">>, [{radix, 2}])),
    ?assertEqual({<<"17">>, <<"15">>}, Shown("15", [{radix, 8}])),
    Big = 1 bsl 70,
    ?assertEqual({integer_to_binary(Big), integer_to_binary(Big)}, Shown(Big, [])),
    ?assertEqual({<<"1.23456e+5">>, <<"123456">>}, Shown(123456, [{notation, exponential}])),
    ?assertEqual({<<"-7">>, <<"-7">>}, Shown(-7, [{notation, exponential}])),
    %% clamped to min / max
    ?assertEqual({<<"10">>, <<"10">>}, Shown(3, [{min, 10}])),
    ?assertEqual({<<"20">>, <<"20">>}, Shown(99, [{max, 20}])),
    %% no spin buttons, no menu
    P = r(?M:formatted_input(0, [disabled], [{spin_buttons, false}, {drop_down, false},
                                             {placeholder, <<"n">>}, {drop_down_width, 90}])),
    ?assertNot(has_quiet(<<"ah-fmt-spin">>, P)),
    ?assertNot(has_quiet(<<"ah-fmt-popup">>, P)),
    ?assert(has(<<"ah-fmt-input-group ah-fmt-input-disabled">>, P)),
    ?assert(has(<<"style=\"width:120px\"">>,
                r(?M:formatted_input(0, [], [{drop_down_width, 120}])))),
    ?assertError({aihtml, {bad_option, radix, 3}}, r(?M:formatted_input(1, [], [{radix, 3}]))),
    ?assertError({aihtml, {bad_option, value, <<"x">>}}, r(?M:formatted_input(<<"x">>, [], []))),
    ?assertError({aihtml, {bad_option, notation, sci}},
                 r(?M:formatted_input(1, [], [{notation, sci}]))).

%%%===================================================================
%%% range_selector
%%%===================================================================

range_selector_test() ->
    H = r(?M:range_selector({0, 100}, {10, 50}, [<<"mt-2">>],
                            [{major_ticks, 25}, {name, r}, {id, rs}])),
    ?assert(has(<<"<div class=\"ah-range-selector mt-2\" role=\"group\" data-ah=\"range-selector\" "
                  "data-ah-value=\"10,50\" data-ah-min=\"0\" data-ah-max=\"100\" data-ah-step=\"1\" "
                  "data-ah-page=\"25\" data-ah-min-span=\"0\" "
                  "data-ah-format=\"{&quot;f&quot;:&quot;number&quot;}\" id=\"rs\">">>, H)),
    ?assertEqual(5, count(<<"ah-range-selector-tick-major">>, H)),
    ?assertEqual(0, count(<<"ah-range-selector-tick-minor">>, H)),
    ?assert(has(<<"<div class=\"ah-range-selector-label\" style=\"left:75.0%\">75</div>">>, H)),
    ?assert(has(<<"<div class=\"ah-range-selector-slider\" style=\"left:10.0%;width:40.0%\">">>, H)),
    ?assert(has(<<"class=\"ah-range-selector-shutter-right\" style=\"left:50.0%;width:50.0%\"">>, H)),
    ?assert(has(<<"class=\"ah-range-selector-marker ah-range-selector-marker-left\" style=\"left:10.0%\" "
                  "role=\"slider\" tabindex=\"0\" aria-label=\"Minimum\" aria-valuenow=\"10\" "
                  "aria-valuetext=\"10\" aria-valuemin=\"0\" aria-valuemax=\"100\">"
                  "<span class=\"ah-range-selector-marker-value\">10</span>">>, H)),
    ?assert(has(<<"<input type=\"hidden\" name=\"r\" value=\"10,50\">">>, H)).

range_values_test() ->
    Val = fun(Range, V) ->
                  {match, [X]} = re:run(r(?M:range_selector(Range, V, [], [])),
                                        <<"data-ah-value=\"([^\"]*)\"">>,
                                        [{capture, all_but_first, binary}]),
                  X
          end,
    ?assertEqual(<<"0,200">>, Val({0, 200}, undefined)),
    ?assertEqual(<<"10,40">>, Val({0, 200, 10}, {42, 13})),       % ordered and snapped
    ?assertEqual(<<"0,200">>, Val({0, 200}, {-5, 900})),           % clamped
    ?assertEqual(<<"2.5,7.5">>, Val({0, 10, 0.1}, {2.5, 7.5})),
    ?assertEqual(<<"-800,-300">>, Val({-1000, -100, 10}, {-800, -300})),
    ?assertError({aihtml, {bad_range, {5, 1, 1}}}, r(?M:range_selector({5, 1}, undefined, [], []))),
    ?assertError({aihtml, {bad_range_value, 7}}, r(?M:range_selector({0, 10}, 7, [], []))),
    ?assertError({aihtml, {bad_option, min_span, 50}},
                 r(?M:range_selector({0, 10}, undefined, [], [{min_span, 50}]))),
    ?assertError({aihtml, {bad_option, labels_format, fancy}},
                 r(?M:range_selector({0, 10}, undefined, [], [{labels_format, fancy}]))).

range_ticks_test() ->
    H = r(?M:range_selector({0, 10, 0.5}, {2.5, 7.5}, [],
                            [{major_ticks, 2.5}, {minor_ticks, 0.5}, {show_minor_ticks, true},
                             {show_labels, false}, {show_markers, false}])),
    ?assertEqual(5, count(<<"tick-major">>, H)),
    ?assertEqual(21, count(<<"tick-minor">>, H)),
    ?assertEqual(0, count(<<"ah-range-selector-label\"">>, H)),
    ?assertEqual(2, count(<<";display:none\"">>, H)),
    T = r(?M:range_selector({0, 100}, undefined, [disabled],
                            [{tick_values, [0, 33, 100]}, {show_major_ticks, false}])),
    ?assertEqual(0, count(<<"tick-major">>, T)),
    ?assertEqual(3, count(<<"ah-range-selector-label\"">>, T)),
    ?assert(has(<<"left:33.0%\">33<">>, T)),
    ?assert(has(<<"ah-range-selector ah-range-selector-disabled">>, T)),
    ?assert(has(<<"tabindex=\"-1\"">>, T)).

count(Needle, Hay) -> length(binary:matches(Hay, Needle)).

range_formats_test() ->
    F = fun(V, Fmt) ->
                H = r(?M:range_selector({V, V + 1}, undefined, [],
                                        [{markers_format, Fmt}, {show_labels, false}])),
                {match, [X]} = re:run(H, <<"marker-value\">([^<]*)<">>,
                                      [{capture, all_but_first, binary}]),
                X
        end,
    Day = 1704067200000,                                   % 2024-01-01T00:00Z
    ?assertEqual(<<"12">>, F(12, number)),
    ?assertEqual(<<"12.25">>, F(12.25, number)),
    ?assertEqual(<<"12.3">>, F(12.25, {fixed, 1})),
    ?assertEqual(<<"$1,234,567">>, F(1234567, currency)),
    ?assertEqual(<<"$-1,500">>, F(-1500, currency)),
    ?assertEqual(<<"1/1/2024">>, F(Day, date)),
    ?assertEqual(<<"Jan">>, F(Day, month)),
    ?assertEqual(<<"12:00 AM">>, F(Day, time)),
    ?assertEqual(<<"4:05 PM">>, F(Day + (16 * 60 + 5) * 60000, time)),
    ?assertEqual(<<"≈ 3 mm"/utf8>>, F(3, {<<"≈ "/utf8>>, number, <<" mm">>})),
    %% the browser gets the markers' format
    H = r(?M:range_selector({0, 1}, undefined, [],
                            [{labels_format, currency}, {markers_format, {<<"<">>, {fixed, 2}, <<>>}}])),
    ?assert(has(<<"data-ah-format=\"{&quot;f&quot;:&quot;fixed&quot;,&quot;n&quot;:2,"
                  "&quot;p&quot;:&quot;&lt;&quot;,&quot;s&quot;:&quot;&quot;}\"">>, H)).

%%%===================================================================
%%% repeat_button
%%%===================================================================

repeat_button_test() ->
    H = r(?M:repeat_button(<<"+">>, 1, [secondary, sm, <<"px-2">>],
                           [{id, plus}, {interval, 100}, {title, <<"More">>}])),
    ?assertEqual(<<"<button class=\"ah-btn ah-btn-sm ah-btn-secondary px-2\" type=\"button\" "
                   "value=\"1\" id=\"plus\" data-ah=\"repeat-button\" data-ah-delay=\"300\" "
                   "data-ah-interval=\"100\" title=\"More\">+</button>">>, H),
    %% the same markup as button/4 apart from the behaviour's attributes
    B = r(aihtml_form_buttons:button(<<"+">>, 1, [secondary, sm, <<"px-2">>],
                                     [{id, plus}, {title, <<"More">>}])),
    ?assertEqual(B, re:replace(H, <<" data-ah=\"repeat-button\" data-ah-delay=\"300\" "
                                    "data-ah-interval=\"100\"">>, <<>>, [{return, binary}])),
    D = r(?M:repeat_button(<<"x">>, undefined, [round], [{disabled, true}, {icon, <<"*">>}])),
    ?assert(has(<<"ah-btn-round">>, D)),
    ?assert(has(<<"ah-btn-disabled">>, D)),
    ?assert(has(<<" disabled">>, D)),
    ?assert(has(<<"ah-btn-img">>, D)),
    ?assertError({aihtml, {bad_option, interval, 0}},
                 r(?M:repeat_button(<<"x">>, undefined, [], [{interval, 0}]))),
    ?assertError({aihtml, {unknown_modifier, repeat_button, huge, _}},
                 ?M:repeat_button(<<"x">>, undefined, [huge], [])).

%%%===================================================================
%%% catalog
%%%===================================================================

catalog_test() ->
    Names = [N || #{name := N} <- ?M:catalog()],
    ?assertEqual([masked_input, formatted_input, range_selector, repeat_button], Names),
    [begin
         Arity = length(binary:matches(maps:get(signature, E), <<",">>)) + 1,
         ?assert(erlang:function_exported(?M, N, Arity)),
         ?assertEqual(form, maps:get(category, E))
     end || #{name := N} = E <- ?M:catalog()].

catalog_docs_test() ->
    [begin
         #{flags := Fl, options := Op, option_docs := Docs, methods := Ms} =
             aihtml_catalog:entry(?M, N),
         ?assertEqual(lists:sort(Fl ++ Op), lists:sort(maps:keys(Docs))),
         [?assert(is_binary(A) andalso is_binary(Doc)) || #{args := A, doc := Doc} <- Ms]
     end || #{name := N} <- ?M:catalog()].

%%%===================================================================
%%% element records (designs/05-records.md)
%%%===================================================================

record_equals_builder_test() ->
    ?assertEqual(r(?M:masked_input(<<"12">>, [square, <<"w-40">>],
                                   [{id, m}, {name, n}, {mask, <<"99-99">>},
                                    {prompt_char, <<"#">>}, {title, <<"t">>}])),
                 r(#ah_masked_input{value = <<"12">>, square = true, css = [<<"w-40">>],
                                    id = m, name = n, mask = <<"99-99">>,
                                    prompt_char = <<"#">>, attrs = [{title, <<"t">>}]})),
    ?assertEqual(r(?M:formatted_input(7, [disabled], [{id, f}, {radix, 2}, {max, 9}])),
                 r(#ah_formatted_input{value = 7, disabled = true, id = f, radix = 2, max = 9})),
    ?assertEqual(r(?M:range_selector({0, 50, 5}, {5, 25}, [],
                                     [{major_ticks, 5}, {min_span, 5}, {labels_format, currency}])),
                 r(#ah_range_selector{range = {0, 50, 5}, value = {5, 25}, major_ticks = 5,
                                      min_span = 5, labels_format = currency})),
    ?assertEqual(r(?M:repeat_button(<<"+">>, up, [warning, lg], [{delay, 100}])),
                 r(#ah_repeat_button{body = <<"+">>, value = up, variant = warning, size = lg,
                                     delay = 100})).

builder_fills_fields_test() ->
    ?assertMatch(#ah_formatted_input{value = 1, radix = 16, spin_buttons = false,
                                     attrs = [{title, <<"t">>}]},
                 ?M:formatted_input(1, [], [{radix, 16}, {spin_buttons, false},
                                            {title, <<"t">>}])),
    ?assertMatch(#ah_repeat_button{variant = success, round = true, interval = 20, css = [<<"x">>]},
                 ?M:repeat_button(<<"a">>, undefined, [success, round, <<"x">>], [{interval, 20}])),
    ?assertError({aihtml, {record_only_field, ah_range_selector, postback}},
                 ?M:range_selector({0, 1}, undefined, [], [{postback, pick}])).

postback_test() ->
    Token = fun(Html) ->
                    {match, [T]} = re:run(r(Html), <<"data-ah-on=\"([a-z]+:[^\"]+)\"">>,
                                          [{capture, all_but_first, binary}]),
                    [Ev, Tok] = binary:split(T, <<":">>),
                    {ok, Ref} = aihtml_action:unsign(Tok),
                    {Ev, Ref}
            end,
    ?assertEqual({<<"change">>, {?MODULE, typed, #{}}},
                 Token(#ah_masked_input{postback = typed})),
    ?assertEqual({<<"change">>, {?MODULE, set, #{id => 1}}},
                 Token(#ah_formatted_input{postback = {set, #{id => 1}}})),
    ?assertEqual({<<"change">>, {other, picked, #{}}},
                 Token(#ah_range_selector{postback = picked, delegate = other})),
    ?assertEqual({<<"click">>, {?MODULE, step, 1}},
                 Token(#ah_repeat_button{body = <<"+">>, postback = {step, 1}})).

field_validation_test() ->
    ?assertError({aihtml, {bad_flag, masked_input, square, yes}},
                 r(#ah_masked_input{square = yes})),
    ?assertError({aihtml, {bad_modifier, repeat_button, variant, loud, _}},
                 r(#ah_repeat_button{variant = loud})),
    ?assertError({aihtml, {bad_option, delay, -1}}, r(#ah_repeat_button{delay = -1})),
    ?assertError({aihtml, {bad_option, min, x}}, r(#ah_formatted_input{min = x})),
    ?assertError({aihtml, {bad_option, major_ticks, -1}},
                 r(#ah_range_selector{major_ticks = -1})),
    ?assertError({aihtml, {modifier_in_css, range_selector, disabled}},
                 r(#ah_range_selector{css = [disabled]})).

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

default(ah_masked_input) -> #ah_masked_input{};
default(ah_formatted_input) -> #ah_formatted_input{};
default(ah_range_selector) -> #ah_range_selector{};
default(ah_repeat_button) -> #ah_repeat_button{}.
