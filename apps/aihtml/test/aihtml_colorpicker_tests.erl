%% Tests for aihtml_colorpicker.
-module(aihtml_colorpicker_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_colorpicker.hrl").

-export([action/4]).

-define(M, aihtml_colorpicker).

r(Html) -> aihtml_html:render_binary(Html).

has(Bin, Part) -> binary:match(Bin, Part) =/= nomatch.

count(Bin, Part) -> length(binary:matches(Bin, Part)).

bad(Fun) ->
    try Fun(), no_error
    catch error:{aihtml, E} -> element(1, E)
    end.

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
    ?assertEqual(bad_value, bad(fun() -> r(?M:colorpicker(<<"#12345678">>, [], [])) end)),
    ?assertEqual(bad_value,
                 bad(fun() -> r(?M:colorpicker(undefined, [], [{swatches, [<<"x">>]}])) end)).

colorpicker_escaping_test() ->
    H = r(?M:colorpicker(<<"#000">>, [clearable],
                         [{clear_label, <<"<x>">>}, {aria_label, <<"a\"b">>}])),
    ?assert(has(H, <<"<a href=\"#\" role=\"button\">&lt;x&gt;</a>">>)),
    ?assert(has(H, <<"aria-label=\"a&quot;b\"">>)).

%%%-------------------------------------------------------------------
%%% catalog
%%%-------------------------------------------------------------------

catalog_test() ->
    [C] = ?M:catalog(),
    ?assertMatch(#{name := colorpicker, behavior := <<"colorpicker">>}, C),
    [?assert(erlang:function_exported(?M, N, 3)) || #{name := N} <- [C]],
    %% every option and flag is documented
    [?assertEqual([], (Opts ++ Flags) -- maps:keys(Docs))
     || #{options := Opts, flags := Flags, option_docs := Docs} <- [C]],
    [?assertNotEqual([], Ms) || #{methods := Ms} <- [C]].

%%%-------------------------------------------------------------------
%%% element records (designs/05-records.md)
%%%-------------------------------------------------------------------

-spec action(atom(), term(), map(), term()) -> ok.
action(_, _, _, _) -> ok.

record_equals_builder_test() ->
    ?assertEqual(r(?M:colorpicker(<<"#22c55e80">>, [alpha, clearable, no_preview],
                                  [{name, overlay}, {id, c1},
                                   {swatches, [<<"#fff">>, {1, 2, 3}]},
                                   {width, 200}, {height, <<"8rem">>},
                                   {clear_label, <<"None">>}])),
                 r(#ah_colorpicker{value = <<"#22c55e80">>, alpha = true, clearable = true,
                                   no_preview = true, name = overlay, id = c1,
                                   swatches = [<<"#fff">>, {1, 2, 3}], width = 200,
                                   height = <<"8rem">>, clear_label = <<"None">>})),
    ?assertEqual(r(?M:colorpicker(undefined, [inline, no_inputs, disabled],
                                  [{placeholder, <<"x">>}])),
                 r(#ah_colorpicker{inline = true, no_inputs = true, disabled = true,
                                   placeholder = <<"x">>})).

builder_fills_fields_test() ->
    C = ?M:colorpicker(<<"#abc">>, [alpha], [{swatches, [<<"#000">>]}, {id, c}]),
    ?assertMatch(#ah_colorpicker{value = <<"#abc">>, alpha = true, id = c,
                                 swatches = [<<"#000">>], placeholder = <<"No color">>,
                                 clear_label = <<"Clear">>, attrs = []}, C),
    ?assertError({aihtml, {record_only_field, ah_colorpicker, postback}},
                 ?M:colorpicker(undefined, [], [{postback, pick}])).

postback_test() ->
    Token = fun(Html) ->
                    {match, [Tk]} = re:run(r(Html), <<"data-ah-on=\"([a-z]+:[^\"]+)\"">>,
                                           [{capture, all_but_first, binary}]),
                    [Ev, Tok] = binary:split(Tk, <<":">>),
                    {ok, Ref} = aihtml_action:unsign(Tok),
                    {Ev, Ref}
            end,
    ?assertEqual({<<"change">>, {other_mod, pick_color, #{}}},
                 Token(#ah_colorpicker{postback = pick_color, delegate = other_mod})).

field_validation_test() ->
    ?assertError({aihtml, {bad_flag, colorpicker, alpha, 1}},
                 r(#ah_colorpicker{alpha = 1})),
    ?assertError({aihtml, {modifier_in_css, colorpicker, inline}},
                 r(#ah_colorpicker{css = [inline]})),
    %% an alpha value needs the alpha flag
    ?assertError({aihtml, {bad_value, colorpicker, <<"#22c55e80">>}},
                 r(#ah_colorpicker{value = <<"#22c55e80">>})),
    ?assertError({aihtml, {bad_value, colorpicker, <<"x">>}},
                 r(#ah_colorpicker{swatches = [<<"x">>]})).

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

default(ah_colorpicker) -> #ah_colorpicker{}.
