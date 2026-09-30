-module(aihtml_slider_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_slider.hrl").

-define(M, aihtml_slider).

r(Html) -> aihtml_html:render_binary(Html).

has(Bin, Sub) -> binary:match(Bin, Sub) =/= nomatch.

token(Html) ->
    {match, [T]} = re:run(aihtml_html:render_binary(Html), <<"data-ah-on=\"([a-z]+:[^\"]+)\"">>,
                          [{capture, all_but_first, binary}]),
    [Ev, Tok] = binary:split(T, <<":">>),
    {ok, Ref} = aihtml_action:unsign(Tok),
    {Ev, Ref}.

slider_single_test() ->
    H = r(?M:ah_slider({0, 100}, 25, [], [{name, v}])),
    ?assert(has(H, <<"class=\"ah-slider ah-slider-horizontal ah-slider-buttons-hidden ah-slider-ticks-hidden\"">>)),
    ?assert(has(H, <<"role=\"slider\" tabindex=\"0\" aria-valuenow=\"25\"">>)),
    ?assert(has(H, <<"data-ah-value=\"25\"">>)),
    ?assert(has(H, <<"data-ah-step=\"1\"">>)),
    ?assert(has(H, <<"left:calc((100% - 18px) * 0.25)">>)),
    ?assert(has(H, <<"<input type=\"hidden\" name=\"v\" value=\"25\">">>)).

slider_clamps_test() ->
    ?assert(has(r(?M:ah_slider({0, 10}, 42, [], [])), <<"data-ah-value=\"10\"">>)),
    ?assert(has(r(?M:ah_slider({0, 10}, undefined, [], [])), <<"data-ah-value=\"0\"">>)).

slider_range_test() ->
    H = r(?M:ah_slider({0, 100, 5}, {80, 20}, [success], [{min_range, 10}])),
    ?assert(has(H, <<"data-ah-value=\"20,80\"">>)),
    ?assert(has(H, <<"role=\"group\"">>)),
    ?assert(has(H, <<"ah-slider-range-slider">>)),
    ?assert(has(H, <<"ah-slider-thumb-start\" style=\"left:calc((100% - 18px) * 0.2)\" role=\"slider\"">>)),
    ?assert(has(H, <<"data-ah-min-range=\"10\"">>)),
    ?assert(has(H, <<"ah-slider-success">>)).

slider_ticks_buttons_vertical_test() ->
    H = r(?M:ah_slider({0, 1, 0.1}, 0.5, [vertical, buttons, tooltip],
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
    ?assertError({aihtml, {bad_slider_range, {5, 1, 1}}}, ?M:ah_slider({5, 1}, 2, [], [])).

%%% element records (designs/05-records.md)

record_equals_builder_test() ->
    ?assertEqual(r(?M:ah_slider({0, 10, 2}, {2, 8}, [vertical, buttons],
                                [{ticks, 2}, {min_range, 2}, {name, r}])),
                 r(#ah_slider{range = {0, 10, 2}, value = {2, 8}, orientation = vertical,
                              buttons = true, ticks = 2, min_range = 2, name = r})),
    ?assertEqual(r(?M:ah_slider({0, 10}, 3, [], [])), r(#ah_slider{range = {0, 10}, value = 3})).

builder_fills_fields_test() ->
    ?assertMatch(#ah_slider{range = {0, 5, 1}, value = 2, template = info, tooltip = true,
                            ticks = 1, ticks_position = both},
                 ?M:ah_slider({0, 5}, 2, [info, tooltip], [{ticks, 1}, {ticks_position, both}])),
    ?assertError({aihtml, {record_only_field, ah_slider, postback}},
                 ?M:ah_slider({0, 1}, 0, [], [{postback, x}])).

postback_test() ->
    ?assertEqual({<<"change">>, {other_mod, vol, #{}}},
                 token(#ah_slider{value = 3, postback = vol, delegate = other_mod})).

field_validation_test() ->
    ?assertError({aihtml, {bad_modifier, slider, orientation, up, _}},
                 r(#ah_slider{orientation = up})),
    ?assertError({aihtml, {bad_slider_range, {5, 1, 1}}}, r(#ah_slider{range = {5, 1}})),
    ?assertError({aihtml, {bad_slider_range, {0, 1, 0}}}, r(#ah_slider{range = {0, 1, 0}})),
    ?assertError({aihtml, {bad_option, ticks_position, left}},
                 r(#ah_slider{ticks = 1, ticks_position = left})).

%%% catalog

catalog_test() ->
    Names = [N || #{name := N} <- ?M:catalog()],
    ?assertEqual([slider], Names),
    [begin
         ?assert(is_binary(maps:get(signature, E))),
         ?assert(erlang:function_exported(?M, aihtml_catalog:builder(N), 4))
     end || #{name := N} = E <- ?M:catalog()].

catalog_docs_test() ->
    [begin
         Docs = maps:get(option_docs, E),
         [?assert(maps:is_key(O, Docs)) || O <- maps:get(options, E, [])],
         ?assert(is_list(maps:get(methods, E)))
     end || E <- ?M:catalog()].

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

default(ah_slider) -> #ah_slider{}.
