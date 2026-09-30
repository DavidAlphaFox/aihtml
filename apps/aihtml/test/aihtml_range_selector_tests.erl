% Tests for aihtml_range_selector.
-module(aihtml_range_selector_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_range_selector.hrl").

-define(M, aihtml_range_selector).

r(Html) -> aihtml_html:render_binary(Html).

has(Needle, Hay) ->
    case binary:match(Hay, Needle) of
        nomatch -> ?debugFmt("~s not in~n~s", [Needle, Hay]), false;
        _ -> true
    end.
%%%===================================================================
%%% range_selector
%%%===================================================================

range_selector_test() ->
    H = r(?M:ah_range_selector({0, 100}, {10, 50}, [<<"mt-2">>],
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
                  {match, [X]} = re:run(r(?M:ah_range_selector(Range, V, [], [])),
                                        <<"data-ah-value=\"([^\"]*)\"">>,
                                        [{capture, all_but_first, binary}]),
                  X
          end,
    ?assertEqual(<<"0,200">>, Val({0, 200}, undefined)),
    ?assertEqual(<<"10,40">>, Val({0, 200, 10}, {42, 13})),       % ordered and snapped
    ?assertEqual(<<"0,200">>, Val({0, 200}, {-5, 900})),           % clamped
    ?assertEqual(<<"2.5,7.5">>, Val({0, 10, 0.1}, {2.5, 7.5})),
    ?assertEqual(<<"-800,-300">>, Val({-1000, -100, 10}, {-800, -300})),
    ?assertError({aihtml, {bad_range, {5, 1, 1}}}, r(?M:ah_range_selector({5, 1}, undefined, [], []))),
    ?assertError({aihtml, {bad_range_value, 7}}, r(?M:ah_range_selector({0, 10}, 7, [], []))),
    ?assertError({aihtml, {bad_option, min_span, 50}},
                 r(?M:ah_range_selector({0, 10}, undefined, [], [{min_span, 50}]))),
    ?assertError({aihtml, {bad_option, labels_format, fancy}},
                 r(?M:ah_range_selector({0, 10}, undefined, [], [{labels_format, fancy}]))).

range_ticks_test() ->
    H = r(?M:ah_range_selector({0, 10, 0.5}, {2.5, 7.5}, [],
                               [{major_ticks, 2.5}, {minor_ticks, 0.5}, {show_minor_ticks, true},
                                {show_labels, false}, {show_markers, false}])),
    ?assertEqual(5, count(<<"tick-major">>, H)),
    ?assertEqual(21, count(<<"tick-minor">>, H)),
    ?assertEqual(0, count(<<"ah-range-selector-label\"">>, H)),
    ?assertEqual(2, count(<<";display:none\"">>, H)),
    T = r(?M:ah_range_selector({0, 100}, undefined, [disabled],
                               [{tick_values, [0, 33, 100]}, {show_major_ticks, false}])),
    ?assertEqual(0, count(<<"tick-major">>, T)),
    ?assertEqual(3, count(<<"ah-range-selector-label\"">>, T)),
    ?assert(has(<<"left:33.0%\">33<">>, T)),
    ?assert(has(<<"ah-range-selector ah-range-selector-disabled">>, T)),
    ?assert(has(<<"tabindex=\"-1\"">>, T)).

count(Needle, Hay) -> length(binary:matches(Hay, Needle)).

range_formats_test() ->
    F = fun(V, Fmt) ->
                H = r(?M:ah_range_selector({V, V + 1}, undefined, [],
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
    H = r(?M:ah_range_selector({0, 1}, undefined, [],
                               [{labels_format, currency}, {markers_format, {<<"<">>, {fixed, 2}, <<>>}}])),
    ?assert(has(<<"data-ah-format=\"{&quot;f&quot;:&quot;fixed&quot;,&quot;n&quot;:2,"
                  "&quot;p&quot;:&quot;&lt;&quot;,&quot;s&quot;:&quot;&quot;}\"">>, H)).

%%%===================================================================
%%% catalog
%%%===================================================================

catalog_test() ->
    Names = [N || #{name := N} <- ?M:catalog()],
    ?assertEqual([range_selector], Names),
    [begin
         Arity = length(binary:matches(maps:get(signature, E), <<",">>)) + 1,
         ?assert(erlang:function_exported(?M, aihtml_catalog:builder(N), Arity)),
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
    ?assertEqual(r(?M:ah_range_selector({0, 50, 5}, {5, 25}, [],
                                        [{major_ticks, 5}, {min_span, 5}, {labels_format, currency}])),
                 r(#ah_range_selector{range = {0, 50, 5}, value = {5, 25}, major_ticks = 5,
                                      min_span = 5, labels_format = currency})).

builder_fills_fields_test() ->
    ?assertError({aihtml, {record_only_field, ah_range_selector, postback}},
                 ?M:ah_range_selector({0, 1}, undefined, [], [{postback, pick}])).

postback_test() ->
    Token = fun(Html) ->
                    {match, [T]} = re:run(r(Html), <<"data-ah-on=\"([a-z]+:[^\"]+)\"">>,
                                          [{capture, all_but_first, binary}]),
                    [Ev, Tok] = binary:split(T, <<":">>),
                    {ok, Ref} = aihtml_action:unsign(Tok),
                    {Ev, Ref}
            end,
    ?assertEqual({<<"change">>, {other, picked, #{}}},
                 Token(#ah_range_selector{postback = picked, delegate = other})).

field_validation_test() ->
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

default(ah_range_selector) -> #ah_range_selector{}.
