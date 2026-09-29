%% Tests for aihtml_datepicker.
-module(aihtml_datepicker_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_datepicker.hrl").

-define(M, aihtml_datepicker).

r(Html) -> aihtml_html:render_binary(Html).

has(Needle, Hay) ->
    case binary:match(Hay, Needle) of
        nomatch -> ?debugFmt("~s not in~n~s", [Needle, Hay]), false;
        _ -> true
    end.

has_quiet(Needle, Hay) -> binary:match(Hay, Needle) =/= nomatch.

datepicker_single_test() ->
    H = r(?M:datepicker(<<"2026-09-29">>, [<<"w-64">>], [{id, dp}, {name, due}, {title, <<"t">>}])),
    ?assert(has(<<"class=\"ah-datepicker w-64\"">>, H)),
    ?assert(has(<<"id=\"dp\"">>, H)),
    ?assert(has(<<"data-ah=\"datepicker\"">>, H)),
    ?assert(has(<<"data-ah-value=\"2026-09-29\"">>, H)),
    ?assert(has(<<"<input type=\"hidden\" name=\"due\" value=\"2026-09-29\">">>, H)),
    ?assert(has(<<"value=\"2026-09-29\"">>, H)),
    ?assert(has(<<"id=\"dp-input\"">>, H)),
    ?assert(has(<<"role=\"combobox\"">>, H)),
    ?assert(has(<<"aria-haspopup=\"dialog\"">>, H)),
    ?assert(has(<<"class=\"ah-datepicker-popup\"">>, H)),
    ?assert(has(<<"placeholder=\"Select date...\"">>, H)),
    ?assert(has(<<"title=\"t\"">>, H)),
    %% name is not written on the root
    ?assertEqual(1, length(binary:matches(H, <<"name=">>))).

datepicker_value_forms_test() ->
    H = r(?M:datepicker({2026, 1, 5}, [], [{format, <<"d MMM yy">>}])),
    ?assert(has(<<"data-ah-value=\"2026-01-05\"">>, H)),
    ?assert(has(<<"value=\"5 Jan 26\"">>, H)),
    E = r(?M:datepicker(undefined, [], [])),
    ?assert(has(<<"data-ah-value=\"\"">>, E)),
    ?assert(has(<<"id=\"ah-p">>, E)),          % an id is generated
    ?assertError({aihtml, {bad_date, <<"2026-02-30">>}},
                 r(?M:datepicker(<<"2026-02-30">>, [], []))),
    ?assertError({aihtml, {bad_date, _}}, r(?M:datepicker(<<"29/09/2026">>, [], []))).

datepicker_range_test() ->
    H = r(?M:datepicker({<<"2026-09-18">>, {2026, 9, 10}}, [], [{name, p}])),
    ?assert(has(<<"ah-datepicker-range">>, H)),
    ?assert(has(<<"data-ah-range">>, H)),
    %% sorted
    ?assert(has(<<"data-ah-value=\"2026-09-10,2026-09-18\"">>, H)),
    ?assert(has(<<"value=\"2026-09-10 - 2026-09-18\"">>, H)),
    E = r(?M:datepicker(undefined, [range], [])),
    ?assert(has(<<"class=\"ah-datepicker ah-datepicker-range\"">>, E)),
    ?assert(has(<<"data-ah-value=\"\"">>, E)).

datepicker_options_test() ->
    H = r(?M:datepicker(undefined, [disabled, clearable],
                        [{min, {2026, 1, 1}}, {max, <<"2026-12-31">>},
                         {disabled_dates, [<<"2026-05-01">>, {2026, 10, 1}]},
                         {first_day, 1}, {week_numbers, true}, {other_month_days, false},
                         {weekends, true}, {placeholder, <<"Due">>},
                         {labels, #{today => <<"今天"/utf8>>}}])),
    ?assert(has(<<"ah-datepicker-disabled">>, H)),
    ?assert(has(<<"ah-datepicker-clearable">>, H)),
    ?assert(has(<<"data-ah-min=\"2026-01-01\"">>, H)),
    ?assert(has(<<"data-ah-max=\"2026-12-31\"">>, H)),
    ?assert(has(<<"data-ah-disabled-dates=\"2026-05-01,2026-10-01\"">>, H)),
    ?assert(has(<<"data-ah-first-day=\"1\"">>, H)),
    ?assert(has(<<"data-ah-week-numbers">>, H)),
    ?assert(has(<<"data-ah-weekends">>, H)),
    ?assert(has(<<"data-ah-other-month-days=\"false\"">>, H)),
    ?assert(has(<<"placeholder=\"Due\"">>, H)),
    ?assert(has(<<" disabled">>, H)),
    ?assert(has(<<"aria-disabled=\"true\"">>, H)),
    %% no clear button when disabled
    ?assertNot(has_quiet(<<"ah-datepicker-clear\"">>, H)),
    %% labels travel as JSON, escaped in the attribute
    ?assert(has(<<"&quot;today&quot;:&quot;"/utf8>>, H)),
    ?assertError({aihtml, {bad_first_day, 7}}, r(?M:datepicker(undefined, [], [{first_day, 7}]))),
    ?assertError({aihtml, {bad_datepicker_label, months}},
                 r(?M:datepicker(undefined, [], [{labels, #{months => [<<"x">>]}}]))),
    ?assertError({aihtml, {unknown_modifier, datepicker, big, _}},
                 ?M:datepicker(undefined, [big], [])).

datepicker_labels_format_test() ->
    Months = [<<"M", (integer_to_binary(N))/binary>> || N <- lists:seq(1, 12)],
    H = r(?M:datepicker(<<"2026-03-07">>, [], [{format, <<"MMMM/dd">>},
                                                {labels, #{months => Months}}])),
    ?assert(has(<<"value=\"M3/07\"">>, H)).

datepicker_clearable_test() ->
    H = r(?M:datepicker(<<"2026-03-07">>, [clearable], [])),
    ?assert(has(<<"class=\"ah-datepicker-clear\"">>, H)),
    ?assert(has(<<"aria-label=\"Clear\"">>, H)).

datepicker_inline_test() ->
    H = r(?M:datepicker(<<"2026-09-15">>, [inline], [{id, di}, {first_day, 1}, {week_numbers, true},
                                                      {min, <<"2026-09-05">>},
                                                      {disabled_dates, [<<"2026-09-21">>]}])),
    ?assert(has(<<"ah-datepicker-inline">>, H)),
    ?assert(has(<<"role=\"group\"">>, H)),
    ?assert(has(<<"<div class=\"ah-datepicker-title\" id=\"di-title\" aria-live=\"polite\">September 2026</div>">>, H)),
    %% Monday first: the grid starts on Monday 31 August, week 36
    ?assert(has(<<"<div class=\"ah-datepicker-weekday\">Mo</div><div class=\"ah-datepicker-weekday\">Tu</div>">>, H)),
    ?assert(has(<<"<div class=\"ah-datepicker-week-num\">36</div><div class=\"ah-datepicker-day ah-datepicker-day-other-month ah-datepicker-day-disabled\" role=\"gridcell\" id=\"di-d2026-08-31\"">>, H)),
    ?assert(has(<<"<div class=\"ah-datepicker-week-num\">40</div>">>, H)),
    ?assertNot(has_quiet(<<"-d2026-10-05">>, H)),       % five rows
    ?assert(has(<<"ah-datepicker-day ah-datepicker-day-selected ah-datepicker-day-focused\" role=\"gridcell\" id=\"di-d2026-09-15\" data-date=\"2026-09-15\" aria-selected=\"true\" aria-disabled=\"false\" aria-label=\"15 September 2026\"">>, H)),
    ?assert(has(<<"id=\"di-d2026-09-21\" data-date=\"2026-09-21\" aria-selected=\"false\" aria-disabled=\"true\"">>, H)),
    %% the non-inline popup starts empty
    ?assert(has(<<"<div class=\"ah-datepicker-popup\" role=\"dialog\" aria-label=\"Choose date\"></div>">>,
                r(?M:datepicker(<<"2026-09-15">>, [], [])))).

datepicker_inline_range_week_numbers_test() ->
    %% Sunday first, a range across the turn of the year: week 1 of 2027
    H = r(?M:datepicker({<<"2026-12-30">>, <<"2027-01-02">>}, [inline],
                        [{id, dr}, {week_numbers, true}, {other_month_days, false}])),
    ?assert(has(<<"December 2026">>, H)),
    ?assert(has(<<"ah-datepicker-day-in-range ah-datepicker-day-range-start">>, H)),
    ?assert(has(<<"<div class=\"ah-datepicker-week-num\">1</div>">>, H)),
    ?assert(has(<<"ah-datepicker-day-empty">>, H)).

%%%===================================================================
%%% Catalog
%%%===================================================================

catalog_test() ->
    [#{name := datepicker}] = ?M:catalog(),
    E = aihtml_catalog:entry(?M, datepicker),
    #{flags := Flags} = E,
    [_ | _] = aihtml_catalog:classes(E, Flags),
    %% every option and flag is documented, every behaviour method listed
    #{flags := Fl, options := Op, option_docs := Docs, methods := Ms} = E,
    ?assertEqual(lists:sort(Fl ++ Op), lists:sort(maps:keys(Docs))),
    ?assert(lists:member(setValue, [Name || #{name := Name} <- Ms])).

%%%===================================================================
%%% element records (designs/05-records.md)
%%%===================================================================

record_equals_builder_test() ->
    Labels = #{today => <<"Now">>},
    ?assertEqual(r(?M:datepicker(<<"2026-09-15">>, [inline, clearable, <<"w-64">>],
                                 [{id, dp}, {name, due}, {first_day, 1}, {min, {2026, 9, 5}},
                                  {week_numbers, true}, {labels, Labels}, {title, <<"t">>}])),
                 r(#ah_datepicker{value = <<"2026-09-15">>, inline = true, clearable = true,
                                  css = [<<"w-64">>], id = dp, name = due, first_day = 1,
                                  min = {2026, 9, 5}, week_numbers = true, labels = Labels,
                                  attrs = [{title, <<"t">>}]})).

builder_fills_fields_test() ->
    D = ?M:datepicker({<<"2026-01-01">>, undefined}, [range, disabled, <<"x">>],
                      [{id, d}, {format, <<"d MMM">>}, {max, <<"2026-12-31">>},
                       {disabled_dates, [{2026, 5, 1}]}, {other_month_days, false},
                       {title, <<"t">>}]),
    ?assertMatch(#ah_datepicker{value = {<<"2026-01-01">>, undefined}, range = true,
                                disabled = true, readonly = false, id = d,
                                format = <<"d MMM">>, max = <<"2026-12-31">>,
                                disabled_dates = [{2026, 5, 1}], other_month_days = false,
                                css = [<<"x">>], attrs = [{title, <<"t">>}]}, D).

generated_id_test() ->
    %% each render gets its own id
    R = #ah_datepicker{},
    ?assertNotEqual(r(R), r(R)).

postback_test() ->
    Token = fun(Html) ->
                    {match, [T]} = re:run(r(Html), <<"data-ah-on=\"([a-z]+:[^\"]+)\"">>,
                                          [{capture, all_but_first, binary}]),
                    [Ev, Tok] = binary:split(T, <<":">>),
                    {ok, Ref} = aihtml_action:unsign(Tok),
                    {Ev, Ref}
            end,
    ?assertEqual({<<"change">>, {?MODULE, picked, #{id => 7}}},
                 Token(#ah_datepicker{postback = {picked, #{id => 7}}})).

field_validation_test() ->
    ?assertError({aihtml, {bad_first_day, 9}}, r(#ah_datepicker{first_day = 9})),
    ?assertError({aihtml, {bad_date, <<"soon">>}}, r(#ah_datepicker{min = <<"soon">>})),
    ?assertError({aihtml, {bad_datepicker_label, today_}},
                 r(#ah_datepicker{labels = #{today_ => <<"x">>}})),
    ?assertError({aihtml, {bad_flag, datepicker, inline, yes}},
                 r(#ah_datepicker{inline = yes})).

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

default(ah_datepicker) -> #ah_datepicker{}.
