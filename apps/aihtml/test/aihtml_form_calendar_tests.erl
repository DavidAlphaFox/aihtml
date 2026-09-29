%% Tests for aihtml_form_calendar.
-module(aihtml_form_calendar_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_form_calendar.hrl").

-define(M, aihtml_form_calendar).

r(Html) -> aihtml_html:render_binary(Html).

has(Needle, Hay) ->
    case binary:match(Hay, Needle) of
        nomatch -> ?debugFmt("~s not in~n~s", [Needle, Hay]), false;
        _ -> true
    end.

count(Needle, Hay) -> length(binary:matches(Hay, Needle)).

events() ->
    [#{id => a, title => <<"Standup">>, start => <<"2030-03-04T09:00">>,
       'end' => <<"2030-03-04T09:15">>, rrule => <<"FREQ=WEEKLY;BYDAY=MO,WE,FR;COUNT=6">>},
     #{id => b, title => <<"Trip">>, start => <<"2030-03-08">>, 'end' => <<"2030-03-13">>,
       color => <<"#e11d48">>},
     #{id => c, title => <<"Lunch">>, start => {{2030, 3, 11}, {12, 0, 0}}, status => confirmed},
     #{id => d, title => <<"Review">>, start => <<"2030-03-11T11:30">>,
       'end' => <<"2030-03-11T13:00">>, status => tentative},
     #{id => e, title => <<"Party">>, start => <<"2030-03-11">>}].

%%%===================================================================
%%% calendar
%%%===================================================================

calendar_root_test() ->
    H = r(?M:calendar(<<"2030-03-11">>, [<<"shadow">>],
                      [{id, cal}, {name, day}, {events, events()}, {title, <<"t">>}])),
    ?assert(has(<<"<div class=\"ah-calendar shadow\" id=\"cal\" data-ah=\"calendar\" "
                  "data-ah-value=\"2030-03-11\" data-ah-view=\"month\"">>, H)),
    ?assert(has(<<"data-view=\"month\" data-start=\"2030-02-24\" data-end=\"2030-04-07\"">>, H)),
    ?assert(has(<<"style=\"height:600px\"">>, H)),
    ?assert(has(<<"<input type=\"hidden\" name=\"day\" value=\"2030-03-11\">">>, H)),
    ?assert(has(<<"title=\"t\"">>, H)),
    ?assert(has(<<"<h2 class=\"ah-calendar-title\" id=\"cal-title\" aria-live=\"polite\">"
                  "March 2030</h2>">>, H)),
    ?assert(has(<<"<button class=\"ah-calendar-view-btn ah-calendar-view-btn-active\" "
                  "type=\"button\" data-view=\"month\" aria-pressed=\"true\">Month</button>">>, H)),
    ?assertEqual(4, count(<<"ah-calendar-view-btn\"">>, H) + 1),
    %% no labels attribute unless customised; events travel as JSON
    ?assertNot(has_quiet(<<"data-ah-labels">>, H)),
    ?assert(has(<<"&quot;rrule&quot;:&quot;FREQ=WEEKLY;BYDAY=MO,WE,FR;COUNT=6&quot;">>, H)),
    ?assertEqual(1, count(<<"name=">>, H)).

has_quiet(Needle, Hay) -> binary:match(Hay, Needle) =/= nomatch.

calendar_month_view_test() ->
    H = r(?M:calendar(<<"2030-03-11">>, [], [{id, cal}, {events, events()}, {day_max_events, 2}])),
    %% 6 weeks: Feb 24 .. Apr 5
    ?assertEqual(6, count(<<"class=\"ah-calendar-week-row\"">>, H)),
    ?assert(has(<<"<div class=\"ah-calendar-day ah-calendar-day-other\" data-date=\"2030-02-24\">">>, H)),
    %% the weekly series: Mon, Wed, Fri from Mar 4, six times
    %% (Mar 11's is in the hidden third row)
    ?assertEqual(5, count(<<"data-eventid=\"a_2030">>, H)),
    ?assert(has(<<"data-eventid=\"a_20300304T090000\"">>, H)),
    ?assert(has(<<"data-eventid=\"a_20300315T090000\"">>, H)),
    ?assertNot(has_quiet(<<"a_20300318T090000">>, H)),
    %% the trip spans Fri Mar 8 .. Tue Mar 12 across two week rows
    ?assert(has(<<"ah-calendar-daygrid-event-multi ah-calendar-daygrid-event-end\" "
                  "data-eventid=\"b\" style=\"grid-column:6/8;grid-row:2;background:#e11d48;\"">>, H)),
    ?assert(has(<<"ah-calendar-daygrid-event-multi ah-calendar-daygrid-event-start\" "
                  "data-eventid=\"b\" style=\"grid-column:1/4;grid-row:2;">>, H)),
    %% timed events show their time; Mar 11 has 5 bars, 2 rows shown, "+3 more"
    ?assert(has(<<"<span class=\"ah-calendar-event-time\">9:00 AM</span>">>, H)),
    ?assert(has(<<"<div class=\"ah-calendar-day-more\" data-date=\"2030-03-11\" "
                  "style=\"grid-column:2;grid-row:4;\" role=\"button\" tabindex=\"0\">+3 more</div>">>,
                H)),
    ?assert(has(<<"<div class=\"ah-calendar-daygrid-header-cell\">Sun</div>">>, H)).

calendar_week_view_test() ->
    H = r(?M:calendar(<<"2030-03-11">>, [editable],
                      [{id, cal}, {events, events()}, {view, week}, {first_day, 1}])),
    ?assert(has(<<"data-start=\"2030-03-11\" data-end=\"2030-03-18\"">>, H)),
    ?assert(has(<<"Mar 11 – Mar 17, 2030"/utf8>>, H)),
    ?assert(has(<<"<span class=\"ah-calendar-timegrid-header-day\">Mon</span>">>, H)),
    %% all day events in the all-day row
    ?assert(has(<<"<div class=\"ah-calendar-event ah-calendar-allday-event\" data-eventid=\"e\"">>, H)),
    %% Lunch 12:00-13:00 and Review 11:30-13:00 overlap: two columns
    ?assert(has(<<"data-eventid=\"d\" style=\"position:absolute;top:460px;height:60px;"
                  "left:calc(100% * 0 / 2);width:calc(100% / 2);">>, H)),
    ?assert(has(<<"data-eventid=\"c\" style=\"position:absolute;top:480px;height:40px;"
                  "left:calc(100% * 1 / 2);width:calc(100% / 2);">>, H)),
    ?assert(has(<<"<div class=\"ah-calendar-event-time\">11:30 AM – 1:00 PM</div>"/utf8>>, H)),
    %% editable: resize handles
    ?assert(has(<<"ah-calendar-timegrid-resize-handle">>, H)),
    ?assert(has(<<"<div class=\"ah-calendar-timegrid-slot\" style=\"height:40px;\">"
                  "<div class=\"ah-calendar-timegrid-slot-label\">12 AM</div>">>, H)),
    ?assert(has(<<"style=\"position:relative;height:960px;\"">>, H)).

calendar_day_24h_test() ->
    H = r(?M:calendar({2030, 3, 11}, [],
                      [{events, events()}, {view, day}, {hour_format, 24}, {slot_duration, 60},
                       {slot_height, 30}, {height, undefined}])),
    ?assert(has(<<"Monday, March 11, 2030">>, H)),
    ?assert(has(<<"<div class=\"ah-calendar-timegrid-slot-label\">13:00</div>">>, H)),
    ?assert(has(<<"11:30 – 13:00"/utf8>>, H)),
    ?assertNot(has_quiet(<<"resize-handle">>, H)),
    ?assertNot(has_quiet(<<"style=\"height:">>, binary:part(H, 0, 900))).

calendar_list_view_test() ->
    H = r(?M:calendar(<<"2030-03-08">>, [], [{events, events()}, {view, list}, {agenda_days, 7}])),
    ?assert(has(<<"<span class=\"ah-calendar-list-day-name\">Friday</span>"
                  "<span class=\"ah-calendar-list-day-date\">March 8, 2030</span>">>, H)),
    ?assert(has(<<"<div class=\"ah-calendar-list-event-time\">All day</div>">>, H)),
    ?assert(has(<<"background:var(--ah-color-success);">>, H)),
    ?assert(has(<<"background:var(--ah-color-warning);">>, H)),
    ?assert(has(<<"<span class=\"ah-calendar-event-recurring-icon\">↻ </span>Standup"/utf8>>, H)),
    E = r(?M:calendar(<<"2031-01-01">>, [], [{events, events()}, {view, list}])),
    ?assert(has(<<"No events in this period">>, E)).

calendar_labels_test() ->
    H = r(?M:calendar(<<"2030-03-11">>, [],
                      [{labels, #{today => <<"今天"/utf8>>, title_month => <<"yyyy年M月"/utf8>>,
                                  more => <<"还有{n}项"/utf8>>}},
                       {views, [month, list]}])),
    ?assert(has(<<">今天</button>"/utf8>>, H)),
    ?assert(has(<<">2030年3月</h2>"/utf8>>, H)),
    ?assert(has(<<"data-ah-labels=">>, H)),
    ?assertEqual(2, count(<<"data-view=\"">>, H) - 1).

%% The occurrence days of one recurring event, from the agenda view.
occurrences(Event, From, Days) ->
    H = r(?M:calendar(From, [], [{events, [Event#{id => r}]}, {view, list},
                                 {agenda_days, Days}])),
    {match, Ms} = re:run(H, <<"data-eventid=\"r_([0-9]{8})T">>,
                         [global, {capture, all_but_first, binary}]),
    [binary_to_integer(D) || [D] <- Ms].

recurrence_test() ->
    %% monthly on the 31st clamps and keeps the clamped day (date-fns addMonths)
    ?assertEqual([20300131, 20300228, 20300328],
                 occurrences(#{start => <<"2030-01-31T10:00">>, rrule => <<"FREQ=MONTHLY;COUNT=3">>},
                             <<"2030-01-01">>, 365)),
    %% weekly by day, every other week, until, exception
    ?assertEqual([20300304, 20300308, 20300322],
                 occurrences(#{start => <<"2030-03-04T10:00">>, exdates => [<<"2030-03-18">>],
                               rrule => <<"RRULE:FREQ=WEEKLY;INTERVAL=2;BYDAY=MO,FR;UNTIL=20300325">>},
                             <<"2030-03-01">>, 60)),
    %% COUNT counts occurrences before the range too
    ?assertEqual([20300105], occurrences(#{start => <<"2030-01-01T08:00">>,
                                           rrule => <<"FREQ=DAILY;COUNT=5">>},
                                         <<"2030-01-05">>, 30)),
    ?assertEqual([20300615, 20310615],
                 occurrences(#{start => <<"2030-06-15T08:00">>, rrule => <<"FREQ=YEARLY;BYMONTH=6">>},
                             <<"2030-01-01">>, 730)).

set_events_test() ->
    Ops = aihtml_action:render_ops(
            fun(Ctx) ->
                    ?M:set_events(Ctx, #{id => <<"cal">>}, [#{title => <<"x">>,
                                                               start => {2030, 1, 2}}]),
                    ?M:add_event(Ctx, {id, <<"cal">>}, #{id => 7, start => <<"2030-01-02T10:00">>})
            end),
    ?assertMatch([#{op := call, id := <<"cal">>, method := <<"setEvents">>,
                    args := [[#{<<"id">> := <<"ev1">>, <<"start">> := <<"2030-01-02">>,
                                <<"end">> := <<"2030-01-03">>, <<"allDay">> := true}]]},
                  #{op := call, id := <<"cal">>, method := <<"addEvent">>,
                    args := [#{<<"id">> := <<"7">>, <<"end">> := <<"2030-01-02T11:00">>,
                               <<"allDay">> := false}]}], Ops).

%%%===================================================================
%%% datetime_input
%%%===================================================================

datetime_input_date_test() ->
    H = r(?M:datetime_input(<<"2026-09-29">>, [<<"w-48">>], [{id, d}, {name, due}, {title, <<"t">>}])),
    ?assert(has(<<"<div class=\"ah-dti-group w-48\" id=\"d\" data-ah=\"datetime_input\" "
                  "data-ah-value=\"2026-09-29\" data-ah-format=\"yyyy-MM-dd\"">>, H)),
    ?assert(has(<<"<input class=\"ah-dti-input\" type=\"text\" id=\"d-input\" readonly">>, H)),
    ?assert(has(<<"value=\"2026-09-29\"">>, H)),
    ?assert(has(<<"aria-controls=\"d-dropdown\"">>, H)),
    ?assert(has(<<"<div class=\"ah-dti-cal-btn\" data-action=\"toggle-dropdown\" "
                  "aria-hidden=\"true\">📅</div>"/utf8>>, H)),
    ?assert(has(<<"<input type=\"hidden\" name=\"due\" value=\"2026-09-29\">">>, H)),
    ?assert(has(<<"<div class=\"ah-dti-dropdown\" id=\"d-dropdown\" role=\"dialog\" "
                  "aria-label=\"Choose date\" hidden></div>">>, H)),
    ?assert(has(<<"<span class=\"ah-dti-live\" aria-live=\"polite\" aria-atomic=\"true\">">>, H)),
    ?assert(has(<<"title=\"t\"">>, H)),
    ?assertEqual(1, count(<<"name=">>, H)).

datetime_input_formats_test() ->
    V = fun(Value, Format) ->
                H = r(?M:datetime_input(Value, [], [{format, Format}])),
                {match, [Iso, Shown]} =
                    re:run(H, <<"data-ah-value=\"([^\"]*)\".* value=\"([^\"]*)\"">>,
                           [{capture, all_but_first, binary}]),
                {Iso, Shown}
        end,
    ?assertEqual({<<"2026-09-29T14:05">>, <<"2026-09-29 02:05 PM">>},
                 V(<<"2026-09-29T14:05:09">>, <<"yyyy-MM-dd hh:mm a">>)),
    ?assertEqual({<<"2026-09-29T09:05:30">>, <<"29/09/26 09:05:30">>},
                 V({{2026, 9, 29}, {9, 5, 30}}, <<"d/M/yy H:m:s">>)),
    ?assertEqual({<<"00:30">>, <<"12:30 AM">>}, V(<<"00:30">>, <<"hh:mm a">>)),
    ?assertEqual({<<"2026-09-01">>, <<"2026年09月01日"/utf8>>},
                 V({2026, 9, 1}, <<"yyyy年MM月dd日"/utf8>>)),
    ?assertEqual({<<>>, <<>>}, V(undefined, <<"yyyy-MM-dd">>)),
    %% a time alone has no calendar
    T = r(?M:datetime_input(<<"10:00">>, [], [{format, <<"HH:mm">>}])),
    ?assertNot(has_quiet(<<"ah-dti-cal-btn">>, T)),
    ?assertNot(has_quiet(<<"ah-dti-dropdown">>, T)).

datetime_input_flags_test() ->
    H = r(?M:datetime_input(undefined, [disabled, readonly, spinner, no_calendar, show_time,
                                        floating_label, no_rounded],
                            [{placeholder, <<"Birthday">>}, {min, {2026, 1, 1}},
                             {max, <<"2026-12-31T08:00">>}])),
    ?assert(has(<<"class=\"ah-dti-group ah-dti-disabled ah-dti-no-rounded ah-dti-readonly\"">>, H)),
    ?assert(has(<<"data-ah-min=\"2026-01-01\" data-ah-max=\"2026-12-31\"">>, H)),
    ?assert(has(<<"data-ah-show-time">>, H)),
    ?assert(has(<<"<div class=\"ah-dti-spinner\" aria-hidden=\"true\">">>, H)),
    ?assert(has(<<"aria-label=\"Increment\"">>, H)),
    ?assert(has(<<"<label class=\"ah-dti-label\" for=\"">>, H)),
    ?assert(has(<<">Birthday</label>">>, H)),
    ?assertNot(has_quiet(<<"placeholder=">>, H)),
    ?assert(has(<<" disabled">>, H)),
    ?assertNot(has_quiet(<<"ah-dti-cal-btn">>, H)).

%%%===================================================================
%%% Catalog
%%%===================================================================

catalog_test() ->
    [#{name := calendar}, #{name := datetime_input}] = ?M:catalog(),
    ?assertEqual([{set_events, 3}, {add_event, 3}], ?M:facade_extras()),
    [?assert(erlang:function_exported(?M, F, A)) || {F, A} <- ?M:facade_extras()],
    %% every option and flag is documented, every behaviour method listed
    [begin
         #{flags := Fl, options := Op, option_docs := Docs, methods := Ms} =
             aihtml_catalog:entry(?M, N),
         ?assertEqual(lists:sort(Fl ++ Op), lists:sort(maps:keys(Docs))),
         ?assert(lists:member(setValue, [Name || #{name := Name} <- Ms]))
     end || N <- [calendar, datetime_input]].

%%%===================================================================
%%% element records (designs/05-records.md)
%%%===================================================================

record_equals_builder_test() ->
    Evs = events(),
    Labels = #{today => <<"Now">>},
    ?assertEqual(r(?M:calendar(<<"2030-03-11">>, [editable, selectable, <<"h-96">>],
                               [{id, c}, {name, n}, {events, Evs}, {view, week},
                                {views, [week, day]}, {first_day, 1}, {agenda_days, 5},
                                {day_max_events, 4}, {slot_duration, 15}, {slot_height, 12},
                                {height, 400}, {hour_format, 24}, {labels, Labels},
                                {title, <<"t">>}])),
                 r(#ah_calendar{value = <<"2030-03-11">>, editable = true, selectable = true,
                                css = [<<"h-96">>], id = c, name = n, events = Evs, view = week,
                                views = [week, day], first_day = 1, agenda_days = 5,
                                day_max_events = 4, slot_duration = 15, slot_height = 12,
                                height = 400, hour_format = 24, labels = Labels,
                                attrs = [{title, <<"t">>}]})),
    ?assertEqual(r(?M:datetime_input(<<"2026-09-29T10:00">>, [spinner, show_time, <<"w-60">>],
                                     [{id, d}, {name, at}, {format, <<"yyyy-MM-dd HH:mm">>},
                                      {placeholder, <<"P">>}, {min, <<"2026-01-01T00:00">>},
                                      {first_day, 1}, {labels, #{time => <<"T">>}}])),
                 r(#ah_datetime_input{value = <<"2026-09-29T10:00">>, spinner = true,
                                      show_time = true, css = [<<"w-60">>], id = d, name = at,
                                      format = <<"yyyy-MM-dd HH:mm">>, placeholder = <<"P">>,
                                      min = <<"2026-01-01T00:00">>, first_day = 1,
                                      labels = #{time => <<"T">>}})).

builder_fills_fields_test() ->
    C = ?M:calendar({2030, 1, 1}, [selectable, <<"x">>],
                    [{view, list}, {hour_format, 24}, {height, undefined}, {title, <<"t">>}]),
    ?assertMatch(#ah_calendar{value = {2030, 1, 1}, selectable = true, editable = false,
                              view = list, hour_format = 24, height = 600, events = [],
                              css = [<<"x">>], attrs = [{title, <<"t">>}]}, C),
    D = ?M:datetime_input(undefined, [no_rounded], [{format, <<"HH:mm">>}, {max, <<"18:00">>}]),
    ?assertMatch(#ah_datetime_input{no_rounded = true, format = <<"HH:mm">>, max = <<"18:00">>,
                                    id = undefined, attrs = []}, D),
    ?assertError({aihtml, {record_only_field, ah_calendar, postback}},
                 ?M:calendar(undefined, [], [{postback, go}])).

generated_id_test() ->
    H = r(#ah_datetime_input{postback = changed}),
    {match, [Id]} = re:run(H, <<"^<div class=\"ah-dti-group\" id=\"(ah-cal[0-9]+)\"">>,
                           [{capture, all_but_first, binary}]),
    ?assert(has(<<"aria-controls=\"", Id/binary, "-dropdown\"">>, H)),
    ?assertEqual(1, count(<<" id=\"", Id/binary, "\"">>, H)),
    R = #ah_calendar{},
    ?assertNotEqual(r(R), r(R)).

postback_test() ->
    Token = fun(Html) ->
                    {match, [T]} = re:run(r(Html), <<"data-ah-on=\"([a-z]+:[^\"]+)\"">>,
                                          [{capture, all_but_first, binary}]),
                    [Ev, Tok] = binary:split(T, <<":">>),
                    {ok, Ref} = aihtml_action:unsign(Tok),
                    {Ev, Ref}
            end,
    ?assertEqual({<<"change">>, {?MODULE, load, #{v => 1}}},
                 Token(#ah_calendar{postback = {load, #{v => 1}}})),
    ?assertEqual({<<"change">>, {other_mod, changed, #{}}},
                 Token(#ah_datetime_input{postback = changed, delegate = other_mod})),
    H = r(#ah_calendar{id = c, postback = load}),
    ?assertMatch({match, _}, re:run(H, <<"^<div class=\"ah-calendar\" id=\"c\" "
                                         "data-ah=\"calendar\"[^>]* data-ah-on=\"change:">>)).

field_validation_test() ->
    ?assertError({aihtml, {bad_calendar_view, year}}, r(#ah_calendar{view = year})),
    ?assertError({aihtml, {bad_calendar_views, [month, year]}},
                 r(#ah_calendar{views = [month, year]})),
    ?assertError({aihtml, {bad_first_day, 7}}, r(#ah_calendar{first_day = 7})),
    ?assertError({aihtml, {bad_option, slot_duration, 0}}, r(#ah_calendar{slot_duration = 0})),
    ?assertError({aihtml, {bad_hour_format, 13}}, r(#ah_calendar{hour_format = 13})),
    ?assertError({aihtml, {bad_calendar_label, tomorrow}},
                 r(#ah_calendar{labels = #{tomorrow => <<"x">>}})),
    ?assertError({aihtml, {bad_calendar_label, weekdays}},
                 r(#ah_calendar{labels = #{weekdays => [<<"x">>]}})),
    ?assertError({aihtml, {bad_calendar_event, #{title := <<"x">>}}},
                 r(#ah_calendar{events = [#{title => <<"x">>}]})),
    ?assertError({aihtml, {bad_date, <<"2030-02-30">>}},
                 r(#ah_calendar{events = [#{start => <<"2030-02-30">>}]})),
    ?assertError({aihtml, {bad_calendar_color, <<"red;position:fixed">>}},
                 r(#ah_calendar{events = [#{start => {2030, 1, 1},
                                            color => <<"red;position:fixed">>}]})),
    ?assertError({aihtml, {bad_rrule, _}},
                 r(#ah_calendar{events = [#{start => {2030, 1, 1}, rrule => <<"FREQ=SOMETIMES">>}]})),
    ?assertError({aihtml, {bad_flag, calendar, editable, yes}}, r(#ah_calendar{editable = yes})),
    ?assertError({aihtml, {bad_date, <<"soon">>}}, r(#ah_datetime_input{value = <<"soon">>})),
    ?assertError({aihtml, {bad_datetime_format, <<"--">>}}, r(#ah_datetime_input{format = <<"--">>})),
    ?assertError({aihtml, {bad_datetime_label, now}},
                 r(#ah_datetime_input{labels = #{now => <<"x">>}})),
    ?assertError({aihtml, {modifier_in_css, datetime_input, spinner}},
                 r(#ah_datetime_input{css = [spinner]})),
    ?assertError({aihtml, {unknown_modifier, calendar, big, _}},
                 ?M:calendar(undefined, [big], [])).

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

default(ah_calendar) -> #ah_calendar{};
default(ah_datetime_input) -> #ah_datetime_input{}.
