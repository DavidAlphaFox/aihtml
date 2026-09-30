%% Tests for aihtml_scheduler.
-module(aihtml_scheduler_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_scheduler.hrl").

-define(M, aihtml_scheduler).

r(Html) -> aihtml_html:render_binary(Html).

has(Needle, Hay) ->
    case binary:match(Hay, Needle) of
        nomatch -> ?debugFmt("~s not in~n~s", [Needle, Hay]), false;
        _ -> true
    end.

count(Needle, Hay) -> length(binary:matches(Hay, Needle)).

rcount(Re, Hay) ->
    case re:run(Hay, Re, [global]) of
        {match, L} -> length(L);
        nomatch -> 0
    end.

has_quiet(Needle, Hay) -> binary:match(Hay, Needle) =/= nomatch.

%%%===================================================================
%%% scheduler
%%%===================================================================

events() ->
    [#{id => a, title => <<"Meet">>, start => <<"2026-09-29T09:00">>,
       'end' => <<"2026-09-29T10:30">>, resource => r1},
     #{id => o, title => <<"Overlap">>, start => <<"2026-09-29T10:00">>,
       'end' => <<"2026-09-29T11:00">>, resource => r1, status => tentative},
     #{id => w, title => <<"Weekly">>, start => <<"2026-09-01T14:00">>,
       'end' => <<"2026-09-01T15:00">>, rrule => <<"FREQ=WEEKLY;BYDAY=TU">>,
       exdates => [<<"2026-09-22">>]},
     #{id => t, title => <<"Trip">>, start => <<"2026-09-28">>, 'end' => <<"2026-10-01">>,
       status => out_of_office, color => <<"#123456">>}].

res() -> [#{id => r1, name => <<"Room A">>, color => <<"#4285f4">>},
          #{id => r2, name => <<"Room B">>}].

sch(View, Extra) ->
    r(?M:ah_scheduler(events(), <<"2026-09-29">>, [editable],
                      [{id, s}, {view, View}, {today, <<"2026-09-29">>} | Extra])).

scheduler_week_test() ->
    H = sch(week, []),
    ?assert(has(<<"<div class=\"ah-scheduler\" id=\"s\" data-ah=\"scheduler\" style=\"height:600px;\" "
                  "data-ah-value=\"2026-09-29\" data-view=\"week\" data-start=\"2026-09-28\" "
                  "data-end=\"2026-10-05\"">>, H)),
    ?assert(has(<<"<h2 class=\"ah-scheduler-title\" aria-live=\"polite\">Sep 28 – Oct 4, 2026</h2>"/utf8>>, H)),
    %% no source, no postback: no navigation buttons
    ?assertNot(has_quiet(<<"ah-scheduler-btn-prev">>, H)),
    ?assertEqual(7, rcount(<<"class=\"ah-scheduler-dayview-col[ \"]">>, H)),
    ?assert(has(<<"ah-scheduler-dayview-col ah-scheduler-dayview-col-today\" data-date=\"2026-09-29\" "
                  "style=\"position:relative;height:960px;\"">>, H)),
    %% overlapping appointments share the column
    ?assert(has(<<"top:360px;height:60px;left:0%;width:50%;">>, H)),
    ?assert(has(<<"top:400px;height:40px;left:50%;width:50%;">>, H)),
    ?assert(has(<<"ah-scheduler-event ah-scheduler-timegrid-event ah-scheduler-status-tentative">>, H)),
    ?assert(has(<<"<div class=\"ah-scheduler-event-time\">9:00 AM – 10:30 AM</div>"/utf8>>, H)),
    %% the series, not on the 22nd, occurs on the 29th
    ?assert(has(<<"data-eventid=\"w_20260929T140000\" data-source=\"w\"">>, H)),
    ?assert(has(<<"ah-scheduler-event-recurring-icon">>, H)),
    %% the all day trip on Monday to Wednesday
    ?assertEqual(3, count(<<"ah-scheduler-allday-event ah-scheduler-status-out-of-office">>, H)),
    ?assert(has(<<"style=\"background:#123456;border-left:3px solid var(--ah-color-info, #6366f1);\"">>, H)),
    ?assert(has(<<"<div class=\"ah-scheduler-timegrid-slot-label\">12 AM</div>">>, H)),
    ?assert(has(<<"<template class=\"ah-scheduler-menu-event\">">>, H)),
    ?assert(has(<<"data-action=\"create\"">>, H)),
    ?assertEqual(3, count(<<"ah-scheduler-timegrid-resize-handle">>, H)).

scheduler_resources_test() ->
    H = sch(day, [{resources, res()}, {day_start, 8}, {day_end, 12}, {hour_format, 24},
                  {slot_duration, 60}, {slot_height, 30}]),
    ?assertEqual(2, count(<<"class=\"ah-scheduler-dayview-res-col\"">>, H)),
    ?assert(has(<<"data-date=\"2026-09-29\" data-resourceid=\"r1\" style=\"position:relative;height:120px;">>, H)),
    ?assert(has(<<"<div class=\"ah-scheduler-dayview-resource-label\" "
                  "style=\"border-bottom:2px solid var(--ah-color-primary);\">Room B</div>">>, H)),
    ?assert(has(<<"<div class=\"ah-scheduler-timegrid-slot-label\">08:00</div>">>, H)),
    ?assert(has(<<"top:30px;height:45px;">>, H)),
    ?assert(has(<<"<div class=\"ah-scheduler-event-time\">09:00 – 10:30</div>"/utf8>>, H)),
    %% the 14:00 weekly meeting is after day_end, and has no resource
    ?assertNot(has_quiet(<<"data-source=\"w\"">>, H)),
    ?assert(has(<<"<h2 class=\"ah-scheduler-title\" aria-live=\"polite\">2026-09-29 Tuesday</h2>">>, H)).

scheduler_month_test() ->
    Many = [#{id => N, title => N, start => <<"2026-09-15T0", (integer_to_binary(N))/binary, ":00">>}
            || N <- lists:seq(1, 5)],
    H = r(?M:ah_scheduler(Many ++ events(), {2026, 9, 29}, [],
                          [{view, month}, {day_max_events, 2}, {today, <<"2026-09-29">>},
                           {resources, res()}, {source, {?MODULE, load, #{}}}])),
    ?assert(has(<<"data-start=\"2026-08-31\" data-end=\"2026-10-05\"">>, H)),
    ?assert(has(<<"ah-scheduler-btn-prev">>, H)),
    ?assert(has(<<"data-ah-sync=\"queue\"">>, H)),
    ?assert(has(<<"<div class=\"ah-scheduler-monthview-header-cell\">Mon</div>">>, H)),
    ?assertEqual(35, rcount(<<"class=\"ah-scheduler-day( ah-scheduler-day-[a-z]+)*\" data-date">>, H)),
    ?assert(has(<<"ah-scheduler-day ah-scheduler-day-other\" data-date=\"2026-08-31\"">>, H)),
    ?assert(has(<<"ah-scheduler-day-num ah-scheduler-day-num-today">>, H)),
    %% 5 + the weekly one on the 15th (Tuesday, column 2), room for 2: "+4 more" and its popover
    ?assert(has(<<"data-date=\"2026-09-15\" role=\"button\" tabindex=\"0\" aria-haspopup=\"dialog\" "
                  "style=\"grid-column:2;grid-row:4;\">+4 more<template>">>, H)),
    ?assert(has(<<"<span>2026-09-15 Tue</span>">>, H)),
    %% the three day trip spans Monday to Wednesday of the last week
    ?assert(has(<<"ah-scheduler-status-out-of-office ah-scheduler-month-event-multi\"">>, H)),
    ?assert(has(<<"grid-column:1/4;grid-row:2;">>, H)),
    ?assert(has(<<"<span class=\"ah-scheduler-month-event-resource-dot\" style=\"background:#4285f4;\">">>, H)),
    ?assert(has(<<"<span class=\"ah-scheduler-event-time\">9:00 AM</span>">>, H)).

scheduler_agenda_timeline_test() ->
    A = sch(agenda, [{agenda_days, 2}, {resources, res()}]),
    ?assert(has(<<"<span class=\"ah-scheduler-agenda-day-date\">September 29, 2026</span>">>, A)),
    ?assert(has(<<"<div class=\"ah-scheduler-agenda-event-time\">All day</div>">>, A)),
    ?assert(has(<<"<div class=\"ah-scheduler-agenda-event-resource-name\">Room A</div>">>, A)),
    E = r(?M:ah_scheduler([], <<"2026-09-29">>, [], [{view, agenda}])),
    ?assert(has(<<"<div class=\"ah-scheduler-agenda-empty\">">>, E)),
    T = sch(timeline_day, [{resources, res()}, {day_start, 8}, {day_end, 18}]),
    ?assertEqual(10, count(<<"class=\"ah-scheduler-timeline-slot-header\"">>, T)),
    ?assert(has(<<"data-from=\"2026-09-29T08:00\" data-to=\"2026-09-29T18:00\" style=\"min-width:600px;\"">>, T)),
    ?assert(has(<<"left:10%;width:15%;">>, T)),
    ?assert(has(<<"<div class=\"ah-scheduler-timeline-resource-name\">Room B</div>">>, T)),
    ?assertEqual(4, count(<<"ah-scheduler-timeline-resize-handle ">>, T)),
    W = sch(timeline_week, []),
    ?assert(has(<<"ah-scheduler-timeline-slot-header ah-scheduler-timeline-slot-today\" "
                  "style=\"width:14.286%;\">Tue 29</div>">>, W)),
    %% no resources: one row with every appointment, all day ones too
    ?assert(has(<<"<div class=\"ah-scheduler-timeline-resource-name\">—</div>"/utf8>>, W)),
    ?assert(has(<<"data-eventid=\"t\"">>, W)).

scheduler_labels_test() ->
    H = r(?M:ah_scheduler([], <<"2026-09-29">>, [no_all_day],
                          [{labels, #{weekdays_short => [<<"日"/utf8>>, <<"一"/utf8>>, <<"二"/utf8>>,
                                                         <<"三"/utf8>>, <<"四"/utf8>>, <<"五"/utf8>>,
                                                         <<"六"/utf8>>],
                                      range_start => <<"M月d日"/utf8>>,
                                      range_end => <<"M月d日"/utf8>>}},
                           {first_day, 0}])),
    ?assert(has(<<">9月27日 – 10月3日</h2>"/utf8>>, H)),
    ?assert(has(<<"<span class=\"ah-scheduler-dayview-header-day\">日</span>"/utf8>>, H)),
    ?assertNot(has_quiet(<<"allday-row">>, H)),
    ?assertError({aihtml, {bad_label, scheduler, todays}},
                 r(?M:ah_scheduler([], undefined, [], [{labels, #{todays => <<"x">>}}]))),
    ?assertError({aihtml, {bad_label, scheduler, weekdays}},
                 r(?M:ah_scheduler([], undefined, [], [{labels, #{weekdays => [<<"x">>]}}]))),
    ?assertError({aihtml, {bad_option, view, year}},
                 r(?M:ah_scheduler([], undefined, [], [{view, year}]))),
    ?assertError({aihtml, {bad_option, slot_duration, 25}},
                 r(?M:ah_scheduler([], undefined, [], [{slot_duration, 25}]))),
    ?assertError({aihtml, {bad_scheduler_status, <<"gone">>}},
                 r(?M:ah_scheduler([#{start => <<"2026-01-01">>, status => <<"gone">>}], undefined, [], []))),
    ?assertError({aihtml, {bad_rrule, <<"HOURLY">>}},
                 r(?M:ah_scheduler([#{start => <<"2026-01-01">>, rrule => <<"FREQ=HOURLY">>}], undefined, [], []))).

scheduler_round_trip_test() ->
    Ev = #{id => <<"s1">>, value => <<"2026-10-06">>,
           data => #{<<"view">> => <<"month">>, <<"firstDay">> => <<"1">>}},
    ?assertEqual(#{view => month, date => <<"2026-10-06">>, start => <<"2026-09-28">>,
                   'end' => <<"2026-11-02">>}, ?M:scheduler_range(Ev)),
    ?assertMatch(#{view := agenda, start := <<"2026-10-06">>, 'end' := <<"2026-10-13">>},
                 ?M:scheduler_range(Ev#{data => #{<<"view">> => <<"agenda">>,
                                                  <<"agendaDays">> => <<"7">>}})),
    [#{op := html, id := <<"s1">>, swap := morph, html := H}] =
        aihtml_action:render_ops(
          fun(Ctx) -> ?M:scheduler_update(Ctx, Ev, ?M:ah_scheduler(events(), undefined, [], [])) end),
    ?assert(has(<<"id=\"s1\" data-ah=\"scheduler\"">>, H)),
    ?assert(has(<<"data-ah-value=\"2026-10-06\" data-view=\"month\"">>, H)),
    ?assertError({aihtml, {bad_scheduler_view, <<"year">>}},
                 ?M:scheduler_range(Ev#{data => #{<<"view">> => <<"year">>}})).

%%%===================================================================
%%% Catalog
%%%===================================================================

catalog_test() ->
    [#{name := scheduler}] = ?M:catalog(),
    ?assertEqual([{scheduler_update, 3}, {scheduler_range, 1}], ?M:facade_extras()),
    [?assert(erlang:function_exported(?M, F, A)) || {F, A} <- ?M:facade_extras()],
    #{flags := Fl, options := Op, option_docs := Docs, methods := Ms} = E =
        aihtml_catalog:entry(?M, scheduler),
    ?assertEqual(lists:sort(Fl ++ Op), lists:sort(maps:keys(Docs))),
    [_ | _] = aihtml_catalog:classes(E, Fl),
    [?assert(is_binary(D)) || #{doc := D} <- Ms].

catalog_docs_test() ->
    [begin
         ?assert(byte_size(maps:get(doc, E)) > 0),
         [?assert(byte_size(maps:get(K, maps:get(option_docs, E))) > 0)
          || K <- maps:get(options, E, []) ++ maps:get(flags, E, [])]
     end || E <- ?M:catalog()].

%%%===================================================================
%%% element records (designs/05-records.md)
%%%===================================================================

record_equals_builder_test() ->
    ?assertEqual(r(?M:ah_scheduler(events(), <<"2026-09-29">>, [no_all_day],
                                   [{id, s}, {view, month}, {resources, res()}, {name, shown},
                                    {today, <<"2026-09-29">>}])),
                 r(#ah_scheduler{items = events(), value = <<"2026-09-29">>, no_all_day = true,
                                 id = s, view = month, resources = res(), name = shown,
                                 today = <<"2026-09-29">>})).

builder_fills_fields_test() ->
    ?assertMatch(#ah_scheduler{value = <<"2026-01-01">>, editable = true, view = agenda,
                               source = {m, a, #{}}},
                 ?M:ah_scheduler([], <<"2026-01-01">>, [editable],
                                 [{view, agenda}, {source, {m, a, #{}}}])).

postback_test() ->
    Token = fun(Html) ->
                    {match, [T]} = re:run(r(Html), <<"data-ah-on=\"([a-z:-]+:[^\" ]+)\"">>,
                                          [{capture, all_but_first, binary}]),
                    [Tok | Rev] = lists:reverse(binary:split(T, <<":">>, [global])),
                    {ok, Ref} = aihtml_action:unsign(Tok),
                    {iolist_to_binary(lists:join(<<":">>, lists:reverse(Rev))), Ref}
            end,
    ?assertEqual({<<"change">>, {?MODULE, shown, #{a => 1}}},
                 Token(#ah_scheduler{postback = {shown, #{a => 1}}})),
    %% a postback on change is enough for the navigation buttons
    ?assert(has(<<"ah-scheduler-btn-next">>, r(#ah_scheduler{postback = shown}))).

%% href: the toolbar entries become links to the state they lead to.
-define(HREF, <<"/s?d={date}&v={view}">>).

href_of(Class, Html) ->
    {match, [U]} = re:run(Html, <<"<a class=\"", Class/binary, "\" href=\"([^\"]*)\"">>,
                          [{capture, all_but_first, binary}]),
    U.

view_link(V, Html) ->
    {match, [U]} = re:run(Html, <<"<a class=\"ah-scheduler-view-btn[^\"]*\" href=\"([^\"]*)\" "
                                  "data-view=\"", V/binary, "\"">>,
                          [{capture, all_but_first, binary}]),
    U.

nav_links(View, Date, Extra) ->
    H = r(?M:ah_scheduler([], Date, [], [{view, View}, {today, <<"2026-09-29">>},
                                         {href, ?HREF} | Extra])),
    {href_of(<<"ah-scheduler-btn ah-scheduler-btn-prev">>, H),
     href_of(<<"ah-scheduler-btn ah-scheduler-btn-today">>, H),
     href_of(<<"ah-scheduler-btn ah-scheduler-btn-next">>, H)}.

scheduler_href_links_test() ->
    V = fun(D, W) -> <<"/s?d=", D/binary, "&amp;v=", W/binary>> end,
    ?assertEqual({V(<<"2026-09-22">>, <<"week">>), V(<<"2026-09-29">>, <<"week">>),
                  V(<<"2026-10-06">>, <<"week">>)}, nav_links(week, <<"2026-09-29">>, [])),
    ?assertEqual({V(<<"2026-09-28">>, <<"day">>), V(<<"2026-09-29">>, <<"day">>),
                  V(<<"2026-09-30">>, <<"day">>)}, nav_links(day, <<"2026-09-29">>, [])),
    %% months clamp the day (Jan 31 -> Feb 28)
    ?assertEqual({V(<<"2025-12-31">>, <<"month">>), V(<<"2026-09-29">>, <<"month">>),
                  V(<<"2026-02-28">>, <<"month">>)}, nav_links(month, <<"2026-01-31">>, [])),
    ?assertEqual({V(<<"2026-08-29">>, <<"timeline_month">>), V(<<"2026-09-29">>, <<"timeline_month">>),
                  V(<<"2026-10-29">>, <<"timeline_month">>)},
                 nav_links(timeline_month, <<"2026-09-29">>, [])),
    ?assertEqual({V(<<"2026-09-22">>, <<"timeline_week">>), V(<<"2026-09-29">>, <<"timeline_week">>),
                  V(<<"2026-10-06">>, <<"timeline_week">>)},
                 nav_links(timeline_week, <<"2026-09-29">>, [])),
    ?assertEqual({V(<<"2026-09-19">>, <<"agenda">>), V(<<"2026-09-29">>, <<"agenda">>),
                  V(<<"2026-10-09">>, <<"agenda">>)},
                 nav_links(agenda, <<"2026-09-29">>, [{agenda_days, 10}])),
    %% view links: the shown date in each view; the active one is aria-current
    H = r(?M:ah_scheduler([], <<"2026-09-10">>, [], [{view, week}, {href, ?HREF}])),
    ?assertEqual(V(<<"2026-09-10">>, <<"month">>), view_link(<<"month">>, H)),
    ?assertEqual(V(<<"2026-09-10">>, <<"day">>), view_link(<<"day">>, H)),
    ?assert(has(<<"data-view=\"week\" aria-current=\"true\"">>, H)),
    ?assertNot(has_quiet(<<"<button">>, H)),
    %% the template goes to the root for the behaviour; href alone makes the
    %% toolbar navigable
    ?assert(has(<<"data-href=\"/s?d={date}&amp;v={view}\"">>, H)),
    ?assert(has(<<"ah-scheduler-btn-next">>, H)).

scheduler_no_href_unchanged_test() ->
    H = sch(week, [{source, {?MODULE, load, #{}}}]),
    ?assertNot(has_quiet(<<"<a ">>, H)),
    ?assertNot(has_quiet(<<"data-href">>, H)),
    ?assert(has(<<"<button class=\"ah-scheduler-btn ah-scheduler-btn-prev\" type=\"button\"">>, H)).

field_validation_test() ->
    ?assertError({aihtml, {bad_option, first_day, 7}}, r(#ah_scheduler{first_day = 7})),
    ?assertError({aihtml, {bad_option, day_end, 3}}, r(#ah_scheduler{day_start = 5, day_end = 3})),
    ?assertError({aihtml, {bad_option, hour_format, 10}}, r(#ah_scheduler{hour_format = 10})),
    ?assertError({aihtml, {bad_option, views, []}}, r(#ah_scheduler{views = []})).

records_match_catalog_test() ->
    Base = [module, id, css, attrs, postback, delegate],
    [begin
         Tag = list_to_atom("ah_" ++ atom_to_list(N)),
         Fields = ?M:fields(Tag),
         ?assertEqual(Base, lists:sublist(Fields, 6)),
         Defaults = maps:from_list(lists:zip(Fields, tl(tuple_to_list(default(Tag))))),
         [?assertEqual({N, F, false}, {N, F, maps:get(F, Defaults)})
          || F <- maps:get(flags, E, [])],
         [?assert(lists:member(O, Fields)) || O <- maps:get(options, E, [])],
         ?assertEqual(?M, maps:get(module, Defaults))
     end || #{name := N} = E <- ?M:catalog()].

default(ah_scheduler) -> #ah_scheduler{}.
