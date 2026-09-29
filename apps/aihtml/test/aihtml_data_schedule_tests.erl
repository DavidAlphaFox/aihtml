%% Tests for aihtml_data_schedule.
-module(aihtml_data_schedule_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_data_schedule.hrl").

-define(M, aihtml_data_schedule).

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

%%%===================================================================
%%% gantt
%%%===================================================================

tasks() ->
    [#{id => t1, name => <<"A">>, start => <<"2026-09-01">>, 'end' => <<"2026-09-05">>,
       progress => 50},
     #{id => t2, name => <<"B">>, start => <<"2026-09-05">>, 'end' => <<"2026-09-09">>,
       dependencies => [t1], color => <<"#f00">>}].

gantt_default_rows_test() ->
    H = r(?M:gantt(tasks(), [<<"w-full">>], [{id, g}, {today, <<"2026-09-03">>}])),
    ?assert(has(<<"<div class=\"ah-gantt w-full\" id=\"g\" data-ah=\"gantt\" style=\"height:500px;\"">>, H)),
    %% a week before September to a week after it
    ?assert(has(<<"data-origin=\"2026-08-25\"">>, H)),
    ?assert(has(<<"<div class=\"ah-gantt-month-cell\" style=\"width:420px;\">Aug 2026</div>">>, H)),
    ?assert(has(<<"<div class=\"ah-gantt-month-cell\" style=\"width:1800px;\">Sep 2026</div>">>, H)),
    ?assertEqual(7 + 30 + 7, count(<<"class=\"ah-gantt-day-cell">>, H)),
    ?assert(has(<<"ah-gantt-day-cell ah-gantt-day-today\" style=\"width:60px;\">3<">>, H)),
    %% one row per task, labelled with its name
    ?assert(has(<<"data-rowid=\"t1\" data-level=\"0\" role=\"treeitem\" aria-level=\"1\" tabindex=\"0\"">>, H)),
    ?assert(has(<<"<span class=\"ah-gantt-sidebar-label\">B</span>">>, H)),
    %% bars: 7 days after the origin, 4 days wide, second row
    ?assert(has(<<"data-taskid=\"t1\" data-rowid=\"t1\" data-start=\"2026-09-01\" data-end=\"2026-09-05\"">>, H)),
    ?assert(has(<<"left:420px;top:4px;width:240px;height:32px;background:var(--ah-color-primary);">>, H)),
    ?assert(has(<<"left:660px;top:44px;width:240px;height:32px;background:#f00;">>, H)),
    ?assert(has(<<"<div class=\"ah-gantt-task-progress\" style=\"width:50%;\"></div>">>, H)),
    ?assert(has(<<"aria-label=\"A: 2026-09-01 – 2026-09-05, 50%\""/utf8>>, H)),
    ?assert(has(<<"data-deps=\"t1\"">>, H)),
    %% not editable: no resize handles
    ?assertEqual(0, count(<<"ah-gantt-resize-handle">>, H)),
    %% a finish-to-start curve from row 0 to row 1
    ?assert(has(<<"<path class=\"ah-gantt-dep-line\" d=\"M660,20 C690,20 630,60 660,60\"">>, H)),
    %% today at noon of 3 September: 9.5 days
    ?assert(has(<<"<div class=\"ah-gantt-today-marker\" style=\"display:block;left:570px;\">">>, H)).

gantt_rows_test() ->
    Rows = [#{id => p, label => <<"Phase">>}, #{id => c1, label => <<"One">>, parent => p},
            #{id => c2, label => <<"Two">>, parent => p}, #{id => z, label => <<"Z">>}],
    Tasks = [#{id => a, row => c1, start => {2026, 9, 1}, 'end' => {2026, 9, 3}},
             #{id => b, row => c2, start => {{2026, 9, 2}, {12, 0, 0}}, 'end' => <<"2026-09-04T12:00">>},
             #{id => x, row => nowhere, start => <<"2026-09-01">>, 'end' => <<"2026-09-02">>}],
    H = r(?M:gantt(Tasks, [editable], [{rows, Rows}, {collapsed, [p]}, {id, g},
                                      {labels, #{tasks => <<"{n} jobs">>}}])),
    %% parent collapsed: children hidden, summary bar shown on its row
    ?assert(has(<<"data-rowid=\"p\" data-level=\"0\" role=\"treeitem\" aria-level=\"1\" "
                  "aria-expanded=\"false\"">>, H)),
    ?assert(has(<<"data-rowid=\"c1\" data-parent=\"p\" data-level=\"1\" role=\"treeitem\" "
                  "aria-level=\"2\" tabindex=\"-1\" hidden style=\"height:40px;padding-left:36px;\"">>, H)),
    ?assert(has(<<"<span class=\"ah-gantt-expand-icon\" aria-hidden=\"true\">">>, H)),
    ?assert(has(<<"<div class=\"ah-gantt-summary-label\">2 jobs</div>">>, H)),
    ?assert(has(<<"<div class=\"ah-gantt-summary-bar\" data-rowid=\"p\" data-count=\"2\" "
                  "style=\"position:absolute;left:420px;top:4px;width:210px;">>, H)),
    ?assert(has(<<"data-start=\"2026-09-02T12:00\" data-end=\"2026-09-04T12:00\"">>, H)),
    %% the task of an unknown row is not drawn
    ?assertNot(has_quiet(<<"data-taskid=\"x\"">>, H)),
    %% visible rows: p and z; the layer is two rows high
    ?assert(has(<<"<div class=\"ah-gantt-tasks-layer\" style=\"width:2640px;height:80px;\">">>, H)),
    ?assertEqual(4, count(<<"ah-gantt-resize-handle ah-gantt-resize-">>, H)),
    ?assert(has(<<"data-collapsed=\"p\"">>, H)),
    ?assert(has(<<"data-editable">>, H)),
    %% both ends hidden: the path is empty
    ?assertEqual(0, count(<<"ah-gantt-dep-line">>, H)).

gantt_misc_test() ->
    %% no tasks: the current month
    H = r(#ah_gantt{no_dependencies = true, today = {2026, 2, 10}, height = undefined}),
    ?assert(has(<<"data-origin=\"2026-02-01\"">>, H)),
    ?assertEqual(28, count(<<"class=\"ah-gantt-day-cell">>, H)),
    ?assertNot(has_quiet(<<"<svg">>, H)),
    ?assert(has(<<"data-ah=\"gantt\" data-origin">>, H)),
    ?assertError({aihtml, {bad_date, <<"soon">>}},
                 r(?M:gantt([#{id => a, start => <<"soon">>, 'end' => <<"2026-01-01">>}], [], []))),
    ?assertError({aihtml, {bad_gantt_progress, 120}},
                 r(?M:gantt([(hd(tasks()))#{progress => 120}], [], []))),
    ?assertError({aihtml, {bad_color, <<"red;x">>}},
                 r(?M:gantt([(hd(tasks()))#{color => <<"red;x">>}], [], []))),
    ?assertError({aihtml, {bad_option, column_width, 0}},
                 r(?M:gantt([], [], [{column_width, 0}]))).

has_quiet(Needle, Hay) -> binary:match(Hay, Needle) =/= nomatch.

gantt_update_test() ->
    G = ?M:gantt(tasks(), [], [{collapsed, [x]}]),
    [#{op := html, id := <<"g1">>, swap := morph, html := H}] =
        aihtml_action:render_ops(
          fun(Ctx) ->
                  ?M:gantt_update(Ctx, #{id => <<"g1">>, data => #{<<"collapsed">> => <<"a,b">>}}, G)
          end),
    ?assert(has(<<"id=\"g1\"">>, H)),
    ?assert(has(<<"data-collapsed=\"a,b\"">>, H)).

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
    r(?M:scheduler(events(), <<"2026-09-29">>, [editable],
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
    H = r(?M:scheduler(Many ++ events(), {2026, 9, 29}, [],
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
    E = r(?M:scheduler([], <<"2026-09-29">>, [], [{view, agenda}])),
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
    H = r(?M:scheduler([], <<"2026-09-29">>, [no_all_day],
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
                 r(?M:scheduler([], undefined, [], [{labels, #{todays => <<"x">>}}]))),
    ?assertError({aihtml, {bad_label, scheduler, weekdays}},
                 r(?M:scheduler([], undefined, [], [{labels, #{weekdays => [<<"x">>]}}]))),
    ?assertError({aihtml, {bad_option, view, year}},
                 r(?M:scheduler([], undefined, [], [{view, year}]))),
    ?assertError({aihtml, {bad_option, slot_duration, 25}},
                 r(?M:scheduler([], undefined, [], [{slot_duration, 25}]))),
    ?assertError({aihtml, {bad_scheduler_status, <<"gone">>}},
                 r(?M:scheduler([#{start => <<"2026-01-01">>, status => <<"gone">>}], undefined, [], []))),
    ?assertError({aihtml, {bad_rrule, <<"HOURLY">>}},
                 r(?M:scheduler([#{start => <<"2026-01-01">>, rrule => <<"FREQ=HOURLY">>}], undefined, [], []))).

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
          fun(Ctx) -> ?M:scheduler_update(Ctx, Ev, ?M:scheduler(events(), undefined, [], [])) end),
    ?assert(has(<<"id=\"s1\" data-ah=\"scheduler\"">>, H)),
    ?assert(has(<<"data-ah-value=\"2026-10-06\" data-view=\"month\"">>, H)),
    ?assertError({aihtml, {bad_scheduler_view, <<"year">>}},
                 ?M:scheduler_range(Ev#{data => #{<<"view">> => <<"year">>}})).

%%%===================================================================
%%% swimlane
%%%===================================================================

lanes() -> [#{id => l1, name => <<"L1">>, color => blue}, #{id => l2, name => <<"L2">>}].
phases() -> [#{id => p1, label => <<"P1">>}, #{id => p2, label => <<"P2">>}].

swimlane_test() ->
    Nodes = [#{id => a, lane => l1, phase => p1, label => <<"A">>, type => start},
             #{id => b, lane => l1, phase => p1, label => <<"B">>, variant => outline,
               color => <<"#ff0000">>},
             #{id => c, lane => l2, phase => p2, label => <<"C">>, type => decision, dimmed => true},
             #{id => d, lane => nowhere, phase => p2}],
    H = r(?M:swimlane(Nodes, [legend, editable],
                      [{id, sw}, {lanes, lanes()}, {phases, phases()}, {selected, a},
                       {flows, [#{from => a, to => c, label => <<"go">>},
                                #{from => b, to => c, dashed => true, arrow => false},
                                #{from => a, to => d}]}])),
    ?assert(has(<<"<div class=\"ah-swimlane\" id=\"sw\" data-ah=\"swimlane\" data-axis=\"discrete\"">>, H)),
    ?assert(has(<<"data-editable data-selected=\"a\"">>, H)),
    %% two nodes stacked in the first cell: (110 - 114) / 2 = -2 and 60
    ?assert(has(<<"style=\"left:29px;top:-2px;width:132px;height:52px;background-color:#3B82F6\"">>, H)),
    ?assert(has(<<"style=\"left:29px;top:60px;width:132px;height:52px;color:#ff0000\"">>, H)),
    ?assert(has(<<"data-type=\"start\" data-variant=\"solid\" data-state=\"selected\" tabindex=\"0\" "
                  "role=\"button\" aria-pressed=\"true\"">>, H)),
    ?assert(has(<<"data-dimmed=\"true\"">>, H)),
    %% lane without colour: the default grey
    ?assert(has(<<"background-color:#6B7280\"><span class=\"ah-swimlane-node__label\">C</span>">>, H)),
    ?assertNot(has_quiet(<<"data-id=\"d\"">>, H)),
    ?assert(has(<<"<marker id=\"sw-arrow-active\"">>, H)),
    ?assert(has(<<"<g data-from=\"a\" data-to=\"c\" data-active=\"true\">">>, H)),
    ?assert(has(<<"<g data-from=\"b\" data-to=\"c\" data-dim=\"true\">">>, H)),
    ?assert(has(<<"marker-end=\"url(#sw-arrow-active)\"">>, H)),
    ?assert(has(<<"data-dashed=\"true\">">>, H)),
    ?assert(has(<<"<text class=\"ah-swimlane-flows__label\"">>, H)),
    %% the flow to an unplaced node is not drawn
    ?assertEqual(2, count(<<"<g ">>, H)),
    ?assert(has(<<"<div class=\"ah-swimlane-legend\">">>, H)),
    ?assert(has(<<"<div class=\"ah-swimlane-grid__phase\" data-phase-id=\"p2\" style=\"width:190px\">P2</div>">>, H)).

swimlane_geometry_test() ->
    %% a forward elbow with rounded corners (sigil's flow-points, points->path)
    F = r(?M:swimlane([#{id => a, lane => l1, phase => p1}, #{id => b, lane => l2, phase => p2}],
                      [], [{lanes, lanes()}, {phases, phases()}, {flows, [#{from => a, to => b}]}])),
    ?assert(has(<<"d=\"M161,55 L180,55 Q190,55 190,65 L190,155 Q190,165 200,165 L219,165\"">>, F)),
    H = r(?M:swimlane([#{id => a, lane => l1, value => 0}, #{id => b, lane => l2, value => 10}],
                      [], [{lanes, lanes()}, {axis, continuous}, {axis_width, 400},
                           {flows, [#{from => a, to => b}]}])),
    ?assert(has(<<"left:0px;top:29px;">>, H)),
    ?assert(has(<<"left:268px;top:139px;">>, H)),
    ?assert(has(<<"<div class=\"ah-swimlane-grid__tick-label\" style=\"position:absolute;left:66px;"
                  "transform:translateX(-50%)\">0</div>">>, H)),
    ?assertError({aihtml, {bad_option, axis, round}}, r(?M:swimlane([], [], [{axis, round}]))),
    ?assertError({aihtml, {bad_swimlane_node_type, box}},
                 r(?M:swimlane([#{id => a, lane => l1, type => box}], [], []))),
    ?assertError({aihtml, {bad_color, magenta}},
                 r(?M:swimlane([], [], [{lanes, [#{id => x, color => magenta}]}]))).

swimlane_update_test() ->
    S = ?M:swimlane([], [], []),
    [#{op := html, id := <<"w">>, html := H}] =
        aihtml_action:render_ops(
          fun(Ctx) -> ?M:swimlane_update(Ctx, #{id => <<"w">>, data => #{<<"selected">> => <<"n">>}}, S) end),
    ?assert(has(<<"data-selected=\"n\"">>, H)).

%%%===================================================================
%%% Catalog
%%%===================================================================

catalog_test() ->
    [#{name := gantt}, #{name := scheduler}, #{name := swimlane}] = ?M:catalog(),
    ?assertEqual([{scheduler_update, 3}, {scheduler_range, 1}, {gantt_update, 3},
                  {swimlane_update, 3}], ?M:facade_extras()),
    [?assert(erlang:function_exported(?M, F, A)) || {F, A} <- ?M:facade_extras()],
    [begin
         #{flags := Fl, options := Op, option_docs := Docs, methods := Ms} = E =
             aihtml_catalog:entry(?M, N),
         ?assertEqual(lists:sort(Fl ++ Op), lists:sort(maps:keys(Docs))),
         [_ | _] = aihtml_catalog:classes(E, Fl),
         [?assert(is_binary(D)) || #{doc := D} <- Ms]
     end || #{name := N} <- ?M:catalog()].

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
    ?assertEqual(r(?M:gantt(tasks(), [editable, <<"x">>],
                            [{id, g}, {rows, [#{id => t1}, #{id => t2}]}, {today, {2026, 9, 3}},
                             {height, 300}, {title, <<"t">>}])),
                 r(#ah_gantt{items = tasks(), editable = true, css = [<<"x">>], id = g,
                             rows = [#{id => t1}, #{id => t2}], today = {2026, 9, 3},
                             height = 300, attrs = [{title, <<"t">>}]})),
    ?assertEqual(r(?M:scheduler(events(), <<"2026-09-29">>, [no_all_day],
                                [{id, s}, {view, month}, {resources, res()}, {name, shown},
                                 {today, <<"2026-09-29">>}])),
                 r(#ah_scheduler{items = events(), value = <<"2026-09-29">>, no_all_day = true,
                                 id = s, view = month, resources = res(), name = shown,
                                 today = <<"2026-09-29">>})),
    ?assertEqual(r(?M:swimlane([#{id => a, lane => l1, phase => p1}], [legend],
                               [{id, w}, {lanes, lanes()}, {phases, phases()}])),
                 r(#ah_swimlane{items = [#{id => a, lane => l1, phase => p1}], legend = true,
                                id = w, lanes = lanes(), phases = phases()})).

builder_fills_fields_test() ->
    ?assertMatch(#ah_gantt{items = [], no_dependencies = true, column_width = 30,
                           attrs = [{role, x}]},
                 ?M:gantt([], [no_dependencies], [{column_width, 30}, {role, x}])),
    ?assertMatch(#ah_scheduler{value = <<"2026-01-01">>, editable = true, view = agenda,
                               source = {m, a, #{}}},
                 ?M:scheduler([], <<"2026-01-01">>, [editable],
                              [{view, agenda}, {source, {m, a, #{}}}])),
    ?assertError({aihtml, {record_only_field, ah_swimlane, postback}},
                 ?M:swimlane([], [], [{postback, x}])).

postback_test() ->
    Token = fun(Html) ->
                    {match, [T]} = re:run(r(Html), <<"data-ah-on=\"([a-z:-]+:[^\" ]+)\"">>,
                                          [{capture, all_but_first, binary}]),
                    [Tok | Rev] = lists:reverse(binary:split(T, <<":">>, [global])),
                    {ok, Ref} = aihtml_action:unsign(Tok),
                    {iolist_to_binary(lists:join(<<":">>, lists:reverse(Rev))), Ref}
            end,
    ?assertEqual({<<"ah:task-change">>, {?MODULE, moved, #{}}},
                 Token(#ah_gantt{postback = moved})),
    ?assertEqual({<<"change">>, {?MODULE, shown, #{a => 1}}},
                 Token(#ah_scheduler{postback = {shown, #{a => 1}}})),
    ?assertEqual({<<"ah:node-change">>, {?MODULE, moved, #{}}},
                 Token(#ah_swimlane{postback = moved})),
    %% a postback on change is enough for the navigation buttons
    ?assert(has(<<"ah-scheduler-btn-next">>, r(#ah_scheduler{postback = shown}))).

field_validation_test() ->
    ?assertError({aihtml, {bad_flag, gantt, editable, yes}}, r(#ah_gantt{editable = yes})),
    ?assertError({aihtml, {bad_option, first_day, 7}}, r(#ah_scheduler{first_day = 7})),
    ?assertError({aihtml, {bad_option, day_end, 3}}, r(#ah_scheduler{day_start = 5, day_end = 3})),
    ?assertError({aihtml, {bad_option, hour_format, 10}}, r(#ah_scheduler{hour_format = 10})),
    ?assertError({aihtml, {bad_option, views, []}}, r(#ah_scheduler{views = []})),
    ?assertError({aihtml, {bad_label, gantt, x}}, r(#ah_gantt{labels = #{x => <<"y">>}})),
    ?assertError({aihtml, {modifier_in_css, swimlane, legend}}, r(#ah_swimlane{css = [legend]})),
    ?assertError({aihtml, {unknown_modifier, gantt, big, _}}, ?M:gantt([], [big], [])).

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

default(ah_gantt) -> #ah_gantt{};
default(ah_scheduler) -> #ah_scheduler{};
default(ah_swimlane) -> #ah_swimlane{}.

generated_id_test() ->
    H = r(#ah_swimlane{flows = []}),
    ?assertMatch({match, _}, re:run(H, <<"^<div class=\"ah-swimlane\" id=\"ah-s[0-9]+\"">>)),
    ?assertNotEqual(r(#ah_gantt{}), r(#ah_gantt{})).
