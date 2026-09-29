%% @doc Demos of the schedule components (aihtml_data_schedule), shown on
%% /components/<name>. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
%%
%% The module is also the action module of the demos that talk to the
%% server: the scheduler loads every range it navigates to from
%% appointments/2 (standing in for a database), and the edit demos
%% report what the postback received.
-module(aihtml_example_demo_data_schedule).
-behaviour(aihtml_action).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0, action/4]).
-export([gantt_basic/0, gantt_rows/0, gantt_edit/0, gantt_compact/0,
         sched_week/0, sched_timeline/0, sched_month/0, sched_agenda/0, sched_locale/0,
         sched_record/0,
         swim_basic/0, swim_edit/0, swim_variants/0, swim_continuous/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => gantt, title => <<"Gantt">>,
       summary => <<"甘特图：任务条、分组行、进度、依赖连线，拖拽调整排期并通知服务端。"/utf8>>,
       demos => [{<<"任务、进度与依赖"/utf8>>, gantt_basic},
                 {<<"分组行、折叠与汇总条"/utf8>>, gantt_rows},
                 {<<"拖拽改期，服务端收到改动"/utf8>>, gantt_edit},
                 {<<"紧凑尺寸、不画依赖线"/utf8>>, gantt_compact}]},
     #{component => scheduler, title => <<"Scheduler">>,
       summary => <<"资源调度：日/周/月/日程/时间线视图，翻页时由服务端加载并渲染新的日期范围。"/utf8>>,
       demos => [{<<"周视图、资源列、服务端翻页"/utf8>>, sched_week},
                 {<<"时间线视图"/utf8>>, sched_timeline},
                 {<<"月视图与“更多”弹层"/utf8>>, sched_month},
                 {<<"日程列表、24 小时制"/utf8>>, sched_agenda},
                 {<<"中文标签、工作时间"/utf8>>, sched_locale},
                 {<<"record 写法"/utf8>>, sched_record}]},
     #{component => swimlane, title => <<"Swimlane">>,
       summary => <<"泳道图：泳道 × 阶段的跨职能流程，节点可拖到别的格子，连线自动重排。"/utf8>>,
       demos => [{<<"订单履约流程"/utf8>>, swim_basic},
                 {<<"拖拽换格，服务端收到改动"/utf8>>, swim_edit},
                 {<<"描边、淡化与选中"/utf8>>, swim_variants},
                 {<<"连续数值轴"/utf8>>, swim_continuous}]}].

%%%===================================================================
%%% Gantt
%%%===================================================================

-spec gantt_basic() -> aihtml:html().
gantt_basic() ->
    gantt([#{id => research, name => <<"Research">>, start => <<"2026-09-07">>,
             'end' => <<"2026-09-14">>, progress => 100},
           #{id => design, name => <<"Design">>, start => <<"2026-09-14">>,
             'end' => <<"2026-09-24">>, progress => 70, dependencies => [research]},
           #{id => build, name => <<"Build">>, start => <<"2026-09-24">>,
             'end' => <<"2026-10-14">>, progress => 20, dependencies => [design],
             color => <<"var(--ah-color-success)">>},
           #{id => launch, name => <<"Launch">>, start => <<"2026-10-14">>,
             'end' => <<"2026-10-16">>, dependencies => [build],
             color => <<"var(--ah-color-warning)">>}],
          [], [{height, 260}, {sidebar_width, 180}]).

-spec gantt_rows() -> aihtml:html().
gantt_rows() ->
    gantt(project_tasks(), [editable],
          [{rows, project_rows()}, {collapsed, [<<"testing">>]}, {height, 440},
           {sidebar_width, 200},
           {labels, #{task => <<"任务"/utf8>>, tasks => <<"{n} 项任务"/utf8>>}}]).

%% Dragging a bar calls action(task_changed, ...) below.
-spec gantt_edit() -> aihtml:html().
gantt_edit() ->
    'div'([gantt(project_tasks(), [editable],
                 [{rows, project_rows()}, {height, 300}, {sidebar_width, 200},
                  on('ah:task-change', {?MODULE, task_changed, #{}})]),
           p(<<"拖动任务条或它的两端；选中任务条后可用方向键调整。"/utf8>>,
             [<<"text-sm text-muted mt-2">>], [{id, <<"gantt-log">>}])], [], []).

-spec gantt_compact() -> aihtml:html().
gantt_compact() ->
    gantt([#{id => T, name => N, start => S, 'end' => E}
           || {T, N, S, E} <- [{a, <<"Kick-off">>, <<"2026-09-28">>, <<"2026-09-29">>},
                               {b, <<"Survey">>, <<"2026-09-29">>, <<"2026-10-06">>},
                               {c, <<"Report">>, <<"2026-10-05">>, <<"2026-10-09">>}]],
          [no_dependencies], [{height, 180}, {column_width, 32}, {row_height, 30},
                              {sidebar_width, 140}]).

%%%===================================================================
%%% Scheduler
%%%===================================================================

%% Navigating calls action(load_range, ...) below, which renders the
%% range the event asks for from appointments/2.
-spec sched_week() -> aihtml:html().
sched_week() ->
    'div'([rooms(week, appointments(<<"2026-09-28">>, <<"2026-10-05">>)),
           p(<<"拖动预约换时间或会议室，在空白处拖动选择时段；右键打开菜单。"/utf8>>,
             [<<"text-sm text-muted mt-2">>], [{id, <<"rooms-log">>}])], [], []).

-spec sched_timeline() -> aihtml:html().
sched_timeline() ->
    scheduler(appointments(<<"2026-09-01">>, <<"2026-11-01">>), undefined, [editable],
              [{view, timeline_day}, {views, [timeline_day, timeline_week, timeline_month]},
               {resources, resources()}, {day_start, 7}, {day_end, 21}, {height, 300},
               {source, {?MODULE, load_timeline, #{}}}]).

-spec sched_month() -> aihtml:html().
sched_month() ->
    scheduler(appointments(<<"2026-08-20">>, <<"2026-10-15">>), <<"2026-09-29">>, [editable],
              [{view, month}, {views, [month, week, agenda]}, {resources, resources()},
               {day_max_events, 2}, {height, 640},
               {source, {?MODULE, load_month, #{}}}]).

-spec sched_agenda() -> aihtml:html().
sched_agenda() ->
    scheduler(appointments(<<"2026-09-28">>, <<"2026-10-05">>), <<"2026-09-28">>, [],
              [{view, agenda}, {agenda_days, 7}, {hour_format, 24}, {resources, resources()},
               {toolbar, false}, {height, 420}]).

-spec sched_locale() -> aihtml:html().
sched_locale() ->
    scheduler(appointments(<<"2026-09-28">>, <<"2026-10-05">>), <<"2026-09-29">>, [no_all_day],
              [{view, week}, {first_day, 1}, {day_start, 8}, {day_end, 19},
               {hour_format, 24}, {slot_height, 24}, {height, 520},
               {labels, #{today => <<"今天"/utf8>>, all_day_short => <<"全天"/utf8>>,
                          weekdays_short => [<<"周日"/utf8>>, <<"周一"/utf8>>, <<"周二"/utf8>>,
                                             <<"周三"/utf8>>, <<"周四"/utf8>>, <<"周五"/utf8>>,
                                             <<"周六"/utf8>>],
                          range_start => <<"M月d日"/utf8>>,
                          range_end => <<"M月d日，yyyy年"/utf8>>}}]).

%% The same component as a record: options are checked field names; the
%% postback runs action(shown, ...) on every navigation.
-spec sched_record() -> aihtml:html().
sched_record() ->
    'div'([#ah_scheduler{items = appointments(<<"2026-09-28">>, <<"2026-10-05">>),
                         value = <<"2026-09-29">>, view = day, views = [day, week],
                         resources = resources(), editable = true, day_start = 8,
                         day_end = 18, height = 420, postback = shown},
           p(<<"切换日期或视图时，服务端会收到 change 事件。"/utf8>>,
             [<<"text-sm text-muted mt-2">>], [{id, <<"sched-log">>}])], [], []).

rooms(View, Events) ->
    scheduler(Events, <<"2026-09-29">>, [editable],
              [{view, View}, {views, [day, week, month, timeline_day, agenda]},
               {resources, resources()}, {height, 560},
               {source, {?MODULE, load_rooms, #{}}},
               on('ah:event-change', {?MODULE, appointment_changed, #{}})]).

%%%===================================================================
%%% Swimlane
%%%===================================================================

-spec swim_basic() -> aihtml:html().
swim_basic() ->
    swimlane(order_nodes(), [legend],
             [{lanes, order_lanes()}, {phases, order_phases()}, {flows, order_flows()},
              {labels, #{corner => <<"泳道 / 阶段"/utf8>>, start => <<"起点"/utf8>>,
                         task => <<"任务"/utf8>>, decision => <<"判定"/utf8>>,
                         'end' => <<"终点"/utf8>>}},
              {selected, n3}]).

%% Dropping a node in another cell calls action(node_changed, ...) below.
-spec swim_edit() -> aihtml:html().
swim_edit() ->
    'div'([swimlane(order_nodes(), [editable],
                    [{lanes, order_lanes()}, {phases, order_phases()}, {flows, order_flows()},
                     on('ah:node-change', {?MODULE, node_changed, #{}})]),
           p(<<"拖动节点到别的格子，或选中后按 Shift+方向键。"/utf8>>,
             [<<"text-sm text-muted mt-2">>], [{id, <<"swim-log">>}])], [], []).

-spec swim_variants() -> aihtml:html().
swim_variants() ->
    swimlane([#{id => a, lane => dev, phase => plan, label => <<"Spec">>, type => start},
              #{id => b, lane => dev, phase => build, label => <<"Code">>, variant => outline},
              #{id => c, lane => qa, phase => build, label => <<"Test plan">>, dimmed => true},
              #{id => d, lane => qa, phase => ship, label => <<"Pass?">>, type => decision},
              #{id => e, lane => dev, phase => ship, label => <<"Release">>, type => 'end',
                color => teal}],
             [],
             [{lanes, [#{id => dev, name => <<"Development">>, color => indigo},
                       #{id => qa, name => <<"QA">>, color => pink}]},
              {phases, [#{id => plan, label => <<"Plan">>}, #{id => build, label => <<"Build">>},
                        #{id => ship, label => <<"Ship">>}]},
              {flows, [#{from => a, to => b}, #{from => b, to => d},
                       #{from => c, to => d, dashed => true, arrow => false},
                       #{from => d, to => e, label => <<"yes">>},
                       #{from => d, to => b, label => <<"no">>, dashed => true}]},
              {selected, d}, {lane_height, 96}, {phase_width, 170}]).

-spec swim_continuous() -> aihtml:html().
swim_continuous() ->
    swimlane([#{id => q1, lane => web, value => 3, label => <<"Beta">>, type => start},
              #{id => q2, lane => web, value => 20, label => <<"GA">>, type => 'end'},
              #{id => q3, lane => app, value => 8, label => <<"TestFlight">>},
              #{id => q4, lane => app, value => 26, label => <<"Store">>, type => 'end'}],
             [],
             [{lanes, [#{id => web, name => <<"Web">>, color => blue},
                       #{id => app, name => <<"Mobile">>, color => orange}]},
              {flows, [#{from => q1, to => q3}, #{from => q1, to => q2},
                       #{from => q3, to => q4}]},
              {axis, continuous}, {value_domain, {0, 30}}, {axis_width, 720},
              {value_ticks, [0, 10, 20, 30]}, {lane_height, 90},
              {labels, #{corner => <<"Days">>}}]).

%%%===================================================================
%%% Actions
%%%===================================================================

-spec action(atom(), term(), aihtml_action:event(), aihtml_action:ctx()) -> ok.
action(load_rooms, _Args, Event, Ctx) ->
    #{start := S, 'end' := E} = scheduler_range(Event),
    scheduler_update(Ctx, Event, rooms(week, appointments(S, E)));
action(load_timeline, _Args, Event, Ctx) ->
    #{start := S, 'end' := E} = scheduler_range(Event),
    scheduler_update(Ctx, Event, sched_with(sched_timeline(), appointments(S, E)));
action(load_month, _Args, Event, Ctx) ->
    #{start := S, 'end' := E} = scheduler_range(Event),
    scheduler_update(Ctx, Event, sched_with(sched_month(), appointments(S, E)));
action(appointment_changed, _Args, #{data := D}, Ctx) ->
    aihtml_action:html(Ctx, {id, <<"rooms-log">>},
                       [<<"服务端收到："/utf8>>, maps:get(<<"event">>, D), <<" → "/utf8>>,
                        maps:get(<<"from">>, D), <<" – "/utf8>>, maps:get(<<"to">>, D),
                        <<"，"/utf8>>, maps:get(<<"resource">>, D)]);
action(shown, _Args, #{value := V, data := D}, Ctx) ->
    aihtml_action:html(Ctx, {id, <<"sched-log">>},
                       [<<"服务端收到："/utf8>>, maps:get(<<"view">>, D), <<" "/utf8>>, V]);
action(task_changed, _Args, #{data := D}, Ctx) ->
    aihtml_action:html(Ctx, {id, <<"gantt-log">>},
                       [<<"服务端收到："/utf8>>, maps:get(<<"task">>, D), <<" "/utf8>>,
                        maps:get(<<"kind">>, D), <<" → "/utf8>>, maps:get(<<"from">>, D),
                        <<" – "/utf8>>, maps:get(<<"to">>, D), <<"，行 "/utf8>>,
                        maps:get(<<"row">>, D)]);
action(node_changed, _Args, #{data := D}, Ctx) ->
    aihtml_action:html(Ctx, {id, <<"swim-log">>},
                       [<<"服务端收到："/utf8>>, maps:get(<<"node">>, D), <<" → "/utf8>>,
                        maps:get(<<"lane">>, D), <<" / "/utf8>>, maps:get(<<"phase">>, D)]).

sched_with(#ah_scheduler{} = S, Events) -> S#ah_scheduler{items = Events}.

%%%===================================================================
%%% Data
%%%===================================================================

resources() ->
    [#{id => a, name => <<"会议室 A"/utf8>>, color => <<"#4285f4">>},
     #{id => b, name => <<"会议室 B"/utf8>>, color => <<"#ea4335">>},
     #{id => c, name => <<"会议室 C"/utf8>>, color => <<"#34a853">>}].

%% The appointments overlapping [Start, End): a stand-in for a query.
appointments(Start, End) ->
    All = [#{id => weekly, title => <<"团队周会"/utf8>>, start => <<"2026-01-05T09:00">>,
             'end' => <<"2026-01-05T10:00">>, resource => a, color => <<"#4285f4">>,
             rrule => <<"FREQ=WEEKLY;BYDAY=MO">>},
           #{id => standup, title => <<"站会"/utf8>>, start => <<"2026-01-06T09:30">>,
             'end' => <<"2026-01-06T09:45">>, resource => c, color => <<"#34a853">>,
             status => free, rrule => <<"FREQ=WEEKLY;BYDAY=TU,TH">>},
           #{id => review, title => <<"产品评审"/utf8>>, start => <<"2026-09-29T09:00">>,
             'end' => <<"2026-09-29T10:30">>, resource => a, color => <<"#4285f4">>},
           #{id => tech, title => <<"技术方案讨论"/utf8>>, start => <<"2026-09-29T10:00">>,
             'end' => <<"2026-09-29T11:00">>, resource => b, status => tentative,
             color => <<"#ea4335">>},
           #{id => demo, title => <<"客户演示"/utf8>>, start => <<"2026-09-29T14:00">>,
             'end' => <<"2026-09-29T15:30">>, resource => a, color => <<"#fbbc05">>},
           #{id => lunch, title => <<"午餐会"/utf8>>, start => <<"2026-09-30T12:00">>,
             'end' => <<"2026-09-30T13:00">>, resource => b, color => <<"#ea4335">>},
           #{id => oneone, title => <<"1:1 面谈"/utf8>>, start => <<"2026-09-30T11:00">>,
             'end' => <<"2026-09-30T11:30">>, resource => c, status => free,
             color => <<"#34a853">>},
           #{id => partner, title => <<"外部合作方会议"/utf8>>, start => <<"2026-10-01T14:00">>,
             'end' => <<"2026-10-01T16:00">>, resource => a, status => out_of_office,
             color => <<"#9c27b0">>},
           #{id => training, title => <<"全天培训"/utf8>>, start => <<"2026-10-01">>,
             'end' => <<"2026-10-03">>, resource => b, color => <<"#6366f1">>},
           #{id => retro, title => <<"项目复盘"/utf8>>, start => <<"2026-10-02T09:00">>,
             'end' => <<"2026-10-02T10:00">>, resource => c, color => <<"#ea4335">>},
           #{id => interview, title => <<"面试"/utf8>>, start => <<"2026-09-27T15:00">>,
             'end' => <<"2026-09-27T16:00">>, resource => b, color => <<"#ff7a00">>},
           #{id => offsite, title => <<"团建"/utf8>>, start => <<"2026-09-17">>,
             'end' => <<"2026-09-19">>, color => <<"#14b8a6">>}],
    [E || #{start := S} = E <- All,
          maps:is_key(rrule, E) orelse
              (S < End andalso day_of(maps:get('end', E)) >= Start)].

day_of(<<D:10/binary, _/binary>>) -> D.

project_rows() ->
    [#{id => design, label => <<"设计阶段"/utf8>>},
     #{id => wire, label => <<"线框图"/utf8>>, parent => design},
     #{id => mock, label => <<"视觉稿"/utf8>>, parent => design},
     #{id => dev, label => <<"开发阶段"/utf8>>},
     #{id => fe, label => <<"前端"/utf8>>, parent => dev},
     #{id => be, label => <<"后端"/utf8>>, parent => dev},
     #{id => testing, label => <<"测试阶段"/utf8>>},
     #{id => unit, label => <<"单元测试"/utf8>>, parent => testing},
     #{id => e2e, label => <<"端到端测试"/utf8>>, parent => testing},
     #{id => launch, label => <<"上线发布"/utf8>>}].

project_tasks() ->
    [#{id => t1, row => wire, name => <<"线框图"/utf8>>, start => <<"2026-09-18">>,
       'end' => <<"2026-09-23">>, progress => 100, color => <<"var(--ah-color-info)">>},
     #{id => t2, row => mock, name => <<"视觉稿"/utf8>>, start => <<"2026-09-23">>,
       'end' => <<"2026-09-29">>, progress => 80, dependencies => [t1],
       color => <<"var(--ah-color-info)">>},
     #{id => t3, row => fe, name => <<"前端开发"/utf8>>, start => <<"2026-09-29">>,
       'end' => <<"2026-10-12">>, progress => 10, dependencies => [t2]},
     #{id => t4, row => be, name => <<"后端开发"/utf8>>, start => <<"2026-09-28">>,
       'end' => <<"2026-10-14">>, progress => 5, dependencies => [t2]},
     #{id => t5, row => unit, name => <<"单元测试"/utf8>>, start => <<"2026-10-08">>,
       'end' => <<"2026-10-16">>, dependencies => [t3], color => <<"var(--ah-color-warning)">>},
     #{id => t6, row => e2e, name => <<"端到端测试"/utf8>>, start => <<"2026-10-14">>,
       'end' => <<"2026-10-20">>, dependencies => [t4], color => <<"var(--ah-color-warning)">>},
     #{id => t7, row => launch, name => <<"上线发布"/utf8>>, start => <<"2026-10-20">>,
       'end' => <<"2026-10-22">>, dependencies => [t5, t6],
       color => <<"var(--ah-color-success)">>}].

order_lanes() ->
    [#{id => customer, name => <<"Customer">>, color => blue},
     #{id => sales, name => <<"Sales">>, color => green},
     #{id => warehouse, name => <<"Warehouse">>, color => orange},
     #{id => finance, name => <<"Finance">>, color => purple}].

order_phases() ->
    [#{id => request, label => <<"Request">>}, #{id => review, label => <<"Review">>},
     #{id => fulfill, label => <<"Fulfill">>}, #{id => close, label => <<"Close">>}].

order_nodes() ->
    [#{id => n1, lane => customer, phase => request, label => <<"Place order">>, type => start},
     #{id => n2, lane => sales, phase => review, label => <<"Validate order">>},
     #{id => n3, lane => sales, phase => review, label => <<"In stock?">>, type => decision},
     #{id => n4, lane => warehouse, phase => fulfill, label => <<"Pick & pack">>},
     #{id => n5, lane => finance, phase => fulfill, label => <<"Issue invoice">>},
     #{id => n6, lane => customer, phase => close, label => <<"Receive goods">>, type => 'end'}].

order_flows() ->
    [#{from => n1, to => n2}, #{from => n2, to => n3},
     #{from => n3, to => n4, label => <<"yes">>}, #{from => n4, to => n5},
     #{from => n5, to => n6, dashed => true}].
