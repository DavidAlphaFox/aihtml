%% @doc Demos of the resource scheduler (aihtml_scheduler), shown on
%% /components/scheduler. Each function is one example, written the way
%% an application writes it; the docs page prints its source under it.
%%
%% The module is also the action module of the demos that talk to the
%% server: the scheduler loads every range it navigates to from
%% appointments/2 (standing in for a database), and the edit demos
%% report what the postback received.
-module(aihtml_example_demo_scheduler).
-behaviour(aihtml_action).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0, action/4]).
-export([sched_week/0, sched_timeline/0, sched_month/0, sched_agenda/0, sched_locale/0,
         sched_record/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => scheduler, title => <<"Scheduler">>,
       summary => <<"资源调度：日/周/月/日程/时间线视图，翻页时由服务端加载并渲染新的日期范围。"/utf8>>,
       demos => [{<<"周视图、资源列、服务端翻页"/utf8>>, sched_week},
                 {<<"时间线视图"/utf8>>, sched_timeline},
                 {<<"月视图与“更多”弹层"/utf8>>, sched_month},
                 {<<"日程列表、24 小时制"/utf8>>, sched_agenda},
                 {<<"中文标签、工作时间"/utf8>>, sched_locale},
                 {<<"record 写法"/utf8>>, sched_record}]}].

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
                       [<<"服务端收到："/utf8>>, maps:get(<<"view">>, D), <<" "/utf8>>, V]).

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
