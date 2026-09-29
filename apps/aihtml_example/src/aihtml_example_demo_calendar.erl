%% @doc Demos of the event calendar (aihtml_calendar), shown on
%% /components/calendar. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
%%
%% The module is also the action module of the demos that talk to the
%% server: a calendar that loads each visible range and creates events
%% from a selection.
-module(aihtml_example_demo_calendar).
-behaviour(aihtml_action).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0, action/4]).
-export([cal_month/0, cal_week/0, cal_recurring/0, cal_agenda/0, cal_locale/0,
         cal_server/0, cal_record/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => calendar, title => <<"Calendar">>,
       summary => <<"带月、周、日和日程视图的事件日历，支持循环事件、跨天事件和拖拽编辑。"/utf8>>,
       demos => [{<<"月视图：单次、全天、跨天与重叠事件"/utf8>>, cal_month},
                 {<<"周视图：拖动、调整时长、拖选新建"/utf8>>, cal_week},
                 {<<"循环事件"/utf8>>, cal_recurring},
                 {<<"日程视图与事件状态"/utf8>>, cal_agenda},
                 {<<"中文标签、周一开头、24 小时制"/utf8>>, cal_locale},
                 {<<"按可见范围从服务端加载，选区新建事件"/utf8>>, cal_server},
                 {<<"record 写法"/utf8>>, cal_record}]}].

%%%===================================================================
%%% Calendar
%%%===================================================================

%% The toolbar entries are also links (href): opened directly,
%% /components/calendar/state renders the date and view of its query.
-spec cal_month() -> aihtml:html().
cal_month() ->
    View = aihtml_example_state:param(<<"cal_view">>, <<"month">>),
    calendar(aihtml_example_state:param(<<"cal_date">>, <<"2026-09-15">>), [],
             [{height, 560},
              {view, hd([V || V <- [month, week, day, list], atom_to_binary(V) =:= View]
                        ++ [month])},
              {href, <<"/components/calendar/state?cal_date={date}&cal_view={view}">>},
              {events, [#{title => <<"Kick-off">>, start => <<"2026-09-01T10:00">>,
                          'end' => <<"2026-09-01T11:30">>},
                        #{title => <<"Design review">>, start => <<"2026-09-15T14:00">>,
                          'end' => <<"2026-09-15T15:00">>, color => <<"#7c3aed">>},
                        #{title => <<"1:1">>, start => <<"2026-09-15T16:00">>},
                        #{title => <<"Deploy">>, start => <<"2026-09-15T18:00">>,
                          color => <<"#0891b2">>},
                        #{title => <<"Team offsite">>, start => <<"2026-09-09">>,
                          'end' => <<"2026-09-12">>, color => <<"#16a34a">>},
                        #{title => <<"Conference">>, start => <<"2026-09-19">>,
                          'end' => <<"2026-09-23">>, color => <<"#ea580c">>},
                        #{title => <<"Holiday">>, start => {2026, 9, 28}, all_day => true,
                          color => <<"#e11d48">>}]}]).

-spec cal_week() -> aihtml:html().
cal_week() ->
    calendar(<<"2026-09-16">>, [editable, selectable],
             [{view, week}, {height, 520},
              {events, [#{id => 1, title => <<"Standup">>, start => <<"2026-09-14T09:00">>,
                          'end' => <<"2026-09-14T09:30">>},
                        #{id => 2, title => <<"Planning">>, start => <<"2026-09-15T10:00">>,
                          'end' => <<"2026-09-15T12:00">>, color => <<"#7c3aed">>},
                        #{id => 3, title => <<"Pairing">>, start => <<"2026-09-15T11:00">>,
                          'end' => <<"2026-09-15T12:30">>, color => <<"#0891b2">>},
                        #{id => 4, title => <<"Lunch">>, start => <<"2026-09-16T12:00">>,
                          color => <<"#16a34a">>},
                        #{id => 5, title => <<"Release">>, start => <<"2026-09-17">>,
                          color => <<"#ea580c">>}]}]).

-spec cal_recurring() -> aihtml:html().
cal_recurring() ->
    calendar(<<"2026-09-14">>, [],
             [{view, week}, {height, 480},
              {events, [#{title => <<"Daily standup">>, start => <<"2026-09-01T09:00">>,
                          'end' => <<"2026-09-01T09:15">>, rrule => <<"FREQ=DAILY;COUNT=20">>},
                        #{title => <<"Team sync">>, start => <<"2026-09-07T14:00">>,
                          'end' => <<"2026-09-07T15:00">>, color => <<"#7c3aed">>,
                          rrule => <<"FREQ=WEEKLY;BYDAY=MO,WE,FR">>},
                        #{title => <<"Sprint review">>, start => <<"2026-09-04T16:00">>,
                          'end' => <<"2026-09-04T17:00">>, color => <<"#0891b2">>,
                          rrule => <<"FREQ=WEEKLY;INTERVAL=1;BYDAY=FR">>,
                          exdates => [<<"2026-09-18">>]},
                        #{title => <<"Monthly report">>, start => <<"2026-09-15">>,
                          color => <<"#ea580c">>, rrule => <<"FREQ=MONTHLY">>}]}]).

-spec cal_agenda() -> aihtml:html().
cal_agenda() ->
    calendar(<<"2026-09-14">>, [],
             [{view, list}, {agenda_days, 7}, {height, 420},
              {events, [#{title => <<"Client call">>, start => <<"2026-09-14T10:00">>,
                          status => confirmed},
                        #{title => <<"Budget meeting">>, start => <<"2026-09-15T15:00">>,
                          'end' => <<"2026-09-15T16:30">>, status => tentative},
                        #{title => <<"Workshop">>, start => <<"2026-09-16T09:00">>,
                          status => cancelled, color => <<"#94a3b8">>},
                        #{title => <<"Yoga">>, start => <<"2026-09-14T18:00">>,
                          color => <<"#16a34a">>, rrule => <<"FREQ=WEEKLY;BYDAY=MO,TH">>},
                        #{title => <<"Company day">>, start => <<"2026-09-18">>,
                          color => <<"#ea580c">>}]}]).

-spec cal_locale() -> aihtml:html().
cal_locale() ->
    calendar(<<"2026-09-29">>, [],
             [{view, week}, {first_day, 1}, {hour_format, 24}, {height, 480},
              {views, [month, week, list]},
              {labels, #{today => <<"今天"/utf8>>, prev => <<"上一页"/utf8>>,
                         next => <<"下一页"/utf8>>, month => <<"月"/utf8>>,
                         week => <<"周"/utf8>>, list => <<"日程"/utf8>>,
                         all_day => <<"全天"/utf8>>, all_day_short => <<"全天"/utf8>>,
                         more => <<"还有 {n} 项"/utf8>>, no_events => <<"这段时间没有事件"/utf8>>,
                         no_events_hint => <<"换个日期范围看看"/utf8>>,
                         weekdays => [<<"星期日"/utf8>>, <<"星期一"/utf8>>, <<"星期二"/utf8>>,
                                      <<"星期三"/utf8>>, <<"星期四"/utf8>>, <<"星期五"/utf8>>,
                                      <<"星期六"/utf8>>],
                         weekdays_short => [<<"日"/utf8>>, <<"一"/utf8>>, <<"二"/utf8>>,
                                            <<"三"/utf8>>, <<"四"/utf8>>, <<"五"/utf8>>,
                                            <<"六"/utf8>>],
                         title_month => <<"yyyy年M月"/utf8>>,
                         title_day => <<"yyyy年M月d日 EEEE"/utf8>>,
                         range_start => <<"M月d日"/utf8>>, range_end => <<"M月d日"/utf8>>,
                         list_date => <<"yyyy年M月d日"/utf8>>}},
              {events, [#{title => <<"周会"/utf8>>, start => <<"2026-09-28T10:00">>,
                          'end' => <<"2026-09-28T11:00">>},
                        #{title => <<"国庆假期"/utf8>>, start => <<"2026-10-01">>,
                          'end' => <<"2026-10-08">>, color => <<"#e11d48">>},
                        #{title => <<"代码评审"/utf8>>, start => <<"2026-09-30T15:00">>,
                          'end' => <<"2026-09-30T16:30">>, color => <<"#7c3aed">>}]}]).

-spec cal_server() -> aihtml:html().
cal_server() ->
    'div'([calendar(<<"2026-09-29">>, [selectable],
                    [{id, <<"cal-server">>}, {height, 460},
                     {events, server_events(<<"2026-08-30">>, <<"2026-10-04">>)},
                     on(change, {?MODULE, cal_load, #{}}),
                     on('ah:select', {?MODULE, cal_create, #{}}),
                     on('ah:event-click', {?MODULE, cal_clicked, #{}})]),
           p(<<"翻页或切换视图时服务端按可见范围返回事件；拖选日期新建事件。"/utf8>>,
             [<<"text-sm text-muted mt-2">>], [{id, <<"cal-server-log">>}])],
          [], []).

%% The same component as a record: options are checked field names, and
%% the postback runs action(cal_load, ...) below on every navigation.
-spec cal_record() -> aihtml:html().
cal_record() ->
    #ah_calendar{id = <<"cal-record">>, value = <<"2026-09-29">>, view = day,
                 views = [day, week], slot_duration = 60, slot_height = 32, height = 420,
                 editable = true,
                 events = server_events(<<"2026-09-27">>, <<"2026-10-04">>),
                 postback = cal_load}.

%%%===================================================================
%%% Actions
%%%===================================================================

-spec action(atom(), term(), aihtml_action:event(), aihtml_action:ctx()) -> ok.
action(cal_load, _Args, #{data := #{<<"start">> := From, <<"end">> := To} = Data} = Event, Ctx) ->
    set_events(Ctx, Event, server_events(From, To)),
    aihtml_action:html(Ctx, {id, <<"cal-server-log">>},
                       [<<"服务端返回了 "/utf8>>, maps:get(<<"view">>, Data), <<" "/utf8>>,
                        From, <<" ~ "/utf8>>, To, <<" 的事件"/utf8>>]);
action(cal_create, _Args, #{data := #{<<"from">> := From, <<"to">> := To}} = Event, Ctx) ->
    add_event(Ctx, Event, #{id => <<"new-", From/binary>>, title => <<"新事件"/utf8>>,
                            start => From, 'end' => To, color => <<"#16a34a">>}),
    aihtml_action:html(Ctx, {id, <<"cal-server-log">>},
                       [<<"服务端新建了事件："/utf8>>, From, <<" ~ "/utf8>>, To]);
action(cal_clicked, _Args, #{data := #{<<"event">> := Id}}, Ctx) ->
    aihtml_action:html(Ctx, {id, <<"cal-server-log">>}, [<<"点击了事件 "/utf8>>, Id]).

%%%===================================================================
%%% Data
%%%===================================================================

%% Made-up events for [From, To): a standup on weekdays, a review on
%% Thursdays, as the server would read them from a database.
server_events(From, To) ->
    F = calendar:date_to_gregorian_days(iso(From)),
    T = calendar:date_to_gregorian_days(iso(To)),
    lists:append(
      [begin
           Date = calendar:gregorian_days_to_date(D),
           Iso = iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0B", tuple_to_list(Date))),
           case calendar:day_of_the_week(Date) of
               4 -> [#{id => <<"review-", Iso/binary>>, title => <<"Review">>,
                       start => <<Iso/binary, "T15:00">>, 'end' => <<Iso/binary, "T16:30">>,
                       color => <<"#7c3aed">>}];
               W when W < 6 -> [#{id => <<"standup-", Iso/binary>>, title => <<"Standup">>,
                                  start => <<Iso/binary, "T09:30">>,
                                  'end' => <<Iso/binary, "T09:45">>}];
               _ -> []
           end
       end || D <- lists:seq(F, T - 1)]).

iso(<<Y:4/binary, "-", M:2/binary, "-", D:2/binary>>) ->
    {binary_to_integer(Y), binary_to_integer(M), binary_to_integer(D)}.

