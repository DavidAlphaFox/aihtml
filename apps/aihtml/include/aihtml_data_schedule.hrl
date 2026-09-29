%% Element records of aihtml_data_schedule (designs/05-records.md). Field
%% names follow the catalog: flags and options are fields of the same
%% name, with the catalog's defaults (aihtml_data_schedule_tests checks
%% that they agree).
-ifndef(AIHTML_DATA_SCHEDULE_HRL).
-define(AIHTML_DATA_SCHEDULE_HRL, true).

-include("aihtml_element.hrl").

%% A day: an ISO date (<<"2026-09-29">> or "2026-09-29"), a
%% calendar:date() or undefined.
-type ah_sch_date() :: binary() | string() | calendar:date() | undefined.
%% A point in time, local and without a zone: an ISO date (midnight), an
%% ISO date-time (<<"2026-09-29T14:30">>, seconds are dropped), a
%% calendar:date() or a calendar:datetime().
-type ah_sch_time() :: binary() | string() | calendar:date() | calendar:datetime().

%% A gantt task. `end' is exclusive (a task from 2026-01-05 to 2026-01-12
%% lasts 7 days); `row' names its row when `rows' is given;
%% `dependencies' are ids of tasks that must finish first.
-type ah_gantt_task() :: #{id := term(), name => unicode:chardata(),
                           start := ah_sch_time(), 'end' := ah_sch_time(),
                           row => term(), progress => number(),
                           color => unicode:chardata(),
                           dependencies => [term()] | term()}.
%% A gantt row (sidebar line); rows with a `parent' nest under it.
-type ah_gantt_row() :: #{id := term(), label => unicode:chardata(), parent => term()}.
-type ah_gantt_label_key() :: task | tasks | months_short.
%% Texts of the gantt: `task' (sidebar title), `tasks' (summary bar
%% label, "{n}" is the count), `months_short' (12).
-type ah_gantt_labels() :: #{ah_gantt_label_key() => unicode:chardata() | [unicode:chardata()]}.

-type ah_sch_status() :: free | busy | tentative | out_of_office.
%% A scheduler appointment. `start' is required; `end' defaults to one day
%% (all day) or one hour later; a start and end at midnight, or
%% `all_day', make an all day appointment. `resource' is a resource id.
%% `rrule' is an iCalendar RRULE subset (FREQ, INTERVAL, COUNT, UNTIL,
%% BYDAY, BYMONTHDAY, BYMONTH), `exdates' the days left out of the series.
-type ah_sch_event() :: #{id => term(), title => unicode:chardata(),
                          start := ah_sch_time(), 'end' => ah_sch_time(),
                          all_day => boolean(), resource => term(),
                          status => ah_sch_status(), color => unicode:chardata(),
                          rrule => unicode:chardata(), exdates => [ah_sch_date()]}.
%% A scheduler resource (room, person): a column in the day and week
%% views, a row in the timeline views.
-type ah_sch_resource() :: #{id := term(), name => unicode:chardata(),
                             color => unicode:chardata()}.
-type ah_sch_view() :: day | week | month | agenda
                     | timeline_day | timeline_week | timeline_month.
-type ah_sch_label_key() :: today | prev | next | day | week | month | agenda
                          | timeline_day | timeline_week | timeline_month
                          | all_day | all_day_short | more | no_events | hint_navigate
                          | edit | delete | copy | new | am | pm
                          | months | months_short | weekdays | weekdays_short
                          | title_day | title_month | range_start | range_end
                          | agenda_date | popover_date.
%% Texts of the scheduler. `months', `months_short' (12), `weekdays' and
%% `weekdays_short' (7, from Sunday) are lists; `more' holds {n}; the
%% title_*, range_*, agenda_date and popover_date keys are display
%% formats (yyyy MMMM MMM MM M dd d EEEE EEE).
-type ah_sch_labels() :: #{ah_sch_label_key() => unicode:chardata() | [unicode:chardata()]}.

%% A swimlane lane (row, role) and phase (column, stage).
-type ah_swim_lane() :: #{id := term(), name => unicode:chardata(),
                          color => atom() | unicode:chardata()}.
-type ah_swim_phase() :: #{id := term(), label => unicode:chardata()}.
%% A swimlane node, placed in the cell of its lane and phase (or, on a
%% continuous axis, at its `value'). Nodes of one cell stack vertically.
%% `color' is a CSS colour or one of default, blue, green, red, orange,
%% purple, teal, pink, indigo, yellow; it defaults to the lane's.
-type ah_swim_node() :: #{id := term(), lane := term(), phase => term(),
                          label => unicode:chardata(),
                          type => start | task | decision | 'end',
                          color => atom() | unicode:chardata(),
                          variant => solid | outline, dimmed => boolean(),
                          value => number()}.
%% A connection between two nodes, drawn as an orthogonal line.
-type ah_swim_flow() :: #{from := term(), to := term(), label => unicode:chardata(),
                          dashed => boolean(), arrow => boolean()}.
-type ah_swim_label_key() :: corner | start | task | decision | 'end'.
%% Texts of the swimlane: the corner cell and the legend entries.
-type ah_swim_labels() :: #{ah_swim_label_key() => unicode:chardata()}.

%% A gantt chart: rows in a sidebar, task bars on a day scale, dependency
%% lines, today marker. With `editable', bars are dragged (move, resize)
%% and the postback fires on ah:task-change (Event.data: task, from, to,
%% row, kind, days). Without an `id' one is generated at render.
-record(ah_gantt, {?AH_BASE(aihtml_data_schedule),
                   items = [] :: [ah_gantt_task()],
                   editable = false :: boolean(),
                   no_dependencies = false :: boolean(),
                   rows = undefined :: undefined | [ah_gantt_row()],
                   collapsed = [] :: [term()],
                   height = 500 :: undefined | pos_integer(),
                   sidebar_width = 250 :: pos_integer(),
                   column_width = 60 :: pos_integer(),
                   row_height = 40 :: pos_integer(),
                   today = undefined :: ah_sch_date(),
                   labels = #{} :: ah_gantt_labels()}).

%% A resource scheduler with day, week, month, agenda and timeline views,
%% all rendered on the server. The value is the date shown; navigation
%% fires change (the postback) with the new date in Event.value and the
%% view and range in Event.data, and `source' loads the new range
%% (answer with scheduler_update/3). Edits fire ah:event-change.
%% Without an `id' one is generated at render.
-record(ah_scheduler, {?AH_BASE(aihtml_data_schedule),
                       items = [] :: [ah_sch_event()],
                       value = undefined :: ah_sch_date(),
                       editable = false :: boolean(),
                       no_all_day = false :: boolean(),
                       view = week :: ah_sch_view(),
                       views = [day, week, month, agenda] :: [ah_sch_view()],
                       resources = [] :: [ah_sch_resource()],
                       first_day = 1 :: 0..6,
                       slot_duration = 30 :: pos_integer(),
                       slot_height = 20 :: pos_integer(),
                       day_start = 0 :: 0..23,
                       day_end = 24 :: 1..24,
                       height = 600 :: undefined | pos_integer(),
                       agenda_days = 30 :: pos_integer(),
                       day_max_events = 3 :: pos_integer(),
                       hour_format = 12 :: 12 | 24,
                       today = undefined :: ah_sch_date(),
                       toolbar = true :: boolean(),
                       source = undefined :: undefined | aihtml_action:ref(),
                       labels = #{} :: ah_sch_labels(),
                       name = undefined :: undefined | atom() | iodata()}).

%% A swimlane (cross-functional flow chart): lanes by phases, nodes in
%% the cells, flows as orthogonal lines. Clicking a node selects it and
%% highlights its flows (ah:select); with `editable' nodes are dragged to
%% another cell and the postback fires on ah:node-change (Event.data:
%% node, lane, phase, oldLane, oldPhase). Without an `id' one is
%% generated at render.
-record(ah_swimlane, {?AH_BASE(aihtml_data_schedule),
                      items = [] :: [ah_swim_node()],
                      editable = false :: boolean(),
                      legend = false :: boolean(),
                      lanes = [] :: [ah_swim_lane()],
                      phases = [] :: [ah_swim_phase()],
                      flows = [] :: [ah_swim_flow()],
                      selected = undefined :: term(),
                      axis = discrete :: discrete | continuous,
                      value_domain = undefined :: undefined | {number(), number()},
                      value_ticks = undefined :: undefined | [number()],
                      axis_width = undefined :: undefined | pos_integer(),
                      lane_height = 110 :: pos_integer(),
                      phase_width = 190 :: pos_integer(),
                      node_width = 132 :: pos_integer(),
                      node_height = 52 :: pos_integer(),
                      lane_label_width = 150 :: pos_integer(),
                      height = undefined :: undefined | pos_integer(),
                      labels = #{} :: ah_swim_labels()}).

-endif.
