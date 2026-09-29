%% Element record of aihtml_scheduler (designs/05-records.md). Field
%% names follow the catalog: flags and options are fields of the same
%% name, with the catalog's defaults (aihtml_scheduler_tests checks that
%% they agree).
-ifndef(AIHTML_SCHEDULER_HRL).
-define(AIHTML_SCHEDULER_HRL, true).

-include("aihtml_element.hrl").

%% A resource scheduler with day, week, month, agenda and timeline views,
%% all rendered on the server. The value is the date shown; navigation
%% fires change (the postback) with the new date in Event.value and the
%% view and range in Event.data, and `source' loads the new range
%% (answer with scheduler_update/3). Edits fire ah:event-change.
%% Without an `id' one is generated at render.
-record(ah_scheduler, {?AH_BASE(aihtml_scheduler),
                       items = [] :: [aihtml_scheduler:event()],
                       value = undefined :: aihtml_lib_date:date(),
                       editable = false :: boolean(),
                       no_all_day = false :: boolean(),
                       view = week :: aihtml_scheduler:view(),
                       views = [day, week, month, agenda] :: [aihtml_scheduler:view()],
                       resources = [] :: [aihtml_scheduler:resource()],
                       first_day = 1 :: 0..6,
                       slot_duration = 30 :: pos_integer(),
                       slot_height = 20 :: pos_integer(),
                       day_start = 0 :: 0..23,
                       day_end = 24 :: 1..24,
                       height = 600 :: undefined | pos_integer(),
                       agenda_days = 30 :: pos_integer(),
                       day_max_events = 3 :: pos_integer(),
                       hour_format = 12 :: 12 | 24,
                       today = undefined :: aihtml_lib_date:date(),
                       toolbar = true :: boolean(),
                       source = undefined :: undefined | aihtml_action:ref(),
                       labels = #{} :: aihtml_scheduler:labels(),
                       name = undefined :: undefined | atom() | iodata()}).

-endif.
