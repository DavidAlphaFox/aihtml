%% Element record of aihtml_calendar (designs/05-records.md). Field names
%% follow the catalog: flags and options are fields of the same name,
%% with the catalog's defaults (aihtml_calendar_tests checks that they
%% agree).
-ifndef(AIHTML_CALENDAR_HRL).
-define(AIHTML_CALENDAR_HRL, true).

-include("aihtml_element.hrl").

%% An event calendar with month, week, day and agenda views; navigation
%% runs in the browser. The value is the date the view shows; postback
%% fires on change (every navigation or view switch), whose action may
%% answer with set_events/3. Without an `id' one is generated at render.
-record(ah_calendar, {?AH_BASE(aihtml_calendar),
                      value = undefined :: aihtml_lib_date:date(),
                      editable = false :: boolean(),
                      selectable = false :: boolean(),
                      events = [] :: [aihtml_calendar:event()],
                      view = month :: aihtml_calendar:view(),
                      views = [month, week, day, list] :: [aihtml_calendar:view()],
                      first_day = 0 :: 0..6,
                      agenda_days = 30 :: pos_integer(),
                      day_max_events = 3 :: pos_integer(),
                      slot_duration = 30 :: pos_integer(),
                      slot_height = 20 :: pos_integer(),
                      height = 600 :: undefined | pos_integer(),
                      hour_format = 12 :: 12 | 24,
                      labels = #{} :: aihtml_calendar:labels(),
                      name = undefined :: undefined | atom() | iodata()}).

-endif.
