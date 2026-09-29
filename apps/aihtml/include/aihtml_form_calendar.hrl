%% Element records of aihtml_form_calendar (designs/05-records.md). Field
%% names follow the catalog: flags and options are fields of the same
%% name, with the catalog's defaults (aihtml_form_calendar_tests checks
%% that they agree).
-ifndef(AIHTML_FORM_CALENDAR_HRL).
-define(AIHTML_FORM_CALENDAR_HRL, true).

-include("aihtml_element.hrl").

%% A day: an ISO date (<<"2026-09-29">> or "2026-09-29"), a
%% calendar:date() or undefined.
-type ah_cal_date() :: binary() | string() | calendar:date() | undefined.
%% A point in time, local and without a zone: an ISO date (a whole day),
%% an ISO date-time (<<"2026-09-29T14:30">>, seconds are dropped), a
%% calendar:date() or a calendar:datetime().
-type ah_cal_time() :: binary() | string() | calendar:date() | calendar:datetime().
-type ah_cal_status() :: confirmed | tentative | cancelled | binary().
%% A calendar event. `start' is required; `end' defaults to one day (all
%% day events) or one hour later; a start without a time, or `all_day',
%% makes an all day event. `rrule' is an iCalendar RRULE subset (FREQ,
%% INTERVAL, COUNT, UNTIL, BYDAY, BYMONTHDAY, BYMONTH), `exdates' the days
%% left out of the series, `status' colours the agenda row.
-type ah_cal_event() :: #{id => term(), title => unicode:chardata(),
                          start := ah_cal_time(), 'end' => ah_cal_time(),
                          all_day => boolean(), color => unicode:chardata(),
                          rrule => unicode:chardata(), exdates => [ah_cal_date()],
                          status => ah_cal_status()}.
-type ah_cal_view() :: month | week | day | list.
-type ah_cal_label_key() :: today | prev | next | month | week | day | list
                          | all_day | all_day_short | more | no_events | no_events_hint
                          | am | pm | months | months_short | weekdays | weekdays_short
                          | title_month | title_day | range_start | range_end | list_date.
%% Texts of the calendar. `months', `months_short' (12), `weekdays' and
%% `weekdays_short' (7, from Sunday) are lists; `more' holds {n}; the
%% title_*, range_* and list_date keys are display formats (yyyy MMMM
%% MMM MM M dd d EEEE EEE).
-type ah_cal_labels() :: #{ah_cal_label_key() => unicode:chardata() | [unicode:chardata()]}.

%% The value of a datetime_input: an ISO date, date-time or time
%% (<<"2026-09-29">>, <<"2026-09-29T14:30">>, <<"14:30">>), a
%% calendar:date(), a calendar:datetime() or undefined.
-type ah_dti_value() :: binary() | string() | calendar:date() | calendar:datetime()
                      | undefined.
-type ah_dti_label_key() :: months | weekdays | title | time | prev_month | next_month.
%% Texts of the drop-down calendar: `months' (12) and `weekdays' (7, from
%% Sunday) are lists, `title' is a display format (yyyy MMMM MM M).
-type ah_dti_labels() :: #{ah_dti_label_key() => unicode:chardata() | [unicode:chardata()]}.

%% An event calendar with month, week, day and agenda views; navigation
%% runs in the browser. The value is the date the view shows; postback
%% fires on change (every navigation or view switch), whose action may
%% answer with set_events/3. Without an `id' one is generated at render.
-record(ah_calendar, {?AH_BASE(aihtml_form_calendar),
                      value = undefined :: ah_cal_date(),
                      editable = false :: boolean(),
                      selectable = false :: boolean(),
                      events = [] :: [ah_cal_event()],
                      view = month :: ah_cal_view(),
                      views = [month, week, day, list] :: [ah_cal_view()],
                      first_day = 0 :: 0..6,
                      agenda_days = 30 :: pos_integer(),
                      day_max_events = 3 :: pos_integer(),
                      slot_duration = 30 :: pos_integer(),
                      slot_height = 20 :: pos_integer(),
                      height = 600 :: undefined | pos_integer(),
                      hour_format = 12 :: 12 | 24,
                      labels = #{} :: ah_cal_labels(),
                      name = undefined :: undefined | atom() | iodata()}).

%% A segmented date/time field: each part of the format is edited with
%% digits and arrow keys, with an optional drop-down month calendar;
%% postback fires on change. Without an `id' one is generated at render.
-record(ah_datetime_input, {?AH_BASE(aihtml_form_calendar),
                            value = undefined :: ah_dti_value(),
                            disabled = false :: boolean(),
                            readonly = false :: boolean(),
                            spinner = false :: boolean(),
                            no_calendar = false :: boolean(),
                            show_time = false :: boolean(),
                            floating_label = false :: boolean(),
                            no_rounded = false :: boolean(),
                            placeholder = <<>> :: undefined | unicode:chardata(),
                            format = <<"yyyy-MM-dd">> :: unicode:chardata(),
                            min = undefined :: ah_dti_value(),
                            max = undefined :: ah_dti_value(),
                            first_day = 0 :: 0..6,
                            labels = #{} :: ah_dti_labels(),
                            name = undefined :: undefined | atom() | iodata()}).

-endif.
