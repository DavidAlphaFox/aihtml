%% Element record of aihtml_timepicker (designs/05-records.md). Field
%% names follow the catalog: modifier groups, flags and options are
%% fields of the same name, with the catalog's defaults
%% (aihtml_timepicker_tests checks that they agree).
-ifndef(AIHTML_TIMEPICKER_HRL).
-define(AIHTML_TIMEPICKER_HRL, true).

-include("aihtml_element.hrl").

%% A clock-face time picker; postback fires on change.
%% `placeholder' undefined means "--:-- --" (12h) or "--:--" (24h).
-record(ah_timepicker, {?AH_BASE(aihtml_timepicker),
                        value = undefined :: aihtml_timepicker:value(),
                        name = undefined :: undefined | atom() | iodata(),
                        view = undefined :: undefined | portrait | landscape,
                        inline = false :: boolean(),
                        disabled = false :: boolean(),
                        clearable = false :: boolean(),
                        format = '12h' :: aihtml_timepicker:format(),
                        minute_step = 5 :: 1..30,
                        auto_switch = true :: boolean(),
                        min = undefined :: aihtml_timepicker:value(),
                        max = undefined :: aihtml_timepicker:value(),
                        placeholder = undefined :: undefined | iodata(),
                        footer = undefined :: aihtml_html:html()}).

-endif.
