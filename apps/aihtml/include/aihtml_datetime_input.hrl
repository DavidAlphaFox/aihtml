%% Element record of aihtml_datetime_input (designs/05-records.md). Field
%% names follow the catalog: flags and options are fields of the same
%% name, with the catalog's defaults (aihtml_datetime_input_tests checks
%% that they agree).
-ifndef(AIHTML_DATETIME_INPUT_HRL).
-define(AIHTML_DATETIME_INPUT_HRL, true).

-include("aihtml_element.hrl").

%% A segmented date/time field: each part of the format is edited with
%% digits and arrow keys, with an optional drop-down month calendar;
%% postback fires on change. Without an `id' one is generated at render.
-record(ah_datetime_input, {?AH_BASE(aihtml_datetime_input),
                            value = undefined :: aihtml_datetime_input:value(),
                            disabled = false :: boolean(),
                            readonly = false :: boolean(),
                            spinner = false :: boolean(),
                            no_calendar = false :: boolean(),
                            show_time = false :: boolean(),
                            floating_label = false :: boolean(),
                            no_rounded = false :: boolean(),
                            placeholder = <<>> :: undefined | unicode:chardata(),
                            format = <<"yyyy-MM-dd">> :: unicode:chardata(),
                            min = undefined :: aihtml_datetime_input:value(),
                            max = undefined :: aihtml_datetime_input:value(),
                            first_day = 0 :: 0..6,
                            labels = #{} :: aihtml_datetime_input:labels(),
                            name = undefined :: undefined | atom() | iodata()}).

-endif.
