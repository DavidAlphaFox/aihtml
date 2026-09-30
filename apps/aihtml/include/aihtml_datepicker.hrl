%% Element record of aihtml_datepicker (designs/05-records.md). Field
%% names follow the catalog: flags and options are fields of the same
%% name, with the catalog's defaults (aihtml_datepicker_tests checks
%% that they agree).
-ifndef(AIHTML_DATEPICKER_HRL).
-define(AIHTML_DATEPICKER_HRL, true).

-include("aihtml_element.hrl").

%% A read-only text field with a month grid popup; postback fires on change.
%% A pair value implies `range'. Without an `id' one is generated at render.
-record(ah_datepicker, {?AH_BASE(aihtml_datepicker),
                        value = undefined :: aihtml_datepicker:date_value(),
                        name = undefined :: undefined | atom() | iodata(),
                        disabled = false :: boolean(),
                        readonly = false :: boolean(),
                        range = false :: boolean(),
                        clearable = false :: boolean(),
                        inline = false :: boolean(),
                        placeholder = <<"Select date...">> :: undefined | unicode:chardata(),
                        format = <<"yyyy-MM-dd">> :: unicode:chardata(),
                        min = undefined :: aihtml_datepicker:date(),
                        max = undefined :: aihtml_datepicker:date(),
                        disabled_dates = [] :: [aihtml_datepicker:date()],
                        first_day :: 0..6 | undefined,   % undefined: the language's
                        week_numbers = false :: boolean(),
                        other_month_days = true :: boolean(),
                        weekends = false :: boolean(),
                        labels = #{} :: aihtml_datepicker:labels()}).

-endif.
