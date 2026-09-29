%% Element records of aihtml_form_pickers (designs/05-records.md). Field
%% names follow the catalog: flags and options are fields of the same
%% name, with the catalog's defaults (aihtml_form_pickers_tests checks
%% that they agree).
-ifndef(AIHTML_FORM_PICKERS_HRL).
-define(AIHTML_FORM_PICKERS_HRL, true).

-include("aihtml_element.hrl").

%% An ISO date (<<"2026-09-29">> or "2026-09-29"), a calendar:date() or
%% undefined.
-type ah_picker_date() :: binary() | string() | calendar:date() | undefined.
%% One date, or a range {From, To}.
-type ah_picker_date_value() :: ah_picker_date() | {ah_picker_date(), ah_picker_date()}.
-type ah_picker_label_key() :: months | months_short | weekdays | title | today | clear
                             | prev_month | next_month | prev_year | next_year.
%% Texts of the calendar; `months', `months_short' (12) and `weekdays'
%% (7, from Sunday) are lists, `title' is a display format.
-type ah_picker_labels() :: #{ah_picker_label_key() => unicode:chardata()
                                                      | [unicode:chardata()]}.
%% A combobox item: a text that is both value and label, `{Value, Label}',
%% or a map with `value' and optionally `label', `description', `group'
%% and `disabled'.
-type ah_picker_item() :: binary() | atom() | integer() | {term(), term()}
                        | #{value := term(), label => term(), description => term(),
                            group => term(), disabled => boolean()}.
-type ah_picker_search_mode() :: contains_ignore_case | contains | starts_with_ignore_case
                               | starts_with | equals_ignore_case | equals | none.

%% A read-only text field with a month grid popup; postback fires on change.
%% A pair value implies `range'. Without an `id' one is generated at render.
-record(ah_datepicker, {?AH_BASE(aihtml_form_pickers),
                        value = undefined :: ah_picker_date_value(),
                        name = undefined :: undefined | atom() | iodata(),
                        disabled = false :: boolean(),
                        readonly = false :: boolean(),
                        range = false :: boolean(),
                        clearable = false :: boolean(),
                        inline = false :: boolean(),
                        placeholder = <<"Select date...">> :: undefined | unicode:chardata(),
                        format = <<"yyyy-MM-dd">> :: unicode:chardata(),
                        min = undefined :: ah_picker_date(),
                        max = undefined :: ah_picker_date(),
                        disabled_dates = [] :: [ah_picker_date()],
                        first_day = 0 :: 0..6,
                        week_numbers = false :: boolean(),
                        other_month_days = true :: boolean(),
                        weekends = false :: boolean(),
                        labels = #{} :: ah_picker_labels()}).

%% An editable field with a filtered list; postback fires on change.
%% `value' is a list of values with `multiple' or `checkboxes'. `search'
%% is an action ref run (debounced) as the user types. Without an `id'
%% one is generated at render.
-record(ah_combobox, {?AH_BASE(aihtml_form_pickers),
                      items = [] :: [ah_picker_item()],
                      value = undefined :: term(),
                      name = undefined :: undefined | atom() | iodata(),
                      disabled = false :: boolean(),
                      no_arrow = false :: boolean(),
                      multiple = false :: boolean(),
                      checkboxes = false :: boolean(),
                      free_text = false :: boolean(),
                      placeholder = <<>> :: undefined | unicode:chardata(),
                      search_mode = contains_ignore_case :: ah_picker_search_mode(),
                      min_length = undefined :: undefined | non_neg_integer(),
                      empty_text = undefined :: undefined | unicode:chardata(),
                      dropdown_height = undefined :: undefined | pos_integer(),
                      search = undefined :: undefined | aihtml_action:ref()}).

-endif.
