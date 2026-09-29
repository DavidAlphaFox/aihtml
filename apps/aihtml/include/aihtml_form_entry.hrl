%% Element records of aihtml_form_entry (designs/05-records.md). Field
%% names follow the catalog: modifier groups, flags and options are fields
%% of the same name, with the catalog's defaults (aihtml_form_entry_tests
%% checks that they agree).
-ifndef(AIHTML_FORM_ENTRY_HRL).
-define(AIHTML_FORM_ENTRY_HRL, true).

-include("aihtml_element.hrl").

%% Radix of a formatted_input: binary, octal, decimal, hexadecimal.
-type ah_entry_radix() :: 2 | 8 | 10 | 16.
%% An integer, or its decimal text (<<"-42">>, "12345678901234567890").
-type ah_entry_integer() :: integer() | binary() | string().
%% How a range_selector writes a value: `number' (integers as is, others
%% with 2 decimals), `{fixed, Decimals}', `currency' ($1,234), `date'
%% (M/D/YYYY), `month' (Jan), `time' (4:00 PM) - the last three read the
%% value as a UTC timestamp in milliseconds - or `{Prefix, Format, Suffix}'.
-type ah_entry_format() :: number | {fixed, 0..20} | currency | date | month | time
                         | {unicode:chardata(), ah_entry_format(), unicode:chardata()}.
-type ah_entry_btn_variant() :: primary | secondary | outlined | success | warning | error
                          | info | default | borderless.

%% A text field with an input mask such as "(999) 999-9999", enforced in
%% the browser; postback fires on change (blur after an edit). `value'
%% holds the typed characters without the literals (with them when
%% `include_literals' is set).
-record(ah_masked_input, {?AH_BASE(aihtml_form_entry),
                          value = undefined :: undefined | unicode:chardata(),
                          name = undefined :: undefined | atom() | iodata(),
                          disabled = false :: boolean(),
                          readonly = false :: boolean(),
                          square = false :: boolean(),
                          floating_label = false :: boolean(),
                          mask = <<"99999">> :: unicode:chardata(),
                          prompt_char = <<"_">> :: unicode:chardata(),
                          placeholder = <<>> :: unicode:chardata(),
                          include_literals = false :: boolean()}).

%% An integer field in binary, octal, decimal or hexadecimal, with spin
%% buttons and a radix menu; postback fires on change. `value' is the
%% decimal value (any size).
-record(ah_formatted_input, {?AH_BASE(aihtml_form_entry),
                             value = 0 :: ah_entry_integer(),
                             name = undefined :: undefined | atom() | iodata(),
                             disabled = false :: boolean(),
                             radix = 10 :: ah_entry_radix(),
                             min = undefined :: undefined | ah_entry_integer(),
                             max = undefined :: undefined | ah_entry_integer(),
                             upper_case = false :: boolean(),
                             spin_buttons = true :: boolean(),
                             spin_step = 1 :: ah_entry_integer(),
                             drop_down = true :: boolean(),
                             drop_down_width = undefined :: undefined | pos_integer(),
                             notation = default :: default | exponential,
                             placeholder = <<>> :: unicode:chardata()}).

%% A range {Lo, Hi} chosen on a track with ticks, labels and two markers;
%% postback fires on change (end of a drag, a key press). `range' is
%% {Min, Max} or {Min, Max, Step}; the default value is the whole range.
-record(ah_range_selector, {?AH_BASE(aihtml_form_entry),
                            range = {0, 200} :: {number(), number()}
                                              | {number(), number(), number()},
                            value = undefined :: undefined | {number(), number()},
                            name = undefined :: undefined | atom() | iodata(),
                            disabled = false :: boolean(),
                            major_ticks = 10 :: number(),
                            minor_ticks = 1 :: number(),
                            tick_values = undefined :: undefined | [number()],
                            show_major_ticks = true :: boolean(),
                            show_minor_ticks = false :: boolean(),
                            show_labels = true :: boolean(),
                            show_markers = true :: boolean(),
                            labels_format = number :: ah_entry_format(),
                            markers_format = undefined :: undefined | ah_entry_format(),
                            min_span = 0 :: number()}).

%% A button that fires click again and again while it is held (mouse,
%% touch, Enter or Space): once on press, then every `interval' ms after
%% `delay' ms; postback fires on each click. Renders as button/4 does.
-record(ah_repeat_button, {?AH_BASE(aihtml_form_entry),
                           body = [] :: aihtml_html:html(),
                           value = undefined :: term(),
                           variant = primary :: ah_entry_btn_variant(),
                           size = md :: sm | md | lg,
                           round = false :: boolean(),
                           disabled = false :: boolean(),
                           icon = undefined :: aihtml_html:html(),
                           img = undefined :: undefined | iodata(),
                           icon_position = left :: left | right | top | bottom,
                           delay = 300 :: non_neg_integer(),
                           interval = 50 :: pos_integer()}).

-endif.
