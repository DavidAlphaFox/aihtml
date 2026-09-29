%% Element record of aihtml_range_selector (designs/05-records.md). Field names
%% follow the catalog: modifier groups, flags and options are fields of
%% the same name, with the catalog's defaults (aihtml_range_selector_tests checks
%% that they agree).
-ifndef(AIHTML_RANGE_SELECTOR_HRL).
-define(AIHTML_RANGE_SELECTOR_HRL, true).

-include("aihtml_element.hrl").

%% A range {Lo, Hi} chosen on a track with ticks, labels and two markers;
%% postback fires on change (end of a drag, a key press). `range' is
%% {Min, Max} or {Min, Max, Step}; the default value is the whole range.
-record(ah_range_selector, {?AH_BASE(aihtml_range_selector),
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
                            labels_format = number :: aihtml_range_selector:format(),
                            markers_format = undefined :: undefined | aihtml_range_selector:format(),
                            min_span = 0 :: number()}).

-endif.
