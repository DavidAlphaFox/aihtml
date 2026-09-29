%% Element record of aihtml_slider (designs/05-records.md). Field names
%% follow the catalog: modifier groups, flags and options are fields of
%% the same name, with the catalog's defaults (aihtml_slider_tests checks
%% that they agree).
-ifndef(AIHTML_SLIDER_HRL).
-define(AIHTML_SLIDER_HRL, true).

-include("aihtml_element.hrl").

%% A slider; `value' is a number or {Lo, Hi} for two thumbs. Postback fires
%% on change (on release and on each keyboard or button step).
-record(ah_slider, {?AH_BASE(aihtml_slider),
                    range = {0, 100} :: aihtml_slider:range(),
                    value = undefined :: undefined | number() | {number(), number()},
                    orientation = horizontal :: horizontal | vertical,
                    template = undefined :: undefined | aihtml_lib_select:template()
                                          | info | secondary,
                    disabled = false :: boolean(),
                    buttons = false :: boolean(),
                    tooltip = false :: boolean(),
                    name = undefined :: undefined | atom() | iodata(),
                    ticks = false :: false | number(),
                    minor_ticks = false :: false | number(),
                    labels = true :: boolean(),
                    ticks_position = bottom :: top | bottom | both,
                    min_range = undefined :: undefined | number()}).

-endif.
