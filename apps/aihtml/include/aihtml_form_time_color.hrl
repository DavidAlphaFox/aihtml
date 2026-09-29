%% Element records of aihtml_form_time_color (designs/05-records.md).
%% Field names follow the catalog: modifier groups, flags and options are
%% fields of the same name, with the catalog's defaults
%% (aihtml_form_time_color_tests checks that they agree).
-ifndef(AIHTML_FORM_TIME_COLOR_HRL).
-define(AIHTML_FORM_TIME_COLOR_HRL, true).

-include("aihtml_element.hrl").

%% "HH:MM", "HH:MM:SS", {H, M}, {H, M, S}; undefined or <<>> is empty.
-type ah_tc_time() :: undefined | binary() | string()
                    | {integer(), integer()} | {integer(), integer(), integer()}.
-type ah_tc_time_format() :: '12h' | '24h' | 12 | 24 | binary().
%% "#rrggbb[aa]", "#rgb[a]" (the # is optional), {R, G, B}, {R, G, B, A};
%% undefined or <<>> is empty.
-type ah_tc_color() :: undefined | binary() | string()
                     | {integer(), integer(), integer()}
                     | {integer(), integer(), integer(), integer()}.
%% Pixels or a CSS length.
-type ah_tc_length() :: undefined | non_neg_integer() | iodata().

%% A clock-face time picker; postback fires on change.
%% `placeholder' undefined means "--:-- --" (12h) or "--:--" (24h).
-record(ah_timepicker, {?AH_BASE(aihtml_form_time_color),
                        value = undefined :: ah_tc_time(),
                        name = undefined :: undefined | atom() | iodata(),
                        view = undefined :: undefined | portrait | landscape,
                        inline = false :: boolean(),
                        disabled = false :: boolean(),
                        clearable = false :: boolean(),
                        format = '12h' :: ah_tc_time_format(),
                        minute_step = 5 :: 1..30,
                        auto_switch = true :: boolean(),
                        min = undefined :: ah_tc_time(),
                        max = undefined :: ah_tc_time(),
                        placeholder = undefined :: undefined | iodata(),
                        footer = undefined :: aihtml_html:html()}).

%% An HSV colour picker; postback fires on change (the element also fires
%% `input' while dragging).
-record(ah_colorpicker, {?AH_BASE(aihtml_form_time_color),
                         value = undefined :: ah_tc_color(),
                         name = undefined :: undefined | atom() | iodata(),
                         inline = false :: boolean(),
                         disabled = false :: boolean(),
                         clearable = false :: boolean(),
                         alpha = false :: boolean(),
                         no_inputs = false :: boolean(),
                         no_preview = false :: boolean(),
                         swatches = [] :: [ah_tc_color()],
                         placeholder = <<"No color">> :: iodata(),
                         width = undefined :: ah_tc_length(),
                         height = undefined :: ah_tc_length(),
                         clear_label = <<"Clear">> :: aihtml_html:html()}).

-endif.
