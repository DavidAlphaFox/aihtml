%% Element record of aihtml_colorpicker (designs/05-records.md). Field
%% names follow the catalog: flags and options are fields of the same
%% name, with the catalog's defaults (aihtml_colorpicker_tests checks
%% that they agree).
-ifndef(AIHTML_COLORPICKER_HRL).
-define(AIHTML_COLORPICKER_HRL, true).

-include("aihtml_element.hrl").

%% An HSV colour picker; postback fires on change (the element also fires
%% `input' while dragging).
-record(ah_colorpicker, {?AH_BASE(aihtml_colorpicker),
                         value = undefined :: aihtml_colorpicker:color(),
                         name = undefined :: undefined | atom() | iodata(),
                         inline = false :: boolean(),
                         disabled = false :: boolean(),
                         clearable = false :: boolean(),
                         alpha = false :: boolean(),
                         no_inputs = false :: boolean(),
                         no_preview = false :: boolean(),
                         swatches = [] :: [aihtml_colorpicker:color()],
                         placeholder = <<"No color">> :: iodata(),
                         width = undefined :: aihtml_colorpicker:css_length(),
                         height = undefined :: aihtml_colorpicker:css_length(),
                         clear_label = <<"Clear">> :: aihtml_html:html()}).

-endif.
