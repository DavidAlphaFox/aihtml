%% Element record of aihtml_progressbar (designs/05-records.md). Field
%% names follow the catalog: modifier groups, flags and options are
%% fields of the same name, with the catalog's defaults (aihtml_progressbar_tests
%% checks that they agree). Option fields default to what the component
%% used when the option was left out.
-ifndef(AIHTML_PROGRESSBAR_HRL).
-define(AIHTML_PROGRESSBAR_HRL, true).

-include("aihtml_element.hrl").

%% Linear progress; `value' may be undefined when indeterminate. Postback
%% fires on change (setValue in the browser).
-record(ah_progressbar, {?AH_BASE(aihtml_progressbar),
                         value = undefined :: undefined | number(),
                         orientation = horizontal :: horizontal | vertical,
                         layout = normal :: normal | reverse,
                         color = primary :: primary | success | warning | error | info,
                         show_text = false :: boolean(),
                         disabled = false :: boolean(),
                         indeterminate = false :: boolean(),
                         striped = false :: boolean(),
                         animated = false :: boolean(),
                         min = 0 :: number(),
                         max = 100 :: number(),
                         text = undefined :: aihtml_html:html(),
                         color_ranges = [] :: [{number(), aihtml_lib_color:css_color()}]}).

-endif.
