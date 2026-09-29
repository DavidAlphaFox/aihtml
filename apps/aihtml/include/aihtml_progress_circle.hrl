%% Element record of aihtml_progress_circle (designs/05-records.md). Field
%% names follow the catalog: modifier groups, flags and options are
%% fields of the same name, with the catalog's defaults (aihtml_progress_circle_tests
%% checks that they agree). Option fields default to what the component
%% used when the option was left out.
-ifndef(AIHTML_PROGRESS_CIRCLE_HRL).
-define(AIHTML_PROGRESS_CIRCLE_HRL, true).

-include("aihtml_element.hrl").

%% Circular progress, `value' 0..100. Postback fires on change (setValue
%% in the browser).
-record(ah_progress_circle, {?AH_BASE(aihtml_progress_circle),
                             value = undefined :: undefined | number(),
                             size = md :: sm | md | lg,
                             color = primary :: primary | success | warning | info | error,
                             disabled = false :: boolean(),
                             indeterminate = false :: boolean(),
                             label = undefined :: aihtml_html:html(),
                             show_value = true :: boolean()}).

-endif.
