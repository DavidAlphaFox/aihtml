%% Element record of aihtml_segmented_control (designs/05-records.md). Field names
%% follow the catalog: modifier groups, flags and options are fields of
%% the same name, with the catalog's defaults (aihtml_segmented_control_tests checks
%% that they agree).
-ifndef(AIHTML_SEGMENTED_CONTROL_HRL).
-define(AIHTML_SEGMENTED_CONTROL_HRL, true).

-include("aihtml_element.hrl").

%% Mutually exclusive segments; postback fires on change.
-record(ah_segmented_control, {?AH_BASE(aihtml_segmented_control),
                               items = [] :: [aihtml_lib_button:item()],
                               value = undefined :: term(),
                               name = undefined :: undefined | atom() | iodata(),
                               size = md :: aihtml_button:size(),
                               full_width = false :: boolean(),
                               disabled = false :: boolean()}).

-endif.
