%% Element record of aihtml_radiobutton_group (designs/05-records.md). Field names
%% follow the catalog: modifier groups, flags and options are fields of
%% the same name, with the catalog's defaults (aihtml_radiobutton_group_tests checks
%% that they agree).
-ifndef(AIHTML_RADIOBUTTON_GROUP_HRL).
-define(AIHTML_RADIOBUTTON_GROUP_HRL, true).

-include("aihtml_element.hrl").

%% Mutually exclusive radio buttons; `value' is the selected one. Postback
%% fires on change of the root.
-record(ah_radiobutton_group, {?AH_BASE(aihtml_radiobutton_group),
                               items = [] :: [aihtml_lib_choice:item()],
                               value = undefined :: undefined | aihtml_lib_choice:value(),
                               name = undefined :: aihtml_lib_choice:name(),
                               disabled = false :: boolean(),
                               required = false :: boolean(),
                               form = undefined :: aihtml_lib_choice:name(),
                               layout = vertical :: aihtml_lib_choice:layout(),
                               size = undefined :: undefined | aihtml_lib_choice:size(),
                               label_before = false :: boolean()}).

-endif.
