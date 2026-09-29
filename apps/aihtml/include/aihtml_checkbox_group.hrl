%% Element record of aihtml_checkbox_group (designs/05-records.md). Field names
%% follow the catalog: modifier groups, flags and options are fields of
%% the same name, with the catalog's defaults (aihtml_checkbox_group_tests checks
%% that they agree).
-ifndef(AIHTML_CHECKBOX_GROUP_HRL).
-define(AIHTML_CHECKBOX_GROUP_HRL, true).

-include("aihtml_element.hrl").

%% Checkboxes; `value' lists the checked ones. name, disabled, required
%% and form go to every input. Postback fires on change of the root.
-record(ah_checkbox_group, {?AH_BASE(aihtml_checkbox_group),
                            items = [] :: [aihtml_lib_choice:item()],
                            value = [] :: [aihtml_lib_choice:value()],
                            name = undefined :: aihtml_lib_choice:name(),
                            disabled = false :: boolean(),
                            required = false :: boolean(),
                            form = undefined :: aihtml_lib_choice:name(),
                            layout = vertical :: aihtml_lib_choice:layout(),
                            size = undefined :: undefined | aihtml_lib_choice:size(),
                            label_before = false :: boolean()}).

-endif.
