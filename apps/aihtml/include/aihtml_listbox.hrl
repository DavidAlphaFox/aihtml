%% Element record of aihtml_listbox (designs/05-records.md). Field names
%% follow the catalog: modifier groups, flags and options are fields of
%% the same name, with the catalog's defaults (aihtml_listbox_tests checks
%% that they agree).
-ifndef(AIHTML_LISTBOX_HRL).
-define(AIHTML_LISTBOX_HRL, true).

-include("aihtml_element.hrl").

%% A focusable list with single or multiple selection and keyboard
%% navigation; postback fires on change. `value' is a list of values with
%% `multiple' or `checkboxes'. `search' is an action ref run (debounced)
%% as the user types in the filter. Without an `id' one is generated.
-record(ah_listbox, {?AH_BASE(aihtml_listbox),
                     items = [] :: [aihtml_lib_list:item()],
                     value = undefined :: term(),
                     name = undefined :: undefined | atom() | iodata(),
                     disabled = false :: boolean(),
                     multiple = false :: boolean(),
                     checkboxes = false :: boolean(),
                     check_all = false :: boolean(),
                     filterable = false :: boolean(),
                     empty_text = <<"No data">> :: unicode:chardata(),
                     filter_placeholder = <<"Search">> :: unicode:chardata(),
                     check_all_label = <<"Select all">> :: unicode:chardata(),
                     search = undefined :: undefined | aihtml_action:ref()}).

-endif.
