%% Element record of aihtml_activity_bar (designs/05-records.md). Field
%% names follow the catalog: modifier groups, flags and options are fields
%% of the same name, with the catalog's defaults (aihtml_activity_bar_tests
%% checks that they agree).
-ifndef(AIHTML_ACTIVITY_BAR_HRL).
-define(AIHTML_ACTIVITY_BAR_HRL, true).

-include("aihtml_element.hrl").

%% A VS Code-style vertical icon rail; `value' is the active item and
%% postback fires on change.
-record(ah_activity_bar, {?AH_BASE(aihtml_activity_bar),
                          items = [] :: [aihtml_activity_bar:item()],
                          value = undefined :: term(),
                          name = undefined :: undefined | atom() | iodata(),
                          placement = left :: left | right}).

-endif.
