%% Element record of aihtml_toolbar (designs/05-records.md). Field names
%% follow the catalog: modifier groups, flags and options are fields of
%% the same name, with the catalog's defaults (aihtml_toolbar_tests
%% checks that they agree).
-ifndef(AIHTML_TOOLBAR_HRL).
-define(AIHTML_TOOLBAR_HRL, true).

-include("aihtml_element.hrl").

%% Row of tools with an overflow popup. Postback fires on change (a tool
%% with a key was clicked; data-ah-value is its key).
-record(ah_toolbar, {?AH_BASE(aihtml_toolbar),
                     tools = [] :: [aihtml_toolbar:tool()],
                     disabled = false :: boolean(),
                     popup_width = undefined :: undefined | aihtml_lib_nav:px()}).

-endif.
