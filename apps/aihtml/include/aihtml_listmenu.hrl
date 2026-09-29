%% Element record of aihtml_listmenu (designs/05-records.md). Field names
%% follow the catalog: modifier groups, flags and options are fields of
%% the same name, with the catalog's defaults (aihtml_listmenu_tests
%% checks that they agree).
-ifndef(AIHTML_LISTMENU_HRL).
-define(AIHTML_LISTMENU_HRL, true).

-include("aihtml_element.hrl").

%% Drill-down list menu; `value' is the selected leaf. Postback fires on
%% change. `filter_placeholder' undefined means "Filter..." (aria-label
%% "Filter").
-record(ah_listmenu, {?AH_BASE(aihtml_listmenu),
                      items = [] :: [aihtml_lib_nav:item()],
                      value = undefined :: undefined | aihtml_lib_nav:key(),
                      disabled = false :: boolean(),
                      header = true :: boolean(),
                      back_button = true :: boolean(),
                      filter = false :: boolean(),
                      arrows = true :: boolean(),
                      back_label = <<"Back">> :: aihtml_html:html(),
                      filter_placeholder = undefined :: undefined | binary(),
                      animation = undefined :: undefined | slide | fade | none,
                      name = undefined :: aihtml_lib_nav:name()}).

-endif.
