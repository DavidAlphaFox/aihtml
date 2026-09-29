%% Element record of aihtml_tabs (designs/05-records.md). Field names
%% follow the catalog: modifier groups, flags and options are fields of
%% the same name, with the catalog's defaults (aihtml_tabs_tests checks
%% that they agree). Options default to what the component does when
%% the option is left out.
-ifndef(AIHTML_TABS_HRL).
-define(AIHTML_TABS_HRL, true).

-include("aihtml_element.hrl").

%% Tabbed panels; `value' is the active key (undefined: the first enabled
%% tab) and postback fires on change. Without an `id' one is generated
%% (ah-tabs-N), the tab and panel ids derive from it.
-record(ah_tabs, {?AH_BASE(aihtml_tabs),
                  items = [] :: [aihtml_tabs:tab()],
                  value = undefined :: undefined | aihtml_lib_layout:key(),
                  position = top :: top | bottom | left | right,
                  disabled = false :: boolean(),
                  animation = undefined :: undefined | fade | none,
                  selection_mode = undefined :: undefined | click | hover,
                  scrollable = false :: boolean(),
                  name = undefined :: aihtml_lib_layout:name()}).

-endif.
