%% Element record of aihtml_panel (designs/05-records.md). Field names
%% follow the catalog: modifier groups, flags and options are fields of
%% the same name, with the catalog's defaults (aihtml_panel_tests checks
%% that they agree). Options default to what the component does when
%% the option is left out.
-ifndef(AIHTML_PANEL_HRL).
-define(AIHTML_PANEL_HRL, true).

-include("aihtml_element.hrl").

%% A scrollable container, optionally collapsible; no postback event.
%% Without an `id' one is generated (ah-panel-N), the header and body ids
%% derive from it.
-record(ah_panel, {?AH_BASE(aihtml_panel),
                   body = [] :: aihtml_html:html(),
                   bordered = false :: boolean(),
                   title = undefined :: undefined | aihtml_html:html(),
                   actions = undefined :: undefined | aihtml_html:html(),
                   collapsible = false :: boolean(),
                   collapsed = false :: boolean(),
                   height = undefined :: aihtml_lib_layout:css_length(),
                   max_height = undefined :: aihtml_lib_layout:css_length(),
                   toggle_label = <<"Toggle">> :: aihtml_html:html()}).

-endif.
