%% Element record of aihtml_tab_bar (designs/05-records.md). Field names
%% follow the catalog: modifier groups, flags and options are fields of
%% the same name, with the catalog's defaults (aihtml_tab_bar_tests checks
%% that they agree). Options default to what the component does when
%% the option is left out.
-ifndef(AIHTML_TAB_BAR_HRL).
-define(AIHTML_TAB_BAR_HRL, true).

-include("aihtml_element.hrl").

%% An editor-style strip of closable tabs; `value' is the active id and
%% postback fires on change.
-record(ah_tab_bar, {?AH_BASE(aihtml_tab_bar),
                     items = [] :: [aihtml_tab_bar:tab()],
                     value = undefined :: undefined | aihtml_lib_layout:key(),
                     closable = true :: boolean(),
                     close_label = <<"close">> :: aihtml_html:html(),
                     name = undefined :: aihtml_lib_layout:name()}).

-endif.
