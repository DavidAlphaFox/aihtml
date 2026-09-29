%% Element record of aihtml_link_button (designs/05-records.md). Field names
%% follow the catalog: modifier groups, flags and options are fields of
%% the same name, with the catalog's defaults (aihtml_link_button_tests checks
%% that they agree).
-ifndef(AIHTML_LINK_BUTTON_HRL).
-define(AIHTML_LINK_BUTTON_HRL, true).

-include("aihtml_element.hrl").

%% An <a href> styled as a button; disabled drops the href.
-record(ah_link_button, {?AH_BASE(aihtml_link_button),
                         body = [] :: aihtml_html:html(),
                         href = undefined :: undefined | iodata(),
                         variant = primary :: aihtml_button:variant(),
                         size = md :: aihtml_button:size(),
                         round = false :: boolean(),
                         disabled = false :: boolean()}).

-endif.
