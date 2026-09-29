%% Element record of aihtml_empty (designs/05-records.md). Field names
%% follow the catalog: modifier groups, flags and options are fields of
%% the same name, with the catalog's defaults (aihtml_empty_tests checks
%% that they agree). Options default to what the component does when
%% the option is left out.
-ifndef(AIHTML_EMPTY_HRL).
-define(AIHTML_EMPTY_HRL, true).

-include("aihtml_element.hrl").

%% An empty-state placeholder, `body' being the action area; no postback
%% event.
-record(ah_empty, {?AH_BASE(aihtml_empty),
                   body = [] :: aihtml_html:html(),
                   compact = false :: boolean(),
                   icon = undefined :: undefined | aihtml_html:html(),
                   title = undefined :: undefined | aihtml_html:html(),
                   description = undefined :: undefined | aihtml_html:html()}).

-endif.
