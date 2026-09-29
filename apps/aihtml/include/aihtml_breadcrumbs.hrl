%% Element record of aihtml_breadcrumbs (designs/05-records.md). Field names
%% follow the catalog: modifier groups, flags and options are fields of
%% the same name, with the catalog's defaults (aihtml_breadcrumbs_tests checks
%% that they agree). Options default to what the component does when
%% the option is left out.
-ifndef(AIHTML_BREADCRUMBS_HRL).
-define(AIHTML_BREADCRUMBS_HRL, true).

-include("aihtml_element.hrl").

%% An ancestor path; no postback event.
-record(ah_breadcrumbs, {?AH_BASE(aihtml_breadcrumbs),
                         items = [] :: [aihtml_breadcrumbs:crumb()],
                         separator = <<"/">> :: aihtml_html:html() | none,
                         active_last = false :: boolean(),
                         max_items = undefined :: undefined | pos_integer(),
                         label = <<"breadcrumb">> :: aihtml_html:html()}).

-endif.
