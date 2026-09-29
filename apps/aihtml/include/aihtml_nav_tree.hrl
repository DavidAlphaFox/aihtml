%% Element record of aihtml_nav_tree (designs/05-records.md). Field names follow
%% the catalog: modifier groups, flags and options are fields of the same
%% name, with the catalog's defaults (aihtml_nav_tree_tests checks that they
%% agree). The field types are aihtml_nav_tree's.
-ifndef(AIHTML_NAV_TREE_HRL).
-define(AIHTML_NAV_TREE_HRL, true).

-include("aihtml_element.hrl").

%% A grouped side navigation of links with collapsible nodes (native
%% <details>); `value' is the active route. Postback fires on change when
%% a link is chosen.
-record(ah_nav_tree, {?AH_BASE(aihtml_nav_tree),
                      items = [] :: [aihtml_nav_tree:item()],
                      value = undefined :: undefined | iodata() | atom(),
                      route_prefix = <<"#/">> :: iodata()}).

-endif.
