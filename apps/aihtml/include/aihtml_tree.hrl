%% Element record of aihtml_tree (designs/05-records.md). Field names follow
%% the catalog: modifier groups, flags and options are fields of the same
%% name, with the catalog's defaults (aihtml_tree_tests checks that they
%% agree). The field types are aihtml_tree's.
-ifndef(AIHTML_TREE_HRL).
-define(AIHTML_TREE_HRL, true).

-include("aihtml_element.hrl").

%% A hierarchical list with expand / collapse, single selection and
%% keyboard navigation; postback fires on change (the selected value).
%% `load' is an action ref that supplies the children of lazy nodes (see
%% aihtml_tree:set_children/3). Without an `id' one is generated.
-record(ah_tree, {?AH_BASE(aihtml_tree),
                  items = [] :: [aihtml_tree:item()],
                  value = undefined :: term(),
                  name = undefined :: undefined | atom() | iodata(),
                  disabled = false :: boolean(),
                  toggle_mode = click :: click | dblclick,
                  animation = slide :: slide | none,
                  load = undefined :: undefined | aihtml_action:ref()}).

-endif.
