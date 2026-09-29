%% The element record of aihtml_sortable (designs/05-records.md).
-ifndef(AIHTML_SORTABLE_HRL).
-define(AIHTML_SORTABLE_HRL, true).

-include("aihtml_element.hrl").

%% A reorderable list (pointer drag and keyboard). `value' is the order of
%% item keys (a list or "a,b,c"; undefined keeps the order of `items').
%% Lists with the same `group' exchange items. Postback fires on change,
%% after a drop that changed the order.
-record(ah_sortable, {?AH_BASE(aihtml_sortable),
                      items = [] :: [aihtml_sortable:item()],
                      value = undefined :: undefined | [term()] | iodata(),
                      orientation = vertical :: aihtml_sortable:orientation(),
                      handle = false :: boolean(),
                      group = undefined :: undefined | atom() | iodata(),
                      name = undefined :: undefined | atom() | iodata(),
                      disabled = false :: boolean()}).

-endif.
