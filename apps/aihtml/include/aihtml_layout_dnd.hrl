%% Element records of aihtml_layout_dnd (designs/05-records.md).
-ifndef(AIHTML_LAYOUT_DND_HRL).
-define(AIHTML_LAYOUT_DND_HRL, true).

-include("aihtml_element.hrl").

%% {Key, Content} | {Key, Content, ItemAttrs}: Key identifies the item in
%% the order (data-value), ItemAttrs are HTML attributes of the item.
-type ah_dnd_item() :: {term(), aihtml_html:html()}
                     | {term(), aihtml_html:html(), aihtml_html:attrs()}.
-type ah_dnd_orientation() :: vertical | horizontal | grid.
-type ah_dnd_tolerance() :: intersect | fit | pointer.

%% A reorderable list (pointer drag and keyboard). `value' is the order of
%% item keys (a list or "a,b,c"; undefined keeps the order of `items').
%% Lists with the same `group' exchange items. Postback fires on change,
%% after a drop that changed the order.
-record(ah_sortable, {?AH_BASE(aihtml_layout_dnd),
                      items = [] :: [ah_dnd_item()],
                      value = undefined :: undefined | [term()] | iodata(),
                      orientation = vertical :: ah_dnd_orientation(),
                      handle = false :: boolean(),
                      group = undefined :: undefined | atom() | iodata(),
                      name = undefined :: undefined | atom() | iodata(),
                      disabled = false :: boolean()}).

%% A scope of draggable items and drop zones, marked inside `body' with
%% draggable_attrs/2 and drop_zone_attrs/2. Postback fires on 'ah:drop';
%% Event.data holds drag (the item key), drop (the zone) and from (the
%% zone the item came from, "" when none).
-record(ah_dragdrop, {?AH_BASE(aihtml_layout_dnd),
                      body = [] :: aihtml_html:html(),
                      tolerance = intersect :: ah_dnd_tolerance(),
                      move = false :: boolean(),
                      revert = false :: boolean(),
                      disabled = false :: boolean()}).

-endif.
