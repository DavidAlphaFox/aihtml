%% The element record of aihtml_dragdrop (designs/05-records.md).
-ifndef(AIHTML_DRAGDROP_HRL).
-define(AIHTML_DRAGDROP_HRL, true).

-include("aihtml_element.hrl").

%% A scope of draggable items and drop zones, marked inside `body' with
%% draggable_attrs/2 and drop_zone_attrs/2. Postback fires on 'ah:drop';
%% Event.data holds drag (the item key), drop (the zone) and from (the
%% zone the item came from, "" when none).
-record(ah_dragdrop, {?AH_BASE(aihtml_dragdrop),
                      body = [] :: aihtml_html:html(),
                      tolerance = intersect :: aihtml_dragdrop:tolerance(),
                      move = false :: boolean(),
                      revert = false :: boolean(),
                      disabled = false :: boolean()}).

-endif.
