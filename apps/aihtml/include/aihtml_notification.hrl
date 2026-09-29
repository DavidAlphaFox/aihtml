%% The element record of aihtml_notification (designs/05-records.md). Field names
%% follow the catalog: modifier groups, flags and options are fields of
%% the same name, with the catalog's defaults (aihtml_notification_tests checks
%% that they agree). Options written as a `data-ah-*' attribute only when
%% set default to `undefined', which leaves the browser default in place.
%%
%% Overlays are opened by id (opens({id, Id}), aihtml_lib_overlay:open/2),
%% so they usually want `id' set.
-ifndef(AIHTML_NOTIFICATION_HRL).
-define(AIHTML_NOTIFICATION_HRL, true).

-include("aihtml_element.hrl").

%% A hidden card template; each open clones it into a corner stack.
%% Postback fires on ah:close (when a card closes).
-record(ah_notification, {?AH_BASE(aihtml_notification),
                          body = [] :: aihtml_html:html(),
                          variant = info :: aihtml_notification:variant(),
                          position = top_right :: aihtml_lib_overlay:corner(),
                          auto_close = true :: boolean(),
                          delay = 3000 :: non_neg_integer(),
                          closable = true :: boolean(),
                          close_on_click = true :: boolean(),
                          width = undefined :: undefined | aihtml_lib_overlay:len()}).

-endif.
