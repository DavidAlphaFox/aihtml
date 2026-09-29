%% The element record of aihtml_drawer (designs/05-records.md). Field names
%% follow the catalog: modifier groups, flags and options are fields of
%% the same name, with the catalog's defaults (aihtml_drawer_tests checks
%% that they agree). Options written as a `data-ah-*' attribute only when
%% set default to `undefined', which leaves the browser default in place.
%%
%% Overlays are opened by id (opens({id, Id}), aihtml_lib_overlay:open/2),
%% so they usually want `id' set.
-ifndef(AIHTML_DRAWER_HRL).
-define(AIHTML_DRAWER_HRL, true).

-include("aihtml_element.hrl").

%% A modal panel sliding in from `side', swipe to dismiss. `size' defaults
%% to 50vh (top, bottom) or 380px (left, right). Postback fires on
%% ah:close.
-record(ah_drawer, {?AH_BASE(aihtml_drawer),
                    body = [] :: aihtml_html:html(),
                    side = bottom :: aihtml_lib_overlay:side(),
                    title = undefined :: aihtml_html:html(),
                    description = undefined :: aihtml_html:html(),
                    footer = undefined :: aihtml_html:html(),
                    size = undefined :: undefined | aihtml_lib_overlay:len(),
                    closable = true :: boolean(),
                    handle = true :: boolean(),
                    dismissible = undefined :: undefined | boolean(),
                    close_on_overlay = undefined :: undefined | boolean(),
                    close_on_esc = undefined :: undefined | boolean(),
                    open = false :: boolean()}).

-endif.
