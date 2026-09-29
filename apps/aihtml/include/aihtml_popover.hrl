%% The element record of aihtml_popover (designs/05-records.md). Field names
%% follow the catalog: modifier groups, flags and options are fields of
%% the same name, with the catalog's defaults (aihtml_popover_tests checks
%% that they agree). Options written as a `data-ah-*' attribute only when
%% set default to `undefined', which leaves the browser default in place.
%%
%% Overlays are opened by id (opens({id, Id}), aihtml_lib_overlay:open/2),
%% so they usually want `id' set.
-ifndef(AIHTML_POPOVER_HRL).
-define(AIHTML_POPOVER_HRL, true).

-include("aihtml_element.hrl").

%% A bubble anchored to the element that opened it or to `anchor' (a
%% selector). Postback fires on ah:close.
-record(ah_popover, {?AH_BASE(aihtml_popover),
                     body = [] :: aihtml_html:html(),
                     position = bottom :: aihtml_lib_overlay:side(),
                     no_arrow = false :: boolean(),
                     title = undefined :: aihtml_html:html(),
                     closable = false :: boolean(),
                     anchor = undefined :: undefined | binary() | string(),
                     modal = undefined :: undefined | boolean(),
                     auto_close = undefined :: undefined | boolean(),
                     width = undefined :: undefined | aihtml_lib_overlay:len()}).

-endif.
