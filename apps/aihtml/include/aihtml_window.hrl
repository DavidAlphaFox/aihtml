%% The element record of aihtml_window (designs/05-records.md). Field names
%% follow the catalog: modifier groups, flags and options are fields of
%% the same name, with the catalog's defaults (aihtml_window_tests checks
%% that they agree). Options written as a `data-ah-*' attribute only when
%% set default to `undefined', which leaves the browser default in place.
%%
%% Overlays are opened by id (opens({id, Id}), aihtml_lib_overlay:open/2),
%% so they usually want `id' set.
-ifndef(AIHTML_WINDOW_HRL).
-define(AIHTML_WINDOW_HRL, true).

-include("aihtml_element.hrl").

%% A floating, draggable, resizable window; `collapsed' implies
%% `collapsible'. Postback fires on ah:close.
-record(ah_window, {?AH_BASE(aihtml_window),
                    body = [] :: aihtml_html:html(),
                    title = undefined :: aihtml_html:html(),
                    footer = undefined :: aihtml_html:html(),
                    closable = true :: boolean(),
                    collapsible = false :: boolean(),
                    collapsed = false :: boolean(),
                    modal = false :: boolean(),
                    draggable = true :: boolean(),
                    resizable = true :: boolean(),
                    width = 300 :: aihtml_lib_overlay:len(),
                    height = auto :: auto | aihtml_lib_overlay:len(),
                    close_on_overlay = undefined :: undefined | boolean(),
                    close_on_esc = undefined :: undefined | boolean(),
                    open = false :: boolean()}).

-endif.
