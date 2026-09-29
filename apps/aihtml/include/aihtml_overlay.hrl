%% Element records of aihtml_overlay (designs/05-records.md). Field names
%% follow the catalog: modifier groups, flags and options are fields of
%% the same name, with the catalog's defaults (aihtml_overlay_tests checks
%% that they agree). The toast catalog entry is an action (toast/3), not
%% an element, so it has no record.
%%
%% Options written as a `data-ah-*' attribute only when set (trigger,
%% show_delay, anchor, modal, close_on_esc ...) default to `undefined',
%% which leaves the browser default in place.
%%
%% Overlays are opened by id (opens({id, Id}), aihtml_overlay:open/2), so
%% most of them want `id' set.
-ifndef(AIHTML_OVERLAY_HRL).
-define(AIHTML_OVERLAY_HRL, true).

-include("aihtml_element.hrl").

%% A length: integer px or a CSS length such as <<"50vh">>.
-type ah_overlay_len() :: non_neg_integer() | binary() | string().
-type ah_overlay_side() :: top | bottom | left | right.
-type ah_overlay_corner() :: top_right | top_left | bottom_right | bottom_left.

%% A bubble shown over `anchor' (the wrapped element) on hover, focus or
%% click; `body' is the bubble content. No postback event.
-record(ah_tooltip, {?AH_BASE(aihtml_overlay),
                     body = [] :: aihtml_html:html(),
                     anchor = [] :: aihtml_html:html(),
                     position = bottom :: ah_overlay_side() | mouse,
                     no_arrow = false :: boolean(),
                     trigger = undefined :: undefined | hover | click | none,
                     show_delay = undefined :: undefined | non_neg_integer(),
                     auto_hide = undefined :: undefined | boolean(),
                     auto_hide_delay = undefined :: undefined | non_neg_integer(),
                     disabled = undefined :: undefined | boolean(),
                     width = undefined :: undefined | ah_overlay_len()}).

%% A bubble anchored to the element that opened it or to `anchor' (a
%% selector). Postback fires on ah:close.
-record(ah_popover, {?AH_BASE(aihtml_overlay),
                     body = [] :: aihtml_html:html(),
                     position = bottom :: ah_overlay_side(),
                     no_arrow = false :: boolean(),
                     title = undefined :: aihtml_html:html(),
                     closable = false :: boolean(),
                     anchor = undefined :: undefined | binary() | string(),
                     modal = undefined :: undefined | boolean(),
                     auto_close = undefined :: undefined | boolean(),
                     width = undefined :: undefined | ah_overlay_len()}).

%% A modal panel sliding in from `side', swipe to dismiss. `size' defaults
%% to 50vh (top, bottom) or 380px (left, right). Postback fires on
%% ah:close.
-record(ah_drawer, {?AH_BASE(aihtml_overlay),
                    body = [] :: aihtml_html:html(),
                    side = bottom :: ah_overlay_side(),
                    title = undefined :: aihtml_html:html(),
                    description = undefined :: aihtml_html:html(),
                    footer = undefined :: aihtml_html:html(),
                    size = undefined :: undefined | ah_overlay_len(),
                    closable = true :: boolean(),
                    handle = true :: boolean(),
                    dismissible = undefined :: undefined | boolean(),
                    close_on_overlay = undefined :: undefined | boolean(),
                    close_on_esc = undefined :: undefined | boolean(),
                    open = false :: boolean()}).

%% A modal side panel; `size' defaults to 380px. Postback fires on
%% ah:close.
-record(ah_sheet, {?AH_BASE(aihtml_overlay),
                   body = [] :: aihtml_html:html(),
                   side = right :: ah_overlay_side(),
                   title = undefined :: aihtml_html:html(),
                   description = undefined :: aihtml_html:html(),
                   footer = undefined :: aihtml_html:html(),
                   size = undefined :: undefined | ah_overlay_len(),
                   closable = true :: boolean(),
                   close_on_overlay = undefined :: undefined | boolean(),
                   close_on_esc = undefined :: undefined | boolean(),
                   open = false :: boolean()}).

%% A hidden card template; each open clones it into a corner stack.
%% Postback fires on ah:close (when a card closes).
-record(ah_notification, {?AH_BASE(aihtml_overlay),
                          body = [] :: aihtml_html:html(),
                          variant = info :: info | success | warning | error,
                          position = top_right :: ah_overlay_corner(),
                          auto_close = true :: boolean(),
                          delay = 3000 :: non_neg_integer(),
                          closable = true :: boolean(),
                          close_on_click = true :: boolean(),
                          width = undefined :: undefined | ah_overlay_len()}).

%% A floating, draggable, resizable window; `collapsed' implies
%% `collapsible'. Postback fires on ah:close.
-record(ah_window, {?AH_BASE(aihtml_overlay),
                    body = [] :: aihtml_html:html(),
                    title = undefined :: aihtml_html:html(),
                    footer = undefined :: aihtml_html:html(),
                    closable = true :: boolean(),
                    collapsible = false :: boolean(),
                    collapsed = false :: boolean(),
                    modal = false :: boolean(),
                    draggable = true :: boolean(),
                    resizable = true :: boolean(),
                    width = 300 :: ah_overlay_len(),
                    height = auto :: auto | ah_overlay_len(),
                    close_on_overlay = undefined :: undefined | boolean(),
                    close_on_esc = undefined :: undefined | boolean(),
                    open = false :: boolean()}).

-endif.
