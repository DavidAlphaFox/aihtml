%% The element record of aihtml_tooltip (designs/05-records.md). Field names
%% follow the catalog: modifier groups, flags and options are fields of
%% the same name, with the catalog's defaults (aihtml_tooltip_tests checks
%% that they agree). Options written as a `data-ah-*' attribute only when
%% set default to `undefined', which leaves the browser default in place.
-ifndef(AIHTML_TOOLTIP_HRL).
-define(AIHTML_TOOLTIP_HRL, true).

-include("aihtml_element.hrl").

%% A bubble shown over `anchor' (the wrapped element) on hover, focus or
%% click; `body' is the bubble content. No postback event.
-record(ah_tooltip, {?AH_BASE(aihtml_tooltip),
                     body = [] :: aihtml_html:html(),
                     anchor = [] :: aihtml_html:html(),
                     position = bottom :: aihtml_tooltip:position(),
                     no_arrow = false :: boolean(),
                     trigger = undefined :: undefined | aihtml_tooltip:trigger(),
                     show_delay = undefined :: undefined | non_neg_integer(),
                     auto_hide = undefined :: undefined | boolean(),
                     auto_hide_delay = undefined :: undefined | non_neg_integer(),
                     disabled = undefined :: undefined | boolean(),
                     width = undefined :: undefined | aihtml_lib_overlay:len()}).

-endif.
