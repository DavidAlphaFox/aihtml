%% The element record of aihtml_sheet (designs/05-records.md). Field names
%% follow the catalog: modifier groups, flags and options are fields of
%% the same name, with the catalog's defaults (aihtml_sheet_tests checks
%% that they agree). Options written as a `data-ah-*' attribute only when
%% set default to `undefined', which leaves the browser default in place.
%%
%% Overlays are opened by id (opens({id, Id}), aihtml_lib_overlay:open/2),
%% so they usually want `id' set.
-ifndef(AIHTML_SHEET_HRL).
-define(AIHTML_SHEET_HRL, true).

-include("aihtml_element.hrl").

%% A modal side panel; `size' defaults to 380px. Postback fires on
%% ah:close.
-record(ah_sheet, {?AH_BASE(aihtml_sheet),
                   body = [] :: aihtml_html:html(),
                   side = right :: aihtml_lib_overlay:side(),
                   title = undefined :: aihtml_html:html(),
                   description = undefined :: aihtml_html:html(),
                   footer = undefined :: aihtml_html:html(),
                   size = undefined :: undefined | aihtml_lib_overlay:len(),
                   closable = true :: boolean(),
                   close_on_overlay = undefined :: undefined | boolean(),
                   close_on_esc = undefined :: undefined | boolean(),
                   open = false :: boolean()}).

-endif.
