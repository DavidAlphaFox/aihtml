%% Element record of aihtml_dropdown_button (designs/05-records.md). Field names
%% follow the catalog: modifier groups, flags and options are fields of
%% the same name, with the catalog's defaults (aihtml_dropdown_button_tests checks
%% that they agree).
-ifndef(AIHTML_DROPDOWN_BUTTON_HRL).
-define(AIHTML_DROPDOWN_BUTTON_HRL, true).

-include("aihtml_element.hrl").

%% A button that opens a menu; `value' is the initially selected item and
%% postback fires on change.
-record(ah_dropdown_button, {?AH_BASE(aihtml_dropdown_button),
                             body = [] :: aihtml_html:html(),
                             items = [] :: [aihtml_lib_button:item()],
                             value = undefined :: term(),
                             name = undefined :: undefined | atom() | iodata(),
                             variant = undefined :: undefined | primary | success | warning
                                                  | error | outlined,
                             size = md :: aihtml_button:size(),
                             rounded = false :: boolean(),
                             auto_open = false :: boolean(),
                             disabled = false :: boolean()}).

-endif.
