%% Element record of aihtml_split_button (designs/05-records.md). Field names
%% follow the catalog: modifier groups, flags and options are fields of
%% the same name, with the catalog's defaults (aihtml_split_button_tests checks
%% that they agree).
-ifndef(AIHTML_SPLIT_BUTTON_HRL).
-define(AIHTML_SPLIT_BUTTON_HRL, true).

-include("aihtml_element.hrl").

%% A main action plus an arrow that opens a menu; postback fires on a
%% click on the main half.
-record(ah_split_button, {?AH_BASE(aihtml_split_button),
                          body = [] :: aihtml_html:html(),
                          items = [] :: [aihtml_lib_button:item()],
                          value = undefined :: term(),
                          name = undefined :: undefined | atom() | iodata(),
                          variant = primary :: primary | secondary | success | warning
                                             | error | info | outlined,
                          size = md :: aihtml_button:size(),
                          menu_align = 'end' :: start | 'end',
                          disabled = false :: boolean()}).

-endif.
