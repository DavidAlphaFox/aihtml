%% Element record of aihtml_toggle_button (designs/05-records.md). Field names
%% follow the catalog: modifier groups, flags and options are fields of
%% the same name, with the catalog's defaults (aihtml_toggle_button_tests checks
%% that they agree).
-ifndef(AIHTML_TOGGLE_BUTTON_HRL).
-define(AIHTML_TOGGLE_BUTTON_HRL, true).

-include("aihtml_element.hrl").

%% A pressed / released button; postback fires on change.
-record(ah_toggle_button, {?AH_BASE(aihtml_toggle_button),
                           body = [] :: aihtml_html:html(),
                           value = false :: boolean(),
                           name = undefined :: undefined | atom() | iodata(),
                           variant = primary :: aihtml_button:variant(),
                           size = md :: aihtml_button:size(),
                           round = false :: boolean(),
                           disabled = false :: boolean(),
                           icon = undefined :: aihtml_html:html(),
                           img = undefined :: undefined | iodata(),
                           icon_position = left :: aihtml_button:icon_position()}).

-endif.
