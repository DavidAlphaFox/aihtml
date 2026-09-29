%% Element record of aihtml_button (designs/05-records.md). Field names
%% follow the catalog: modifier groups, flags and options are fields of
%% the same name, with the catalog's defaults (aihtml_button_tests checks
%% that they agree).
-ifndef(AIHTML_BUTTON_HRL).
-define(AIHTML_BUTTON_HRL, true).

-include("aihtml_element.hrl").

%% A native <button type="button">; `value' becomes its value attribute.
-record(ah_button, {?AH_BASE(aihtml_button),
                    body = [] :: aihtml_html:html(),
                    value = undefined :: term(),
                    variant = primary :: aihtml_button:variant(),
                    size = md :: aihtml_button:size(),
                    round = false :: boolean(),
                    disabled = false :: boolean(),
                    icon = undefined :: aihtml_html:html(),
                    img = undefined :: undefined | iodata(),
                    icon_position = left :: aihtml_button:icon_position()}).

-endif.
