%% Element record of aihtml_repeat_button (designs/05-records.md). Field names
%% follow the catalog: modifier groups, flags and options are fields of
%% the same name, with the catalog's defaults (aihtml_repeat_button_tests checks
%% that they agree).
-ifndef(AIHTML_REPEAT_BUTTON_HRL).
-define(AIHTML_REPEAT_BUTTON_HRL, true).

-include("aihtml_element.hrl").

%% A button that fires click again and again while it is held (mouse,
%% touch, Enter or Space): once on press, then every `interval' ms after
%% `delay' ms; postback fires on each click. Renders as ah_button/4 does.
-record(ah_repeat_button, {?AH_BASE(aihtml_repeat_button),
                           body = [] :: aihtml_html:html(),
                           value = undefined :: term(),
                           variant = primary :: aihtml_button:variant(),
                           size = md :: aihtml_button:size(),
                           round = false :: boolean(),
                           disabled = false :: boolean(),
                           icon = undefined :: aihtml_html:html(),
                           img = undefined :: undefined | iodata(),
                           icon_position = left :: aihtml_button:icon_position(),
                           delay = 300 :: non_neg_integer(),
                           interval = 50 :: pos_integer()}).

-endif.
