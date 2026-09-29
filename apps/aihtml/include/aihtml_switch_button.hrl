%% Element record of aihtml_switch_button (designs/05-records.md). Field names
%% follow the catalog: modifier groups, flags and options are fields of
%% the same name, with the catalog's defaults (aihtml_switch_button_tests checks
%% that they agree).
-ifndef(AIHTML_SWITCH_BUTTON_HRL).
-define(AIHTML_SWITCH_BUTTON_HRL, true).

-include("aihtml_element.hrl").

%% An on/off switch (a native checkbox with role=switch); `id', `attrs'
%% and the postback go to the input. Postback fires on change.
-record(ah_switch_button, {?AH_BASE(aihtml_switch_button),
                           body = [] :: aihtml_html:html(),
                           value = undefined :: undefined | aihtml_lib_choice:value(),
                           checked = false :: boolean(),
                           disabled = false :: boolean(),
                           size = undefined :: undefined | aihtml_lib_choice:size(),
                           on_label = undefined :: aihtml_html:html(),
                           off_label = undefined :: aihtml_html:html(),
                           locked = false :: boolean(),
                           width = undefined :: undefined | pos_integer(),
                           height = undefined :: undefined | pos_integer(),
                           thumb_size = undefined :: undefined | pos_integer()}).

-endif.
