%% Element record of aihtml_button_group (designs/05-records.md). Field names
%% follow the catalog: modifier groups, flags and options are fields of
%% the same name, with the catalog's defaults (aihtml_button_group_tests checks
%% that they agree).
-ifndef(AIHTML_BUTTON_GROUP_HRL).
-define(AIHTML_BUTTON_GROUP_HRL, true).

-include("aihtml_element.hrl").

%% Joined buttons. `value' is the selected item in radio mode, a list (or
%% "a,b") in checkbox mode, ignored in default mode. Postback fires on
%% change in radio and checkbox mode, on click in default mode.
-record(ah_button_group, {?AH_BASE(aihtml_button_group),
                          items = [] :: [aihtml_lib_button:item()],
                          value = undefined :: term(),
                          name = undefined :: undefined | atom() | iodata(),
                          mode = default :: default | radio | checkbox,
                          orientation = horizontal :: horizontal | vertical,
                          shape = rounded :: rounded | square,
                          fill = undefined :: undefined | filled | outlined,
                          disabled = false :: boolean()}).

-endif.
