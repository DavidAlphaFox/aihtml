%% The element record of aihtml_ribbon (designs/05-records.md). Field
%% names follow the catalog: modifier groups, flags and options are fields
%% of the same name, with the catalog's defaults (aihtml_ribbon_tests
%% checks that they agree).
-ifndef(AIHTML_RIBBON_HRL).
-define(AIHTML_RIBBON_HRL, true).

-include("aihtml_element.hrl").

%% An Office-style ribbon: tabs over panels of command groups. `value' is
%% the active tab's key (a user switch fires change); postback fires on
%% 'ah:command' (a command was clicked: Event.data.command is its key,
%% Event.data.pressed the new state of a toggle).
-record(ah_ribbon, {?AH_BASE(aihtml_ribbon),
                    items = [] :: [aihtml_ribbon:tab()],
                    value = undefined :: term(),
                    name = undefined :: undefined | atom() | iodata(),
                    position = top :: aihtml_ribbon:position(),
                    mode = default :: aihtml_ribbon:mode(),
                    color = undefined :: undefined | aihtml_ribbon:color(),
                    animation = undefined :: undefined | aihtml_ribbon:animation(),
                    collapsible = false :: boolean(),
                    selection_mode = click :: click | hover,
                    width = undefined :: undefined | integer() | iodata(),
                    height = undefined :: undefined | integer() | iodata(),
                    disabled = false :: boolean()}).

-endif.
