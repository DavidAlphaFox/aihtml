%% Element record of aihtml_timeline (designs/05-records.md). Field
%% names follow the catalog: modifier groups, flags and options are
%% fields of the same name, with the catalog's defaults (aihtml_timeline_tests
%% checks that they agree). Option fields default to what the component
%% used when the option was left out.
-ifndef(AIHTML_TIMELINE_HRL).
-define(AIHTML_TIMELINE_HRL, true).

-include("aihtml_element.hrl").

%% Events along an axis; cards with a description expand on click.
%% Postback fires on 'ah:toggle'.
-record(ah_timeline, {?AH_BASE(aihtml_timeline),
                      items = [] :: [aihtml_timeline:item()],
                      position = both :: both | near | far,
                      horizontal = false :: boolean(),
                      disabled = false :: boolean(),
                      collapsible = true :: boolean()}).

-endif.
