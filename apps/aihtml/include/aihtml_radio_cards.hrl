%% Element record of aihtml_radio_cards (designs/05-records.md). Field names
%% follow the catalog: modifier groups, flags and options are fields of
%% the same name, with the catalog's defaults (aihtml_radio_cards_tests checks
%% that they agree).
-ifndef(AIHTML_RADIO_CARDS_HRL).
-define(AIHTML_RADIO_CARDS_HRL, true).

-include("aihtml_element.hrl").

%% Selectable cards (item Opts description, icon). Postback fires on
%% change of the root.
-record(ah_radio_cards, {?AH_BASE(aihtml_radio_cards),
                         items = [] :: [aihtml_lib_choice:item()],
                         value = undefined :: undefined | aihtml_lib_choice:value(),
                         name = undefined :: aihtml_lib_choice:name(),
                         disabled = false :: boolean(),
                         required = false :: boolean(),
                         form = undefined :: aihtml_lib_choice:name(),
                         columns = auto :: aihtml_radio_cards:columns(),
                         align = center :: aihtml_radio_cards:align()}).

-endif.
