%% Element record of aihtml_transfer (designs/05-records.md). Field names
%% follow the catalog: modifier groups, flags and options are fields of
%% the same name, with the catalog's defaults (aihtml_transfer_tests checks
%% that they agree).
-ifndef(AIHTML_TRANSFER_HRL).
-define(AIHTML_TRANSFER_HRL, true).

-include("aihtml_element.hrl").

%% Two lists with move buttons; `value' is the list of keys in the right
%% (target) list, in order. Postback fires on change. Without an `id' one
%% is generated at render.
-record(ah_transfer, {?AH_BASE(aihtml_transfer),
                      items = [] :: [aihtml_lib_list:item()],
                      value = [] :: [term()],
                      name = undefined :: undefined | atom() | iodata(),
                      disabled = false :: boolean(),
                      no_filter = false :: boolean(),
                      source_title = <<"Source">> :: unicode:chardata(),
                      target_title = <<"Target">> :: unicode:chardata(),
                      filter_placeholder = <<"Search">> :: unicode:chardata(),
                      empty_text = <<"No data">> :: unicode:chardata()}).

-endif.
