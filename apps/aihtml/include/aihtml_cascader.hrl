%% Element record of aihtml_cascader (designs/05-records.md). Field names
%% follow the catalog: modifier groups, flags and options are fields of
%% the same name, with the catalog's defaults (aihtml_cascader_tests checks
%% that they agree).
-ifndef(AIHTML_CASCADER_HRL).
-define(AIHTML_CASCADER_HRL, true).

-include("aihtml_element.hrl").

%% A text field whose popup shows one menu column per level; the value is
%% the path of values to a leaf. Postback fires on change. `load' is an
%% action ref run when a `lazy' node opens. Without an `id' one is
%% generated at render.
-record(ah_cascader, {?AH_BASE(aihtml_cascader),
                      items = [] :: [aihtml_cascader:cascader_node()],
                      value = undefined :: undefined | [term()],
                      name = undefined :: undefined | atom() | iodata(),
                      size = undefined :: undefined | sm | lg,
                      disabled = false :: boolean(),
                      filterable = false :: boolean(),
                      change_on_select = false :: boolean(),
                      no_arrow = false :: boolean(),
                      no_clear = false :: boolean(),
                      placeholder = <<"Please select">> :: undefined | unicode:chardata(),
                      separator = <<" / ">> :: unicode:chardata(),
                      popup_height = 240 :: pos_integer(),
                      empty_text = <<"No results found">> :: unicode:chardata(),
                      load = undefined :: undefined | aihtml_action:ref()}).

-endif.
