%% Element record of aihtml_combobox (designs/05-records.md). Field names
%% follow the catalog: flags and options are fields of the same name,
%% with the catalog's defaults (aihtml_combobox_tests checks that they
%% agree).
-ifndef(AIHTML_COMBOBOX_HRL).
-define(AIHTML_COMBOBOX_HRL, true).

-include("aihtml_element.hrl").

%% An editable field with a filtered list; postback fires on change.
%% `value' is a list of values with `multiple' or `checkboxes'. `search'
%% is an action ref run (debounced) as the user types. Without an `id'
%% one is generated at render.
-record(ah_combobox, {?AH_BASE(aihtml_combobox),
                      items = [] :: [aihtml_combobox:item()],
                      value = undefined :: term(),
                      name = undefined :: undefined | atom() | iodata(),
                      disabled = false :: boolean(),
                      no_arrow = false :: boolean(),
                      multiple = false :: boolean(),
                      checkboxes = false :: boolean(),
                      free_text = false :: boolean(),
                      placeholder = <<>> :: undefined | unicode:chardata(),
                      search_mode = contains_ignore_case :: aihtml_combobox:search_mode(),
                      min_length = undefined :: undefined | non_neg_integer(),
                      empty_text = undefined :: undefined | unicode:chardata(),
                      dropdown_height = undefined :: undefined | pos_integer(),
                      search = undefined :: undefined | aihtml_action:ref()}).

-endif.
