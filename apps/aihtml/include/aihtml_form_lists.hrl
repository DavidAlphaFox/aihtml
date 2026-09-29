%% Element records of aihtml_form_lists (designs/05-records.md). Field
%% names follow the catalog: modifier groups, flags and options are fields
%% of the same name, with the catalog's defaults (aihtml_form_lists_tests
%% checks that they agree).
-ifndef(AIHTML_FORM_LISTS_HRL).
-define(AIHTML_FORM_LISTS_HRL, true).

-include("aihtml_element.hrl").

%% A listbox or transfer item: a text that is both value and label,
%% `{Value, Label}', or a map with `value' and optionally `label',
%% `disabled', `group' (listbox: a header per group) and `icon' (listbox:
%% an image URL; transfer: a short text such as an emoji).
-type ah_fl_item() :: binary() | atom() | integer() | {term(), term()}
                    | #{value := term(), label => term(), disabled => boolean(),
                        group => term(), icon => iodata()}.
%% The children of a cascader node: a list, or `lazy' (loaded by the
%% cascader's `load' action when the node is opened).
-type ah_fl_children() :: [ah_fl_node()] | lazy.
%% A cascader node: a text that is both value and label, `{Value, Label}',
%% `{Value, Label, Children}', or a map with `value' and optionally
%% `label', `disabled' and `children'. A node without children is a leaf.
-type ah_fl_node() :: binary() | atom() | integer() | {term(), term()}
                    | {term(), term(), ah_fl_children()}
                    | #{value := term(), label => term(), disabled => boolean(),
                        children => ah_fl_children()}.
-type ah_fl_size() :: sm | lg.

%% A text field whose popup shows one menu column per level; the value is
%% the path of values to a leaf. Postback fires on change. `load' is an
%% action ref run when a `lazy' node opens. Without an `id' one is
%% generated at render.
-record(ah_cascader, {?AH_BASE(aihtml_form_lists),
                      items = [] :: [ah_fl_node()],
                      value = undefined :: undefined | [term()],
                      name = undefined :: undefined | atom() | iodata(),
                      size = undefined :: undefined | ah_fl_size(),
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

%% A focusable list with single or multiple selection and keyboard
%% navigation; postback fires on change. `value' is a list of values with
%% `multiple' or `checkboxes'. `search' is an action ref run (debounced)
%% as the user types in the filter. Without an `id' one is generated.
-record(ah_listbox, {?AH_BASE(aihtml_form_lists),
                     items = [] :: [ah_fl_item()],
                     value = undefined :: term(),
                     name = undefined :: undefined | atom() | iodata(),
                     disabled = false :: boolean(),
                     multiple = false :: boolean(),
                     checkboxes = false :: boolean(),
                     check_all = false :: boolean(),
                     filterable = false :: boolean(),
                     empty_text = <<"No data">> :: unicode:chardata(),
                     filter_placeholder = <<"Search">> :: unicode:chardata(),
                     check_all_label = <<"Select all">> :: unicode:chardata(),
                     search = undefined :: undefined | aihtml_action:ref()}).

%% Two lists with move buttons; `value' is the list of keys in the right
%% (target) list, in order. Postback fires on change. Without an `id' one
%% is generated at render.
-record(ah_transfer, {?AH_BASE(aihtml_form_lists),
                      items = [] :: [ah_fl_item()],
                      value = [] :: [term()],
                      name = undefined :: undefined | atom() | iodata(),
                      disabled = false :: boolean(),
                      no_filter = false :: boolean(),
                      source_title = <<"Source">> :: unicode:chardata(),
                      target_title = <<"Target">> :: unicode:chardata(),
                      filter_placeholder = <<"Search">> :: unicode:chardata(),
                      empty_text = <<"No data">> :: unicode:chardata()}).

-endif.
