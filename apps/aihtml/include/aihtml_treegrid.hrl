%% Element record of aihtml_treegrid (designs/05-records.md). Field names follow
%% the catalog: modifier groups, flags and options are fields of the same
%% name, with the catalog's defaults (aihtml_treegrid_tests checks that they
%% agree). The column model is aihtml_lib_table's.
-ifndef(AIHTML_TREEGRID_HRL).
-define(AIHTML_TREEGRID_HRL, true).

-include("aihtml_element.hrl").

%% A table whose rows form a tree: the tree column is indented with an
%% expand arrow, siblings sort within their parent, rows are selected by
%% key (keyboard: the treegrid pattern). `items' are nested (children in
%% `children_field') or flat (parent key in `parent_field'); a row whose
%% children field is `lazy' gets its children from the `load' action
%% (aihtml_treegrid:treegrid_children/3). Postback fires on change
%% (Event.value: the selected keys, comma separated).
-record(ah_treegrid, {?AH_BASE(aihtml_treegrid),
                      columns = [] :: [aihtml_lib_table:column()],
                      items = [] :: [aihtml_lib_table:row()],
                      value = undefined :: term(),
                      name = undefined :: undefined | atom() | iodata(),
                      disabled = false :: boolean(),
                      selection_mode = single :: aihtml_lib_table:selection(),
                      key_field = id :: atom() | binary(),
                      children_field = children :: atom() | binary(),
                      parent_field = parent_id :: atom() | binary(),
                      tree_column = undefined :: undefined | atom() | binary(),
                      expanded = [] :: [term()] | all,
                      sortable = true :: boolean(),
                      sort = undefined :: aihtml_lib_table:sort(),
                      indent = 24 :: non_neg_integer(),
                      alt_rows = true :: boolean(),
                      hover = true :: boolean(),
                      show_header = true :: boolean(),
                      resizable = false :: boolean(),
                      height = undefined :: aihtml_lib_table:css_length(),
                      empty_text = <<"No data to display">> :: aihtml_html:html(),
                      load = undefined :: undefined | aihtml_action:ref()}).

-endif.
