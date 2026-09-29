%% Element records of aihtml_data_tables (designs/05-records.md). Field
%% names follow the catalog: modifier groups, flags and options are fields
%% of the same name, with the catalog's defaults (aihtml_data_tables_tests
%% checks that they agree).
-ifndef(AIHTML_DATA_TABLES_HRL).
-define(AIHTML_DATA_TABLES_HRL, true).

-include("aihtml_element.hrl").

%% A column: a field name alone, `{Field, Title}', or a map. `field' is
%% the key of the value in each row (an atom key also finds the same name
%% as a binary key); `render' turns a value into HTML (fun(Value, Row));
%% `type' drives sorting, the advanced filter and the cell editor.
-type ah_dtb_column() :: atom() | binary()
                       | {atom() | binary(), aihtml_html:html()}
                       | #{field := atom() | binary(),
                           title => aihtml_html:html(),
                           width => pos_integer() | binary(),
                           align => left | center | right,
                           sortable => boolean(),
                           filterable => boolean(),
                           editable => boolean(),
                           type => text | number | date | checkbox,
                           render => fun((term(), map()) -> aihtml_html:html()),
                           class => binary(),
                           hidden => boolean()}.
%% A row: a map from field names to values (binaries, numbers, atoms,
%% or HTML).
-type ah_dtb_row() :: map().
%% Selection mode of both tables.
-type ah_dtb_selection() :: none | single | multiple | checkbox.
%% A sort: the column's field and the direction.
-type ah_dtb_sort() :: undefined | {atom() | binary(), asc | desc}.
%% A column filter: a text (contains, case-insensitive) or an advanced
%% condition with its operand.
-type ah_dtb_condition() :: contains | not_contains | equals | not_equals
                          | starts_with | ends_with | gt | gte | lt | lte
                          | empty | not_empty.
-type ah_dtb_filter() :: iodata() | {ah_dtb_condition(), iodata() | number()}.
%% A CSS length: pixels or a literal such as <<"60vh">>.
-type ah_dtb_length() :: undefined | pos_integer() | binary().

%% A table whose rows form a tree: the tree column is indented with an
%% expand arrow, siblings sort within their parent, rows are selected by
%% key (keyboard: the treegrid pattern). `items' are nested (children in
%% `children_field') or flat (parent key in `parent_field'); a row whose
%% children field is `lazy' gets its children from the `load' action
%% (aihtml_data_tables:treegrid_children/3). Postback fires on change
%% (Event.value: the selected keys, comma separated).
-record(ah_treegrid, {?AH_BASE(aihtml_data_tables),
                      columns = [] :: [ah_dtb_column()],
                      items = [] :: [ah_dtb_row()],
                      value = undefined :: term(),
                      name = undefined :: undefined | atom() | iodata(),
                      disabled = false :: boolean(),
                      selection_mode = single :: ah_dtb_selection(),
                      key_field = id :: atom() | binary(),
                      children_field = children :: atom() | binary(),
                      parent_field = parent_id :: atom() | binary(),
                      tree_column = undefined :: undefined | atom() | binary(),
                      expanded = [] :: [term()] | all,
                      sortable = true :: boolean(),
                      sort = undefined :: ah_dtb_sort(),
                      indent = 24 :: non_neg_integer(),
                      alt_rows = true :: boolean(),
                      hover = true :: boolean(),
                      show_header = true :: boolean(),
                      resizable = false :: boolean(),
                      height = undefined :: ah_dtb_length(),
                      empty_text = <<"No data to display">> :: aihtml_html:html(),
                      load = undefined :: undefined | aihtml_action:ref()}).

%% A data table: sorting, filtering (filter row, search bar or advanced
%% conditions), paging, selection, row details, inline editing, column
%% resize and chooser. Local mode: every row is rendered and the browser
%% sorts, filters and pages. Remote mode (`source', an action ref): each
%% view change fires 'ah:query' on the root and the action answers with
%% aihtml_data_tables:datatable_rows/3. Postback fires on change
%% (Event.value: the selected keys, comma separated); cell edits go to
%% the `edit' action.
-record(ah_datatable, {?AH_BASE(aihtml_data_tables),
                       columns = [] :: [ah_dtb_column()],
                       rows = [] :: [ah_dtb_row()],
                       value = undefined :: term(),
                       name = undefined :: undefined | atom() | iodata(),
                       disabled = false :: boolean(),
                       selection_mode = single :: ah_dtb_selection(),
                       key_field = id :: atom() | binary(),
                       sortable = true :: boolean(),
                       sort = undefined :: ah_dtb_sort(),
                       filter = none :: none | row | search | advanced,
                       filters = #{} :: #{atom() | binary() => ah_dtb_filter()},
                       search = <<>> :: iodata(),
                       page_size = undefined :: undefined | pos_integer(),
                       page = 1 :: pos_integer(),
                       page_sizes = [5, 10, 25, 50] :: [pos_integer()],
                       total = undefined :: undefined | non_neg_integer(),
                       source = undefined :: undefined | aihtml_action:ref(),
                       editable = false :: boolean(),
                       edit = undefined :: undefined | aihtml_action:ref(),
                       row_details = undefined :: undefined | fun((map()) -> aihtml_html:html()),
                       expanded = [] :: [term()],
                       resizable = false :: boolean(),
                       column_chooser = false :: boolean(),
                       alt_rows = true :: boolean(),
                       hover = true :: boolean(),
                       show_header = true :: boolean(),
                       height = undefined :: ah_dtb_length(),
                       empty_text = <<"No data to display">> :: aihtml_html:html(),
                       texts = #{} :: #{atom() => unicode:chardata()}}).

-endif.
