%% Element record of aihtml_datatable (designs/05-records.md). Field names follow
%% the catalog: modifier groups, flags and options are fields of the same
%% name, with the catalog's defaults (aihtml_datatable_tests checks that they
%% agree). The column model is aihtml_lib_table's.
-ifndef(AIHTML_DATATABLE_HRL).
-define(AIHTML_DATATABLE_HRL, true).

-include("aihtml_element.hrl").

%% A data table: sorting, filtering (filter row, search bar or advanced
%% conditions), paging, selection, row details, inline editing, column
%% resize and chooser. Local mode: every row is rendered and the browser
%% sorts, filters and pages. Remote mode (`source', an action ref): each
%% view change fires 'ah:query' on the root and the action answers with
%% aihtml_datatable:datatable_rows/3. With `href' (a URL template) the
%% pager's buttons are links a crawler can follow. Postback fires on change
%% (Event.value: the selected keys, comma separated); cell edits go to
%% the `edit' action.
-record(ah_datatable, {?AH_BASE(aihtml_datatable),
                       columns = [] :: [aihtml_lib_table:column()],
                       rows = [] :: [aihtml_lib_table:row()],
                       value = undefined :: term(),
                       name = undefined :: undefined | atom() | iodata(),
                       disabled = false :: boolean(),
                       selection_mode = single :: aihtml_lib_table:selection(),
                       key_field = id :: atom() | binary(),
                       sortable = true :: boolean(),
                       sort = undefined :: aihtml_lib_table:sort(),
                       filter = none :: none | row | search | advanced,
                       filters = #{} :: #{atom() | binary() => aihtml_datatable:filter()},
                       search = <<>> :: iodata(),
                       page_size = undefined :: undefined | pos_integer(),
                       page = 1 :: pos_integer(),
                       page_sizes = [5, 10, 25, 50] :: [pos_integer()],
                       total = undefined :: undefined | non_neg_integer(),
                       source = undefined :: undefined | aihtml_action:ref(),
                       href = undefined :: undefined | iodata(),
                       editable = false :: boolean(),
                       edit = undefined :: undefined | aihtml_action:ref(),
                       row_details = undefined :: undefined | fun((map()) -> aihtml_html:html()),
                       expanded = [] :: [term()],
                       resizable = false :: boolean(),
                       column_chooser = false :: boolean(),
                       alt_rows = true :: boolean(),
                       hover = true :: boolean(),
                       show_header = true :: boolean(),
                       height = undefined :: aihtml_lib_table:css_length(),
                       empty_text = <<"No data to display">> :: aihtml_html:html(),
                       texts = #{} :: #{atom() => unicode:chardata()}}).

-endif.
