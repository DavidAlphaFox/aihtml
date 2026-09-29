%% Element record of aihtml_datagrid (designs/05-records.md). Field names
%% follow the catalog: the modifier group, flags and options are fields of
%% the same name, with the catalog's defaults (aihtml_datagrid_tests
%% checks that they agree). The field types are aihtml_datagrid's.
-ifndef(AIHTML_DATAGRID_HRL).
-define(AIHTML_DATAGRID_HRL, true).

-include("aihtml_element.hrl").

%% A data grid: sortable, filterable and pageable rows with selection,
%% keyboard navigation, column resize / pinning / hiding, inline editing,
%% grouping with aggregates and CSV / Excel / PDF export. Postback fires
%% on change (the selection; Event.value is the selected keys, comma
%% separated). Local mode: the rows are rendered here and the browser
%% sorts, filters, pages and groups them. Remote mode (`source' set): every
%% view change runs that action, which answers with
%% aihtml_datagrid:datagrid_rows/4. Without an `id' one is generated.
-record(ah_datagrid, {?AH_BASE(aihtml_datagrid),
                      columns = [] :: [aihtml_datagrid:column()],
                      rows = [] :: [aihtml_datagrid:row()],
                      selection = single :: none | single | multi | checkbox,
                      filter_row = false :: boolean(),
                      pageable = false :: boolean(),
                      statusbar = false :: boolean(),
                      value = [] :: [term()],
                      key_field = id :: aihtml_datagrid:key(),
                      height = undefined :: undefined | pos_integer() | iodata(),
                      page = 1 :: pos_integer(),
                      page_size = 10 :: pos_integer(),
                      page_sizes = [10, 20, 50, 100] :: [pos_integer()],
                      sort = [] :: [{aihtml_datagrid:key(), asc | desc}],
                      filters = [] :: [{aihtml_datagrid:key(), unicode:chardata()}]
                                   | #{aihtml_datagrid:key() => unicode:chardata()},
                      group_by = [] :: [aihtml_datagrid:key()],
                      edit_mode = dblclick :: dblclick | click,
                      column_menu = true :: boolean(),
                      toolbar = [] :: [aihtml_datagrid:tool()],
                      export_name = <<"data">> :: unicode:chardata(),
                      labels = #{} :: #{atom() => unicode:chardata()},
                      source = undefined :: undefined | aihtml_action:ref(),
                      total = undefined :: undefined | non_neg_integer(),
                      name = undefined :: undefined | atom() | iodata()}).

-endif.
