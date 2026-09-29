%% Element records of aihtml_data_grid (designs/05-records.md). Field
%% names follow the catalog: the modifier group, flags and options are
%% fields of the same name, with the catalog's defaults
%% (aihtml_data_grid_tests checks that they agree).
-ifndef(AIHTML_DATA_GRID_HRL).
-define(AIHTML_DATA_GRID_HRL, true).

-include("aihtml_element.hrl").

%% A column key: the key of the cell's value in each row map (an atom
%% also finds a binary key of the same name, and the other way round).
-type ah_dg_key() :: atom() | binary().
%% How a cell shows its value. text (default), number, date, bool (the
%% yes / no labels), select (the label of the matching option), textarea
%% are plain text and can be edited; progress, rating, image, link, badge
%% and command are display types.
-type ah_dg_type() :: text | number | date | bool | select | textarea
                    | progress | rating | image | link | badge | command.
%% An aggregate of a column's numeric values (status bar, group rows).
-type ah_dg_aggregate() :: sum | avg | count | min | max.
%% A column: a key, `{Key, Title}', or a map. Map keys (all optional but
%% `key'): title, width (px, default 100), min_width (default 40), align
%% (left | center | right), type, format ("n2" number, "c2" currency,
%% "p1" percentage, "yyyy-MM-dd HH:mm" date), currency (the symbol of
%% "c" formats, default "¥"), sortable, filterable, resizable, groupable
%% (default true), editable, pinned, hidden (default false), options (of
%% a select column: values or {Value, Label}), aggregates, badges (badge
%% columns: #{Value => {Text, success | warning | danger | info |
%% default}}), max (progress: 100, rating: 5), link_text, target (link
%% columns, default "_blank"), commands (command columns: [{Name,
%% Label}], a click fires 'ah:command'), render ({Mod, Fun}: the cell
%% content is Mod:Fun(Value, Row), any html).
-type ah_dg_column() :: ah_dg_key()
                      | {ah_dg_key(), unicode:chardata()}
                      | #{key := ah_dg_key(), title => unicode:chardata(),
                          width => pos_integer(), min_width => pos_integer(),
                          align => left | center | right, type => ah_dg_type(),
                          format => unicode:chardata(), currency => unicode:chardata(),
                          sortable => boolean(), filterable => boolean(),
                          resizable => boolean(), groupable => boolean(),
                          editable => boolean(), pinned => boolean(), hidden => boolean(),
                          options => [term() | {term(), unicode:chardata()}],
                          aggregates => [ah_dg_aggregate()],
                          badges => #{term() => {unicode:chardata(), atom()}},
                          max => number(), link_text => unicode:chardata(),
                          target => unicode:chardata(),
                          commands => [{atom() | binary(), unicode:chardata()}],
                          render => {module(), atom()}}.
%% A row: a map from column keys to values; the `key_field' value
%% identifies it (selection, edits, row ids).
-type ah_dg_row() :: #{term() => term()}.
%% A toolbar entry: export buttons (the export runs in the browser),
%% a search box over all columns, a separator, a flexible spacer, or a
%% custom button (`{Name, Label}' or a map) whose click fires
%% 'ah:toolbar' with its name.
-type ah_dg_tool() :: export_csv | export_xlsx | export_pdf | search | separator | spacer
                    | {atom() | binary(), unicode:chardata()}
                    | #{name := atom() | binary(), label := unicode:chardata(),
                        icon => unicode:chardata()}.

%% A data grid: sortable, filterable and pageable rows with selection,
%% keyboard navigation, column resize / pinning / hiding, inline editing,
%% grouping with aggregates and CSV / Excel / PDF export. Postback fires
%% on change (the selection; Event.value is the selected keys, comma
%% separated). Local mode: the rows are rendered here and the browser
%% sorts, filters, pages and groups them. Remote mode (`source' set): every
%% view change runs that action, which answers with
%% aihtml_data_grid:datagrid_rows/4. Without an `id' one is generated.
-record(ah_datagrid, {?AH_BASE(aihtml_data_grid),
                      columns = [] :: [ah_dg_column()],
                      rows = [] :: [ah_dg_row()],
                      selection = single :: none | single | multi | checkbox,
                      filter_row = false :: boolean(),
                      pageable = false :: boolean(),
                      statusbar = false :: boolean(),
                      value = [] :: [term()],
                      key_field = id :: ah_dg_key(),
                      height = undefined :: undefined | pos_integer() | iodata(),
                      page = 1 :: pos_integer(),
                      page_size = 10 :: pos_integer(),
                      page_sizes = [10, 20, 50, 100] :: [pos_integer()],
                      sort = [] :: [{ah_dg_key(), asc | desc}],
                      filters = [] :: [{ah_dg_key(), unicode:chardata()}]
                                   | #{ah_dg_key() => unicode:chardata()},
                      group_by = [] :: [ah_dg_key()],
                      edit_mode = dblclick :: dblclick | click,
                      column_menu = true :: boolean(),
                      toolbar = [] :: [ah_dg_tool()],
                      export_name = <<"data">> :: unicode:chardata(),
                      labels = #{} :: #{atom() => unicode:chardata()},
                      source = undefined :: undefined | aihtml_action:ref(),
                      total = undefined :: undefined | non_neg_integer(),
                      name = undefined :: undefined | atom() | iodata()}).

-endif.
