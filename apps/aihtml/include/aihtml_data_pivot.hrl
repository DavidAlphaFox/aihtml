%% Element records of aihtml_data_pivot (designs/05-records.md). Field
%% names follow the catalog: flags and options are fields of the same
%% name, with the catalog's defaults (aihtml_data_pivot_tests checks that
%% they agree).
-ifndef(AIHTML_DATA_PIVOT_HRL).
-define(AIHTML_DATA_PIVOT_HRL, true).

-include("aihtml_element.hrl").

%% A field name: an atom or a binary (a map key of the data rows).
-type ah_pg_field_name() :: atom() | binary().
%% One data row: a map from field names to values. Numbers are
%% aggregated; texts, atoms, dates ({Y, M, D}) and numbers group; `null'
%% and `undefined' are blanks.
-type ah_pg_row() :: #{ah_pg_field_name() => term()} | [{ah_pg_field_name(), term()}].
-type ah_pg_agg() :: sum | count | avg | min | max | product.
%% A measure: a field (summed, or the field's default `agg'), `{Field,
%% Agg}', or a map with `field', `agg' and an optional `label'. A map
%% without `field' and with `agg => count' counts records.
-type ah_pg_value_spec() :: ah_pg_field_name() | {ah_pg_field_name(), ah_pg_agg()}
                          | #{field => ah_pg_field_name(), agg => ah_pg_agg(),
                              label => unicode:chardata()}.
%% The pivot layout, the component's value: the row dimensions, the
%% column dimensions and the measures, each in order.
-type ah_pg_layout() :: #{rows => [ah_pg_field_name()],
                          columns => [ah_pg_field_name()],
                          values => [ah_pg_value_spec()]}.
%% Number display: fixed `decimals' (default: integers as they are, other
%% numbers with 2), a `thousands' separator (default none), the `decimal'
%% separator (default "."), a `prefix' and a `suffix'.
-type ah_pg_format() :: #{decimals => non_neg_integer(), thousands => unicode:chardata(),
                          decimal => unicode:chardata(), prefix => unicode:chardata(),
                          suffix => unicode:chardata()}.
%% A field of the data: its name, and optionally a `label', the number
%% `format' used when it is a measure and its default `agg' when it is
%% dropped on the values.
-type ah_pg_field() :: ah_pg_field_name() | {ah_pg_field_name(), unicode:chardata()}
                     | #{name := ah_pg_field_name(), label => unicode:chardata(),
                         format => ah_pg_format(), agg => ah_pg_agg()}.
%% A member path: the keys of the dimensions from the outermost.
-type ah_pg_path() :: [number() | binary() | null].
%% The view state: expanded row and column members, row order (by key,
%% or by the values of one column: `col' is the member path the column
%% aggregates, [] for the grand total, `vi' the measure) and column order.
-type ah_pg_view() :: #{expanded_rows => [ah_pg_path()] | all,
                        expanded_cols => [ah_pg_path()] | all,
                        row_sort => null | #{by := key, dir := asc | desc}
                                   | #{by := value, dir := asc | desc,
                                       col := ah_pg_path(), vi := non_neg_integer()},
                        col_sort => asc | desc}.
-type ah_pg_locale() :: en | zh.

%% A pivot table: the rows are grouped by the row and column dimensions
%% of `value' (the layout) and each crossing shows the measures, with
%% subtotals and grand totals, aggregated on the server for the first
%% render. Postback fires on 'ah:cell-click' (Event.data's `cell' holds
%% the row and column members, see pivotgrid_cell/1). With `source' the
%% view changes are aggregated on the server (pivotgrid_rows/3), otherwise
%% in the browser from a JSON data island. Without an `id' one is
%% generated at render.
-record(ah_pivotgrid, {?AH_BASE(aihtml_data_pivot),
                       items = [] :: [ah_pg_row()],
                       value = #{} :: ah_pg_layout(),
                       name = undefined :: undefined | atom() | iodata(),
                       expand_all = false :: boolean(),
                       values_on_rows = false :: boolean(),
                       field_list = false :: boolean(),
                       fields = undefined :: undefined | [ah_pg_field()],
                       view = #{} :: ah_pg_view(),
                       row_subtotals = true :: boolean(),
                       col_subtotals = true :: boolean(),
                       grand_totals = true :: boolean(),
                       format = #{} :: ah_pg_format(),
                       height = undefined :: undefined | pos_integer() | iodata(),
                       locale = en :: ah_pg_locale(),
                       labels = #{} :: #{atom() => unicode:chardata()},
                       source = undefined :: undefined | aihtml_action:ref()}).

-endif.
