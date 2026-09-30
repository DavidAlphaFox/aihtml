%% The element record of aihtml_pivotgrid (designs/05-records.md). Field
%% names follow the catalog: flags and options are fields of the same
%% name, with the catalog's defaults (aihtml_pivotgrid_tests checks that
%% they agree).
-ifndef(AIHTML_PIVOTGRID_HRL).
-define(AIHTML_PIVOTGRID_HRL, true).

-include("aihtml_element.hrl").

%% A pivot table: the rows are grouped by the row and column dimensions
%% of `value' (the layout) and each crossing shows the measures, with
%% subtotals and grand totals, aggregated on the server for the first
%% render. Postback fires on 'ah:cell-click' (Event.data's `cell' holds
%% the row and column members, see pivotgrid_cell/1). With `source' the
%% view changes are aggregated on the server (pivotgrid_rows/3), otherwise
%% in the browser from a JSON data island. Without an `id' one is
%% generated at render.
-record(ah_pivotgrid, {?AH_BASE(aihtml_pivotgrid),
                       items = [] :: [aihtml_pivotgrid:row()],
                       value = #{} :: aihtml_pivotgrid:layout(),
                       name = undefined :: undefined | atom() | iodata(),
                       expand_all = false :: boolean(),
                       values_on_rows = false :: boolean(),
                       field_list = false :: boolean(),
                       fields = undefined :: undefined | [aihtml_pivotgrid:field()],
                       view = #{} :: aihtml_pivotgrid:view(),
                       row_subtotals = true :: boolean(),
                       col_subtotals = true :: boolean(),
                       grand_totals = true :: boolean(),
                       format = #{} :: aihtml_pivotgrid:format(),
                       height = undefined :: undefined | pos_integer() | iodata(),
                       locale :: aihtml_pivotgrid:locale() | undefined,   % undefined: the page's
                       labels = #{} :: #{atom() => unicode:chardata()},
                       source = undefined :: undefined | aihtml_action:ref()}).

-endif.
