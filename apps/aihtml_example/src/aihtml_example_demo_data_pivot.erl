%% @doc Demos of the pivot table (aihtml_data_pivot), shown on
%% /components/pivotgrid. Each function is one example, written the way
%% an application writes it; the docs page prints its source under it.
%%
%% The module is also the action module of the demos that talk to the
%% server: the cell click, the remote aggregation and the export buttons.
-module(aihtml_example_demo_data_pivot).
-behaviour(aihtml_action).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0, action/4]).
-export([pivot_basic/0, pivot_expanded/0, pivot_measures/0, pivot_values_on_rows/0,
         pivot_field_list/0, pivot_no_totals/0, pivot_sorted/0, pivot_cell_click/0,
         pivot_remote/0, pivot_export/0, pivot_empty/0, pivot_record/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => pivotgrid, title => <<"PivotGrid">>,
       summary => <<"数据透视表：按行列维度分组聚合，服务端计算，支持展开折叠、排序、拖动字段重新透视和导出 Excel。"/utf8>>,
       demos => [{<<"基本透视表"/utf8>>, pivot_basic},
                 {<<"全部展开、中文与数字格式"/utf8>>, pivot_expanded},
                 {<<"多个度量"/utf8>>, pivot_measures},
                 {<<"度量放在行上"/utf8>>, pivot_values_on_rows},
                 {<<"字段列表：拖动重新透视"/utf8>>, pivot_field_list},
                 {<<"不显示小计与合计"/utf8>>, pivot_no_totals},
                 {<<"按值排序的初始视图"/utf8>>, pivot_sorted},
                 {<<"点击单元格通知服务端"/utf8>>, pivot_cell_click},
                 {<<"服务端聚合（remote 模式）"/utf8>>, pivot_remote},
                 {<<"服务端触发导出"/utf8>>, pivot_export},
                 {<<"空数据"/utf8>>, pivot_empty},
                 {<<"record 写法"/utf8>>, pivot_record}]}].

%%%===================================================================
%%% Demos
%%%===================================================================

-spec pivot_basic() -> aihtml:html().
pivot_basic() ->
    pivotgrid(sales(), #{rows => [country, city], columns => [year, quarter],
                         values => [{sales, sum}]},
              [], [{height, 320}]).

-spec pivot_expanded() -> aihtml:html().
pivot_expanded() ->
    pivotgrid(sales(), #{rows => [country, city], columns => [year], values => [sales]},
              [expand_all],
              [{fields, fields()}, {locale, zh}, {height, 360}]).

-spec pivot_measures() -> aihtml:html().
pivot_measures() ->
    pivotgrid(sales(), #{rows => [country], columns => [product],
                         values => [{sales, sum}, {units, avg}, {sales, count}]},
              [], [{fields, fields()}, {locale, zh}]).

-spec pivot_values_on_rows() -> aihtml:html().
pivot_values_on_rows() ->
    pivotgrid(sales(), #{rows => [country], columns => [year],
                         values => [{sales, sum}, {units, sum}]},
              [values_on_rows], [{fields, fields()}, {locale, zh}]).

-spec pivot_field_list() -> aihtml:html().
pivot_field_list() ->
    pivotgrid(sales(), #{rows => [product], columns => [year], values => [sales]},
              [field_list], [{fields, fields()}, {locale, zh}, {height, 380}]).

-spec pivot_no_totals() -> aihtml:html().
pivot_no_totals() ->
    pivotgrid(sales(), #{rows => [country, city], columns => [year, quarter], values => [units]},
              [expand_all],
              [{fields, fields()}, {locale, zh}, {row_subtotals, false},
               {col_subtotals, false}, {grand_totals, false}, {height, 320}]).

-spec pivot_sorted() -> aihtml:html().
pivot_sorted() ->
    pivotgrid(sales(), #{rows => [city], columns => [year], values => [sales]},
              [], [{fields, fields()}, {locale, zh},
                   {view, #{row_sort => #{by => value, col => [], vi => 0, dir => desc}}}]).

%% A click on a value cell runs action(cell, ...) below, which reads the
%% cell's members with pivotgrid_cell/1.
-spec pivot_cell_click() -> aihtml:html().
pivot_cell_click() ->
    'div'([pivotgrid(sales(), #{rows => [country], columns => [product], values => [sales]},
                     [], [{fields, fields()}, {locale, zh},
                          on('ah:cell-click', {?MODULE, cell, <<"pivot-cell">>})]),
           p(<<"点击一个数值单元格"/utf8>>, [<<"text-sm text-muted mt-2">>],
             [{id, <<"pivot-cell">>}])],
          [], []).

%% No data goes to the page: every expand, sort or layout change runs
%% action(pivot, ...) below, which aggregates on the server.
-spec pivot_remote() -> aihtml:html().
pivot_remote() ->
    pivotgrid(sales(), #{rows => [country, city], columns => [year, quarter], values => [sales]},
              [field_list],
              [{fields, fields()}, {locale, zh}, {height, 380},
               {source, {?MODULE, pivot, #{}}}]).

-spec pivot_export() -> aihtml:html().
pivot_export() ->
    'div'([toolbar([button(<<"导出 Excel"/utf8>>, xlsx, [], [on(click, {?MODULE, export, xlsx})]),
                    button(<<"导出 CSV"/utf8>>, csv, [], [on(click, {?MODULE, export, csv})]),
                    button(<<"全部展开"/utf8>>, expand, [], [on(click, {?MODULE, export, expand})])]),
           pivotgrid(sales(), #{rows => [country, city], columns => [year], values => [sales]},
                     [], [{id, <<"pivot-export">>}, {fields, fields()}, {locale, zh}])],
          [], []).

-spec pivot_empty() -> aihtml:html().
pivot_empty() ->
    pivotgrid([], #{rows => [country], columns => [year], values => [sales]},
              [], [{fields, [country, year, sales]}, {locale, zh}]).

%% The same component as a record: options are checked field names, and
%% the postback runs action(cell, ...) below on a cell click.
-spec pivot_record() -> aihtml:html().
pivot_record() ->
    'div'([#ah_pivotgrid{items = sales(),
                         value = #{rows => [product, country], columns => [quarter],
                                   values => [{units, sum}]},
                         fields = fields(), locale = zh, col_subtotals = false,
                         view = #{expanded_rows => [[<<"手机"/utf8>>]]},
                         format = #{thousands => <<",">>},
                         postback = {cell, <<"pivot-cell-2">>}},
           p(<<"点击一个数值单元格"/utf8>>, [<<"text-sm text-muted mt-2">>],
             [{id, <<"pivot-cell-2">>}])],
          [], []).

%%%===================================================================
%%% Actions
%%%===================================================================

-spec action(atom(), term(), aihtml_action:event(), aihtml_action:ctx()) -> ok.
action(cell, Target, Event, Ctx) ->
    #{filter := Filter, text := Text} = pivotgrid_cell(Event),
    Records = [R || R <- sales(), matches(R, Filter)],
    aihtml_action:html(Ctx, {id, Target},
                       [<<"服务端收到："/utf8>>, Text, <<"，条件 "/utf8>>,
                        iolist_to_binary(json:encode(Filter)),
                        <<"，共 "/utf8>>, integer_to_binary(length(Records)),
                        <<" 条明细"/utf8>>]);
action(pivot, _Args, Event, Ctx) ->
    %% a real application would query only the fields of the layout
    #{layout := #{rows := _, columns := _}} = pivotgrid_view(Event),
    pivotgrid_rows(Ctx, Event, sales());
action(export, xlsx, _Event, Ctx) ->
    aihtml_action:call(Ctx, {id, <<"pivot-export">>}, exportXlsx,
                       [#{filename => <<"sales.xlsx">>, sheetName => <<"销售"/utf8>>}]);
action(export, csv, _Event, Ctx) ->
    aihtml_action:call(Ctx, {id, <<"pivot-export">>}, exportCsv,
                       [#{filename => <<"sales.csv">>}]);
action(export, expand, _Event, Ctx) ->
    aihtml_action:call(Ctx, {id, <<"pivot-export">>}, expandAll, []).

matches(Row, Filter) ->
    maps:fold(fun(F, V, Acc) ->
                      Acc andalso maps:get(binary_to_existing_atom(F), Row, null) =:= V
              end, true, Filter).

%%%===================================================================
%%% Data
%%%===================================================================

toolbar(Children) ->
    'div'(Children, [<<"flex gap-2 mb-2">>], []).

fields() ->
    [{country, <<"国家"/utf8>>}, {city, <<"城市"/utf8>>}, {year, <<"年份"/utf8>>},
     {quarter, <<"季度"/utf8>>}, {product, <<"产品"/utf8>>},
     #{name => sales, label => <<"销售额"/utf8>>,
       format => #{thousands => <<",">>, prefix => <<"¥"/utf8>>}},
     #{name => units, label => <<"数量"/utf8>>, format => #{thousands => <<",">>}}].

%% Made-up sales, the same on every render: 7 cities, 2 years, 4
%% quarters, 2 products.
sales() ->
    Cities = [{<<"中国"/utf8>>, <<"北京"/utf8>>}, {<<"中国"/utf8>>, <<"上海"/utf8>>},
              {<<"中国"/utf8>>, <<"深圳"/utf8>>}, {<<"美国"/utf8>>, <<"纽约"/utf8>>},
              {<<"美国"/utf8>>, <<"洛杉矶"/utf8>>}, {<<"日本"/utf8>>, <<"东京"/utf8>>},
              {<<"日本"/utf8>>, <<"大阪"/utf8>>}],
    [#{country => Country, city => City, year => Year, quarter => Q, product => Product,
       units => Units, sales => Units * Price}
     || {Ci, {Country, City}} <- lists:enumerate(Cities),
        Year <- [2023, 2024],
        {Qi, Q} <- lists:enumerate([<<"Q1">>, <<"Q2">>, <<"Q3">>, <<"Q4">>]),
        {Pi, Product, Price} <- [{1, <<"电脑"/utf8>>, 5200}, {2, <<"手机"/utf8>>, 2600}],
        Units <- [20 + (Ci * 37 + Qi * 11 + Pi * 17 + (Year - 2023) * 23) rem 60]].
