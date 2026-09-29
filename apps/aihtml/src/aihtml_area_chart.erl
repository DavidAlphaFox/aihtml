%%%-------------------------------------------------------------------
%%% @doc The area chart, ported from sigil (data/area_chart): a smooth
%%% line chart with filled areas (or a plain line chart) of series over
%%% categories. The echarts option is built here, as sigil's
%%% area-chart/create does, and drawn like any chart (see aihtml_chart and
%%% aihtml_lib_chart).
%%%
%%% area_chart/3 builds an element record (#ah_area_chart{}, defined in
%%% include/aihtml_area_chart.hrl) and render/1 turns it into HTML, so
%%% pages may also write the record directly (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_area_chart).
-behaviour(aihtml_element).

-include("aihtml_area_chart.hrl").

-export([area_chart/3, option/1, render/1, fields/1, catalog/0]).

-export_type([element/0]).

-import(aihtml_lib_chart, [chart_root/8, value_axis/2, name_side/2, axis_chart/8, color_kv/1,
                          norm_series/1, categories/2, bool/2, list/2]).

-define(E, aihtml_element).
-define(L, aihtml_lib_chart).

-type css() :: aihtml_html:css().
-type attrs() :: aihtml_html:attrs().
-type html() :: aihtml_html:html().
-type element() :: #ah_area_chart{}.
-type series() :: aihtml_lib_chart:series().

%%%===================================================================
%%% Builder
%%%===================================================================

%% @doc An area chart: `Series' are `{Name, Values}' (or maps with
%% `color'), one value per category. Css: `line' (no fill: a line chart),
%% `straight' (no smoothing), `stack', `loading', `disabled'. Options:
%% `categories' (the x axis), `title', `colors', `y_name', `legend' (top |
%% bottom | left | right | none), `grid' and `tooltip' (booleans, default
%% true), `height', `width', `renderer'.
-spec area_chart([series()], css(), attrs()) -> #ah_area_chart{}.
area_chart(Series, Css, Attrs) ->
    ?E:build(?MODULE, #ah_area_chart{series = Series}, Css, Attrs).

%% @doc The field names of the record.
-spec fields(atom()) -> [atom()].
fields(ah_area_chart) -> record_info(fields, ah_area_chart).

%% @doc Internal: the echarts option of a record (aihtml_chart:chart_option/1
%% dispatches here), after the same checks as render/1.
-spec option(element()) -> aihtml_lib_chart:option().
option(#ah_area_chart{series = S0, line = Line, straight = Straight, stack = Stack,
                           categories = Cats0, title = Title, colors = Colors,
                           y_name = YName, legend = Legend, grid = Grid,
                           tooltip = Tooltip}) ->
    [bool(K, V) || {K, V} <- [{line, Line}, {straight, Straight}, {stack, Stack},
                              {grid, Grid}, {tooltip, Tooltip}]],
    Series = [norm_series(S) || S <- list(series, S0)],
    Cats = categories(Cats0, Series),
    SeriesCfg = [maps:merge(#{type => line, name => N, data => D,
                              smooth => not Straight, symbol => circle, symbolSize => 6,
                              lineStyle => #{width => 2}, itemStyle => #{borderWidth => 2}},
                            maps:from_list([{areaStyle, #{opacity => 0.15}} || not Line]
                                           ++ [{stack, <<"total">>} || Stack]
                                           ++ color_kv(C)))
                 || #{name := N, data := D} = C <- Series],
    axis_chart(#{xAxis => #{type => category, boundaryGap => false, data => Cats,
                            axisLine => #{show => true}, axisTick => #{show => false}},
                 yAxis => value_axis(Grid, YName),
                 series => SeriesCfg},
               #{trigger => axis,
                 axisPointer => #{type => cross,
                                  label => #{backgroundColor => <<"--ah-color-text-secondary">>}}},
               Series, Title, Colors, Legend, Tooltip, name_side(YName, top)).

%%%===================================================================
%%% Rendering
%%%===================================================================

-spec render(element()) -> html().
render(#ah_area_chart{loading = L, disabled = D, width = W, height = H, renderer = Rd} = R) ->
    Classes = ?E:classes(?MODULE, R),
    chart_root(R, Classes, option(R), L, D, W, H, Rd).

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => area_chart, category => data,
       signature => <<"area_chart(Series, Css, Attrs)">>,
       root => <<"ah-chart">>, flags => [line, straight, stack, loading, disabled],
       classes => #{line => [], straight => [], stack => [], loading => []},
       options => [categories, title, colors, y_name, legend, grid, tooltip,
                   width, height, renderer],
       behavior => <<"chart">>, events => ?L:events(),
       doc => <<"A smooth area chart (or line chart) of series over categories, "
                "with legend, tooltip and grid.">>,
       option_docs => maps:merge(maps:merge(?L:size_docs(), ?L:axis_docs()),
                                 #{line => <<"Lines without the filled area.">>,
                                   straight => <<"Straight segments instead of smooth curves.">>}),
       methods => ?L:methods()}].
