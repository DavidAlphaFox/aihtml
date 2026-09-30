%%%-------------------------------------------------------------------
%%% @doc The bar chart, ported from sigil (data/bar_chart): vertical or
%%% horizontal bars of series over categories, grouped or stacked. The
%%% echarts option is built here, as sigil's bar-chart/create does, and
%%% drawn like any chart (see aihtml_chart and aihtml_lib_chart).
%%%
%%% ah_bar_chart/3 builds an element record (#ah_bar_chart{}, defined in
%%% include/aihtml_bar_chart.hrl) and render/1 turns it into HTML, so
%%% pages may also write the record directly (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_bar_chart).
-behaviour(aihtml_element).

-include("aihtml_bar_chart.hrl").

-export([ah_bar_chart/3, option/1, render/1, fields/1, catalog/0]).

-export_type([element/0]).

-import(aihtml_lib_chart, [chart_root/8, value_axis/2, name_side/2, axis_chart/8, color_kv/1,
                          norm_series/1, categories/2, bool/2, list/2]).

-define(E, aihtml_element).
-define(L, aihtml_lib_chart).

-type css() :: aihtml_html:css().
-type attrs() :: aihtml_html:attrs().
-type html() :: aihtml_html:html().
-type element() :: #ah_bar_chart{}.
-type series() :: aihtml_lib_chart:series().

%%%===================================================================
%%% Builder
%%%===================================================================

%% @doc A bar chart: `Series' as for area_chart. Css: `horizontal',
%% `stack', `loading', `disabled'. Options: those of area_chart plus
%% `bar_width' (pixels or a percentage).
-spec ah_bar_chart([series()], css(), attrs()) -> #ah_bar_chart{}.
ah_bar_chart(Series, Css, Attrs) ->
    ?E:build(?MODULE, #ah_bar_chart{series = Series}, Css, Attrs).

%% @doc The field names of the record.
-spec fields(atom()) -> [atom()].
fields(ah_bar_chart) -> record_info(fields, ah_bar_chart).

%% @doc Internal: the echarts option of a record (aihtml_chart:chart_option/1
%% dispatches here), after the same checks as render/1.
-spec option(element()) -> aihtml_lib_chart:option().
option(#ah_bar_chart{series = S0, horizontal = Horiz, stack = Stack, categories = Cats0,
                         title = Title, colors = Colors, y_name = YName, legend = Legend,
                         grid = Grid, tooltip = Tooltip, bar_width = BarWidth}) ->
    [bool(K, V) || {K, V} <- [{horizontal, Horiz}, {stack, Stack}, {grid, Grid},
                              {tooltip, Tooltip}]],
    BarWidth =:= undefined orelse is_number(BarWidth) orelse is_binary(BarWidth)
        orelse error({aihtml, {bad_option, bar_width, BarWidth}}),
    Series = [norm_series(S) || S <- list(series, S0)],
    Cats = categories(Cats0, Series),
    Radius = case Horiz of
                 true -> [0, 4, 4, 0];
                 false -> [4, 4, 0, 0]
             end,
    Last = length(Series),
    SeriesCfg = [maps:merge(#{type => bar, name => N, data => D, barMaxWidth => 40,
                              %% stacked: only the outer segment is rounded
                              itemStyle => #{borderRadius => case Stack andalso I =/= Last of
                                                                 true -> 0;
                                                                 false -> Radius
                                                             end}},
                            maps:from_list([{stack, <<"total">>} || Stack]
                                           ++ [{barWidth, BarWidth} || BarWidth =/= undefined]
                                           ++ color_kv(C)))
                 || {I, #{name := N, data := D} = C} <- lists:enumerate(Series)],
    CatAxis = #{type => category, data => Cats, axisLine => #{show => true},
                axisTick => #{show => false}},
    ValAxis = value_axis(Grid, YName),
    Axes = case Horiz of
               true -> #{xAxis => ValAxis, yAxis => CatAxis};
               false -> #{xAxis => CatAxis, yAxis => ValAxis}
           end,
    axis_chart(Axes#{series => SeriesCfg},
               #{trigger => axis, axisPointer => #{type => shadow}},
               Series, Title, Colors, Legend, Tooltip,
               name_side(YName, case Horiz of true -> right; false -> top end)).

%%%===================================================================
%%% Rendering
%%%===================================================================

-spec render(element()) -> html().
render(#ah_bar_chart{loading = L, disabled = D, width = W, height = H, renderer = Rd} = R) ->
    Classes = ?E:classes(?MODULE, R),
    chart_root(R, Classes, option(R), L, D, W, H, Rd).

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => bar_chart, category => data,
       signature => <<"ah_bar_chart(Series, Css, Attrs)">>,
       root => <<"ah-chart">>, flags => [horizontal, stack, loading, disabled],
       classes => #{horizontal => [], stack => [], loading => []},
       options => [categories, title, colors, y_name, legend, grid, tooltip, bar_width,
                   width, height, renderer],
       behavior => <<"chart">>, events => ?L:events(),
       doc => <<"A bar chart of series over categories, vertical or horizontal, "
                "grouped or stacked, with rounded bars.">>,
       option_docs => maps:merge(maps:merge(?L:size_docs(), ?L:axis_docs()),
                                 #{horizontal => <<"Bars grow to the right; categories on the y axis.">>,
                                   bar_width => <<"Bar width in pixels or a percentage (<<\"30%\">>).">>}),
       methods => ?L:methods()}].
