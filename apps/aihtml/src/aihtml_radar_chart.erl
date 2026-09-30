%%%-------------------------------------------------------------------
%%% @doc The radar chart, ported from sigil (data/radar_chart): one
%%% polygon per series over named axes (indicators). The echarts option is
%%% built here, as sigil's radar-chart/create does, and drawn like any
%%% chart (see aihtml_chart and aihtml_lib_chart).
%%%
%%% ah_radar_chart/3 builds an element record (#ah_radar_chart{}, defined in
%%% include/aihtml_radar_chart.hrl) and render/1 turns it into HTML, so
%%% pages may also write the record directly (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_radar_chart).
-behaviour(aihtml_element).

-include("aihtml_radar_chart.hrl").

-export([ah_radar_chart/3, option/1, render/1, fields/1, catalog/0]).

-export_type([element/0, indicator/0]).

-import(aihtml_lib_chart, [chart_root/8, common/7, legend_ok/1, norm_series/1, bool/2, list/2,
                          text/1]).

-define(E, aihtml_element).
-define(L, aihtml_lib_chart).

-type css() :: aihtml_html:css().
-type attrs() :: aihtml_html:attrs().
-type html() :: aihtml_html:html().
-type element() :: #ah_radar_chart{}.
-type series() :: aihtml_lib_chart:series().
%% A radar axis: `{Name, Max}' or a map.
-type indicator() :: {aihtml_lib_chart:text(), number()}
                   | #{name := aihtml_lib_chart:text(), max => number(), min => number()}.

%%%===================================================================
%%% Builder
%%%===================================================================

%% @doc A radar chart: `Series' as for area_chart, one value per
%% indicator. Css: `polygon' (default) or `circle', `loading',
%% `disabled'. Options: `indicators' (`{Name, Max}'), `title', `colors',
%% `legend', `tooltip', `split_number', `radius', `area_opacity',
%% `height', `width', `renderer'.
-spec ah_radar_chart([series()], css(), attrs()) -> #ah_radar_chart{}.
ah_radar_chart(Series, Css, Attrs) ->
    ?E:build(?MODULE, #ah_radar_chart{series = Series}, Css, Attrs).

%% @doc The field names of the record.
-spec fields(atom()) -> [atom()].
fields(ah_radar_chart) -> record_info(fields, ah_radar_chart).

%% @doc Internal: the echarts option of a record (aihtml_chart:chart_option/1
%% dispatches here), after the same checks as render/1.
-spec option(element()) -> aihtml_lib_chart:option().
option(#ah_radar_chart{series = S0, shape = Shape, indicators = Ind0, title = Title,
                             colors = Colors, legend = Legend, tooltip = Tooltip,
                             split_number = Split, radius = Radius,
                             area_opacity = Opacity}) ->
    bool(tooltip, Tooltip),
    legend_ok(Legend),
    is_integer(Split) andalso Split > 0 orelse error({aihtml, {bad_option, split_number, Split}}),
    is_number(Radius) orelse is_binary(Radius) orelse error({aihtml, {bad_option, radius, Radius}}),
    is_number(Opacity) andalso Opacity >= 0 andalso Opacity =< 1
        orelse error({aihtml, {bad_option, area_opacity, Opacity}}),
    Series = [norm_series(S) || S <- list(series, S0)],
    Indicators = [norm_indicator(I) || I <- list(indicators, Ind0)],
    Line = <<"--ah-color-border">>,
    Radar = #{indicator => Indicators, shape => Shape, splitNumber => Split,
              axisName => #{color => <<"--ah-color-text-secondary">>, fontSize => 12},
              splitLine => #{lineStyle => #{color => Line, type => dashed}},
              splitArea => #{show => false},
              axisLine => #{lineStyle => #{color => Line}},
              center => [<<"50%">>, <<"55%">>], radius => Radius},
    Data = [maps:merge(#{name => N, value => D},
                       maps:from_list([{itemStyle, #{color => C}}
                                       || C <- [maps:get(color, S, undefined)], C =/= undefined]))
            || #{name := N, data := D} = S <- Series],
    RadarSeries = #{type => radar, symbol => circle, symbolSize => 4,
                    lineStyle => #{width => 2}, areaStyle => #{opacity => Opacity},
                    emphasis => #{lineStyle => #{width => 3},
                                  areaStyle => #{opacity => min(1, Opacity + 0.14)}},
                    data => Data},
    Opt = common(#{radar => Radar, series => [RadarSeries]}, #{trigger => item},
                 [N || #{name := N} <- Series], Title, Colors, Legend, Tooltip),
    case Opt of
        #{legend := L} -> Opt#{legend := L#{icon => circle, itemWidth => 8, itemHeight => 8}};
        _ -> Opt
    end.

norm_indicator({Name, Max}) when is_number(Max) -> norm_indicator(#{name => Name, max => Max});
norm_indicator(#{name := N} = M) ->
    [is_number(V) orelse error({aihtml, {bad_indicator, M}}) || V <- maps:values(maps:with([max, min], M))],
    maps:merge(maps:with([max, min], M), #{name => text(N)});
norm_indicator(Other) -> error({aihtml, {bad_indicator, Other}}).

%%%===================================================================
%%% Rendering
%%%===================================================================

-spec render(element()) -> html().
render(#ah_radar_chart{loading = L, disabled = D, width = W, height = H, renderer = Rd} = R) ->
    Classes = ?E:classes(?MODULE, R),
    chart_root(R, Classes, option(R), L, D, W, H, Rd).

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => radar_chart, category => data,
       signature => <<"ah_radar_chart(Series, Css, Attrs)">>,
       root => <<"ah-chart">>,
       groups => #{shape => {[polygon, circle], polygon}},
       flags => [loading, disabled],
       classes => #{polygon => [], circle => [], loading => []},
       options => [indicators, title, colors, legend, tooltip, split_number, radius,
                   area_opacity, width, height, renderer],
       behavior => <<"chart">>, events => ?L:events(),
       doc => <<"A radar chart comparing series over several named axes.">>,
       option_docs => maps:merge(?L:size_docs(),
                                 #{indicators => <<"The axes: {Name, Max} or #{name, max, min}.">>,
                                   title => <<"A title above the chart.">>,
                                   colors => maps:get(colors, ?L:axis_docs()),
                                   legend => maps:get(legend, ?L:axis_docs()),
                                   tooltip => <<"Show a tooltip on hover (default true).">>,
                                   split_number => <<"Number of rings (default 4).">>,
                                   radius => <<"Radius, pixels or a percentage (default \"62%\").">>,
                                   area_opacity => <<"Opacity of the filled areas, 0..1 (default 0.18).">>}),
       methods => ?L:methods()}].
