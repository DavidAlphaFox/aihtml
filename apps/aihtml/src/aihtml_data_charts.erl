%%%-------------------------------------------------------------------
%%% @doc Charts, ported from sigil (data/chart, data/area_chart,
%%% data/bar_chart, data/donut_chart, data/radar_chart,
%%% data/relation_graph). All of them draw with echarts, which the browser
%%% loads on demand (AH.vendor("echarts")) when the first chart mounts.
%%%
%%%   chart(Option, Css, Attrs)             any echarts option (an Erlang map)
%%%   area_chart(Series, Css, Attrs)        line / area chart over categories
%%%   bar_chart(Series, Css, Attrs)         vertical or horizontal bars
%%%   donut_chart(Items, Css, Attrs)        donut or pie of named values
%%%   radar_chart(Series, Css, Attrs)       radar over named indicators
%%%   relation_graph(Graph, Css, Attrs)     node-link graph in a panel
%%%   chart_option(Chart)                   the echarts option of a chart record
%%%   chart_update(Ctx, Target, Chart)      (in an action) redraw a chart in place
%%%
%%% == How it works ==
%%%
%%% The server builds the whole echarts option (the convenience charts
%%% build it from their simple data, as sigil's helpers do) and writes it
%%% as JSON into a data island inside the chart's root:
%%%
%%%   <div class="ah-chart" data-ah="chart" role="img">
%%%     <script type="application/json" class="ah-chart-data">{...}</script>
%%%   </div>
%%%
%%% The behaviour loads echarts, themes it from the --ah-* custom properties
%%% (palette, text, border, paper colours, font), draws the option, follows
%%% size changes and theme changes, and fires 'ah:chart-click' (and
%%% 'ah:chart-dblclick', 'ah:chart-legendselectchanged', ...) with the
%%% clicked item. Strings "--ah-color-x" or "var(--ah-color-x)" in an
%%% option are resolved against the theme in the browser, so options may
%%% name theme colours.
%%%
%%% An action updates a chart without re-rendering it with
%%% `chart_update(Ctx, Target, ChartOrOption)', which calls the behaviour
%%% method setOption with the option of a chart record (merged, so series
%%% animate to their new values) or with an option map; the methods
%%% setData, resize, showLoading, ... are reachable with
%%% aihtml_action:call/4 too.
%%%
%%% relation_graph fires 'ah:select' with the selected node id in
%%% `data-ah-value', and 'ah:refresh' when its refresh button is pressed
%%% (bind an action to it that answers with chart_update/3 or a morph).
%%%
%%% Each function builds an element record (#ah_chart{} ..., defined in
%%% include/aihtml_data_charts.hrl) and render/1 turns it into HTML, so
%%% pages may also write the records directly (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_data_charts).
-behaviour(aihtml_element).

-include("aihtml_data_charts.hrl").

-export([chart/3, area_chart/3, bar_chart/3, donut_chart/3, radar_chart/3,
         relation_graph/3, chart_option/1, chart_update/3,
         render/1, fields/1, catalog/0, facade_extras/0]).

-export_type([element/0, option/0, series/0, item/0, indicator/0, graph/0, chart/0]).

-define(H, aihtml_html).
-define(E, aihtml_element).

-type css() :: aihtml_html:css().
-type attrs() :: aihtml_html:attrs().
-type html() :: aihtml_html:html().
-type option() :: ah_ch_option().
-type series() :: ah_ch_series().
-type item() :: ah_ch_item().
-type indicator() :: ah_ch_indicator().
-type graph() :: ah_ch_graph().
-type element() :: #ah_chart{} | #ah_area_chart{} | #ah_bar_chart{} | #ah_donut_chart{}
                 | #ah_radar_chart{} | #ah_relation_graph{}.
%% A chart record, or an option map for chart_update/3.
-type chart() :: element() | option().

%% sigil's default palette for relation graph categories.
-define(PALETTE, [<<"--ah-color-primary">>, <<"--ah-color-success">>,
                  <<"--ah-color-warning">>, <<"--ah-color-error">>,
                  <<"--ah-color-info">>, <<"--ah-color-secondary">>]).

%%%===================================================================
%%% Builders
%%%===================================================================

%% @doc A chart of any echarts option. `Option' is a map as echarts
%% documents it (binaries for text; "--ah-color-*" strings are theme
%% colours). Css: `loading', `disabled'. Options: `height', `width'
%% (pixels or a CSS length; the default height is 400px), `renderer'
%% (canvas | svg).
-spec chart(option(), css(), attrs()) -> #ah_chart{}.
chart(Option, Css, Attrs) ->
    build(#ah_chart{option = Option}, Css, Attrs).

%% @doc An area chart: `Series' are `{Name, Values}' (or maps with
%% `color'), one value per category. Css: `line' (no fill: a line chart),
%% `straight' (no smoothing), `stack', `loading', `disabled'. Options:
%% `categories' (the x axis), `title', `colors', `y_name', `legend' (top |
%% bottom | left | right | none), `grid' and `tooltip' (booleans, default
%% true), `height', `width', `renderer'.
-spec area_chart([series()], css(), attrs()) -> #ah_area_chart{}.
area_chart(Series, Css, Attrs) ->
    build(#ah_area_chart{series = Series}, Css, Attrs).

%% @doc A bar chart: `Series' as for area_chart. Css: `horizontal',
%% `stack', `loading', `disabled'. Options: those of area_chart plus
%% `bar_width' (pixels or a percentage).
-spec bar_chart([series()], css(), attrs()) -> #ah_bar_chart{}.
bar_chart(Series, Css, Attrs) ->
    build(#ah_bar_chart{series = Series}, Css, Attrs).

%% @doc A donut chart: `Items' are `{Name, Value}' (or maps with `color').
%% Css: `pie' (no hole), `loading', `disabled'. Options: `title',
%% `colors', `legend' (default right), `labels' (boolean), `tooltip',
%% `radius' ({Inner, Outer}), `center' ({X, Y}), `height', `width',
%% `renderer'.
-spec donut_chart([item()], css(), attrs()) -> #ah_donut_chart{}.
donut_chart(Items, Css, Attrs) ->
    build(#ah_donut_chart{items = Items}, Css, Attrs).

%% @doc A radar chart: `Series' as for area_chart, one value per
%% indicator. Css: `polygon' (default) or `circle', `loading',
%% `disabled'. Options: `indicators' (`{Name, Max}'), `title', `colors',
%% `legend', `tooltip', `split_number', `radius', `area_opacity',
%% `height', `width', `renderer'.
-spec radar_chart([series()], css(), attrs()) -> #ah_radar_chart{}.
radar_chart(Series, Css, Attrs) ->
    build(#ah_radar_chart{series = Series}, Css, Attrs).

%% @doc A relation graph. `Graph' is `#{nodes, edges, categories}' (or
%% `{Nodes, Edges}'), in sigil's vocabulary: nodes `#{id, label,
%% category, root, parent, x, y}', edges `#{source, target, label, kind}'.
%% Css: layout `force' (default), `circular', `fixed' (the nodes' x / y)
%% or `tree'; tree orientation `lr' (default), `tb', `rl', `bt'; node
%% shape `circle' (default), `square', `round_rect'; `directed' (arrows),
%% `loading'. Options: `selected', `focus' (a node id to centre),
%% `details' (#{NodeId => Html} shown in the detail card when that node
%% is selected), `edge_labels' (auto | boolean), `roam', `toolbar',
%% `error', `empty_text', `height' (default 420), `width', `renderer'.
-spec relation_graph(graph(), css(), attrs()) -> #ah_relation_graph{}.
relation_graph(Graph, Css, Attrs) ->
    build(#ah_relation_graph{graph = Graph}, Css, Attrs).

build(R, Css, Attrs) ->
    Tag = element(1, R),
    ?E:build(R, fields(Tag), entry(?E:component_name(Tag)), Css, Attrs).

%% @doc The field names of one of this group's records.
-spec fields(atom()) -> [atom()].
fields(ah_chart) -> record_info(fields, ah_chart);
fields(ah_area_chart) -> record_info(fields, ah_area_chart);
fields(ah_bar_chart) -> record_info(fields, ah_bar_chart);
fields(ah_donut_chart) -> record_info(fields, ah_donut_chart);
fields(ah_radar_chart) -> record_info(fields, ah_radar_chart);
fields(ah_relation_graph) -> record_info(fields, ah_relation_graph).

%% @doc Functions besides the components that the aihtml facade re-exports.
-spec facade_extras() -> [{atom(), arity()}].
facade_extras() -> [{chart_option, 1}, {chart_update, 3}].

%%%===================================================================
%%% Updating a chart from an action
%%%===================================================================

%% @doc The echarts option a chart record draws (for #ah_chart{} its
%% `option'), after the same checks as render/1.
-spec chart_option(element()) -> option().
chart_option(#ah_chart{option = O}) -> check_option(O);
chart_option(#ah_area_chart{} = R) -> area_option(R);
chart_option(#ah_bar_chart{} = R) -> bar_option(R);
chart_option(#ah_donut_chart{} = R) -> donut_option(R);
chart_option(#ah_radar_chart{} = R) -> radar_option(R);
chart_option(#ah_relation_graph{} = R) -> graph_option(R);
chart_option(Other) -> error({aihtml, {not_a_chart, Other}}).

%% @doc In an action: redraw the chart `Target' (usually `{id, Id}') in
%% place. With a chart record the chart gets that record's option, merged
%% into the current one so that series animate to their new data (a
%% relation graph is replaced, since nodes may have gone); with a map the
%% map is merged like echarts' setOption. Other fields of the record
%% (size, css, attrs) are not applied: re-render the chart for those.
-spec chart_update(aihtml_action:ctx(), aihtml_action:target(), chart()) -> ok.
chart_update(Ctx, Target, #ah_relation_graph{} = R) ->
    aihtml_action:call(Ctx, Target, setOption, [chart_option(R), true]);
chart_update(Ctx, Target, Option) when is_map(Option) ->
    aihtml_action:call(Ctx, Target, setOption, [check_option(Option), false]);
chart_update(Ctx, Target, R) ->
    aihtml_action:call(Ctx, Target, setOption, [chart_option(R), false]).

%%%===================================================================
%%% Rendering
%%%===================================================================

-spec render(element()) -> html().
render(#ah_chart{loading = L, disabled = D, width = W, height = H, renderer = Rd} = R) ->
    chart_root(R, classes(R), L, D, W, H, Rd);
render(#ah_area_chart{loading = L, disabled = D, width = W, height = H, renderer = Rd} = R) ->
    chart_root(R, classes(R), L, D, W, H, Rd);
render(#ah_bar_chart{loading = L, disabled = D, width = W, height = H, renderer = Rd} = R) ->
    chart_root(R, classes(R), L, D, W, H, Rd);
render(#ah_donut_chart{loading = L, disabled = D, width = W, height = H, renderer = Rd} = R) ->
    chart_root(R, classes(R), L, D, W, H, Rd);
render(#ah_radar_chart{loading = L, disabled = D, width = W, height = H, renderer = Rd} = R) ->
    chart_root(R, classes(R), L, D, W, H, Rd);
render(#ah_relation_graph{} = R) ->
    render_graph(R).

classes(R) ->
    Tag = element(1, R),
    ?E:classes(R, fields(Tag), entry(?E:component_name(Tag))).

%% Classes first: they check the modifier fields before the option does.
chart_root(R, Classes, Loading, Disabled, W, H, Renderer) ->
    Option = chart_option(R),
    bool(loading, Loading),
    bool(disabled, Disabled),
    ?H:el('div',
          [island(Option),
           case Disabled of
               true -> ?H:el('div', [], [<<"ah-chart-overlay">>], []);
               false -> []
           end],
          Classes,
          [[{role, img}, {data_ah, <<"chart">>},
            {data_ah_renderer, renderer(Renderer)},
            {data_ah_loading, Loading andalso <<"true">>},
            {aria_disabled, Disabled andalso <<"true">>},
            {style, size_style(W, H)}],
           ?E:root_attrs(R, 'ah:chart-click')]).

%% The option as JSON in a script element. No "<" is left in it (JSON
%% allows < in strings, the only place "<" can appear), so the data
%% can neither close the script element nor open a comment in it.
island(Option) ->
    Json = try iolist_to_binary(json:encode(Option))
           catch error:_ -> error({aihtml, {bad_option, option, Option}})
           end,
    ?H:el(script, {safe, binary:replace(Json, <<"<">>, <<"\\u003c">>, [global])},
          [<<"ah-chart-data">>], [{type, <<"application/json">>}]).

renderer(R) ->
    lists:member(R, [canvas, svg]) orelse error({aihtml, {bad_option, renderer, R}}),
    case R of
        svg -> <<"svg">>;
        _ -> undefined
    end.

size_style(W, H) ->
    case [[P, size(K, V), $;] || {P, K, V} <- [{<<"width:">>, width, W},
                                               {<<"height:">>, height, H}],
                                  V =/= undefined] of
        [] -> undefined;
        L -> iolist_to_binary(L)
    end.

size(_, N) when is_integer(N), N > 0 -> [integer_to_binary(N), <<"px">>];
size(K, B) when is_binary(B), B =/= <<>> ->
    %% a CSS length, not a way to smuggle more declarations into style
    case binary:match(B, [<<";">>, <<"\"">>, <<"<">>, <<"{">>]) of
        nomatch -> B;
        _ -> error({aihtml, {bad_option, K, B}})
    end;
size(K, V) -> error({aihtml, {bad_option, K, V}}).

check_option(O) ->
    is_map(O) orelse error({aihtml, {bad_option, option, O}}),
    O.

%%%===================================================================
%%% area_chart / bar_chart (sigil's area-chart/create, bar-chart/create)
%%%===================================================================

area_option(#ah_area_chart{series = S0, line = Line, straight = Straight, stack = Stack,
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

bar_option(#ah_bar_chart{series = S0, horizontal = Horiz, stack = Stack, categories = Cats0,
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

value_axis(Grid, YName) ->
    maps:merge(#{type => value, axisLine => #{show => false}, axisTick => #{show => false},
                 splitLine => #{show => Grid, lineStyle => #{type => dashed}}},
               maps:from_list([{name, text(YName)} || YName =/= undefined])).

name_side(undefined, _) -> none;
name_side(_, Side) -> Side.

%% Title, tooltip, legend, grid and palette shared by area and bar charts.
%% The grid leaves room for them and for the value axis name (NameSide).
axis_chart(Base, TooltipCfg, Series, Title, Colors, Legend, Tooltip, NameSide) ->
    legend_ok(Legend),
    HasTitle = Title =/= undefined,
    Top = 16 + case HasTitle of true -> 28; false -> 0 end
             + case Legend of top -> 28; _ -> 0 end
             + case NameSide of top -> 20; _ -> 0 end,
    Grid = #{left => 12, top => Top,
             right => case Legend of right -> 120; _ -> 20 end
                 + case NameSide of right -> 40; _ -> 0 end,
             bottom => case Legend of bottom -> 40; _ -> 12 end,
             containLabel => true},
    Grid1 = case Legend of left -> Grid#{left => 120}; _ -> Grid end,
    common(Base#{grid => Grid1}, TooltipCfg, [N || #{name := N} <- Series],
           Title, Colors, Legend, Tooltip).

common(Opt, TooltipCfg, Names, Title, Colors, Legend, Tooltip) ->
    maps:from_list(
      maps:to_list(Opt)
      ++ [{title, title(Title)} || Title =/= undefined]
      ++ [{tooltip, TooltipCfg} || Tooltip]
      ++ [{legend, legend(Legend, Names, Title =/= undefined)} || Legend =/= none]
      ++ [{color, colors(Colors)} || Colors =/= undefined]).

title(T) -> #{text => text(T), left => center}.

legend(Pos, Names, HasTitle) ->
    Place = case Pos of
                bottom -> #{bottom => 0, left => center};
                top -> #{top => case HasTitle of true -> 28; false -> 0 end, left => center};
                right -> #{right => 10, top => middle, orient => vertical};
                left -> #{left => 10, top => middle, orient => vertical}
            end,
    Place#{data => Names, type => scroll}.

legend_ok(L) ->
    lists:member(L, [top, bottom, left, right, none])
        orelse error({aihtml, {bad_option, legend, L}}).

colors(Cs) ->
    is_list(Cs) andalso lists:all(fun is_binary/1, Cs)
        orelse error({aihtml, {bad_option, colors, Cs}}),
    Cs.

color_kv(#{color := C}) when is_binary(C) -> [{color, C}];
color_kv(#{color := C}) -> error({aihtml, {bad_color, C}});
color_kv(_) -> [].

norm_series({Name, Data} = S) -> norm_series(S, #{name => Name, data => Data});
norm_series(#{data := _} = M) -> norm_series(M, M);
norm_series(Other) -> error({aihtml, {bad_series, Other}}).

norm_series(Orig, #{data := Data} = M) ->
    is_list(Data) andalso lists:all(fun(V) -> is_number(V) orelse V =:= null end, Data)
        orelse error({aihtml, {bad_series, Orig}}),
    Name = case M of
               #{name := N} -> text(N);
               _ -> <<>>
           end,
    maps:merge(maps:with([color], M), #{name => Name, data => Data}).

%% Without categories the x axis counts 1..N.
categories([], Series) ->
    N = lists:max([0 | [length(D) || #{data := D} <- Series]]),
    [integer_to_binary(I) || I <- lists:seq(1, N)];
categories(Cats, _) ->
    is_list(Cats) orelse error({aihtml, {bad_option, categories, Cats}}),
    [text(C) || C <- Cats].

%%%===================================================================
%%% donut_chart (sigil's donut-chart/create)
%%%===================================================================

donut_option(#ah_donut_chart{items = Items0, pie = Pie, title = Title, colors = Colors,
                             legend = Legend, labels = Labels, tooltip = Tooltip,
                             radius = Radius, center = Center0}) ->
    [bool(K, V) || {K, V} <- [{pie, Pie}, {labels, Labels}, {tooltip, Tooltip}]],
    legend_ok(Legend),
    {Inner, Outer} = case Radius of
                         {I, O} when (is_number(I) orelse is_binary(I)),
                                     (is_number(O) orelse is_binary(O)) -> {I, O};
                         _ -> error({aihtml, {bad_option, radius, Radius}})
                     end,
    %% beside a side legend, below a title or top legend, above a bottom one
    CY = if
             Title =/= undefined; Legend =:= top -> <<"55%">>;
             Legend =:= bottom -> <<"45%">>;
             true -> <<"50%">>
         end,
    Center0 =:= undefined orelse (is_tuple(Center0) andalso tuple_size(Center0) =:= 2)
        orelse error({aihtml, {bad_option, center, Center0}}),
    Center = case Center0 of
                 undefined when Legend =:= right -> [<<"40%">>, CY];
                 undefined when Legend =:= left -> [<<"60%">>, CY];
                 undefined -> [<<"50%">>, CY];
                 {X, Y} -> [X, Y]
             end,
    Items = [norm_item(I) || I <- list(items, Items0)],
    Data = [maps:merge(#{name => N, value => V},
                       maps:from_list([{itemStyle, #{color => C}} || C <- [maps:get(color, It, undefined)],
                                                                     C =/= undefined]))
            || #{name := N, value := V} = It <- Items],
    Series = #{type => pie,
               radius => [case Pie of true -> 0; false -> Inner end, Outer],
               center => Center,
               data => Data,
               itemStyle => #{borderRadius => 4, borderColor => <<"--ah-color-bg-paper">>,
                              borderWidth => 2},
               label => case Labels of
                            true -> #{show => true, formatter => <<"{b}: {d}%">>};
                            false -> #{show => false}
                        end,
               emphasis => #{label => #{show => true, fontSize => 14, fontWeight => bold}},
               animationType => scale,
               animationEasing => elasticOut},
    common(#{series => [Series]},
           #{trigger => item, formatter => <<"{b}: {c} ({d}%)">>},
           [N || #{name := N} <- Items], Title, Colors, Legend, Tooltip).

norm_item({Name, Value}) when is_number(Value) -> norm_item(#{name => Name, value => Value});
norm_item(#{name := N, value := V} = M) when is_number(V) ->
    case M of
        #{color := C} when not is_binary(C) -> error({aihtml, {bad_color, C}});
        _ -> ok
    end,
    maps:merge(maps:with([color], M), #{name => text(N), value => V});
norm_item(Other) -> error({aihtml, {bad_item, Other}}).

%%%===================================================================
%%% radar_chart (sigil's radar-chart/create)
%%%===================================================================

radar_option(#ah_radar_chart{series = S0, shape = Shape, indicators = Ind0, title = Title,
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
%%% relation_graph (sigil's relation-graph and relation-graph.model)
%%%===================================================================

-define(ICON_FIT, <<"<svg viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"1.8\" "
                    "stroke-linecap=\"round\" stroke-linejoin=\"round\" aria-hidden=\"true\">"
                    "<path d=\"M8 3H5a2 2 0 00-2 2v3M16 3h3a2 2 0 012 2v3M8 21H5a2 2 0 01-2-2v-3"
                    "M16 21h3a2 2 0 002-2v-3\"/></svg>">>).
-define(ICON_REFRESH, <<"<svg viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" "
                        "stroke-width=\"1.8\" stroke-linecap=\"round\" stroke-linejoin=\"round\" "
                        "aria-hidden=\"true\"><path d=\"M21 12a9 9 0 11-2.64-6.36M21 3v6h-6\"/></svg>">>).
-define(ICON_CLOSE, <<"<svg viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" "
                      "stroke-width=\"1.8\" stroke-linecap=\"round\" stroke-linejoin=\"round\" "
                      "aria-hidden=\"true\"><path d=\"M18 6L6 18M6 6l12 12\"/></svg>">>).

render_graph(#ah_relation_graph{layout = Layout, loading = Loading, selected = Sel0,
                                focus = Focus0, details = Details0, error = Err,
                                empty_text = Empty, toolbar = Toolbar, width = W,
                                height = H, renderer = Renderer} = R) ->
    Classes = classes(R),
    [bool(K, V) || {K, V} <- [{loading, Loading}, {toolbar, Toolbar}]],
    Option = graph_option(R),
    {Nodes, _, _} = graph_data(R#ah_relation_graph.graph),
    Sel = opt_text(Sel0),
    Focus = opt_text(Focus0),
    is_map(Details0) orelse error({aihtml, {bad_option, details, Details0}}),
    Details = maps:from_list([{text(K), V} || K := V <- Details0]),
    State = if
                Loading ->
                    ?H:el('div', [?H:el(span, [], [<<"ah-relation-graph__spinner">>],
                                        [{aria_hidden, <<"true">>}]),
                                  ?H:el(span, <<"Loading…"/utf8>>, [], [])],
                          [<<"ah-relation-graph__state">>], [{data_kind, loading}]);
                Err =/= undefined ->
                    ?H:el('div', ?H:el(span, Err, [], []),
                          [<<"ah-relation-graph__state">>], [{data_kind, error}, {role, alert}]);
                Nodes =:= [] ->
                    ?H:el('div', ?H:el(span, Empty, [], []),
                          [<<"ah-relation-graph__state">>], [{data_kind, empty}]);
                true -> []
            end,
    ?H:el('div',
          [?H:el('div', island(Option), [<<"ah-relation-graph__canvas">>, <<"ah-chart">>],
                 [{data_ah, <<"chart">>}, {data_ah_renderer, renderer(Renderer)},
                  {aria_hidden, <<"true">>}, {style, <<"height:100%;">>}]),
           case Toolbar of
               false -> [];
               true ->
                   ?H:el('div',
                         [tool(<<"fit">>, <<"Fit view">>, ?ICON_FIT),
                          tool(<<"refresh">>, <<"Refresh">>, ?ICON_REFRESH)],
                         [<<"ah-relation-graph__toolbar">>], [{role, toolbar}])
           end,
           ?H:el('div',
                 [?H:el(button, {safe, ?ICON_CLOSE}, [<<"ah-relation-graph__detail-close">>],
                        [{type, button}, {aria_label, <<"Close">>}]),
                  ?H:el('div',
                        [?H:el('div', Html, [<<"ah-relation-graph__detail-item">>],
                               [{data_node, Id}, {hidden, Id =/= Sel}])
                         || {Id, Html} <- lists:sort(maps:to_list(Details))],
                        [<<"ah-relation-graph__detail-body">>], [])],
                 [<<"ah-relation-graph__detail">>],
                 [{data_visible, atom_to_binary(maps:is_key(Sel, Details))}]),
           State,
           ?H:el('div', [], [<<"ah-relation-graph__live">>],
                 [{aria_live, polite}, {aria_atomic, <<"true">>}])],
          Classes,
          [[{role, group}, {aria_roledescription, <<"relation graph">>}, {tabindex, 0},
            {data_ah, <<"relation-graph">>}, {data_ah_value, Sel},
            {data_layout, Layout},
            {data_ah_focus, case Focus of <<>> -> undefined; _ -> Focus end},
            {style, size_style(W, H)}],
           ?E:root_attrs(R, 'ah:select')]).

tool(Act, Label, Icon) ->
    ?H:el(button, {safe, Icon}, [<<"ah-relation-graph__tool">>],
          [{type, button}, {data_act, Act}, {title, Label}, {aria_label, Label}]).

opt_text(undefined) -> <<>>;
opt_text(V) -> text(V).

%% Nodes, edges and categories, normalised: ids and labels are binaries,
%% categories are indexes.
graph_data({Nodes, Edges}) when is_list(Nodes), is_list(Edges) ->
    graph_data(#{nodes => Nodes, edges => Edges});
graph_data(#{nodes := Nodes0} = G) when is_list(Nodes0) ->
    maps:size(maps:without([nodes, edges, categories], G)) =:= 0
        orelse error({aihtml, {bad_graph, G}}),
    Cats = [norm_category(C) || C <- list(categories, maps:get(categories, G, []))],
    CatNames = [N || #{name := N} <- Cats],
    Nodes = [norm_node(N, CatNames) || N <- Nodes0],
    Edges = [norm_edge(E) || E <- list(edges, maps:get(edges, G, []))],
    {Nodes, Edges, Cats};
graph_data(G) -> error({aihtml, {bad_graph, G}}).

norm_category(#{name := N} = M) ->
    case M of
        #{color := C} when not is_binary(C) -> error({aihtml, {bad_color, C}});
        _ -> ok
    end,
    maps:merge(maps:with([color], M), #{name => text(N)});
norm_category(N) -> #{name => text(N)}.

norm_node(#{id := Id0} = M, Cats) ->
    Id = text(Id0),
    Label = case M of #{label := L} -> text(L); _ -> Id end,
    Cat = case M of
              #{category := C} when is_integer(C), C >= 0, C < length(Cats) -> C;
              #{category := C} when is_integer(C) -> error({aihtml, {unknown_category, C}});
              #{category := C} ->
                  case index_of(text(C), Cats, 0) of
                      false -> error({aihtml, {unknown_category, C}});
                      I -> I
                  end;
              _ -> undefined
          end,
    [is_number(V) orelse error({aihtml, {bad_node, M}}) || V <- maps:values(maps:with([x, y], M))],
    [is_boolean(V) orelse error({aihtml, {bad_node, M}})
     || V <- maps:values(maps:with([root, collapsed], M))],
    maps:merge(maps:with([x, y, item_style, collapsed], M),
               #{id => Id, label => Label, category => Cat,
                 root => maps:get(root, M, false),
                 parent => case M of #{parent := P} -> text(P); _ -> undefined end});
norm_node({Id, Label}, Cats) -> norm_node(#{id => Id, label => Label}, Cats);
norm_node(Id, Cats) when is_binary(Id); is_atom(Id); is_integer(Id) -> norm_node(#{id => Id}, Cats);
norm_node([_ | _] = Id, Cats) -> norm_node(#{id => Id}, Cats);
norm_node(Other, _) -> error({aihtml, {bad_node, Other}}).

norm_edge({S, T}) -> norm_edge(#{source => S, target => T});
norm_edge({S, T, L}) -> norm_edge(#{source => S, target => T, label => L});
norm_edge(#{source := S, target := T} = M) ->
    Kind = maps:get(kind, M, solid),
    lists:member(Kind, [solid, dashed]) orelse error({aihtml, {bad_edge, M}}),
    maps:merge(maps:with([line_style, symbol, symbol_size], M),
               #{source => text(S), target => text(T), kind => Kind,
                 label => case M of #{label := L} -> text(L); _ -> undefined end});
norm_edge(Other) -> error({aihtml, {bad_edge, Other}}).

graph_option(#ah_relation_graph{graph = G, layout = Layout} = R) ->
    lists:member(Layout, [force, circular, fixed, tree])
        orelse error({aihtml, {bad_option, layout, Layout}}),
    Data = graph_data(G),
    case Layout of
        tree -> tree_option(R, Data);
        _ -> network_option(R, Data)
    end.

%% sigil's graph-option: force / circular / fixed (sigil's :none) layouts.
network_option(#ah_relation_graph{layout = Layout, node_shape = Shape, directed = Directed,
                                  edge_labels = EdgeLabels, roam = Roam},
               {Nodes, Edges, Cats}) ->
    [bool(K, V) || {K, V} <- [{directed, Directed}, {roam, Roam}]],
    lists:member(Shape, [circle, square, round_rect])
        orelse error({aihtml, {bad_option, node_shape, Shape}}),
    EdgeLabels =:= auto orelse is_boolean(EdgeLabels)
        orelse error({aihtml, {bad_option, edge_labels, EdgeLabels}}),
    ShowEdgeLabels = case EdgeLabels of
                         auto -> lists:any(fun(#{label := L}) -> L =/= undefined end, Edges);
                         _ -> EdgeLabels
                     end,
    Palette = case Cats of
                  [] -> ?PALETTE;
                  _ -> [maps:get(color, C, lists:nth((I - 1) rem length(?PALETTE) + 1, ?PALETTE))
                        || {I, C} <- lists:enumerate(Cats)]
              end,
    Series0 = #{type => graph,
                layout => case Layout of
                              circular -> circular;
                              fixed -> none;
                              force -> force
                          end,
                roam => Roam,
                draggable => true,
                %% room for the labels, the toolbar and the legend
                top => 40, left => 40, right => 40,
                bottom => case Cats of [] -> 40; _ -> 56 end,
                data => [graph_node(N, Shape) || N <- Nodes],
                links => [edge_link(E) || E <- Edges],
                categories => [#{name => N} || #{name := N} <- Cats],
                label => node_label(),
                edgeLabel => #{show => ShowEdgeLabels, fontSize => 11,
                               color => <<"--ah-color-text-secondary">>,
                               formatter => <<"{c}">>},
                emphasis => #{focus => adjacency, lineStyle => #{width => 3}},
                lineStyle => #{color => <<"--ah-color-border">>, curveness => 0.08}},
    Series1 = case Directed of
                  true -> Series0#{edgeSymbol => [none, arrow], edgeSymbolSize => 9};
                  false -> Series0
              end,
    Series = case Layout of
                 force -> Series1#{force => #{repulsion => 260, edgeLength => 120,
                                              gravity => 0.08}};
                 _ -> Series1
             end,
    Opt = #{color => Palette, tooltip => #{trigger => item}, animationDuration => 400,
            series => [Series]},
    case Cats of
        [] -> Opt;
        _ -> Opt#{legend => [#{data => [N || #{name := N} <- Cats], bottom => 0,
                               left => center, icon => circle, itemWidth => 8,
                               itemHeight => 8,
                               textStyle => #{color => <<"--ah-color-text-secondary">>}}]}
    end.

node_label() ->
    #{show => true, position => inside, color => <<"--ah-color-primary-contrast">>,
      fontSize => 12, overflow => truncate}.

graph_node(#{id := Id, label := Label, category := Cat, root := Root} = N, Shape) ->
    maps:from_list(
      [{id, Id}, {name, Label}, {symbol, symbol(Shape)},
       {symbolSize, symbol_size(Shape, Label, Root)}, {label, node_label()}]
      ++ [{category, Cat} || Cat =/= undefined]
      ++ [{K, V} || K <- [x, y], V <- [maps:get(K, N, undefined)], V =/= undefined]
      ++ [{itemStyle, V} || V <- [maps:get(item_style, N, undefined)], V =/= undefined]).

symbol(square) -> rect;
symbol(round_rect) -> roundRect;
symbol(circle) -> circle.

%% Round rectangles grow with their label so that it fits.
symbol_size(round_rect, Label, Root) ->
    W = max(52, min(150, 24 + 13 * string:length(Label))),
    [W, case Root of true -> 32; false -> 27 end];
symbol_size(_, _, true) -> 46;
symbol_size(_, _, false) -> 34.

edge_link(#{source := S, target := T, kind := Kind, label := L} = E) ->
    Dashed = Kind =:= dashed,
    Base = #{type => case Dashed of true -> dashed; false -> solid end,
             color => <<"--ah-color-border">>,
             width => case Dashed of true -> 1.4; false -> 1.8 end,
             opacity => case Dashed of true -> 0.55; false -> 0.9 end},
    Style = case maps:get(line_style, E, #{}) of
                M when is_map(M) -> maps:merge(Base, M);
                Bad -> error({aihtml, {bad_edge, Bad}})
            end,
    maps:from_list([{source, S}, {target, T},
                    {value, case L of undefined -> <<>>; _ -> L end},
                    {lineStyle, Style}]
                   ++ [{symbol, V} || V <- [maps:get(symbol, E, undefined)], V =/= undefined]
                   ++ [{symbolSize, V} || V <- [maps:get(symbol_size, E, undefined)],
                                          V =/= undefined]).

%% sigil's tree-option: a tidy tree from the edges (source is the parent)
%% or, without edges, from the nodes' `parent'.
tree_option(#ah_relation_graph{orient = Orient, roam = Roam}, {Nodes, Edges, _}) ->
    bool(roam, Roam),
    lists:member(Orient, [lr, tb, rl, bt]) orelse error({aihtml, {bad_option, orient, Orient}}),
    Forest = case Edges of
                 [] -> forest(Nodes, [{P, Id} || #{id := Id, parent := P} <- Nodes,
                                                 P =/= undefined]);
                 _ -> forest(Nodes, [{S, T} || #{source := S, target := T} <- Edges])
             end,
    Root = case Forest of
               [One] -> One;
               _ -> #{name => <<>>, id => <<"__root__">>, children => Forest,
                      itemStyle => #{opacity => 0}, label => #{show => false}}
           end,
    Vertical = Orient =:= tb orelse Orient =:= bt,
    #{tooltip => #{trigger => item, triggerOn => mousemove},
      animationDuration => 400,
      series =>
          [#{type => tree, data => [Root],
             orient => string:uppercase(atom_to_binary(Orient)),
             top => <<"6%">>, bottom => <<"6%">>, left => <<"10%">>, right => <<"16%">>,
             symbol => circle, symbolSize => 12, roam => Roam,
             initialTreeDepth => -1, expandAndCollapse => true,
             itemStyle => #{color => <<"--ah-color-primary">>,
                            borderColor => <<"--ah-color-primary">>},
             lineStyle => #{color => <<"--ah-color-border">>, width => 1.4, curveness => 0.5},
             label => #{position => case Vertical of true -> top; false -> left end,
                        verticalAlign => middle, align => right, fontSize => 12,
                        color => <<"--ah-color-text">>},
             leaves => #{label => #{position => case Vertical of true -> bottom; false -> right end,
                                    verticalAlign => middle, align => left}},
             emphasis => #{focus => descendant}}]}.

%% Parent -> child links to a forest; roots are nodes nobody points to
%% (or whose parent is unknown). A node seen on the path again is cut, so
%% cycles end.
forest(Nodes, Links) ->
    ById = maps:from_list([{Id, N} || #{id := Id} = N <- Nodes]),
    Known = [{P, C} || {P, C} <- Links, maps:is_key(P, ById), maps:is_key(C, ById)],
    Kids = lists:foldl(fun({P, C}, M) -> maps:update_with(P, fun(L) -> [C | L] end, [C], M) end,
                       #{}, Known),
    HasParent = maps:from_list([{C, true} || {_, C} <- Known]),
    [T || #{id := Id} <- Nodes, not maps:is_key(Id, HasParent),
          T <- [tree_node(Id, [], ById, Kids)], T =/= none].

tree_node(Id, Seen, ById, Kids) ->
    case lists:member(Id, Seen) of
        true -> none;
        false ->
            #{label := Label} = N = maps:get(Id, ById),
            Children = [T || C <- lists:reverse(maps:get(Id, Kids, [])),
                             T <- [tree_node(C, [Id | Seen], ById, Kids)], T =/= none],
            maps:from_list([{name, Label}, {id, Id}]
                           ++ [{children, Children} || Children =/= []]
                           ++ [{collapsed, true} || maps:get(collapsed, N, false)])
    end.

%%%===================================================================
%%% Catalog
%%%===================================================================

-define(CHART_METHODS,
        [#{name => setOption, args => <<"(Option, NotMerge)">>,
           doc => <<"Apply an echarts option: merged into the current one, or replacing it when "
                    "NotMerge is true (a list of component names replaces just those).">>},
         #{name => setData, args => <<"([Data, ...])">>,
           doc => <<"Replace the data of the series, one data list per series, in order.">>},
         #{name => resize, args => <<"()">>, doc => <<"Fit the chart to its container again.">>},
         #{name => showLoading, args => <<"()">>, doc => <<"Show the loading animation.">>},
         #{name => hideLoading, args => <<"()">>, doc => <<"Hide the loading animation.">>},
         #{name => dispatchAction, args => <<"(Action)">>,
           doc => <<"Run an echarts action, e.g. #{type => highlight, seriesIndex => 0}.">>},
         #{name => toggleSeries, args => <<"(Name)">>,
           doc => <<"Show or hide the series of this legend name.">>},
         #{name => getOption, args => <<"()">>,
           doc => <<"Return echarts' current option (browser side).">>},
         #{name => getDataURL, args => <<"(Opts)">>,
           doc => <<"Return the chart as an image data URL (browser side).">>},
         #{name => saveAsImage, args => <<"(Filename)">>,
           doc => <<"Download the chart as a PNG file.">>}]).

-define(SIZE_DOCS,
        #{loading => <<"Show echarts' loading animation until hideLoading is called.">>,
          disabled => <<"Grey the chart out and ignore the pointer.">>,
          width => <<"Width in pixels or a CSS length (default: the container's).">>,
          height => <<"Height in pixels or a CSS length (default 400px).">>,
          renderer => <<"canvas (default) or svg.">>}).

-define(AXIS_DOCS,
        #{categories => <<"The category labels (default 1, 2, 3 ...).">>,
          title => <<"A title above the chart.">>,
          colors => <<"Series colours: CSS colours or \"--ah-color-*\" tokens "
                      "(default the theme palette).">>,
          y_name => <<"The name of the value axis.">>,
          legend => <<"Legend position: bottom (default), top, left, right or none.">>,
          grid => <<"Dashed grid lines on the value axis (default true).">>,
          tooltip => <<"Show a tooltip on hover (default true).">>,
          stack => <<"Stack the series on each other.">>}).

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    Events = [<<"ah:chart-click">>, <<"ah:chart-dblclick">>, <<"ah:chart-mouseover">>,
              <<"ah:chart-mouseout">>, <<"ah:chart-legendselectchanged">>,
              <<"ah:chart-datazoom">>, <<"ah:chart-restore">>],
    [#{name => chart, category => data,
       signature => <<"chart(Option, Css, Attrs)">>,
       root => <<"ah-chart">>, flags => [loading, disabled],
       classes => #{loading => []},
       options => [width, height, renderer],
       behavior => <<"chart">>, events => Events,
       doc => <<"An echarts chart of any option built on the server, themed from the "
                "current theme and redrawn when it changes; echarts loads on demand.">>,
       option_docs => ?SIZE_DOCS,
       methods => ?CHART_METHODS},
     #{name => area_chart, category => data,
       signature => <<"area_chart(Series, Css, Attrs)">>,
       root => <<"ah-chart">>, flags => [line, straight, stack, loading, disabled],
       classes => #{line => [], straight => [], stack => [], loading => []},
       options => [categories, title, colors, y_name, legend, grid, tooltip,
                   width, height, renderer],
       behavior => <<"chart">>, events => Events,
       doc => <<"A smooth area chart (or line chart) of series over categories, "
                "with legend, tooltip and grid.">>,
       option_docs => maps:merge(maps:merge(?SIZE_DOCS, ?AXIS_DOCS),
                                 #{line => <<"Lines without the filled area.">>,
                                   straight => <<"Straight segments instead of smooth curves.">>}),
       methods => ?CHART_METHODS},
     #{name => bar_chart, category => data,
       signature => <<"bar_chart(Series, Css, Attrs)">>,
       root => <<"ah-chart">>, flags => [horizontal, stack, loading, disabled],
       classes => #{horizontal => [], stack => [], loading => []},
       options => [categories, title, colors, y_name, legend, grid, tooltip, bar_width,
                   width, height, renderer],
       behavior => <<"chart">>, events => Events,
       doc => <<"A bar chart of series over categories, vertical or horizontal, "
                "grouped or stacked, with rounded bars.">>,
       option_docs => maps:merge(maps:merge(?SIZE_DOCS, ?AXIS_DOCS),
                                 #{horizontal => <<"Bars grow to the right; categories on the y axis.">>,
                                   bar_width => <<"Bar width in pixels or a percentage (<<\"30%\">>).">>}),
       methods => ?CHART_METHODS},
     #{name => donut_chart, category => data,
       signature => <<"donut_chart(Items, Css, Attrs)">>,
       root => <<"ah-chart">>, flags => [pie, loading, disabled],
       classes => #{pie => [], loading => []},
       options => [title, colors, legend, labels, tooltip, radius, center,
                   width, height, renderer],
       behavior => <<"chart">>, events => Events,
       doc => <<"A donut or pie chart of named values with percentages.">>,
       option_docs => maps:merge(?SIZE_DOCS,
                                 #{pie => <<"A full pie, without the hole.">>,
                                   title => <<"A title above the chart.">>,
                                   colors => maps:get(colors, ?AXIS_DOCS),
                                   legend => <<"Legend position: right (default), left, top, "
                                               "bottom or none.">>,
                                   labels => <<"Name and percentage next to each slice (default true).">>,
                                   tooltip => <<"Show a tooltip on hover (default true).">>,
                                   radius => <<"{Inner, Outer} radius, numbers or percentages "
                                               "(default {\"50%\", \"70%\"}).">>,
                                   center => <<"{X, Y} of the centre (default beside the legend).">>}),
       methods => ?CHART_METHODS},
     #{name => radar_chart, category => data,
       signature => <<"radar_chart(Series, Css, Attrs)">>,
       root => <<"ah-chart">>,
       groups => #{shape => {[polygon, circle], polygon}},
       flags => [loading, disabled],
       classes => #{polygon => [], circle => [], loading => []},
       options => [indicators, title, colors, legend, tooltip, split_number, radius,
                   area_opacity, width, height, renderer],
       behavior => <<"chart">>, events => Events,
       doc => <<"A radar chart comparing series over several named axes.">>,
       option_docs => maps:merge(?SIZE_DOCS,
                                 #{indicators => <<"The axes: {Name, Max} or #{name, max, min}.">>,
                                   title => <<"A title above the chart.">>,
                                   colors => maps:get(colors, ?AXIS_DOCS),
                                   legend => maps:get(legend, ?AXIS_DOCS),
                                   tooltip => <<"Show a tooltip on hover (default true).">>,
                                   split_number => <<"Number of rings (default 4).">>,
                                   radius => <<"Radius, pixels or a percentage (default \"62%\").">>,
                                   area_opacity => <<"Opacity of the filled areas, 0..1 (default 0.18).">>}),
       methods => ?CHART_METHODS},
     #{name => relation_graph, category => data,
       signature => <<"relation_graph(Graph, Css, Attrs)">>,
       root => <<"ah-relation-graph">>,
       groups => #{layout => {[force, circular, fixed, tree], force},
                   orient => {[lr, tb, rl, bt], lr},
                   node_shape => {[circle, square, round_rect], circle}},
       flags => [directed, loading],
       classes => #{force => [], circular => [], fixed => [], tree => [],
                    lr => [], tb => [], rl => [], bt => [],
                    circle => [], square => [], round_rect => [],
                    directed => [], loading => []},
       options => [edge_labels, roam, selected, focus, details, error, empty_text, toolbar,
                   width, height, renderer],
       behavior => <<"relation-graph">>,
       events => [<<"ah:select">>, <<"ah:node-click">>, <<"ah:refresh">> | Events],
       doc => <<"A node-link graph (force, circular, fixed or tree layout) with categories, "
                "selection, a detail card, a toolbar and loading / error / empty states.">>,
       option_docs =>
           #{directed => <<"Arrows at the target end of the edges.">>,
             loading => <<"Show the loading state over the graph.">>,
             edge_labels => <<"Show edge labels: auto (default: when an edge has one), true or false.">>,
             roam => <<"Pan and zoom with the mouse (default true).">>,
             selected => <<"The id of the selected node.">>,
             focus => <<"The id of a node to centre in view (force, circular and fixed layouts).">>,
             details => <<"#{NodeId => Html}: shown in the detail card when that node is selected.">>,
             error => <<"An error message shown instead of the graph.">>,
             empty_text => <<"Shown when there are no nodes (default \"No data\").">>,
             toolbar => <<"Show the fit and refresh buttons (default true).">>,
             width => maps:get(width, ?SIZE_DOCS),
             height => <<"Height in pixels or a CSS length (default 420).">>,
             renderer => maps:get(renderer, ?SIZE_DOCS)},
       methods =>
           [#{name => select, args => <<"(Id)">>,
              doc => <<"Select a node (null clears), without firing ah:select.">>},
            #{name => getSelected, args => <<"()">>, doc => <<"Return the selected node id.">>},
            #{name => focus, args => <<"(Id)">>, doc => <<"Pan the node into the centre.">>},
            #{name => fit, args => <<"()">>, doc => <<"Redraw the graph, resetting pan and zoom.">>},
            #{name => setOption, args => <<"(Option, NotMerge)">>,
              doc => <<"Apply an echarts option to the graph (chart_update/3 sends the "
                       "option of a relation_graph record).">>}]}].

%%%===================================================================
%%% Internal
%%%===================================================================

entry(Name) -> aihtml_catalog:entry(?MODULE, Name).

%% The 0-based position of X in L, or false.
index_of(_, [], _) -> false;
index_of(X, [X | _], I) -> I;
index_of(X, [_ | T], I) -> index_of(X, T, I + 1).

bool(K, V) ->
    is_boolean(V) orelse error({aihtml, {bad_option, K, V}}),
    V.

list(_, L) when is_list(L) -> L;
list(K, V) -> error({aihtml, {bad_option, K, V}}).

text(B) when is_binary(B) -> B;
text(A) when is_atom(A) -> atom_to_binary(A);
text(I) when is_integer(I) -> integer_to_binary(I);
text(F) when is_float(F) -> float_to_binary(F, [short]);
text(L) when is_list(L) ->
    case unicode:characters_to_binary(L) of
        B when is_binary(B) -> B;
        _ -> error({aihtml, {bad_text, L}})
    end;
text(X) -> error({aihtml, {bad_text, X}}).
