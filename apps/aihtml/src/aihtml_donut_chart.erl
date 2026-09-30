%%%-------------------------------------------------------------------
%%% @doc The donut chart, ported from sigil (data/donut_chart): a donut
%%% or pie of named values. The echarts option is built here, as sigil's
%%% donut-chart/create does, and drawn like any chart (see aihtml_chart
%%% and aihtml_lib_chart).
%%%
%%% ah_donut_chart/3 builds an element record (#ah_donut_chart{}, defined in
%%% include/aihtml_donut_chart.hrl) and render/1 turns it into HTML, so
%%% pages may also write the record directly (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_donut_chart).
-behaviour(aihtml_element).

-include("aihtml_donut_chart.hrl").

-export([ah_donut_chart/3, option/1, render/1, fields/1, catalog/0]).

-export_type([element/0, item/0]).

-import(aihtml_lib_chart, [chart_root/8, common/7, legend_ok/1, bool/2, list/2, text/1]).

-define(E, aihtml_element).
-define(L, aihtml_lib_chart).

-type css() :: aihtml_html:css().
-type attrs() :: aihtml_html:attrs().
-type html() :: aihtml_html:html().
-type element() :: #ah_donut_chart{}.
%% A slice of a donut chart.
-type item() :: {aihtml_lib_chart:text(), number()}
              | #{name := aihtml_lib_chart:text(), value := number(), color => binary()}.

%%%===================================================================
%%% Builder
%%%===================================================================

%% @doc A donut chart: `Items' are `{Name, Value}' (or maps with `color').
%% Css: `pie' (no hole), `loading', `disabled'. Options: `title',
%% `colors', `legend' (default right), `labels' (boolean), `tooltip',
%% `radius' ({Inner, Outer}), `center' ({X, Y}), `height', `width',
%% `renderer'.
-spec ah_donut_chart([item()], css(), attrs()) -> #ah_donut_chart{}.
ah_donut_chart(Items, Css, Attrs) ->
    ?E:build(?MODULE, #ah_donut_chart{items = Items}, Css, Attrs).

%% @doc The field names of the record.
-spec fields(atom()) -> [atom()].
fields(ah_donut_chart) -> record_info(fields, ah_donut_chart).

%% @doc Internal: the echarts option of a record (aihtml_chart:chart_option/1
%% dispatches here), after the same checks as render/1.
-spec option(element()) -> aihtml_lib_chart:option().
option(#ah_donut_chart{items = Items0, pie = Pie, title = Title, colors = Colors,
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
%%% Rendering
%%%===================================================================

-spec render(element()) -> html().
render(#ah_donut_chart{loading = L, disabled = D, width = W, height = H, renderer = Rd} = R) ->
    Classes = ?E:classes(?MODULE, R),
    chart_root(R, Classes, option(R), L, D, W, H, Rd).

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => donut_chart, category => data,
       signature => <<"ah_donut_chart(Items, Css, Attrs)">>,
       root => <<"ah-chart">>, flags => [pie, loading, disabled],
       classes => #{pie => [], loading => []},
       options => [title, colors, legend, labels, tooltip, radius, center,
                   width, height, renderer],
       behavior => <<"chart">>, events => ?L:events(),
       doc => <<"A donut or pie chart of named values with percentages.">>,
       option_docs => maps:merge(?L:size_docs(),
                                 #{pie => <<"A full pie, without the hole.">>,
                                   title => <<"A title above the chart.">>,
                                   colors => maps:get(colors, ?L:axis_docs()),
                                   legend => <<"Legend position: right (default), left, top, "
                                               "bottom or none.">>,
                                   labels => <<"Name and percentage next to each slice (default true).">>,
                                   tooltip => <<"Show a tooltip on hover (default true).">>,
                                   radius => <<"{Inner, Outer} radius, numbers or percentages "
                                               "(default {\"50%\", \"70%\"}).">>,
                                   center => <<"{X, Y} of the centre (default beside the legend).">>}),
       methods => ?L:methods()}].
