%% @doc Demos of the chart component (aihtml_chart), shown on
%% /components/chart. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
%%
%% The module is also the action module of the demos that talk to the
%% server: a clicked bar or slice.
-module(aihtml_example_demo_chart).
-behaviour(aihtml_action).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0, action/4]).
-export([chart_line/0, chart_mixed/0, chart_scatter/0, chart_heatmap/0, chart_funnel/0,
         chart_tokens/0, chart_states/0, chart_click/0, chart_record/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => chart, title => <<"Chart">>,
      summary => <<"通用 ECharts 图表：服务端用 Erlang map 写 option，按当前主题配色，主题切换时自动重绘。"/utf8>>,
      demos => [{<<"折线图、标注点与均线"/utf8>>, chart_line},
                {<<"柱线混合、双 Y 轴"/utf8>>, chart_mixed},
                {<<"散点图"/utf8>>, chart_scatter},
                {<<"热力图与视觉映射"/utf8>>, chart_heatmap},
                {<<"漏斗图"/utf8>>, chart_funnel},
                {<<"在 option 里引用主题色"/utf8>>, chart_tokens},
                {<<"加载中与禁用"/utf8>>, chart_states},
                {<<"点击数据项通知服务端"/utf8>>, chart_click},
                {<<"record 写法"/utf8>>, chart_record}]}].

%%%===================================================================
%%% Chart
%%%===================================================================

-spec chart_line() -> aihtml:html().
chart_line() ->
    ah_chart(#{tooltip => #{trigger => axis},
               xAxis => #{type => category, boundaryGap => false, data => months()},
               yAxis => #{type => value},
               series => [#{name => <<"销售额"/utf8>>, type => line, smooth => true,
                            data => [820, 932, 901, 1290, 1330, 1520],
                            markPoint => #{data => [#{type => max, name => <<"最大值"/utf8>>},
                                                    #{type => min, name => <<"最小值"/utf8>>}]},
                            markLine => #{data => [#{type => average, name => <<"平均值"/utf8>>}]}}]},
             [], [{height, 300}, {aria_label, <<"上半年销售额折线图"/utf8>>}]).

-spec chart_mixed() -> aihtml:html().
chart_mixed() ->
    ah_chart(#{tooltip => #{trigger => axis, axisPointer => #{type => cross}},
               legend => #{top => 0},
               xAxis => #{type => category, data => [<<"Q1">>, <<"Q2">>, <<"Q3">>, <<"Q4">>]},
               yAxis => [#{type => value, name => <<"金额 (万)"/utf8>>},
                         #{type => value, name => <<"增长率 (%)"/utf8>>, alignTicks => true}],
               series => [#{name => <<"收入"/utf8>>, type => bar, data => [1200, 1800, 2400, 3000]},
                          #{name => <<"利润"/utf8>>, type => bar, data => [300, 500, 800, 1200]},
                          #{name => <<"增长率"/utf8>>, type => line, yAxisIndex => 1,
                            data => [12, 50, 33, 25]}]},
             [], [{height, 320}]).

-spec chart_scatter() -> aihtml:html().
chart_scatter() ->
    Points = [[H, W] || {H, W} <- [{161, 51}, {167, 59}, {159, 49}, {157, 63}, {155, 53},
                                   {170, 59}, {172, 75}, {175, 68}, {180, 82}, {168, 64},
                                   {178, 70}, {163, 57}, {185, 90}, {173, 66}, {166, 58}]],
    ah_chart(#{tooltip => #{trigger => item},
               xAxis => #{type => value, name => <<"身高 cm"/utf8>>, scale => true},
               yAxis => #{type => value, name => <<"体重 kg"/utf8>>, scale => true},
               series => [#{type => scatter, symbolSize => 12, data => Points}]},
             [], [{height, 300}]).

-spec chart_heatmap() -> aihtml:html().
chart_heatmap() ->
    Days = [<<"周一"/utf8>>, <<"周二"/utf8>>, <<"周三"/utf8>>, <<"周四"/utf8>>, <<"周五"/utf8>>],
    Hours = [<<"9:00">>, <<"11:00">>, <<"13:00">>, <<"15:00">>, <<"17:00">>],
    Data = [[H, D, (H * 3 + D * 5) rem 10] || H <- lists:seq(0, 4), D <- lists:seq(0, 4)],
    ah_chart(#{tooltip => #{position => top},
               grid => #{top => 10, bottom => 60, left => 60},
               xAxis => #{type => category, data => Hours},
               yAxis => #{type => category, data => Days},
               visualMap => #{min => 0, max => 9, orient => horizontal, left => center, bottom => 0,
                              calculable => true,
                              inRange => #{color => [<<"--ah-color-primary-lighter">>,
                                                     <<"--ah-color-primary">>]}},
               series => [#{type => heatmap, data => Data, label => #{show => true}}]},
             [], [{height, 320}]).

-spec chart_funnel() -> aihtml:html().
chart_funnel() ->
    ah_chart(#{tooltip => #{trigger => item, formatter => <<"{b}: {c}">>},
               series => [#{type => funnel, left => <<"10%">>, width => <<"80%">>, sort => descending,
                            label => #{position => inside},
                            data => [#{name => <<"访问"/utf8>>, value => 1000},
                                     #{name => <<"注册"/utf8>>, value => 620},
                                     #{name => <<"下单"/utf8>>, value => 310},
                                     #{name => <<"支付"/utf8>>, value => 240}]}]},
             [], [{height, 300}]).

-spec chart_tokens() -> aihtml:html().
chart_tokens() ->
    ah_chart(#{xAxis => #{type => category, data => [<<"成功"/utf8>>, <<"警告"/utf8>>, <<"失败"/utf8>>]},
               yAxis => #{type => value},
               series => [#{type => bar, barWidth => <<"40%">>,
                            data => [#{value => 86, itemStyle => #{color => <<"--ah-color-success">>}},
                                     #{value => 23, itemStyle => #{color => <<"--ah-color-warning">>}},
                                     #{value => 7, itemStyle => #{color => <<"var(--ah-color-error)">>}}]}]},
             [], [{height, 260}]).

-spec chart_states() -> aihtml:html().
chart_states() ->
    Option = #{xAxis => #{type => category, data => [<<"A">>, <<"B">>, <<"C">>]},
               yAxis => #{type => value},
               series => [#{type => bar, data => [5, 9, 4]}]},
    row([box(ah_chart(Option, [loading], [{height, 220}])),
         box(ah_chart(Option, [disabled], [{height, 220}]))]).

-spec chart_click() -> aihtml:html().
chart_click() ->
    ah_div([ah_bar_chart([{<<"订单"/utf8>>, [120, 200, 150, 80]}], [],
                         [{categories, [<<"北京"/utf8>>, <<"上海"/utf8>>, <<"广州"/utf8>>, <<"深圳"/utf8>>]},
                          {height, 260},
                          on('ah:chart-click', {?MODULE, chart_clicked, #{}})]),
            ah_span(<<"点击一根柱子"/utf8>>, [<<"text-sm text-muted">>], [{id, <<"chart-clicked">>}])],
           [], []).

%% The same components as records: options are checked field names, and
%% the postback runs action(chart_clicked, ...) below on a click.
-spec chart_record() -> aihtml:html().
chart_record() ->
    row([box(#ah_donut_chart{items = [{<<"直接访问"/utf8>>, 335}, {<<"搜索引擎"/utf8>>, 1548},
                                      {<<"邮件营销"/utf8>>, 310}],
                             legend = bottom, labels = false, height = 260,
                             postback = chart_clicked}),
         box(#ah_chart{option = #{xAxis => #{type => category, data => [a, b, c]},
                                  yAxis => #{type => value},
                                  series => [#{type => line, data => [3, 7, 5]}]},
                       height = 260, renderer = svg}),
         ah_span(<<"点击一块扇区"/utf8>>, [<<"text-sm text-muted">>], [{id, <<"chart-clicked">>}])]).

%%%===================================================================
%%% Actions
%%%===================================================================

-spec action(atom(), term(), aihtml_action:event(), aihtml_action:ctx()) -> ok.
action(chart_clicked, _Args, #{data := Data, value := Name}, Ctx) ->
    aihtml_action:html(Ctx, {id, <<"chart-clicked">>},
                       [<<"服务端收到："/utf8>>, maps:get(<<"series">>, Data, <<>>), <<" / "/utf8>>,
                        Name, <<" = "/utf8>>, maps:get(<<"value">>, Data, <<>>)]).

%%%===================================================================
%%% Data
%%%===================================================================

months() ->
    [<<"1月"/utf8>>, <<"2月"/utf8>>, <<"3月"/utf8>>, <<"4月"/utf8>>, <<"5月"/utf8>>, <<"6月"/utf8>>].

box(Chart) ->
    ah_div(Chart, [<<"flex-1 min-w-64 rounded-md border border-line p-2">>], []).

row(Children) ->
    ah_div(Children, [<<"flex flex-wrap items-start gap-4">>], []).
