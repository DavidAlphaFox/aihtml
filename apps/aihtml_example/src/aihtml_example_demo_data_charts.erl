%% @doc Demos of the chart components (aihtml_data_charts), shown on
%% /components/<name>. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
%%
%% The module is also the action module of the demos that talk to the
%% server: a clicked bar, new data for a chart, a selected graph node.
-module(aihtml_example_demo_data_charts).
-behaviour(aihtml_action).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0, action/4]).
-export([chart_line/0, chart_mixed/0, chart_scatter/0, chart_heatmap/0, chart_funnel/0,
         chart_tokens/0, chart_states/0, chart_click/0, chart_record/0,
         area_basic/0, area_line/0, area_stack/0, area_live/0,
         bar_basic/0, bar_horizontal/0, bar_stack/0, bar_colors/0,
         donut_basic/0, donut_pie/0, donut_plain/0,
         radar_basic/0, radar_circle/0,
         graph_force/0, graph_circular/0, graph_tree/0, graph_fixed/0, graph_states/0,
         graph_select/0]).

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
                 {<<"record 写法"/utf8>>, chart_record}]},
     #{component => area_chart, title => <<"AreaChart">>,
       summary => <<"面积图 / 折线图：传入系列和类目，服务端生成 ECharts option。"/utf8>>,
       demos => [{<<"平滑面积图"/utf8>>, area_basic},
                 {<<"折线、直线段、Y 轴名称"/utf8>>, area_line},
                 {<<"堆叠面积"/utf8>>, area_stack},
                 {<<"服务端刷新数据"/utf8>>, area_live}]},
     #{component => bar_chart, title => <<"BarChart">>,
       summary => <<"柱状图：分组、堆叠或横向，圆角柱子，按主题配色。"/utf8>>,
       demos => [{<<"分组柱状图"/utf8>>, bar_basic},
                 {<<"横向柱状图"/utf8>>, bar_horizontal},
                 {<<"堆叠柱状图"/utf8>>, bar_stack},
                 {<<"自定义颜色与柱宽"/utf8>>, bar_colors}]},
     #{component => donut_chart, title => <<"DonutChart">>,
       summary => <<"环形图 / 饼图：展示各部分占比，带图例和百分比标签。"/utf8>>,
       demos => [{<<"环形图"/utf8>>, donut_basic},
                 {<<"饼图、底部图例"/utf8>>, donut_pie},
                 {<<"无标签、自定义颜色"/utf8>>, donut_plain}]},
     #{component => radar_chart, title => <<"RadarChart">>,
       summary => <<"雷达图：在多个维度上对比几组数据。"/utf8>>,
       demos => [{<<"多边形雷达"/utf8>>, radar_basic},
                 {<<"圆形雷达、单组数据"/utf8>>, radar_circle}]},
     #{component => relation_graph, title => <<"RelationGraph">>,
       summary => <<"关系图：力导向、环形、树形或固定坐标布局，支持分类配色、选中节点和详情卡。"/utf8>>,
       demos => [{<<"力导向布局、分类与详情卡"/utf8>>, graph_force},
                 {<<"环形布局、有向边与边标签"/utf8>>, graph_circular},
                 {<<"树形布局"/utf8>>, graph_tree},
                 {<<"固定坐标、圆角矩形节点"/utf8>>, graph_fixed},
                 {<<"加载中、空数据与出错"/utf8>>, graph_states},
                 {<<"选中节点通知服务端"/utf8>>, graph_select}]}].

%%%===================================================================
%%% Chart
%%%===================================================================

-spec chart_line() -> aihtml:html().
chart_line() ->
    chart(#{tooltip => #{trigger => axis},
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
    chart(#{tooltip => #{trigger => axis, axisPointer => #{type => cross}},
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
    chart(#{tooltip => #{trigger => item},
            xAxis => #{type => value, name => <<"身高 cm"/utf8>>, scale => true},
            yAxis => #{type => value, name => <<"体重 kg"/utf8>>, scale => true},
            series => [#{type => scatter, symbolSize => 12, data => Points}]},
          [], [{height, 300}]).

-spec chart_heatmap() -> aihtml:html().
chart_heatmap() ->
    Days = [<<"周一"/utf8>>, <<"周二"/utf8>>, <<"周三"/utf8>>, <<"周四"/utf8>>, <<"周五"/utf8>>],
    Hours = [<<"9:00">>, <<"11:00">>, <<"13:00">>, <<"15:00">>, <<"17:00">>],
    Data = [[H, D, (H * 3 + D * 5) rem 10] || H <- lists:seq(0, 4), D <- lists:seq(0, 4)],
    chart(#{tooltip => #{position => top},
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
    chart(#{tooltip => #{trigger => item, formatter => <<"{b}: {c}">>},
            series => [#{type => funnel, left => <<"10%">>, width => <<"80%">>, sort => descending,
                         label => #{position => inside},
                         data => [#{name => <<"访问"/utf8>>, value => 1000},
                                  #{name => <<"注册"/utf8>>, value => 620},
                                  #{name => <<"下单"/utf8>>, value => 310},
                                  #{name => <<"支付"/utf8>>, value => 240}]}]},
          [], [{height, 300}]).

-spec chart_tokens() -> aihtml:html().
chart_tokens() ->
    chart(#{xAxis => #{type => category, data => [<<"成功"/utf8>>, <<"警告"/utf8>>, <<"失败"/utf8>>]},
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
    row([box(chart(Option, [loading], [{height, 220}])),
         box(chart(Option, [disabled], [{height, 220}]))]).

-spec chart_click() -> aihtml:html().
chart_click() ->
    'div'([bar_chart([{<<"订单"/utf8>>, [120, 200, 150, 80]}], [],
                     [{categories, [<<"北京"/utf8>>, <<"上海"/utf8>>, <<"广州"/utf8>>, <<"深圳"/utf8>>]},
                      {height, 260},
                      on('ah:chart-click', {?MODULE, chart_clicked, #{}})]),
           span(<<"点击一根柱子"/utf8>>, [<<"text-sm text-muted">>], [{id, <<"chart-clicked">>}])],
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
         span(<<"点击一块扇区"/utf8>>, [<<"text-sm text-muted">>], [{id, <<"chart-clicked">>}])]).

%%%===================================================================
%%% AreaChart
%%%===================================================================

-spec area_basic() -> aihtml:html().
area_basic() ->
    area_chart([{<<"活跃用户"/utf8>>, [1200, 1800, 2400, 3200, 4100, 5200]},
                {<<"新注册"/utf8>>, [400, 600, 800, 1000, 1200, 1500]}],
               [], [{categories, months()}, {title, <<"用户增长趋势"/utf8>>}, {height, 300}]).

-spec area_line() -> aihtml:html().
area_line() ->
    area_chart([{<<"响应时间"/utf8>>, [120, 132, 101, 134, 90, 230, 210]}],
               [line, straight],
               [{categories, [<<"周一"/utf8>>, <<"周二"/utf8>>, <<"周三"/utf8>>, <<"周四"/utf8>>,
                              <<"周五"/utf8>>, <<"周六"/utf8>>, <<"周日"/utf8>>]},
                {y_name, <<"毫秒"/utf8>>}, {legend, none}, {height, 260}]).

-spec area_stack() -> aihtml:html().
area_stack() ->
    area_chart([{<<"邮件"/utf8>>, [120, 132, 101, 134, 90, 230]},
                {<<"广告"/utf8>>, [220, 182, 191, 234, 290, 330]},
                {<<"搜索"/utf8>>, [320, 332, 301, 334, 390, 330]}],
               [stack], [{categories, months()}, {legend, top}, {height, 300}]).

-spec area_live() -> aihtml:html().
area_live() ->
    'div'([area_chart(visits(), [], [{id, <<"live-area">>}, {categories, hours()}, {height, 260}]),
           button(<<"刷新数据"/utf8>>, refresh, [outlined],
                  [on(click, {?MODULE, refresh_visits, #{}})])],
          [<<"flex flex-col gap-2 items-start">>], []).

%%%===================================================================
%%% BarChart
%%%===================================================================

-spec bar_basic() -> aihtml:html().
bar_basic() ->
    bar_chart([{<<"一季度"/utf8>>, [420, 380, 290, 150, 80]},
               {<<"二季度"/utf8>>, [480, 420, 350, 180, 95]}],
              [], [{categories, regions()}, {title, <<"各区域销售额"/utf8>>}, {height, 300}]).

-spec bar_horizontal() -> aihtml:html().
bar_horizontal() ->
    bar_chart([{<<"销量"/utf8>>, [850, 620, 480, 320]}], [horizontal],
              [{categories, [<<"产品 A"/utf8>>, <<"产品 B"/utf8>>, <<"产品 C"/utf8>>,
                             <<"产品 D"/utf8>>]},
               {legend, none}, {height, 260}]).

-spec bar_stack() -> aihtml:html().
bar_stack() ->
    bar_chart([{<<"线上"/utf8>>, [320, 400, 480, 560]},
               {<<"门店"/utf8>>, [180, 220, 260, 300]}],
              [stack], [{categories, [<<"Q1">>, <<"Q2">>, <<"Q3">>, <<"Q4">>]}, {height, 300}]).

-spec bar_colors() -> aihtml:html().
bar_colors() ->
    bar_chart([#{name => <<"完成"/utf8>>, data => [8, 12, 5], color => <<"--ah-color-success">>},
               #{name => <<"逾期"/utf8>>, data => [2, 1, 4], color => <<"#e8684a">>}],
              [], [{categories, [<<"设计"/utf8>>, <<"开发"/utf8>>, <<"测试"/utf8>>]},
                   {bar_width, 18}, {grid, false}, {legend, right}, {height, 260}]).

%%%===================================================================
%%% DonutChart
%%%===================================================================

-spec donut_basic() -> aihtml:html().
donut_basic() ->
    donut_chart([{<<"Windows">>, 45}, {<<"macOS">>, 28}, {<<"Linux">>, 18},
                 {<<"其他"/utf8>>, 9}],
                [], [{title, <<"操作系统分布"/utf8>>}, {height, 300}]).

-spec donut_pie() -> aihtml:html().
donut_pie() ->
    donut_chart([{<<"直接访问"/utf8>>, 335}, {<<"邮件营销"/utf8>>, 310},
                 {<<"联盟广告"/utf8>>, 234}, {<<"视频广告"/utf8>>, 135}],
                [pie], [{legend, bottom}, {height, 300}]).

-spec donut_plain() -> aihtml:html().
donut_plain() ->
    donut_chart([#{name => <<"已用"/utf8>>, value => 72, color => <<"--ah-color-primary">>},
                 #{name => <<"剩余"/utf8>>, value => 28, color => <<"--ah-color-border">>}],
                [], [{labels, false}, {legend, none}, {radius, {<<"60%">>, <<"80%">>}},
                     {height, 200}, {width, 240}]).

%%%===================================================================
%%% RadarChart
%%%===================================================================

-spec radar_basic() -> aihtml:html().
radar_basic() ->
    radar_chart([{<<"一班"/utf8>>, [80, 50, 30, 40, 100]},
                 {<<"二班"/utf8>>, [60, 90, 70, 80, 50]}],
                [], [{indicators, [{<<"语文"/utf8>>, 120}, {<<"数学"/utf8>>, 120},
                                   {<<"英语"/utf8>>, 120}, {<<"物理"/utf8>>, 120},
                                   {<<"化学"/utf8>>, 120}]},
                     {height, 320}]).

-spec radar_circle() -> aihtml:html().
radar_circle() ->
    radar_chart([{<<"候选人"/utf8>>, [9, 7, 8, 6, 9, 7]}], [circle],
                [{indicators, [{<<"沟通"/utf8>>, 10}, {<<"技术"/utf8>>, 10}, {<<"协作"/utf8>>, 10},
                               {<<"领导力"/utf8>>, 10}, {<<"学习"/utf8>>, 10},
                               {<<"执行"/utf8>>, 10}]},
                 {legend, none}, {split_number, 5}, {area_opacity, 0.3}, {height, 300}]).

%%%===================================================================
%%% RelationGraph
%%%===================================================================

-spec graph_force() -> aihtml:html().
graph_force() ->
    relation_graph(team(), [],
                   [{selected, <<"li">>},
                    {details, #{<<"li">> => [strong(<<"李雷"/utf8>>, [], []),
                                             p(<<"后端负责人，维护订单与支付服务。"/utf8>>, [], [])],
                                <<"han">> => p(<<"韩梅梅：前端负责人。"/utf8>>, [], [])}}]).

-spec graph_circular() -> aihtml:html().
graph_circular() ->
    relation_graph(#{nodes => [<<"网关"/utf8>>, <<"订单"/utf8>>, <<"支付"/utf8>>,
                               <<"库存"/utf8>>, <<"通知"/utf8>>, <<"用户"/utf8>>],
                     edges => [{<<"网关"/utf8>>, <<"订单"/utf8>>, <<"HTTP">>},
                               {<<"网关"/utf8>>, <<"用户"/utf8>>, <<"HTTP">>},
                               {<<"订单"/utf8>>, <<"支付"/utf8>>, <<"RPC">>},
                               {<<"订单"/utf8>>, <<"库存"/utf8>>, <<"RPC">>},
                               #{source => <<"支付"/utf8>>, target => <<"通知"/utf8>>,
                                 label => <<"MQ">>, kind => dashed}]},
                   [circular, directed], [{height, 360}]).

-spec graph_tree() -> aihtml:html().
graph_tree() ->
    relation_graph(#{nodes => [#{id => ceo, label => <<"CEO">>},
                               #{id => cto, label => <<"CTO">>, parent => ceo},
                               #{id => cfo, label => <<"CFO">>, parent => ceo},
                               #{id => fe, label => <<"前端组"/utf8>>, parent => cto},
                               #{id => be, label => <<"后端组"/utf8>>, parent => cto},
                               #{id => fin, label => <<"财务部"/utf8>>, parent => cfo}]},
                   [tree, tb], [{height, 340}]).

-spec graph_fixed() -> aihtml:html().
graph_fixed() ->
    relation_graph(#{nodes => [#{id => a, label => <<"需求评审"/utf8>>, x => 0, y => 0, root => true},
                               #{id => b, label => <<"设计"/utf8>>, x => 200, y => -60},
                               #{id => c, label => <<"开发"/utf8>>, x => 200, y => 60},
                               #{id => d, label => <<"上线"/utf8>>, x => 400, y => 0}],
                     edges => [{a, b}, {a, c}, {b, d}, {c, d}]},
                   [fixed, round_rect, directed], [{height, 280}, {roam, false}]).

-spec graph_states() -> aihtml:html().
graph_states() ->
    'div'([relation_graph(team(), [loading], [{height, 220}]),
           relation_graph(#{nodes => []}, [], [{height, 220}, {empty_text, <<"暂无关系"/utf8>>}]),
           relation_graph(team(), [], [{height, 220}, {toolbar, false},
                                       {error, <<"加载失败，请稍后重试"/utf8>>}])],
          [<<"grid grid-cols-1 md:grid-cols-3 gap-4">>], []).

-spec graph_select() -> aihtml:html().
graph_select() ->
    'div'([#ah_relation_graph{id = <<"team-graph">>, graph = team(), layout = circular,
                              height = 320, postback = node_selected},
           span(<<"点击一个节点，或聚焦图后用方向键切换"/utf8>>, [<<"text-sm text-muted">>],
                [{id, <<"node-selected">>}])],
          [<<"flex flex-col gap-2">>], []).

%%%===================================================================
%%% Actions
%%%===================================================================

-spec action(atom(), term(), aihtml_action:event(), aihtml_action:ctx()) -> ok.
action(chart_clicked, _Args, #{data := Data, value := Name}, Ctx) ->
    aihtml_action:html(Ctx, {id, <<"chart-clicked">>},
                       [<<"服务端收到："/utf8>>, maps:get(<<"series">>, Data, <<>>), <<" / "/utf8>>,
                        Name, <<" = "/utf8>>, maps:get(<<"value">>, Data, <<>>)]);
action(refresh_visits, _Args, _Event, Ctx) ->
    chart_update(Ctx, {id, <<"live-area">>}, #ah_area_chart{series = visits(),
                                                            categories = hours()});
action(node_selected, _Args, #{value := Id}, Ctx) ->
    aihtml_action:html(Ctx, {id, <<"node-selected">>},
                       case Id of
                           <<>> -> <<"没有选中节点"/utf8>>;
                           _ -> [<<"服务端收到节点："/utf8>>, Id]
                       end).

%%%===================================================================
%%% Data
%%%===================================================================

months() ->
    [<<"1月"/utf8>>, <<"2月"/utf8>>, <<"3月"/utf8>>, <<"4月"/utf8>>, <<"5月"/utf8>>, <<"6月"/utf8>>].

hours() -> [<<"08:00">>, <<"10:00">>, <<"12:00">>, <<"14:00">>, <<"16:00">>, <<"18:00">>].

regions() ->
    [<<"华北"/utf8>>, <<"华东"/utf8>>, <<"华南"/utf8>>, <<"西南"/utf8>>, <<"东北"/utf8>>].

visits() ->
    [{<<"访问量"/utf8>>, [rand:uniform(900) + 100 || _ <- hours()]},
     {<<"下单量"/utf8>>, [rand:uniform(300) || _ <- hours()]}].

team() ->
    #{categories => [<<"负责人"/utf8>>, <<"前端"/utf8>>, <<"后端"/utf8>>],
      nodes => [#{id => li, label => <<"李雷"/utf8>>, category => <<"负责人"/utf8>>, root => true},
                #{id => han, label => <<"韩梅梅"/utf8>>, category => <<"负责人"/utf8>>, root => true},
                #{id => wang, label => <<"王"/utf8>>, category => <<"后端"/utf8>>},
                #{id => zhao, label => <<"赵"/utf8>>, category => <<"后端"/utf8>>},
                #{id => chen, label => <<"陈"/utf8>>, category => <<"前端"/utf8>>},
                #{id => liu, label => <<"刘"/utf8>>, category => <<"前端"/utf8>>}],
      edges => [{li, wang}, {li, zhao}, {han, chen}, {han, liu},
                #{source => li, target => han, label => <<"协作"/utf8>>, kind => dashed}]}.

box(Chart) ->
    'div'(Chart, [<<"flex-1 min-w-64 rounded-md border border-line p-2">>], []).

row(Children) ->
    'div'(Children, [<<"flex flex-wrap items-start gap-4">>], []).
