%% @doc Demos of the area chart (aihtml_area_chart), shown on
%% /components/area_chart. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
%%
%% The module is also the action module of the demos that talk to the
%% server: new data for a chart.
-module(aihtml_example_demo_area_chart).
-behaviour(aihtml_action).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0, action/4]).
-export([area_basic/0, area_line/0, area_stack/0, area_live/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => area_chart, title => <<"AreaChart">>,
      summary => <<"面积图 / 折线图：传入系列和类目，服务端生成 ECharts option。"/utf8>>,
      demos => [{<<"平滑面积图"/utf8>>, area_basic},
                {<<"折线、直线段、Y 轴名称"/utf8>>, area_line},
                {<<"堆叠面积"/utf8>>, area_stack},
                {<<"服务端刷新数据"/utf8>>, area_live}]}].

%%%===================================================================
%%% AreaChart
%%%===================================================================

-spec area_basic() -> aihtml:html().
area_basic() ->
    ah_area_chart([{<<"活跃用户"/utf8>>, [1200, 1800, 2400, 3200, 4100, 5200]},
                   {<<"新注册"/utf8>>, [400, 600, 800, 1000, 1200, 1500]}],
                  [], [{categories, months()}, {title, <<"用户增长趋势"/utf8>>}, {height, 300}]).

-spec area_line() -> aihtml:html().
area_line() ->
    ah_area_chart([{<<"响应时间"/utf8>>, [120, 132, 101, 134, 90, 230, 210]}],
                  [line, straight],
                  [{categories, [<<"周一"/utf8>>, <<"周二"/utf8>>, <<"周三"/utf8>>, <<"周四"/utf8>>,
                                 <<"周五"/utf8>>, <<"周六"/utf8>>, <<"周日"/utf8>>]},
                   {y_name, <<"毫秒"/utf8>>}, {legend, none}, {height, 260}]).

-spec area_stack() -> aihtml:html().
area_stack() ->
    ah_area_chart([{<<"邮件"/utf8>>, [120, 132, 101, 134, 90, 230]},
                   {<<"广告"/utf8>>, [220, 182, 191, 234, 290, 330]},
                   {<<"搜索"/utf8>>, [320, 332, 301, 334, 390, 330]}],
                  [stack], [{categories, months()}, {legend, top}, {height, 300}]).

-spec area_live() -> aihtml:html().
area_live() ->
    ah_div([ah_area_chart(visits(), [], [{id, <<"live-area">>}, {categories, hours()}, {height, 260}]),
            ah_button(<<"刷新数据"/utf8>>, refresh, [outlined],
                      [on(click, {?MODULE, refresh_visits, #{}})])],
           [<<"flex flex-col gap-2 items-start">>], []).

%%%===================================================================
%%% Actions
%%%===================================================================

-spec action(atom(), term(), aihtml_action:event(), aihtml_action:ctx()) -> ok.
action(refresh_visits, _Args, _Event, Ctx) ->
    chart_update(Ctx, {id, <<"live-area">>}, #ah_area_chart{series = visits(),
                                                            categories = hours()}).

%%%===================================================================
%%% Data
%%%===================================================================

months() ->
    [<<"1月"/utf8>>, <<"2月"/utf8>>, <<"3月"/utf8>>, <<"4月"/utf8>>, <<"5月"/utf8>>, <<"6月"/utf8>>].

hours() -> [<<"08:00">>, <<"10:00">>, <<"12:00">>, <<"14:00">>, <<"16:00">>, <<"18:00">>].

visits() ->
    [{<<"访问量"/utf8>>, [rand:uniform(900) + 100 || _ <- hours()]},
     {<<"下单量"/utf8>>, [rand:uniform(300) || _ <- hours()]}].
