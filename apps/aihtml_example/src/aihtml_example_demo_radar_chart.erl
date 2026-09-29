%% @doc Demos of the radar chart (aihtml_radar_chart), shown on
%% /components/radar_chart. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
-module(aihtml_example_demo_radar_chart).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0]).
-export([radar_basic/0, radar_circle/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => radar_chart, title => <<"RadarChart">>,
      summary => <<"雷达图：在多个维度上对比几组数据。"/utf8>>,
      demos => [{<<"多边形雷达"/utf8>>, radar_basic},
                {<<"圆形雷达、单组数据"/utf8>>, radar_circle}]}].

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
