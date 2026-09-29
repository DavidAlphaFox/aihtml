%% @doc Demos of the donut chart (aihtml_donut_chart), shown on
%% /components/donut_chart. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
-module(aihtml_example_demo_donut_chart).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0]).
-export([donut_basic/0, donut_pie/0, donut_plain/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => donut_chart, title => <<"DonutChart">>,
      summary => <<"环形图 / 饼图：展示各部分占比，带图例和百分比标签。"/utf8>>,
      demos => [{<<"环形图"/utf8>>, donut_basic},
                {<<"饼图、底部图例"/utf8>>, donut_pie},
                {<<"无标签、自定义颜色"/utf8>>, donut_plain}]}].

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
