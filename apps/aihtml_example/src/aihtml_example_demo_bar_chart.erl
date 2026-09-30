%% @doc Demos of the bar chart (aihtml_bar_chart), shown on
%% /components/bar_chart. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
-module(aihtml_example_demo_bar_chart).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0]).
-export([bar_basic/0, bar_horizontal/0, bar_stack/0, bar_colors/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => bar_chart, title => <<"BarChart">>,
      summary => <<"柱状图：分组、堆叠或横向，圆角柱子，按主题配色。"/utf8>>,
      demos => [{<<"分组柱状图"/utf8>>, bar_basic},
                {<<"横向柱状图"/utf8>>, bar_horizontal},
                {<<"堆叠柱状图"/utf8>>, bar_stack},
                {<<"自定义颜色与柱宽"/utf8>>, bar_colors}]}].

%%%===================================================================
%%% BarChart
%%%===================================================================

-spec bar_basic() -> aihtml:html().
bar_basic() ->
    ah_bar_chart([{<<"一季度"/utf8>>, [420, 380, 290, 150, 80]},
                  {<<"二季度"/utf8>>, [480, 420, 350, 180, 95]}],
                 [], [{categories, regions()}, {title, <<"各区域销售额"/utf8>>}, {height, 300}]).

-spec bar_horizontal() -> aihtml:html().
bar_horizontal() ->
    ah_bar_chart([{<<"销量"/utf8>>, [850, 620, 480, 320]}], [horizontal],
                 [{categories, [<<"产品 A"/utf8>>, <<"产品 B"/utf8>>, <<"产品 C"/utf8>>,
                                <<"产品 D"/utf8>>]},
                  {legend, none}, {height, 260}]).

-spec bar_stack() -> aihtml:html().
bar_stack() ->
    ah_bar_chart([{<<"线上"/utf8>>, [320, 400, 480, 560]},
                  {<<"门店"/utf8>>, [180, 220, 260, 300]}],
                 [stack], [{categories, [<<"Q1">>, <<"Q2">>, <<"Q3">>, <<"Q4">>]}, {height, 300}]).

-spec bar_colors() -> aihtml:html().
bar_colors() ->
    ah_bar_chart([#{name => <<"完成"/utf8>>, data => [8, 12, 5], color => <<"--ah-color-success">>},
                  #{name => <<"逾期"/utf8>>, data => [2, 1, 4], color => <<"#e8684a">>}],
                 [], [{categories, [<<"设计"/utf8>>, <<"开发"/utf8>>, <<"测试"/utf8>>]},
                      {bar_width, 18}, {grid, false}, {legend, right}, {height, 260}]).

%%%===================================================================
%%% Data
%%%===================================================================

regions() ->
    [<<"华北"/utf8>>, <<"华东"/utf8>>, <<"华南"/utf8>>, <<"西南"/utf8>>, <<"东北"/utf8>>].
