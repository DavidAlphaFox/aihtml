%% @doc Demos of the slider component (aihtml_slider), shown on
%% /components/slider. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
-module(aihtml_example_demo_slider).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0]).
-export([slider_basic/0, slider_range/0, slider_ticks/0, slider_vertical/0,
         slider_record/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => slider, title => <<"Slider">>,
       summary => <<"拖动或用键盘选择数值，也可以选一个区间。"/utf8>>,
       demos => [{<<"单值与提示气泡"/utf8>>, slider_basic},
                 {<<"区间选择"/utf8>>, slider_range},
                 {<<"刻度与步进按钮"/utf8>>, slider_ticks},
                 {<<"竖向与禁用"/utf8>>, slider_vertical},
                 {<<"record 写法"/utf8>>, slider_record}]}].

-spec slider_basic() -> aihtml:html().
slider_basic() ->
    'div'([slider({0, 100}, 40, [tooltip], [{name, volume}, {aria_label, <<"Volume">>}]),
           slider({0, 100}, 70, [success], [{aria_label, <<"Brightness">>}])],
          [<<"flex flex-col gap-6 max-w-sm">>], []).

-spec slider_range() -> aihtml:html().
slider_range() ->
    'div'(slider({0, 1000, 50}, {200, 600}, [tooltip],
                 [{name, price}, {min_range, 100}, {ticks, 250}]),
          [<<"max-w-sm">>], []).

-spec slider_ticks() -> aihtml:html().
slider_ticks() ->
    'div'([slider({0, 10}, 3, [buttons, warning], [{ticks, 1}, {ticks_position, both}]),
           slider({0, 1, 0.05}, 0.5, [info], [{ticks, 0.25}, {minor_ticks, 0.05}])],
          [<<"flex flex-col gap-8 max-w-sm py-4">>], []).

-spec slider_vertical() -> aihtml:html().
slider_vertical() ->
    row([slider({0, 100}, 60, [vertical], [{ticks, 25}]),
         slider({0, 100}, {20, 80}, [vertical, secondary], []),
         slider({0, 100}, 30, [vertical, disabled], [])]).

-spec slider_record() -> aihtml:html().
slider_record() ->
    'div'(#ah_slider{range = {0, 1000, 50}, value = {200, 600}, template = success,
                     tooltip = true, ticks = 250, minor_ticks = 50, min_range = 100,
                     name = price},
          [<<"max-w-sm">>], []).

row(Children) ->
    'div'(Children, [<<"flex flex-wrap items-start gap-4">>], []).
