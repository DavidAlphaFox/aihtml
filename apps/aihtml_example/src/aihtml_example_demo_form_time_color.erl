%% @doc Demos of the time and colour pickers (aihtml_form_time_color),
%% shown on /components/<name>. Each function is one example, written the
%% way an application writes it; the docs page prints its source under it.
-module(aihtml_example_demo_form_time_color).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0]).
-export([time_basic/0, time_24h/0, time_range/0, time_states/0,
         time_inline/0, time_landscape/0, time_records/0,
         color_basic/0, color_swatches/0, color_alpha/0, color_states/0,
         color_inline/0, color_compact/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => timepicker, title => <<"TimePicker">>,
       summary => <<"表盘式时间选择，12/24 小时制，值为 HH:MM。"/utf8>>,
       demos => [{<<"弹出表盘"/utf8>>, time_basic},
                 {<<"24 小时制与分钟步长"/utf8>>, time_24h},
                 {<<"可选范围与占位文字"/utf8>>, time_range},
                 {<<"可清除与禁用"/utf8>>, time_states},
                 {<<"内嵌表盘"/utf8>>, time_inline},
                 {<<"横向布局与底部内容"/utf8>>, time_landscape},
                 {<<"record 写法"/utf8>>, time_records}]},
     #{component => colorpicker, title => <<"ColorPicker">>,
       summary => <<"HSV 取色面板加十六进制与 RGB 输入，值为 #rrggbb。"/utf8>>,
       demos => [{<<"弹出取色面板"/utf8>>, color_basic},
                 {<<"预设色板"/utf8>>, color_swatches},
                 {<<"透明度"/utf8>>, color_alpha},
                 {<<"空值、可清除与禁用"/utf8>>, color_states},
                 {<<"内嵌面板"/utf8>>, color_inline},
                 {<<"精简面板与自定义尺寸"/utf8>>, color_compact}]}].

-spec time_basic() -> aihtml:html().
time_basic() ->
    row([timepicker(<<"14:30">>, [<<"w-48">>], [{name, start}]),
         timepicker({9, 0}, [<<"w-48">>], [{name, finish}])]).

-spec time_24h() -> aihtml:html().
time_24h() ->
    row([timepicker(<<"21:15">>, [<<"w-40">>], [{format, '24h'}]),
         timepicker({9, 45}, [<<"w-40">>], [{format, '24h'}, {minute_step, 15},
                                           {name, meeting}])]).

-spec time_range() -> aihtml:html().
time_range() ->
    timepicker(undefined, [<<"w-56">>],
               [{placeholder, <<"Office hours only">>},
                {min, <<"09:00">>}, {max, <<"17:30">>}, {name, visit}]).

-spec time_states() -> aihtml:html().
time_states() ->
    row([timepicker(<<"08:20">>, [clearable, <<"w-48">>], []),
         timepicker(<<"07:05">>, [disabled, <<"w-48">>], [])]).

-spec time_inline() -> aihtml:html().
time_inline() ->
    row([timepicker(<<"08:20">>, [inline], [{name, alarm}]),
         timepicker(<<"21:00">>, [inline], [{format, '24h'}, {auto_switch, false}])]).

-spec time_landscape() -> aihtml:html().
time_landscape() ->
    row([timepicker(<<"00:10">>, [inline, landscape], []),
         timepicker(<<"12:00">>, [inline],
                    [{format, '24h'},
                     {footer, span(<<"Times are local">>, [<<"text-xs text-muted">>], [])}])]).

-spec time_records() -> aihtml:html().
time_records() ->
    row([#ah_timepicker{value = <<"09:30">>, name = standup, format = '24h',
                        minute_step = 15, min = <<"08:00">>, max = <<"18:00">>,
                        clearable = true, css = [<<"w-48">>]},
         #ah_timepicker{value = {18, 45}, inline = true, view = landscape,
                        auto_switch = false}]).

-spec color_basic() -> aihtml:html().
color_basic() ->
    row([colorpicker(<<"#3b82f6">>, [], [{name, brand}]),
         colorpicker(<<"#F97316">>, [], [{name, accent}])]).

-spec color_swatches() -> aihtml:html().
color_swatches() ->
    colorpicker(<<"#22c55e">>, [],
                [{name, label_color},
                 {swatches, [<<"#ef4444">>, <<"#f97316">>, <<"#eab308">>, <<"#22c55e">>,
                             <<"#06b6d4">>, <<"#3b82f6">>, <<"#8b5cf6">>, <<"#ec4899">>]}]).

-spec color_alpha() -> aihtml:html().
color_alpha() ->
    row([colorpicker(<<"#22c55e80">>, [alpha], [{name, overlay}]),
         colorpicker({139, 92, 246, 200}, [inline, alpha], [])]).

-spec color_states() -> aihtml:html().
color_states() ->
    row([colorpicker(undefined, [clearable], [{placeholder, <<"Pick a colour">>}]),
         colorpicker(<<"#ec4899">>, [clearable], [{clear_label, <<"No colour">>}]),
         colorpicker(<<"#f97316">>, [disabled], [])]).

-spec color_inline() -> aihtml:html().
color_inline() ->
    colorpicker(<<"ff0000">>, [inline], [{name, highlight}]).

-spec color_compact() -> aihtml:html().
color_compact() ->
    row([colorpicker(<<"#0ea5e9">>, [inline, no_inputs], [{width, 200}, {height, 120}]),
         colorpicker(<<"#8b5cf6">>, [inline, no_preview], [{height, 100}])]).

row(Children) ->
    'div'(Children, [<<"flex flex-wrap items-start gap-4">>], []).
