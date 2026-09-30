%% @doc Demos of the time picker (aihtml_timepicker), shown on
%% /components/timepicker. Each function is one example, written the way
%% an application writes it; the docs page prints its source under it.
-module(aihtml_example_demo_timepicker).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0]).
-export([time_basic/0, time_24h/0, time_range/0, time_states/0,
         time_inline/0, time_landscape/0, time_records/0]).

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
                 {<<"record 写法"/utf8>>, time_records}]}].

-spec time_basic() -> aihtml:html().
time_basic() ->
    row([ah_timepicker(<<"14:30">>, [<<"w-48">>], [{name, start}]),
         ah_timepicker({9, 0}, [<<"w-48">>], [{name, finish}])]).

-spec time_24h() -> aihtml:html().
time_24h() ->
    row([ah_timepicker(<<"21:15">>, [<<"w-40">>], [{format, '24h'}]),
         ah_timepicker({9, 45}, [<<"w-40">>], [{format, '24h'}, {minute_step, 15},
                                              {name, meeting}])]).

-spec time_range() -> aihtml:html().
time_range() ->
    ah_timepicker(undefined, [<<"w-56">>],
                  [{placeholder, <<"Office hours only">>},
                   {min, <<"09:00">>}, {max, <<"17:30">>}, {name, visit}]).

-spec time_states() -> aihtml:html().
time_states() ->
    row([ah_timepicker(<<"08:20">>, [clearable, <<"w-48">>], []),
         ah_timepicker(<<"07:05">>, [disabled, <<"w-48">>], [])]).

-spec time_inline() -> aihtml:html().
time_inline() ->
    row([ah_timepicker(<<"08:20">>, [inline], [{name, alarm}]),
         ah_timepicker(<<"21:00">>, [inline], [{format, '24h'}, {auto_switch, false}])]).

-spec time_landscape() -> aihtml:html().
time_landscape() ->
    row([ah_timepicker(<<"00:10">>, [inline, landscape], []),
         ah_timepicker(<<"12:00">>, [inline],
                       [{format, '24h'},
                        {footer, ah_span(<<"Times are local">>, [<<"text-xs text-muted">>], [])}])]).

-spec time_records() -> aihtml:html().
time_records() ->
    row([#ah_timepicker{value = <<"09:30">>, name = standup, format = '24h',
                        minute_step = 15, min = <<"08:00">>, max = <<"18:00">>,
                        clearable = true, css = [<<"w-48">>]},
         #ah_timepicker{value = {18, 45}, inline = true, view = landscape,
                        auto_switch = false}]).

row(Children) ->
    ah_div(Children, [<<"flex flex-wrap items-start gap-4">>], []).
