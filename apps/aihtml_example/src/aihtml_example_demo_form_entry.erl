%% @doc Demos of the entry components (aihtml_form_entry), shown on
%% /components/<name>. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
%%
%% The module is also the action module of the demos that talk to the
%% server: a masked input's change, repeat buttons stepping a number and
%% a range selector's change.
-module(aihtml_example_demo_form_entry).
-behaviour(aihtml_action).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0, action/4]).
-export([mask_phone/0, mask_formats/0, mask_label/0, mask_states/0, mask_change/0,
         fmt_basic/0, fmt_limits/0, fmt_big/0, fmt_plain/0,
         range_basic/0, range_dates/0, range_time/0, range_money/0, range_decimal/0,
         range_states/0, range_record/0,
         repeat_counter/0, repeat_variants/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => masked_input, title => <<"MaskedInput">>,
       summary => <<"带输入掩码的文本框，电话、日期、编码只能按格式输入。"/utf8>>,
       demos => [{<<"电话与邮编"/utf8>>, mask_phone},
                 {<<"日期、时间、自定义字符类"/utf8>>, mask_formats},
                 {<<"浮动标签、占位字符、值带字面量"/utf8>>, mask_label},
                 {<<"禁用、只读、直角"/utf8>>, mask_states},
                 {<<"失焦后通知服务端"/utf8>>, mask_change}]},
     #{component => formatted_input, title => <<"FormattedInput">>,
       summary => <<"二、八、十、十六进制的整数输入，带微调按钮和进制切换。"/utf8>>,
       demos => [{<<"十进制与十六进制"/utf8>>, fmt_basic},
                 {<<"范围、步长、大写"/utf8>>, fmt_limits},
                 {<<"大整数与指数表示"/utf8>>, fmt_big},
                 {<<"无按钮、禁用"/utf8>>, fmt_plain}]},
     #{component => range_selector, title => <<"RangeSelector">>,
       summary => <<"在带刻度的轨道上拖动两端或整段来选择一个区间。"/utf8>>,
       demos => [{<<"默认刻度"/utf8>>, range_basic},
                 {<<"日期：按月刻度"/utf8>>, range_dates},
                 {<<"时间：12 小时制"/utf8>>, range_time},
                 {<<"金额与最小跨度"/utf8>>, range_money},
                 {<<"小数与单位"/utf8>>, range_decimal},
                 {<<"负数、隐藏标记、禁用"/utf8>>, range_states},
                 {<<"record 写法"/utf8>>, range_record}]},
     #{component => repeat_button, title => <<"RepeatButton">>,
       summary => <<"按住不放时连续触发点击的按钮，适合步进调整数值。"/utf8>>,
       demos => [{<<"按住连续加减"/utf8>>, repeat_counter},
                 {<<"样式、尺寸、间隔"/utf8>>, repeat_variants}]}].

%%%===================================================================
%%% MaskedInput
%%%===================================================================

-spec mask_phone() -> aihtml:html().
mask_phone() ->
    row([masked_input(<<"5551234567">>, [<<"w-48">>],
                      [{mask, <<"(999) 999-9999">>}, {name, phone}]),
         masked_input(undefined, [<<"w-40">>], [{mask, <<"99999-9999">>}, {name, zip}])]).

-spec mask_formats() -> aihtml:html().
mask_formats() ->
    row([masked_input(<<"09/29/2026">>, [<<"w-36">>], [{mask, <<"99/99/9999">>}]),
         masked_input(undefined, [<<"w-24">>], [{mask, <<"[0-2][0-9]:[0-5][0-9]">>}]),
         masked_input(<<"ABC1234">>, [<<"w-36">>], [{mask, <<"LLL-9999">>}]),
         masked_input(undefined, [<<"w-56">>],
                      [{mask, <<"[0-9A-F][0-9A-F]:[0-9A-F][0-9A-F]:[0-9A-F][0-9A-F]:"
                                "[0-9A-F][0-9A-F]">>}])]).

-spec mask_label() -> aihtml:html().
mask_label() ->
    row([masked_input(undefined, [floating_label, <<"w-48">>],
                      [{mask, <<"(999) 999-9999">>}, {placeholder, <<"手机号"/utf8>>}]),
         masked_input(<<"4111111111111111">>, [<<"w-60">>],
                      [{mask, <<"9999 9999 9999 9999">>}, {prompt_char, <<"*">>},
                       {include_literals, true}, {name, card}])]).

-spec mask_states() -> aihtml:html().
mask_states() ->
    row([masked_input(<<"12345">>, [disabled, <<"w-32">>], []),
         masked_input(<<"12345">>, [readonly, <<"w-32">>], []),
         masked_input(undefined, [square, <<"w-32">>], [])]).

%% Leaving the field after an edit runs action(masked, ...) below.
-spec mask_change() -> aihtml:html().
mask_change() ->
    row([masked_input(undefined, [<<"w-48">>],
                      [{mask, <<"(999) 999-9999">>},
                       on(change, {?MODULE, masked, #{}})]),
         span(<<"输入后按 Tab 离开"/utf8>>, [<<"text-sm text-muted">>], [{id, <<"masked-out">>}])]).

%%%===================================================================
%%% FormattedInput
%%%===================================================================

-spec fmt_basic() -> aihtml:html().
fmt_basic() ->
    row([formatted_input(1234, [<<"w-44">>], [{name, count}]),
         formatted_input(255, [<<"w-44">>], [{radix, 16}])]).

-spec fmt_limits() -> aihtml:html().
fmt_limits() ->
    row([formatted_input(128, [<<"w-44">>],
                         [{min, 0}, {max, 255}, {spin_step, 16}, {radix, 16},
                          {upper_case, true}]),
         formatted_input(5, [<<"w-44">>], [{min, 0}, {max, 7}, {radix, 2}])]).

-spec fmt_big() -> aihtml:html().
fmt_big() ->
    row([formatted_input(1 bsl 64, [<<"w-72">>], [{radix, 16}]),
         formatted_input(<<"98765432109876543210">>, [<<"w-56">>],
                         [{notation, exponential}])]).

-spec fmt_plain() -> aihtml:html().
fmt_plain() ->
    row([formatted_input(42, [<<"w-40">>],
                         [{spin_buttons, false}, {drop_down, false},
                          {placeholder, <<"整数"/utf8>>}]),
         formatted_input(42, [disabled, <<"w-40">>], [])]).

%%%===================================================================
%%% RangeSelector
%%%===================================================================

-spec range_basic() -> aihtml:html().
range_basic() ->
    range_selector({0, 200}, {10, 50}, [],
                   [{major_ticks, 20}, {minor_ticks, 5}, {show_minor_ticks, true},
                    {name, range}]).

-spec range_dates() -> aihtml:html().
range_dates() ->
    Day = 86400000,
    Months = [ms({2024, M, 1}) || M <- lists:seq(1, 12)],
    range_selector({ms({2024, 1, 1}), ms({2024, 12, 31}), Day},
                   {ms({2024, 3, 1}), ms({2024, 9, 1})}, [],
                   [{tick_values, Months}, {labels_format, month},
                    {markers_format, date}]).

-spec range_time() -> aihtml:html().
range_time() ->
    Hour = 3600000,
    range_selector({0, 24 * Hour, Hour div 2}, {8 * Hour, 18 * Hour}, [],
                   [{major_ticks, 4 * Hour}, {minor_ticks, Hour},
                    {show_minor_ticks, true}, {labels_format, time}]).

-spec range_money() -> aihtml:html().
range_money() ->
    range_selector({1000, 10000, 100}, {2500, 7500}, [],
                   [{major_ticks, 1500}, {labels_format, currency},
                    {min_span, 1000}, {name, budget}]).

-spec range_decimal() -> aihtml:html().
range_decimal() ->
    range_selector({0, 10, 0.1}, {2.5, 7.5}, [],
                   [{major_ticks, 2.5}, {minor_ticks, 0.5}, {show_minor_ticks, true},
                    {labels_format, {fixed, 1}},
                    {markers_format, {<<>>, {fixed, 1}, <<" mm">>}}]).

-spec range_states() -> aihtml:html().
range_states() ->
    'div'([range_selector({-1000, -100, 10}, {-800, -300}, [],
                          [{major_ticks, 100}, {show_markers, false}]),
           range_selector({0, 100}, {20, 60}, [disabled], [{major_ticks, 25}])],
          [<<"flex flex-col gap-2">>], []).

%% The same component as a record: options are checked field names, and
%% the postback runs action(range_picked, ...) below on change.
-spec range_record() -> aihtml:html().
range_record() ->
    'div'([#ah_range_selector{range = {0, 100, 5}, value = {20, 80}, name = score,
                              major_ticks = 20, minor_ticks = 5, show_minor_ticks = true,
                              markers_format = {<<>>, number, <<" 分"/utf8>>},
                              min_span = 10, postback = range_picked},
           span(<<"拖动后显示服务端收到的值"/utf8>>, [<<"text-sm text-muted">>],
                [{id, <<"range-picked">>}])],
          [<<"flex flex-col gap-2">>], []).

%%%===================================================================
%%% RepeatButton
%%%===================================================================

%% Each click (repeated while held) runs action(step, ...) below with the
%% number's current value, and the server sets the new one.
-spec repeat_counter() -> aihtml:html().
repeat_counter() ->
    Step = fun(Label, By) ->
                   repeat_button(Label, By, [outlined],
                                 [{interval, 80},
                                  on(click, {?MODULE, step, #{by => By}},
                                     #{include => [<<"input[name=qty]">>]})])
           end,
    row([Step(<<"−"/utf8>>, -1),
         formatted_input(10, [<<"w-32">>],
                         [{id, qty}, {name, qty}, {min, 0}, {max, 999},
                          {spin_buttons, false}, {drop_down, false}]),
         Step(<<"+">>, 1)]).

-spec repeat_variants() -> aihtml:html().
repeat_variants() ->
    row([repeat_button(<<"Primary">>, undefined, [], []),
         repeat_button(<<"Slow">>, undefined, [secondary, sm], [{delay, 600}, {interval, 250}]),
         repeat_button(<<"Round">>, undefined, [success, round], []),
         repeat_button(<<"Next">>, undefined, [outlined, lg],
                       [{icon, <<"▶"/utf8>>}, {icon_position, right}]),
         repeat_button(<<"Disabled">>, undefined, [], [{disabled, true}])]).

%%%===================================================================
%%% Actions
%%%===================================================================

-spec action(atom(), term(), aihtml_action:event(), aihtml_action:ctx()) -> ok.
action(masked, _Args, #{value := Value}, Ctx) ->
    aihtml_action:html(Ctx, {id, <<"masked-out">>}, [<<"服务端收到："/utf8>>, Value]);
action(step, #{by := By}, #{values := Values}, Ctx) ->
    Old = binary_to_integer(maps:get(<<"qty">>, Values, <<"0">>)),
    aihtml_action:call(Ctx, {id, qty}, setValue, [max(0, min(999, Old + By))]);
action(range_picked, _Args, #{value := Value}, Ctx) ->
    aihtml_action:html(Ctx, {id, <<"range-picked">>}, [<<"服务端收到："/utf8>>, Value]).

%%%===================================================================
%%% Helpers
%%%===================================================================

%% Milliseconds since 1970 (UTC) of a date.
ms(Date) ->
    (calendar:datetime_to_gregorian_seconds({Date, {0, 0, 0}}) - 62167219200) * 1000.

row(Children) ->
    'div'(Children, [<<"flex flex-wrap items-center gap-4">>], []).
