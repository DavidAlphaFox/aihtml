%% @doc Demos of the range selector (aihtml_range_selector), shown on
%% /components/range_selector.
%% Each function is one example, written the way an application writes
%% it; the docs page prints its source under it.
%%
%% The module is also the action module of range_record (a range
%% selector's change).
-module(aihtml_example_demo_range_selector).
-behaviour(aihtml_action).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0, action/4]).
-export([range_basic/0, range_dates/0, range_time/0, range_money/0, range_decimal/0,
         range_states/0, range_record/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => range_selector, title => <<"RangeSelector">>,
       summary => <<"在带刻度的轨道上拖动两端或整段来选择一个区间。"/utf8>>,
       demos => [{<<"默认刻度"/utf8>>, range_basic},
                 {<<"日期：按月刻度"/utf8>>, range_dates},
                 {<<"时间：12 小时制"/utf8>>, range_time},
                 {<<"金额与最小跨度"/utf8>>, range_money},
                 {<<"小数与单位"/utf8>>, range_decimal},
                 {<<"负数、隐藏标记、禁用"/utf8>>, range_states},
                 {<<"record 写法"/utf8>>, range_record}]}].

-spec range_basic() -> aihtml:html().
range_basic() ->
    ah_range_selector({0, 200}, {10, 50}, [],
                      [{major_ticks, 20}, {minor_ticks, 5}, {show_minor_ticks, true},
                       {name, range}]).

-spec range_dates() -> aihtml:html().
range_dates() ->
    Day = 86400000,
    Months = [ms({2024, M, 1}) || M <- lists:seq(1, 12)],
    ah_range_selector({ms({2024, 1, 1}), ms({2024, 12, 31}), Day},
                      {ms({2024, 3, 1}), ms({2024, 9, 1})}, [],
                      [{tick_values, Months}, {labels_format, month},
                       {markers_format, date}]).

-spec range_time() -> aihtml:html().
range_time() ->
    Hour = 3600000,
    ah_range_selector({0, 24 * Hour, Hour div 2}, {8 * Hour, 18 * Hour}, [],
                      [{major_ticks, 4 * Hour}, {minor_ticks, Hour},
                       {show_minor_ticks, true}, {labels_format, time}]).

-spec range_money() -> aihtml:html().
range_money() ->
    ah_range_selector({1000, 10000, 100}, {2500, 7500}, [],
                      [{major_ticks, 1500}, {labels_format, currency},
                       {min_span, 1000}, {name, budget}]).

-spec range_decimal() -> aihtml:html().
range_decimal() ->
    ah_range_selector({0, 10, 0.1}, {2.5, 7.5}, [],
                      [{major_ticks, 2.5}, {minor_ticks, 0.5}, {show_minor_ticks, true},
                       {labels_format, {fixed, 1}},
                       {markers_format, {<<>>, {fixed, 1}, <<" mm">>}}]).

-spec range_states() -> aihtml:html().
range_states() ->
    ah_div([ah_range_selector({-1000, -100, 10}, {-800, -300}, [],
                              [{major_ticks, 100}, {show_markers, false}]),
            ah_range_selector({0, 100}, {20, 60}, [disabled], [{major_ticks, 25}])],
           [<<"flex flex-col gap-2">>], []).

%% The same component as a record: options are checked field names, and
%% the postback runs action(range_picked, ...) below on change.
-spec range_record() -> aihtml:html().
range_record() ->
    ah_div([#ah_range_selector{range = {0, 100, 5}, value = {20, 80}, name = score,
                               major_ticks = 20, minor_ticks = 5, show_minor_ticks = true,
                               markers_format = {<<>>, number, <<" 分"/utf8>>},
                               min_span = 10, postback = range_picked},
            ah_span(<<"拖动后显示服务端收到的值"/utf8>>, [<<"text-sm text-muted">>],
                    [{id, <<"range-picked">>}])],
           [<<"flex flex-col gap-2">>], []).

-spec action(atom(), term(), aihtml_action:event(), aihtml_action:ctx()) -> ok.
action(range_picked, _Args, #{value := Value}, Ctx) ->
    aihtml_action:html(Ctx, {id, <<"range-picked">>}, [<<"服务端收到："/utf8>>, Value]).

%% Milliseconds since 1970 (UTC) of a date.
ms(Date) ->
    (calendar:datetime_to_gregorian_seconds({Date, {0, 0, 0}}) - 62167219200) * 1000.
