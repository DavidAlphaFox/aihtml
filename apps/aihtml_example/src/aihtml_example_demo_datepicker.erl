%% @doc Demos of the date picker (aihtml_datepicker), shown on
%% /components/datepicker. Each function is one example, written the way
%% an application writes it; the docs page prints its source under it.
%%
%% The module is also the action module of the change event demos.
-module(aihtml_example_demo_datepicker).
-behaviour(aihtml_action).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0, action/4]).
-export([date_basic/0, date_range/0, date_limits/0, date_locale/0, date_inline/0,
         date_states/0, date_change/0, date_record/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => datepicker, title => <<"DatePicker">>,
       summary => <<"带月历弹层的日期输入，支持范围、限制日期和完整的键盘操作。"/utf8>>,
       demos => [{<<"单个日期与空值"/utf8>>, date_basic},
                 {<<"日期范围"/utf8>>, date_range},
                 {<<"可选范围、禁用日期、周数"/utf8>>, date_limits},
                 {<<"周一开头、中文标签、显示格式"/utf8>>, date_locale},
                 {<<"内嵌月历"/utf8>>, date_inline},
                 {<<"禁用与只读"/utf8>>, date_states},
                 {<<"选中后通知服务端"/utf8>>, date_change},
                 {<<"record 写法"/utf8>>, date_record}]}].

%%%===================================================================
%%% DatePicker
%%%===================================================================

-spec date_basic() -> aihtml:html().
date_basic() ->
    row([ah_datepicker(<<"2026-09-29">>, [], [{name, due}]),
         ah_datepicker(undefined, [clearable], [{placeholder, <<"选择日期"/utf8>>}])]).

-spec date_range() -> aihtml:html().
date_range() ->
    row([ah_datepicker({<<"2026-09-10">>, <<"2026-09-18">>}, [clearable, <<"w-64">>],
                       [{name, period}]),
         ah_datepicker(undefined, [range, <<"w-64">>], [{placeholder, <<"开始 - 结束"/utf8>>}])]).

-spec date_limits() -> aihtml:html().
date_limits() ->
    ah_datepicker({2026, 9, 15}, [],
                  [{min, <<"2026-09-05">>}, {max, <<"2026-10-20">>},
                   {disabled_dates, [<<"2026-09-21">>, <<"2026-09-22">>]},
                   {week_numbers, true}, {format, <<"d MMM yyyy">>}]).

-spec date_locale() -> aihtml:html().
date_locale() ->
    Months = [<<"一月"/utf8>>, <<"二月"/utf8>>, <<"三月"/utf8>>, <<"四月"/utf8>>,
              <<"五月"/utf8>>, <<"六月"/utf8>>, <<"七月"/utf8>>, <<"八月"/utf8>>,
              <<"九月"/utf8>>, <<"十月"/utf8>>, <<"十一月"/utf8>>, <<"十二月"/utf8>>],
    ah_datepicker(<<"2026-09-29">>, [],
                  [{first_day, 1}, {format, <<"yyyy/MM/dd">>}, {other_month_days, false},
                   {weekends, true},
                   {labels, #{months => Months, title => <<"yyyy年 MMMM"/utf8>>,
                              weekdays => [<<"日"/utf8>>, <<"一"/utf8>>, <<"二"/utf8>>,
                                           <<"三"/utf8>>, <<"四"/utf8>>, <<"五"/utf8>>,
                                           <<"六"/utf8>>],
                              today => <<"今天"/utf8>>, clear => <<"清除"/utf8>>}}]).

-spec date_inline() -> aihtml:html().
date_inline() ->
    row([ah_datepicker(<<"2026-09-29">>, [inline], [{first_day, 1}]),
         ah_datepicker({<<"2026-09-08">>, <<"2026-09-12">>}, [inline], [{week_numbers, true}])]).

-spec date_states() -> aihtml:html().
date_states() ->
    row([ah_datepicker(<<"2026-01-01">>, [disabled], []),
         ah_datepicker(<<"2026-01-01">>, [readonly], [])]).

-spec date_change() -> aihtml:html().
date_change() ->
    row([ah_datepicker(undefined, [], [on(change, {?MODULE, date_picked, #{}})]),
         ah_span(<<"还没有选择"/utf8>>, [<<"text-sm text-muted">>], [{id, <<"date-picked">>}])]).

%% The same component as a record: options are checked field names, and
%% the postback runs action(date_picked, ...) below on change.
-spec date_record() -> aihtml:html().
date_record() ->
    row([#ah_datepicker{value = <<"2026-09-29">>, name = start, clearable = true,
                        min = <<"2026-09-01">>, max = <<"2026-12-31">>, first_day = 1,
                        format = <<"d MMM yyyy">>, week_numbers = true,
                        postback = date_picked},
         ah_span(<<"还没有选择"/utf8>>, [<<"text-sm text-muted">>], [{id, <<"date-picked">>}])]).

%%%===================================================================
%%% Actions
%%%===================================================================

-spec action(atom(), term(), aihtml_action:event(), aihtml_action:ctx()) -> ok.
action(date_picked, _Args, #{value := Value}, Ctx) ->
    aihtml_action:html(Ctx, {id, <<"date-picked">>}, [<<"服务端收到："/utf8>>, Value]).

row(Children) ->
    ah_div(Children, [<<"flex flex-wrap items-start gap-4">>], []).
