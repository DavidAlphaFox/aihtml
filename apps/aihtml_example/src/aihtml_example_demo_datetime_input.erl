%% @doc Demos of the segmented date/time field (aihtml_datetime_input),
%% shown on /components/datetime_input. Each function is one example,
%% written the way an application writes it; the docs page prints its
%% source under it.
%%
%% The module is also the action module of the change event demo.
-module(aihtml_example_demo_datetime_input).
-behaviour(aihtml_action).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0, action/4]).
-export([dti_date/0, dti_datetime/0, dti_time/0, dti_limits/0, dti_states/0,
         dti_change/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => datetime_input, title => <<"DateTimeInput">>,
       summary => <<"分段编辑的日期时间输入框，逐段用数字和方向键修改，可弹出月历。"/utf8>>,
       demos => [{<<"日期"/utf8>>, dti_date},
                 {<<"日期加时间，下拉里调时分"/utf8>>, dti_datetime},
                 {<<"只有时间：12 小时制与微调按钮"/utf8>>, dti_time},
                 {<<"可选范围、周一开头、中文标签"/utf8>>, dti_limits},
                 {<<"浮动标签、直角、禁用与只读"/utf8>>, dti_states},
                 {<<"改完后通知服务端"/utf8>>, dti_change}]}].

%%%===================================================================
%%% DateTimeInput
%%%===================================================================

-spec dti_date() -> aihtml:html().
dti_date() ->
    row([ah_datetime_input(<<"2026-09-29">>, [], [{name, due}]),
         ah_datetime_input(undefined, [], [{placeholder, <<"yyyy-MM-dd">>}]),
         ah_datetime_input({2026, 9, 29}, [], [{format, <<"dd/MM/yyyy">>}])]).

-spec dti_datetime() -> aihtml:html().
dti_datetime() ->
    row([ah_datetime_input(<<"2026-09-29T14:30">>, [show_time, <<"w-56">>],
                           [{format, <<"yyyy-MM-dd HH:mm">>}, {name, starts_at}]),
         ah_datetime_input({{2026, 9, 29}, {9, 5, 30}}, [<<"w-64">>],
                           [{format, <<"yyyy-MM-dd HH:mm:ss">>}])]).

-spec dti_time() -> aihtml:html().
dti_time() ->
    row([ah_datetime_input(<<"09:30">>, [spinner, <<"w-36">>], [{format, <<"hh:mm a">>}]),
         ah_datetime_input(<<"18:45">>, [spinner, <<"w-32">>], [{format, <<"HH:mm">>}])]).

-spec dti_limits() -> aihtml:html().
dti_limits() ->
    ah_datetime_input(<<"2026-09-29">>, [],
                      [{format, <<"yyyy年MM月dd日"/utf8>>}, {first_day, 1},
                       {min, <<"2026-09-10">>}, {max, <<"2026-10-20">>},
                       {labels, #{title => <<"yyyy年 M月"/utf8>>,
                                  months => [<<"一月"/utf8>>, <<"二月"/utf8>>, <<"三月"/utf8>>,
                                             <<"四月"/utf8>>, <<"五月"/utf8>>, <<"六月"/utf8>>,
                                             <<"七月"/utf8>>, <<"八月"/utf8>>, <<"九月"/utf8>>,
                                             <<"十月"/utf8>>, <<"十一月"/utf8>>, <<"十二月"/utf8>>],
                                  weekdays => [<<"日"/utf8>>, <<"一"/utf8>>, <<"二"/utf8>>,
                                               <<"三"/utf8>>, <<"四"/utf8>>, <<"五"/utf8>>,
                                               <<"六"/utf8>>],
                                  time => <<"时间"/utf8>>}}]).

-spec dti_states() -> aihtml:html().
dti_states() ->
    row([ah_datetime_input(undefined, [floating_label, <<"mt-2">>],
                           [{placeholder, <<"出生日期"/utf8>>}]),
         ah_datetime_input(<<"2026-09-29">>, [no_rounded], []),
         ah_datetime_input(<<"2026-09-29">>, [disabled], []),
         ah_datetime_input(<<"2026-09-29">>, [readonly, spinner], [])]).

-spec dti_change() -> aihtml:html().
dti_change() ->
    row([ah_datetime_input(undefined, [show_time],
                           [{format, <<"yyyy-MM-dd HH:mm">>}, {placeholder, <<"开始时间"/utf8>>},
                            on(change, {?MODULE, dti_changed, #{}})]),
         ah_span(<<"还没有修改"/utf8>>, [<<"text-sm text-muted">>], [{id, <<"dti-changed">>}])]).

%%%===================================================================
%%% Actions
%%%===================================================================

-spec action(atom(), term(), aihtml_action:event(), aihtml_action:ctx()) -> ok.
action(dti_changed, _Args, #{value := Value}, Ctx) ->
    aihtml_action:html(Ctx, {id, <<"dti-changed">>}, [<<"服务端收到："/utf8>>, Value]).

row(Children) ->
    ah_div(Children, [<<"flex flex-wrap items-start gap-4">>], []).
