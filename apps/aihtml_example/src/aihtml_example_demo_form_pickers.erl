%% @doc Demos of the picker components (aihtml_form_pickers), shown on
%% /components/<name>. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
%%
%% The module is also the action module of two demos: the server-side
%% search of a combobox and a datepicker's change event.
-module(aihtml_example_demo_form_pickers).
-behaviour(aihtml_action).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0, action/4]).
-export([date_basic/0, date_range/0, date_limits/0, date_locale/0, date_inline/0,
         date_states/0, date_change/0,
         combo_basic/0, combo_free_text/0, combo_groups/0, combo_multiple/0,
         combo_search/0, combo_disabled/0]).

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
                 {<<"选中后通知服务端"/utf8>>, date_change}]},
     #{component => combobox, title => <<"ComboBox">>,
       summary => <<"可输入过滤的下拉选择，支持分组、多选、勾选框和服务端搜索。"/utf8>>,
       demos => [{<<"输入过滤，只能选列表里的项"/utf8>>, combo_basic},
                 {<<"自由输入、前缀匹配、无箭头"/utf8>>, combo_free_text},
                 {<<"分组、描述、禁用项"/utf8>>, combo_groups},
                 {<<"多选标签与勾选框"/utf8>>, combo_multiple},
                 {<<"服务端搜索"/utf8>>, combo_search},
                 {<<"禁用"/utf8>>, combo_disabled}]}].

%%%===================================================================
%%% DatePicker
%%%===================================================================

-spec date_basic() -> aihtml:html().
date_basic() ->
    row([datepicker(<<"2026-09-29">>, [], [{name, due}]),
         datepicker(undefined, [clearable], [{placeholder, <<"选择日期"/utf8>>}])]).

-spec date_range() -> aihtml:html().
date_range() ->
    row([datepicker({<<"2026-09-10">>, <<"2026-09-18">>}, [clearable, <<"w-64">>],
                    [{name, period}]),
         datepicker(undefined, [range, <<"w-64">>], [{placeholder, <<"开始 - 结束"/utf8>>}])]).

-spec date_limits() -> aihtml:html().
date_limits() ->
    datepicker({2026, 9, 15}, [],
               [{min, <<"2026-09-05">>}, {max, <<"2026-10-20">>},
                {disabled_dates, [<<"2026-09-21">>, <<"2026-09-22">>]},
                {week_numbers, true}, {format, <<"d MMM yyyy">>}]).

-spec date_locale() -> aihtml:html().
date_locale() ->
    Months = [<<"一月"/utf8>>, <<"二月"/utf8>>, <<"三月"/utf8>>, <<"四月"/utf8>>,
              <<"五月"/utf8>>, <<"六月"/utf8>>, <<"七月"/utf8>>, <<"八月"/utf8>>,
              <<"九月"/utf8>>, <<"十月"/utf8>>, <<"十一月"/utf8>>, <<"十二月"/utf8>>],
    datepicker(<<"2026-09-29">>, [],
               [{first_day, 1}, {format, <<"yyyy/MM/dd">>}, {other_month_days, false},
                {weekends, true},
                {labels, #{months => Months, title => <<"yyyy年 MMMM"/utf8>>,
                           weekdays => [<<"日"/utf8>>, <<"一"/utf8>>, <<"二"/utf8>>,
                                        <<"三"/utf8>>, <<"四"/utf8>>, <<"五"/utf8>>,
                                        <<"六"/utf8>>],
                           today => <<"今天"/utf8>>, clear => <<"清除"/utf8>>}}]).

-spec date_inline() -> aihtml:html().
date_inline() ->
    row([datepicker(<<"2026-09-29">>, [inline], [{first_day, 1}]),
         datepicker({<<"2026-09-08">>, <<"2026-09-12">>}, [inline], [{week_numbers, true}])]).

-spec date_states() -> aihtml:html().
date_states() ->
    row([datepicker(<<"2026-01-01">>, [disabled], []),
         datepicker(<<"2026-01-01">>, [readonly], [])]).

-spec date_change() -> aihtml:html().
date_change() ->
    row([datepicker(undefined, [], [on(change, {?MODULE, date_picked, #{}})]),
         span(<<"还没有选择"/utf8>>, [<<"text-sm text-muted">>], [{id, <<"date-picked">>}])]).

%%%===================================================================
%%% ComboBox
%%%===================================================================

-spec combo_basic() -> aihtml:html().
combo_basic() ->
    combobox(fruits(), <<"Cherry">>, [<<"w-56">>],
             [{name, fruit}, {placeholder, <<"选一种水果"/utf8>>}]).

-spec combo_free_text() -> aihtml:html().
combo_free_text() ->
    row([combobox(fruits(), undefined, [free_text, <<"w-56">>],
                  [{placeholder, <<"任意水果"/utf8>>}, {search_mode, starts_with_ignore_case}]),
         combobox(fruits(), undefined, [no_arrow, <<"w-56">>],
                  [{placeholder, <<"没有箭头"/utf8>>}])]).

-spec combo_groups() -> aihtml:html().
combo_groups() ->
    People = [#{value => 1, label => <<"Ada Lovelace">>, description => <<"Analyst">>,
                group => <<"Engineering">>},
              #{value => 2, label => <<"Alan Turing">>, description => <<"Cryptography">>,
                group => <<"Engineering">>},
              #{value => 3, label => <<"Grace Hopper">>, description => <<"Compilers">>,
                group => <<"Engineering">>},
              #{value => 4, label => <<"Joan Clarke">>, description => <<"Cryptanalysis">>,
                group => <<"Research">>},
              #{value => 5, label => <<"Katherine Johnson">>,
                description => <<"Orbital mechanics">>, group => <<"Research">>,
                disabled => true}],
    combobox(People, 3, [<<"w-72">>], [{name, person}]).

-spec combo_multiple() -> aihtml:html().
combo_multiple() ->
    row([combobox(fruits(), [<<"Apple">>, <<"Mango">>], [multiple, <<"w-72">>],
                  [{name, fruits}, {placeholder, <<"水果"/utf8>>}]),
         combobox([<<"Reading">>, <<"Music">>, <<"Sports">>, <<"Travel">>, <<"Coding">>],
                  [<<"Music">>], [checkboxes, <<"w-72">>], [{placeholder, <<"爱好"/utf8>>}])]).

%% Typing calls action(search, ...) below, which answers with set_items.
-spec combo_search() -> aihtml:html().
combo_search() ->
    combobox([], undefined, [<<"w-72">>],
             [{name, city}, {placeholder, <<"输入城市名，如 an"/utf8>>},
              {search, {?MODULE, search, #{}}}]).

-spec combo_disabled() -> aihtml:html().
combo_disabled() ->
    combobox(fruits(), <<"Apple">>, [disabled, <<"w-56">>], []).

%%%===================================================================
%%% Actions
%%%===================================================================

-spec action(atom(), term(), aihtml_action:event(), aihtml_action:ctx()) -> ok.
action(search, _Args, #{value := Query} = Event, Ctx) ->
    Q = string:lowercase(Query),
    Found = [C || C <- cities(), string:find(string:lowercase(C), Q) =/= nomatch],
    set_items(Ctx, Event, lists:sublist(Found, 8));
action(date_picked, _Args, #{value := Value}, Ctx) ->
    aihtml_action:html(Ctx, {id, <<"date-picked">>}, [<<"服务端收到："/utf8>>, Value]).

%%%===================================================================
%%% Data
%%%===================================================================

fruits() ->
    [<<"Apple">>, <<"Apricot">>, <<"Banana">>, <<"Blueberry">>, <<"Cherry">>,
     <<"Grape">>, <<"Lemon">>, <<"Mango">>, <<"Orange">>, <<"Peach">>].

cities() ->
    [<<"Amsterdam">>, <<"Athens">>, <<"Bangkok">>, <<"Beijing">>, <<"Berlin">>,
     <<"Dalian">>, <<"Istanbul">>, <<"Jakarta">>, <<"London">>, <<"Madrid">>,
     <<"Milan">>, <<"Nanjing">>, <<"Oslo">>, <<"Paris">>, <<"Santiago">>,
     <<"Seoul">>, <<"Shanghai">>, <<"Stockholm">>, <<"Tokyo">>, <<"Vienna">>].

row(Children) ->
    'div'(Children, [<<"flex flex-wrap items-start gap-4">>], []).
