%% @doc Demos of the dropdownlist component (aihtml_dropdownlist), shown on
%% /components/dropdownlist. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
-module(aihtml_example_demo_dropdownlist).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0]).
-export([dropdown_basic/0, dropdown_templates/0, dropdown_groups/0, dropdown_states/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => dropdownlist, title => <<"DropDownList">>,
       summary => <<"弹出列表的单选下拉，支持键盘、输入跳转、分组与过滤。"/utf8>>,
       demos => [{<<"基本用法"/utf8>>, dropdown_basic},
                 {<<"颜色模板"/utf8>>, dropdown_templates},
                 {<<"分组、禁用项与过滤"/utf8>>, dropdown_groups},
                 {<<"无箭头、禁用、占满宽度"/utf8>>, dropdown_states}]}].

-spec dropdown_basic() -> aihtml:html().
dropdown_basic() ->
    row([ah_dropdownlist(fruits(), banana, [<<"w-48">>], [{name, fruit}]),
         ah_dropdownlist(fruits(), undefined, [<<"w-48">>], [{placeholder, <<"Pick a fruit">>}])]).

-spec dropdown_templates() -> aihtml:html().
dropdown_templates() ->
    row([ah_dropdownlist(fruits(), apple, [primary, <<"w-40">>], []),
         ah_dropdownlist(fruits(), cherry, [success, <<"w-40">>], []),
         ah_dropdownlist(fruits(), lemon, [warning, <<"w-40">>], []),
         ah_dropdownlist(fruits(), grape, [danger, <<"w-40">>], [])]).

-spec dropdown_groups() -> aihtml:html().
dropdown_groups() ->
    Items = [{group, <<"Citrus">>, [{lemon, <<"Lemon">>}, {orange, <<"Orange">>}]},
             {group, <<"Berries">>, [{straw, <<"Strawberry">>},
                                     {blue, <<"Blueberry">>, #{disabled => true}},
                                     {rasp, <<"Raspberry">>}]}],
    row([ah_dropdownlist(Items, rasp, [<<"w-56">>], [{filterable, true}, {name, berry}]),
         ah_dropdownlist(Items, undefined, [<<"w-56">>], [{dropdown_height, 120}])]).

-spec dropdown_states() -> aihtml:html().
dropdown_states() ->
    ah_div([row([ah_dropdownlist(fruits(), cherry, [simple, <<"w-40">>], []),
                 ah_dropdownlist(fruits(), apple, [disabled, <<"w-40">>], [])]),
            ah_dropdownlist(fruits(), mango, [block], [])],
           [<<"flex flex-col gap-3">>], []).

row(Children) ->
    ah_div(Children, [<<"flex flex-wrap items-start gap-4">>], []).

fruits() -> aihtml_example_fixture_select:fruits().
