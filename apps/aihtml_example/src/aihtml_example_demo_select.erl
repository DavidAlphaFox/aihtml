%% @doc Demos of the select component (aihtml_select), shown on
%% /components/select. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
-module(aihtml_example_demo_select).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0]).
-export([select_basic/0, select_sizes/0, select_groups/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => select, title => <<"Select">>,
       summary => <<"原生下拉框，外观与 DropDownList 一致。"/utf8>>,
       demos => [{<<"基本用法与占位项"/utf8>>, select_basic},
                 {<<"尺寸与颜色"/utf8>>, select_sizes},
                 {<<"分组、多选与禁用"/utf8>>, select_groups}]}].

-spec select_basic() -> aihtml:html().
select_basic() ->
    row([select(fruits(), grape, [], [{name, fruit}]),
         select(fruits(), undefined, [], [{name, other}, {placeholder, <<"Choose…"/utf8>>}])]).

-spec select_sizes() -> aihtml:html().
select_sizes() ->
    row([select(fruits(), apple, [sm], []),
         select(fruits(), apple, [], []),
         select(fruits(), apple, [lg], []),
         select(fruits(), peach, [primary], []),
         select(fruits(), lemon, [danger], [])]).

-spec select_groups() -> aihtml:html().
select_groups() ->
    row([select([{group, <<"Warm">>, [red, orange]}, {group, <<"Cool">>, [blue, green]}],
                blue, [], [{name, colour}]),
         select(fruits(), [apple, mango], [], [{multiple, true}, {size, 4}]),
         select(fruits(), lemon, [], [{disabled, true}])]).

row(Children) ->
    'div'(Children, [<<"flex flex-wrap items-start gap-4">>], []).

fruits() -> aihtml_example_fixture_select:fruits().
