%% @doc Demos of the number input (aihtml_number_input), shown on
%% /components/number_input. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
-module(aihtml_example_demo_number_input).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0]).
-export([number_basic/0, number_symbols/0, number_states/0, number_record/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => number_input, title => <<"NumberInput">>,
       summary => <<"数值输入框，带微调按钮，支持方向键、滚轮和范围限制。"/utf8>>,
       demos => [{<<"范围与步长"/utf8>>, number_basic},
                 {<<"前缀、后缀与无按钮"/utf8>>, number_symbols},
                 {<<"尺寸与状态"/utf8>>, number_states},
                 {<<"record 写法"/utf8>>, number_record}]}].

-spec number_basic() -> aihtml:html().
number_basic() ->
    row([number_input(5, [<<"w-40">>], [{min, 0}, {max, 10}, {name, qty}]),
         number_input(<<"2.5">>, [<<"w-40">>], [{step, 0.5}, {min, 0}]),
         number_input(undefined, [<<"w-40">>], [{label, <<"Amount">>}])]).

-spec number_symbols() -> aihtml:html().
number_symbols() ->
    row([number_input(19.5, [<<"w-44">>], [{step, 0.5}, {symbol, <<"$">>}]),
         number_input(75, [<<"w-40">>], [{min, 0}, {max, 100}, {symbol, <<"%">>},
                                         {symbol_position, right}]),
         number_input(undefined, [<<"w-40">>], [{spin, false},
                                                {placeholder, <<"No spin">>}])]).

-spec number_states() -> aihtml:html().
number_states() ->
    row([number_input(1, [sm, <<"w-32">>], []),
         number_input(2, [lg, <<"w-40">>], []),
         number_input(200, [invalid, <<"w-40">>], [{max, 100}]),
         number_input(3, [readonly, <<"w-32">>], []),
         number_input(4, [disabled, <<"w-32">>], [])]).

-spec number_record() -> aihtml:html().
number_record() ->
    #ah_number_input{value = 12.5, min = 0, max = 100, step = 0.5,
                     symbol = <<"%">>, symbol_position = right,
                     label = <<"Discount">>, css = [<<"w-44">>],
                     attrs = [{name, discount}]}.

row(Children) ->
    'div'(Children, [<<"flex flex-wrap items-start gap-4">>], []).
