%% @doc Demos of the formatted input (aihtml_formatted_input), shown on
%% /components/formatted_input.
%% Each function is one example, written the way an application writes
%% it; the docs page prints its source under it.
-module(aihtml_example_demo_formatted_input).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0]).
-export([fmt_basic/0, fmt_limits/0, fmt_big/0, fmt_plain/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => formatted_input, title => <<"FormattedInput">>,
       summary => <<"二、八、十、十六进制的整数输入，带微调按钮和进制切换。"/utf8>>,
       demos => [{<<"十进制与十六进制"/utf8>>, fmt_basic},
                 {<<"范围、步长、大写"/utf8>>, fmt_limits},
                 {<<"大整数与指数表示"/utf8>>, fmt_big},
                 {<<"无按钮、禁用"/utf8>>, fmt_plain}]}].

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

row(Children) ->
    'div'(Children, [<<"flex flex-wrap items-center gap-4">>], []).
