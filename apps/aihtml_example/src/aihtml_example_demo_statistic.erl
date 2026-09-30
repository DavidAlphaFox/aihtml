%% @doc Demos of the statistic component (aihtml_statistic), shown on
%% /components/statistic. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
-module(aihtml_example_demo_statistic).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0]).
-export([statistic_basic/0, statistic_delta/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => statistic, title => <<"Statistic">>,
       summary => <<"大号数字，带前后缀、千分位和涨跌。"/utf8>>,
       demos => [{<<"前后缀与精度"/utf8>>, statistic_basic},
                 {<<"涨跌与加载中"/utf8>>, statistic_delta}]}].

%%% Statistic

-spec statistic_basic() -> aihtml:html().
statistic_basic() ->
    row([ah_statistic(1284500, [primary], [{title, <<"Revenue">>}, {prefix, <<"¥"/utf8>>}]),
         ah_statistic(98.456, [success], [{title, <<"Uptime">>}, {suffix, <<"%">>}, {precision, 2}]),
         ah_statistic(1284500, [], [{title, <<"No separators">>}, {group_separator, false}])]).

-spec statistic_delta() -> aihtml:html().
statistic_delta() ->
    row([ah_statistic(3210, [], [{title, <<"Orders">>}, {delta, 128}]),
         ah_statistic(-3250.5, [error], [{title, <<"Balance">>}, {precision, 1}, {delta, -120}]),
         ah_statistic(42, [], [{title, <<"Tickets">>}, {delta, 0}]),
         ah_statistic(0, [loading], [{title, <<"Loading">>}])]).

%% Layout helpers of the demos.
row(Children) ->
    ah_div(Children, [<<"flex flex-wrap items-center gap-4">>], []).
