%% @doc Demos of the time_ago component (aihtml_time_ago), shown on
%% /components/time_ago. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
-module(aihtml_example_demo_time_ago).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0]).
-export([time_ago_units/0, time_ago_options/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => time_ago, title => <<"TimeAgo">>,
       summary => <<"「3m ago」式相对时间，浏览器每分钟刷新。"/utf8>>,
       demos => [{<<"各时间单位"/utf8>>, time_ago_units},
                 {<<"自定义文案、固定时间"/utf8>>, time_ago_options}]}].

%%% TimeAgo

-spec time_ago_units() -> aihtml:html().
time_ago_units() ->
    Now = erlang:system_time(second),
    row([ah_time_ago(Now - Ago, [], [])
         || Ago <- [5, 3 * 60, 2 * 3600, 3 * 86400, 90 * 86400]]).

-spec time_ago_options() -> aihtml:html().
time_ago_options() ->
    Now = erlang:system_time(second),
    Chinese = #{just_now => <<"刚刚"/utf8>>, minutes => <<"{n} 分钟前"/utf8>>,
                hours => <<"{n} 小时前"/utf8>>, days => <<"{n} 天前"/utf8>>,
                months => <<"{n} 个月前"/utf8>>},
    row([ah_time_ago(Now - 600, [], [{labels, Chinese}]),
         ah_time_ago({{2026, 1, 1}, {0, 0, 0}}, [<<"text-muted">>], [{live, false}]),
         ah_time_ago(<<"2026-07-22T08:00:00Z">>, [], [{title, false}])]).

%% Layout helpers of the demos.
row(Children) ->
    ah_div(Children, [<<"flex flex-wrap items-center gap-4">>], []).
