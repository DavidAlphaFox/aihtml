%% @doc Demos of the kpi_card component (aihtml_kpi_card), shown on
%% /components/kpi_card. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
-module(aihtml_example_demo_kpi_card).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0]).
-export([kpi_trends/0, kpi_colors/0, kpi_record/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => kpi_card, title => <<"KpiCard">>,
       summary => <<"指标卡：一个数字加环比趋势。"/utf8>>,
       demos => [{<<"趋势与图标"/utf8>>, kpi_trends},
                 {<<"颜色与禁用"/utf8>>, kpi_colors},
                 {<<"record 写法"/utf8>>, kpi_record}]}].

%%% KpiCard

-spec kpi_trends() -> aihtml:html().
kpi_trends() ->
    grid([kpi_card(<<"12,480">>, [], [{title, <<"Active users">>}, {trend, 5.2},
                                      {trend_label, <<"vs last month">>}, {icon, users}]),
          kpi_card(<<"845">>, [warning], [{title, <<"Installs">>}, {trend, -3.5},
                                          {trend_label, <<"vs last week">>}, {icon, install}]),
          kpi_card(<<"4.8">>, [], [{title, <<"Rating">>}, {icon, star}])]).

-spec kpi_colors() -> aihtml:html().
kpi_colors() ->
    grid([kpi_card(<<"3,210">>, [success], [{title, <<"Downloads">>}, {trend, 12}, {icon, download}]),
          kpi_card(<<"96%">>, [info], [{title, <<"SLA">>}, {trend, 0.4}]),
          kpi_card(<<"7">>, [error], [{title, <<"Incidents">>}, {trend, -30}]),
          kpi_card(<<"—"/utf8>>, [disabled], [{title, <<"Disabled">>}])]).

-spec kpi_record() -> aihtml:html().
kpi_record() ->
    grid([#ah_kpi_card{value = <<"1,024">>, color = success, icon = users,
                       title = <<"Signups">>, trend = 8.5, trend_label = <<"vs last week">>},
          #ah_kpi_card{value = <<"37">>, color = error, css = [<<"shadow-sm">>],
                       title = <<"Churned">>, trend = -2.1}]).

%% Layout helpers of the demos.
grid(Children) ->
    'div'(Children, [<<"grid grid-cols-1 sm:grid-cols-3 gap-4">>], []).
