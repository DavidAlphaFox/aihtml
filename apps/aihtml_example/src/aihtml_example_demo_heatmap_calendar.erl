%% @doc Demos of the contribution heatmap (aihtml_heatmap_calendar), shown
%% on /components/heatmap_calendar. Each function is one example, written
%% the way an application writes it; the docs page prints its source under
%% it.
%%
%% The module is also the action module of the demo that tells the server
%% the clicked day.
-module(aihtml_example_demo_heatmap_calendar).
-behaviour(aihtml_action).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0, action/4]).
-export([heatmap_basic/0, heatmap_custom/0, heatmap_select/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => heatmap_calendar, title => <<"HeatmapCalendar">>,
       summary => <<"GitHub 风格的贡献热力图，按日期铺格子、按数值深浅着色。"/utf8>>,
       demos => [{<<"最近一年"/utf8>>, heatmap_basic},
                 {<<"中文标签、阈值与提示"/utf8>>, heatmap_custom},
                 {<<"点击日期通知服务端"/utf8>>, heatmap_select}]}].

%%%===================================================================
%%% HeatmapCalendar
%%%===================================================================

-spec heatmap_basic() -> aihtml:html().
heatmap_basic() ->
    ah_heatmap_calendar(activity(), [], [{end_date, {2026, 9, 29}}]).

-spec heatmap_custom() -> aihtml:html().
heatmap_custom() ->
    ah_heatmap_calendar(activity(), [],
                        [{months, 6}, {end_date, {2026, 9, 29}}, {thresholds, [0, 2, 5, 8]},
                         {weekday_labels, [<<>>, <<"一"/utf8>>, <<>>, <<"三"/utf8>>, <<>>,
                                           <<"五"/utf8>>, <<>>]},
                         {month_labels, [<<(integer_to_binary(M))/binary, "月"/utf8>>
                                         || M <- lists:seq(1, 12)]},
                         {legend, {<<"少"/utf8>>, <<"多"/utf8>>}},
                         {tooltip, <<"{date}：{value} 次提交"/utf8>>}]).

-spec heatmap_select() -> aihtml:html().
heatmap_select() ->
    ah_div([#ah_heatmap_calendar{data = activity(), months = 4, end_date = {2026, 9, 29},
                                 postback = day_picked},
            ah_p(<<"点击任意一天"/utf8>>, [<<"text-sm text-muted mt-2">>], [{id, <<"day-picked">>}])],
           [], []).

%%%===================================================================
%%% Actions
%%%===================================================================

-spec action(atom(), term(), aihtml_action:event(), aihtml_action:ctx()) -> ok.
action(day_picked, _Args, #{value := Day}, Ctx) ->
    aihtml_action:html(Ctx, {id, <<"day-picked">>}, [<<"服务端收到："/utf8>>, Day]).

%%%===================================================================
%%% Data
%%%===================================================================

%% A year of made-up commit counts, the same on every render.
activity() ->
    Last = calendar:date_to_gregorian_days(2026, 9, 29),
    maps:from_list([{calendar:gregorian_days_to_date(D), N}
                    || D <- lists:seq(Last - 380, Last),
                       N <- [max(0, erlang:phash2(D, 14) - 5)], N > 0]).
