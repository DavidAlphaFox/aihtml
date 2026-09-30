%% @doc Demos of the scrollbar component (aihtml_scrollbar), shown on
%% /components/scrollbar. Each function is one example, written the way
%% an application writes it; the docs page prints its source under it.
%%
%% The module is also the action module of the demo that sends the
%% scrollbar's value to the server.
-module(aihtml_example_demo_scrollbar).
-behaviour(aihtml_action).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0, action/4]).
-export([sb_horizontal/0, sb_vertical/0, sb_area/0, sb_area_both/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => scrollbar, title => <<"Scrollbar">>,
       summary => <<"自定义滚动条：可独立取值，也可包住内容作为滚动区域。"/utf8>>,
       demos => [{<<"水平滚动条，拖动后通知服务端"/utf8>>, sb_horizontal},
                 {<<"垂直、无按钮、禁用"/utf8>>, sb_vertical},
                 {<<"包住内容的滚动区域"/utf8>>, sb_area},
                 {<<"横竖两个方向"/utf8>>, sb_area_both}]}].

%%%===================================================================
%%% Demos
%%%===================================================================

%% Releasing the thumb (or a click) runs action(scrolled, ...).
-spec sb_horizontal() -> aihtml:html().
sb_horizontal() ->
    ah_div([ah_scrollbar([], [<<"max-w-sm">>],
                         [{value, 250}, {max, 1000}, {name, offset},
                          {label, <<"偏移量"/utf8>>}, on(change, {?MODULE, scrolled, #{}})]),
            ah_span(<<"值：250"/utf8>>, [<<"text-sm text-muted">>], [{id, <<"sb-value">>}])],
           [<<"flex items-center gap-4">>], []).

-spec sb_vertical() -> aihtml:html().
sb_vertical() ->
    ah_div([ah_scrollbar([], [vertical], [{height, 220}, {value, 400}]),
            ah_scrollbar([], [vertical], [{height, 220}, {max, 100}, {value, 30},
                                          {show_buttons, false}]),
            ah_scrollbar([], [vertical, disabled], [{height, 220}, {value, 700}])],
           [<<"flex gap-8">>], []).

-spec sb_area() -> aihtml:html().
sb_area() ->
    ah_scrollbar([ah_p(<<"第 "/utf8, (integer_to_binary(N))/binary, " 条消息：自定义滚动条跟随内容滚动，"
                         "滚轮、触摸和键盘仍是原生的。"/utf8>>,
                       [<<"px-3 py-2 border-b border-line text-sm">>], [])
                  || N <- lists:seq(1, 30)],
                 [<<"max-w-md border border-line rounded">>],
                 [{height, 220}, {label, <<"消息"/utf8>>}]).

-spec sb_area_both() -> aihtml:html().
sb_area_both() ->
    Cols = lists:seq(1, 14),
    Row = fun(R) ->
                  ah_tr([ah_td(<<"R", (integer_to_binary(R))/binary, "C", (integer_to_binary(C))/binary>>,
                               [<<"px-3 py-1 border border-line whitespace-nowrap">>], [])
                         || C <- Cols])
          end,
    ah_scrollbar(ah_table([Row(R) || R <- lists:seq(1, 20)], [<<"text-sm">>], []),
                 [<<"max-w-lg border border-line rounded">>],
                 [{height, 200}, {step, 20}]).

%%%===================================================================
%%% Actions
%%%===================================================================

-spec action(atom(), term(), aihtml_action:event(), aihtml_action:ctx()) -> ok.
action(scrolled, _Args, #{value := V}, Ctx) ->
    aihtml_action:html(Ctx, {id, <<"sb-value">>}, [<<"服务端收到值："/utf8>>, V]).
