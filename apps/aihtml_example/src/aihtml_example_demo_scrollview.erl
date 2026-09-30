%% @doc Demos of the scrollview component (aihtml_scrollview), shown on
%% /components/scrollview. Each function is one example, written the way
%% an application writes it; the docs page prints its source under it.
%%
%% The module is also the action module of the demos that talk to the
%% server: a page change and remote pager buttons.
-module(aihtml_example_demo_scrollview).
-behaviour(aihtml_action).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0, action/4]).
-export([sv_basic/0, sv_slideshow/0, sv_cards/0, sv_server/0, sv_record/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => scrollview, title => <<"ScrollView">>,
       summary => <<"横向分页的轮播容器，支持拖拽滑动、页码圆点、键盘翻页和自动轮播。"/utf8>>,
       demos => [{<<"拖拽或点圆点翻页"/utf8>>, sv_basic},
                 {<<"自动轮播"/utf8>>, sv_slideshow},
                 {<<"卡片分页、不回弹"/utf8>>, sv_cards},
                 {<<"服务端翻页与翻页通知"/utf8>>, sv_server},
                 {<<"record 写法"/utf8>>, sv_record}]}].

%%%===================================================================
%%% Demos
%%%===================================================================

-spec sv_basic() -> aihtml:html().
sv_basic() ->
    ah_scrollview([slide(<<"#0ea5e9">>, <<"#6366f1">>, <<"圣托里尼，希腊"/utf8>>,
                         <<"爱琴海火山悬崖上的白色房屋。"/utf8>>),
                   slide(<<"#f97316">>, <<"#db2777">>, <<"京都，日本"/utf8>>,
                         <<"古老寺庙和竹林。"/utf8>>),
                   slide(<<"#10b981">>, <<"#0f766e">>, <<"班夫，加拿大"/utf8>>,
                         <<"落基山脉中碧绿的湖泊。"/utf8>>),
                   slide(<<"#eab308">>, <<"#dc2626">>, <<"阿马尔菲海岸，意大利"/utf8>>,
                         <<"地中海边色彩斑斓的悬崖村庄。"/utf8>>)],
                  [<<"rounded-lg">>], [{height, 260}, {label, <<"旅行目的地"/utf8>>}]).

-spec sv_slideshow() -> aihtml:html().
sv_slideshow() ->
    ah_scrollview([slide(<<"#6366f1">>, <<"#a855f7">>, <<"新品上市"/utf8>>, <<"秋季系列今日发布。"/utf8>>),
                   slide(<<"#0891b2">>, <<"#2563eb">>, <<"会员日"/utf8>>, <<"全场九折，仅限本周。"/utf8>>),
                   slide(<<"#059669">>, <<"#65a30d">>, <<"免费配送"/utf8>>, <<"订单满 99 元包邮。"/utf8>>)],
                  [<<"rounded-lg">>],
                  [{height, 180}, {slide_show, true}, {slide_duration, 2500},
                   {animation_duration, 500}]).

-spec sv_cards() -> aihtml:html().
sv_cards() ->
    Plans = [{<<"基础版"/utf8>>, <<"¥0"/utf8>>, <<"个人项目，3 个页面"/utf8>>},
             {<<"专业版"/utf8>>, <<"¥49"/utf8>>, <<"团队协作，无限页面"/utf8>>},
             {<<"企业版"/utf8>>, <<"¥199"/utf8>>, <<"私有部署，专属支持"/utf8>>}],
    ah_scrollview([ah_card(ah_p(Desc, [<<"text-sm text-muted">>], []), [<<"m-4">>],
                           [{title, Name}, {subtitle, Price}])
                   || {Name, Price, Desc} <- Plans],
                  [<<"w-80 border border-line rounded-lg pb-8">>],
                  [{current_page, 1}, {bounce, false}, {move_threshold, 0.25}]).

%% Page changes run action(page_changed, ...); the buttons call the
%% pager's methods from the server.
-spec sv_server() -> aihtml:html().
sv_server() ->
    ah_div([ah_scrollview([slide(<<"#334155">>, <<"#0f172a">>, <<"第一章"/utf8>>, <<"开端"/utf8>>),
                           slide(<<"#1e3a8a">>, <<"#312e81">>, <<"第二章"/utf8>>, <<"发展"/utf8>>),
                           slide(<<"#7c2d12">>, <<"#78350f">>, <<"第三章"/utf8>>, <<"高潮"/utf8>>),
                           slide(<<"#14532d">>, <<"#064e3b">>, <<"第四章"/utf8>>, <<"结局"/utf8>>)],
                          [<<"rounded-lg">>],
                          [{id, <<"story">>}, {height, 160}, {name, chapter},
                           on(change, {?MODULE, page_changed, #{}})]),
            ah_div([ah_button(<<"上一页"/utf8>>, back, [outlined, sm],
                              [on(click, {?MODULE, pager, back})]),
                    ah_button(<<"下一页"/utf8>>, forward, [outlined, sm],
                              [on(click, {?MODULE, pager, forward})]),
                    ah_span(<<"当前第 1 页"/utf8>>, [<<"text-sm text-muted">>],
                            [{id, <<"story-page">>}])],
                   [<<"flex items-center gap-3 mt-3">>], [])],
           [], []).

-spec sv_record() -> aihtml:html().
sv_record() ->
    #ah_scrollview{body = [slide(<<"#be123c">>, <<"#9f1239">>, <<"A">>, <<"record">>),
                           slide(<<"#4d7c0f">>, <<"#365314">>, <<"B">>, <<"record">>),
                           slide(<<"#1d4ed8">>, <<"#1e3a8a">>, <<"C">>, <<"record">>)],
                   height = 140, current_page = 2, animation_duration = 450,
                   css = [<<"rounded-lg">>], postback = page_changed}.

%%%===================================================================
%%% Actions
%%%===================================================================

-spec action(atom(), term(), aihtml_action:event(), aihtml_action:ctx()) -> ok.
action(page_changed, _Args, #{value := Page}, Ctx) ->
    N = binary_to_integer(Page) + 1,
    aihtml_action:html(Ctx, {id, <<"story-page">>},
                       [<<"当前第 "/utf8>>, integer_to_binary(N), <<" 页"/utf8>>]);
action(pager, Method, _Event, Ctx) ->
    aihtml_action:call(Ctx, {id, <<"story">>}, Method, []).

%%%===================================================================
%%% Helpers
%%%===================================================================

slide(From, To, Title, Desc) ->
    ah_div([ah_h3(Title, [<<"text-2xl font-bold m-0">>], []),
            ah_p(Desc, [<<"m-0 mt-1 text-sm opacity-80">>], [])],
           [<<"h-full flex flex-col justify-end p-6 text-white">>],
           [{style, <<"height:100%;background:linear-gradient(135deg,", From/binary, ",",
                      To/binary, ")">>}]).
