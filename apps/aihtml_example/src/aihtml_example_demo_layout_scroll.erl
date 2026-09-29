%% @doc Demos of the scrolling layout components (aihtml_layout_scroll),
%% shown on /components/<name>. Each function is one example, written the
%% way an application writes it; the docs page prints its source under it.
%%
%% The module is also the action module of the demos that talk to the
%% server: a page change, a scrollbar value, remote pager buttons and a
%% responsive panel whose content loads on first view.
-module(aihtml_example_demo_layout_scroll).
-behaviour(aihtml_action).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0, action/4]).
-export([sv_basic/0, sv_slideshow/0, sv_cards/0, sv_server/0, sv_record/0,
         sb_horizontal/0, sb_vertical/0, sb_area/0, sb_area_both/0,
         rp_basic/0, rp_animation/0, rp_external/0, rp_load/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => scrollview, title => <<"ScrollView">>,
       summary => <<"横向分页的轮播容器，支持拖拽滑动、页码圆点、键盘翻页和自动轮播。"/utf8>>,
       demos => [{<<"拖拽或点圆点翻页"/utf8>>, sv_basic},
                 {<<"自动轮播"/utf8>>, sv_slideshow},
                 {<<"卡片分页、不回弹"/utf8>>, sv_cards},
                 {<<"服务端翻页与翻页通知"/utf8>>, sv_server},
                 {<<"record 写法"/utf8>>, sv_record}]},
     #{component => scrollbar, title => <<"Scrollbar">>,
       summary => <<"自定义滚动条：可独立取值，也可包住内容作为滚动区域。"/utf8>>,
       demos => [{<<"水平滚动条，拖动后通知服务端"/utf8>>, sb_horizontal},
                 {<<"垂直、无按钮、禁用"/utf8>>, sb_vertical},
                 {<<"包住内容的滚动区域"/utf8>>, sb_area},
                 {<<"横竖两个方向"/utf8>>, sb_area_both}]},
     #{component => responsive_panel, title => <<"ResponsivePanel">>,
       summary => <<"父容器够宽时就地展开，窄于断点时收成按钮和浮层。"/utf8>>,
       demos => [{<<"宽容器展开，窄容器折叠"/utf8>>, rp_basic},
                 {<<"浮层动画：fade、slide、none"/utf8>>, rp_animation},
                 {<<"外部切换按钮，不自动关闭"/utf8>>, rp_external},
                 {<<"首次显示时由服务端加载内容"/utf8>>, rp_load}]}].

%%%===================================================================
%%% ScrollView
%%%===================================================================

-spec sv_basic() -> aihtml:html().
sv_basic() ->
    scrollview([slide(<<"#0ea5e9">>, <<"#6366f1">>, <<"圣托里尼，希腊"/utf8>>,
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
    scrollview([slide(<<"#6366f1">>, <<"#a855f7">>, <<"新品上市"/utf8>>, <<"秋季系列今日发布。"/utf8>>),
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
    scrollview([card(p(Desc, [<<"text-sm text-muted">>], []), [<<"m-4">>],
                     [{title, Name}, {subtitle, Price}])
                || {Name, Price, Desc} <- Plans],
               [<<"w-80 border border-line rounded-lg pb-8">>],
               [{current_page, 1}, {bounce, false}, {move_threshold, 0.25}]).

%% Page changes run action(page_changed, ...); the buttons call the
%% pager's methods from the server.
-spec sv_server() -> aihtml:html().
sv_server() ->
    'div'([scrollview([slide(<<"#334155">>, <<"#0f172a">>, <<"第一章"/utf8>>, <<"开端"/utf8>>),
                       slide(<<"#1e3a8a">>, <<"#312e81">>, <<"第二章"/utf8>>, <<"发展"/utf8>>),
                       slide(<<"#7c2d12">>, <<"#78350f">>, <<"第三章"/utf8>>, <<"高潮"/utf8>>),
                       slide(<<"#14532d">>, <<"#064e3b">>, <<"第四章"/utf8>>, <<"结局"/utf8>>)],
                      [<<"rounded-lg">>],
                      [{id, <<"story">>}, {height, 160}, {name, chapter},
                       on(change, {?MODULE, page_changed, #{}})]),
           'div'([button(<<"上一页"/utf8>>, back, [outlined, sm],
                         [on(click, {?MODULE, pager, back})]),
                  button(<<"下一页"/utf8>>, forward, [outlined, sm],
                         [on(click, {?MODULE, pager, forward})]),
                  span(<<"当前第 1 页"/utf8>>, [<<"text-sm text-muted">>],
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
%%% Scrollbar
%%%===================================================================

%% Releasing the thumb (or a click) runs action(scrolled, ...).
-spec sb_horizontal() -> aihtml:html().
sb_horizontal() ->
    'div'([scrollbar([], [<<"max-w-sm">>],
                     [{value, 250}, {max, 1000}, {name, offset},
                      {label, <<"偏移量"/utf8>>}, on(change, {?MODULE, scrolled, #{}})]),
           span(<<"值：250"/utf8>>, [<<"text-sm text-muted">>], [{id, <<"sb-value">>}])],
          [<<"flex items-center gap-4">>], []).

-spec sb_vertical() -> aihtml:html().
sb_vertical() ->
    'div'([scrollbar([], [vertical], [{height, 220}, {value, 400}]),
           scrollbar([], [vertical], [{height, 220}, {max, 100}, {value, 30},
                                      {show_buttons, false}]),
           scrollbar([], [vertical, disabled], [{height, 220}, {value, 700}])],
          [<<"flex gap-8">>], []).

-spec sb_area() -> aihtml:html().
sb_area() ->
    scrollbar([p(<<"第 "/utf8, (integer_to_binary(N))/binary, " 条消息：自定义滚动条跟随内容滚动，"
                   "滚轮、触摸和键盘仍是原生的。"/utf8>>,
                 [<<"px-3 py-2 border-b border-line text-sm">>], [])
               || N <- lists:seq(1, 30)],
              [<<"max-w-md border border-line rounded">>],
              [{height, 220}, {label, <<"消息"/utf8>>}]).

-spec sb_area_both() -> aihtml:html().
sb_area_both() ->
    Cols = lists:seq(1, 14),
    Row = fun(R) ->
                  tr([td(<<"R", (integer_to_binary(R))/binary, "C", (integer_to_binary(C))/binary>>,
                         [<<"px-3 py-1 border border-line whitespace-nowrap">>], [])
                      || C <- Cols])
          end,
    scrollbar(table([Row(R) || R <- lists:seq(1, 20)], [<<"text-sm">>], []),
              [<<"max-w-lg border border-line rounded">>],
              [{height, 200}, {step, 20}]).

%%%===================================================================
%%% ResponsivePanel
%%%===================================================================

-spec rp_basic() -> aihtml:html().
rp_basic() ->
    'div'([frame(<<"w-full">>, responsive_panel(nav(), [], [{breakpoint, 400}])),
           frame(<<"w-72">>, responsive_panel(nav(), [], [{breakpoint, 400}]))],
          [<<"flex flex-col gap-4">>], []).

-spec rp_animation() -> aihtml:html().
rp_animation() ->
    'div'([frame(<<"w-40">>, responsive_panel(nav(), [], [{breakpoint, 500}, {animation, A}]))
           || A <- [fade, slide, none]],
          [<<"flex flex-wrap gap-6">>], []).

-spec rp_external() -> aihtml:html().
rp_external() ->
    'div'([button(<<"菜单"/utf8>>, menu, [outlined, sm], [{id, <<"rp-menu-btn">>}]),
           frame(<<"w-60">>,
                 responsive_panel(nav(), [],
                                  [{breakpoint, 500}, {toggle_button, <<"#rp-menu-btn">>},
                                   {auto_close, false}, {collapse_width, 240},
                                   {toggle_content, <<"⋯"/utf8>>}, {toggle_size, 36}]))],
          [<<"flex items-start gap-4">>], []).

%% The content is empty until it is first shown; then action(load_nav, ...)
%% fills it from the server.
-spec rp_load() -> aihtml:html().
rp_load() ->
    'div'([frame(<<"w-full">>,
                 responsive_panel(p(<<"加载中…"/utf8>>, [<<"text-sm text-muted p-2">>], []), [],
                                  [{breakpoint, 400}, {load, {?MODULE, load_nav, #{}}}])),
           frame(<<"w-60">>,
                 #ah_responsive_panel{body = p(<<"加载中…"/utf8>>, [<<"text-sm text-muted p-2">>], []),
                                      breakpoint = 400, animation = slide,
                                      load = {?MODULE, load_nav, #{}}})],
          [<<"flex flex-col gap-4">>], []).

%%%===================================================================
%%% Actions
%%%===================================================================

-spec action(atom(), term(), aihtml_action:event(), aihtml_action:ctx()) -> ok.
action(page_changed, _Args, #{value := Page}, Ctx) ->
    N = binary_to_integer(Page) + 1,
    aihtml_action:html(Ctx, {id, <<"story-page">>},
                       [<<"当前第 "/utf8>>, integer_to_binary(N), <<" 页"/utf8>>]);
action(pager, Method, _Event, Ctx) ->
    aihtml_action:call(Ctx, {id, <<"story">>}, Method, []);
action(scrolled, _Args, #{value := V}, Ctx) ->
    aihtml_action:html(Ctx, {id, <<"sb-value">>}, [<<"服务端收到值："/utf8>>, V]);
action(load_nav, _Args, #{id := Id}, Ctx) ->
    aihtml_action:html(Ctx, {id, Id}, nav()).

%%%===================================================================
%%% Helpers
%%%===================================================================

slide(From, To, Title, Desc) ->
    'div'([h3(Title, [<<"text-2xl font-bold m-0">>], []),
           p(Desc, [<<"m-0 mt-1 text-sm opacity-80">>], [])],
          [<<"h-full flex flex-col justify-end p-6 text-white">>],
          [{style, <<"height:100%;background:linear-gradient(135deg,", From/binary, ",",
                     To/binary, ")">>}]).

nav() ->
    ul([li(a(Label, [<<"block px-3 py-1.5 text-sm rounded text-fg no-underline hover:bg-surface-2">>], [{href, <<"#">>}]))
        || Label <- [<<"仪表盘"/utf8>>, <<"数据分析"/utf8>>, <<"报表"/utf8>>,
                     <<"用户"/utf8>>, <<"设置"/utf8>>]],
       [<<"list-none m-0 py-2 px-0">>], []).

frame(Width, Panel) ->
    'div'(Panel, [Width, <<"border-2 border-dashed border-line rounded p-3">>], []).
