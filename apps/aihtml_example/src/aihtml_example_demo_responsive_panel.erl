%% @doc Demos of the responsive_panel component (aihtml_responsive_panel),
%% shown on /components/responsive_panel. Each function is one example,
%% written the way an application writes it; the docs page prints its
%% source under it.
%%
%% The module is also the action module of the demo whose content loads
%% from the server on first view.
-module(aihtml_example_demo_responsive_panel).
-behaviour(aihtml_action).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0, action/4]).
-export([rp_basic/0, rp_animation/0, rp_external/0, rp_load/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => responsive_panel, title => <<"ResponsivePanel">>,
       summary => <<"父容器够宽时就地展开，窄于断点时收成按钮和浮层。"/utf8>>,
       demos => [{<<"宽容器展开，窄容器折叠"/utf8>>, rp_basic},
                 {<<"浮层动画：fade、slide、none"/utf8>>, rp_animation},
                 {<<"外部切换按钮，不自动关闭"/utf8>>, rp_external},
                 {<<"首次显示时由服务端加载内容"/utf8>>, rp_load}]}].

%%%===================================================================
%%% Demos
%%%===================================================================

-spec rp_basic() -> aihtml:html().
rp_basic() ->
    ah_div([frame(<<"w-full">>, ah_responsive_panel(nav(), [], [{breakpoint, 400}])),
            frame(<<"w-72">>, ah_responsive_panel(nav(), [], [{breakpoint, 400}]))],
           [<<"flex flex-col gap-4">>], []).

-spec rp_animation() -> aihtml:html().
rp_animation() ->
    ah_div([frame(<<"w-40">>, ah_responsive_panel(nav(), [], [{breakpoint, 500}, {animation, A}]))
            || A <- [fade, slide, none]],
           [<<"flex flex-wrap gap-6">>], []).

-spec rp_external() -> aihtml:html().
rp_external() ->
    ah_div([ah_button(<<"菜单"/utf8>>, menu, [outlined, sm], [{id, <<"rp-menu-btn">>}]),
            frame(<<"w-60">>,
                  ah_responsive_panel(nav(), [],
                                      [{breakpoint, 500}, {toggle_button, <<"#rp-menu-btn">>},
                                       {auto_close, false}, {collapse_width, 240},
                                       {toggle_content, <<"⋯"/utf8>>}, {toggle_size, 36}]))],
           [<<"flex items-start gap-4">>], []).

%% The content is empty until it is first shown; then action(load_nav, ...)
%% fills it from the server.
-spec rp_load() -> aihtml:html().
rp_load() ->
    ah_div([frame(<<"w-full">>,
                  ah_responsive_panel(ah_p(<<"加载中…"/utf8>>, [<<"text-sm text-muted p-2">>], []), [],
                                      [{breakpoint, 400}, {load, {?MODULE, load_nav, #{}}}])),
            frame(<<"w-60">>,
                  #ah_responsive_panel{body = ah_p(<<"加载中…"/utf8>>, [<<"text-sm text-muted p-2">>], []),
                                       breakpoint = 400, animation = slide,
                                       load = {?MODULE, load_nav, #{}}})],
           [<<"flex flex-col gap-4">>], []).

%%%===================================================================
%%% Actions
%%%===================================================================

-spec action(atom(), term(), aihtml_action:event(), aihtml_action:ctx()) -> ok.
action(load_nav, _Args, #{id := Id}, Ctx) ->
    aihtml_action:html(Ctx, {id, Id}, nav()).

%%%===================================================================
%%% Helpers
%%%===================================================================

nav() ->
    ah_ul([ah_li(ah_a(Label, [<<"block px-3 py-1.5 text-sm rounded text-fg no-underline hover:bg-surface-2">>], [{href, <<"#">>}]))
           || Label <- [<<"仪表盘"/utf8>>, <<"数据分析"/utf8>>, <<"报表"/utf8>>,
                        <<"用户"/utf8>>, <<"设置"/utf8>>]],
          [<<"list-none m-0 py-2 px-0">>], []).

frame(Width, Panel) ->
    ah_div(Panel, [Width, <<"border-2 border-dashed border-line rounded p-3">>], []).
