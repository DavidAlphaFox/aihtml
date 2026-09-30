%% @doc Demos of the dock layout (aihtml_dock_layout), shown on
%% /components/dock_layout. Each function is one example, written the way
%% an application writes it; the docs page prints its source under it.
%%
%% The module is also the action module of the demos that talk to the
%% server: the saved layout and the panels opened from an action.
-module(aihtml_example_demo_dock_layout).
-behaviour(aihtml_action).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0, action/4]).
-export([dl_ide/0, dl_autohide/0, dl_restore/0, dl_save/0, dl_open/0, dl_options/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => dock_layout, title => <<"DockLayout">>,
       summary => <<"IDE 式停靠布局：分割、标签组、文档区，拖动标签停靠或浮动，边缘自动隐藏。"/utf8>>,
       demos => [{<<"IDE 布局"/utf8>>, dl_ide},
                 {<<"自动隐藏与文档区"/utf8>>, dl_autohide},
                 {<<"从保存的 JSON 渲染"/utf8>>, dl_restore},
                 {<<"布局变化回传服务端"/utf8>>, dl_save},
                 {<<"服务端打开面板"/utf8>>, dl_open},
                 {<<"反馈线调整、禁止浮动"/utf8>>, dl_options}]}].

%%%===================================================================
%%% DockLayout
%%%===================================================================

-spec dl_ide() -> aihtml:html().
dl_ide() ->
    frame(ah_dock_layout(
               [{split, horizontal,
                 [{tabs, [explorer, search], #{size => 22}},
                  {split, vertical, [{documents, [main, style], #{size => 65, close => true}},
                                     {tabs, [console, problems], #{size => 35}}],
                   #{size => 56}},
                  {tabs, [props], #{size => 22}}]},
                {float, [inspector], #{x => 120, y => 80, width => 260, height => 180}}],
               [], [{panels, ide_panels()}])).

-spec dl_autohide() -> aihtml:html().
dl_autohide() ->
    frame(ah_dock_layout(
               [{split, vertical,
                 [{documents, [main, style]},
                  {tabs, [console], #{size => 30}}]},
                {autohide, left, [explorer, search], #{size => 240}},
                {autohide, right, [props], #{size => 220}}],
               [], [{panels, ide_panels()}])).

-spec dl_restore() -> aihtml:html().
dl_restore() ->
    Json = <<"[{\"type\":\"split\",\"orientation\":\"horizontal\",\"items\":["
             "{\"type\":\"documents\",\"size\":70,\"items\":[\"style\",\"main\"],\"active\":\"main\"},"
             "{\"type\":\"tabs\",\"size\":30,\"items\":[\"console\",\"explorer\"]}]},"
             "{\"type\":\"autohide\",\"edge\":\"bottom\",\"size\":160,\"items\":[\"problems\"]}]">>,
    frame(ah_dock_layout(Json, [], [{panels, ide_panels()}])).

-spec dl_save() -> aihtml:html().
dl_save() ->
    ah_div([frame(ah_dock_layout({split, horizontal, [{tabs, [explorer]}, {documents, [main]}]},
                                 [], [{panels, ide_panels()}, {name, layout},
                                      on(change, {?MODULE, layout_saved, #{}})])),
            ah_pre(<<"拖动标签、调整大小或切换标签后，服务端收到的布局显示在这里。"/utf8>>,
                   [<<"text-xs text-muted whitespace-pre-wrap">>], [{id, <<"layout-saved">>}])],
           [<<"flex flex-col gap-3">>], []).

-spec dl_open() -> aihtml:html().
dl_open() ->
    ah_div([row([ah_button(<<"打开 Console"/utf8>>, console, [], [on(click, {?MODULE, open, #{}})]),
                 ah_button(<<"浮动打开 Inspector"/utf8>>, inspector, [],
                           [on(click, {?MODULE, open, #{}})]),
                 ah_button(<<"右侧打开 Properties"/utf8>>, props, [],
                           [on(click, {?MODULE, open, #{}})])]),
            frame(ah_dock_layout({documents, [main]}, [],
                                 [{id, <<"dl-open">>}, {panels, ide_panels()}]))],
           [<<"flex flex-col gap-3">>], []).

-spec dl_options() -> aihtml:html().
dl_options() ->
    frame(#ah_dock_layout{layout = {split, horizontal, [{tabs, [explorer]}, {tabs, [console]},
                                                        {tabs, [props]}]},
                          panels = ide_panels(), resize_mode = feedback, min_size = 160,
                          allow_float = false,
                          labels = #{auto_hide => <<"自动隐藏"/utf8>>, float => <<"浮动"/utf8>>,
                                     dock => <<"停靠"/utf8>>, close => <<"关闭"/utf8>>}}).

%%%===================================================================
%%% Actions
%%%===================================================================

-spec action(atom(), term(), aihtml_action:event(), aihtml_action:ctx()) -> ok.
action(layout_saved, _Args, #{value := Json}, Ctx) ->
    aihtml_action:html(Ctx, {id, <<"layout-saved">>}, Json);
action(open, _Args, #{value := Which}, Ctx) ->
    Panel = lists:keyfind(binary_to_existing_atom(Which), 1, ide_panels()),
    Where = case Which of
                <<"inspector">> -> #{float => {80, 60}};
                <<"props">> -> #{edge => right};
                _ -> #{}
            end,
    dock_layout_open(Ctx, {id, <<"dl-open">>}, Panel, Where).

%%%===================================================================
%%% Data
%%%===================================================================

ide_panels() ->
    [{explorer, <<"资源管理器"/utf8>>,
      ah_ul([ah_li(<<"src/">>), ah_li(<<"include/">>), ah_li(<<"rebar.config">>)],
            [<<"text-sm leading-6">>], [])},
     {search, <<"搜索"/utf8>>, ah_input(<<>>, [], [{placeholder, <<"跨文件搜索"/utf8>>}])},
     {main, <<"main.erl">>,
      ah_pre(<<"-module(main).\n-export([start/0]).\n\nstart() ->\n    ok.">>, [<<"text-xs">>], [])},
     {style, <<"style.css">>, ah_pre(<<".container {\n  display: flex;\n}">>, [<<"text-xs">>], [])},
     {console, <<"控制台"/utf8>>, ah_pre(<<"$ rebar3 compile\n===> Compiling main">>,
                                        [<<"text-xs text-success">>], [])},
     {problems, <<"问题"/utf8>>, ah_p(<<"未检测到问题。"/utf8>>, [<<"text-sm text-muted">>], [])},
     {props, <<"属性"/utf8>>, ah_p(<<"variant: primary"/utf8>>, [<<"text-sm">>], [])},
     {inspector, <<"检查器"/utf8>>, ah_p(<<"DOM 树检查器。"/utf8>>, [<<"text-sm">>], [])}].

frame(Layout) ->
    ah_div(Layout, [<<"h-[26rem] border border-line rounded overflow-hidden">>], []).

row(Children) ->
    ah_div(Children, [<<"flex flex-wrap items-start gap-2">>], []).
