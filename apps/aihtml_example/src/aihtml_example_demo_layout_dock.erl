%% @doc Demos of the docking layouts (aihtml_layout_dock), shown on
%% /components/<name>. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
%%
%% The module is also the action module of the demos that talk to the
%% server: the saved layouts and the panels opened from an action.
-module(aihtml_example_demo_layout_dock).
-behaviour(aihtml_action).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0, action/4]).
-export([dk_basic/0, dk_vertical/0, dk_floating/0, dk_saved/0, dk_add/0, dk_record/0,
         dl_ide/0, dl_autohide/0, dl_restore/0, dl_save/0, dl_open/0, dl_options/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => docking, title => <<"Docking">>,
       summary => <<"可在面板之间拖动、折叠、关闭和浮动的窗口组，布局可保存到服务端。"/utf8>>,
       demos => [{<<"在两个面板之间拖动窗口"/utf8>>, dk_basic},
                 {<<"纵向排列、固定与折叠"/utf8>>, dk_vertical},
                 {<<"浮动窗口，禁止浮动"/utf8>>, dk_floating},
                 {<<"布局变化回传服务端"/utf8>>, dk_saved},
                 {<<"服务端添加窗口"/utf8>>, dk_add},
                 {<<"record 写法"/utf8>>, dk_record}]},
     #{component => dock_layout, title => <<"DockLayout">>,
       summary => <<"IDE 式停靠布局：分割、标签组、文档区，拖动标签停靠或浮动，边缘自动隐藏。"/utf8>>,
       demos => [{<<"IDE 布局"/utf8>>, dl_ide},
                 {<<"自动隐藏与文档区"/utf8>>, dl_autohide},
                 {<<"从保存的 JSON 渲染"/utf8>>, dl_restore},
                 {<<"布局变化回传服务端"/utf8>>, dl_save},
                 {<<"服务端打开面板"/utf8>>, dl_open},
                 {<<"反馈线调整、禁止浮动"/utf8>>, dl_options}]}].

%%%===================================================================
%%% Docking
%%%===================================================================

-spec dk_basic() -> aihtml:html().
dk_basic() ->
    docking([{inbox, [{mail, <<"收件箱"/utf8>>, p(<<"拖动标题栏，把我移到右边的面板。"/utf8>>)},
                      {tasks, <<"待办"/utf8>>, p(<<"3 项未完成"/utf8>>)}]},
             {side, [{calendar, <<"日历"/utf8>>, p(<<"今天没有会议。"/utf8>>)},
                     {notes, <<"便签"/utf8>>, p(<<"周五前提交报告。"/utf8>>)}]}],
            [<<"h-80">>], []).

-spec dk_vertical() -> aihtml:html().
dk_vertical() ->
    docking([{top, [{a, <<"固定窗口（不能拖动）"/utf8>>, p(<<"pinned"/utf8>>), #{pinned => true}},
                    {b, <<"已折叠"/utf8>>, p(<<"展开后可见"/utf8>>), #{collapsed => true}}]},
             {bottom, [{c, <<"窗口 C"/utf8>>, p(<<"Alt+方向键可用键盘移动"/utf8>>)}]}],
            [vertical, <<"h-80">>], [{offset, 8}]).

-spec dk_floating() -> aihtml:html().
dk_floating() ->
    'div'([docking([{left, [{log, <<"日志"/utf8>>, p(<<"拖到面板外会浮动"/utf8>>)}]},
                    {right, [{tip, <<"浮动窗口"/utf8>>, p(<<"floating"/utf8>>),
                              #{floating => {60, 120, 220}}}]}],
                   [<<"h-64">>], []),
           docking([{left, [{x, <<"不允许浮动"/utf8>>, p(<<"拖到面板外会回到原处"/utf8>>)}]},
                    {right, []}],
                   [<<"h-40">>], [{allow_float, false}, {collapse_buttons, false}])],
          [<<"flex flex-col gap-4">>], []).

-spec dk_saved() -> aihtml:html().
dk_saved() ->
    Saved = <<"{\"panels\":[{\"id\":\"l\",\"windows\":[\"w2\"]},{\"id\":\"r\",\"windows\":[\"w1\"]}],"
              "\"collapsed\":[\"w1\"],\"closed\":[\"w3\"]}">>,
    'div'([docking([{l, [{w1, <<"甲"/utf8>>, p(<<"1">>)}, {w2, <<"乙"/utf8>>, p(<<"2">>)}]},
                    {r, [{w3, <<"丙（已关闭）"/utf8>>, p(<<"3">>)}]}],
                   [<<"h-56">>],
                   [{layout, Saved}, {name, layout}, on(change, {?MODULE, docking_saved, #{}})]),
           pre(<<"拖动、折叠或关闭窗口后，服务端收到的布局显示在这里。"/utf8>>,
               [<<"text-xs text-muted whitespace-pre-wrap">>], [{id, <<"docking-saved">>}])],
          [<<"flex flex-col gap-3">>], []).

-spec dk_add() -> aihtml:html().
dk_add() ->
    'div'([button(<<"添加窗口"/utf8>>, add, [], [on(click, {?MODULE, docking_add, #{}})]),
           docking([{main, [{first, <<"第一个"/utf8>>, p(<<"服务端渲染的内容"/utf8>>)}]},
                    {more, []}],
                  [<<"h-64 w-full">>], [{id, <<"docking-add">>}])],
          [<<"flex flex-col gap-3 items-start">>], []).

%% The same component as a record: options are checked field names, and
%% the postback runs action(docking_saved, ...) below on change.
-spec dk_record() -> aihtml:html().
dk_record() ->
    #ah_docking{items = [{a, [{r1, <<"Record A">>, p(<<"a">>)}]},
                         {b, [{r2, <<"Record B">>, p(<<"b">>)}]}],
                orientation = horizontal, offset = 4, drag_opacity = 0.6,
                labels = #{collapse => <<"收起"/utf8>>, close => <<"关闭"/utf8>>},
                css = [<<"h-48">>], postback = docking_saved}.

%%%===================================================================
%%% DockLayout
%%%===================================================================

-spec dl_ide() -> aihtml:html().
dl_ide() ->
    frame(dock_layout(
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
    frame(dock_layout(
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
    frame(dock_layout(Json, [], [{panels, ide_panels()}])).

-spec dl_save() -> aihtml:html().
dl_save() ->
    'div'([frame(dock_layout({split, horizontal, [{tabs, [explorer]}, {documents, [main]}]},
                             [], [{panels, ide_panels()}, {name, layout},
                                  on(change, {?MODULE, layout_saved, #{}})])),
           pre(<<"拖动标签、调整大小或切换标签后，服务端收到的布局显示在这里。"/utf8>>,
               [<<"text-xs text-muted whitespace-pre-wrap">>], [{id, <<"layout-saved">>}])],
          [<<"flex flex-col gap-3">>], []).

-spec dl_open() -> aihtml:html().
dl_open() ->
    'div'([row([button(<<"打开 Console"/utf8>>, console, [], [on(click, {?MODULE, open, #{}})]),
                button(<<"浮动打开 Inspector"/utf8>>, inspector, [],
                       [on(click, {?MODULE, open, #{}})]),
                button(<<"右侧打开 Properties"/utf8>>, props, [],
                       [on(click, {?MODULE, open, #{}})])]),
           frame(dock_layout({documents, [main]}, [],
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
action(docking_saved, _Args, #{value := Json}, Ctx) ->
    aihtml_action:html(Ctx, {id, <<"docking-saved">>}, Json);
action(layout_saved, _Args, #{value := Json}, Ctx) ->
    aihtml_action:html(Ctx, {id, <<"layout-saved">>}, Json);
action(docking_add, _Args, _Event, Ctx) ->
    N = erlang:unique_integer([positive]),
    docking_add_window(
      Ctx, {id, <<"docking-add">>}, more,
      {<<"w", (integer_to_binary(N))/binary>>, <<"新窗口 "/utf8, (integer_to_binary(N))/binary>>,
       p(<<"由 action 在服务端渲染"/utf8>>)});
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
      ul([li(<<"src/">>), li(<<"include/">>), li(<<"rebar.config">>)],
         [<<"text-sm leading-6">>], [])},
     {search, <<"搜索"/utf8>>, input(<<>>, [], [{placeholder, <<"跨文件搜索"/utf8>>}])},
     {main, <<"main.erl">>,
      pre(<<"-module(main).\n-export([start/0]).\n\nstart() ->\n    ok.">>, [<<"text-xs">>], [])},
     {style, <<"style.css">>, pre(<<".container {\n  display: flex;\n}">>, [<<"text-xs">>], [])},
     {console, <<"控制台"/utf8>>, pre(<<"$ rebar3 compile\n===> Compiling main">>,
                                     [<<"text-xs text-success">>], [])},
     {problems, <<"问题"/utf8>>, p(<<"未检测到问题。"/utf8>>, [<<"text-sm text-muted">>], [])},
     {props, <<"属性"/utf8>>, p(<<"variant: primary"/utf8>>, [<<"text-sm">>], [])},
     {inspector, <<"检查器"/utf8>>, p(<<"DOM 树检查器。"/utf8>>, [<<"text-sm">>], [])}].

frame(Layout) ->
    'div'(Layout, [<<"h-[26rem] border border-line rounded overflow-hidden">>], []).

row(Children) ->
    'div'(Children, [<<"flex flex-wrap items-start gap-2">>], []).
