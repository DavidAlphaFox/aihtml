%% @doc Demos of the ribbon and the tile layout (aihtml_layout_tiles),
%% shown on /components/<name>. Each function is one example, written the
%% way an application writes it; the docs page prints its source under it.
%%
%% The module is also the action module of the server-driven demos: ribbon
%% commands reported to the server, and a tile layout whose arrangement
%% the server receives and can reset.
-module(aihtml_example_demo_layout_tiles).
-behaviour(aihtml_action).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0, action/4]).
-export([rb_office/0, rb_basic/0, rb_positions/0, rb_collapsible/0, rb_popup/0,
         rb_colors/0, rb_server/0, rb_record/0,
         tl_ide/0, tl_dashboard/0, tl_saved/0, tl_server/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => ribbon, title => <<"Ribbon">>,
       summary => <<"Office 风格的功能区：选项卡下分组排列大小按钮、开关和下拉菜单，命令可回传服务端。"/utf8>>,
       demos => [{<<"Office 风格功能区"/utf8>>, rb_office},
                 {<<"基本用法与禁用选项卡"/utf8>>, rb_basic},
                 {<<"底部与左侧位置"/utf8>>, rb_positions},
                 {<<"可折叠（双击选项卡或 Ctrl+F1）"/utf8>>, rb_collapsible},
                 {<<"弹出模式与悬停切换"/utf8>>, rb_popup},
                 {<<"颜色模板与动画"/utf8>>, rb_colors},
                 {<<"命令交给服务端处理"/utf8>>, rb_server},
                 {<<"record 写法"/utf8>>, rb_record}]},
     #{component => tile_layout, title => <<"TileLayout">>,
       summary => <<"IDE 式平铺布局：拖动分割条调整大小，把标签拖到其他面板或面板边缘重新排列。"/utf8>>,
       demos => [{<<"IDE 风格布局"/utf8>>, tl_ide},
                 {<<"仪表盘：平铺面板与最小尺寸"/utf8>>, tl_dashboard},
                 {<<"按保存的排列渲染"/utf8>>, tl_saved},
                 {<<"排列交给服务端保存"/utf8>>, tl_server}]}].

%%%===================================================================
%%% Ribbon
%%%===================================================================

-spec rb_office() -> aihtml:html().
rb_office() ->
    ribbon([#{key => home, label => <<"开始"/utf8>>,
              groups => [{<<"剪贴板"/utf8>>,
                          [#{key => paste, label => <<"粘贴"/utf8>>, icon => icon(paste), size => large,
                             items => [{paste, <<"粘贴"/utf8>>}, {paste_text, <<"只粘贴文本"/utf8>>}]},
                           {stack, [{cut, icon(cut), <<"剪切"/utf8>>},
                                    {copy, icon(copy), <<"复制"/utf8>>},
                                    #{key => painter, icon => icon(brush), label => <<"格式刷"/utf8>>,
                                      disabled => true}]}]},
                         {<<"字体"/utf8>>,
                          [{stack, [#{key => bold, icon => <<"B">>, label => <<"加粗"/utf8>>,
                                      toggle => true, pressed => true},
                                    #{key => italic, icon => <<"I">>, label => <<"斜体"/utf8>>,
                                      toggle => true},
                                    #{key => underline, icon => <<"U">>, label => <<"下划线"/utf8>>,
                                      toggle => true}]},
                           separator,
                           {stack, [#{key => size, label => <<"字号"/utf8>>,
                                      items => [{s12, <<"12">>}, {s14, <<"14">>}, {s18, <<"18">>},
                                                divider, {grow, <<"增大字号"/utf8>>}]},
                                    {superscript, <<"x²"/utf8>>, <<"上标"/utf8>>},
                                    {subscript, <<"x₂"/utf8>>, <<"下标"/utf8>>}]}]},
                         {<<"编辑"/utf8>>,
                          [#{key => find, label => <<"查找"/utf8>>, icon => icon(search), size => large},
                           #{key => replace, label => <<"替换"/utf8>>, icon => icon(replace), size => large}]}]},
            #{key => insert, label => <<"插入"/utf8>>,
              groups => [{<<"表格"/utf8>>, [#{key => table, label => <<"表格"/utf8>>, icon => icon(table),
                                              size => large}]},
                         {<<"插图"/utf8>>, [#{key => image, label => <<"图片"/utf8>>, icon => icon(image),
                                              size => large},
                                            #{key => chart, label => <<"图表"/utf8>>, icon => icon(chart),
                                              size => large},
                                            #{key => shape, label => <<"形状"/utf8>>, icon => icon(shape),
                                              size => large,
                                              items => [{rect, <<"矩形"/utf8>>}, {circle, <<"圆形"/utf8>>},
                                                        {arrow, <<"箭头"/utf8>>}]}]}]},
            #{key => view, label => <<"视图"/utf8>>,
              groups => [{<<"显示"/utf8>>, [#{key => ruler, label => <<"标尺"/utf8>>, toggle => true},
                                             #{key => grid, label => <<"网格线"/utf8>>, toggle => true,
                                               pressed => true},
                                             #{key => nav, label => <<"导航窗格"/utf8>>, toggle => true}]},
                         {<<"缩放"/utf8>>, [{zoom, icon(search), <<"100%">>},
                                             {one_page, icon(copy), <<"单页"/utf8>>}]}]}],
           home, [collapsible], []).

-spec rb_basic() -> aihtml:html().
rb_basic() ->
    ribbon([{home, <<"首页"/utf8>>, para(<<"首页内容区域"/utf8>>)},
            {edit, <<"编辑（禁用）"/utf8>>, para(<<"编辑内容区域"/utf8>>), [{disabled, true}]},
            {view, <<"视图"/utf8>>, para(<<"视图内容区域"/utf8>>)}],
           home, [], [{name, tab}]).

-spec rb_positions() -> aihtml:html().
rb_positions() ->
    'div'([ribbon([{file, <<"文件"/utf8>>, para(<<"文件内容"/utf8>>)},
                   {home, <<"首页"/utf8>>, para(<<"首页内容"/utf8>>)},
                   {help, <<"帮助"/utf8>>, para(<<"帮助内容"/utf8>>)}],
                  home, [bottom], [{height, 120}]),
           ribbon([{file, <<"文件"/utf8>>, para(<<"文件内容"/utf8>>)},
                   {home, <<"首页"/utf8>>, para(<<"首页内容"/utf8>>)},
                   {data, <<"数据"/utf8>>, para(<<"数据内容"/utf8>>)},
                   {help, <<"帮助"/utf8>>, para(<<"帮助内容"/utf8>>)}],
                  home, [left], [{height, 170}])],
          [<<"flex flex-col gap-4">>], []).

-spec rb_collapsible() -> aihtml:html().
rb_collapsible() ->
    'div'([ribbon(small_tabs(), home, [collapsible, collapsed], []),
           p(<<"折叠后点击选项卡，面板浮在页面上方；点击命令或页面其他位置会收起。"/utf8>>,
             [<<"text-sm text-muted">>], [])],
          [<<"flex flex-col gap-2 pb-24">>], []).

-spec rb_popup() -> aihtml:html().
rb_popup() ->
    'div'(ribbon(small_tabs(), home, [popup], [{selection_mode, hover}]),
          [<<"pb-24">>], []).

-spec rb_colors() -> aihtml:html().
rb_colors() ->
    'div'([ribbon(small_tabs(), home, [C, A], [])
           || {C, A} <- [{primary, slide}, {success, fade}, {warning, fade}, {danger, slide}]],
          [<<"flex flex-col gap-3">>], []).

%% Clicking a command runs action(command, ...) below.
-spec rb_server() -> aihtml:html().
rb_server() ->
    'div'([ribbon(small_tabs(), home, [], [on('ah:command', {?MODULE, command, #{}})]),
           span(<<"还没有执行命令"/utf8>>, [<<"text-sm text-muted">>], [{id, <<"rb-log">>}])],
          [<<"flex flex-col gap-2">>], []).

-spec rb_record() -> aihtml:html().
rb_record() ->
    'div'([#ah_ribbon{items = small_tabs(), value = insert, color = success, collapsible = true,
                      postback = command},
           span(<<"还没有执行命令"/utf8>>, [<<"text-sm text-muted">>], [{id, <<"rb-log">>}])],
          [<<"flex flex-col gap-2">>], []).

small_tabs() ->
    [#{key => home, label => <<"开始"/utf8>>,
       groups => [{<<"剪贴板"/utf8>>, [#{key => paste, label => <<"粘贴"/utf8>>, icon => icon(paste),
                                          size => large},
                                        {stack, [{cut, icon(cut), <<"剪切"/utf8>>},
                                                 {copy, icon(copy), <<"复制"/utf8>>}]}]},
                  {<<"格式"/utf8>>, [#{key => bold, icon => <<"B">>, label => <<"加粗"/utf8>>,
                                        toggle => true}]}]},
     #{key => insert, label => <<"插入"/utf8>>,
       groups => [{<<"插图"/utf8>>, [#{key => image, label => <<"图片"/utf8>>, icon => icon(image),
                                        size => large},
                                      #{key => chart, label => <<"图表"/utf8>>, icon => icon(chart),
                                        size => large}]}]},
     #{key => view, label => <<"视图"/utf8>>,
       groups => [{<<"缩放"/utf8>>, [{zoom, icon(search), <<"100%">>}]}]}].

%%%===================================================================
%%% TileLayout
%%%===================================================================

-spec tl_ide() -> aihtml:html().
tl_ide() ->
    tile_layout(ide_layout(), undefined, [<<"border rounded">>], [{height, 420}]).

-spec tl_dashboard() -> aihtml:html().
tl_dashboard() ->
    tile_layout({rows, [#{columns => [#{id => sales, label => <<"销售额"/utf8>>, min => 120,
                                        content => kpi(<<"销售额"/utf8>>, <<"¥ 1,284,300"/utf8>>)},
                                      #{id => orders, label => <<"订单"/utf8>>, min => 120,
                                        content => kpi(<<"订单"/utf8>>, <<"3,942">>)},
                                      #{id => users, label => <<"活跃用户"/utf8>>, min => 120,
                                        content => kpi(<<"活跃用户"/utf8>>, <<"18,204">>)}],
                          size => 110, resize => false},
                        #{columns => [{tabs, [{trend, <<"趋势"/utf8>>, pane(<<"最近 30 天的销售趋势图"/utf8>>)},
                                              {region, <<"地区"/utf8>>, pane(<<"按地区汇总"/utf8>>)}]},
                                      #{id => todo, label => <<"待办"/utf8>>, size => <<"35%">>, min => 160,
                                        content => pane(<<"3 个待审批的退款"/utf8>>)}]}]},
                undefined, [<<"border rounded">>], [{height, 380}]).

%% A stored arrangement: the terminal moved next to the editor, the
%% outline tab closed.
-spec tl_saved() -> aihtml:html().
tl_saved() ->
    Saved = <<"{\"closed\":[\"outline\"],\"root\":{\"id\":\"root\",\"items\":["
              "{\"active\":\"search\",\"id\":\"left\",\"size\":\"22fr\",\"tabs\":[\"explorer\",\"search\"],\"type\":\"tabs\"},"
              "{\"active\":\"core\",\"id\":\"editors\",\"size\":\"48fr\",\"tabs\":[\"core\",\"tiles\"],\"type\":\"tabs\"},"
              "{\"active\":\"terminal\",\"id\":\"bottom\",\"size\":\"30fr\",\"tabs\":[\"terminal\",\"output\"],\"type\":\"tabs\"}"
              "],\"type\":\"columns\"}}">>,
    tile_layout(ide_layout(), Saved, [<<"border rounded">>], [{height, 320}]).

%% Every change posts the arrangement to action(arranged, ...); the button
%% runs action(reset, ...), which renders the layout afresh.
-spec tl_server() -> aihtml:html().
tl_server() ->
    'div'([button(<<"重置布局"/utf8>>, undefined, [outlined, sm],
                  [on(click, {?MODULE, reset, #{}})]),
           server_layout(),
           pre(<<"拖动标签或分割条后，这里显示服务端收到的排列"/utf8>>,
               [<<"text-xs text-muted whitespace-pre-wrap break-all m-0">>], [{id, <<"tl-state">>}])],
          [<<"flex flex-col gap-2 items-start">>], []).

server_layout() ->
    tile_layout({columns, [{tabs, [{a, <<"甲"/utf8>>, pane(<<"面板甲"/utf8>>)},
                                   {b, <<"乙"/utf8>>, pane(<<"面板乙"/utf8>>)}]},
                           {tabs, [{c, <<"丙"/utf8>>, pane(<<"面板丙"/utf8>>)},
                                   {d, <<"丁"/utf8>>, pane(<<"面板丁"/utf8>>)}]}]},
                undefined, [<<"border rounded w-full">>],
                [{id, <<"tl-server">>}, {height, 220}, on(change, {?MODULE, arranged, #{}})]).

ide_layout() ->
    #{id => root,
      columns => [#{id => left, size => <<"22%">>, min => 120,
                    tabs => [{explorer, <<"资源管理器"/utf8>>,
                              pane(<<"src/  components/  tile_layout.erl  core.erl"/utf8>>)},
                             {search, <<"搜索"/utf8>>,
                              input(undefined, [], [{placeholder, <<"搜索文件…"/utf8>>}])}]},
                  #{id => center,
                    rows => [#{id => editors, min => 100,
                               tabs => [{core, <<"core.erl">>, src_block(<<"-module(core).\n-export([main/0]).\n\nmain() -> ok.">>)},
                                        {tiles, <<"tiles.erl">>, src_block(<<"%% tile layout\n-module(tiles).">>)}]},
                             #{id => bottom, size => <<"35%">>, min => 80,
                               tabs => [{terminal, <<"终端"/utf8>>, src_block(<<"$ rebar3 compile\n===> Compiling aihtml">>)},
                                        {output, <<"输出"/utf8>>, pane(<<"构建成功，0 个警告"/utf8>>)}]}]},
                  #{id => right, size => <<"18%">>, min => 100,
                    tabs => [#{id => outline, label => <<"大纲"/utf8>>, close => false,
                               content => pane(<<"main/0"/utf8>>)}]}]}.

%%%===================================================================
%%% Actions
%%%===================================================================

-spec action(atom(), term(), aihtml_action:event(), aihtml_action:ctx()) -> ok.
action(command, _Args, #{data := Data, value := Tab}, Ctx) ->
    Cmd = maps:get(<<"command">>, Data, <<>>),
    State = case maps:get(<<"pressed">>, Data, undefined) of
                undefined -> <<>>;
                P -> [<<"（按下："/utf8>>, P, <<"）"/utf8>>]
            end,
    aihtml_action:html(Ctx, {id, <<"rb-log">>},
                       [<<"服务端执行了 "/utf8>>, Cmd, State, <<"，当前选项卡 "/utf8>>, Tab]);
action(arranged, _Args, #{value := Value}, Ctx) ->
    aihtml_action:html(Ctx, {id, <<"tl-state">>}, Value);
action(reset, _Args, _Event, Ctx) ->
    aihtml_action:html(Ctx, {id, <<"tl-server">>}, server_layout(), outer),
    aihtml_action:html(Ctx, {id, <<"tl-state">>}, <<"布局已重置"/utf8>>).

%%%===================================================================
%%% Helpers
%%%===================================================================

para(T) ->
    p(T, [<<"m-0 text-sm text-muted">>], []).

pane(T) ->
    'div'(T, [<<"p-3 text-sm text-muted">>], []).

src_block(T) ->
    pre(T, [<<"m-0 p-3 text-xs font-mono whitespace-pre text-muted">>], []).

kpi(Label, Value) ->
    'div'([span(Label, [<<"text-xs text-muted">>], []),
           span(Value, [<<"text-2xl font-bold">>], [])],
          [<<"flex flex-col gap-1 p-4">>], []).

%% 20px line icons (stroke follows the text colour).
icon(Name) ->
    {safe, [<<"<svg viewBox=\"0 0 24 24\" width=\"20\" height=\"20\" fill=\"none\" "
              "stroke=\"currentColor\" stroke-width=\"1.7\" stroke-linecap=\"round\" "
              "stroke-linejoin=\"round\">">>, path(Name), <<"</svg>">>]}.

path(paste) -> <<"<rect x=\"5\" y=\"4\" width=\"14\" height=\"17\" rx=\"2\"/><path d=\"M9 4h6v3H9z\"/>">>;
path(cut) -> <<"<circle cx=\"6\" cy=\"18\" r=\"3\"/><circle cx=\"18\" cy=\"18\" r=\"3\"/>"
               "<path d=\"M8 16 20 4M16 16 4 4\"/>">>;
path(copy) -> <<"<rect x=\"8\" y=\"8\" width=\"12\" height=\"12\" rx=\"2\"/>"
                "<path d=\"M4 16V6a2 2 0 0 1 2-2h10\"/>">>;
path(brush) -> <<"<path d=\"M4 4h14v5H4zM11 9v4M9 13h4v7H9z\"/>">>;
path(search) -> <<"<circle cx=\"11\" cy=\"11\" r=\"7\"/><path d=\"m20 20-3.5-3.5\"/>">>;
path(replace) -> <<"<path d=\"M4 7h12l-3-3M20 17H8l3 3\"/>">>;
path(table) -> <<"<rect x=\"3\" y=\"4\" width=\"18\" height=\"16\" rx=\"2\"/>"
                 "<path d=\"M3 10h18M3 15h18M9 4v16M15 4v16\"/>">>;
path(image) -> <<"<rect x=\"3\" y=\"4\" width=\"18\" height=\"16\" rx=\"2\"/>"
                 "<circle cx=\"9\" cy=\"10\" r=\"2\"/><path d=\"m21 17-5-5-9 8\"/>">>;
path(chart) -> <<"<path d=\"M4 20V10M10 20V4M16 20v-7M22 20H2\"/>">>;
path(shape) -> <<"<circle cx=\"8\" cy=\"8\" r=\"5\"/><rect x=\"11\" y=\"11\" width=\"10\" height=\"10\" rx=\"1\"/>">>.
