%% @doc Demos of the bar components (aihtml_layout_bars), shown on
%% /components/<name>. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
%%
%% The module is also the action module of the server-driven demos: the
%% activity bar that switches a side panel, the command palette that
%% reports its choice and the palette with server-side search.
-module(aihtml_example_demo_layout_bars).
-behaviour(aihtml_action).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0, action/4]).
-export([ab_basic/0, ab_right/0, ab_server/0,
         nav_basic/0, nav_multiple/0, nav_toggle_fade/0, nav_icons/0, nav_rich/0,
         nav_fit/0, nav_record/0,
         cmd_inline/0, cmd_palette/0, cmd_select/0, cmd_search/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => activity_bar, title => <<"ActivityBar">>,
       summary => <<"VS Code 式的竖向图标导航轨，激活项在外缘显示高亮条。"/utf8>>,
       demos => [{<<"基本用法"/utf8>>, ab_basic},
                 {<<"靠右放置、分隔线与禁用项"/utf8>>, ab_right},
                 {<<"切换时由服务端替换侧栏"/utf8>>, ab_server}]},
     #{component => navigationbar, title => <<"NavigationBar">>,
       summary => <<"可折叠的分节导航栏（手风琴），支持单开、多开、动画和键盘操作。"/utf8>>,
       demos => [{<<"单项展开（默认）"/utf8>>, nav_basic},
                 {<<"多项展开"/utf8>>, nav_multiple},
                 {<<"切换模式与淡入淡出"/utf8>>, nav_toggle_fade},
                 {<<"箭头居左、加减号图标"/utf8>>, nav_icons},
                 {<<"结构化标题、操作区、禁用项"/utf8>>, nav_rich},
                 {<<"固定高度，展开项填满"/utf8>>, nav_fit},
                 {<<"record 写法"/utf8>>, nav_record}]},
     #{component => command, title => <<"Command">>,
       summary => <<"命令面板：输入过滤、分组、快捷键提示和键盘导航，可作为 ⌘K 浮层。"/utf8>>,
       demos => [{<<"内嵌面板"/utf8>>, cmd_inline},
                 {<<"⌘K 浮层"/utf8>>, cmd_palette},
                 {<<"选中后通知服务端"/utf8>>, cmd_select},
                 {<<"服务端搜索"/utf8>>, cmd_search}]}].

%%%===================================================================
%%% ActivityBar
%%%===================================================================

-spec ab_basic() -> aihtml:html().
ab_basic() ->
    'div'([activity_bar([{files, icon(files), <<"资源管理器"/utf8>>},
                         {search, icon(search), <<"搜索"/utf8>>},
                         {git, icon(git), <<"源代码管理"/utf8>>},
                         {run, icon(run), <<"运行和调试"/utf8>>}],
                        files, [], [{name, view}]),
           'div'(<<"侧栏内容"/utf8>>, [<<"flex-1 p-4 text-sm text-muted">>], [])],
          [<<"flex h-64 w-80 border rounded overflow-hidden">>], []).

-spec ab_right() -> aihtml:html().
ab_right() ->
    'div'(['div'(<<"编辑区"/utf8>>, [<<"flex-1 p-4 text-sm text-muted">>], []),
           activity_bar([{outline, icon(list), <<"大纲"/utf8>>},
                         {chat, icon(chat), <<"对话"/utf8>>},
                         divider,
                         {settings, icon(settings), <<"设置"/utf8>>},
                         {ext, icon(box), <<"扩展（不可用）"/utf8>>, [{disabled, true}]}],
                        chat, [right], [])],
          [<<"flex h-64 w-80 border rounded overflow-hidden">>], []).

%% A change runs action(view, ...) below, which renders the side panel.
-spec ab_server() -> aihtml:html().
ab_server() ->
    'div'([activity_bar([{files, icon(files), <<"资源管理器"/utf8>>},
                         {search, icon(search), <<"搜索"/utf8>>},
                         {git, icon(git), <<"源代码管理"/utf8>>}],
                        files, [], [on(change, {?MODULE, view, #{}})]),
           'div'(side_panel(<<"files">>), [<<"flex-1 p-4 text-sm">>], [{id, <<"ab-panel">>}])],
          [<<"flex h-64 w-96 border rounded overflow-hidden">>], []).

%%%===================================================================
%%% NavigationBar
%%%===================================================================

-spec nav_basic() -> aihtml:html().
nav_basic() ->
    navigationbar([{<<"快速入门"/utf8>>, para(<<"aihtml 用 Erlang 在服务端生成 HTML，浏览器端只做 jQuery 增强。"/utf8>>)},
                   {<<"安装"/utf8>>, para(<<"把 aihtml 加进 rebar.config 的依赖，页面引入 aihtml.css 和 aihtml.js。"/utf8>>)},
                   {<<"基本用法"/utf8>>, para(<<"组件函数返回 record，交给 aihtml_html:render/1 输出。"/utf8>>)}],
                  0, [<<"max-w-md">>], []).

-spec nav_multiple() -> aihtml:html().
nav_multiple() ->
    navigationbar([{<<"功能 1"/utf8>>, para(<<"多项模式下可以同时展开多个分节。"/utf8>>)},
                   {<<"功能 2"/utf8>>, para(<<"每个分节独立展开或折叠。"/utf8>>)},
                   {<<"功能 3"/utf8>>, para(<<"适合常见问题、设置面板和文档目录。"/utf8>>)}],
                  [0, 2], [<<"max-w-md">>], [{expand_mode, multiple}, {name, open}]).

-spec nav_toggle_fade() -> aihtml:html().
nav_toggle_fade() ->
    navigationbar([{<<"项目 1"/utf8>>, para(<<"切换模式最多展开一项，再次点击可以收起。"/utf8>>)},
                   {<<"项目 2"/utf8>>, para(<<"内容用淡入淡出显示和隐藏。"/utf8>>)},
                   {<<"项目 3"/utf8>>, para(<<"双击标题才切换：toggle_mode 为 dblclick。"/utf8>>)}],
                  undefined, [<<"max-w-md">>],
                  [{expand_mode, toggle}, {animation, fade}, {toggle_mode, dblclick}]).

-spec nav_icons() -> aihtml:html().
nav_icons() ->
    navigationbar([{<<"左侧箭头 1"/utf8>>, para(<<"箭头位于标题左侧。"/utf8>>)},
                   {<<"左侧箭头 2"/utf8>>, para(<<"同时给出展开和收起图标时，两者互相替换。"/utf8>>)}],
                  0, [square, <<"max-w-md">>],
                  [{arrow_position, left}, {expand_icon, <<"+">>},
                   {collapse_icon, <<"−"/utf8>>}, {expand_mode, toggle}]).

-spec nav_rich() -> aihtml:html().
nav_rich() ->
    navigationbar([#{header => #{title => <<"订单 #1024"/utf8>>, subheader => <<"3 件商品"/utf8>>,
                                 extra => <<"¥ 268.00"/utf8>>},
                     content => para(<<"收货地址：大连市中山区人民路 1 号"/utf8>>),
                     actions => [button(<<"取消"/utf8>>, undefined, [outlined, sm], []),
                                 button(<<"发货"/utf8>>, undefined, [sm], [])]},
                   #{header => #{title => <<"订单 #1025"/utf8>>, subheader => <<"已锁定"/utf8>>},
                     content => para(<<"这一项被禁用，无法展开。"/utf8>>), disabled => true},
                   #{header => #{title => <<"订单 #1026"/utf8>>, subheader => <<"1 件商品"/utf8>>,
                                 extra => <<"¥ 59.00"/utf8>>},
                     content => para(<<"等待付款。"/utf8>>)}],
                  0, [<<"max-w-lg">>], [{expand_mode, toggle}]).

-spec nav_fit() -> aihtml:html().
nav_fit() ->
    navigationbar([{<<"收件箱"/utf8>>, [para(<<"固定高度时，展开的分节填满剩下的空间，内容过长则滚动。"/utf8>>)
                                       || _ <- lists:seq(1, 8)]},
                   {<<"已发送"/utf8>>, para(<<"一次只展开一项。"/utf8>>)},
                   {<<"草稿"/utf8>>, para(<<"没有草稿。"/utf8>>)}],
                  0, [<<"max-w-sm">>], [{height, 300}]).

%% The same component as a record: options are checked field names and
%% the postback runs action(sections, ...) below on change.
-spec nav_record() -> aihtml:html().
nav_record() ->
    'div'([#ah_navigationbar{items = [{<<"基本信息"/utf8>>, para(<<"姓名、邮箱、电话"/utf8>>)},
                                      {<<"安全设置"/utf8>>, para(<<"密码、两步验证"/utf8>>)},
                                      {<<"通知"/utf8>>, para(<<"邮件、短信、站内信"/utf8>>)}],
                             value = [0], expand_mode = multiple, disable_gutters = true,
                             css = [<<"max-w-md">>], postback = sections},
           span(<<"展开的分节：0"/utf8>>, [<<"text-sm text-muted">>], [{id, <<"nav-open">>}])],
          [<<"flex flex-col gap-2">>], []).

%%%===================================================================
%%% Command
%%%===================================================================

-spec cmd_inline() -> aihtml:html().
cmd_inline() ->
    command(commands(), [<<"max-w-md">>], [{placeholder, <<"输入命令或搜索…"/utf8>>},
                                           {empty_text, <<"没有匹配的命令"/utf8>>}]).

%% Ctrl+K / ⌘K or the button opens the palette.
-spec cmd_palette() -> aihtml:html().
cmd_palette() ->
    'div'([button(<<"打开命令面板"/utf8>>, undefined, [outlined],
                  [{onclick, <<"AH.invoke('#cmd-palette', 'open')">>}]),
           span([<<"或按 "/utf8>>, kbd(<<"Ctrl K">>, [], [])], [<<"text-sm text-muted">>], []),
           command(commands(), [palette], [{id, <<"cmd-palette">>}, {hotkey, <<"k">>}])],
          [<<"flex items-center gap-3">>], []).

%% Choosing a command runs action(ran, ...) below with its value.
-spec cmd_select() -> aihtml:html().
cmd_select() ->
    'div'([command([#{value => deploy, label => <<"部署到生产环境"/utf8>>, shortcut => <<"⌘D"/utf8>>},
                    #{value => rollback, label => <<"回滚上一版本"/utf8>>},
                    #{value => logs, label => <<"查看日志"/utf8>>, description => <<"最近 1 小时"/utf8>>}],
                   [<<"max-w-sm">>], [on('ah:select', {?MODULE, ran, #{}})]),
           span(<<"还没有执行命令"/utf8>>, [<<"text-sm text-muted">>], [{id, <<"cmd-ran">>}])],
          [<<"flex flex-col gap-2">>], []).

%% Typing calls action(search, ...) below, which answers with
%% set_command_items.
-spec cmd_search() -> aihtml:html().
cmd_search() ->
    command([], [<<"max-w-sm">>], [{placeholder, <<"搜索城市，如 an"/utf8>>},
                                   {empty_text, <<"输入后开始搜索"/utf8>>},
                                   {search, {?MODULE, search, #{}}}]).

%%%===================================================================
%%% Actions
%%%===================================================================

-spec action(atom(), term(), aihtml_action:event(), aihtml_action:ctx()) -> ok.
action(view, _Args, #{value := View}, Ctx) ->
    aihtml_action:html(Ctx, {id, <<"ab-panel">>}, side_panel(View));
action(sections, _Args, #{value := Open}, Ctx) ->
    aihtml_action:html(Ctx, {id, <<"nav-open">>}, [<<"展开的分节："/utf8>>, Open]);
action(ran, _Args, #{value := Cmd}, Ctx) ->
    aihtml_action:html(Ctx, {id, <<"cmd-ran">>}, [<<"服务端执行了："/utf8>>, Cmd]);
action(search, _Args, #{value := Query} = Event, Ctx) ->
    Q = string:lowercase(Query),
    Found = [#{value => C, label => C, description => Country}
             || {C, Country} <- cities(), Q =/= <<>>,
                string:find(string:lowercase(C), Q) =/= nomatch],
    set_command_items(Ctx, Event, [#{heading => <<"城市"/utf8>>, items => lists:sublist(Found, 8)}]).

%%%===================================================================
%%% Data
%%%===================================================================

side_panel(<<"files">>) -> <<"src/  test/  rebar.config  README.md">>;
side_panel(<<"search">>) -> <<"在文件中搜索…"/utf8>>;
side_panel(<<"git">>) -> <<"没有需要提交的更改"/utf8>>;
side_panel(_) -> <<>>.

commands() ->
    [#{heading => <<"文件"/utf8>>,
       items => [#{value => new, label => <<"新建文件"/utf8>>, icon => icon(files),
                   shortcut => <<"⌘N"/utf8>>},
                 #{value => open, label => <<"打开…"/utf8>>, icon => icon(box),
                   shortcut => <<"⌘O"/utf8>>},
                 #{value => save_all, label => <<"全部保存"/utf8>>, disabled => true}]},
     #{heading => <<"视图"/utf8>>,
       items => [#{value => theme, label => <<"切换深色主题"/utf8>>, icon => icon(settings),
                   description => <<"在浅色和深色之间切换"/utf8>>},
                 #{value => sidebar, label => <<"显示侧栏"/utf8>>, icon => icon(list),
                   shortcut => <<"⌘B"/utf8>>},
                 #{value => search, label => <<"在文件中搜索"/utf8>>, icon => icon(search),
                   shortcut => <<"⇧⌘F"/utf8>>}]}].

cities() ->
    [{<<"Amsterdam">>, <<"Netherlands">>}, {<<"Athens">>, <<"Greece">>},
     {<<"Bangkok">>, <<"Thailand">>}, {<<"Beijing">>, <<"China">>},
     {<<"Dalian">>, <<"China">>}, {<<"Istanbul">>, <<"Türkiye"/utf8>>},
     {<<"Jakarta">>, <<"Indonesia">>}, {<<"London">>, <<"United Kingdom">>},
     {<<"Madrid">>, <<"Spain">>}, {<<"Nanjing">>, <<"China">>},
     {<<"Santiago">>, <<"Chile">>}, {<<"Shanghai">>, <<"China">>},
     {<<"Tokyo">>, <<"Japan">>}, {<<"Vienna">>, <<"Austria">>}].

para(T) ->
    p(T, [<<"m-0 text-sm text-muted">>], []).

%% 20px line icons (stroke follows the text colour).
icon(Name) ->
    {safe, [<<"<svg viewBox=\"0 0 24 24\" width=\"20\" height=\"20\" fill=\"none\" "
              "stroke=\"currentColor\" stroke-width=\"1.8\" stroke-linecap=\"round\" "
              "stroke-linejoin=\"round\">">>, path(Name), <<"</svg>">>]}.

path(files) -> <<"<path d=\"M14 3H6a2 2 0 0 0-2 2v14a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V9z\"/>"
                 "<path d=\"M14 3v6h6\"/>">>;
path(search) -> <<"<circle cx=\"11\" cy=\"11\" r=\"7\"/><path d=\"m20 20-3.5-3.5\"/>">>;
path(git) -> <<"<circle cx=\"6\" cy=\"6\" r=\"2.5\"/><circle cx=\"6\" cy=\"18\" r=\"2.5\"/>"
               "<circle cx=\"18\" cy=\"8\" r=\"2.5\"/><path d=\"M6 8.5v7M18 10.5c0 4-6 3-11 6\"/>">>;
path(run) -> <<"<path d=\"M7 4v16l13-8z\"/>">>;
path(list) -> <<"<path d=\"M8 6h13M8 12h13M8 18h13M3 6h.01M3 12h.01M3 18h.01\"/>">>;
path(chat) -> <<"<path d=\"M21 12a8 8 0 0 1-11.5 7.2L4 20l1-4.6A8 8 0 1 1 21 12z\"/>">>;
path(settings) -> <<"<circle cx=\"12\" cy=\"12\" r=\"3\"/><path d=\"M12 2v3M12 19v3M4.2 4.2l2.1 2.1"
                    "M17.7 17.7l2.1 2.1M2 12h3M19 12h3M4.2 19.8l2.1-2.1M17.7 6.3l2.1-2.1\"/>">>;
path(box) -> <<"<path d=\"M21 8 12 3 3 8v8l9 5 9-5z\"/><path d=\"M3 8l9 5 9-5M12 13v8\"/>">>.
