%% @doc Demos of the navigation bar (aihtml_navigationbar), shown on
%% /components/navigationbar. Each function is one example, written the
%% way an application writes it; the docs page prints its source under it.
%%
%% The module is also the action module of the record demo, whose
%% postback reports the expanded sections.
-module(aihtml_example_demo_navigationbar).
-behaviour(aihtml_action).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0, action/4]).
-export([nav_basic/0, nav_multiple/0, nav_toggle_fade/0, nav_icons/0, nav_rich/0,
         nav_fit/0, nav_record/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => navigationbar, title => <<"NavigationBar">>,
       summary => <<"可折叠的分节导航栏（手风琴），支持单开、多开、动画和键盘操作。"/utf8>>,
       demos => [{<<"单项展开（默认）"/utf8>>, nav_basic},
                 {<<"多项展开"/utf8>>, nav_multiple},
                 {<<"切换模式与淡入淡出"/utf8>>, nav_toggle_fade},
                 {<<"箭头居左、加减号图标"/utf8>>, nav_icons},
                 {<<"结构化标题、操作区、禁用项"/utf8>>, nav_rich},
                 {<<"固定高度，展开项填满"/utf8>>, nav_fit},
                 {<<"record 写法"/utf8>>, nav_record}]}].

%%%===================================================================
%%% NavigationBar
%%%===================================================================

-spec nav_basic() -> aihtml:html().
nav_basic() ->
    ah_navigationbar([{<<"快速入门"/utf8>>, para(<<"aihtml 用 Erlang 在服务端生成 HTML，浏览器端由 Stimulus 控制器增强。"/utf8>>)},
                      {<<"安装"/utf8>>, para(<<"把 aihtml 加进 rebar.config 的依赖，页面用 aihtml_page 输出，自动引入 aihtml.css 和运行时。"/utf8>>)},
                      {<<"基本用法"/utf8>>, para(<<"组件函数返回 record，交给 aihtml_html:render/1 输出。"/utf8>>)}],
                     0, [<<"max-w-md">>], []).

-spec nav_multiple() -> aihtml:html().
nav_multiple() ->
    ah_navigationbar([{<<"功能 1"/utf8>>, para(<<"多项模式下可以同时展开多个分节。"/utf8>>)},
                      {<<"功能 2"/utf8>>, para(<<"每个分节独立展开或折叠。"/utf8>>)},
                      {<<"功能 3"/utf8>>, para(<<"适合常见问题、设置面板和文档目录。"/utf8>>)}],
                     [0, 2], [<<"max-w-md">>], [{expand_mode, multiple}, {name, open}]).

-spec nav_toggle_fade() -> aihtml:html().
nav_toggle_fade() ->
    ah_navigationbar([{<<"项目 1"/utf8>>, para(<<"切换模式最多展开一项，再次点击可以收起。"/utf8>>)},
                      {<<"项目 2"/utf8>>, para(<<"内容用淡入淡出显示和隐藏。"/utf8>>)},
                      {<<"项目 3"/utf8>>, para(<<"双击标题才切换：toggle_mode 为 dblclick。"/utf8>>)}],
                     undefined, [<<"max-w-md">>],
                     [{expand_mode, toggle}, {animation, fade}, {toggle_mode, dblclick}]).

-spec nav_icons() -> aihtml:html().
nav_icons() ->
    ah_navigationbar([{<<"左侧箭头 1"/utf8>>, para(<<"箭头位于标题左侧。"/utf8>>)},
                      {<<"左侧箭头 2"/utf8>>, para(<<"同时给出展开和收起图标时，两者互相替换。"/utf8>>)}],
                     0, [square, <<"max-w-md">>],
                     [{arrow_position, left}, {expand_icon, <<"+">>},
                      {collapse_icon, <<"−"/utf8>>}, {expand_mode, toggle}]).

-spec nav_rich() -> aihtml:html().
nav_rich() ->
    ah_navigationbar([#{header => #{title => <<"订单 #1024"/utf8>>, subheader => <<"3 件商品"/utf8>>,
                                    extra => <<"¥ 268.00"/utf8>>},
                        content => para(<<"收货地址：大连市中山区人民路 1 号"/utf8>>),
                        actions => [ah_button(<<"取消"/utf8>>, undefined, [outlined, sm], []),
                                    ah_button(<<"发货"/utf8>>, undefined, [sm], [])]},
                      #{header => #{title => <<"订单 #1025"/utf8>>, subheader => <<"已锁定"/utf8>>},
                        content => para(<<"这一项被禁用，无法展开。"/utf8>>), disabled => true},
                      #{header => #{title => <<"订单 #1026"/utf8>>, subheader => <<"1 件商品"/utf8>>,
                                    extra => <<"¥ 59.00"/utf8>>},
                        content => para(<<"等待付款。"/utf8>>)}],
                     0, [<<"max-w-lg">>], [{expand_mode, toggle}]).

-spec nav_fit() -> aihtml:html().
nav_fit() ->
    ah_navigationbar([{<<"收件箱"/utf8>>, [para(<<"固定高度时，展开的分节填满剩下的空间，内容过长则滚动。"/utf8>>)
                                          || _ <- lists:seq(1, 8)]},
                      {<<"已发送"/utf8>>, para(<<"一次只展开一项。"/utf8>>)},
                      {<<"草稿"/utf8>>, para(<<"没有草稿。"/utf8>>)}],
                     0, [<<"max-w-sm">>], [{height, 300}]).

%% The same component as a record: options are checked field names and
%% the postback runs action(sections, ...) below on change.
-spec nav_record() -> aihtml:html().
nav_record() ->
    ah_div([#ah_navigationbar{items = [{<<"基本信息"/utf8>>, para(<<"姓名、邮箱、电话"/utf8>>)},
                                       {<<"安全设置"/utf8>>, para(<<"密码、两步验证"/utf8>>)},
                                       {<<"通知"/utf8>>, para(<<"邮件、短信、站内信"/utf8>>)}],
                              value = [0], expand_mode = multiple, disable_gutters = true,
                              css = [<<"max-w-md">>], postback = sections},
            ah_span(<<"展开的分节：0"/utf8>>, [<<"text-sm text-muted">>], [{id, <<"nav-open">>}])],
           [<<"flex flex-col gap-2">>], []).

%%%===================================================================
%%% Actions
%%%===================================================================

-spec action(atom(), term(), aihtml_action:event(), aihtml_action:ctx()) -> ok.
action(sections, _Args, #{value := Open}, Ctx) ->
    aihtml_action:html(Ctx, {id, <<"nav-open">>}, [<<"展开的分节："/utf8>>, Open]).

%%%===================================================================
%%% Data
%%%===================================================================

para(T) ->
    ah_p(T, [<<"m-0 text-sm text-muted">>], []).
