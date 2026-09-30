%% @doc Demos of the ribbon (aihtml_ribbon), shown on /components/ribbon.
%% Each function is one example, written the way an application writes it;
%% the docs page prints its source under it.
%%
%% The module is also the action module of the server-driven demo: ribbon
%% commands reported to the server.
-module(aihtml_example_demo_ribbon).
-behaviour(aihtml_action).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0, action/4]).
-export([rb_office/0, rb_basic/0, rb_positions/0, rb_collapsible/0, rb_popup/0,
         rb_colors/0, rb_server/0, rb_record/0]).

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
                 {<<"record 写法"/utf8>>, rb_record}]}].

%%%===================================================================
%%% Ribbon
%%%===================================================================

-spec rb_office() -> aihtml:html().
rb_office() ->
    ah_ribbon([#{key => home, label => <<"开始"/utf8>>,
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
    ah_ribbon([{home, <<"首页"/utf8>>, para(<<"首页内容区域"/utf8>>)},
               {edit, <<"编辑（禁用）"/utf8>>, para(<<"编辑内容区域"/utf8>>), [{disabled, true}]},
               {view, <<"视图"/utf8>>, para(<<"视图内容区域"/utf8>>)}],
              home, [], [{name, tab}]).

-spec rb_positions() -> aihtml:html().
rb_positions() ->
    ah_div([ah_ribbon([{file, <<"文件"/utf8>>, para(<<"文件内容"/utf8>>)},
                       {home, <<"首页"/utf8>>, para(<<"首页内容"/utf8>>)},
                       {help, <<"帮助"/utf8>>, para(<<"帮助内容"/utf8>>)}],
                      home, [bottom], [{height, 120}]),
            ah_ribbon([{file, <<"文件"/utf8>>, para(<<"文件内容"/utf8>>)},
                       {home, <<"首页"/utf8>>, para(<<"首页内容"/utf8>>)},
                       {data, <<"数据"/utf8>>, para(<<"数据内容"/utf8>>)},
                       {help, <<"帮助"/utf8>>, para(<<"帮助内容"/utf8>>)}],
                      home, [left], [{height, 170}])],
           [<<"flex flex-col gap-4">>], []).

-spec rb_collapsible() -> aihtml:html().
rb_collapsible() ->
    ah_div([ah_ribbon(small_tabs(), home, [collapsible, collapsed], []),
            ah_p(<<"折叠后点击选项卡，面板浮在页面上方；点击命令或页面其他位置会收起。"/utf8>>,
                 [<<"text-sm text-muted">>], [])],
           [<<"flex flex-col gap-2 pb-24">>], []).

-spec rb_popup() -> aihtml:html().
rb_popup() ->
    ah_div(ah_ribbon(small_tabs(), home, [popup], [{selection_mode, hover}]),
           [<<"pb-24">>], []).

-spec rb_colors() -> aihtml:html().
rb_colors() ->
    ah_div([ah_ribbon(small_tabs(), home, [C, A], [])
            || {C, A} <- [{primary, slide}, {success, fade}, {warning, fade}, {danger, slide}]],
           [<<"flex flex-col gap-3">>], []).

%% Clicking a command runs action(command, ...) below.
-spec rb_server() -> aihtml:html().
rb_server() ->
    ah_div([ah_ribbon(small_tabs(), home, [], [on('ah:command', {?MODULE, command, #{}})]),
            ah_span(<<"还没有执行命令"/utf8>>, [<<"text-sm text-muted">>], [{id, <<"rb-log">>}])],
           [<<"flex flex-col gap-2">>], []).

-spec rb_record() -> aihtml:html().
rb_record() ->
    ah_div([#ah_ribbon{items = small_tabs(), value = insert, color = success, collapsible = true,
                       postback = command},
            ah_span(<<"还没有执行命令"/utf8>>, [<<"text-sm text-muted">>], [{id, <<"rb-log">>}])],
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
                       [<<"服务端执行了 "/utf8>>, Cmd, State, <<"，当前选项卡 "/utf8>>, Tab]).

%%%===================================================================
%%% Helpers
%%%===================================================================

para(T) ->
    ah_p(T, [<<"m-0 text-sm text-muted">>], []).


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
