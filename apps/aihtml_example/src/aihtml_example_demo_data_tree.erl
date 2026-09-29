%% @doc Demos of the tree and text view components (aihtml_data_tree),
%% shown on /components/<name>. Each function is one example, written the
%% way an application writes it; the docs page prints its source under it.
%%
%% The module is also the action module of the demos that talk to the
%% server: the lazy tree's children, the selection of a tree, the route of
%% a nav tree and the day of a heatmap.
-module(aihtml_example_demo_data_tree).
-behaviour(aihtml_action).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0, action/4]).
-export([tree_basic/0, tree_selected/0, tree_icons/0, tree_dblclick/0, tree_lazy/0,
         tree_change/0, tree_record/0,
         nav_basic/0, nav_plain/0, nav_change/0,
         diff_unified/0, diff_split/0, diff_word/0, diff_plain/0,
         heatmap_basic/0, heatmap_custom/0, heatmap_select/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => tree, title => <<"Tree">>,
       summary => <<"可展开折叠的层级列表，支持单选、键盘导航和服务端懒加载子节点。"/utf8>>,
       demos => [{<<"文件树"/utf8>>, tree_basic},
                 {<<"选中值与表单字段"/utf8>>, tree_selected},
                 {<<"图标与禁用项"/utf8>>, tree_icons},
                 {<<"双击展开、无动画、整体禁用"/utf8>>, tree_dblclick},
                 {<<"服务端懒加载子节点"/utf8>>, tree_lazy},
                 {<<"选中后通知服务端"/utf8>>, tree_change},
                 {<<"record 写法"/utf8>>, tree_record}]},
     #{component => nav_tree, title => <<"NavTree">>,
       summary => <<"分组的树状侧边导航，可折叠子菜单带圆角连接线，当前路由高亮。"/utf8>>,
       demos => [{<<"分组、图标与当前路由"/utf8>>, nav_basic},
                 {<<"不分组、外部链接"/utf8>>, nav_plain},
                 {<<"切换路由时通知服务端"/utf8>>, nav_change}]},
     #{component => diff, title => <<"Diff">>,
       summary => <<"两段文本的差异视图，由服务端计算，支持单栏、左右分栏和词级对比。"/utf8>>,
       demos => [{<<"单栏、行号与统计"/utf8>>, diff_unified},
                 {<<"左右分栏"/utf8>>, diff_split},
                 {<<"词级差异"/utf8>>, diff_word},
                 {<<"最简用法"/utf8>>, diff_plain}]},
     #{component => heatmap_calendar, title => <<"HeatmapCalendar">>,
       summary => <<"GitHub 风格的贡献热力图，按日期铺格子、按数值深浅着色。"/utf8>>,
       demos => [{<<"最近一年"/utf8>>, heatmap_basic},
                 {<<"中文标签、阈值与提示"/utf8>>, heatmap_custom},
                 {<<"点击日期通知服务端"/utf8>>, heatmap_select}]}].

%%%===================================================================
%%% Tree
%%%===================================================================

-spec tree_basic() -> aihtml:html().
tree_basic() ->
    box(tree(files(), undefined, [], [{aria_label, <<"文件"/utf8>>}])).

-spec tree_selected() -> aihtml:html().
tree_selected() ->
    box(tree(files(), <<"年度总结"/utf8>>, [], [{name, file}])).

-spec tree_icons() -> aihtml:html().
tree_icons() ->
    Folder = <<"📁"/utf8>>,
    Doc = <<"📄"/utf8>>,
    box(tree([#{label => <<"src">>, icon => Folder, expanded => true,
                items => [#{label => <<"app.erl">>, icon => Doc},
                          #{label => <<"sup.erl">>, icon => Doc},
                          #{label => <<"legacy.erl">>, icon => Doc, disabled => true}]},
              #{label => <<"priv">>, icon => Folder,
                items => [#{label => <<"index.html">>, icon => Doc}]},
              #{label => <<"rebar.config">>, icon => Doc}],
             <<"app.erl">>, [], [])).

-spec tree_dblclick() -> aihtml:html().
tree_dblclick() ->
    Items = [{a, <<"根节点 A"/utf8>>,
              [{a1, <<"子节点 A1"/utf8>>, [{a1a, <<"叶子 A1a"/utf8>>}, {a1b, <<"叶子 A1b"/utf8>>}]},
               {a2, <<"子节点 A2"/utf8>>}]},
             {b, <<"根节点 B"/utf8>>, [{b1, <<"子节点 B1"/utf8>>}]}],
    row([box(tree(Items, undefined, [], [{toggle_mode, dblclick}, {animation, none}])),
         box(tree(Items, a2, [disabled], []))]).

%% Expanding a lazy node runs action(children, ...) below, which answers
%% with set_children.
-spec tree_lazy() -> aihtml:html().
tree_lazy() ->
    box(tree([#{label => <<"华北"/utf8>>, value => <<"north">>, lazy => true},
              #{label => <<"华东"/utf8>>, value => <<"east">>, lazy => true},
              #{label => <<"海外（无下级）"/utf8>>, value => <<"abroad">>, lazy => true}],
             undefined, [], [{load, {?MODULE, children, #{}}}])).

-spec tree_change() -> aihtml:html().
tree_change() ->
    row([box(tree(files(), undefined, [], [on(change, {?MODULE, file_picked, #{}})])),
         span(<<"还没有选择"/utf8>>, [<<"text-sm text-muted">>], [{id, <<"file-picked">>}])]).

%% The same component as a record: options are checked field names, and
%% the postback runs action(file_picked, ...) below on change.
-spec tree_record() -> aihtml:html().
tree_record() ->
    row([box(#ah_tree{items = files(), value = <<"音乐"/utf8>>, name = file,
                      toggle_mode = click, animation = slide,
                      postback = file_picked}),
         span(<<"还没有选择"/utf8>>, [<<"text-sm text-muted">>], [{id, <<"file-picked">>}])]).

%%%===================================================================
%%% NavTree
%%%===================================================================

-spec nav_basic() -> aihtml:html().
nav_basic() ->
    side(nav_tree(nav_groups(), <<"user/cards">>, [], [])).

-spec nav_plain() -> aihtml:html().
nav_plain() ->
    side(nav_tree([{<<"概览"/utf8>>, <<"overview">>},
                   #{label => <<"设置"/utf8>>,
                     items => [{<<"账号"/utf8>>, <<"settings/account">>},
                               {<<"通知"/utf8>>, <<"settings/notify">>},
                               #{label => <<"安全"/utf8>>,
                                 items => [{<<"密码"/utf8>>, <<"settings/security/password">>},
                                           {<<"两步验证"/utf8>>, <<"settings/security/2fa">>}]}]},
                   #{label => <<"帮助文档"/utf8>>, href => <<"https://example.com/docs">>}],
                  <<"settings/security/2fa">>, [], [])).

-spec nav_change() -> aihtml:html().
nav_change() ->
    row([side(nav_tree(nav_groups(), <<"dashboard">>, [],
                       [on(change, {?MODULE, route_picked, #{}})])),
         span(<<"点击左侧链接"/utf8>>, [<<"text-sm text-muted">>], [{id, <<"route-picked">>}])]).

%%%===================================================================
%%% Diff
%%%===================================================================

-spec diff_unified() -> aihtml:html().
diff_unified() ->
    diff(old_code(), new_code(), [line_numbers, stats], []).

-spec diff_split() -> aihtml:html().
diff_split() ->
    diff(old_code(), new_code(), [split, stats], []).

-spec diff_word() -> aihtml:html().
diff_word() ->
    diff(<<"The quick brown fox jumps over the lazy dog. 今天天气晴朗，适合出门散步。"/utf8>>,
         <<"The quick red fox leaps over the sleepy dog. 今天天气多云，适合在家读书。"/utf8>>,
         [word], []).

-spec diff_plain() -> aihtml:html().
diff_plain() ->
    diff(<<"apple\nbanana\ncherry\n">>, <<"apple\nblueberry\ncherry\ndate\n">>, [], []).

%%%===================================================================
%%% HeatmapCalendar
%%%===================================================================

-spec heatmap_basic() -> aihtml:html().
heatmap_basic() ->
    heatmap_calendar(activity(), [], [{end_date, {2026, 9, 29}}]).

-spec heatmap_custom() -> aihtml:html().
heatmap_custom() ->
    heatmap_calendar(activity(), [],
                     [{months, 6}, {end_date, {2026, 9, 29}}, {thresholds, [0, 2, 5, 8]},
                      {weekday_labels, [<<>>, <<"一"/utf8>>, <<>>, <<"三"/utf8>>, <<>>,
                                        <<"五"/utf8>>, <<>>]},
                      {month_labels, [<<(integer_to_binary(M))/binary, "月"/utf8>>
                                      || M <- lists:seq(1, 12)]},
                      {legend, {<<"少"/utf8>>, <<"多"/utf8>>}},
                      {tooltip, <<"{date}：{value} 次提交"/utf8>>}]).

-spec heatmap_select() -> aihtml:html().
heatmap_select() ->
    'div'([#ah_heatmap_calendar{data = activity(), months = 4, end_date = {2026, 9, 29},
                                postback = day_picked},
           p(<<"点击任意一天"/utf8>>, [<<"text-sm text-muted mt-2">>], [{id, <<"day-picked">>}])],
          [], []).

%%%===================================================================
%%% Actions
%%%===================================================================

-spec action(atom(), term(), aihtml_action:event(), aihtml_action:ctx()) -> ok.
action(children, _Args, #{data := #{<<"value">> := Region}} = Event, Ctx) ->
    set_children(Ctx, Event, regions(Region));
action(file_picked, _Args, #{value := Value}, Ctx) ->
    aihtml_action:html(Ctx, {id, <<"file-picked">>}, [<<"服务端收到："/utf8>>, Value]);
action(route_picked, _Args, #{value := Route}, Ctx) ->
    aihtml_action:html(Ctx, {id, <<"route-picked">>}, [<<"当前路由："/utf8>>, Route]);
action(day_picked, _Args, #{value := Day}, Ctx) ->
    aihtml_action:html(Ctx, {id, <<"day-picked">>}, [<<"服务端收到："/utf8>>, Day]).

%%%===================================================================
%%% Data
%%%===================================================================

files() ->
    [#{label => <<"文档"/utf8>>, expanded => true,
       items => [#{label => <<"工作"/utf8>>,
                   items => [#{label => <<"报告"/utf8>>,
                               items => [<<"第一季度报告"/utf8>>, <<"第二季度报告"/utf8>>,
                                         <<"年度总结"/utf8>>]},
                             <<"演示文稿"/utf8>>]},
                 #{label => <<"个人"/utf8>>, items => [<<"照片"/utf8>>, <<"音乐"/utf8>>]}]},
     #{label => <<"下载"/utf8>>, expanded => true,
       items => [<<"软件"/utf8>>, <<"归档"/utf8>>]},
     #{label => <<"桌面"/utf8>>, items => [<<"快捷方式"/utf8>>, <<"壁纸"/utf8>>]},
     #{label => <<"回收站"/utf8>>, disabled => true}].

regions(<<"north">>) ->
    [<<"北京"/utf8>>, <<"天津"/utf8>>,
     #{label => <<"河北"/utf8>>, value => <<"hebei">>, lazy => true}];
regions(<<"hebei">>) -> [<<"石家庄"/utf8>>, <<"保定"/utf8>>, <<"唐山"/utf8>>];
regions(<<"east">>) -> [<<"上海"/utf8>>, <<"江苏"/utf8>>, <<"浙江"/utf8>>];
regions(_) -> [].

nav_groups() ->
    [#{group => <<"OVERVIEW">>,
       items => [#{label => <<"Dashboard">>, icon => icon(home), route => <<"dashboard">>},
                 #{label => <<"Analytics">>, icon => icon(chart), route => <<"analytics">>}]},
     #{group => <<"MANAGEMENT">>,
       items => [#{label => <<"User">>, icon => icon(user),
                   items => [{<<"Profile">>, <<"user/profile">>}, {<<"Cards">>, <<"user/cards">>},
                             {<<"List">>, <<"user/list">>}, {<<"Account">>, <<"user/account">>}]},
                 #{label => <<"Invoice">>, icon => icon(file),
                   items => [{<<"List">>, <<"invoice/list">>},
                             {<<"Details">>, <<"invoice/details">>},
                             {<<"Create">>, <<"invoice/create">>}]}]}].

icon(Name) ->
    Paths = #{home => <<"<path d=\"m3 9 9-7 9 7v11a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2z\"/>"
                        "<polyline points=\"9 22 9 12 15 12 15 22\"/>">>,
              chart => <<"<path d=\"M3 3v18h18\"/><path d=\"M18 17V9\"/><path d=\"M13 17V5\"/>"
                         "<path d=\"M8 17v-3\"/>">>,
              user => <<"<circle cx=\"12\" cy=\"8\" r=\"5\"/><path d=\"M20 21a8 8 0 0 0-16 0\"/>">>,
              file => <<"<path d=\"M14 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8z\"/>"
                        "<path d=\"M14 2v6h6\"/>">>},
    {safe, [<<"<svg width=\"20\" height=\"20\" viewBox=\"0 0 24 24\" fill=\"none\" "
              "stroke=\"currentColor\" stroke-width=\"1.8\" stroke-linecap=\"round\" "
              "stroke-linejoin=\"round\">">>, maps:get(Name, Paths), <<"</svg>">>]}.

old_code() ->
    <<"function greet(name) {\n"
      "  console.log('hi')\n"
      "  return name\n"
      "}\n"
      "\n"
      "export default greet\n">>.

new_code() ->
    <<"function greet(name) {\n"
      "  console.log('hello', name)\n"
      "  return name.trim()\n"
      "}\n"
      "\n"
      "greet.version = 2\n"
      "export default greet\n">>.

%% A year of made-up commit counts, the same on every render.
activity() ->
    Last = calendar:date_to_gregorian_days(2026, 9, 29),
    maps:from_list([{calendar:gregorian_days_to_date(D), N}
                    || D <- lists:seq(Last - 380, Last),
                       N <- [max(0, erlang:phash2(D, 14) - 5)], N > 0]).

box(Tree) ->
    'div'(Tree, [<<"w-80 rounded-md border border-border p-2">>], []).

side(Nav) ->
    'div'(Nav, [<<"w-64 rounded-md border border-border p-3 bg-surface">>], []).

row(Children) ->
    'div'(Children, [<<"flex flex-wrap items-start gap-4">>], []).
