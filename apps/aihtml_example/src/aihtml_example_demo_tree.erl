%% @doc Demos of the tree (aihtml_tree), shown on /components/tree. Each
%% function is one example, written the way an application writes it; the
%% docs page prints its source under it.
%%
%% The module is also the action module of the demos that talk to the
%% server: the lazy tree's children and the selection of a tree.
-module(aihtml_example_demo_tree).
-behaviour(aihtml_action).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0, action/4]).
-export([tree_basic/0, tree_selected/0, tree_icons/0, tree_dblclick/0, tree_lazy/0,
         tree_change/0, tree_record/0]).

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
                 {<<"record 写法"/utf8>>, tree_record}]}].

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
%%% Actions
%%%===================================================================

-spec action(atom(), term(), aihtml_action:event(), aihtml_action:ctx()) -> ok.
action(children, _Args, #{data := #{<<"value">> := Region}} = Event, Ctx) ->
    set_children(Ctx, Event, regions(Region));
action(file_picked, _Args, #{value := Value}, Ctx) ->
    aihtml_action:html(Ctx, {id, <<"file-picked">>}, [<<"服务端收到："/utf8>>, Value]).

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

box(Tree) ->
    'div'(Tree, [<<"w-80 rounded-md border border-border p-2">>], []).

row(Children) ->
    'div'(Children, [<<"flex flex-wrap items-start gap-4">>], []).
