%% @doc Demos of the activity bar (aihtml_activity_bar), shown on
%% /components/activity_bar. Each function is one example, written the way
%% an application writes it; the docs page prints its source under it.
%%
%% The module is also the action module of the server-driven demo: the
%% activity bar that switches a side panel.
-module(aihtml_example_demo_activity_bar).
-behaviour(aihtml_action).

-include_lib("aihtml/include/aihtml.hrl").

-import(aihtml_example_fixture_layout_bars, [icon/1]).

-export([demos/0, action/4]).
-export([ab_basic/0, ab_right/0, ab_server/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => activity_bar, title => <<"ActivityBar">>,
       summary => <<"VS Code 式的竖向图标导航轨，激活项在外缘显示高亮条。"/utf8>>,
       demos => [{<<"基本用法"/utf8>>, ab_basic},
                 {<<"靠右放置、分隔线与禁用项"/utf8>>, ab_right},
                 {<<"切换时由服务端替换侧栏"/utf8>>, ab_server}]}].

%%%===================================================================
%%% ActivityBar
%%%===================================================================

-spec ab_basic() -> aihtml:html().
ab_basic() ->
    ah_div([ah_activity_bar([{files, icon(files), <<"资源管理器"/utf8>>},
                             {search, icon(search), <<"搜索"/utf8>>},
                             {git, icon(git), <<"源代码管理"/utf8>>},
                             {run, icon(run), <<"运行和调试"/utf8>>}],
                            files, [], [{name, view}]),
            ah_div(<<"侧栏内容"/utf8>>, [<<"flex-1 p-4 text-sm text-muted">>], [])],
           [<<"flex h-64 w-80 border rounded overflow-hidden">>], []).

-spec ab_right() -> aihtml:html().
ab_right() ->
    ah_div([ah_div(<<"编辑区"/utf8>>, [<<"flex-1 p-4 text-sm text-muted">>], []),
            ah_activity_bar([{outline, icon(list), <<"大纲"/utf8>>},
                             {chat, icon(chat), <<"对话"/utf8>>},
                             divider,
                             {settings, icon(settings), <<"设置"/utf8>>},
                             {ext, icon(box), <<"扩展（不可用）"/utf8>>, [{disabled, true}]}],
                            chat, [right], [])],
           [<<"flex h-64 w-80 border rounded overflow-hidden">>], []).

%% A change runs action(view, ...) below, which renders the side panel.
-spec ab_server() -> aihtml:html().
ab_server() ->
    ah_div([ah_activity_bar([{files, icon(files), <<"资源管理器"/utf8>>},
                             {search, icon(search), <<"搜索"/utf8>>},
                             {git, icon(git), <<"源代码管理"/utf8>>}],
                            files, [], [on(change, {?MODULE, view, #{}})]),
            ah_div(side_panel(<<"files">>), [<<"flex-1 p-4 text-sm">>], [{id, <<"ab-panel">>}])],
           [<<"flex h-64 w-96 border rounded overflow-hidden">>], []).

%%%===================================================================
%%% Actions
%%%===================================================================

-spec action(atom(), term(), aihtml_action:event(), aihtml_action:ctx()) -> ok.
action(view, _Args, #{value := View}, Ctx) ->
    aihtml_action:html(Ctx, {id, <<"ab-panel">>}, side_panel(View)).

%%%===================================================================
%%% Data
%%%===================================================================

side_panel(<<"files">>) -> <<"src/  test/  rebar.config  README.md">>;
side_panel(<<"search">>) -> <<"在文件中搜索…"/utf8>>;
side_panel(<<"git">>) -> <<"没有需要提交的更改"/utf8>>;
side_panel(_) -> <<>>.
