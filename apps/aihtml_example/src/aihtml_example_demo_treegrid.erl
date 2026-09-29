%% @doc Demos of the tree grid (aihtml_treegrid), shown on
%% /components/treegrid. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
%%
%% The module is also the action module of the demos that talk to the
%% server: the lazy tree grid's children and the selection.
-module(aihtml_example_demo_treegrid).
-behaviour(aihtml_action).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0, action/4]).
-export([tg_nested/0, tg_flat/0, tg_selection/0, tg_sorted/0, tg_lazy/0, tg_change/0,
         tg_record/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => treegrid, title => <<"TreeGrid">>,
       summary => <<"带层级缩进、可展开折叠的表格，同级排序、行选择、键盘导航，子节点可由服务端懒加载。"/utf8>>,
       demos => [{<<"嵌套数据"/utf8>>, tg_nested},
                 {<<"扁平数据（parent_id）"/utf8>>, tg_flat},
                 {<<"多选与复选框"/utf8>>, tg_selection},
                 {<<"排序与自定义单元格"/utf8>>, tg_sorted},
                 {<<"服务端懒加载子节点"/utf8>>, tg_lazy},
                 {<<"选中后通知服务端"/utf8>>, tg_change},
                 {<<"record 写法"/utf8>>, tg_record}]}].

%%%===================================================================
%%% TreeGrid
%%%===================================================================

-spec tg_nested() -> aihtml:html().
tg_nested() ->
    treegrid(dept_columns(), departments(), [], [{expanded, [1, 10]}]).

-spec tg_flat() -> aihtml:html().
tg_flat() ->
    Staff = [#{id => 100, name => <<"CEO 办公室"/utf8>>, role => <<"管理层"/utf8>>},
             #{id => 101, name => <<"CTO">>, role => <<"管理层"/utf8>>, parent_id => 100},
             #{id => 102, name => <<"首席架构师"/utf8>>, role => <<"技术"/utf8>>, parent_id => 101},
             #{id => 103, name => <<"高级工程师 A"/utf8>>, role => <<"技术"/utf8>>, parent_id => 102},
             #{id => 104, name => <<"高级工程师 B"/utf8>>, role => <<"技术"/utf8>>, parent_id => 102},
             #{id => 110, name => <<"CFO">>, role => <<"管理层"/utf8>>, parent_id => 100},
             #{id => 111, name => <<"财务分析师"/utf8>>, role => <<"财务"/utf8>>, parent_id => 110},
             #{id => 120, name => <<"COO">>, role => <<"管理层"/utf8>>, parent_id => 100},
             #{id => 121, name => <<"运营经理"/utf8>>, role => <<"运营"/utf8>>, parent_id => 120}],
    treegrid([#{field => name, title => <<"姓名/职位"/utf8>>, width => 260},
              #{field => role, title => <<"角色"/utf8>>}],
             Staff, [], [{expanded, all}, {height, 300}]).

-spec tg_selection() -> aihtml:html().
tg_selection() ->
    'div'([treegrid(dept_columns(), departments(), [],
                    [{selection_mode, multiple}, {expanded, all}, {value, [3, 4]}, {height, 260}]),
           treegrid(dept_columns(), departments(), [],
                    [{selection_mode, checkbox}, {expanded, [1]}, {name, teams}])],
          [<<"grid gap-4">>], []).

-spec tg_sorted() -> aihtml:html().
tg_sorted() ->
    Status = fun(<<"招聘中"/utf8>> = S, _) -> chip(S, [soft, warning, small], []);
                (S, _) -> chip(S, [soft, success, small], [])
             end,
    treegrid([#{field => name, title => <<"部门/团队"/utf8>>, width => 240},
              #{field => budget, title => <<"预算"/utf8>>, align => right, type => number,
                render => fun(V, _) -> [<<"¥"/utf8>>, thousands(V)] end},
              #{field => headcount, title => <<"人数"/utf8>>, align => center},
              #{field => status, title => <<"状态"/utf8>>, render => Status, sortable => false}],
             departments(), [], [{expanded, all}, {sort, {budget, desc}}, {alt_rows, false}]).

%% Expanding a lazy row runs action(folder, ...) below, which answers with
%% treegrid_children.
-spec tg_lazy() -> aihtml:html().
tg_lazy() ->
    treegrid(file_columns(),
             [#{path => <<"/src">>, name => <<"src">>, size => <<"—"/utf8>>, children => lazy},
              #{path => <<"/priv">>, name => <<"priv">>, size => <<"—"/utf8>>, children => lazy},
              #{path => <<"/README.md">>, name => <<"README.md">>, size => <<"4 KB">>}],
             [], [{key_field, path}, {load, {?MODULE, folder, #{}}}]).

-spec tg_change() -> aihtml:html().
tg_change() ->
    'div'([treegrid(dept_columns(), departments(), [],
                    [{expanded, [1]}, on(change, {?MODULE, team_picked, #{}})]),
           p(<<"点一行试试"/utf8>>, [<<"text-sm text-muted mt-2">>], [{id, <<"team-picked">>}])],
          [], []).

%% The postback runs action(team_picked, ...) below on change.
-spec tg_record() -> aihtml:html().
tg_record() ->
    #ah_treegrid{columns = [#{field => name, title => <<"部门"/utf8>>, width => 220},
                            #{field => headcount, title => <<"人数"/utf8>>, align => right}],
                 items = departments(), expanded = all, selection_mode = multiple,
                 sort = {headcount, desc}, indent = 20, height = 280,
                 postback = team_picked}.

dept_columns() ->
    [#{field => name, title => <<"部门/团队"/utf8>>, width => 240},
     #{field => budget, title => <<"预算"/utf8>>, align => right, type => number},
     #{field => headcount, title => <<"人数"/utf8>>, align => center},
     #{field => status, title => <<"状态"/utf8>>}].

departments() ->
    [#{id => 1, name => <<"工程部"/utf8>>, budget => 500000, headcount => 25, status => <<"活跃"/utf8>>,
       children => [#{id => 2, name => <<"前端组"/utf8>>, budget => 200000, headcount => 10,
                      status => <<"活跃"/utf8>>,
                      children => [#{id => 3, name => <<"React 小组"/utf8>>, budget => 100000,
                                     headcount => 5, status => <<"活跃"/utf8>>},
                                   #{id => 4, name => <<"Vue 小组"/utf8>>, budget => 100000,
                                     headcount => 5, status => <<"活跃"/utf8>>}]},
                    #{id => 5, name => <<"后端组"/utf8>>, budget => 200000, headcount => 10,
                      status => <<"活跃"/utf8>>,
                      children => [#{id => 6, name => <<"Java 小组"/utf8>>, budget => 120000,
                                     headcount => 6, status => <<"活跃"/utf8>>},
                                   #{id => 7, name => <<"Go 小组"/utf8>>, budget => 80000,
                                     headcount => 4, status => <<"招聘中"/utf8>>}]},
                    #{id => 8, name => <<"测试组"/utf8>>, budget => 100000, headcount => 5,
                      status => <<"活跃"/utf8>>}]},
     #{id => 10, name => <<"市场部"/utf8>>, budget => 200000, headcount => 12, status => <<"活跃"/utf8>>,
       children => [#{id => 11, name => <<"品牌推广"/utf8>>, budget => 100000, headcount => 6,
                      status => <<"活跃"/utf8>>},
                    #{id => 12, name => <<"数字营销"/utf8>>, budget => 100000, headcount => 6,
                      status => <<"招聘中"/utf8>>}]},
     #{id => 20, name => <<"人事部"/utf8>>, budget => 150000, headcount => 8, status => <<"活跃"/utf8>>,
       children => [#{id => 21, name => <<"招聘"/utf8>>, budget => 80000, headcount => 4,
                      status => <<"活跃"/utf8>>},
                    #{id => 22, name => <<"培训"/utf8>>, budget => 70000, headcount => 4,
                      status => <<"活跃"/utf8>>}]},
     #{id => 30, name => <<"财务部"/utf8>>, budget => 120000, headcount => 6, status => <<"活跃"/utf8>>}].

file_columns() ->
    [#{field => name, title => <<"名称"/utf8>>, width => 260},
     #{field => size, title => <<"大小"/utf8>>, align => right, width => 120}].

%% A folder's entries, as a file system or a database would list them.
folder(<<"/src">>) ->
    [#{path => <<"/src/app.erl">>, name => <<"app.erl">>, size => <<"2 KB">>},
     #{path => <<"/src/web">>, name => <<"web">>, size => <<"—"/utf8>>, children => lazy},
     #{path => <<"/src/db.erl">>, name => <<"db.erl">>, size => <<"9 KB">>}];
folder(<<"/src/web">>) ->
    [#{path => <<"/src/web/router.erl">>, name => <<"router.erl">>, size => <<"3 KB">>},
     #{path => <<"/src/web/views">>, name => <<"views（空目录）"/utf8>>, size => <<"—"/utf8>>,
       children => lazy}];
folder(<<"/priv">>) ->
    [#{path => <<"/priv/static">>, name => <<"static">>, size => <<"—"/utf8>>,
       children => [#{path => <<"/priv/static/app.css">>, name => <<"app.css">>, size => <<"12 KB">>}]}];
folder(_) -> [].

thousands(N) when is_integer(N) ->
    S = integer_to_list(N),
    L = length(S),
    list_to_binary(lists:append([[C | [$, || I > 1, (I - 1) rem 3 =:= 0]]
                                 || {C, I} <- lists:zip(S, lists:seq(L, 1, -1))]));
thousands(_) -> <<>>.

%%%===================================================================
%%% Actions
%%%===================================================================

-spec action(atom(), term(), aihtml_action:event(), aihtml_action:ctx()) -> ok.
action(folder, _Args, #{data := #{<<"key">> := Path}} = Event, Ctx) ->
    treegrid_children(Ctx, Event,
                      treegrid(file_columns(), folder(Path), [],
                               [{key_field, path}, {load, {?MODULE, folder, #{}}}]));
action(team_picked, _Args, #{value := Keys}, Ctx) ->
    aihtml_action:html(Ctx, {id, <<"team-picked">>}, [<<"服务端收到的选中行："/utf8>>, Keys]).
