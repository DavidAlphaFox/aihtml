%% @doc Demos of the gantt chart (aihtml_gantt), shown on
%% /components/gantt. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
%%
%% The module is also the action module of the edit demo, which reports
%% what the postback received.
-module(aihtml_example_demo_gantt).
-behaviour(aihtml_action).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0, action/4]).
-export([gantt_basic/0, gantt_rows/0, gantt_edit/0, gantt_compact/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => gantt, title => <<"Gantt">>,
       summary => <<"甘特图：任务条、分组行、进度、依赖连线，拖拽调整排期并通知服务端。"/utf8>>,
       demos => [{<<"任务、进度与依赖"/utf8>>, gantt_basic},
                 {<<"分组行、折叠与汇总条"/utf8>>, gantt_rows},
                 {<<"拖拽改期，服务端收到改动"/utf8>>, gantt_edit},
                 {<<"紧凑尺寸、不画依赖线"/utf8>>, gantt_compact}]}].

%%%===================================================================
%%% Gantt
%%%===================================================================

-spec gantt_basic() -> aihtml:html().
gantt_basic() ->
    ah_gantt([#{id => research, name => <<"Research">>, start => <<"2026-09-07">>,
                'end' => <<"2026-09-14">>, progress => 100},
              #{id => design, name => <<"Design">>, start => <<"2026-09-14">>,
                'end' => <<"2026-09-24">>, progress => 70, dependencies => [research]},
              #{id => build, name => <<"Build">>, start => <<"2026-09-24">>,
                'end' => <<"2026-10-14">>, progress => 20, dependencies => [design],
                color => <<"var(--ah-color-success)">>},
              #{id => launch, name => <<"Launch">>, start => <<"2026-10-14">>,
                'end' => <<"2026-10-16">>, dependencies => [build],
                color => <<"var(--ah-color-warning)">>}],
             [], [{height, 260}, {sidebar_width, 180}]).

-spec gantt_rows() -> aihtml:html().
gantt_rows() ->
    ah_gantt(project_tasks(), [editable],
             [{rows, project_rows()}, {collapsed, [<<"testing">>]}, {height, 440},
              {sidebar_width, 200},
              {labels, #{task => <<"任务"/utf8>>, tasks => <<"{n} 项任务"/utf8>>}}]).

%% Dragging a bar calls action(task_changed, ...) below.
-spec gantt_edit() -> aihtml:html().
gantt_edit() ->
    ah_div([ah_gantt(project_tasks(), [editable],
                     [{rows, project_rows()}, {height, 300}, {sidebar_width, 200},
                      on('ah:task-change', {?MODULE, task_changed, #{}})]),
            ah_p(<<"拖动任务条或它的两端；选中任务条后可用方向键调整。"/utf8>>,
                 [<<"text-sm text-muted mt-2">>], [{id, <<"gantt-log">>}])], [], []).

-spec gantt_compact() -> aihtml:html().
gantt_compact() ->
    ah_gantt([#{id => T, name => N, start => S, 'end' => E}
              || {T, N, S, E} <- [{a, <<"Kick-off">>, <<"2026-09-28">>, <<"2026-09-29">>},
                                  {b, <<"Survey">>, <<"2026-09-29">>, <<"2026-10-06">>},
                                  {c, <<"Report">>, <<"2026-10-05">>, <<"2026-10-09">>}]],
             [no_dependencies], [{height, 180}, {column_width, 32}, {row_height, 30},
                                 {sidebar_width, 140}]).


%%%===================================================================
%%% Actions
%%%===================================================================

-spec action(atom(), term(), aihtml_action:event(), aihtml_action:ctx()) -> ok.
action(task_changed, _Args, #{data := D}, Ctx) ->
    aihtml_action:html(Ctx, {id, <<"gantt-log">>},
                       [<<"服务端收到："/utf8>>, maps:get(<<"task">>, D), <<" "/utf8>>,
                        maps:get(<<"kind">>, D), <<" → "/utf8>>, maps:get(<<"from">>, D),
                        <<" – "/utf8>>, maps:get(<<"to">>, D), <<"，行 "/utf8>>,
                        maps:get(<<"row">>, D)]).

%%%===================================================================
%%% Data
%%%===================================================================

project_rows() ->
    [#{id => design, label => <<"设计阶段"/utf8>>},
     #{id => wire, label => <<"线框图"/utf8>>, parent => design},
     #{id => mock, label => <<"视觉稿"/utf8>>, parent => design},
     #{id => dev, label => <<"开发阶段"/utf8>>},
     #{id => fe, label => <<"前端"/utf8>>, parent => dev},
     #{id => be, label => <<"后端"/utf8>>, parent => dev},
     #{id => testing, label => <<"测试阶段"/utf8>>},
     #{id => unit, label => <<"单元测试"/utf8>>, parent => testing},
     #{id => e2e, label => <<"端到端测试"/utf8>>, parent => testing},
     #{id => launch, label => <<"上线发布"/utf8>>}].

project_tasks() ->
    [#{id => t1, row => wire, name => <<"线框图"/utf8>>, start => <<"2026-09-18">>,
       'end' => <<"2026-09-23">>, progress => 100, color => <<"var(--ah-color-info)">>},
     #{id => t2, row => mock, name => <<"视觉稿"/utf8>>, start => <<"2026-09-23">>,
       'end' => <<"2026-09-29">>, progress => 80, dependencies => [t1],
       color => <<"var(--ah-color-info)">>},
     #{id => t3, row => fe, name => <<"前端开发"/utf8>>, start => <<"2026-09-29">>,
       'end' => <<"2026-10-12">>, progress => 10, dependencies => [t2]},
     #{id => t4, row => be, name => <<"后端开发"/utf8>>, start => <<"2026-09-28">>,
       'end' => <<"2026-10-14">>, progress => 5, dependencies => [t2]},
     #{id => t5, row => unit, name => <<"单元测试"/utf8>>, start => <<"2026-10-08">>,
       'end' => <<"2026-10-16">>, dependencies => [t3], color => <<"var(--ah-color-warning)">>},
     #{id => t6, row => e2e, name => <<"端到端测试"/utf8>>, start => <<"2026-10-14">>,
       'end' => <<"2026-10-20">>, dependencies => [t4], color => <<"var(--ah-color-warning)">>},
     #{id => t7, row => launch, name => <<"上线发布"/utf8>>, start => <<"2026-10-20">>,
       'end' => <<"2026-10-22">>, dependencies => [t5, t6],
       color => <<"var(--ah-color-success)">>}].
