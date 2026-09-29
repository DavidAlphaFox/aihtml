%% @doc Demos of the relation graph (aihtml_relation_graph), shown on
%% /components/relation_graph. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
%%
%% The module is also the action module of the demos that talk to the
%% server: a selected graph node.
-module(aihtml_example_demo_relation_graph).
-behaviour(aihtml_action).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0, action/4]).
-export([graph_force/0, graph_circular/0, graph_tree/0, graph_fixed/0, graph_states/0,
         graph_select/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => relation_graph, title => <<"RelationGraph">>,
      summary => <<"关系图：力导向、环形、树形或固定坐标布局，支持分类配色、选中节点和详情卡。"/utf8>>,
      demos => [{<<"力导向布局、分类与详情卡"/utf8>>, graph_force},
                {<<"环形布局、有向边与边标签"/utf8>>, graph_circular},
                {<<"树形布局"/utf8>>, graph_tree},
                {<<"固定坐标、圆角矩形节点"/utf8>>, graph_fixed},
                {<<"加载中、空数据与出错"/utf8>>, graph_states},
                {<<"选中节点通知服务端"/utf8>>, graph_select}]}].

%%%===================================================================
%%% RelationGraph
%%%===================================================================

-spec graph_force() -> aihtml:html().
graph_force() ->
    relation_graph(team(), [],
                   [{selected, <<"li">>},
                    {details, #{<<"li">> => [strong(<<"李雷"/utf8>>, [], []),
                                             p(<<"后端负责人，维护订单与支付服务。"/utf8>>, [], [])],
                                <<"han">> => p(<<"韩梅梅：前端负责人。"/utf8>>, [], [])}}]).

-spec graph_circular() -> aihtml:html().
graph_circular() ->
    relation_graph(#{nodes => [<<"网关"/utf8>>, <<"订单"/utf8>>, <<"支付"/utf8>>,
                               <<"库存"/utf8>>, <<"通知"/utf8>>, <<"用户"/utf8>>],
                     edges => [{<<"网关"/utf8>>, <<"订单"/utf8>>, <<"HTTP">>},
                               {<<"网关"/utf8>>, <<"用户"/utf8>>, <<"HTTP">>},
                               {<<"订单"/utf8>>, <<"支付"/utf8>>, <<"RPC">>},
                               {<<"订单"/utf8>>, <<"库存"/utf8>>, <<"RPC">>},
                               #{source => <<"支付"/utf8>>, target => <<"通知"/utf8>>,
                                 label => <<"MQ">>, kind => dashed}]},
                   [circular, directed], [{height, 360}]).

-spec graph_tree() -> aihtml:html().
graph_tree() ->
    relation_graph(#{nodes => [#{id => ceo, label => <<"CEO">>},
                               #{id => cto, label => <<"CTO">>, parent => ceo},
                               #{id => cfo, label => <<"CFO">>, parent => ceo},
                               #{id => fe, label => <<"前端组"/utf8>>, parent => cto},
                               #{id => be, label => <<"后端组"/utf8>>, parent => cto},
                               #{id => fin, label => <<"财务部"/utf8>>, parent => cfo}]},
                   [tree, tb], [{height, 340}]).

-spec graph_fixed() -> aihtml:html().
graph_fixed() ->
    relation_graph(#{nodes => [#{id => a, label => <<"需求评审"/utf8>>, x => 0, y => 0, root => true},
                               #{id => b, label => <<"设计"/utf8>>, x => 200, y => -60},
                               #{id => c, label => <<"开发"/utf8>>, x => 200, y => 60},
                               #{id => d, label => <<"上线"/utf8>>, x => 400, y => 0}],
                     edges => [{a, b}, {a, c}, {b, d}, {c, d}]},
                   [fixed, round_rect, directed], [{height, 280}, {roam, false}]).

-spec graph_states() -> aihtml:html().
graph_states() ->
    'div'([relation_graph(team(), [loading], [{height, 220}]),
           relation_graph(#{nodes => []}, [], [{height, 220}, {empty_text, <<"暂无关系"/utf8>>}]),
           relation_graph(team(), [], [{height, 220}, {toolbar, false},
                                       {error, <<"加载失败，请稍后重试"/utf8>>}])],
          [<<"grid grid-cols-1 md:grid-cols-3 gap-4">>], []).

-spec graph_select() -> aihtml:html().
graph_select() ->
    'div'([#ah_relation_graph{id = <<"team-graph">>, graph = team(), layout = circular,
                              height = 320, postback = node_selected},
           span(<<"点击一个节点，或聚焦图后用方向键切换"/utf8>>, [<<"text-sm text-muted">>],
                [{id, <<"node-selected">>}])],
          [<<"flex flex-col gap-2">>], []).

%%%===================================================================
%%% Actions
%%%===================================================================

-spec action(atom(), term(), aihtml_action:event(), aihtml_action:ctx()) -> ok.
action(node_selected, _Args, #{value := Id}, Ctx) ->
    aihtml_action:html(Ctx, {id, <<"node-selected">>},
                       case Id of
                           <<>> -> <<"没有选中节点"/utf8>>;
                           _ -> [<<"服务端收到节点："/utf8>>, Id]
                       end).

%%%===================================================================
%%% Data
%%%===================================================================

team() ->
    #{categories => [<<"负责人"/utf8>>, <<"前端"/utf8>>, <<"后端"/utf8>>],
      nodes => [#{id => li, label => <<"李雷"/utf8>>, category => <<"负责人"/utf8>>, root => true},
                #{id => han, label => <<"韩梅梅"/utf8>>, category => <<"负责人"/utf8>>, root => true},
                #{id => wang, label => <<"王"/utf8>>, category => <<"后端"/utf8>>},
                #{id => zhao, label => <<"赵"/utf8>>, category => <<"后端"/utf8>>},
                #{id => chen, label => <<"陈"/utf8>>, category => <<"前端"/utf8>>},
                #{id => liu, label => <<"刘"/utf8>>, category => <<"前端"/utf8>>}],
      edges => [{li, wang}, {li, zhao}, {han, chen}, {han, liu},
                #{source => li, target => han, label => <<"协作"/utf8>>, kind => dashed}]}.
