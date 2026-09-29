%% @doc Demos of the swimlane (aihtml_swimlane), shown on
%% /components/swimlane. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
%%
%% The module is also the action module of the edit demo, which reports
%% what the postback received.
-module(aihtml_example_demo_swimlane).
-behaviour(aihtml_action).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0, action/4]).
-export([swim_basic/0, swim_edit/0, swim_variants/0, swim_continuous/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => swimlane, title => <<"Swimlane">>,
       summary => <<"泳道图：泳道 × 阶段的跨职能流程，节点可拖到别的格子，连线自动重排。"/utf8>>,
       demos => [{<<"订单履约流程"/utf8>>, swim_basic},
                 {<<"拖拽换格，服务端收到改动"/utf8>>, swim_edit},
                 {<<"描边、淡化与选中"/utf8>>, swim_variants},
                 {<<"连续数值轴"/utf8>>, swim_continuous}]}].

%%%===================================================================
%%% Swimlane
%%%===================================================================

-spec swim_basic() -> aihtml:html().
swim_basic() ->
    swimlane(order_nodes(), [legend],
             [{lanes, order_lanes()}, {phases, order_phases()}, {flows, order_flows()},
              {labels, #{corner => <<"泳道 / 阶段"/utf8>>, start => <<"起点"/utf8>>,
                         task => <<"任务"/utf8>>, decision => <<"判定"/utf8>>,
                         'end' => <<"终点"/utf8>>}},
              {selected, n3}]).

%% Dropping a node in another cell calls action(node_changed, ...) below.
-spec swim_edit() -> aihtml:html().
swim_edit() ->
    'div'([swimlane(order_nodes(), [editable],
                    [{lanes, order_lanes()}, {phases, order_phases()}, {flows, order_flows()},
                     on('ah:node-change', {?MODULE, node_changed, #{}})]),
           p(<<"拖动节点到别的格子，或选中后按 Shift+方向键。"/utf8>>,
             [<<"text-sm text-muted mt-2">>], [{id, <<"swim-log">>}])], [], []).

-spec swim_variants() -> aihtml:html().
swim_variants() ->
    swimlane([#{id => a, lane => dev, phase => plan, label => <<"Spec">>, type => start},
              #{id => b, lane => dev, phase => build, label => <<"Code">>, variant => outline},
              #{id => c, lane => qa, phase => build, label => <<"Test plan">>, dimmed => true},
              #{id => d, lane => qa, phase => ship, label => <<"Pass?">>, type => decision},
              #{id => e, lane => dev, phase => ship, label => <<"Release">>, type => 'end',
                color => teal}],
             [],
             [{lanes, [#{id => dev, name => <<"Development">>, color => indigo},
                       #{id => qa, name => <<"QA">>, color => pink}]},
              {phases, [#{id => plan, label => <<"Plan">>}, #{id => build, label => <<"Build">>},
                        #{id => ship, label => <<"Ship">>}]},
              {flows, [#{from => a, to => b}, #{from => b, to => d},
                       #{from => c, to => d, dashed => true, arrow => false},
                       #{from => d, to => e, label => <<"yes">>},
                       #{from => d, to => b, label => <<"no">>, dashed => true}]},
              {selected, d}, {lane_height, 96}, {phase_width, 170}]).

-spec swim_continuous() -> aihtml:html().
swim_continuous() ->
    swimlane([#{id => q1, lane => web, value => 3, label => <<"Beta">>, type => start},
              #{id => q2, lane => web, value => 20, label => <<"GA">>, type => 'end'},
              #{id => q3, lane => app, value => 8, label => <<"TestFlight">>},
              #{id => q4, lane => app, value => 26, label => <<"Store">>, type => 'end'}],
             [],
             [{lanes, [#{id => web, name => <<"Web">>, color => blue},
                       #{id => app, name => <<"Mobile">>, color => orange}]},
              {flows, [#{from => q1, to => q3}, #{from => q1, to => q2},
                       #{from => q3, to => q4}]},
              {axis, continuous}, {value_domain, {0, 30}}, {axis_width, 720},
              {value_ticks, [0, 10, 20, 30]}, {lane_height, 90},
              {labels, #{corner => <<"Days">>}}]).

%%%===================================================================
%%% Actions
%%%===================================================================

-spec action(atom(), term(), aihtml_action:event(), aihtml_action:ctx()) -> ok.
action(node_changed, _Args, #{data := D}, Ctx) ->
    aihtml_action:html(Ctx, {id, <<"swim-log">>},
                       [<<"服务端收到："/utf8>>, maps:get(<<"node">>, D), <<" → "/utf8>>,
                        maps:get(<<"lane">>, D), <<" / "/utf8>>, maps:get(<<"phase">>, D)]).

%%%===================================================================
%%% Data
%%%===================================================================

order_lanes() ->
    [#{id => customer, name => <<"Customer">>, color => blue},
     #{id => sales, name => <<"Sales">>, color => green},
     #{id => warehouse, name => <<"Warehouse">>, color => orange},
     #{id => finance, name => <<"Finance">>, color => purple}].

order_phases() ->
    [#{id => request, label => <<"Request">>}, #{id => review, label => <<"Review">>},
     #{id => fulfill, label => <<"Fulfill">>}, #{id => close, label => <<"Close">>}].

order_nodes() ->
    [#{id => n1, lane => customer, phase => request, label => <<"Place order">>, type => start},
     #{id => n2, lane => sales, phase => review, label => <<"Validate order">>},
     #{id => n3, lane => sales, phase => review, label => <<"In stock?">>, type => decision},
     #{id => n4, lane => warehouse, phase => fulfill, label => <<"Pick & pack">>},
     #{id => n5, lane => finance, phase => fulfill, label => <<"Issue invoice">>},
     #{id => n6, lane => customer, phase => close, label => <<"Receive goods">>, type => 'end'}].

order_flows() ->
    [#{from => n1, to => n2}, #{from => n2, to => n3},
     #{from => n3, to => n4, label => <<"yes">>}, #{from => n4, to => n5},
     #{from => n5, to => n6, dashed => true}].
