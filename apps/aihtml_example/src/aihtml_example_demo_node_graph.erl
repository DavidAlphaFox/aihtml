%% @doc Demos of the node graph (aihtml_node_graph), shown on
%% /components/<name>. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
%%
%% The module is also the action module of the demos that talk to the
%% server: the edit log of a graph and the reset button.
-module(aihtml_example_demo_node_graph).
-behaviour(aihtml_action).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0, action/4]).
-export([graph_pipeline/0, graph_readonly/0, graph_layout/0, graph_types/0,
         graph_change/0, graph_record/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => node_graph, title => <<"NodeGraph">>,
       summary => <<"工作流节点图：带类型插槽的节点卡片、拖拽连线、平移缩放和撤销，每次编辑都回传整张图。"/utf8>>,
       demos => [{<<"工作流编辑器：节点库、网格吸附、缩略图"/utf8>>, graph_pipeline},
                 {<<"只读、折线连线、无工具条"/utf8>>, graph_readonly},
                 {<<"服务端自动布局"/utf8>>, graph_layout},
                 {<<"多类型插槽、折叠节点与中转点"/utf8>>, graph_types},
                 {<<"编辑后通知服务端"/utf8>>, graph_change},
                 {<<"record 写法"/utf8>>, graph_record}]}].

%%%===================================================================
%%% Demos
%%%===================================================================

-spec graph_pipeline() -> aihtml:html().
graph_pipeline() ->
    ah_node_graph(pipeline(), [minimap],
                  [{snap, 10}, {height, 560}, {library, library()}]).

-spec graph_readonly() -> aihtml:html().
graph_readonly() ->
    ah_node_graph(pipeline(), [read_only, no_toolbar, auto_fit, <<"max-w-3xl">>],
                  [{link_mode, linear}, {height, 280}]).

-spec graph_layout() -> aihtml:html().
graph_layout() ->
    Step = fun(Id, Title, Ins, Outs) ->
                   #{id => Id, title => Title, width => 180,
                     inputs => [{In, <<"ROWS">>} || In <- Ins],
                     outputs => [{Out, <<"ROWS">>} || Out <- Outs]}
           end,
    ah_node_graph(#{nodes => [Step(orders, <<"订单表"/utf8>>, [], [rows]),
                              Step(users, <<"用户表"/utf8>>, [], [rows]),
                              Step(join, <<"关联"/utf8>>, [left, right], [rows]),
                              Step(filter, <<"过滤"/utf8>>, [rows], [rows]),
                              Step(agg, <<"按月汇总"/utf8>>, [rows], [rows]),
                              Step(report, <<"报表"/utf8>>, [rows], []),
                              Step(export, <<"导出 CSV"/utf8>>, [rows], [])],
                    links => [{{orders, 0}, {join, 0}}, {{users, 0}, {join, 1}},
                              {{join, 0}, {filter, 0}}, {{filter, 0}, {agg, 0}},
                              {{agg, 0}, {report, 0}}, {{filter, 0}, {export, 0}}]},
                  [auto_fit], [{layout, auto}, {link_mode, straight}, {height, 320}]).

-spec graph_types() -> aihtml:html().
graph_types() ->
    ah_node_graph(#{nodes => [#{id => img, type => <<"LoadImage">>, title => <<"读取图片"/utf8>>,
                                pos => {20, 40}, width => 200,
                                outputs => [{image, <<"IMAGE">>}, {mask, <<"MASK">>}]},
                              #{id => any, type => <<"Preview">>, title => <<"预览（任意类型）"/utf8>>,
                                pos => {330, 20}, width => 230,
                                inputs => [#{name => in, type => <<"IMAGE,MASK,LATENT">>},
                                           #{name => extra, type => <<"*">>, optional => true,
                                             shape => hollow}],
                                body => ah_p(<<"多类型插槽画成扇形，可选插槽是空心圆。"/utf8>>,
                                             [<<"text-xs text-muted pb-2">>], [])},
                              #{id => save, type => <<"SaveImage">>, title => <<"保存"/utf8>>,
                                pos => {330, 200}, collapsed => true, color => <<"#d98324">>,
                                inputs => [{images, <<"IMAGE">>}]}],
                    links => [#{source => {img, 0}, target => {any, 0}},
                              #{source => {img, 0}, target => {save, 0},
                                points => [{250, 230}]}],
                    groups => [#{title => <<"输入"/utf8>>, bounds => {0, 0, 250, 160},
                                 color => <<"#3b82f6">>}]},
                  [], [{height, 300}]).

-spec graph_change() -> aihtml:html().
graph_change() ->
    ah_div([ah_node_graph(small(), [], [{id, <<"ng-live">>}, {height, 260},
                                        on(change, {?MODULE, graph_changed, #{log => <<"ng-log">>}})]),
            ah_div([ah_button(<<"重置"/utf8>>, reset, [<<"mt-2">>],
                              [on(click, {?MODULE, reset, #{}})]),
                    ah_span(<<"拖动节点、连线或按 Delete 删除试试"/utf8>>,
                            [<<"text-sm text-muted">>], [{id, <<"ng-log">>}])],
                   [<<"flex items-center gap-3 mt-2">>], [])],
           [], []).

-spec graph_record() -> aihtml:html().
graph_record() ->
    ah_div([#ah_node_graph{id = <<"ng-rec">>, graph = small(), link_mode = linear,
                           snap = 20, height = 240, allow_cycles = true,
                           postback = {graph_changed, #{log => <<"ng-rec-log">>}}},
            ah_p(<<"允许成环、20px 吸附"/utf8>>, [<<"text-sm text-muted mt-2">>],
                 [{id, <<"ng-rec-log">>}])],
           [], []).

%%%===================================================================
%%% Actions
%%%===================================================================

-spec action(atom(), term(), aihtml_action:event(), aihtml_action:ctx()) -> ok.
action(graph_changed, #{log := Log}, #{value := Value, data := Data}, Ctx) ->
    #{<<"nodes">> := Nodes, <<"links">> := Links} = json:decode(Value),
    #{<<"nodes">> := Changed} = json:decode(maps:get(<<"changed">>, Data, <<"{\"nodes\":[]}">>)),
    aihtml_action:html(Ctx, {id, Log},
                       [<<"服务端收到 "/utf8>>, maps:get(<<"op">>, Data, <<"?">>),
                        <<"：改动节点 "/utf8>>,
                        lists:join(<<", ">>, [Id || #{<<"id">> := Id} <- Changed]),
                        <<"；共 "/utf8>>, integer_to_binary(length(Nodes)),
                        <<" 个节点、"/utf8>>, integer_to_binary(length(Links)),
                        <<" 条连线"/utf8>>]);
action(reset, _Args, _Event, Ctx) ->
    set_node_graph(Ctx, {id, <<"ng-live">>}, small()),
    aihtml_action:html(Ctx, {id, <<"ng-log">>}, <<"已恢复服务端保存的图"/utf8>>).

%%%===================================================================
%%% Data
%%%===================================================================

small() ->
    #{nodes => [#{id => src, type => <<"Source">>, title => <<"数据源"/utf8>>,
                  pos => {30, 50}, width => 200, outputs => [{<<"IMAGE">>, <<"IMAGE">>}]},
                #{id => op, type => <<"Transform">>, title => <<"变换"/utf8>>,
                  pos => {300, 30}, width => 200,
                  inputs => [{image, <<"IMAGE">>},
                             #{name => mask, type => <<"MASK">>, optional => true,
                               shape => hollow}],
                  outputs => [{<<"IMAGE">>, <<"IMAGE">>}]},
                #{id => out, type => <<"Sink">>, title => <<"输出"/utf8>>,
                  pos => {570, 60}, width => 200, inputs => [{image, <<"IMAGE">>}]}],
      links => [#{id => e1, source => {src, 0}, target => {op, 0}},
                #{id => e2, source => {op, 0}, target => {out, 0}}]}.

field(Text) ->
    ah_div(Text, [<<"text-xs rounded px-2 py-1 bg-[var(--ah-color-bg-hover)] text-muted truncate">>], []).

pipeline() ->
    #{nodes =>
          [#{id => ckpt, type => <<"CheckpointLoader">>, title => <<"Load Checkpoint">>,
             pos => {40, 80}, color => <<"#7c6ff0">>,
             outputs => [{<<"MODEL">>, <<"MODEL">>}, {<<"CLIP">>, <<"CLIP">>},
                         {<<"VAE">>, <<"VAE">>}],
             widgets => [field(<<"sd_xl_base_1.0.safetensors">>)]},
           #{id => pos, type => <<"CLIPTextEncode">>, title => <<"Prompt (positive)">>,
             pos => {340, 40}, inputs => [{clip, <<"CLIP">>}],
             outputs => [{<<"CONDITIONING">>, <<"CONDITIONING">>}],
             widgets => [field(<<"a tiny lighthouse at dusk">>)]},
           #{id => neg, type => <<"CLIPTextEncode">>, title => <<"Prompt (negative)">>,
             pos => {340, 200}, inputs => [{clip, <<"CLIP">>}],
             outputs => [{<<"CONDITIONING">>, <<"CONDITIONING">>}],
             widgets => [field(<<"blurry, watermark">>)]},
           #{id => latent, type => <<"EmptyLatentImage">>, title => <<"Empty Latent">>,
             pos => {340, 350}, outputs => [{<<"LATENT">>, <<"LATENT">>}],
             widgets => [field(<<"1024 × 1024 × 1"/utf8>>)]},
           #{id => sampler, type => <<"KSampler">>, title => <<"KSampler">>,
             pos => {660, 110}, color => <<"#3aa675">>,
             inputs => [{model, <<"MODEL">>}, {positive, <<"CONDITIONING">>},
                        {negative, <<"CONDITIONING">>}, {latent, <<"LATENT">>},
                        #{name => mask, type => <<"MASK">>, optional => true, shape => hollow}],
             outputs => [{<<"LATENT">>, <<"LATENT">>}],
             widgets => [field(<<"steps 25 · cfg 7.0"/utf8>>), field(<<"sampler dpmpp_2m">>)]},
           #{id => decode, type => <<"VAEDecode">>, title => <<"VAE Decode">>,
             pos => {980, 140}, inputs => [{samples, <<"LATENT">>}, {vae, <<"VAE">>}],
             outputs => [{<<"IMAGE">>, <<"IMAGE">>}]},
           #{id => save, type => <<"SaveImage">>, title => <<"Save Image">>,
             pos => {1260, 140}, color => <<"#d98324">>, inputs => [{images, <<"IMAGE">>}],
             widgets => [field(<<"output/lighthouse_####.png">>)]}],
      links => [#{id => l1, source => {ckpt, 0}, target => {sampler, 0}},
                #{id => l2, source => {ckpt, 1}, target => {pos, 0}},
                #{id => l3, source => {ckpt, 1}, target => {neg, 0}},
                #{id => l4, source => {pos, 0}, target => {sampler, 1}},
                #{id => l5, source => {neg, 0}, target => {sampler, 2}},
                #{id => l6, source => {latent, 0}, target => {sampler, 3}},
                #{id => l7, source => {sampler, 0}, target => {decode, 0}},
                #{id => l8, source => {ckpt, 2}, target => {decode, 1}},
                #{id => l9, source => {decode, 0}, target => {save, 0}}],
      groups => [#{id => g1, title => <<"Conditioning">>, bounds => {320, 10, 280, 300},
                   color => <<"#7c6ff0">>}]}.

library() ->
    [#{type => <<"CLIPTextEncode">>, label => <<"CLIP Text Encode">>,
       category => <<"conditioning">>, inputs => [{clip, <<"CLIP">>}],
       outputs => [{<<"CONDITIONING">>, <<"CONDITIONING">>}],
       widgets => [field(<<"prompt…"/utf8>>)]},
     #{type => <<"EmptyLatentImage">>, label => <<"Empty Latent Image">>, category => <<"latent">>,
       outputs => [{<<"LATENT">>, <<"LATENT">>}]},
     #{type => <<"LatentUpscale">>, label => <<"Latent Upscale">>, category => <<"latent">>,
       inputs => [{samples, <<"LATENT">>}], outputs => [{<<"LATENT">>, <<"LATENT">>}]},
     #{type => <<"VAEDecode">>, label => <<"VAE Decode">>, category => <<"latent">>,
       inputs => [{samples, <<"LATENT">>}, {vae, <<"VAE">>}],
       outputs => [{<<"IMAGE">>, <<"IMAGE">>}]},
     #{type => <<"ImageBlur">>, label => <<"Image Blur">>, category => <<"image">>,
       inputs => [{image, <<"IMAGE">>}], outputs => [{<<"IMAGE">>, <<"IMAGE">>}]},
     #{type => <<"SaveImage">>, label => <<"Save Image">>, category => <<"image">>,
       inputs => [{images, <<"IMAGE">>}]},
     #{type => <<"Preview">>, label => <<"Preview Any">>, category => <<"utility">>,
       inputs => [{any, <<"*">>}]}].
