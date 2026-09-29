%% Tests for aihtml_data_graph (node_graph).
-module(aihtml_data_graph_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_data_graph.hrl").

-define(M, aihtml_data_graph).

r(Html) -> aihtml_html:render_binary(Html).

has(Needle, Hay) ->
    case binary:match(Hay, Needle) of
        nomatch -> ?debugFmt("~s not in~n~s", [Needle, Hay]), false;
        _ -> true
    end.

count(Needle, Hay) -> length(binary:matches(Hay, Needle)).

%% data-ah-value decoded
value(H) ->
    {match, [V]} = re:run(H, <<" data-ah-value=\"([^\"]*)\"">>, [{capture, all_but_first, binary}]),
    json:decode(unescape(V)).

unescape(B) ->
    lists:foldl(fun({F, T}, Acc) -> binary:replace(Acc, F, T, [global]) end, B,
                [{<<"&quot;">>, <<"\"">>}, {<<"&lt;">>, <<"<">>}, {<<"&gt;">>, <<">">>},
                 {<<"&#39;">>, <<"'">>}, {<<"&amp;">>, <<"&">>}]).

two() ->
    #{nodes => [#{id => a, type => <<"Src">>, pos => {0, 0},
                  outputs => [{out, <<"IMAGE">>}]},
                #{id => b, title => <<"Op">>, pos => {400, 0},
                  inputs => [{in, <<"image">>}, #{name => m, type => <<"IMAGE,MASK">>,
                                                  optional => true, shape => hollow}]}],
      links => [#{id => e1, source => {a, 0}, target => {b, 0}}]}.

%%%===================================================================
%%% Rendering
%%%===================================================================

render_test() ->
    H = r(?M:node_graph(two(), [<<"w-full">>], [{id, g}, {name, graph}, {title, <<"t">>}])),
    ?assertMatch({match, _}, re:run(H, <<"^<div class=\"ah-node-graph w-full\" data-ah=\"node-graph\"">>)),
    ?assert(has(<<"id=\"g\"">>, H)),
    ?assert(has(<<"title=\"t\"">>, H)),
    ?assert(has(<<"style=\"height:400px\"">>, H)),
    ?assert(has(<<"data-ah-link-mode=\"spline\"">>, H)),
    ?assert(has(<<"<div class=\"ah-node-graph-viewport\" tabindex=\"0\" role=\"application\"">>, H)),
    ?assert(has(<<"aria-label=\"Node graph\" data-grid=\"true\"">>, H)),
    ?assert(has(<<"<svg class=\"ah-node-graph-links\" aria-hidden=\"true\">">>, H)),
    ?assertEqual(2, count(<<"class=\"ah-node-graph-node\"">>, H)),
    ?assert(has(<<"data-node-id=\"a\" role=\"group\" aria-label=\"Src\"">>, H)),
    ?assert(has(<<"style=\"transform:translate3d(400px,0px,0);--ah-ng-node-width:240px;\"">>, H)),
    %% slots, types (normalised) and connection state
    ?assert(has(<<"data-kind=\"output\" data-connected=\"true\" data-slot-key=\"a:o0\"">>, H)),
    ?assert(has(<<"data-kind=\"input\" data-connected=\"true\" data-slot-key=\"b:i0\"">>, H)),
    ?assert(has(<<"data-kind=\"input\" data-optional=\"true\" data-slot-key=\"b:i1\"">>, H)),
    ?assert(has(<<"title=\"IMAGE\"><span class=\"ah-node-graph-dot\" data-shape=\"circle\" "
                  "style=\"--ah-ng-dot-color:var(--ah-datatype-IMAGE, var(--ah-datatype-default, #aaa))\">">>, H)),
    ?assert(has(<<"data-shape=\"hollow\" data-multi=\"true\"">>, H)),
    ?assert(has(<<"style=\"fill:var(--ah-datatype-MASK, ">>, H)),
    %% the link: output 0 of a at (240, 44), input 0 of b at (400, 44)
    ?assert(has(<<"<g class=\"ah-node-graph-link\" data-link-id=\"e1\"><path class=\"ah-node-graph-link-hit\" "
                  "d=\"M240,44 C280,44 360,44 400,44\">">>, H)),
    %% toolbar with undo/redo/delete disabled, no minimap by default
    ?assert(has(<<"<div class=\"ah-node-graph-toolbar\" role=\"toolbar\"">>, H)),
    ?assert(has(<<"data-action=\"undo\" title=\"Undo\" aria-label=\"Undo\" disabled">>, H)),
    ?assert(has(<<"data-action=\"delete\"">>, H)),
    ?assertNot(has_quiet(<<"ah-node-graph-minimap">>, H)),
    ?assert(has(<<"<span class=\"ah-node-graph-node-resize\"></span>">>, H)),
    %% the value and the hidden input
    #{<<"nodes">> := [A, B], <<"links">> := [L], <<"groups">> := []} = value(H),
    ?assertEqual(#{<<"id">> => <<"a">>, <<"type">> => <<"Src">>, <<"pos">> => [0, 0],
                   <<"inputs">> => [], <<"outputs">> => [#{<<"name">> => <<"out">>,
                                                          <<"type">> => <<"IMAGE">>}]}, A),
    ?assertMatch(#{<<"title">> := <<"Op">>,
                   <<"inputs">> := [_, #{<<"optional">> := true, <<"shape">> := <<"hollow">>}]}, B),
    ?assertEqual(#{<<"id">> => <<"e1">>, <<"source">> => [<<"a">>, 0],
                   <<"target">> => [<<"b">>, 0]}, L),
    ?assert(has(<<"<input type=\"hidden\" name=\"graph\" value=\"{">>, H)),
    ?assertEqual(1, count(<<"name=">>, H)).

has_quiet(Needle, Hay) -> binary:match(Hay, Needle) =/= nomatch.

flags_test() ->
    H = r(?M:node_graph(two(), [read_only, minimap, no_grid, auto_fit, allow_cycles],
                        [{link_mode, linear}, {snap, 10}, {height, auto}, {label, <<"Flow">>}])),
    ?assert(has(<<"class=\"ah-node-graph\" ">>, H)),
    ?assert(has(<<"data-read-only=\"true\"">>, H)),
    ?assert(has(<<"data-ah-allow-cycles=\"true\"">>, H)),
    ?assert(has(<<"data-ah-auto-fit=\"true\"">>, H)),
    ?assert(has(<<"data-ah-snap=\"10\"">>, H)),
    ?assert(has(<<"data-ah-link-mode=\"linear\"">>, H)),
    ?assert(has(<<"aria-label=\"Flow\"">>, H)),
    ?assertNot(has_quiet(<<"data-grid">>, H)),
    ?assertNot(has_quiet(<<"style=\"height">>, H)),
    ?assert(has(<<"<div class=\"ah-node-graph-minimap\" aria-hidden=\"true\"></div>">>, H)),
    %% read-only: no resize handles, no delete tool
    ?assertNot(has_quiet(<<"ah-node-graph-node-resize">>, H)),
    ?assertNot(has_quiet(<<"data-action=\"delete\"">>, H)),
    ?assert(has(<<"d=\"M240,44 L255,44 L385,44 L400,44\"">>, H)),
    H2 = r(?M:node_graph(two(), [no_toolbar], [{link_mode, straight}, {height, <<"50vh">>}])),
    ?assertNot(has_quiet(<<"ah-node-graph-toolbar">>, H2)),
    ?assert(has(<<"style=\"height:50vh\"">>, H2)),
    ?assert(has(<<"d=\"M240,44 L250,44 L320,44 L320,44 L390,44 L400,44\"">>, H2)).

nodes_test() ->
    G = #{nodes => [#{id => 1, type => t, pos => [10.5, 20], width => 300, height => 150,
                      color => <<"#ff0000">>, collapsed => true,
                      inputs => [{a, <<"LATENT">>}], outputs => [b],
                      widgets => [<<"<w>">>, {safe, <<"<i>x</i>">>}],
                      body => {safe, <<"<p>body</p>">>}, data => #{k => 1}},
                    #{pos => {0, 0}, inputs => [#{name => x, type => <<"a b">>, label => <<"X & Y">>}]}],
          groups => [#{title => <<"G">>, bounds => [0, 0, 100, 100], color => <<"rgb(1, 2, 3)">>},
                     #{id => gg, bounds => {1, 2, 3, 4}}]},
    H = r(?M:node_graph(G, [], [])),
    ?assert(has(<<"data-node-id=\"1\" role=\"group\" aria-label=\"t\" data-collapsed=\"true\" "
                  "style=\"transform:translate3d(10.5px,20px,0);--ah-ng-node-width:300px;"
                  "--ah-ng-node-height:150px;--ah-ng-node-accent:#ff0000;\"">>, H)),
    ?assert(has(<<"aria-label=\"Expand node\" aria-expanded=\"false\"">>, H)),
    ?assert(has(<<"<span class=\"ah-node-graph-stub\" data-side=\"in\" "
                  "style=\"--ah-ng-dot-color:var(--ah-datatype-LATENT, ">>, H)),
    ?assert(has(<<"<span class=\"ah-node-graph-stub\" data-side=\"out\" "
                  "style=\"--ah-ng-dot-color:var(--ah-datatype-default, #aaa)\">">>, H)),
    ?assert(has(<<"<div class=\"ah-node-graph-widget\">&lt;w&gt;</div>"
                  "<div class=\"ah-node-graph-widget\"><i>x</i></div>">>, H)),
    ?assert(has(<<"<div class=\"ah-node-graph-custom\"><p>body</p></div>">>, H)),
    %% the id defaults to n<index>; type names become CSS names
    ?assert(has(<<"data-node-id=\"n1\"">>, H)),
    ?assert(has(<<"var(--ah-datatype-A_B, ">>, H)),
    ?assert(has(<<"<span class=\"ah-node-graph-slot-label\">X &amp; Y</span>">>, H)),
    ?assert(has(<<"data-group-id=\"g1\" style=\"transform:translate3d(0px,0px,0);width:100px;"
                  "height:100px;--ah-ng-group-color:rgb(1, 2, 3);\"">>, H)),
    ?assert(has(<<"data-group-id=\"gg\"">>, H)),
    ?assert(has(<<"<span class=\"ah-node-graph-group-title\">Group</span>">>, H)),
    #{<<"nodes">> := [N1, _], <<"groups">> := [_, G2]} = value(H),
    ?assertMatch(#{<<"id">> := <<"1">>, <<"type">> := <<"t">>, <<"pos">> := [10.5, 20],
                   <<"collapsed">> := true, <<"width">> := 300, <<"height">> := 150,
                   <<"data">> := #{<<"k">> := 1}}, N1),
    ?assertNot(maps:is_key(<<"html">>, N1)),
    ?assertEqual(#{<<"id">> => <<"gg">>, <<"bounds">> => [1, 2, 3, 4]}, G2).

reroute_and_collapsed_links_test() ->
    G = #{nodes => [#{id => a, pos => {0, 0}, outputs => [o, p]},
                    #{id => b, pos => {300, 100}, collapsed => true, inputs => [i]}],
          links => [#{source => [a, 1], target => [b, 0], points => [{200, 50}]},
                    {{a, 0}, {zz, 0}}]},
    H = r(?M:node_graph(G, [], [{link_mode, linear}])),
    %% output 1 of a at (240, 64); b collapsed: its inputs sit at y + 15
    ?assert(has(<<"d=\"M240,64 L255,64 L185,50 L200,50 L215,50 L285,115 L300,115\"">>, H)),
    ?assert(has(<<"<circle class=\"ah-node-graph-waypoint\" data-index=\"0\" cx=\"200\" cy=\"50\" r=\"5\"">>, H)),
    %% a link to a node that does not exist is kept but not drawn
    ?assertEqual(1, count(<<"<g class=\"ah-node-graph-link\"">>, H)),
    ?assertMatch(#{<<"links">> := [_, #{<<"id">> := <<"l1">>, <<"target">> := [<<"zz">>, 0]}]},
                 value(H)).

library_island_test() ->
    H = r(?M:node_graph(two(), [], [{library, [#{type => x, label => <<"</script><b>">>,
                                                 category => c, inputs => [i],
                                                 widgets => [{safe, <<"<em>w</em>">>}]}]}])),
    {match, [Json]} = re:run(H, <<"<script class=\"ah-node-graph-data\" type=\"application/json\">(.*?)</script>">>,
                             [{capture, all_but_first, binary}]),
    ?assertNot(has_quiet(<<"</">>, Json)),
    #{<<"library">> := [#{<<"type">> := <<"x">>, <<"label">> := <<"</script><b>">>,
                          <<"category">> := <<"c">>,
                          <<"node">> := #{<<"inputs">> := [#{<<"name">> := <<"i">>}],
                                          <<"html">> := #{<<"widgets">> := [<<"<em>w</em>">>]}}}]} =
        json:decode(Json),
    ?assertNot(has_quiet(<<"ah-node-graph-data">>, r(?M:node_graph(two(), [], [])))).

layout_test() ->
    G = #{nodes => [#{id => c, inputs => [x]}, #{id => a, outputs => [o]},
                    #{id => b, inputs => [x], outputs => [o]}, #{id => d, pos => {5, 5}}],
          links => [{{a, 0}, {b, 0}}, {{b, 0}, {c, 0}}]},
    #{<<"nodes">> := Ns} = value(r(?M:node_graph(G, [], []))),
    P = maps:from_list([{Id, Pos} || #{<<"id">> := Id, <<"pos">> := Pos} <- Ns]),
    [Ax, _] = maps:get(<<"a">>, P),
    [Bx, _] = maps:get(<<"b">>, P),
    [Cx, _] = maps:get(<<"c">>, P),
    ?assert(Ax < Bx andalso Bx < Cx),
    ?assertEqual([5, 5], maps:get(<<"d">>, P)),
    %% layout auto moves every node
    #{<<"nodes">> := Ns2} = value(r(?M:node_graph(G, [], [{layout, auto}]))),
    ?assertNotEqual([5, 5], hd([Pos || #{<<"id">> := <<"d">>, <<"pos">> := Pos} <- Ns2])),
    %% node_graph_layout/1 answers in the input form
    #{nodes := [#{id := c, pos := {_, _}}, #{id := a, pos := {40, _}}, _, #{id := d, pos := _}],
      links := [_, _]} = ?M:node_graph_layout(G),
    %% cycles do not loop forever
    Cyc = #{nodes => [#{id => x, inputs => [i], outputs => [o]}, #{id => y, inputs => [i], outputs => [o]}],
            links => [{{x, 0}, {y, 0}}, {{y, 0}, {x, 0}}]},
    #{nodes := [_, _]} = ?M:node_graph_layout(Cyc).

set_node_graph_test() ->
    Ops = aihtml_action:render_ops(
            fun(Ctx) ->
                    ?M:set_node_graph(Ctx, {id, g}, two()),
                    ?M:set_node_graph(Ctx, #{id => <<"h">>},
                                      #{nodes => [#{id => n, widgets => [{safe, <<"<b>w</b>">>}]}]})
            end),
    [#{op := call, id := <<"g">>, method := <<"setGraph">>, args := [G1]},
     #{op := call, id := <<"h">>, args := [G2]}] = Ops,
    ?assertMatch(#{nodes := [#{id := <<"a">>}, _], links := [#{id := <<"e1">>}]}, G1),
    ?assertMatch(#{nodes := [#{id := <<"n">>, pos := [40, 40],
                               html := #{widgets := [<<"<b>w</b>">>]}}]}, G2).

validation_test() ->
    ?assertError({aihtml, {bad_link_mode, curvy}}, r(#ah_node_graph{link_mode = curvy})),
    ?assertError({aihtml, {bad_snap, 0}}, r(#ah_node_graph{snap = 0})),
    ?assertError({aihtml, {bad_layout, grid}}, r(#ah_node_graph{layout = grid})),
    ?assertError({aihtml, {bad_height, <<"1px;color:red">>}},
                 r(#ah_node_graph{height = <<"1px;color:red">>})),
    ?assertError({aihtml, {bad_graph_color, _}},
                 r(#ah_node_graph{graph = #{nodes => [#{id => a, color => <<"red;x:y">>}]}})),
    ?assertError({aihtml, {duplicate_graph_id, nodes, <<"a">>}},
                 r(#ah_node_graph{graph = #{nodes => [#{id => a}, #{id => <<"a">>}]}})),
    ?assertError({aihtml, {bad_graph_node, x}}, r(#ah_node_graph{graph = #{nodes => [x]}})),
    ?assertError({aihtml, {bad_graph_link, _}},
                 r(#ah_node_graph{graph = #{links => [#{source => {a, -1}, target => {b, 0}}]}})),
    ?assertError({aihtml, {bad_graph_point, _}},
                 r(#ah_node_graph{graph = #{nodes => [#{id => a, pos => {x, 1}}]}})),
    ?assertError({aihtml, {bad_graph_slot_shape, star}},
                 r(#ah_node_graph{graph = #{nodes => [#{id => a, inputs => [#{name => i, shape => star}]}]}})),
    ?assertError({aihtml, {bad_graph_group, _}},
                 r(#ah_node_graph{graph = #{groups => [#{bounds => {1, 2}}]}})),
    ?assertError({aihtml, {bad_graph_library_item, _}}, r(#ah_node_graph{library = [#{label => x}]})),
    ?assertError({aihtml, {bad_flag, node_graph, minimap, yes}}, r(#ah_node_graph{minimap = yes})),
    ?assertError({aihtml, {unknown_modifier, node_graph, big, _}}, ?M:node_graph(#{}, [big], [])).

%%%===================================================================
%%% Catalog
%%%===================================================================

catalog_test() ->
    [#{name := node_graph}] = ?M:catalog(),
    #{flags := Fl, options := Op, option_docs := Docs, methods := Ms} =
        aihtml_catalog:entry(?M, node_graph),
    ?assertEqual(lists:sort(Fl ++ Op), lists:sort(maps:keys(Docs))),
    ?assert(lists:member(setGraph, [N || #{name := N} <- Ms])),
    %% flags write no classes (they are data attributes)
    ?assertEqual([<<"ah-node-graph">>],
                 aihtml_catalog:classes(aihtml_catalog:entry(?M, node_graph), Fl)),
    ?assertEqual([{set_node_graph, 3}, {node_graph_layout, 1}], ?M:facade_extras()),
    [?assert(erlang:function_exported(?M, F, A)) || {F, A} <- ?M:facade_extras()].

%%%===================================================================
%%% element records (designs/05-records.md)
%%%===================================================================

record_equals_builder_test() ->
    Lib = [#{type => t}],
    ?assertEqual(r(?M:node_graph(two(), [read_only, minimap, <<"x">>],
                                 [{id, g}, {name, n}, {link_mode, straight}, {snap, 5},
                                  {height, 300}, {library, Lib}, {label, <<"L">>},
                                  {title, <<"t">>}])),
                 r(#ah_node_graph{graph = two(), read_only = true, minimap = true,
                                  css = [<<"x">>], id = g, name = n, link_mode = straight,
                                  snap = 5, height = 300, library = Lib, label = <<"L">>,
                                  attrs = [{title, <<"t">>}]})).

builder_fills_fields_test() ->
    R = ?M:node_graph(two(), [no_grid, auto_fit, <<"c">>], [{layout, auto}, {data_x, 1}]),
    ?assertMatch(#ah_node_graph{no_grid = true, auto_fit = true, layout = auto,
                                css = [<<"c">>], attrs = [{data_x, 1}], id = undefined}, R),
    ?assertError({aihtml, {record_only_field, ah_node_graph, postback}},
                 ?M:node_graph(#{}, [], [{postback, x}])).

generated_id_test() ->
    H = r(#ah_node_graph{}),
    ?assertMatch({match, _}, re:run(H, <<" id=\"ah-g[0-9]+\"">>)),
    ?assertNotEqual(r(#ah_node_graph{}), H).

postback_test() ->
    H = r(#ah_node_graph{id = g, postback = {saved, #{k => 1}}}),
    {match, [T]} = re:run(H, <<"data-ah-on=\"change:([^\"]+)\"">>, [{capture, all_but_first, binary}]),
    ?assertEqual({ok, {?MODULE, saved, #{k => 1}}}, aihtml_action:unsign(T)).

records_match_catalog_test() ->
    Base = [module, id, css, attrs, postback, delegate],
    [begin
         Tag = list_to_atom("ah_" ++ atom_to_list(N)),
         Fields = ?M:fields(Tag),
         ?assertEqual(Base, lists:sublist(Fields, 6)),
         Defaults = maps:from_list(lists:zip(Fields, tl(tuple_to_list(#ah_node_graph{})))),
         [?assertEqual({N, F, false}, {N, F, maps:get(F, Defaults)})
          || F <- maps:get(flags, E, [])],
         [?assert(lists:member(O, Fields)) || O <- maps:get(options, E, [])],
         ?assertEqual(?M, maps:get(module, Defaults))
     end || #{name := N} = E <- ?M:catalog()].
