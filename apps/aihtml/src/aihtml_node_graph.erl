%%%-------------------------------------------------------------------
%%% @doc Node graph, ported from sigil (data/node_graph and its model,
%%% geometry, layout, link_drag, search and store namespaces). DOM and
%%% class names are sigil's, so priv/css/sigil/components/node_graph.css
%%% applies unchanged.
%%%
%%%   ah_node_graph(Graph, Css, Attrs)       a node editor on an infinite canvas
%%%   set_node_graph(Ctx, Target, Graph)     (in an action) replace the graph
%%%   node_graph_layout(Graph)               place the nodes in layers
%%%
%%% == Data ==
%%%
%%% `Graph' is `#{nodes => [Node], links => [Link], groups => [Group]}'
%%% (see the types below). A node has an `id',
%%% a `type', a `title', `pos' (its top left corner), `width', typed
%%% `inputs' and `outputs', and optionally `widgets' / `body' (server
%%% rendered HTML shown inside the card) and `data' (carried along). A
%%% link joins output `I' of one node to input `J' of another:
%%% `#{source => {From, I}, target => {To, J}}'. An input takes at most
%%% one link; slots connect when their types are compatible (equal, one
%%% of a "A,B" list, or "*"), and links that would close a cycle are
%%% refused unless `allow_cycles'.
%%%
%%% == Rendering ==
%%%
%%% The server renders the whole initial view: node cards
%%% (templates/node_graph_node.mustache), link paths computed here with
%%% sigil's geometry (templates/node_graph_link.mustache), group frames,
%%% toolbar. Nodes without a `pos' (or all nodes with `{layout, auto}')
%%% are placed by a layered layout computed here (node_graph_layout/1).
%%% The browser behaviour (assets/js/components/node_graph.ts) keeps the
%%% graph in memory and redraws with the same templates while the user
%%% edits: drag nodes (grid snap), drag links between slots (incompatible
%%% slots dim, a dragged input link is detached), select with a marquee,
%%% pan (middle button, Space, the hand tool, touch) and zoom (wheel,
%%% pinch, toolbar), rename by double click, collapse, resize, reroute
%%% points, group frames, context menus, a node search menu fed by
%%% `library', undo/redo, copy/paste, and the keyboard.
%%%
%%% == Edits and postbacks ==
%%%
%%% The root carries the current graph as JSON in `data-ah-value' (and a
%%% hidden input when `name' is given). After every edit it fires
%%% `change', so `on(change, Ref)' or the record's `postback' receives
%%%
%%%   Event.value   the whole graph, JSON (json:decode/1 it)
%%%   Event.data    #{<<"op">> => move | connect | disconnect | remove | add
%%%                   | paste | duplicate | resize | collapse | rename
%%%                   | reroute | group-add | group-move | group-resize
%%%                   | group-rename | group-remove | undo | redo,
%%%                   <<"changed">> => JSON {"nodes": [...], "links": [...],
%%%                                          "groups": [...]}  (new or changed objects),
%%%                   <<"removed">> => JSON {"nodes": [Id], "links": [Id],
%%%                                          "groups": [Id]}}
%%%
%%% An action that rejects an edit can answer with
%%% `aihtml_action:call(Ctx, {id, Id}, undo, [])', or with
%%% set_node_graph/3 to show the graph it stored. Selection changes fire
%%% `ah:selection-change' (Event.data selection = "id,id", read with
%%% aihtml_value:split/1); a link dropped
%%% on empty canvas without a `library' fires `ah:link-drop' (Event.data
%%% origin = "node:output:0", point = "x,y").
%%%
%%% Each component function builds an element record (#ah_node_graph{},
%%% include/aihtml_node_graph.hrl) and render/1 turns it into HTML.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_node_graph).
-behaviour(aihtml_element).

-include("aihtml_node_graph.hrl").

-export([ah_node_graph/3, set_node_graph/3, node_graph_layout/1,
         render/1, fields/1, catalog/0, facade_extras/0]).

-export_type([graph/0, element/0, graph_id/0, point/0, slot/0, graph_node/0, endpoint/0,
              link/0, group/0, library_item/0, link_mode/0]).

-define(H, aihtml_html).
-define(E, aihtml_element).

%% Shared templates (see aihtml_tpl): also compiled to AH.tpl.* for the browser.
-compile({parse_transform, beamai_mustache_transform}).
-mustache_template({tpl_node_graph_node, "../templates/node_graph_node.mustache"}).
-mustache_template({tpl_node_graph_link, "../templates/node_graph_link.mustache"}).
-mustache_template({tpl_node_graph_group, "../templates/node_graph_group.mustache"}).
-mustache_template({tpl_node_graph_menu, "../templates/node_graph_menu.mustache"}).
-mustache_template({tpl_node_graph_search, "../templates/node_graph_search.mustache"}).
-mustache_template({tpl_node_graph_minimap, "../templates/node_graph_minimap.mustache"}).

-type element() :: #ah_node_graph{}.
%% Node, link and group ids: written as binaries in the browser.
-type graph_id() :: binary() | atom() | integer() | string().
%% A point of the graph's world coordinates, `{X, Y}' or `[X, Y]'.
-type point() :: {number(), number()} | [number()].
%% A slot (port) of a node: a name (any type), `{Name, Type}', or a map.
%% `type' is a data type such as <<"IMAGE">> ("IMAGE,MASK" for several,
%% "*" or none for any); slots of compatible types connect. `shape' is
%% circle (default), square, grid or hollow.
-type slot() :: binary() | atom() | {term(), term()}
              | #{name := term(), type => term(), label => term(),
                  optional => boolean(), shape => circle | square | grid | hollow}.
%% A node card. `pos' is its top left corner; nodes without one are placed
%% by the server's layered layout. `widgets' are rows of HTML (aihtml
%% elements) shown under the slots, `body' free HTML after them; `data'
%% is any JSON-encodable term carried along untouched.
-type graph_node() :: #{id := graph_id(),
                        type => term(), title => term(),
                        pos => point(),
                        width => number(), height => number(),
                        collapsed => boolean(), color => term(),
                        inputs => [slot()], outputs => [slot()],
                        widgets => [aihtml_html:html()], body => aihtml_html:html(),
                        data => term()}.
%% A link from output `Index' of `source' to input `Index' of `target',
%% through the optional reroute `points'. `{Source, Target}' is short for
%% a map without id.
-type endpoint() :: {graph_id(), non_neg_integer()} | [graph_id() | non_neg_integer()].
-type link() :: #{id => graph_id(),
                  source := endpoint(), target := endpoint(),
                  points => [point()]}
              | {endpoint(), endpoint()}.
%% A group frame: a titled box drawn behind the nodes; moving it moves the
%% nodes it fully contains. `bounds' is `{X, Y, W, H}'.
-type group() :: #{id => graph_id(), title => term(),
                   bounds := {number(), number(), number(), number()} | [number()],
                   color => term()}.
-type graph() :: #{nodes => [graph_node()], links => [link()], groups => [group()]}.
%% An entry of the node search menu (right click on the canvas, or a link
%% dropped on empty canvas): the node it adds, with `label' and `category'
%% for the menu.
-type library_item() :: #{type := term(), label => term(), category => term(),
                          title => term(), width => number(), color => term(),
                          inputs => [slot()], outputs => [slot()],
                          widgets => [aihtml_html:html()], body => aihtml_html:html(),
                          data => term()}.
-type link_mode() :: spline | linear | straight.

%% Card layout, in graph units (= px at zoom 1). They match node_graph.css
%% (30px header, 4px body padding, 20px slot rows) and node_graph.ts.
-define(TITLE_H, 30).
-define(SLOT_H, 20).
-define(PAD_TOP, 4).
-define(PAD_BOTTOM, 8).
-define(DEFAULT_W, 240).
-define(MIN_W, 225).

-define(TWO_SLICES, [<<"M0 50 A 50 50 0 0 1 100 50">>, <<"M100 50 A 50 50 0 0 1 0 50">>]).
-define(THREE_SLICES, [<<"M0 50A50 50 0 0 0 75 93L50 50">>, <<"M75 93A50 50 0 0 0 75 7L50 50">>,
                       <<"M75 7A50 50 0 0 0 0 50L50 50">>]).

%%%===================================================================
%%% node_graph
%%%===================================================================

%% @doc A node editor showing `Graph' (see the module doc). Css flags:
%% `read_only' (look, select, pan and zoom only), `minimap', `auto_fit'
%% (fit the content into the view on load), `allow_cycles', `no_toolbar',
%% `no_grid'. Options: `link_mode' (spline (default) | linear | straight),
%% `snap' (grid in px for dragged nodes), `height' (px, a CSS length or
%% `auto', default 400), `library' (the node search menu's entries),
%% `layout' (`auto' places every node, default `none' places only nodes
%% without `pos'), `label' (aria-label). `name' adds a hidden input
%% holding the graph JSON.
-spec ah_node_graph(graph(), aihtml_html:css(), aihtml_html:attrs()) -> #ah_node_graph{}.
ah_node_graph(Graph, Css, Attrs) ->
    ?E:build(?MODULE, #ah_node_graph{graph = Graph}, Css, Attrs).

render_node_graph(#ah_node_graph{name = Name, read_only = RO, link_mode = Mode,
                                 snap = Snap, height = Height, layout = Layout,
                                 library = Library0} = R0) ->
    {_Id, R} = ensure_id(R0),
    Classes = ?E:classes(?MODULE, R),           % checks the flag fields first
    lists:member(Mode, [spline, linear, straight])
        orelse error({aihtml, {bad_link_mode, Mode}}),
    (Snap =:= undefined orelse (is_integer(Snap) andalso Snap > 0))
        orelse error({aihtml, {bad_snap, Snap}}),
    lists:member(Layout, [none, auto]) orelse error({aihtml, {bad_layout, Layout}}),
    is_list(Library0) orelse error({aihtml, {bad_library, Library0}}),
    #{nodes := Nodes, links := Links, groups := Groups} = G =
        place(normalize(R#ah_node_graph.graph), Layout =:= auto),
    Library = [library_item(I) || I <- Library0],
    Json = json_text(graph_json(G)),
    Conn = connections(Links),
    ById = maps:from_list([{maps:get(id, N), N} || N <- Nodes]),
    Canvas =
        ?H:el('div',
              [?H:el('div', [aihtml_tpl:safe(tpl_node_graph_group(group_view(Gr, RO)))
                             || Gr <- Groups],
                     [<<"ah-node-graph-groups">>], []),
               ?H:el(svg, [aihtml_tpl:safe(tpl_node_graph_link(link_view(L, ById, Mode)))
                           || L <- Links, endpoints_known(L, ById)],
                     [<<"ah-node-graph-links">>], [{aria_hidden, <<"true">>}]),
               ?H:el('div', [aihtml_tpl:safe(tpl_node_graph_node(node_view(N, Conn, RO)))
                             || N <- Nodes],
                     [<<"ah-node-graph-nodes">>], []),
               ?H:el('div', [], [<<"ah-node-graph-marquee">>], [{hidden, true}])],
              [<<"ah-node-graph-canvas">>],
              [{style, <<"transform:scale3d(1,1,1) translate3d(0px,0px,0)">>}]),
    Viewport = ?H:el('div', Canvas, [<<"ah-node-graph-viewport">>],
                     [{tabindex, 0}, {role, application},
                      {aria_roledescription, <<"node graph">>},
                      {aria_label, R#ah_node_graph.label},
                      {data_grid, not R#ah_node_graph.no_grid andalso <<"true">>}]),
    Island = case Library of
                 [] -> [];
                 _ -> ?H:el(script, {safe, script_json(#{library => Library})},
                            [<<"ah-node-graph-data">>], [{type, <<"application/json">>}])
             end,
    ?H:el('div',
          [Viewport,
           [toolbar(RO) || not R#ah_node_graph.no_toolbar],
           [?H:el('div', [], [<<"ah-node-graph-minimap">>], [{aria_hidden, <<"true">>}])
            || R#ah_node_graph.minimap],
           Island,
           hidden(Name, Json)],
          Classes,
          [[{data_ah, <<"node-graph">>}, {data_ah_value, Json},
            {data_ah_link_mode, atom_to_binary(Mode)},
            {data_ah_snap, Snap},
            {data_read_only, RO andalso <<"true">>},
            {data_ah_allow_cycles, R#ah_node_graph.allow_cycles andalso <<"true">>},
            {data_ah_auto_fit, R#ah_node_graph.auto_fit andalso <<"true">>},
            {style, height_style(Height)}],
           ?E:root_attrs(R, change)]).

height_style(auto) -> undefined;
height_style(H) when is_integer(H), H > 0 -> <<"height:", (integer_to_binary(H))/binary, "px">>;
height_style(H) when is_binary(H); is_list(H) ->
    B = text(H),
    case re:run(B, <<"^[0-9.]+(px|em|rem|vh|%)?$">>) of
        {match, _} -> <<"height:", B/binary>>;
        nomatch -> error({aihtml, {bad_height, H}})
    end;
height_style(H) -> error({aihtml, {bad_height, H}}).

toolbar(RO) ->
    ?H:el('div',
          [tool(<<"hand">>, <<"Pan the canvas (or hold Space / middle-drag)">>, ic(hand),
                [{aria_pressed, <<"false">>}]),
           tool(<<"zoom-in">>, <<"Zoom in">>, ic(plus), []),
           ?H:el(span, <<"100%">>, [<<"ah-node-graph-zoom">>], [{aria_live, polite}]),
           tool(<<"zoom-out">>, <<"Zoom out">>, ic(minus), []),
           tool(<<"fit">>, <<"Fit to view">>, ic(fit), []),
           tool(<<"undo">>, <<"Undo">>, ic(undo), [{disabled, true}]),
           tool(<<"redo">>, <<"Redo">>, ic(redo), [{disabled, true}]),
           [tool(<<"delete">>, <<"Delete selection (Del)">>, ic(trash), [{disabled, true}])
            || not RO]],
          [<<"ah-node-graph-toolbar">>],
          [{role, toolbar}, {aria_label, <<"Graph tools">>}]).

tool(Action, Label, D, Extra) ->
    ?H:el(button,
          {safe, [<<"<svg viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" "
                    "stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\" "
                    "aria-hidden=\"true\"><path d=\"">>, D, <<"\"></path></svg>">>]},
          [<<"ah-node-graph-tool">>],
          [{type, button}, {data_action, Action}, {title, Label}, {aria_label, Label} | Extra]).

ic(plus) -> <<"M12 5v14M5 12h14">>;
ic(minus) -> <<"M5 12h14">>;
ic(fit) -> <<"M4 9V4h5M20 9V4h-5M4 15v5h5M20 15v5h-5">>;
ic(undo) -> <<"M9 14 4 9l5-5M4 9h11a5 5 0 0 1 0 10h-3">>;
ic(redo) -> <<"m15 14 5-5-5-5M20 9H9a5 5 0 0 0 0 10h3">>;
ic(trash) -> <<"M3 6h18M8 6V4h8v2M19 6l-1 14H6L5 6M10 11v6M14 11v6">>;
ic(hand) ->
    <<"M18 11V6a2 2 0 0 0-4 0v5M14 10V4a2 2 0 0 0-4 0v6M10 10.5V6a2 2 0 0 0-4 0v8"
      "M18 8a2 2 0 1 1 4 0v6a8 8 0 0 1-8 8h-2c-2.8 0-4.5-.86-5.99-2.34l-3.6-3.6"
      "a2 2 0 0 1 2.83-2.82L7 15">>.

%%%===================================================================
%%% Views (the same fields as node_graph.ts builds for the templates)
%%%===================================================================

node_view(#{id := Id, inputs := Ins, outputs := Outs, collapsed := Collapsed} = N,
          Conn, RO) ->
    {X, Y} = maps:get(pos, N),
    Height = maps:get(height, N),
    Color = maps:get(color, N),
    Type = maps:get(type, N),
    {In, Out} = maps:get(Id, Conn, {[], []}),
    Widgets = maps:get(widgets, N),
    Body = maps:get(body, N),
    #{id => Id,
      title => title(N),
      has_type => Type =/= undefined,
      type => nz(Type),
      style => iolist_to_binary(
                 [<<"transform:translate3d(">>, num(X), <<"px,">>, num(Y), <<"px,0);">>,
                  <<"--ah-ng-node-width:">>, num(node_width(N)), <<"px;">>,
                  [[<<"--ah-ng-node-height:">>, num(Height), <<"px;">>] || Height =/= undefined],
                  [[<<"--ah-ng-node-accent:">>, Color, <<";">>] || Color =/= undefined]]),
      collapsed => Collapsed,
      sized => Height =/= undefined andalso not Collapsed,
      expanded => atom_to_binary(not Collapsed),
      toggle_label => case Collapsed of
                          true -> <<"Expand node">>;
                          false -> <<"Collapse node">>
                      end,
      stub_in => stub(Collapsed, Ins),
      stub_out => stub(Collapsed, Outs),
      inputs => [slot_view(Id, input, I, S, lists:member(I, In))
                 || {I, S} <- lists:enumerate(0, Ins)],
      outputs => [slot_view(Id, output, I, S, lists:member(I, Out))
                  || {I, S} <- lists:enumerate(0, Outs)],
      has_widgets => Widgets =/= [],
      widgets => [#{html => W} || W <- Widgets],
      has_body => Body =/= undefined,
      body => nz(Body),
      resizable => not RO}.

stub(true, [#{type := T} | _]) -> [#{color => link_color(T)}];
stub(_, _) -> [].

slot_view(NodeId, Kind, I, #{name := Name, type := Type, label := Label0,
                            optional := Opt, shape := Shape}, Connected) ->
    Label = case Label0 of
                undefined -> Name;
                _ -> Label0
            end,
    Colors = slot_colors(Type),
    Multi = length(Colors) > 1,
    #{kind => atom_to_binary(Kind),
      key => slot_key(NodeId, Kind, I),
      index => integer_to_binary(I),
      connected => Connected,
      optional => Opt,
      tip => norm_type(Type),
      shape => case Shape of undefined -> <<"circle">>; _ -> Shape end,
      multi => Multi,
      color => hd(Colors),
      slices => case Multi of
                    false -> [];
                    true ->
                        Paths = case length(Colors) of
                                    2 -> ?TWO_SLICES;
                                    _ -> ?THREE_SLICES
                                end,
                        [#{d => D, fill => C} || {D, C} <- lists:zip(Paths, Colors)]
                end,
      has_label => Label =/= <<>>,
      label => Label}.

slot_key(NodeId, Kind, I) ->
    <<NodeId/binary, ":", (case Kind of input -> <<"i">>; output -> <<"o">> end)/binary,
      (integer_to_binary(I))/binary>>.

link_view(#{id := Id, points := Points} = L, ById, Mode) ->
    Color = link_color(link_type(L, ById)),
    #{id => Id, selected => false,
      d => chain_path(Mode, link_points(L, ById)),
      color => Color,
      points => [#{index => integer_to_binary(I), x => num(X), y => num(Y), color => Color}
                 || {I, {X, Y}} <- lists:enumerate(0, Points)]}.

group_view(#{id := Id, title := Title, bounds := {X, Y, W, H}, color := Color}, RO) ->
    #{id => Id,
      title => case Title of undefined -> <<"Group">>; _ -> Title end,
      style => iolist_to_binary(
                 [<<"transform:translate3d(">>, num(X), <<"px,">>, num(Y), <<"px,0);">>,
                  <<"width:">>, num(W), <<"px;height:">>, num(H), <<"px;">>,
                  [[<<"--ah-ng-group-color:">>, Color, <<";">>] || Color =/= undefined]]),
      editable => not RO}.

title(#{title := T}) when T =/= undefined -> T;
title(#{type := T}) when T =/= undefined -> T;
title(#{id := Id}) -> Id.

%% {node id => {connected input indices, connected output indices}}
connections(Links) ->
    lists:foldl(fun(#{source := {S, I}, target := {T, J}}, Acc) ->
                        {SIn, SOut} = maps:get(S, Acc, {[], []}),
                        Acc1 = Acc#{S => {SIn, [I | SOut]}},
                        {TIn, TOut} = maps:get(T, Acc1, {[], []}),
                        Acc1#{T => {[J | TIn], TOut}}
                end, #{}, Links).

%%%===================================================================
%%% Colours (sigil node_graph/colors: data type colours are user data,
%%% they do not follow the palette)
%%%===================================================================

-define(FALLBACK, <<"var(--ah-datatype-default, #aaa)">>).

norm_type(undefined) -> <<"*">>;
norm_type(T) ->
    case string:trim(T) of
        <<>> -> <<"*">>;
        S -> string:uppercase(S)
    end.

type_color(T) ->
    case norm_type(T) of
        <<"*">> -> ?FALLBACK;
        N -> <<"var(--ah-datatype-", (css_name(N))/binary, ", ", ?FALLBACK/binary, ")">>
    end.

css_name(N) -> re:replace(N, <<"[^A-Z0-9_]">>, <<"_">>, [global, {return, binary}]).

slot_colors(T) ->
    case norm_type(T) of
        <<"*">> -> [?FALLBACK];
        N ->
            Parts = [P || P0 <- binary:split(N, <<",">>, [global]),
                          P <- [string:trim(P0)], P =/= <<>>],
            case lists:sublist(Parts, 3) of
                [] -> [?FALLBACK];
                Ps -> [type_color(P) || P <- Ps]
            end
    end.

link_color(T) -> hd(slot_colors(T)).

link_type(#{source := {S, I}}, ById) ->
    case ById of
        #{S := #{outputs := Outs}} when I < length(Outs) ->
            maps:get(type, lists:nth(I + 1, Outs));
        _ -> undefined
    end.

%%%===================================================================
%%% Geometry (sigil node_graph/geometry)
%%%===================================================================

node_width(N) ->
    case maps:get(width, N) of
        undefined -> ?DEFAULT_W;
        W -> max(?MIN_W, W)
    end.

slot_position(#{pos := {X, Y}, collapsed := Collapsed} = N, Kind, I) ->
    Sx = case Kind of input -> X; output -> X + node_width(N) end,
    case Collapsed of
        true -> {Sx, Y + ?TITLE_H / 2};
        false -> {Sx, Y + ?TITLE_H + ?PAD_TOP + ?SLOT_H * I + ?SLOT_H / 2}
    end.

endpoints_known(#{source := {S, _}, target := {T, _}}, ById) ->
    maps:is_key(S, ById) andalso maps:is_key(T, ById).

link_points(#{source := {S, I}, target := {T, J}, points := Points}, ById) ->
    [slot_position(maps:get(S, ById), output, I) | Points]
        ++ [slot_position(maps:get(T, ById), input, J)].

chain_path(Mode, [First | _] = Pts) ->
    iolist_to_binary([<<"M">>, pt(First), <<" ">>,
                      lists:join(<<" ">>, [path_body(Mode, P, Q) || {P, Q} <- pairs(Pts)])]).

pairs([P, Q | Rest]) -> [{P, Q} | pairs([Q | Rest])];
pairs(_) -> [].

%% each segment leaves its left end to the right and enters its right end
%% from the left, so a reroute point makes an S bend
path_body(linear, A, B) ->
    [<<"L">>, pt(offset(A, 15)), <<" L">>, pt(offset(B, -15)), <<" L">>, pt(B)];
path_body(straight, A, B) ->
    {IAx, IAy} = IA = offset(A, 10),
    {IBx, IBy} = IB = offset(B, -10),
    Mid = num(r2(0.5 * (IAx + IBx))),
    [<<"L">>, pt(IA), <<" L">>, Mid, <<",">>, num(r2(IAy)),
     <<" L">>, Mid, <<",">>, num(r2(IBy)), <<" L">>, pt(IB), <<" L">>, pt(B)];
path_body(spline, {Ax, Ay}, {Bx, By} = B) ->
    D = max(30, math:sqrt((Bx - Ax) * (Bx - Ax) + (By - Ay) * (By - Ay)) * 0.25),
    [<<"C">>, pt({Ax + D, Ay}), <<" ">>, pt({Bx - D, By}), <<" ">>, pt(B)].

offset({X, Y}, D) -> {X + D, Y}.

pt({X, Y}) -> [num(r2(X)), <<",">>, num(r2(Y))].

r2(N) -> round(N * 100) / 100.

%% A number as JS's String(n) writes it for the values we produce:
%% integral values without a fraction, others with at most two decimals.
num(N) when is_integer(N) -> integer_to_binary(N);
num(F) when is_float(F) ->
    case F == trunc(F) of
        true -> integer_to_binary(trunc(F));
        false -> float_to_binary(r2(F), [{decimals, 2}, compact])
    end.

%%%===================================================================
%%% Layout: layered placement (longest path from the sources, then one
%%% barycentre pass to reduce crossings). sigil has no auto layout; this
%%% one is here so a graph can come without positions.
%%%===================================================================

%% @doc Place the nodes of `Graph' left to right in layers: a node's
%% layer is the longest path of links leading to it; within a layer nodes
%% are ordered by the position of the nodes feeding them. Every node gets
%% a `pos' (existing ones are replaced). Returns the graph in the input
%% form, ready to store.
-spec node_graph_layout(graph()) -> graph().
node_graph_layout(Graph) ->
    #{nodes := Placed} = place(normalize(Graph), true),
    Pos = maps:from_list([{Id, P} || #{id := Id, pos := P} <- Placed]),
    Graph#{nodes => [N#{pos => maps:get(id(Id), Pos)} || #{id := Id} = N <- given_nodes(Graph)]}.

given_nodes(Graph) ->
    [with_id(N, I) || {I, N} <- lists:enumerate(0, maps:get(nodes, Graph, []))].

with_id(#{id := _} = N, _) -> N;
with_id(N, I) when is_map(N) -> N#{id => <<"n", (integer_to_binary(I))/binary>>};
with_id(N, _) -> error({aihtml, {bad_graph_node, N}}).

%% Positions for the nodes without one (all nodes with Force).
place(#{nodes := Nodes, links := Links} = G, Force) ->
    Need = [Id || #{id := Id, pos := P} <- Nodes, Force orelse P =:= undefined],
    case Need of
        [] -> G;
        _ ->
            Placed = layered(Nodes, Links),
            G#{nodes => [case Force orelse P =:= undefined of
                             true -> N#{pos => maps:get(Id, Placed)};
                             false -> N
                         end || #{id := Id, pos := P} = N <- Nodes]}
    end.

layered(Nodes, Links) ->
    Ids = [Id || #{id := Id} <- Nodes],
    Known = sets:from_list(Ids),
    Edges = [{S, T} || #{source := {S, _}, target := {T, _}} <- Links, S =/= T,
                       sets:is_element(S, Known), sets:is_element(T, Known)],
    Max = length(Ids) - 1,
    Rank = relax(maps:from_list([{Id, 0} || Id <- Ids]), Edges, length(Ids), Max),
    Layers0 = lists:foldl(fun(Id, Acc) ->
                                  R = maps:get(Id, Rank),
                                  Acc#{R => maps:get(R, Acc, []) ++ [Id]}
                          end, #{}, Ids),
    Ranks = lists:sort(maps:keys(Layers0)),
    Layers = order_layers(Ranks, Layers0, Edges),
    ById = maps:from_list([{Id, N} || #{id := Id} = N <- Nodes]),
    Heights = [lists:sum([est_height(maps:get(Id, ById)) + 40 || Id <- maps:get(R, Layers)]) - 40
               || R <- Ranks],
    Tallest = lists:max(Heights),
    {_, Pos} = lists:foldl(
                 fun({R, H}, {X, Acc}) ->
                         Col = maps:get(R, Layers),
                         W = lists:max([node_width(maps:get(Id, ById)) || Id <- Col]),
                         {_, Acc1} = lists:foldl(
                                       fun(Id, {Y, A}) ->
                                               {Y + est_height(maps:get(Id, ById)) + 40,
                                                A#{Id => {X, Y}}}
                                       end, {40 + (Tallest - H) div 2, Acc}, Col),
                         {X + W + 100, Acc1}
                 end, {40, #{}}, lists:zip(Ranks, Heights)),
    Pos.

relax(Rank, _Edges, 0, _Max) -> Rank;
relax(Rank, Edges, Rounds, Max) ->
    Next = lists:foldl(fun({S, T}, R) ->
                               New = min(Max, maps:get(S, R) + 1),
                               case New > maps:get(T, R) of
                                   true -> R#{T => New};
                                   false -> R
                               end
                       end, Rank, Edges),
    case Next =:= Rank of
        true -> Rank;
        false -> relax(Next, Edges, Rounds - 1, Max)
    end.

order_layers([], Layers, _) -> Layers;
order_layers([First | Rest], Layers, Edges) ->
    {_, Out} = lists:foldl(
                 fun(R, {Prev, Acc}) ->
                         Index = maps:from_list(lists:zip(maps:get(Prev, Acc),
                                                          lists:seq(1, length(maps:get(Prev, Acc))))),
                         Col = maps:get(R, Acc),
                         Keyed = [{bary(Id, Index, Edges, P), P, Id}
                                  || {P, Id} <- lists:enumerate(Col)],
                         {R, Acc#{R => [Id || {_, _, Id} <- lists:sort(Keyed)]}}
                 end, {First, Layers}, Rest),
    Out.

bary(Id, Index, Edges, Own) ->
    case [maps:get(S, Index) || {S, T} <- Edges, T =:= Id, maps:is_key(S, Index)] of
        [] -> Own + 0.0;
        Ps -> lists:sum(Ps) / length(Ps)
    end.

est_height(#{collapsed := true}) -> ?TITLE_H;
est_height(#{height := H}) when H =/= undefined -> H;
est_height(#{inputs := Ins, outputs := Outs, widgets := Ws, body := Body}) ->
    ?TITLE_H + ?PAD_TOP + ?SLOT_H * max(length(Ins), length(Outs)) + ?PAD_BOTTOM
        + 30 * length(Ws) + case Body of undefined -> 0; _ -> 40 end.

%%%===================================================================
%%% Normalising the graph
%%%===================================================================

normalize(G) when is_map(G) ->
    Nodes = [norm_node(N, I) || {I, N} <- lists:enumerate(0, list(nodes, maps:get(nodes, G, [])))],
    Links = [norm_link(L, I) || {I, L} <- lists:enumerate(0, list(links, maps:get(links, G, [])))],
    Groups = [group(Gr, I) || {I, Gr} <- lists:enumerate(0, list(groups, maps:get(groups, G, [])))],
    dups(nodes, [maps:get(id, N) || N <- Nodes]),
    dups(links, [maps:get(id, L) || L <- Links]),
    #{nodes => Nodes, links => Links, groups => Groups};
normalize(G) -> error({aihtml, {bad_graph, G}}).

list(_, L) when is_list(L) -> L;
list(K, V) -> error({aihtml, {bad_graph, {K, V}}}).

dups(Kind, Ids) ->
    case Ids -- lists:usort(Ids) of
        [] -> ok;
        [D | _] -> error({aihtml, {duplicate_graph_id, Kind, D}})
    end.

norm_node(#{} = N, I) ->
    Id = case maps:find(id, N) of
             {ok, V} -> id(V);
             error -> <<"n", (integer_to_binary(I))/binary>>
         end,
    #{id => Id,
      type => opt_text(maps:get(type, N, undefined)),
      title => opt_text(maps:get(title, N, undefined)),
      pos => case maps:get(pos, N, undefined) of
                 undefined -> undefined;
                 P -> point(P, N)
             end,
      width => opt_num(maps:get(width, N, undefined), N),
      height => opt_num(maps:get(height, N, undefined), N),
      collapsed => bool(maps:get(collapsed, N, false), N),
      color => color(maps:get(color, N, undefined)),
      inputs => [slot(S) || S <- list(inputs, maps:get(inputs, N, []))],
      outputs => [slot(S) || S <- list(outputs, maps:get(outputs, N, []))],
      widgets => [?H:render_binary(W) || W <- list(widgets, maps:get(widgets, N, []))],
      body => case maps:get(body, N, undefined) of
                  undefined -> undefined;
                  B -> ?H:render_binary(B)
              end,
      data => maps:get(data, N, undefined)};
norm_node(N, _) -> error({aihtml, {bad_graph_node, N}}).

slot(#{name := Name} = S) ->
    Shape = maps:get(shape, S, undefined),
    lists:member(Shape, [undefined, circle, square, grid, hollow,
                         <<"circle">>, <<"square">>, <<"grid">>, <<"hollow">>])
        orelse error({aihtml, {bad_graph_slot_shape, Shape}}),
    #{name => text(Name),
      type => opt_text(maps:get(type, S, undefined)),
      label => opt_text(maps:get(label, S, undefined)),
      optional => bool(maps:get(optional, S, false), S),
      shape => opt_text(Shape)};
slot({Name, Type}) -> slot(#{name => Name, type => Type});
slot(Name) when is_binary(Name); is_atom(Name) -> slot(#{name => Name});
slot(S) -> error({aihtml, {bad_graph_slot, S}}).

norm_link(#{source := S, target := T} = L, I) ->
    Id = case maps:find(id, L) of
             {ok, V} -> id(V);
             error -> <<"l", (integer_to_binary(I))/binary>>
         end,
    #{id => Id, source => endpoint(S, L), target => endpoint(T, L),
      points => [point(P, L) || P <- list(points, maps:get(points, L, []))]};
norm_link({S, T}, I) -> norm_link(#{source => S, target => T}, I);
norm_link(L, _) -> error({aihtml, {bad_graph_link, L}}).

endpoint({Node, Idx}, _) when is_integer(Idx), Idx >= 0 -> {id(Node), Idx};
endpoint([Node, Idx], _) when is_integer(Idx), Idx >= 0 -> {id(Node), Idx};
endpoint(_, L) -> error({aihtml, {bad_graph_link, L}}).

group(#{bounds := B} = G, I) ->
    Bounds = case B of
                 {X, Y, W, H} -> {X, Y, W, H};
                 [X, Y, W, H] -> {X, Y, W, H};
                 _ -> error({aihtml, {bad_graph_group, G}})
             end,
    lists:all(fun is_number/1, tuple_to_list(Bounds)) orelse error({aihtml, {bad_graph_group, G}}),
    #{id => case maps:find(id, G) of
                {ok, V} -> id(V);
                error -> <<"g", (integer_to_binary(I + 1))/binary>>
            end,
      title => opt_text(maps:get(title, G, undefined)),
      bounds => Bounds,
      color => color(maps:get(color, G, undefined))};
group(G, _) -> error({aihtml, {bad_graph_group, G}}).

library_item(I) ->
    (is_map(I) andalso maps:is_key(type, I)) orelse error({aihtml, {bad_graph_library_item, I}}),
    Type = maps:get(type, I),
    N = norm_node(maps:remove(id, I), 0),
    maps:filter(fun(_, V) -> V =/= undefined end,
                #{type => text(Type),
                  label => opt_text(maps:get(label, I, undefined)),
                  category => opt_text(maps:get(category, I, undefined)),
                  node => node_json(N)}).

id(V) when is_binary(V); is_atom(V); is_integer(V); is_list(V) ->
    case text(V) of
        <<>> -> error({aihtml, {bad_graph_id, V}});
        B -> B
    end;
id(V) -> error({aihtml, {bad_graph_id, V}}).

point({X, Y}, _) when is_number(X), is_number(Y) -> {X, Y};
point([X, Y], _) when is_number(X), is_number(Y) -> {X, Y};
point(_, Where) -> error({aihtml, {bad_graph_point, Where}}).

opt_num(undefined, _) -> undefined;
opt_num(N, _) when is_number(N), N > 0 -> N;
opt_num(_, Where) -> error({aihtml, {bad_graph_size, Where}}).

bool(B, _) when is_boolean(B) -> B;
bool(_, Where) -> error({aihtml, {bad_graph_flag, Where}}).

%% A colour goes into a style attribute: letters, digits, # ( ) , . % - and
%% spaces only, so it cannot end the declaration.
color(undefined) -> undefined;
color(C) ->
    B = text(C),
    case re:run(B, <<"^[#a-zA-Z0-9(),.% -]+$">>) of
        {match, _} -> B;
        nomatch -> error({aihtml, {bad_graph_color, C}})
    end.

%%%===================================================================
%%% JSON (the graph as the browser and the postbacks see it)
%%%===================================================================

graph_json(#{nodes := Nodes, links := Links, groups := Groups}) ->
    #{nodes => [node_json(N) || N <- Nodes],
      links => [link_json(L) || L <- Links],
      groups => [group_json(G) || G <- Groups]}.

node_json(N) ->
    {X, Y} = case maps:get(pos, N) of
                 undefined -> {0, 0};
                 P -> P
             end,
    compact(#{id => maps:get(id, N), type => maps:get(type, N),
              title => maps:get(title, N), pos => [X, Y],
              width => maps:get(width, N), height => maps:get(height, N),
              collapsed => maps:get(collapsed, N), color => maps:get(color, N),
              inputs => [slot_json(S) || S <- maps:get(inputs, N)],
              outputs => [slot_json(S) || S <- maps:get(outputs, N)],
              data => maps:get(data, N),
              html => case {maps:get(widgets, N), maps:get(body, N)} of
                          {[], undefined} -> undefined;
                          {Ws, B} -> compact(#{widgets => Ws, body => B})
                      end}).

slot_json(S) -> compact(S).

link_json(#{id := Id, source := {S, I}, target := {T, J}, points := Ps}) ->
    compact(#{id => Id, source => [S, I], target => [T, J],
              points => case Ps of
                            [] -> undefined;
                            _ -> [[X, Y] || {X, Y} <- Ps]
                        end}).

group_json(#{id := Id, title := T, bounds := {X, Y, W, H}, color := C}) ->
    compact(#{id => Id, title => T, bounds => [X, Y, W, H], color => C}).

compact(M) -> maps:filter(fun(_, V) -> V =/= undefined andalso V =/= false end, M).

%% The graph in data-ah-value drops the widget HTML (the cards hold it);
%% only nodes sent by set_node_graph/3 or library entries carry `html'.
json_text(#{nodes := Nodes} = G) ->
    iolist_to_binary(aihtml_json:encode(G#{nodes => [maps:remove(html, N) || N <- Nodes]})).

%% JSON inside <script>: "</" is written "<\/" so the data cannot close
%% the element.
script_json(Term) ->
    binary:replace(iolist_to_binary(aihtml_json:encode(Term)), <<"</">>, <<"<\\/">>, [global]).

%%%===================================================================
%%% Server-side updates
%%%===================================================================

%% @doc Replace the graph of a node graph from inside an action (after a
%% postback stored or rejected an edit, or the server changed the graph):
%% calls the behaviour method `setGraph' with the graph as JSON, widget
%% HTML rendered here. `Target' is `{id, RootId}' or the postback's event
%% (whose `id' is the root's). Nodes without `pos' are laid out. The
%% browser redraws, clears undo history and does not fire change.
-spec set_node_graph(aihtml_action:ctx(), aihtml_action:target() | aihtml_action:event(),
                     graph()) -> ok.
set_node_graph(Ctx, #{id := Id}, Graph) ->
    set_node_graph(Ctx, {id, Id}, Graph);
set_node_graph(Ctx, Target, Graph) ->
    G = graph_json(place(normalize(Graph), false)),
    aihtml_action:call(Ctx, Target, setGraph, [G]).

%% @doc Functions besides the components that the aihtml facade re-exports.
-spec facade_extras() -> [{atom(), arity()}].
facade_extras() -> [{set_node_graph, 3}, {node_graph_layout, 1}].

%%%===================================================================
%%% Records
%%%===================================================================

%% @doc The field names of the record.
-spec fields(atom()) -> [atom()].
fields(ah_node_graph) -> record_info(fields, ah_node_graph).

-spec render(element()) -> aihtml_html:html().
render(#ah_node_graph{} = R) -> render_node_graph(R).

ensure_id(R) ->
    Id = case element(3, R) of
             undefined -> <<"ah-g", (integer_to_binary(erlang:unique_integer([positive])))/binary>>;
             Id0 -> text(Id0)
         end,
    {Id, setelement(3, R, Id)}.

hidden(undefined, _) -> [];
hidden(Name, Value) -> ?H:void(input, [], [{type, hidden}, {name, Name}, {value, Value}]).

nz(undefined) -> <<>>;
nz(B) -> B.

opt_text(undefined) -> undefined;
opt_text(V) -> text(V).

text(B) when is_binary(B) -> B;
text(L) when is_list(L) -> unicode:characters_to_binary(L);
text(A) when is_atom(A) -> atom_to_binary(A);
text(I) when is_integer(I) -> integer_to_binary(I);
text(X) -> beamai_html_escape:to_binary(X, aihtml).

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => node_graph, category => data,
       signature => <<"ah_node_graph(Graph, Css, Attrs)">>,
       root => <<"ah-node-graph">>,
       flags => [read_only, minimap, auto_fit, allow_cycles, no_toolbar, no_grid],
       classes => #{read_only => [], minimap => [], auto_fit => [], allow_cycles => [],
                    no_toolbar => [], no_grid => []},
       options => [link_mode, snap, height, library, layout, label],
       behavior => <<"node-graph">>,
       events => [<<"change">>, <<"ah:selection-change">>, <<"ah:link-drop">>],
       doc => <<"A node editor: cards with typed input and output slots, links dragged "
                "between them, pan and zoom, groups, undo; the server renders the graph "
                "and every edit fires change with the graph as JSON.">>,
       option_docs =>
           #{read_only => <<"Look, select, pan and zoom; no edits.">>,
             minimap => <<"A minimap in the bottom left corner; click to centre the view.">>,
             auto_fit => <<"Fit the content into the view when the page loads.">>,
             allow_cycles => <<"Allow links that close a cycle (refused by default: "
                               "a workflow is a DAG).">>,
             no_toolbar => <<"Hide the zoom / undo / delete toolbar.">>,
             no_grid => <<"No dotted background grid.">>,
             link_mode => <<"Link shape: spline (default), linear or straight.">>,
             snap => <<"Grid in px that dragged nodes snap to (default none).">>,
             height => <<"Height in px, a CSS length, or auto for the stylesheet's "
                         "(default 400).">>,
             library => <<"Entries of the node search menu (right click on the canvas, "
                          "or drop a link on empty canvas): maps with type, label, category, "
                          "inputs, outputs, widgets, width, color.">>,
             layout => <<"auto places every node in layers; none (default) places only "
                         "nodes without pos.">>,
             label => <<"aria-label of the canvas (default \"Node graph\").">>},
       methods =>
           [#{name => getGraph, args => <<"()">>, doc => <<"Return the graph (an object).">>},
            #{name => setGraph, args => <<"(Graph)">>,
              doc => <<"Replace the graph without firing change and clear the history; "
                       "used by set_node_graph/3.">>},
            #{name => getValue, args => <<"()">>, doc => <<"Return data-ah-value (the JSON).">>},
            #{name => fitView, args => <<"()">>, doc => <<"Fit the content into the view.">>},
            #{name => undo, args => <<"()">>, doc => <<"Undo the last edit (fires change).">>},
            #{name => redo, args => <<"()">>, doc => <<"Redo (fires change).">>},
            #{name => getSelection, args => <<"()">>, doc => <<"Selected node ids.">>},
            #{name => selectNodes, args => <<"([Id])">>, doc => <<"Select nodes.">>},
            #{name => deleteSelection, args => <<"()">>,
              doc => <<"Delete the selected nodes and links (fires change).">>}]}].
