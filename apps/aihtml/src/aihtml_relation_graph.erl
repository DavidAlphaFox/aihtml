%%%-------------------------------------------------------------------
%%% @doc The relation graph, ported from sigil (data/relation_graph and
%%% relation-graph.model): a node-link graph (echarts graph or tree
%%% series, force, circular, fixed or tree layout) in a panel with a
%%% toolbar, a detail card and loading / error / empty states. The graph
%%% is drawn by a nested chart (see aihtml_chart and aihtml_lib_chart);
%%% the panel's behaviour is assets/js/components/relation_graph.js.
%%%
%%% relation_graph fires 'ah:select' with the selected node id in
%%% `data-ah-value', and 'ah:refresh' when its refresh button is pressed
%%% (bind an action to it that answers with aihtml_chart:chart_update/3
%%% or a morph).
%%%
%%% relation_graph/3 builds an element record (#ah_relation_graph{},
%%% defined in include/aihtml_relation_graph.hrl) and render/1 turns it
%%% into HTML, so pages may also write the record directly
%%% (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_relation_graph).
-behaviour(aihtml_element).

-include("aihtml_relation_graph.hrl").

-export([relation_graph/3, option/1, render/1, fields/1, catalog/0]).

-export_type([element/0, graph/0, graph_node/0, edge/0, category/0]).

-import(aihtml_lib_chart, [island/1, renderer/1, size_style/2, bool/2, list/2, text/1]).

-define(H, aihtml_html).

%% sigil's default palette for relation graph categories.
-define(PALETTE, [<<"--ah-color-primary">>, <<"--ah-color-success">>,
                  <<"--ah-color-warning">>, <<"--ah-color-error">>,
                  <<"--ah-color-info">>, <<"--ah-color-secondary">>]).

-define(ICON_FIT, <<"<svg viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"1.8\" "
                    "stroke-linecap=\"round\" stroke-linejoin=\"round\" aria-hidden=\"true\">"
                    "<path d=\"M8 3H5a2 2 0 00-2 2v3M16 3h3a2 2 0 012 2v3M8 21H5a2 2 0 01-2-2v-3"
                    "M16 21h3a2 2 0 002-2v-3\"/></svg>">>).
-define(ICON_REFRESH, <<"<svg viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" "
                        "stroke-width=\"1.8\" stroke-linecap=\"round\" stroke-linejoin=\"round\" "
                        "aria-hidden=\"true\"><path d=\"M21 12a9 9 0 11-2.64-6.36M21 3v6h-6\"/></svg>">>).
-define(ICON_CLOSE, <<"<svg viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" "
                      "stroke-width=\"1.8\" stroke-linecap=\"round\" stroke-linejoin=\"round\" "
                      "aria-hidden=\"true\"><path d=\"M18 6L6 18M6 6l12 12\"/></svg>">>).

-define(E, aihtml_element).
-define(L, aihtml_lib_chart).

-type css() :: aihtml_html:css().
-type attrs() :: aihtml_html:attrs().
-type html() :: aihtml_html:html().
-type element() :: #ah_relation_graph{}.
-type text() :: aihtml_lib_chart:text().
%% A relation graph node: an id that is also its label, `{Id, Label}' or a
%% map. `category' is a category name or index, `root' draws it larger,
%% `parent' links it to its parent in the tree layout when there are no
%% edges, `x'/`y' place it in the fixed layout.
-type graph_node() :: text()
                    | {text(), text()}
                    | #{id := text(), label => text(),
                        category => text() | non_neg_integer(),
                        root => boolean(), parent => text(),
                        x => number(), y => number(), collapsed => boolean(),
                        item_style => aihtml_lib_chart:option()}.
%% A relation graph edge: `{Source, Target}', `{Source, Target, Label}' or
%% a map; `kind => dashed' draws a dashed line.
-type edge() :: {text(), text()}
              | {text(), text(), text()}
              | #{source := text(), target := text(),
                  label => text(), kind => solid | dashed,
                  line_style => aihtml_lib_chart:option(), symbol => binary(),
                  symbol_size => number()}.
%% A node category: its name, or a map with a colour.
-type category() :: text() | #{name := text(), color => binary()}.
%% The data of a relation graph; `{Nodes, Edges}' is short for a map
%% without categories.
-type graph() :: #{nodes := [graph_node()], edges => [edge()],
                   categories => [category()]}
               | {[graph_node()], [edge()]}.

%%%===================================================================
%%% Builder
%%%===================================================================

%% @doc A relation graph. `Graph' is `#{nodes, edges, categories}' (or
%% `{Nodes, Edges}'), in sigil's vocabulary: nodes `#{id, label,
%% category, root, parent, x, y}', edges `#{source, target, label, kind}'.
%% Css: layout `force' (default), `circular', `fixed' (the nodes' x / y)
%% or `tree'; tree orientation `lr' (default), `tb', `rl', `bt'; node
%% shape `circle' (default), `square', `round_rect'; `directed' (arrows),
%% `loading'. Options: `selected', `focus' (a node id to centre),
%% `details' (#{NodeId => Html} shown in the detail card when that node
%% is selected), `edge_labels' (auto | boolean), `roam', `toolbar',
%% `error', `empty_text', `height' (default 420), `width', `renderer'.
-spec relation_graph(graph(), css(), attrs()) -> #ah_relation_graph{}.
relation_graph(Graph, Css, Attrs) ->
    ?E:build(?MODULE, #ah_relation_graph{graph = Graph}, Css, Attrs).

%% @doc The field names of the record.
-spec fields(atom()) -> [atom()].
fields(ah_relation_graph) -> record_info(fields, ah_relation_graph).

%% @doc Internal: the echarts option of a record (aihtml_chart:chart_option/1
%% dispatches here), after the same checks as render/1.
-spec option(element()) -> aihtml_lib_chart:option().
option(#ah_relation_graph{graph = G, layout = Layout} = R) ->
    lists:member(Layout, [force, circular, fixed, tree])
        orelse error({aihtml, {bad_option, layout, Layout}}),
    Data = graph_data(G),
    case Layout of
        tree -> tree_option(R, Data);
        _ -> network_option(R, Data)
    end.

%% sigil's graph-option: force / circular / fixed (sigil's :none) layouts.
network_option(#ah_relation_graph{layout = Layout, node_shape = Shape, directed = Directed,
                                  edge_labels = EdgeLabels, roam = Roam},
               {Nodes, Edges, Cats}) ->
    [bool(K, V) || {K, V} <- [{directed, Directed}, {roam, Roam}]],
    lists:member(Shape, [circle, square, round_rect])
        orelse error({aihtml, {bad_option, node_shape, Shape}}),
    EdgeLabels =:= auto orelse is_boolean(EdgeLabels)
        orelse error({aihtml, {bad_option, edge_labels, EdgeLabels}}),
    ShowEdgeLabels = case EdgeLabels of
                         auto -> lists:any(fun(#{label := L}) -> L =/= undefined end, Edges);
                         _ -> EdgeLabels
                     end,
    Palette = case Cats of
                  [] -> ?PALETTE;
                  _ -> [maps:get(color, C, lists:nth((I - 1) rem length(?PALETTE) + 1, ?PALETTE))
                        || {I, C} <- lists:enumerate(Cats)]
              end,
    Series0 = #{type => graph,
                layout => case Layout of
                              circular -> circular;
                              fixed -> none;
                              force -> force
                          end,
                roam => Roam,
                draggable => true,
                %% room for the labels, the toolbar and the legend
                top => 40, left => 40, right => 40,
                bottom => case Cats of [] -> 40; _ -> 56 end,
                data => [graph_node(N, Shape) || N <- Nodes],
                links => [edge_link(E) || E <- Edges],
                categories => [#{name => N} || #{name := N} <- Cats],
                label => node_label(),
                edgeLabel => #{show => ShowEdgeLabels, fontSize => 11,
                               color => <<"--ah-color-text-secondary">>,
                               formatter => <<"{c}">>},
                emphasis => #{focus => adjacency, lineStyle => #{width => 3}},
                lineStyle => #{color => <<"--ah-color-border">>, curveness => 0.08}},
    Series1 = case Directed of
                  true -> Series0#{edgeSymbol => [none, arrow], edgeSymbolSize => 9};
                  false -> Series0
              end,
    Series = case Layout of
                 force -> Series1#{force => #{repulsion => 260, edgeLength => 120,
                                              gravity => 0.08}};
                 _ -> Series1
             end,
    Opt = #{color => Palette, tooltip => #{trigger => item}, animationDuration => 400,
            series => [Series]},
    case Cats of
        [] -> Opt;
        _ -> Opt#{legend => [#{data => [N || #{name := N} <- Cats], bottom => 0,
                               left => center, icon => circle, itemWidth => 8,
                               itemHeight => 8,
                               textStyle => #{color => <<"--ah-color-text-secondary">>}}]}
    end.

node_label() ->
    #{show => true, position => inside, color => <<"--ah-color-primary-contrast">>,
      fontSize => 12, overflow => truncate}.

graph_node(#{id := Id, label := Label, category := Cat, root := Root} = N, Shape) ->
    maps:from_list(
      [{id, Id}, {name, Label}, {symbol, symbol(Shape)},
       {symbolSize, symbol_size(Shape, Label, Root)}, {label, node_label()}]
      ++ [{category, Cat} || Cat =/= undefined]
      ++ [{K, V} || K <- [x, y], V <- [maps:get(K, N, undefined)], V =/= undefined]
      ++ [{itemStyle, V} || V <- [maps:get(item_style, N, undefined)], V =/= undefined]).

symbol(square) -> rect;
symbol(round_rect) -> roundRect;
symbol(circle) -> circle.

%% Round rectangles grow with their label so that it fits.
symbol_size(round_rect, Label, Root) ->
    W = max(52, min(150, 24 + 13 * string:length(Label))),
    [W, case Root of true -> 32; false -> 27 end];
symbol_size(_, _, true) -> 46;
symbol_size(_, _, false) -> 34.

edge_link(#{source := S, target := T, kind := Kind, label := L} = E) ->
    Dashed = Kind =:= dashed,
    Base = #{type => case Dashed of true -> dashed; false -> solid end,
             color => <<"--ah-color-border">>,
             width => case Dashed of true -> 1.4; false -> 1.8 end,
             opacity => case Dashed of true -> 0.55; false -> 0.9 end},
    Style = case maps:get(line_style, E, #{}) of
                M when is_map(M) -> maps:merge(Base, M);
                Bad -> error({aihtml, {bad_edge, Bad}})
            end,
    maps:from_list([{source, S}, {target, T},
                    {value, case L of undefined -> <<>>; _ -> L end},
                    {lineStyle, Style}]
                   ++ [{symbol, V} || V <- [maps:get(symbol, E, undefined)], V =/= undefined]
                   ++ [{symbolSize, V} || V <- [maps:get(symbol_size, E, undefined)],
                                          V =/= undefined]).

%% sigil's tree-option: a tidy tree from the edges (source is the parent)
%% or, without edges, from the nodes' `parent'.
tree_option(#ah_relation_graph{orient = Orient, roam = Roam}, {Nodes, Edges, _}) ->
    bool(roam, Roam),
    lists:member(Orient, [lr, tb, rl, bt]) orelse error({aihtml, {bad_option, orient, Orient}}),
    Forest = case Edges of
                 [] -> forest(Nodes, [{P, Id} || #{id := Id, parent := P} <- Nodes,
                                                 P =/= undefined]);
                 _ -> forest(Nodes, [{S, T} || #{source := S, target := T} <- Edges])
             end,
    Root = case Forest of
               [One] -> One;
               _ -> #{name => <<>>, id => <<"__root__">>, children => Forest,
                      itemStyle => #{opacity => 0}, label => #{show => false}}
           end,
    Vertical = Orient =:= tb orelse Orient =:= bt,
    #{tooltip => #{trigger => item, triggerOn => mousemove},
      animationDuration => 400,
      series =>
          [#{type => tree, data => [Root],
             orient => string:uppercase(atom_to_binary(Orient)),
             top => <<"6%">>, bottom => <<"6%">>, left => <<"10%">>, right => <<"16%">>,
             symbol => circle, symbolSize => 12, roam => Roam,
             initialTreeDepth => -1, expandAndCollapse => true,
             itemStyle => #{color => <<"--ah-color-primary">>,
                            borderColor => <<"--ah-color-primary">>},
             lineStyle => #{color => <<"--ah-color-border">>, width => 1.4, curveness => 0.5},
             label => #{position => case Vertical of true -> top; false -> left end,
                        verticalAlign => middle, align => right, fontSize => 12,
                        color => <<"--ah-color-text">>},
             leaves => #{label => #{position => case Vertical of true -> bottom; false -> right end,
                                    verticalAlign => middle, align => left}},
             emphasis => #{focus => descendant}}]}.

%% Parent -> child links to a forest; roots are nodes nobody points to
%% (or whose parent is unknown). A node seen on the path again is cut, so
%% cycles end.
forest(Nodes, Links) ->
    ById = maps:from_list([{Id, N} || #{id := Id} = N <- Nodes]),
    Known = [{P, C} || {P, C} <- Links, maps:is_key(P, ById), maps:is_key(C, ById)],
    Kids = lists:foldl(fun({P, C}, M) -> maps:update_with(P, fun(L) -> [C | L] end, [C], M) end,
                       #{}, Known),
    HasParent = maps:from_list([{C, true} || {_, C} <- Known]),
    [T || #{id := Id} <- Nodes, not maps:is_key(Id, HasParent),
          T <- [tree_node(Id, [], ById, Kids)], T =/= none].

tree_node(Id, Seen, ById, Kids) ->
    case lists:member(Id, Seen) of
        true -> none;
        false ->
            #{label := Label} = N = maps:get(Id, ById),
            Children = [T || C <- lists:reverse(maps:get(Id, Kids, [])),
                             T <- [tree_node(C, [Id | Seen], ById, Kids)], T =/= none],
            maps:from_list([{name, Label}, {id, Id}]
                           ++ [{children, Children} || Children =/= []]
                           ++ [{collapsed, true} || maps:get(collapsed, N, false)])
    end.

%% The 0-based position of X in L, or false.
index_of(_, [], _) -> false;
index_of(X, [X | _], I) -> I;
index_of(X, [_ | T], I) -> index_of(X, T, I + 1).

%%%===================================================================
%%% Rendering
%%%===================================================================

-spec render(element()) -> html().
render(#ah_relation_graph{layout = Layout, loading = Loading, selected = Sel0,
                          focus = Focus0, details = Details0, error = Err,
                          empty_text = Empty, toolbar = Toolbar, width = W,
                          height = H, renderer = Renderer} = R) ->
    Classes = ?E:classes(?MODULE, R),
    [bool(K, V) || {K, V} <- [{loading, Loading}, {toolbar, Toolbar}]],
    Option = option(R),
    {Nodes, _, _} = graph_data(R#ah_relation_graph.graph),
    Sel = opt_text(Sel0),
    Focus = opt_text(Focus0),
    is_map(Details0) orelse error({aihtml, {bad_option, details, Details0}}),
    Details = maps:from_list([{text(K), V} || K := V <- Details0]),
    State = if
                Loading ->
                    ?H:el('div', [?H:el(span, [], [<<"ah-relation-graph__spinner">>],
                                        [{aria_hidden, <<"true">>}]),
                                  ?H:el(span, <<"Loading…"/utf8>>, [], [])],
                          [<<"ah-relation-graph__state">>], [{data_kind, loading}]);
                Err =/= undefined ->
                    ?H:el('div', ?H:el(span, Err, [], []),
                          [<<"ah-relation-graph__state">>], [{data_kind, error}, {role, alert}]);
                Nodes =:= [] ->
                    ?H:el('div', ?H:el(span, Empty, [], []),
                          [<<"ah-relation-graph__state">>], [{data_kind, empty}]);
                true -> []
            end,
    ?H:el('div',
          [?H:el('div', island(Option), [<<"ah-relation-graph__canvas">>, <<"ah-chart">>],
                 [{data_ah, <<"chart">>}, {data_ah_renderer, renderer(Renderer)},
                  {aria_hidden, <<"true">>}, {style, <<"height:100%;">>}]),
           case Toolbar of
               false -> [];
               true ->
                   ?H:el('div',
                         [tool(<<"fit">>, <<"Fit view">>, ?ICON_FIT),
                          tool(<<"refresh">>, <<"Refresh">>, ?ICON_REFRESH)],
                         [<<"ah-relation-graph__toolbar">>], [{role, toolbar}])
           end,
           ?H:el('div',
                 [?H:el(button, {safe, ?ICON_CLOSE}, [<<"ah-relation-graph__detail-close">>],
                        [{type, button}, {aria_label, <<"Close">>}]),
                  ?H:el('div',
                        [?H:el('div', Html, [<<"ah-relation-graph__detail-item">>],
                               [{data_node, Id}, {hidden, Id =/= Sel}])
                         || {Id, Html} <- lists:sort(maps:to_list(Details))],
                        [<<"ah-relation-graph__detail-body">>], [])],
                 [<<"ah-relation-graph__detail">>],
                 [{data_visible, atom_to_binary(maps:is_key(Sel, Details))}]),
           State,
           ?H:el('div', [], [<<"ah-relation-graph__live">>],
                 [{aria_live, polite}, {aria_atomic, <<"true">>}])],
          Classes,
          [[{role, group}, {aria_roledescription, <<"relation graph">>}, {tabindex, 0},
            {data_ah, <<"relation-graph">>}, {data_ah_value, Sel},
            {data_layout, Layout},
            {data_ah_focus, case Focus of <<>> -> undefined; _ -> Focus end},
            {style, size_style(W, H)}],
           ?E:root_attrs(R, 'ah:select')]).

tool(Act, Label, Icon) ->
    ?H:el(button, {safe, Icon}, [<<"ah-relation-graph__tool">>],
          [{type, button}, {data_act, Act}, {title, Label}, {aria_label, Label}]).

opt_text(undefined) -> <<>>;
opt_text(V) -> text(V).

%% Nodes, edges and categories, normalised: ids and labels are binaries,
%% categories are indexes.
graph_data({Nodes, Edges}) when is_list(Nodes), is_list(Edges) ->
    graph_data(#{nodes => Nodes, edges => Edges});
graph_data(#{nodes := Nodes0} = G) when is_list(Nodes0) ->
    maps:size(maps:without([nodes, edges, categories], G)) =:= 0
        orelse error({aihtml, {bad_graph, G}}),
    Cats = [norm_category(C) || C <- list(categories, maps:get(categories, G, []))],
    CatNames = [N || #{name := N} <- Cats],
    Nodes = [norm_node(N, CatNames) || N <- Nodes0],
    Edges = [norm_edge(E) || E <- list(edges, maps:get(edges, G, []))],
    {Nodes, Edges, Cats};
graph_data(G) -> error({aihtml, {bad_graph, G}}).

norm_category(#{name := N} = M) ->
    case M of
        #{color := C} when not is_binary(C) -> error({aihtml, {bad_color, C}});
        _ -> ok
    end,
    maps:merge(maps:with([color], M), #{name => text(N)});
norm_category(N) -> #{name => text(N)}.

norm_node(#{id := Id0} = M, Cats) ->
    Id = text(Id0),
    Label = case M of #{label := L} -> text(L); _ -> Id end,
    Cat = case M of
              #{category := C} when is_integer(C), C >= 0, C < length(Cats) -> C;
              #{category := C} when is_integer(C) -> error({aihtml, {unknown_category, C}});
              #{category := C} ->
                  case index_of(text(C), Cats, 0) of
                      false -> error({aihtml, {unknown_category, C}});
                      I -> I
                  end;
              _ -> undefined
          end,
    [is_number(V) orelse error({aihtml, {bad_node, M}}) || V <- maps:values(maps:with([x, y], M))],
    [is_boolean(V) orelse error({aihtml, {bad_node, M}})
     || V <- maps:values(maps:with([root, collapsed], M))],
    maps:merge(maps:with([x, y, item_style, collapsed], M),
               #{id => Id, label => Label, category => Cat,
                 root => maps:get(root, M, false),
                 parent => case M of #{parent := P} -> text(P); _ -> undefined end});
norm_node({Id, Label}, Cats) -> norm_node(#{id => Id, label => Label}, Cats);
norm_node(Id, Cats) when is_binary(Id); is_atom(Id); is_integer(Id) -> norm_node(#{id => Id}, Cats);
norm_node([_ | _] = Id, Cats) -> norm_node(#{id => Id}, Cats);
norm_node(Other, _) -> error({aihtml, {bad_node, Other}}).

norm_edge({S, T}) -> norm_edge(#{source => S, target => T});
norm_edge({S, T, L}) -> norm_edge(#{source => S, target => T, label => L});
norm_edge(#{source := S, target := T} = M) ->
    Kind = maps:get(kind, M, solid),
    lists:member(Kind, [solid, dashed]) orelse error({aihtml, {bad_edge, M}}),
    maps:merge(maps:with([line_style, symbol, symbol_size], M),
               #{source => text(S), target => text(T), kind => Kind,
                 label => case M of #{label := L} -> text(L); _ -> undefined end});
norm_edge(Other) -> error({aihtml, {bad_edge, Other}}).


%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => relation_graph, category => data,
       signature => <<"relation_graph(Graph, Css, Attrs)">>,
       root => <<"ah-relation-graph">>,
       groups => #{layout => {[force, circular, fixed, tree], force},
                   orient => {[lr, tb, rl, bt], lr},
                   node_shape => {[circle, square, round_rect], circle}},
       flags => [directed, loading],
       classes => #{force => [], circular => [], fixed => [], tree => [],
                    lr => [], tb => [], rl => [], bt => [],
                    circle => [], square => [], round_rect => [],
                    directed => [], loading => []},
       options => [edge_labels, roam, selected, focus, details, error, empty_text, toolbar,
                   width, height, renderer],
       behavior => <<"relation-graph">>,
       events => [<<"ah:select">>, <<"ah:node-click">>, <<"ah:refresh">> | ?L:events()],
       doc => <<"A node-link graph (force, circular, fixed or tree layout) with categories, "
                "selection, a detail card, a toolbar and loading / error / empty states.">>,
       option_docs =>
           #{directed => <<"Arrows at the target end of the edges.">>,
             loading => <<"Show the loading state over the graph.">>,
             edge_labels => <<"Show edge labels: auto (default: when an edge has one), true or false.">>,
             roam => <<"Pan and zoom with the mouse (default true).">>,
             selected => <<"The id of the selected node.">>,
             focus => <<"The id of a node to centre in view (force, circular and fixed layouts).">>,
             details => <<"#{NodeId => Html}: shown in the detail card when that node is selected.">>,
             error => <<"An error message shown instead of the graph.">>,
             empty_text => <<"Shown when there are no nodes (default \"No data\").">>,
             toolbar => <<"Show the fit and refresh buttons (default true).">>,
             width => maps:get(width, ?L:size_docs()),
             height => <<"Height in pixels or a CSS length (default 420).">>,
             renderer => maps:get(renderer, ?L:size_docs())},
       methods =>
           [#{name => select, args => <<"(Id)">>,
              doc => <<"Select a node (null clears), without firing ah:select.">>},
            #{name => getSelected, args => <<"()">>, doc => <<"Return the selected node id.">>},
            #{name => focus, args => <<"(Id)">>, doc => <<"Pan the node into the centre.">>},
            #{name => fit, args => <<"()">>, doc => <<"Redraw the graph, resetting pan and zoom.">>},
            #{name => setOption, args => <<"(Option, NotMerge)">>,
              doc => <<"Apply an echarts option to the graph (chart_update/3 sends the "
                       "option of a relation_graph record).">>}]}].
