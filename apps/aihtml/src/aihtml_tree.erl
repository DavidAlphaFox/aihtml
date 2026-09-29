%%%-------------------------------------------------------------------
%%% @doc The tree, ported from sigil (data/tree). DOM and class names are
%%% sigil's, so the styles in priv/css/sigil apply unchanged.
%%%
%%%   tree(Items, Value, Css, Attrs)          expandable tree, single selection
%%%   set_children(Ctx, Event, Items)         (in an action) fill a lazy tree node
%%%
%%% The tree is value-bearing: the root carries `data-ah-value' (the
%%% selected value) and fires `change'; a `name' in Attrs goes to a hidden
%%% input.
%%%
%%% == Lazy tree nodes ==
%%%
%%% A node `#{label => ..., lazy => true}' shows an expand arrow but has no
%%% children yet. When the tree has a `load' option (an action ref
%%% `{Mod, Action, Args}'), expanding such a node for the first time POSTs
%%% the action with
%%%
%%%   Event.id                         the node's <li> id
%%%   Event.data                       #{<<"value">> => node value,
%%%                                      <<"tree">> => tree root id,
%%%                                      <<"treeId">> => node path, <<"level">> => depth}
%%%
%%% and the action answers with `set_children(Ctx, Event, Items)': the items
%%% are rendered here, by the same code as the first render, morphed into
%%% the node's group, and the behaviour method `childrenLoaded' expands the
%%% node. While the request runs the node shows a loading state.
%%%
%%% tree/4 builds an element record (#ah_tree{}, defined in
%%% include/aihtml_tree.hrl) and render/1 turns it into HTML, so pages may
%%% also write the record directly (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_tree).
-behaviour(aihtml_element).

-include("aihtml_tree.hrl").

-export([tree/4, set_children/3, render/1, fields/1, catalog/0, facade_extras/0]).

-export_type([element/0, item/0]).

-define(H, aihtml_html).
-define(E, aihtml_element).

-type html() :: aihtml_html:html().
-type css() :: aihtml_html:css().
-type attrs() :: aihtml_html:attrs().
%% A tree node: a text that is both value and label, `{Value, Label}',
%% `{Value, Label, Children}', or a map. `lazy' marks a node whose
%% children the tree's `load' action supplies when it is first expanded.
-type item() :: binary() | atom() | integer()
              | {term(), aihtml_html:html()}
              | {term(), aihtml_html:html(), [item()]}
              | #{value => term(), label => aihtml_html:html(),
                  icon => aihtml_html:html(), expanded => boolean(),
                  disabled => boolean(), lazy => boolean(),
                  items => [item()]}.
-type element() :: #ah_tree{}.

%%%===================================================================
%%% Builder
%%%===================================================================

%% @doc A tree. `Items' are nodes (see the type `item()'); `Value' is
%% the selected node's value (its ancestors are rendered expanded). Css:
%% `disabled'. Options: `toggle_mode' (click (default): a click on a row
%% expands it; dblclick: only a double click or the arrow does),
%% `animation' (slide (default) | none), `load' (an action ref that
%% supplies the children of lazy nodes, see the module doc).
-spec tree([item()], term(), css(), attrs()) -> #ah_tree{}.
tree(Items, Value, Css, Attrs) ->
    ?E:build(?MODULE, #ah_tree{items = Items, value = Value}, Css, Attrs).

%% @doc The field names of this component's record.
-spec fields(atom()) -> [atom()].
fields(ah_tree) -> record_info(fields, ah_tree).

%% @doc Functions besides the components that the aihtml facade re-exports.
-spec facade_extras() -> [{atom(), arity()}].
facade_extras() -> [{set_children, 3}].

%%%===================================================================
%%% Rendering
%%%===================================================================

-spec render(element()) -> html().
render(#ah_tree{} = R) -> render_tree(R).

render_tree(#ah_tree{items = Items0, value = Value, name = Name, disabled = Disabled,
                     toggle_mode = Mode, animation = Anim, load = Load} = R0) ->
    lists:member(Mode, [click, dblclick])
        orelse error({aihtml, {bad_option, toggle_mode, Mode}}),
    lists:member(Anim, [slide, none])
        orelse error({aihtml, {bad_option, animation, Anim}}),
    case Load of
        undefined -> ok;
        {M, A, _} when is_atom(M), is_atom(A) -> ok;
        _ -> error({aihtml, {bad_option, load, Load}})
    end,
    {Id, R} = ensure_id(R0),
    Classes = ?E:classes(?MODULE, R),
    Nodes = [tnode(I) || I <- Items0],
    Sel = case Value of undefined -> undefined; _ -> text(Value) end,
    SelPath = case Sel of undefined -> none; _ -> find_path(Sel, Nodes) end,
    FocusPath = case SelPath of
                    none -> first_enabled(Nodes);
                    _ -> SelPath
                end,
    Ctx = #{tree => Id, prefix => Id, path => [], level => 1,
            selected => SelPath, focus => FocusPath},
    Cur = case SelPath of none -> <<>>; _ -> Sel end,
    ?H:el('div',
          [?H:el(ul, tree_nodes(Nodes, Ctx), [<<"ah-tree-list">>], [{role, presentation}]),
           hidden(Name, Cur)],
          Classes,
          [[{id, Id}, {role, tree},
            {aria_disabled, Disabled andalso <<"true">>},
            {data_ah, <<"tree">>}, {data_ah_value, Cur},
            {data_toggle_mode, Mode}, {data_animation, Anim},
            {data_load, case Load of
                            undefined -> undefined;
                            _ -> aihtml_action:token(Load)
                        end}],
           ?E:root_attrs(R, change)]).

tnode(#{} = M) ->
    Label = case M of
                #{label := L} -> L;
                #{value := V} -> text(V);
                _ -> error({aihtml, {bad_tree_item, M}})
            end,
    Value = case M of
                #{value := V1} -> text(V1);
                _ -> label_text(Label, M)
            end,
    Lazy = bool(lazy, maps:get(lazy, M, false)),
    Children = maps:get(items, M, []),
    is_list(Children) orelse error({aihtml, {bad_tree_item, M}}),
    #{value => Value, label => Label, icon => maps:get(icon, M, undefined),
      expanded => bool(expanded, maps:get(expanded, M, false)),
      disabled => bool(disabled, maps:get(disabled, M, false)),
      lazy => Lazy andalso Children =:= [],
      items => [tnode(C) || C <- Children]};
tnode({V, L}) -> tnode(#{value => V, label => L});
tnode({V, L, Children}) when is_list(Children) -> tnode(#{value => V, label => L, items => Children});
tnode(V) when is_binary(V); is_atom(V); is_integer(V) -> tnode(#{value => V, label => text(V)});
tnode([_ | _] = V) ->
    case io_lib:printable_unicode_list(V) of
        true -> tnode(#{value => V, label => text(V)});
        false -> error({aihtml, {bad_tree_item, V}})
    end;
tnode(Other) -> error({aihtml, {bad_tree_item, Other}}).

label_text(L, _) when is_binary(L); is_atom(L); is_integer(L) -> text(L);
label_text(L, M) when is_list(L) ->
    case io_lib:printable_unicode_list(L) of
        true -> text(L);
        false -> error({aihtml, {tree_item_needs_value, M}})
    end;
label_text(_, M) -> error({aihtml, {tree_item_needs_value, M}}).

bool(_, B) when is_boolean(B) -> B;
bool(K, V) -> error({aihtml, {bad_option, K, V}}).

%% The index path of the first node with value V, or none.
find_path(V, Nodes) -> find_path(V, Nodes, 0, []).

find_path(_, [], _, _) -> none;
find_path(V, [#{value := V} | _], I, Rev) -> lists:reverse([I | Rev]);
find_path(V, [#{items := Kids} | Rest], I, Rev) ->
    case find_path(V, Kids, 0, [I | Rev]) of
        none -> find_path(V, Rest, I + 1, Rev);
        P -> P
    end.

first_enabled(Nodes) ->
    case [I || {I, #{disabled := false}} <- lists:zip(lists:seq(0, length(Nodes) - 1), Nodes)] of
        [I | _] -> [I];
        [] -> none
    end.

tree_nodes(Nodes, #{path := Path} = Ctx) ->
    [tree_node(N, Ctx#{path := Path ++ [I]})
     || {I, N} <- lists:zip(lists:seq(0, length(Nodes) - 1), Nodes)].

tree_node(#{value := V, label := Label, icon := Icon, disabled := Disabled,
            lazy := Lazy, items := Kids} = N,
          #{tree := Tree, prefix := Prefix, path := Path, level := Level,
            selected := SelPath, focus := FocusPath} = Ctx) ->
    PathBin = path_bin(Path),
    LiId = <<Prefix/binary, "-", (integer_to_binary(lists:last(Path)))/binary>>,
    HasKids = Kids =/= [] orelse Lazy,
    %% the ancestors of the selected node are open
    Open = HasKids andalso not Lazy andalso
        (maps:get(expanded, N) orelse is_prefix(Path, SelPath)),
    Selected = Path =:= SelPath,
    Toggle = case {HasKids, Open} of
                 {false, _} -> [<<"ah-tree-toggle">>, <<"ah-tree-toggle-leaf">>];
                 {true, true} -> [<<"ah-tree-toggle">>, <<"ah-tree-toggle-open">>];
                 {true, false} -> [<<"ah-tree-toggle">>]
             end,
    Row = ?H:el('div',
                [?H:el(span, <<"▶"/utf8>>, Toggle, [{aria_hidden, <<"true">>}]),
                 case Icon of
                     undefined -> [];
                     _ -> ?H:el(span, Icon, [<<"ah-tree-icon">>], [{aria_hidden, <<"true">>}])
                 end,
                 ?H:el(span, Label, [<<"ah-tree-label">>], [])],
                [<<"ah-tree-row">>, [<<"ah-tree-row-disabled">> || Disabled],
                 [<<"ah-tree-row-selected">> || Selected]],
                []),
    Group = case HasKids of
                false -> [];
                true ->
                    ?H:el(ul, tree_nodes(Kids, Ctx#{prefix := LiId, level := Level + 1}),
                          [<<"ah-tree-list">>],
                          [{role, group}, {id, <<LiId/binary, "-g">>},
                           {style, case Open of
                                       true -> undefined;
                                       false -> <<"display:none;">>
                                   end}])
            end,
    ?H:el(li, [Row, Group],
          [<<"ah-tree-item">>, [<<"ah-tree-item-leaf">> || not HasKids]],
          [{id, LiId}, {role, treeitem}, {aria_level, Level},
           {data_tree_id, PathBin}, {data_value, V},
           {aria_expanded, HasKids andalso atom_to_binary(Open, utf8)},
           {aria_disabled, Disabled andalso <<"true">>},
           {aria_selected, Selected andalso <<"true">>},
           {tabindex, case Path =:= FocusPath of true -> <<"0">>; false -> <<"-1">> end},
           {data_lazy, Lazy andalso <<"true">>},
           {data_tree, Lazy andalso Tree},
           {data_level, Lazy andalso Level}]).

is_prefix(_, none) -> false;
is_prefix(P, Sel) -> length(P) < length(Sel) andalso lists:prefix(P, Sel).

path_bin(Path) -> iolist_to_binary(lists:join($-, [integer_to_binary(I) || I <- Path])).

%% @doc Answer a tree's `load' action: render `Items' as the children of
%% the lazy node that fired it and expand the node. `Event' is the load
%% action's event. Sends two operations: the rendered nodes morphed into
%% the node's group (`<node id>-g', morph_inner), and a call of the
%% behaviour method `childrenLoaded' on the tree. An empty list turns the
%% node into a leaf.
-spec set_children(aihtml_action:ctx(), aihtml_action:event(), [item()]) -> ok.
set_children(Ctx, #{id := NodeId0, data := Data}, Items) ->
    NodeId = text(NodeId0),
    Tree = maps:get(<<"tree">>, Data),
    Level = binary_to_integer(maps:get(<<"level">>, Data)),
    Path = [binary_to_integer(P) || P <- binary:split(maps:get(<<"treeId">>, Data),
                                                      <<"-">>, [global])],
    Html = tree_nodes([tnode(I) || I <- Items],
                      #{tree => text(Tree), prefix => NodeId, path => Path,
                        level => Level + 1, selected => none, focus => none}),
    aihtml_action:html(Ctx, {id, <<NodeId/binary, "-g">>}, Html, morph_inner),
    aihtml_action:call(Ctx, {id, Tree}, childrenLoaded, [NodeId]).

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => tree, category => data,
       signature => <<"tree(Items, Value, Css, Attrs)">>,
       root => <<"ah-tree">>, flags => [disabled],
       options => [toggle_mode, animation, load],
       behavior => <<"tree">>,
       events => [<<"change">>, <<"ah:expand">>, <<"ah:collapse">>, <<"ah:item-click">>,
                  <<"ah:load">>],
       doc => <<"A hierarchical list: expand and collapse nodes, select one with the "
                "mouse or the keyboard; children of lazy nodes come from the server.">>,
       option_docs =>
           #{disabled => <<"Disable the whole tree.">>,
             toggle_mode => <<"click (default): a click on a row expands or collapses it; "
                              "dblclick: a double click or a click on the arrow does.">>,
             animation => <<"slide (default) or none: how a node opens and closes.">>,
             load => <<"Action ref {Module, Action, Args} run when a lazy node is first "
                       "expanded; it answers with set_children/3.">>},
       methods =>
           [#{name => setValue, args => <<"(Value)">>,
              doc => <<"Select the node with this value (its ancestors open) without firing change.">>},
            #{name => getValue, args => <<"()">>, doc => <<"Return the selected value.">>},
            #{name => expand, args => <<"(Value)">>, doc => <<"Expand the node with this value.">>},
            #{name => collapse, args => <<"(Value)">>, doc => <<"Collapse the node with this value.">>},
            #{name => expandAll, args => <<"()">>, doc => <<"Expand every node (lazy ones stay closed).">>},
            #{name => collapseAll, args => <<"()">>, doc => <<"Collapse every node.">>},
            #{name => ensureVisible, args => <<"(Value)">>,
              doc => <<"Expand the ancestors of the node with this value.">>},
            #{name => childrenLoaded, args => <<"(NodeId)">>,
              doc => <<"Called by set_children/3: end the loading state and expand the node.">>}]}].

%%%===================================================================
%%% Internal
%%%===================================================================

%% The root needs an id: lazy nodes name it and node ids derive from it.
ensure_id(R) ->
    Id = case element(3, R) of
             undefined -> <<"ah-t", (integer_to_binary(erlang:unique_integer([positive])))/binary>>;
             Id0 -> text(Id0)
         end,
    {Id, setelement(3, R, Id)}.

hidden(undefined, _) -> [];
hidden(Name, Value) ->
    ?H:void(input, [], [{type, hidden}, {name, Name}, {value, Value}, {data_ah_input, true}]).

text(undefined) -> <<>>;
text(B) when is_binary(B) -> B;
text(L) when is_list(L) ->
    case unicode:characters_to_binary(L) of
        B when is_binary(B) -> B;
        _ -> error({aihtml, {bad_text, L}})
    end;
text(X) -> beamai_html_escape:to_binary(X, aihtml).
