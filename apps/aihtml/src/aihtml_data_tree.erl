%%%-------------------------------------------------------------------
%%% @doc Trees and text views, ported from sigil (data/tree,
%%% layout/nav_tree, data/diff, data/heatmap_calendar). DOM and class names
%%% are sigil's, so the styles in priv/css/sigil apply unchanged.
%%%
%%%   tree(Items, Value, Css, Attrs)          expandable tree, single selection
%%%   nav_tree(Items, Value, Css, Attrs)      grouped side navigation of links
%%%   diff(Old, New, Css, Attrs)              line or word diff, computed here
%%%   heatmap_calendar(Data, Css, Attrs)      GitHub-style contribution grid
%%%   set_children(Ctx, Event, Items)         (in an action) fill a lazy tree node
%%%
%%% tree and nav_tree are value-bearing: the root carries `data-ah-value'
%%% (the selected value / the active route) and fires `change'; a `name'
%%% in Attrs goes to a hidden input. heatmap_calendar fires 'ah:select'
%%% with the clicked date as `data-ah-value'. diff is static HTML.
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
%%% Each function builds an element record (#ah_tree{} ..., defined in
%%% include/aihtml_data_tree.hrl) and render/1 turns it into HTML, so pages
%%% may also write the records directly (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_data_tree).
-behaviour(aihtml_element).

-include("aihtml_data_tree.hrl").

-export([tree/4, nav_tree/4, diff/4, heatmap_calendar/3, set_children/3,
         render/1, fields/1, catalog/0, facade_extras/0]).
%% The diff model, for tests and for pages that want the numbers.
-export([line_rows/2, word_parts/2, split_rows/1]).

-export_type([element/0, tree_item/0, nav_item/0, diff_row/0]).

-define(H, aihtml_html).
-define(E, aihtml_element).

-type html() :: aihtml_html:html().
-type css() :: aihtml_html:css().
-type attrs() :: aihtml_html:attrs().
-type tree_item() :: ah_dt_tree_item().
-type nav_item() :: ah_dt_nav_item().
-type element() :: #ah_tree{} | #ah_nav_tree{} | #ah_diff{} | #ah_heatmap_calendar{}.
%% One line of a line diff; the numbers are 1-based, `undefined' on the
%% side the line is not on.
-type diff_row() :: #{type := ctx | add | del, text := binary(),
                      old_no := pos_integer() | undefined,
                      new_no := pos_integer() | undefined}.

%%%===================================================================
%%% Builders
%%%===================================================================

%% @doc A tree. `Items' are nodes (see the type `tree_item()'); `Value' is
%% the selected node's value (its ancestors are rendered expanded). Css:
%% `disabled'. Options: `toggle_mode' (click (default): a click on a row
%% expands it; dblclick: only a double click or the arrow does),
%% `animation' (slide (default) | none), `load' (an action ref that
%% supplies the children of lazy nodes, see the module doc).
-spec tree([tree_item()], term(), css(), attrs()) -> #ah_tree{}.
tree(Items, Value, Css, Attrs) ->
    build(#ah_tree{items = Items, value = Value}, Css, Attrs).

%% @doc A grouped side navigation. `Items' are links (`#{label, route}' or
%% `#{label, href}', `{Label, Route}' for short), collapsible nodes
%% (`#{label, items}') and groups (`#{group => Heading, items => [...]}').
%% `Value' is the active route: that link is highlighted and the nodes
%% around it are open. Options: `route_prefix' (prepended to a route to
%% make the href, default "#/").
-spec nav_tree([nav_item()], iodata() | atom() | undefined, css(), attrs()) -> #ah_nav_tree{}.
nav_tree(Items, Value, Css, Attrs) ->
    build(#ah_nav_tree{items = Items, value = Value}, Css, Attrs).

%% @doc The differences between two texts. Css: `line' (default) or `word'
%% (inline word diff, always one column), `unified' (default) or `split'
%% (old and new side by side), `line_numbers' (in unified view; split
%% always shows them), `stats' (a +n / -n bar on top).
-spec diff(unicode:chardata(), unicode:chardata(), css(), attrs()) -> #ah_diff{}.
diff(Old, New, Css, Attrs) ->
    build(#ah_diff{old = Old, new = New}, Css, Attrs).

%% @doc A contribution heatmap. `Data' maps days (ISO dates or
%% `{Y, M, D}') to numbers. Options: `months' (how far back, default 12),
%% `end_date' (the last day, default today), `thresholds' (ascending,
%% default [0, 1, 3, 6]: N thresholds give N + 1 colour levels),
%% `weekday_labels' (7, from Sunday), `month_labels' (12), `legend'
%% (`{Less, More}' texts, or false), `tooltip' (text with {date} and
%% {value}).
-spec heatmap_calendar(ah_dt_heatmap_data(), css(), attrs()) -> #ah_heatmap_calendar{}.
heatmap_calendar(Data, Css, Attrs) ->
    build(#ah_heatmap_calendar{data = Data}, Css, Attrs).

build(R, Css, Attrs) ->
    Tag = element(1, R),
    ?E:build(R, fields(Tag), entry(?E:component_name(Tag)), Css, Attrs).

%% @doc The field names of one of this group's records.
-spec fields(atom()) -> [atom()].
fields(ah_tree) -> record_info(fields, ah_tree);
fields(ah_nav_tree) -> record_info(fields, ah_nav_tree);
fields(ah_diff) -> record_info(fields, ah_diff);
fields(ah_heatmap_calendar) -> record_info(fields, ah_heatmap_calendar).

%% @doc Functions besides the components that the aihtml facade re-exports.
-spec facade_extras() -> [{atom(), arity()}].
facade_extras() -> [{set_children, 3}].

%%%===================================================================
%%% Rendering
%%%===================================================================

-spec render(element()) -> html().
render(#ah_tree{} = R) -> render_tree(R);
render(#ah_nav_tree{} = R) -> render_nav_tree(R);
render(#ah_diff{} = R) -> render_diff(R);
render(#ah_heatmap_calendar{} = R) -> render_heatmap(R).

classes(R) ->
    Tag = element(1, R),
    ?E:classes(R, fields(Tag), entry(?E:component_name(Tag))).

%%%===================================================================
%%% tree
%%%===================================================================

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
    Classes = classes(R),
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
-spec set_children(aihtml_action:ctx(), aihtml_action:event(), [tree_item()]) -> ok.
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
%%% nav_tree
%%%===================================================================

-define(CARET, {safe, <<"<svg class=\"ah-nav-tree__caret-svg\" width=\"16\" height=\"16\" "
                        "viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" "
                        "stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\" "
                        "aria-hidden=\"true\"><path d=\"m6 9 6 6 6-6\"/></svg>">>}).

render_nav_tree(#ah_nav_tree{items = Items, value = Value, route_prefix = Prefix} = R) ->
    Active = case Value of undefined -> undefined; _ -> text(Value) end,
    Groups = nav_groups(Items),
    Cfg = #{active => Active, prefix => text(Prefix)},
    ?H:el(nav,
          [?H:el('div',
                 [case Label of
                      undefined -> [];
                      _ -> ?H:el('div', Label, [<<"ah-nav-tree__group-label">>], [])
                  end,
                  [nav_item(I, Cfg) || I <- GItems]],
                 [<<"ah-nav-tree__group">>], [])
           || {Label, GItems} <- Groups],
          classes(R),
          [[{data_ah, <<"nav-tree">>}, {data_ah_value, case Active of
                                                        undefined -> <<>>;
                                                        _ -> Active
                                                    end}],
           ?E:root_attrs(R, change)]).

%% Groups as given; loose items between them form groups without a heading.
nav_groups(Items) ->
    Rev = lists:foldl(fun(#{group := L, items := Is}, Acc) -> [{{group, L}, Is} | Acc];
                         (I, [{loose, Is} | Acc]) -> [{loose, Is ++ [I]} | Acc];
                         (I, Acc) -> [{loose, [I]} | Acc]
                      end, [], Items),
    [{case K of loose -> undefined; {group, L} -> L end, Is} || {K, Is} <- lists:reverse(Rev)].

nav_item({Label, Route}, Cfg) -> nav_item(#{label => Label, route => Route}, Cfg);
nav_item(#{label := Label, items := [_ | _] = Kids} = I, Cfg) ->
    Open = nav_contains(Kids, maps:get(active, Cfg)),
    ?H:el(details,
          [?H:el(summary,
                 [nav_icon(I), ?H:el(span, Label, [<<"ah-nav-tree__label">>], []),
                  ?H:el(span, ?CARET, [<<"ah-nav-tree__caret">>], [])],
                 [<<"ah-nav-tree__item">>, <<"ah-nav-tree__item--parent">>,
                  [<<"ah-is-open">> || Open]], []),
           ?H:el('div',
                 ?H:el('div', [nav_item(K, Cfg) || K <- Kids],
                       [<<"ah-nav-tree__children-inner">>], []),
                 [<<"ah-nav-tree__children">>], [])],
          [<<"ah-nav-tree__node">>], [{open, Open}]);
nav_item(#{label := Label} = I, #{active := Active, prefix := Prefix}) ->
    Route = case I of #{route := R0} -> text(R0); _ -> undefined end,
    Href = case I of
               #{href := H} -> H;
               _ when Route =/= undefined -> <<Prefix/binary, Route/binary>>;
               _ -> <<"#">>
           end,
    IsActive = Route =/= undefined andalso Route =:= Active,
    ?H:el(a, [nav_icon(I), ?H:el(span, Label, [<<"ah-nav-tree__label">>], [])],
          [<<"ah-nav-tree__item">>, [<<"ah-is-active">> || IsActive]],
          [{href, Href}, {data_route, Route},
           {aria_current, IsActive andalso <<"page">>}]);
nav_item(Other, _) -> error({aihtml, {bad_nav_tree_item, Other}}).

nav_icon(#{icon := Icon}) when Icon =/= undefined ->
    ?H:el(span, Icon, [<<"ah-nav-tree__icon">>], [{aria_hidden, <<"true">>}]);
nav_icon(_) -> [].

nav_contains(_, undefined) -> false;
nav_contains(Items, Active) ->
    lists:any(fun({_, R}) -> text(R) =:= Active;
                 (#{items := [_ | _] = Kids}) -> nav_contains(Kids, Active);
                 (#{route := R}) -> text(R) =:= Active;
                 (_) -> false
              end, Items).

%%%===================================================================
%%% diff
%%%===================================================================

render_diff(#ah_diff{old = Old, new = New, mode = Mode, view = View0,
                     line_numbers = Numbers, stats = Stats} = R) ->
    Classes = classes(R),                       % checks mode, view and the flags
    View = case Mode of word -> unified; line -> View0 end,
    Rows = case Mode of line -> line_rows(Old, New); word -> [] end,
    StatsBar = case Stats andalso Mode =:= line of
                   false -> [];
                   true ->
                       Add = length([x || #{type := add} <- Rows]),
                       Del = length([x || #{type := del} <- Rows]),
                       ?H:el('div',
                             [?H:el(span, [<<"+">>, integer_to_binary(Add)],
                                    [<<"ah-diff__stat">>], [{data_type, add}]),
                              ?H:el(span, [<<"-">>, integer_to_binary(Del)],
                                    [<<"ah-diff__stat">>], [{data_type, del}])],
                             [<<"ah-diff__stats">>], [])
               end,
    Body = case {Mode, View} of
               {word, _} ->
                   ?H:el('div',
                         [?H:el(span, V, [<<"ah-diff__word">>], [{data_type, T}])
                          || #{type := T, value := V} <- word_parts(Old, New)],
                         [<<"ah-diff__words">>], []);
               {line, split} ->
                   ?H:el('div', [diff_pair(P) || P <- split_rows(Rows)],
                         [<<"ah-diff__split">>], []);
               {line, unified} ->
                   ?H:el('div', [diff_row(Row, Numbers) || Row <- Rows],
                         [<<"ah-diff__body">>], [])
           end,
    ?H:el('div', [StatsBar, Body], Classes,
          [[{data_mode, Mode}, {data_view, View}], ?E:root_attrs(R, none)]).

diff_row(#{type := T, text := Text, old_no := O, new_no := N}, Numbers) ->
    ?H:el('div',
          [case Numbers of
               true -> [lineno(O), lineno(N)];
               false -> []
           end,
           ?H:el(span, marker(T), [<<"ah-diff__marker">>], [{aria_hidden, <<"true">>}]),
           ?H:el(span, row_text(Text), [<<"ah-diff__text">>], [])],
          [<<"ah-diff__row">>], [{data_type, T}]).

diff_pair({Left, Right}) ->
    ?H:el('div', [diff_side(old, Left, old_no), diff_side(new, Right, new_no)],
          [<<"ah-diff__pair">>], []).

diff_side(Side, undefined, _) ->
    ?H:el('div', [lineno(undefined), ?H:el(span, row_text(<<>>), [<<"ah-diff__text">>], [])],
          [<<"ah-diff__side">>], [{data_side, Side}, {data_type, empty}]);
diff_side(Side, #{type := T, text := Text} = Row, NoKey) ->
    ?H:el('div', [lineno(maps:get(NoKey, Row)),
                  ?H:el(span, row_text(Text), [<<"ah-diff__text">>], [])],
          [<<"ah-diff__side">>], [{data_side, Side}, {data_type, T}]).

lineno(N) ->
    ?H:el(span, case N of undefined -> <<>>; _ -> integer_to_binary(N) end,
          [<<"ah-diff__lineno">>], [{aria_hidden, <<"true">>}]).

marker(add) -> <<"+">>;
marker(del) -> <<"-">>;
marker(ctx) -> <<" ">>.

%% An empty line keeps its height with a no-break space.
row_text(<<>>) -> <<" "/utf8>>;
row_text(T) -> T.

%% @doc The line diff of two texts: one row per line, removed lines
%% before the added lines that replace them. A final newline does not
%% make an extra empty line.
-spec line_rows(unicode:chardata(), unicode:chardata()) -> [diff_row()].
line_rows(Old, New) ->
    number(group_changes(myers(lines(Old), lines(New))), 1, 1).

number([], _, _) -> [];
number([{eq, L} | Rest], O, N) ->
    [#{type => ctx, text => L, old_no => O, new_no => N} | number(Rest, O + 1, N + 1)];
number([{del, L} | Rest], O, N) ->
    [#{type => del, text => L, old_no => O, new_no => undefined} | number(Rest, O + 1, N)];
number([{ins, L} | Rest], O, N) ->
    [#{type => add, text => L, old_no => undefined, new_no => N} | number(Rest, O, N + 1)].

lines(T) ->
    case text(T) of
        <<>> -> [];
        B ->
            Ls = binary:split(B, <<"\n">>, [global]),
            Ls1 = case lists:last(Ls) of
                      <<>> when length(Ls) > 1 -> lists:droplast(Ls);
                      _ -> Ls
                  end,
            [strip_cr(L) || L <- Ls1]
    end.

strip_cr(L) ->
    case byte_size(L) of
        0 -> L;
        S -> case binary:last(L) of
                 $\r -> binary:part(L, 0, S - 1);
                 _ -> L
             end
    end.

%% @doc Pair the rows of a line diff for the side by side view: a run of
%% removed lines and the run of added lines after it share rows, the
%% shorter side padded with `undefined'; context lines are on both sides.
-spec split_rows([diff_row()]) -> [{diff_row() | undefined, diff_row() | undefined}].
split_rows([]) -> [];
split_rows([#{type := ctx} = R | Rest]) -> [{R, R} | split_rows(Rest)];
split_rows(Rows) ->
    {Dels, Rest1} = lists:splitwith(fun(#{type := T}) -> T =:= del end, Rows),
    {Adds, Rest2} = lists:splitwith(fun(#{type := T}) -> T =:= add end, Rest1),
    pad(Dels, Adds) ++ split_rows(Rest2).

pad([], []) -> [];
pad([D | Ds], [A | As]) -> [{D, A} | pad(Ds, As)];
pad([D | Ds], []) -> [{D, undefined} | pad(Ds, [])];
pad([], [A | As]) -> [{undefined, A} | pad([], As)].

%% @doc The word diff of two texts: words, runs of white space and single
%% other characters (so CJK text compares character by character), merged
%% into runs of the same type.
-spec word_parts(unicode:chardata(), unicode:chardata()) ->
          [#{type := ctx | add | del, value := binary()}].
word_parts(Old, New) ->
    Ops = group_changes(myers(tokens(text(Old)), tokens(text(New)))),
    merge_parts([{case Op of eq -> ctx; del -> del; ins -> add end, V} || {Op, V} <- Ops]).

merge_parts([]) -> [];
merge_parts([{T, A}, {T, B} | Rest]) -> merge_parts([{T, <<A/binary, B/binary>>} | Rest]);
merge_parts([{T, V} | Rest]) -> [#{type => T, value => V} | merge_parts(Rest)].

tokens(B) ->
    [unicode:characters_to_binary(Cs) || Cs <- chunk(unicode:characters_to_list(B))].

chunk([]) -> [];
chunk([C | _] = Cs) ->
    case char_class(C) of
        other -> [[C] | chunk(tl(Cs))];
        Class ->
            {Run, Rest} = lists:splitwith(fun(X) -> char_class(X) =:= Class end, Cs),
            [Run | chunk(Rest)]
    end.

char_class(C) when C =:= $\s; C =:= $\t; C =:= $\n; C =:= $\r; C =:= 16#3000 -> space;
char_class(C) when C >= $a, C =< $z; C >= $A, C =< $Z; C >= $0, C =< $9; C =:= $_ -> word;
char_class(C) when C >= 16#C0, C < 16#2000, C =/= 16#D7, C =/= 16#F7 -> word;
char_class(_) -> other.

%% Within each run of changes, removals first, then insertions.
group_changes(Ops) -> group_changes(Ops, [], []).

group_changes([], Dels, Ins) -> lists:reverse(Dels) ++ lists:reverse(Ins);
group_changes([{eq, _} = E | Rest], Dels, Ins) ->
    lists:reverse(Dels) ++ lists:reverse(Ins) ++ [E | group_changes(Rest, [], [])];
group_changes([{del, _} = D | Rest], Dels, Ins) -> group_changes(Rest, [D | Dels], Ins);
group_changes([{ins, _} = I | Rest], Dels, Ins) -> group_changes(Rest, Dels, [I | Ins]).

%% Myers' O(ND) diff: the shortest edit script from A to B as
%% [{eq | del | ins, Element}]. The common prefix and suffix are cut
%% first, which keeps D small for typical edits.
myers(A, B) ->
    {Pre, A1, B1} = common_prefix(A, B, []),
    {Suf, A2, B2} = common_suffix(A1, B1),
    [{eq, X} || X <- Pre] ++ myers_core(A2, B2) ++ [{eq, X} || X <- Suf].

common_prefix([X | A], [X | B], Acc) -> common_prefix(A, B, [X | Acc]);
common_prefix(A, B, Acc) -> {lists:reverse(Acc), A, B}.

common_suffix(A, B) ->
    {Suf, RA, RB} = common_prefix(lists:reverse(A), lists:reverse(B), []),
    {lists:reverse(Suf), lists:reverse(RA), lists:reverse(RB)}.

myers_core([], B) -> [{ins, X} || X <- B];
myers_core(A, []) -> [{del, X} || X <- A];
myers_core(A, B) ->
    At = list_to_tuple(A), Bt = list_to_tuple(B),
    N = tuple_size(At), M = tuple_size(Bt),
    Trace = myers_forward(At, Bt, N, M, 0, #{1 => 0}, []),
    myers_back(Trace, At, Bt, N, M, []).

%% Returns the V maps saved at the start of each D, the last D first.
myers_forward(At, Bt, N, M, D, V, Trace) ->
    case myers_step(At, Bt, N, M, D, -D, V, V) of
        {done, _} -> [{D, V} | Trace];
        {next, V1} -> myers_forward(At, Bt, N, M, D + 1, V1, [{D, V} | Trace])
    end.

myers_step(_, _, _, _, D, K, _, V1) when K > D -> {next, V1};
myers_step(At, Bt, N, M, D, K, V0, V1) ->
    X0 = case K =:= -D orelse (K =/= D andalso
                                 maps:get(K - 1, V1) < maps:get(K + 1, V1)) of
             true -> maps:get(K + 1, V1);
             false -> maps:get(K - 1, V1) + 1
         end,
    X = snake(At, Bt, N, M, X0, X0 - K),
    V2 = V1#{K => X},
    case X >= N andalso X - K >= M of
        true -> {done, V2};
        false -> myers_step(At, Bt, N, M, D, K + 2, V0, V2)
    end.

snake(At, Bt, N, M, X, Y) when X < N, Y < M ->
    case element(X + 1, At) =:= element(Y + 1, Bt) of
        true -> snake(At, Bt, N, M, X + 1, Y + 1);
        false -> X
    end;
snake(_, _, _, _, X, _) -> X.

myers_back([], _, _, _, _, Acc) -> Acc;
myers_back([{D, V} | Trace], At, Bt, X, Y, Acc) ->
    K = X - Y,
    PrevK = case K =:= -D orelse (K =/= D andalso
                                    maps:get(K - 1, V) < maps:get(K + 1, V)) of
                true -> K + 1;
                false -> K - 1
            end,
    PrevX = maps:get(PrevK, V),
    PrevY = PrevX - PrevK,
    {X1, Y1, Acc1} = diagonal(At, X, Y, PrevX, PrevY, Acc),
    case D of
        0 -> Acc1;
        _ ->
            Op = case X1 =:= PrevX of
                     true -> {ins, element(Y1, Bt)};
                     false -> {del, element(X1, At)}
                 end,
            myers_back(Trace, At, Bt, PrevX, PrevY, [Op | Acc1])
    end.

diagonal(At, X, Y, PX, PY, Acc) when X > PX, Y > PY ->
    diagonal(At, X - 1, Y - 1, PX, PY, [{eq, element(X, At)} | Acc]);
diagonal(_, X, Y, _, _, Acc) -> {X, Y, Acc}.

%%%===================================================================
%%% heatmap_calendar
%%%===================================================================

-define(CELL_PX, 15).
-define(MONTHS, [<<"Jan">>, <<"Feb">>, <<"Mar">>, <<"Apr">>, <<"May">>, <<"Jun">>,
                 <<"Jul">>, <<"Aug">>, <<"Sep">>, <<"Oct">>, <<"Nov">>, <<"Dec">>]).
-define(WEEKDAYS, [<<>>, <<"Mon">>, <<>>, <<"Wed">>, <<>>, <<"Fri">>, <<>>]).

render_heatmap(#ah_heatmap_calendar{data = Data0, months = Months, end_date = End0,
                                    thresholds = Thr, weekday_labels = WL0,
                                    month_labels = ML0, legend = Legend,
                                    tooltip = Tip} = R) ->
    is_integer(Months) andalso Months > 0
        orelse error({aihtml, {bad_option, months, Months}}),
    is_list(Thr) andalso lists:all(fun is_number/1, Thr) andalso lists:sort(Thr) =:= Thr
        orelse error({aihtml, {bad_option, thresholds, Thr}}),
    WL = labels(weekday_labels, WL0, ?WEEKDAYS, 7),
    ML = labels(month_labels, ML0, ?MONTHS, 12),
    Legend =:= false orelse (is_tuple(Legend) andalso tuple_size(Legend) =:= 2)
        orelse error({aihtml, {bad_option, legend, Legend}}),
    Data = heat_data(Data0),
    End = case End0 of
              undefined -> date();
              _ -> to_date(End0)
          end,
    Weeks = build_weeks(Data, End, Months),
    Spans = month_spans(Weeks),
    ?H:el('div',
          [?H:el('div',
                 [?H:el(span, lists:nth(Mo, ML), [<<"ah-heatmap-calendar__month">>],
                        [{style, [<<"width:">>, integer_to_binary(Span * ?CELL_PX), <<"px;">>]}])
                  || {{_, Mo}, Span} <- Spans],
                 [<<"ah-heatmap-calendar__months">>], [{aria_hidden, <<"true">>}]),
           ?H:el('div',
                 [?H:el('div', [?H:el(span, L, [<<"ah-heatmap-calendar__weekday">>], [])
                                || L <- WL],
                        [<<"ah-heatmap-calendar__weekdays">>], [{aria_hidden, <<"true">>}]),
                  ?H:el('div',
                        [?H:el('div',
                               [?H:el('div', [], [<<"ah-heatmap-calendar__cell">>],
                                      [{data_level, level(V, Thr)}, {data_date, iso(D)},
                                       {data_value, num(V)}])
                                || {D, V} <- Week],
                               [<<"ah-heatmap-calendar__week">>], [])
                         || Week <- Weeks],
                        [<<"ah-heatmap-calendar__grid">>], [])],
                 [<<"ah-heatmap-calendar__body">>], []),
           case Legend of
               false -> [];
               {Less, More} ->
                   ?H:el('div',
                         [?H:el(span, Less, [], []),
                          [?H:el('div', [], [<<"ah-heatmap-calendar__legend-cell">>],
                                 [{data_level, L}])
                           || L <- lists:seq(0, length(Thr))],
                          ?H:el(span, More, [], [])],
                         [<<"ah-heatmap-calendar__legend">>], [{aria_hidden, <<"true">>}])
           end,
           ?H:el('div', [], [<<"ah-heatmap-calendar__tooltip">>],
                 [{data_visible, <<"false">>}, {role, tooltip}])],
          classes(R),
          [[{data_ah, <<"heatmap-calendar">>}, {data_tip, text(Tip)}],
           ?E:root_attrs(R, 'ah:select')]).

labels(_, undefined, Default, _) -> Default;
labels(Key, L, _, N) ->
    is_list(L) andalso length(L) =:= N orelse error({aihtml, {bad_option, Key, L}}),
    L.

heat_data(M) when is_map(M) -> heat_data(maps:to_list(M));
heat_data(L) ->
    is_list(L) orelse error({aihtml, {bad_heatmap_data, L}}),
    maps:from_list([{to_date(D), case is_number(V) of
                                     true -> V;
                                     false -> error({aihtml, {bad_heatmap_value, D, V}})
                                 end} || {D, V} <- L]).

to_date({Y, M, D} = Date) when is_integer(Y), is_integer(M), is_integer(D) ->
    calendar:valid_date(Date) orelse error({aihtml, {bad_date, Date}}),
    Date;
to_date(S) when is_list(S) -> to_date(text(S));
to_date(<<Y:4/binary, "-", M:2/binary, "-", D:2/binary>> = B) ->
    Date = try {binary_to_integer(Y), binary_to_integer(M), binary_to_integer(D)}
           catch error:badarg -> error({aihtml, {bad_date, B}})
           end,
    calendar:valid_date(Date) orelse error({aihtml, {bad_date, B}}),
    Date;
to_date(Other) -> error({aihtml, {bad_date, Other}}).

%% Weeks from Sunday, starting in the week of the day `Months' months
%% before `End' and ending in the week of `End'; each week has 7 days.
build_weeks(Data, End, Months) ->
    {Y, M, D} = End,
    Mi = Y * 12 + (M - 1) - Months,
    {Y0, M0} = {Mi div 12, Mi rem 12 + 1},
    Start0 = {Y0, M0, min(D, calendar:last_day_of_the_month(Y0, M0))},
    S0 = calendar:date_to_gregorian_days(Start0),
    S = S0 - calendar:day_of_the_week(Start0) rem 7,       % back to Sunday
    E = calendar:date_to_gregorian_days(End),
    NWeeks = (E - S + 1 + 6) div 7,
    [[begin
          Day = calendar:gregorian_days_to_date(S + W * 7 + I),
          {Day, maps:get(Day, Data, 0)}
      end || I <- lists:seq(0, 6)]
     || W <- lists:seq(0, NWeeks - 1)].

%% Month label spans: weeks grouped by the month of their Wednesday.
month_spans(Weeks) ->
    lists:reverse(
      lists:foldl(fun(Week, Acc) ->
                          {{Y, M, _}, _} = lists:nth(4, Week),
                          case Acc of
                              [{{Y, M}, N} | Rest] -> [{{Y, M}, N + 1} | Rest];
                              _ -> [{{Y, M}, 1} | Acc]
                          end
                  end, [], Weeks)).

level(V, Thr) -> level(V, Thr, 0).

level(_, [], I) -> I;
level(V, [T | _], I) when V =< T -> I;
level(V, [_ | Ts], I) -> level(V, Ts, I + 1).

iso({Y, M, D}) ->
    iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0B", [Y, M, D])).

num(V) when is_integer(V) -> integer_to_binary(V);
num(V) when is_float(V) ->
    case V == trunc(V) of
        true -> integer_to_binary(trunc(V));
        false -> float_to_binary(V, [short])
    end.

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
              doc => <<"Called by set_children/3: end the loading state and expand the node.">>}]},
     #{name => nav_tree, category => layout,
       signature => <<"nav_tree(Items, Value, Css, Attrs)">>,
       root => <<"ah-nav-tree">>, options => [route_prefix],
       behavior => <<"nav-tree">>, events => [<<"change">>],
       doc => <<"A grouped side navigation: links, collapsible nodes with connector lines, "
                "the active route highlighted and its nodes open.">>,
       option_docs => #{route_prefix => <<"Prepended to a route to make its href (default \"#/\").">>},
       methods =>
           [#{name => setValue, args => <<"(Route)">>,
              doc => <<"Mark the link of this route active and open its nodes, without firing change.">>},
            #{name => getValue, args => <<"()">>, doc => <<"Return the active route.">>}]},
     #{name => diff, category => data,
       signature => <<"diff(Old, New, Css, Attrs)">>,
       root => <<"ah-diff">>,
       groups => #{mode => {[line, word], line}, view => {[unified, split], unified}},
       flags => [line_numbers, stats],
       classes => #{line => [], word => [], unified => [], split => [],
                    line_numbers => [], stats => []},
       doc => <<"The differences between two texts, line by line (unified or side by side) "
                "or word by word, computed on the server.">>,
       option_docs => #{line_numbers => <<"Show line numbers in the unified view (split always does).">>,
                        stats => <<"A bar with the number of added and removed lines.">>},
       methods => []},
     #{name => heatmap_calendar, category => data,
       signature => <<"heatmap_calendar(Data, Css, Attrs)">>,
       root => <<"ah-heatmap-calendar">>,
       options => [months, end_date, thresholds, weekday_labels, month_labels, legend, tooltip],
       behavior => <<"heatmap-calendar">>, events => [<<"ah:select">>],
       doc => <<"A contribution heatmap: one column per week, days coloured by value, "
                "a tooltip on hover and a select event on click.">>,
       option_docs =>
           #{months => <<"How many months back from end_date (default 12).">>,
             end_date => <<"The last day shown (default today).">>,
             thresholds => <<"Ascending limits of the colour levels (default [0, 1, 3, 6]); "
                             "a value =< the i-th limit gets level i.">>,
             weekday_labels => <<"7 labels from Sunday (default only Mon, Wed, Fri).">>,
             month_labels => <<"12 month names (default Jan ... Dec).">>,
             legend => <<"{Less, More} texts of the legend, or false to hide it.">>,
             tooltip => <<"Tooltip text; {date} and {value} are replaced "
                          "(default \"{value} · {date}\").">>},
       methods => []}].

%%%===================================================================
%%% Internal
%%%===================================================================

entry(Name) -> aihtml_catalog:entry(?MODULE, Name).

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
