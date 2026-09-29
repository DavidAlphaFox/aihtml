%%%-------------------------------------------------------------------
%%% @doc Docking layouts, ported from sigil (layout/docking and
%%% layout/dock_layout). DOM and class names are sigil's, so the styles in
%%% priv/css/sigil/components/{docking,dock_layout}.css apply unchanged.
%%%
%%%   docking(Panels, Css, Attrs)        panels of windows, dragged between them
%%%   dock_layout(Layout, Css, Attrs)    an IDE layout: splits, tab groups,
%%%                                      documents, float and auto hide
%%%   docking_add_window(Ctx, Target, PanelId, Window)
%%%                                      (in an action) add a window
%%%   dock_layout_open(Ctx, Target, Panel[, Opts])
%%%                                      (in an action) open a panel
%%%
%%% == The layout is view state ==
%%%
%%% Both components keep the arrangement the user made in the browser and
%%% expose it as JSON in `data-ah-value' (and a hidden input with `name').
%%% After every rearrangement they fire `change' on the root, so an action
%%% bound with `on(change, Ref)' (or a record's `postback') receives the
%%% layout in `Event.value' and can store it per user. The page renders
%%% that JSON again: `docking(Panels, Css, [{layout, Json}])' and
%%% `dock_layout(Json, Css, [{panels, Panels}])'. Panel and window contents
%%% are ordinary server-rendered HTML; the JSON holds only ids, order,
%%% sizes and states.
%%%
%%% docking's JSON:
%%%
%%%   {"panels": [{"id": "a", "windows": ["w1", "w2"]}, ...],
%%%    "floating": [{"id": "w3", "x": 40, "y": 30, "width": 280}],
%%%    "collapsed": ["w2"], "closed": ["w4"]}
%%%
%%% Windows the JSON does not mention (added to the page later) stay in
%%% their own panel.
%%%
%%% dock_layout's JSON is the layout tree in its map form (see the
%%% ah_dl_node() type in include/aihtml_layout_dock.hrl):
%%%
%%%   [{"type": "split", "orientation": "horizontal", "size": 100, "items": [
%%%      {"type": "tabs", "id": "left", "size": 22, "active": "explorer",
%%%       "items": ["explorer", "search"], "pin": true, "close": true}, ...]},
%%%    {"type": "float", "id": "f1", "x": 120, "y": 80, "width": 260,
%%%     "height": 180, "items": ["inspector"], "active": "inspector"},
%%%    {"type": "autohide", "id": "out", "edge": "bottom", "size": 200,
%%%     "items": ["output"], "active": "output", "pin": true, "close": true}]
%%%
%%% `size' is the share of the parent in percent (the siblings add up to
%%% 100), px for autohide groups. Panel ids are looked up in the `panels'
%%% option; ids it does not know are skipped, panels the layout does not
%%% name are not shown (closed) until `dock_layout_open/3,4' opens them.
%%%
%%% The browser builds HTML only from the shared templates
%%% templates/dock_layout_{group,float,menu}.mustache, which render the
%%% tab groups and float windows on the server too. Behaviours:
%%% assets/js/components/layout_dock.js. Each component function builds an
%%% element record (#ah_docking{}, #ah_dock_layout{},
%%% include/aihtml_layout_dock.hrl) and render/1 turns it into HTML.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_layout_dock).
-behaviour(aihtml_element).

-include("aihtml_layout_dock.hrl").

-export([docking/3, dock_layout/3, docking_add_window/4, dock_layout_open/3,
         dock_layout_open/4, render/1, fields/1, catalog/0, facade_extras/0]).

-export_type([element/0, window/0, panel/0, layout/0, dl_node/0, dl_panel/0]).

%% Shared templates (see aihtml_tpl): also compiled to AH.tpl.* for the browser.
-compile({parse_transform, beamai_mustache_transform}).
-mustache_template({tpl_dock_layout_group, "../templates/dock_layout_group.mustache"}).
-mustache_template({tpl_dock_layout_float, "../templates/dock_layout_float.mustache"}).
-mustache_template({tpl_dock_layout_menu, "../templates/dock_layout_menu.mustache"}).

%% Record fields are checked at render time for values their types rule
%% out (pages may build records from untyped data).
-dialyzer({no_match, [dk_panel/1, dk_labels/1, registry/1]}).

-define(H, aihtml_html).
-define(E, aihtml_element).

-type html() :: aihtml_html:html().
-type css() :: aihtml_html:css().
-type attrs() :: aihtml_html:attrs().
-type element() :: #ah_docking{} | #ah_dock_layout{}.
-type window() :: ah_dock_window().
-type panel() :: ah_dock_panel().
-type layout() :: ah_dl_layout().
-type dl_node() :: ah_dl_node().
-type dl_panel() :: ah_dl_panel().

-define(DL_LABELS, #{auto_hide => <<"Auto Hide">>, float => <<"Float">>,
                     dock => <<"Dock">>, close => <<"Close">>}).
-define(DK_LABELS, #{collapse => <<"Collapse">>, close => <<"Close">>}).
-define(FLOAT_DEFAULTS, #{x => 100, y => 80, width => 260, height => 180}).
-define(AUTOHIDE_SIZE, 250).

%%%===================================================================
%%% Builders
%%%===================================================================

%% @doc Panels of windows (sigil's docking). `Panels' are `{Id, Windows}'
%% (or maps with `id' and `windows'); a window is `{Id, Title, Body}' or
%% `{Id, Title, Body, Opts}' with Opts `collapsed', `pinned' (cannot be
%% dragged) and `floating' ({X, Y} or {X, Y, Width}). The header of a
%% window drags it to another panel or position; dropped outside every
%% panel it floats over the container (or goes back, without
%% `allow_float').
%%
%% Css: orientation `horizontal' (panels side by side, default) |
%% `vertical', flag `disabled'. Options: `layout' (a saved value, the JSON
%% or its json:decode/1 map, applied to the panels), `allow_float'
%% (default true), `offset' (px between windows, default 5),
%% `drag_opacity' (of the dragged window, default 0.3), `close_buttons',
%% `collapse_buttons' (default true), `labels' (#{collapse, close}).
%% `name' goes to a hidden input.
-spec docking([panel()], css(), attrs()) -> #ah_docking{}.
docking(Panels, Css, Attrs) ->
    build(#ah_docking{items = Panels}, Css, Attrs).

%% @doc An IDE-style dock layout (sigil's dock_layout). `Layout' is a node
%% or a list of top-level nodes of the tree (see ah_dl_node() in
%% include/aihtml_layout_dock.hrl), or the saved JSON value; it names
%% panels by id, found in the `panels' option (a list of `{Id, Title,
%% Body}'), or holds panels inline.
%%
%% Tabs are dragged to another group, to a side of a group (splitting
%% it), to an edge of the layout, or out of it to float; float windows are
%% dragged back the same way. The pin button auto hides a group at the
%% nearest edge; a right click (or the context menu key) on a tab offers
%% Auto Hide, Float, Dock and Close. Splitbars resize.
%%
%% Css: flag `disabled'. Options: `panels', `resizable' (default true),
%% `resize_mode' (`live' (default) | `feedback': a line follows the
%% pointer, sizes change on release), `allow_float', `allow_dock'
%% (default true), `min_size' (px, default 100), `labels' (#{auto_hide,
%% float, dock, close}). `name' goes to a hidden input. The root fills its
%% parent (width and height 100%).
-spec dock_layout(layout(), css(), attrs()) -> #ah_dock_layout{}.
dock_layout(Layout, Css, Attrs) ->
    build(#ah_dock_layout{layout = Layout}, Css, Attrs).

%%%===================================================================
%%% Server-driven operations
%%%===================================================================

%% @doc In an action: render `Window' (as in docking/3) and append it to
%% the panel `PanelId' of the docking `{id, RootId}'. The browser fires
%% `change'.
-spec docking_add_window(aihtml_action:ctx(), {id, iodata() | atom()}, ah_dock_id(),
                         window()) -> ok.
docking_add_window(Ctx, {id, Root0}, PanelId, Window) ->
    Root = text(Root0),
    W = window(Window),
    Html = ?H:render_binary(render_window(Root, W#{floating := undefined}, true, true,
                                          dk_labels(#{}))),
    aihtml_action:call(Ctx, {id, Root}, addWindow, [id_text(PanelId), Html]).

%% @doc `dock_layout_open/4' with the default place.
-spec dock_layout_open(aihtml_action:ctx(), {id, iodata() | atom()}, dl_panel()) -> ok.
dock_layout_open(Ctx, Target, Panel) ->
    dock_layout_open(Ctx, Target, Panel, #{}).

%% @doc In an action: open `Panel' ({Id, Title, Body}) in the dock layout
%% `{id, RootId}'. If a panel with that id is shown already, it is only
%% activated. Where: `#{in => GroupId}' adds a tab to that group,
%% `#{edge => left | right | top | bottom}' docks a new group at that edge
%% of the layout, `#{float => true | {X, Y}}' floats it; by default it
%% joins the document group, else the first tab group, else the right
%% edge. `labels' as in dock_layout/3, for the new group's buttons. The
%% browser fires `change'.
-spec dock_layout_open(aihtml_action:ctx(), {id, iodata() | atom()}, dl_panel(),
                       #{in => ah_dock_id(), edge => left | right | top | bottom,
                         float => boolean() | {number(), number()},
                         labels => ah_dl_labels()}) -> ok.
dock_layout_open(Ctx, {id, Root0}, Panel0, Opts) ->
    Root = text(Root0),
    #{id := Pid} = Panel = dl_panel(Panel0),
    Labels = dl_labels(maps:get(labels, Opts, #{})),
    Gid = <<Root/binary, "-o", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
    Group = #{kind => tabs, id => Gid, items => [Panel], active => Pid,
              pin => true, close => true},
    Html = tpl_bin(tpl_dock_layout_group(group_view(Root, Group, undefined, Labels))),
    Where = maps:from_list(
              [{in, id_text(G)} || G <- [maps:get(in, Opts, undefined)], G =/= undefined]
              ++ [{edge, atom_to_binary(one_of(edge, E, [left, right, top, bottom]))}
                  || E <- [maps:get(edge, Opts, undefined)], E =/= undefined]
              ++ case maps:get(float, Opts, false) of
                     false -> [];
                     true -> [{float, true}];
                     {X, Y} when is_number(X), is_number(Y) ->
                         [{float, true}, {x, round(X)}, {y, round(Y)}];
                     F -> error({aihtml, {bad_option, float, F}})
                 end),
    aihtml_action:call(Ctx, {id, Root}, openPanel, [Html, Where]).

%% @doc Functions besides the components that the aihtml facade re-exports.
-spec facade_extras() -> [{atom(), arity()}].
facade_extras() ->
    [{docking_add_window, 4}, {dock_layout_open, 3}, {dock_layout_open, 4}].

%%%===================================================================
%%% docking
%%%===================================================================

render_docking(#ah_docking{items = Panels0} = R0) ->
    {Id, R} = ensure_id(R0, <<"ah-dk">>),
    Classes = classes(R),
    bool(allow_float, R#ah_docking.allow_float),
    bool(close_buttons, R#ah_docking.close_buttons),
    bool(collapse_buttons, R#ah_docking.collapse_buttons),
    Offset = case R#ah_docking.offset of
                 undefined -> undefined;
                 N when is_integer(N), N >= 0 -> N;
                 N -> error({aihtml, {bad_option, offset, N}})
             end,
    Opacity = case R#ah_docking.drag_opacity of
                  O when is_number(O), O >= 0, O =< 1 -> O;
                  O -> error({aihtml, {bad_option, drag_opacity, O}})
              end,
    Labels = dk_labels(R#ah_docking.labels),
    Panels = [dk_panel(P) || P <- Panels0],
    Saved = saved_docking(R#ah_docking.layout),
    {Placed, Floating, Closed} = apply_docking(Panels, Saved),
    Close = R#ah_docking.close_buttons,
    Collapse = R#ah_docking.collapse_buttons,
    Value = iolist_to_binary(json:encode(docking_value(Placed, Floating, Closed))),
    ?H:el('div',
          [[?H:el('div', [render_window(Id, W, Close, Collapse, Labels) || W <- Ws],
                  [<<"ah-docking-panel">>], [{data_panel_id, P}])
            || {P, Ws} <- Placed],
           [render_window(Id, W, Close, Collapse, Labels) || W <- Floating],
           ?H:el('div', [], [<<"ah-docking-live">>], [{aria_live, polite}]),
           hidden(R#ah_docking.name, Value)],
          Classes,
          [[{id, Id}, {data_ah, <<"docking">>}, {data_ah_value, Value},
            {data_ah_allow_float, not R#ah_docking.allow_float andalso <<"false">>},
            {data_ah_drag_opacity, num(Opacity)},
            {style, case Offset of
                        undefined -> undefined;
                        _ -> <<"--ah-docking-offset:", (integer_to_binary(Offset))/binary, "px">>
                    end},
            {aria_disabled, R#ah_docking.disabled andalso <<"true">>}],
           ?E:root_attrs(R, change)]).

render_window(Root, #{id := W, title := Title, body := Body, collapsed := Collapsed,
                      pinned := Pinned, floating := Floating}, Close, Collapse, Labels) ->
    Base = <<Root/binary, "-w-", (safe_id(W))/binary>>,
    TitleId = <<Base/binary, "-title">>,
    ContentId = <<Base/binary, "-content">>,
    {State, Style} = case Floating of
                         undefined -> {<<"ah-docking-window-docked">>, undefined};
                         {X, Y, Wd} ->
                             {<<"ah-docking-window-floating">>,
                              iolist_to_binary([<<"left:">>, num(X), <<"px;top:">>, num(Y), <<"px;">>,
                                                [[<<"width:">>, num(Wd), <<"px;">>]
                                                 || Wd =/= undefined]])}
                     end,
    ?H:el('div',
          [?H:el('div',
                 [?H:el('div', text(Title), [<<"ah-docking-window-title">>], [{id, TitleId}]),
                  ?H:el('div',
                        [[?H:el(button, [], [<<"ah-docking-window-collapse-btn">>],
                                [{type, button}, {aria_label, maps:get(collapse, Labels)},
                                 {title, maps:get(collapse, Labels)},
                                 {aria_controls, ContentId},
                                 {aria_expanded, atom_to_binary(not Collapsed)}])
                          || Collapse],
                         [?H:el(button, [], [<<"ah-docking-window-close-btn">>],
                                [{type, button}, {aria_label, maps:get(close, Labels)},
                                 {title, maps:get(close, Labels)}])
                          || Close]],
                        [<<"ah-docking-window-buttons">>], [])],
                 [<<"ah-docking-window-header">>],
                 [{tabindex, 0}]),
           ?H:el('div', Body, [<<"ah-docking-window-content">>], [{id, ContentId}])],
          [<<"ah-docking-window">>, State,
           [<<"ah-docking-window-collapsed">> || Collapsed],
           [<<"ah-docking-window-pinned">> || Pinned]],
          [{data_window_id, W}, {role, region}, {aria_labelledby, TitleId}, {style, Style}]).

dk_panel({Id, Windows}) when is_list(Windows) ->
    #{id => id_text(Id), windows => [window(W) || W <- Windows]};
dk_panel(#{id := Id, windows := Windows}) when is_list(Windows) ->
    dk_panel({Id, Windows});
dk_panel(Other) ->
    error({aihtml, {bad_docking_panel, Other}}).

window({Id, Title, Body}) ->
    window({Id, Title, Body, #{}});
window({Id, Title, Body, Opts}) when is_map(Opts) ->
    window(Opts#{id => Id, title => Title, body => Body});
window(#{id := Id} = M) ->
    maps:foreach(fun(K, _) ->
                         lists:member(K, [id, title, body, collapsed, pinned, floating])
                             orelse error({aihtml, {bad_docking_window_option, K}})
                 end, M),
    #{id => id_text(Id),
      title => maps:get(title, M, <<>>),
      body => maps:get(body, M, []),
      collapsed => bool(collapsed, maps:get(collapsed, M, false)),
      pinned => bool(pinned, maps:get(pinned, M, false)),
      floating => case maps:get(floating, M, undefined) of
                      undefined -> undefined;
                      {X, Y} when is_number(X), is_number(Y) -> {X, Y, undefined};
                      {X, Y, W} = F when is_number(X), is_number(Y), is_number(W) -> F;
                      F -> error({aihtml, {bad_docking_window_option, {floating, F}}})
                  end};
window(Other) ->
    error({aihtml, {bad_docking_window, Other}}).

%% The saved value, normalised: undefined or #{panels => [{P, [W]}],
%% floating => [{W, {X, Y, Width}}], collapsed => [W], closed => [W]}.
saved_docking(undefined) -> undefined;
saved_docking(<<>>) -> undefined;
saved_docking(Json) when is_binary(Json) ->
    saved_docking(decode(Json));
saved_docking(M) when is_map(M) ->
    Get = fun(K) -> get_key(K, M, []) end,
    L = fun(_, V) when is_list(V) -> V;
           (K, V) -> error({aihtml, {bad_docking_layout, {K, V}}})
        end,
    #{panels => [{id_text(get_key(id, P, undefined)),
                  [id_text(W) || W <- L(windows, get_key(windows, P, []))]}
                 || P <- L(panels, Get(panels)), is_map(P)],
      floating => [{id_text(get_key(id, F, undefined)),
                    {number_or(get_key(x, F, 0), 0), number_or(get_key(y, F, 0), 0),
                     case get_key(width, F, undefined) of
                         Wd when is_number(Wd) -> Wd;
                         _ -> undefined
                     end}}
                   || F <- L(floating, Get(floating)), is_map(F)],
      collapsed => [id_text(W) || W <- L(collapsed, Get(collapsed))],
      closed => [id_text(W) || W <- L(closed, Get(closed))]};
saved_docking(Other) ->
    error({aihtml, {bad_docking_layout, Other}}).

%% -> {[{PanelId, [Window]}], [FloatingWindow], [ClosedId]}
apply_docking(Panels, undefined) ->
    {[{P, [W || #{floating := undefined} = W <- Ws]} || #{id := P, windows := Ws} <- Panels],
     [W || #{windows := Ws} <- Panels, #{floating := F} = W <- Ws, F =/= undefined],
     []};
apply_docking(Panels, #{panels := SP, floating := SF, collapsed := SC, closed := SX}) ->
    All = [{Wid, P, W} || #{id := P, windows := Ws} <- Panels, #{id := Wid} = W <- Ws],
    Known = maps:from_list([{Wid, W} || {Wid, _, W} <- All]),
    Closed = [Wid || Wid <- SX, maps:is_key(Wid, Known)],
    Floats = [{Wid, Geo} || {Wid, Geo} <- SF, maps:is_key(Wid, Known),
                            not lists:member(Wid, Closed)],
    Listed = lists:append([Ws || {_, Ws} <- SP]),
    Mentioned = Listed ++ Closed ++ [Wid || {Wid, _} <- Floats],
    Mark = fun(Wid, W) ->
                   case lists:member(Wid, Mentioned) of
                       true -> W#{collapsed := lists:member(Wid, SC)};
                       false -> W
                   end
           end,
    Own = fun(P) -> [Wid || {Wid, P1, #{floating := undefined}} <- All, P1 =:= P,
                            not lists:member(Wid, Mentioned)]
          end,
    {Placed, _} =
        lists:mapfoldl(
          fun(#{id := P}, Used) ->
                  Saved = proplists:get_value(P, SP, []),
                  Ids = dedup([Wid || Wid <- Saved, maps:is_key(Wid, Known),
                                      not lists:member(Wid, Closed),
                                      not lists:keymember(Wid, 1, Floats)] ++ Own(P), Used),
                  {{P, [(Mark(Wid, maps:get(Wid, Known)))#{floating := undefined}
                        || Wid <- Ids]},
                   Used ++ Ids}
          end, [], Panels),
    Floating = [(Mark(Wid, maps:get(Wid, Known)))#{floating := Geo} || {Wid, Geo} <- Floats]
        ++ [W || {Wid, _, #{floating := F} = W} <- All, F =/= undefined,
                 not lists:member(Wid, Mentioned)],
    {Placed, Floating, Closed}.

dedup(Ids, Used) ->
    lists:reverse(lists:foldl(fun(I, Acc) ->
                                      case lists:member(I, Acc) orelse lists:member(I, Used) of
                                          true -> Acc;
                                          false -> [I | Acc]
                                      end
                              end, [], Ids)).

docking_value(Placed, Floating, Closed) ->
    #{panels => [#{id => P, windows => [W || #{id := W} <- Ws]} || {P, Ws} <- Placed],
      floating => [maps:from_list([{id, W}, {x, round(X)}, {y, round(Y)}]
                                  ++ [{width, round(Wd)} || Wd =/= undefined])
                   || #{id := W, floating := {X, Y, Wd}} <- Floating],
      collapsed => [W || {_, Ws} <- Placed, #{id := W, collapsed := true} <- Ws]
          ++ [W || #{id := W, collapsed := true} <- Floating],
      closed => Closed}.

dk_labels(Custom) when is_map(Custom) ->
    maps:foreach(fun(K, _) -> maps:is_key(K, ?DK_LABELS)
                                  orelse error({aihtml, {bad_docking_label, K}})
                 end, Custom),
    maps:map(fun(_, V) -> text(V) end, maps:merge(?DK_LABELS, Custom));
dk_labels(Other) ->
    error({aihtml, {bad_option, labels, Other}}).

%%%===================================================================
%%% dock_layout
%%%===================================================================

render_dock_layout(#ah_dock_layout{} = R0) ->
    {Id, R} = ensure_id(R0, <<"ah-dl">>),
    Classes = classes(R),
    [bool(F, V) || {F, V} <- [{resizable, R#ah_dock_layout.resizable},
                              {allow_float, R#ah_dock_layout.allow_float},
                              {allow_dock, R#ah_dock_layout.allow_dock}]],
    Mode = one_of(resize_mode, R#ah_dock_layout.resize_mode, [live, feedback]),
    MinSize = case R#ah_dock_layout.min_size of
                  M when is_integer(M), M >= 0 -> M;
                  M -> error({aihtml, {bad_option, min_size, M}})
              end,
    Labels = dl_labels(R#ah_dock_layout.labels),
    Registry = registry(R#ah_dock_layout.panels),
    Nodes = resolve_layout(Id, parse_layout(R#ah_dock_layout.layout), Registry),
    Main = [N || #{kind := K} = N <- Nodes, K =/= float, K =/= autohide],
    Floats = [N || #{kind := float} = N <- Nodes],
    Hidden = [N || #{kind := autohide} = N <- Nodes],
    Resizable = R#ah_dock_layout.resizable,
    Env = #{root => Id, labels => Labels, resizable => Resizable},
    Value = iolist_to_binary(json:encode(dl_value(Nodes))),
    Strip = fun(Edge) ->
                    ?H:el('div',
                          [?H:el('div', first_title(G), [<<"ah-dl-autohide-tab">>],
                                 [{data_group_id, maps:get(id, G)}, {role, button},
                                  {tabindex, 0}, {aria_expanded, <<"false">>}])
                           || #{edge := E} = G <- Hidden, E =:= Edge],
                          [<<"ah-dl-autohide-strip ah-dl-autohide-strip-", (atom_to_binary(Edge))/binary>>],
                          [])
            end,
    Slot = fun(Edge) ->
                   ?H:el('div',
                         [aihtml_tpl:safe(tpl_dock_layout_group(group_view(Id, G, undefined, Labels)))
                          || #{edge := E} = G <- Hidden, E =:= Edge],
                         [<<"ah-dl-autohide-preview-slot ah-dl-autohide-preview-slot-",
                            (atom_to_binary(Edge))/binary>>],
                         [])
           end,
    ?H:el('div',
          [Strip(top), Slot(top),
           ?H:el('div',
                 [Strip(left), Slot(left),
                  ?H:el('div', children(Main, false, Env), [<<"ah-dl-inner">>], []),
                  Slot(right), Strip(right)],
                 [<<"ah-dl-middle">>], []),
           Slot(bottom), Strip(bottom),
           ?H:el('div', [render_float(Id, F, Labels) || F <- Floats],
                 [<<"ah-dl-float-container">>], []),
           dock_overlay(),
           hidden(R#ah_dock_layout.name, Value)],
          Classes,
          [[{id, Id}, {data_ah, <<"dock_layout">>}, {data_ah_value, Value},
            {data_ah_resizable, not Resizable andalso <<"false">>},
            {data_ah_resize_mode, Mode =:= feedback andalso <<"feedback">>},
            {data_ah_allow_float, not R#ah_dock_layout.allow_float andalso <<"false">>},
            {data_ah_allow_dock, not R#ah_dock_layout.allow_dock andalso <<"false">>},
            {data_ah_min_size, MinSize},
            {data_ah_labels, iolist_to_binary(json:encode(Labels))},
            {aria_disabled, R#ah_dock_layout.disabled andalso <<"true">>}],
           ?E:root_attrs(R, change)]).

%% The children of a container with splitbars between them. Horiz: the
%% container lays them out side by side.
children(Nodes, Vertical, Env) ->
    Weights = weights([maps:get(size, N, undefined) || N <- Nodes]),
    Bar = splitbar(Vertical, Env),
    lists:join(Bar, [render_node(N, W, Env) || {N, W} <- lists:zip(Nodes, Weights)]).

splitbar(Vertical, #{resizable := Resizable}) ->
    ?H:el('div', [], [<<"ah-dl-splitbar">>],
          [{role, separator}, {tabindex, Resizable andalso 0},
           {aria_orientation, case Vertical of
                                  true -> <<"horizontal">>;
                                  false -> <<"vertical">>
                              end}]).

render_node(#{kind := split, orientation := O, items := Items}, W, Env) ->
    ?H:el('div', children(Items, O =:= vertical, Env),
          [<<"ah-dl-group ah-dl-", (atom_to_binary(O))/binary>>], [{style, flex(W)}]);
render_node(#{kind := panel, item := #{id := P, title := T, body := B}}, W, _Env) ->
    ?H:el('div',
          [?H:el('div', text(T), [<<"ah-dl-panel-header">>], []),
           ?H:el('div', B, [<<"ah-dl-panel-body">>], [])],
          [<<"ah-dl-panel">>], [{data_panel_id, P}, {style, flex(W)}]);
render_node(#{kind := K} = G, W, #{root := Root, labels := Labels})
  when K =:= tabs; K =:= documents ->
    aihtml_tpl:safe(tpl_dock_layout_group(group_view(Root, G, flex(W), Labels))).

render_float(Root, #{id := Fid, items := Items, active := Active} = F, Labels) ->
    Group = #{kind => tabs, id => <<Fid/binary, "-g">>, items => Items, active => Active,
              pin => maps:get(pin, F, true), close => maps:get(close, F, true)},
    #{title := Title} = hd([P || #{id := I} = P <- Items, I =:= Active]),
    aihtml_tpl:safe(tpl_dock_layout_float(
                      #{fid => Fid, title => text(Title),
                        x => maps:get(x, F), y => maps:get(y, F),
                        width => maps:get(width, F), height => maps:get(height, F),
                        close_label => maps:get(close, Labels),
                        body => tpl_bin(tpl_dock_layout_group(
                                          group_view(Root, Group, undefined, Labels)))})).

%% The view data of templates/dock_layout_group.mustache.
group_view(Root, #{kind := K, id := Gid, items := Items, active := Active} = G, Style,
           Labels) ->
    Autohide = K =:= autohide,
    #{gid => Gid,
      document => K =:= documents,
      pin => atom_to_binary(K =/= documents andalso maps:get(pin, G, true)),
      close => atom_to_binary(maps:get(close, G, K =/= documents)),
      pinned => atom_to_binary(not Autohide),
      unpinned => Autohide,
      edge => Autohide andalso atom_to_binary(maps:get(edge, G)),
      size => case Autohide of true -> round(maps:get(size, G)); false -> 0 end,
      style => case Style of
                   undefined -> false;
                   _ -> Style
               end,
      pin_label => maps:get(auto_hide, Labels),
      close_label => maps:get(close, Labels),
      tabs => [#{id => P,
                 tid => <<Root/binary, "-t-", (safe_id(P))/binary>>,
                 pid => <<Root/binary, "-p-", (safe_id(P))/binary>>,
                 title => text(T),
                 selected => P =:= Active,
                 body => ?H:render_binary(B)}
               || #{id := P, title := T, body := B} <- Items]}.

first_title(#{items := [#{title := T} | _]}) -> text(T).

dock_overlay() ->
    Zone = fun(Z) -> ?H:el('div', [], [<<"ah-dl-dock-zone">>], [{data_zone, Z}]) end,
    Edge = fun(Z) -> ?H:el('div', [], [<<"ah-dl-dock-edge">>], [{data_zone, Z}]) end,
    ?H:el('div',
          [?H:el('div', [Zone(Z) || Z <- [<<"top">>, <<"left">>, <<"center">>, <<"right">>,
                                          <<"bottom">>]],
                 [<<"ah-dl-dock-cross">>], []),
           [Edge(Z) || Z <- [<<"edge-top">>, <<"edge-left">>, <<"edge-right">>,
                             <<"edge-bottom">>]],
           ?H:el('div', [], [<<"ah-dl-dock-preview">>], [])],
          [<<"ah-dl-dock-overlay">>], [{aria_hidden, <<"true">>}]).

flex(W) -> <<"flex:", (num(W))/binary, " 1 0px">>.

%% Shares of the siblings in percent, adding up to 100: the given sizes,
%% the rest split evenly among the siblings without one.
weights(Sizes) ->
    Given = [S || S <- Sizes, S =/= undefined],
    Missing = length(Sizes) - length(Given),
    Sum = lists:sum(Given),
    Fill = if
               Missing =:= 0 -> 0;
               Given =:= [] -> 1;
               Sum < 100 -> (100 - Sum) / Missing;
               true -> Sum / length(Given)
           end,
    Raw = [case S of undefined -> Fill; _ -> S end || S <- Sizes],
    Total = lists:sum(Raw),
    [round2(X * 100 / Total) || X <- Raw].

round2(X) ->
    R = round(X * 100) / 100,
    case R == trunc(R) of
        true -> trunc(R);
        false -> R
    end.

%% --- the layout tree -------------------------------------------------

registry(Panels) when is_list(Panels) ->
    maps:from_list([begin #{id := I} = P = dl_panel(P0), {I, P} end || P0 <- Panels]);
registry(Other) ->
    error({aihtml, {bad_option, panels, Other}}).

dl_panel({Id, Title, Body}) ->
    #{id => id_text(Id), title => Title, body => Body};
dl_panel(#{id := Id} = M) ->
    #{id => id_text(Id), title => maps:get(title, M, <<>>), body => maps:get(body, M, [])};
dl_panel(Other) ->
    error({aihtml, {bad_dock_panel, Other}}).

parse_layout(<<>>) -> [];
parse_layout(Json) when is_binary(Json) -> parse_layout(decode(Json));
parse_layout(L) when is_list(L) -> [parse_node(N) || N <- L];
parse_layout(N) -> [parse_node(N)].

-define(NODE_KEYS, [type, orientation, items, item, edge, size, active, id, pin, close,
                    x, y, width, height]).

%% -> #{kind, orientation, items (nodes or refs), item, edge, and the opts}
parse_node({split, O, Items}) -> parse_node({split, O, Items, #{}});
parse_node({split, O, Items, Opts}) when is_list(Items), is_map(Opts) ->
    node_opts(Opts, #{kind => split, orientation => orientation(O),
                      items => [parse_node(N) || N <- Items]});
parse_node({K, Refs}) when K =:= tabs; K =:= documents; K =:= float ->
    parse_node({K, Refs, #{}});
parse_node({K, Refs, Opts}) when (K =:= tabs orelse K =:= documents orelse K =:= float),
                                 is_list(Refs), is_map(Opts) ->
    node_opts(Opts, #{kind => K, items => Refs});
parse_node({panel, Ref}) -> parse_node({panel, Ref, #{}});
parse_node({panel, Ref, Opts}) when is_map(Opts) ->
    node_opts(Opts, #{kind => panel, item => Ref});
parse_node({autohide, Edge, Refs}) -> parse_node({autohide, Edge, Refs, #{}});
parse_node({autohide, Edge, Refs, Opts}) when is_list(Refs), is_map(Opts) ->
    node_opts(Opts, #{kind => autohide, edge => edge(Edge), items => Refs});
parse_node(M) when is_map(M) ->
    Opts = maps:from_list([{K, V} || K <- ?NODE_KEYS,
                                     V <- [get_key(K, M, undefined)], V =/= undefined]),
    Type = to_atom(maps:get(type, Opts, undefined), [split, tabs, documents, panel, float,
                                                     autohide]),
    Rest = maps:without([type, orientation, items, item, edge], Opts),
    List = fun(K) ->
                   case maps:get(K, Opts, []) of
                       L when is_list(L) -> L;
                       V -> error({aihtml, {bad_dock_node, {K, V}}})
                   end
           end,
    case Type of
        split ->
            parse_node({split, maps:get(orientation, Opts, horizontal),
                        [N || N <- List(items)], Rest});
        panel -> parse_node({panel, maps:get(item, Opts, undefined), Rest});
        autohide -> parse_node({autohide, maps:get(edge, Opts, left), List(items), Rest});
        K -> parse_node({K, List(items), Rest})
    end;
parse_node(Other) ->
    error({aihtml, {bad_dock_node, Other}}).

node_opts(Opts, Node) ->
    maps:fold(fun(K, V, Acc) -> Acc#{K => node_opt(K, V)} end, Node, Opts).

node_opt(size, V) -> size_opt(V);
node_opt(active, V) -> id_text(V);
node_opt(id, V) -> id_text(V);
node_opt(K, V) when K =:= pin; K =:= close -> bool(K, V);
node_opt(K, V) when (K =:= x orelse K =:= y orelse K =:= width orelse K =:= height),
                    is_number(V) -> round(V);
node_opt(K, V) -> error({aihtml, {bad_dock_node, {K, V}}}).

size_opt(N) when is_number(N), N >= 0 -> N;
size_opt(B) when is_binary(B) ->
    S = string:trim(string:trim(B, trailing, "%")),
    try binary_to_integer(S)
    catch error:badarg ->
            try binary_to_float(S)
            catch error:badarg -> error({aihtml, {bad_dock_node, {size, B}}})
            end
    end;
size_opt(L) when is_list(L) -> size_opt(unicode:characters_to_binary(L));
size_opt(V) -> error({aihtml, {bad_dock_node, {size, V}}}).

orientation(O) -> to_atom(O, [horizontal, vertical]).
edge(E) -> to_atom(E, [left, right, top, bottom]).

to_atom(A, Allowed) when is_atom(A) ->
    lists:member(A, Allowed) orelse error({aihtml, {bad_dock_node, A}}),
    A;
to_atom(B, Allowed) when is_binary(B) ->
    case [A || A <- Allowed, atom_to_binary(A) =:= B] of
        [A] -> A;
        [] -> error({aihtml, {bad_dock_node, B}})
    end;
to_atom(V, _) -> error({aihtml, {bad_dock_node, V}}).

%% Resolve panel refs, give groups ids, drop panels shown twice, unknown
%% panel ids and empty groups; a split left with one child is replaced
%% by it.
resolve_layout(Root, Nodes, Registry) ->
    {Resolved, _} = lists:mapfoldl(fun(N, St) -> resolve(N, St) end,
                                   #{reg => Registry, used => #{}, n => 0, root => Root},
                                   Nodes),
    [N || N <- Resolved, N =/= none].

resolve(#{kind := split, items := Items} = N, St0) ->
    {Kids0, St} = lists:mapfoldl(fun resolve/2, St0, Items),
    case [K || K <- Kids0, K =/= none, maps:get(kind, K) =/= float,
               maps:get(kind, K) =/= autohide] of
        [] -> {none, St};
        [One] -> {case maps:find(size, N) of
                      {ok, S} -> One#{size => S};
                      error -> maps:remove(size, One)
                  end, St};
        Kids -> {N#{items := Kids}, St}
    end;
resolve(#{kind := panel, item := Ref} = N, St0) ->
    case ref(Ref, St0) of
        {none, St} -> {none, St};
        {P, St} -> {N#{item := P}, St}
    end;
resolve(#{items := Refs} = N, St0) ->
    {Ps0, St1} = lists:mapfoldl(fun ref/2, St0, Refs),
    case [P || P <- Ps0, P =/= none] of
        [] -> {none, St1};
        [#{id := First} | _] = Ps ->
            Active = case maps:find(active, N) of
                         {ok, A} -> case [I || #{id := I} <- Ps, I =:= A] of
                                        [] -> First;
                                        _ -> A
                                    end;
                         error -> First
                     end,
            {Id, St} = group_id(N, St1),
            {defaults(N#{items := Ps, active => Active, id => Id}), St}
    end.

defaults(#{kind := float} = N) -> maps:merge(?FLOAT_DEFAULTS, N);
defaults(#{kind := autohide} = N) -> maps:merge(#{size => ?AUTOHIDE_SIZE}, N);
defaults(N) -> N.

group_id(#{id := Id}, St) -> {Id, St};
group_id(_, #{n := N, root := Root} = St) ->
    {<<Root/binary, "-g", (integer_to_binary(N + 1))/binary>>, St#{n := N + 1}}.

ref(Ref, #{reg := Reg, used := Used} = St) ->
    P = case Ref of
            {_, _, _} -> dl_panel(Ref);
            #{id := _} -> dl_panel(Ref);
            undefined -> error({aihtml, {bad_dock_node, {item, undefined}}});
            _ -> maps:get(id_text(Ref), Reg, none)
        end,
    case P of
        none -> {none, St};
        #{id := I} ->
            case maps:is_key(I, Used) of
                true -> {none, St};
                false -> {P, St#{used := Used#{I => true}}}
            end
    end.

%% The JSON value of resolved nodes (the browser writes the same form).
dl_value(Nodes) ->
    Main = [N || #{kind := K} = N <- Nodes, K =/= float, K =/= autohide],
    Ws = weights([maps:get(size, N, undefined) || N <- Main]),
    [node_value(N, W) || {N, W} <- lists:zip(Main, Ws)]
        ++ [node_value(N, undefined) || #{kind := K} = N <- Nodes,
                                        K =:= float orelse K =:= autohide].

node_value(#{kind := split, orientation := O, items := Items}, W) ->
    Ws = weights([maps:get(size, N, undefined) || N <- Items]),
    #{type => split, orientation => O, size => W,
      items => [node_value(N, X) || {N, X} <- lists:zip(Items, Ws)]};
node_value(#{kind := panel, item := #{id := P}}, W) ->
    #{type => panel, size => W, item => P};
node_value(#{kind := documents} = G, W) ->
    (group_value(G))#{type => documents, size => W, close => maps:get(close, G, false)};
node_value(#{kind := tabs} = G, W) ->
    (group_value(G))#{type => tabs, size => W, pin => maps:get(pin, G, true),
                      close => maps:get(close, G, true)};
node_value(#{kind := float} = G, _) ->
    maps:merge(group_value(G), (maps:with([x, y, width, height], G))#{type => float});
node_value(#{kind := autohide, edge := E} = G, _) ->
    (group_value(G))#{type => autohide, edge => E, size => round(maps:get(size, G)),
                      pin => maps:get(pin, G, true), close => maps:get(close, G, true)}.

group_value(#{id := Id, items := Items, active := A}) ->
    #{id => Id, items => [P || #{id := P} <- Items], active => A}.

dl_labels(Custom) when is_map(Custom) ->
    maps:foreach(fun(K, _) -> maps:is_key(K, ?DL_LABELS)
                                  orelse error({aihtml, {bad_dock_layout_label, K}})
                 end, Custom),
    maps:map(fun(_, V) -> text(V) end, maps:merge(?DL_LABELS, Custom));
dl_labels(Other) ->
    error({aihtml, {bad_option, labels, Other}}).

%%%===================================================================
%%% Records
%%%===================================================================

build(R, Css, Attrs) ->
    Tag = element(1, R),
    ?E:build(R, fields(Tag), entry(?E:component_name(Tag)), Css, Attrs).

%% @doc The field names of one of this group's records.
-spec fields(atom()) -> [atom()].
fields(ah_docking) -> record_info(fields, ah_docking);
fields(ah_dock_layout) -> record_info(fields, ah_dock_layout).

-spec render(element()) -> html().
render(#ah_docking{} = R) -> render_docking(R);
render(#ah_dock_layout{} = R) -> render_dock_layout(R).

classes(R) ->
    Tag = element(1, R),
    ?E:classes(R, fields(Tag), entry(?E:component_name(Tag))).

entry(Name) -> aihtml_catalog:entry(?MODULE, Name).

%%%===================================================================
%%% Shared
%%%===================================================================

%% The parts refer to each other by ids derived from the root's, so a root
%% without an id gets one.
ensure_id(R, Prefix) ->
    Id = case element(3, R) of
             undefined -> <<Prefix/binary, (integer_to_binary(erlang:unique_integer([positive])))/binary>>;
             Id0 -> text(Id0)
         end,
    {Id, setelement(3, R, Id)}.

tpl_bin(IoData) ->
    {safe, B} = aihtml_tpl:safe(IoData),
    B.

hidden(undefined, _) -> [];
hidden(Name, Value) -> ?H:void(input, [], [{type, hidden}, {name, Name}, {value, Value}]).

decode(Json) ->
    try json:decode(Json)
    catch error:_ -> error({aihtml, {bad_dock_json, Json}})
    end.

%% A key of a map written by hand (atom keys) or decoded from JSON
%% (binary keys).
get_key(K, M, Default) ->
    case maps:find(K, M) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(K), M, Default)
    end.

number_or(N, _) when is_number(N) -> N;
number_or(_, D) -> D.

id_text(I) when is_binary(I) -> I;
id_text(I) when is_atom(I), I =/= undefined, I =/= null -> atom_to_binary(I);
id_text(I) when is_integer(I) -> integer_to_binary(I);
id_text(I) -> error({aihtml, {bad_dock_id, I}}).

%% An id usable inside an element id.
safe_id(I) ->
    re:replace(I, <<"[^A-Za-z0-9_-]">>, <<"_">>, [global, {return, binary}]).

num(N) when is_integer(N) -> integer_to_binary(N);
num(F) when is_float(F) ->
    case F == trunc(F) of
        true -> integer_to_binary(trunc(F));
        false -> float_to_binary(F, [{decimals, 2}, compact])
    end.

bool(_, B) when is_boolean(B) -> B;
bool(K, V) -> error({aihtml, {bad_option, K, V}}).

one_of(K, V, Allowed) ->
    case lists:member(V, Allowed) of
        true -> V;
        false -> error({aihtml, {bad_option, K, V}})
    end.

text(undefined) -> <<>>;
text(B) when is_binary(B) -> B;
text(L) when is_list(L) -> unicode:characters_to_binary(L);
text(X) -> beamai_html_escape:to_binary(X, aihtml).

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => docking, category => layout,
       signature => <<"docking(Panels, Css, Attrs)">>,
       root => <<"ah-docking">>,
       groups => #{orientation => {[horizontal, vertical], horizontal}},
       flags => [disabled],
       options => [layout, allow_float, offset, drag_opacity, close_buttons,
                   collapse_buttons, labels],
       behavior => <<"docking">>,
       events => [<<"change">>, <<"ah:window-close">>, <<"ah:window-collapse">>,
                  <<"ah:window-expand">>],
       doc => <<"Panels of windows dragged by their header between panels, collapsed, closed "
                "or left floating; the layout JSON is the value, change after each move.">>,
       option_docs =>
           #{orientation => <<"horizontal: panels side by side (default); vertical: stacked.">>,
             disabled => <<"Nothing can be dragged, collapsed or closed.">>,
             layout => <<"A saved value (the JSON of data-ah-value, or its decoded map) "
                         "applied to the panels: order, floating, collapsed and closed windows.">>,
             allow_float => <<"A window dropped outside every panel floats (default true); "
                              "false puts it back.">>,
             offset => <<"Space around docked windows in px (default 5).">>,
             drag_opacity => <<"Opacity of the dragged window, 0..1 (default 0.3).">>,
             close_buttons => <<"Close buttons in the window headers (default true).">>,
             collapse_buttons => <<"Collapse buttons in the window headers (default true).">>,
             labels => <<"#{collapse, close}: the accessible names of the header buttons.">>},
       methods =>
           [#{name => collapse, args => <<"(WindowId)">>, doc => <<"Collapse a window.">>},
            #{name => expand, args => <<"(WindowId)">>, doc => <<"Expand a window.">>},
            #{name => close, args => <<"(WindowId)">>, doc => <<"Close (remove) a window.">>},
            #{name => move, args => <<"(WindowId, PanelId, Index)">>,
              doc => <<"Dock a window in a panel at an index (-1: last).">>},
            #{name => pin, args => <<"(WindowId)">>, doc => <<"Stop a window from being dragged.">>},
            #{name => unpin, args => <<"(WindowId)">>, doc => <<"Let a window be dragged again.">>},
            #{name => addWindow, args => <<"(PanelId, Html)">>,
              doc => <<"Append a server-rendered window; sent by docking_add_window/4.">>},
            #{name => setLayout, args => <<"(Json)">>,
              doc => <<"Rearrange the windows as a saved value says (no change event).">>},
            #{name => disable, args => <<"()">>, doc => <<"Turn dragging and the buttons off.">>},
            #{name => enable, args => <<"()">>, doc => <<"Turn them on again.">>},
            #{name => getValue, args => <<"()">>, doc => <<"Return the layout JSON.">>}]},
     #{name => dock_layout, category => layout,
       signature => <<"dock_layout(Layout, Css, Attrs)">>,
       root => <<"ah-dl">>,
       flags => [disabled],
       options => [panels, resizable, resize_mode, allow_float, allow_dock, min_size, labels],
       behavior => <<"dock_layout">>,
       events => [<<"change">>, <<"ah:panel-close">>],
       doc => <<"An IDE-style layout of splits, tab groups and documents: drag tabs to dock or "
                "float them, auto hide groups at an edge, resize with splitbars; the layout "
                "JSON is the value.">>,
       option_docs =>
           #{disabled => <<"No dragging, resizing or buttons.">>,
             panels => <<"[{Id, Title, Body}]: the panels the layout names by id.">>,
             resizable => <<"Splitbars resize the groups (default true); arrows too when focused.">>,
             resize_mode => <<"live (default) or feedback: a line follows the pointer and the "
                              "sizes change on release.">>,
             allow_float => <<"Tabs dropped outside the dock targets float (default true).">>,
             allow_dock => <<"Show the dock targets while dragging (default true).">>,
             min_size => <<"Smallest size in px a splitbar leaves a group (default 100).">>,
             labels => <<"#{auto_hide, float, dock, close}: menu items and button names.">>},
       methods =>
           [#{name => activate, args => <<"(PanelId)">>,
              doc => <<"Show a panel: select its tab, open its auto hide group, raise its float.">>},
            #{name => float, args => <<"(PanelId)">>, doc => <<"Float a panel.">>},
            #{name => dock, args => <<"(PanelId)">>,
              doc => <<"Dock a floating or auto hidden panel back into the layout.">>},
            #{name => close, args => <<"(PanelId)">>, doc => <<"Close a panel.">>},
            #{name => openPanel, args => <<"(GroupHtml, Where)">>,
              doc => <<"Insert a server-rendered group; sent by dock_layout_open/3,4.">>},
            #{name => getValue, args => <<"()">>, doc => <<"Return the layout JSON.">>}]}].
