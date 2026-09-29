%%%-------------------------------------------------------------------
%%% @doc Panels of windows, ported from sigil (layout/docking). DOM and
%%% class names are sigil's, so the styles in
%%% priv/css/sigil/components/docking.css apply unchanged.
%%%
%%%   docking(Panels, Css, Attrs)        panels of windows, dragged between them
%%%   docking_add_window(Ctx, Target, PanelId, Window)
%%%                                      (in an action) add a window
%%%
%%% == The layout is view state ==
%%%
%%% The component keeps the arrangement the user made in the browser and
%%% exposes it as JSON in `data-ah-value' (and a hidden input with `name').
%%% After every rearrangement it fires `change' on the root, so an action
%%% bound with `on(change, Ref)' (or a record's `postback') receives the
%%% layout in `Event.value' and can store it per user. The page renders
%%% that JSON again: `docking(Panels, Css, [{layout, Json}])'. Window
%%% contents are ordinary server-rendered HTML; the JSON holds only ids,
%%% order, sizes and states:
%%%
%%%   {"panels": [{"id": "a", "windows": ["w1", "w2"]}, ...],
%%%    "floating": [{"id": "w3", "x": 40, "y": 30, "width": 280}],
%%%    "collapsed": ["w2"], "closed": ["w4"]}
%%%
%%% Windows the JSON does not mention (added to the page later) stay in
%%% their own panel.
%%%
%%% Behaviour: assets/js/components/docking.js. docking/3 builds an
%%% #ah_docking{} (include/aihtml_docking.hrl) and render/1 turns it into
%%% HTML.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_docking).
-behaviour(aihtml_element).

-include("aihtml_docking.hrl").

-export([docking/3, docking_add_window/4, render/1, fields/1, catalog/0, facade_extras/0]).

-export_type([window_opts/0, window/0, panel/0, saved/0, orientation/0]).

%% Record fields are checked at render time for values their types rule
%% out (pages may build records from untyped data).
-dialyzer({no_match, [dk_panel/1, dk_labels/1]}).

-define(H, aihtml_html).
-define(E, aihtml_element).
-define(L, aihtml_lib_dock).

%% Options of a docking window: `collapsed' (only its header shows),
%% `pinned' (cannot be dragged) and `floating' ({X, Y} or {X, Y, Width}
%% in px from the container's top left: the window floats over the
%% panels).
-type window_opts() :: #{collapsed => boolean(), pinned => boolean(),
                         floating => {number(), number()}
                                   | {number(), number(), number()}}.
%% A docking window: {Id, Title, Body}, {Id, Title, Body, Opts} or a map
%% with `id', `title', `body' and the options.
-type window() :: {aihtml_lib_dock:id(), unicode:chardata(), aihtml_html:html()}
                | {aihtml_lib_dock:id(), unicode:chardata(), aihtml_html:html(),
                   window_opts()}
                | #{id := aihtml_lib_dock:id(), title => unicode:chardata(),
                    body => aihtml_html:html(), collapsed => boolean(),
                    pinned => boolean(), floating => tuple()}.
%% A docking panel (a column or row of windows): {Id, Windows} or
%% #{id := Id, windows := Windows}.
-type panel() :: {aihtml_lib_dock:id(), [window()]}
               | #{id := aihtml_lib_dock:id(), windows := [window()]}.
%% The saved docking layout: the JSON of data-ah-value (a binary) or its
%% json:decode/1 map.
-type saved() :: undefined | binary() | map().
-type orientation() :: horizontal | vertical.

-define(DK_LABELS, #{collapse => <<"Collapse">>, close => <<"Close">>}).

%%%===================================================================
%%% Builder
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
-spec docking([panel()], aihtml_html:css(), aihtml_html:attrs()) -> #ah_docking{}.
docking(Panels, Css, Attrs) ->
    ?E:build(?MODULE, #ah_docking{items = Panels}, Css, Attrs).

%%%===================================================================
%%% Server-driven operations
%%%===================================================================

%% @doc In an action: render `Window' (as in docking/3) and append it to
%% the panel `PanelId' of the docking `{id, RootId}'. The browser fires
%% `change'.
-spec docking_add_window(aihtml_action:ctx(), {id, iodata() | atom()}, aihtml_lib_dock:id(),
                         window()) -> ok.
docking_add_window(Ctx, {id, Root0}, PanelId, Window) ->
    Root = ?L:text(Root0),
    W = window(Window),
    Html = ?H:render_binary(render_window(Root, W#{floating := undefined}, true, true,
                                          dk_labels(#{}))),
    aihtml_action:call(Ctx, {id, Root}, addWindow, [?L:id_text(PanelId), Html]).

%% @doc Functions besides the component that the aihtml facade re-exports.
-spec facade_extras() -> [{atom(), arity()}].
facade_extras() -> [{docking_add_window, 4}].

%% @doc The field names of #ah_docking{}.
-spec fields(atom()) -> [atom()].
fields(ah_docking) -> record_info(fields, ah_docking).

%%%===================================================================
%%% Rendering
%%%===================================================================

-spec render(#ah_docking{}) -> aihtml_html:html().
render(#ah_docking{} = R) -> render_docking(R).

render_docking(#ah_docking{items = Panels0} = R0) ->
    {Id, R} = ?L:ensure_id(R0, <<"ah-dk">>),
    Classes = ?E:classes(?MODULE, R),
    ?L:bool(allow_float, R#ah_docking.allow_float),
    ?L:bool(close_buttons, R#ah_docking.close_buttons),
    ?L:bool(collapse_buttons, R#ah_docking.collapse_buttons),
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
           ?L:hidden(R#ah_docking.name, Value)],
          Classes,
          [[{id, Id}, {data_ah, <<"docking">>}, {data_ah_value, Value},
            {data_ah_allow_float, not R#ah_docking.allow_float andalso <<"false">>},
            {data_ah_drag_opacity, ?L:num(Opacity)},
            {style, case Offset of
                        undefined -> undefined;
                        _ -> <<"--ah-docking-offset:", (integer_to_binary(Offset))/binary, "px">>
                    end},
            {aria_disabled, R#ah_docking.disabled andalso <<"true">>}],
           ?E:root_attrs(R, change)]).

render_window(Root, #{id := W, title := Title, body := Body, collapsed := Collapsed,
                      pinned := Pinned, floating := Floating}, Close, Collapse, Labels) ->
    Base = <<Root/binary, "-w-", (?L:safe_id(W))/binary>>,
    TitleId = <<Base/binary, "-title">>,
    ContentId = <<Base/binary, "-content">>,
    {State, Style} = case Floating of
                         undefined -> {<<"ah-docking-window-docked">>, undefined};
                         {X, Y, Wd} ->
                             {<<"ah-docking-window-floating">>,
                              iolist_to_binary([<<"left:">>, ?L:num(X), <<"px;top:">>, ?L:num(Y), <<"px;">>,
                                                [[<<"width:">>, ?L:num(Wd), <<"px;">>]
                                                 || Wd =/= undefined]])}
                     end,
    ?H:el('div',
          [?H:el('div',
                 [?H:el('div', ?L:text(Title), [<<"ah-docking-window-title">>], [{id, TitleId}]),
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
    #{id => ?L:id_text(Id), windows => [window(W) || W <- Windows]};
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
    #{id => ?L:id_text(Id),
      title => maps:get(title, M, <<>>),
      body => maps:get(body, M, []),
      collapsed => ?L:bool(collapsed, maps:get(collapsed, M, false)),
      pinned => ?L:bool(pinned, maps:get(pinned, M, false)),
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
    saved_docking(?L:decode(Json));
saved_docking(M) when is_map(M) ->
    Get = fun(K) -> ?L:get_key(K, M, []) end,
    L = fun(_, V) when is_list(V) -> V;
           (K, V) -> error({aihtml, {bad_docking_layout, {K, V}}})
        end,
    #{panels => [{?L:id_text(?L:get_key(id, P, undefined)),
                  [?L:id_text(W) || W <- L(windows, ?L:get_key(windows, P, []))]}
                 || P <- L(panels, Get(panels)), is_map(P)],
      floating => [{?L:id_text(?L:get_key(id, F, undefined)),
                    {number_or(?L:get_key(x, F, 0), 0), number_or(?L:get_key(y, F, 0), 0),
                     case ?L:get_key(width, F, undefined) of
                         Wd when is_number(Wd) -> Wd;
                         _ -> undefined
                     end}}
                   || F <- L(floating, Get(floating)), is_map(F)],
      collapsed => [?L:id_text(W) || W <- L(collapsed, Get(collapsed))],
      closed => [?L:id_text(W) || W <- L(closed, Get(closed))]};
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
    maps:map(fun(_, V) -> ?L:text(V) end, maps:merge(?DK_LABELS, Custom));
dk_labels(Other) ->
    error({aihtml, {bad_option, labels, Other}}).

number_or(N, _) when is_number(N) -> N;
number_or(_, D) -> D.

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
            #{name => getValue, args => <<"()">>, doc => <<"Return the layout JSON.">>}]}].
