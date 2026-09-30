%%%-------------------------------------------------------------------
%%% @doc A swimlane (cross-functional flow chart), ported from sigil
%%% (data/swimlane). See designs/04-components.md.
%%%
%%%   ah_swimlane(Nodes, Css, Attrs)       lanes x phases flow chart
%%%   swimlane_update(Ctx, Event, S)       (in an action) re-render after an edit
%%%
%%% Everything is rendered here, on the server, the flow lines included
%%% (SVG paths). The behaviour (assets/js/components/swimlane.ts) selects
%%% nodes and handles keyboard and drag and drop; after a local change it
%%% moves the existing nodes and recomputes the lines, but never builds
%%% HTML.
%%%
%%% == Edits ==
%%%
%%% Dragging a node (with the `editable' modifier) moves it in place and
%%% fires `ah:node-change' on the root after writing its details (node,
%%% lane, phase, oldLane, oldPhase) to data attributes, so a postback gets
%%% them in `Event.data'. The action stores the change and may answer with
%%% swimlane_update/3, rendering the swimlane again from the stored data;
%%% a refused change is undone the same way.
%%%
%%% ah_swimlane/3 builds an #ah_swimlane{} (include/aihtml_swimlane.hrl) and
%%% render/1 turns it into HTML, so pages may also write the record
%%% directly (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_swimlane).
-behaviour(aihtml_element).

-include("aihtml_swimlane.hrl").

-export([ah_swimlane/3, swimlane_update/3, render/1, fields/1, catalog/0, facade_extras/0]).

-export_type([element/0, item/0, lane/0, phase/0, flow/0, label_key/0, labels/0]).

-define(H, aihtml_html).
-define(E, aihtml_element).

%% The record field types promise well-formed data, but builders pass
%% whatever the page gives: these clauses turn it into {aihtml, _} errors.
-dialyzer({no_match, [render/1, swim_lane/1, swim_phase/1, swim_node/1, swim_flow/1,
                      labels/3]}).

-define(STACK_GAP, 10).
-define(HEADER_H, 46).
-define(SWIM_COLORS, #{default => <<"#6B7280">>, blue => <<"#3B82F6">>, green => <<"#10B981">>,
                       red => <<"#EF4444">>, orange => <<"#F59E0B">>, purple => <<"#8B5CF6">>,
                       teal => <<"#14B8A6">>, pink => <<"#EC4899">>, indigo => <<"#6366F1">>,
                       yellow => <<"#EAB308">>}).

%% A swimlane lane (row, role) and phase (column, stage).
-type lane() :: #{id := term(), name => unicode:chardata(),
                  color => atom() | unicode:chardata()}.
-type phase() :: #{id := term(), label => unicode:chardata()}.
%% A swimlane node, placed in the cell of its lane and phase (or, on a
%% continuous axis, at its `value'). Nodes of one cell stack vertically.
%% `color' is a CSS colour or one of default, blue, green, red, orange,
%% purple, teal, pink, indigo, yellow; it defaults to the lane's.
-type item() :: #{id := term(), lane := term(), phase => term(),
                  label => unicode:chardata(),
                  type => start | task | decision | 'end',
                  color => atom() | unicode:chardata(),
                  variant => solid | outline, dimmed => boolean(),
                  value => number()}.
%% A connection between two nodes, drawn as an orthogonal line.
-type flow() :: #{from := term(), to := term(), label => unicode:chardata(),
                  dashed => boolean(), arrow => boolean()}.
-type label_key() :: corner | start | task | decision | 'end'.
%% Texts of the swimlane: the corner cell and the legend entries.
-type labels() :: #{label_key() => unicode:chardata()}.
-type element() :: #ah_swimlane{}.

%% @doc A swimlane (sigil's swimlane): a grid of lanes (rows) by phases
%% (columns) with `Nodes' in the cells and flows between them. Nodes are
%% maps (see `item()': id, lane, phase, label, type (start, task,
%% decision, end), color, variant (solid, outline), dimmed, value).
%%
%% Css: `editable' (drag nodes to another cell, or Shift+arrow keys),
%% `legend' (a legend of the node types).
%% Options (in Attrs): `lanes' (`#{id, name, color}'), `phases'
%% (`#{id, label}'), `flows' (`#{from, to, label, dashed, arrow}'),
%% `selected' (the selected node id), `axis' (discrete (default) or
%% continuous: nodes placed by their numeric `value'), `value_domain'
%% ({Min, Max}, default the values' range), `value_ticks', `axis_width'
%% (px), `lane_height' (110), `phase_width' (190), `node_width' (132),
%% `node_height' (52), `lane_label_width' (150), `height' (px, default
%% the content's), `labels' (corner, start, task, decision, end).
-spec ah_swimlane([item()], aihtml_html:css(), aihtml_html:attrs()) -> #ah_swimlane{}.
ah_swimlane(Nodes, Css, Attrs) ->
    ?E:build(?MODULE, #ah_swimlane{items = Nodes}, Css, Attrs).

%% @doc The field names of #ah_swimlane{}.
-spec fields(atom()) -> [atom()].
fields(ah_swimlane) -> record_info(fields, ah_swimlane).

-spec render(element()) -> aihtml_html:html().
render(#ah_swimlane{items = Items, editable = Editable} = R0) ->
    {Id, R} = ensure_id(R0),
    Classes = ?E:classes(?MODULE, R),
    %% the defaults are the current language's (aihtml_i18n)
    L = labels(aihtml_i18n:texts(swimlane), R#ah_swimlane.labels, swimlane),
    LH = pos_int(lane_height, R#ah_swimlane.lane_height),
    PW = pos_int(phase_width, R#ah_swimlane.phase_width),
    NW = pos_int(node_width, R#ah_swimlane.node_width),
    NH = pos_int(node_height, R#ah_swimlane.node_height),
    LLW = pos_int(lane_label_width, R#ah_swimlane.lane_label_width),
    Continuous = case R#ah_swimlane.axis of
                     discrete -> false;
                     continuous -> true;
                     A -> error({aihtml, {bad_option, axis, A}})
                 end,
    is_list(Items) orelse error({aihtml, {bad_swimlane_nodes, Items}}),
    Lanes = [swim_lane(X) || X <- R#ah_swimlane.lanes],
    Phases = [swim_phase(X) || X <- R#ah_swimlane.phases],
    Nodes = [swim_node(X) || X <- Items],
    Flows = [swim_flow(X) || X <- R#ah_swimlane.flows],
    LaneIdx = index_map(Lanes),
    PhaseIdx = index_map(Phases),
    NPh = max(1, length(Phases)),
    AW = case R#ah_swimlane.axis_width of
             undefined -> PW * NPh;
             W -> pos_int(axis_width, W)
         end,
    {VMin, VMax} = case R#ah_swimlane.value_domain of
                       {A0, B0} when is_number(A0), is_number(B0) -> {A0, B0};
                       undefined ->
                           case [V || #{value := V} <- Nodes, is_number(V)] of
                               [] -> {0, 1};
                               Vs -> {lists:min(Vs), lists:max(Vs)}
                           end;
                       Dom -> error({aihtml, {bad_option, value_domain, Dom}})
                   end,
    Boxed = case Continuous of
                false ->
                    Valid = [N || #{lane := Ln, phase := Ph} = N <- Nodes,
                                  maps:is_key(Ln, LaneIdx), maps:is_key(Ph, PhaseIdx)],
                    Cell = fun(#{lane := Ln, phase := Ph}) -> {Ln, Ph} end,
                    {Out, _} = lists:mapfoldl(
                                 fun(N, Seen) ->
                                         C = Cell(N),
                                         K = maps:get(C, Seen, 0),
                                         {{N, K}, Seen#{C => K + 1}}
                                 end, #{}, Valid),
                    Counts = lists:foldl(fun(N, Acc) -> maps:update_with(Cell(N), fun(X) -> X + 1 end, 1, Acc) end,
                                         #{}, Valid),
                    [{N, #{x => maps:get(Ph, PhaseIdx) * PW + (PW - NW) / 2,
                           y => maps:get(Ln, LaneIdx) * LH
                               + stack_offset(K, maps:get(Cell(N), Counts), LH, NH),
                           w => NW, h => NH}}
                     || {#{lane := Ln, phase := Ph} = N, K} <- Out];
                true ->
                    [{N, #{x => value_center(V, VMin, VMax, AW, NW) - NW / 2,
                           y => maps:get(Ln, LaneIdx) * LH + (LH - NH) / 2,
                           w => NW, h => NH}}
                     || #{lane := Ln, value := V} = N <- Nodes,
                        maps:is_key(Ln, LaneIdx), is_number(V)]
            end,
    ById = maps:from_list([{I, B} || {#{id := I}, B} <- Boxed]),
    TotalW = case Continuous of true -> AW; false -> PW * NPh end,
    TotalH = LH * max(1, length(Lanes)),
    Sel = case R#ah_swimlane.selected of undefined -> undefined; S -> text(S) end,
    LaneById = maps:from_list([{I, Ln} || #{id := I} = Ln <- Lanes]),
    Header = case Continuous of
                 false ->
                     [?H:el('div', Lbl, [<<"ah-swimlane-grid__phase">>],
                            [{data_phase_id, PId}, {style, [<<"width:">>, num(PW), <<"px">>]}])
                      || #{id := PId, label := Lbl} <- Phases];
                 true ->
                     Ticks = case R#ah_swimlane.value_ticks of
                                 undefined -> axis_ticks(VMin, VMax, 4);
                                 Ts -> Ts
                             end,
                     [?H:el('div', integer_to_binary(round(T)), [<<"ah-swimlane-grid__tick-label">>],
                            [{style, [<<"position:absolute;left:">>,
                                      num(value_center(T, VMin, VMax, AW, NW)),
                                      <<"px;transform:translateX(-50%)">>]}])
                      || T <- Ticks]
             end,
    NodeEl = fun({#{id := NId, lane := Ln, label := Lbl, type := Type, variant := Var,
                    dimmed := Dim} = N, #{x := X, y := Y, w := W, h := Hh}}) ->
                     Color = case maps:get(color, N) of
                                 undefined -> maps:get(color, maps:get(Ln, LaneById), maps:get(default, ?SWIM_COLORS));
                                 C -> C
                             end,
                     ?H:el('div', ?H:el(span, Lbl, [<<"ah-swimlane-node__label">>], []),
                           [<<"ah-swimlane-node">>],
                           [{data_id, NId}, {data_lane, Ln}, {data_phase, maps:get(phase, N)},
                            {data_value, case maps:get(value, N) of
                                             V when is_number(V) -> num(V);
                                             _ -> undefined
                                         end},
                            {data_type, Type}, {data_variant, Var},
                            {data_dimmed, Dim andalso <<"true">>},
                            {data_state, NId =:= Sel andalso <<"selected">>},
                            {tabindex, 0}, {role, button},
                            {aria_pressed, atom_to_binary(NId =:= Sel)},
                            {aria_label, Lbl},
                            {style, [<<"left:">>, num(X), <<"px;top:">>, num(Y), <<"px;width:">>,
                                     num(W), <<"px;height:">>, num(Hh), <<"px;">>,
                                     case Var of
                                         outline -> [<<"color:">>, Color];
                                         solid -> [<<"background-color:">>, Color]
                                     end]}])
             end,
    Canvas = [flows_svg(Id, Flows, ById, Sel, TotalW, TotalH),
              [?H:el('div', [], [<<"ah-swimlane-grid__lane-band">>],
                     [{data_odd, I rem 2 =:= 1 andalso <<"true">>},
                      {style, [<<"top:">>, num(I * LH), <<"px;height:">>, num(LH), <<"px;width:">>,
                               num(TotalW), <<"px">>]}])
               || I <- lists:seq(0, length(Lanes) - 1)],
              [[?H:el('div', [], [<<"ah-swimlane-grid__phase-sep">>],
                      [{style, [<<"left:">>, num(I * PW), <<"px;height:">>, num(TotalH), <<"px">>]}])
                || I <- lists:seq(1, max(0, length(Phases) - 1))] || not Continuous],
              [NodeEl(B) || B <- Boxed]],
    ?H:el('div',
          [?H:el('div',
                 [?H:el('div',
                        [?H:el('div', maps:get(corner, L), [<<"ah-swimlane-lanes__corner">>],
                               [{style, [<<"height:">>, num(?HEADER_H), <<"px">>]}]),
                         ?H:el('div',
                               [?H:el('div',
                                      [?H:el(span, [], [<<"ah-swimlane-lanes__accent">>],
                                             [{style, [<<"background-color:">>, C]}]),
                                       ?H:el(span, Nm, [<<"ah-swimlane-lanes__name">>], [])],
                                      [<<"ah-swimlane-lanes__row">>],
                                      [{data_lane_id, LId}, {style, [<<"height:">>, num(LH), <<"px">>]}])
                                || #{id := LId, name := Nm, color := C} <- Lanes],
                               [<<"ah-swimlane-lanes__body">>], [])],
                        [<<"ah-swimlane-lanes">>], [{style, [<<"width:">>, num(LLW), <<"px">>]}]),
                  ?H:el('div',
                        [?H:el('div',
                               ?H:el('div', Header, [<<"ah-swimlane-grid__header-track">>],
                                     [{style, [<<"width:">>, num(TotalW), <<"px">>,
                                               [<<";position:relative">> || Continuous]]}]),
                               [<<"ah-swimlane-grid__header">>],
                               [{style, [<<"height:">>, num(?HEADER_H), <<"px">>]}]),
                         ?H:el('div',
                               ?H:el('div', Canvas, [<<"ah-swimlane-grid__canvas">>],
                                     [{style, [<<"width:">>, num(TotalW), <<"px;height:">>,
                                               num(TotalH), <<"px">>]}]),
                               [<<"ah-swimlane-grid__body">>], [])],
                        [<<"ah-swimlane-grid">>], [])],
                 [<<"ah-swimlane-content">>], []),
           [?H:el('div',
                  [?H:el('div',
                         [?H:el(span, [], [<<"ah-swimlane-legend__swatch">>], [{data_type, T}]),
                          ?H:el(span, maps:get(T, L), [<<"ah-swimlane-legend__label">>], [])],
                         [<<"ah-swimlane-legend__item">>], [])
                   || T <- [start, task, decision, 'end']],
                  [<<"ah-swimlane-legend">>], []) || R#ah_swimlane.legend]],
          Classes,
          [[{id, Id}, {data_ah, <<"swimlane">>},
            {style, height_style(R#ah_swimlane.height)},
            {data_axis, R#ah_swimlane.axis},
            {data_lane_height, LH}, {data_phase_width, PW},
            {data_node_width, NW}, {data_node_height, NH},
            {data_editable, Editable}, {data_selected, Sel}],
           ?E:root_attrs(R, 'ah:node-change')]).

swim_lane(#{id := Id} = Ln) ->
    #{id => text(Id), name => text(maps:get(name, Ln, Id)),
      color => swim_color(maps:get(color, Ln, default))};
swim_lane(Other) -> error({aihtml, {bad_swimlane_lane, Other}}).

swim_phase(#{id := Id} = P) -> #{id => text(Id), label => text(maps:get(label, P, Id))};
swim_phase(Other) -> error({aihtml, {bad_swimlane_phase, Other}}).

swim_node(#{id := Id, lane := Ln} = N) ->
    Type = maps:get(type, N, task),
    lists:member(Type, [start, task, decision, 'end'])
        orelse error({aihtml, {bad_swimlane_node_type, Type}}),
    Var = maps:get(variant, N, solid),
    lists:member(Var, [solid, outline]) orelse error({aihtml, {bad_swimlane_variant, Var}}),
    #{id => text(Id), lane => text(Ln),
      phase => case maps:get(phase, N, undefined) of undefined -> undefined; P -> text(P) end,
      label => text(maps:get(label, N, Id)), type => Type, variant => Var,
      color => case maps:get(color, N, undefined) of undefined -> undefined; C -> swim_color(C) end,
      dimmed => maps:get(dimmed, N, false) =:= true,
      value => maps:get(value, N, undefined)};
swim_node(Other) -> error({aihtml, {bad_swimlane_node, Other}}).

swim_flow(#{from := F, to := T} = Fl) ->
    #{from => text(F), to => text(T),
      label => case maps:get(label, Fl, undefined) of undefined -> <<>>; Lb -> text(Lb) end,
      dashed => maps:get(dashed, Fl, false) =:= true,
      arrow => maps:get(arrow, Fl, true) =/= false};
swim_flow(Other) -> error({aihtml, {bad_swimlane_flow, Other}}).

swim_color(C) when is_atom(C) ->
    case maps:find(C, ?SWIM_COLORS) of
        {ok, Hex} -> Hex;
        error -> error({aihtml, {bad_color, C}})
    end;
swim_color(C) ->
    B = text(C),
    case catch binary_to_existing_atom(B) of
        A when is_atom(A), is_map_key(A, ?SWIM_COLORS) -> maps:get(A, ?SWIM_COLORS);
        _ -> color(B)
    end.

index_map(Items) ->
    maps:from_list(lists:zip([I || #{id := I} <- Items], lists:seq(0, length(Items) - 1))).

%% Top of the K-th of N nodes stacked, centred, in a lane.
stack_offset(K, N, LH, NH) ->
    (LH - (N * NH + (N - 1) * ?STACK_GAP)) / 2 + K * (NH + ?STACK_GAP).

value_center(_, Min, Max, AW, _) when Min == Max -> AW / 2;
value_center(V, Min, Max, AW, NW) -> NW / 2 + max(0, AW - NW) * (V - Min) / (Max - Min).

axis_ticks(Min, Max, N) when Min == Max; N < 1 -> [Min];
axis_ticks(Min, Max, N) -> [Min + (Max - Min) * I / N || I <- lists:seq(0, N)].

flows_svg(Id, Flows, ById, Sel, W, H) ->
    Marker = fun(Suffix, Cls) ->
                     ?H:el(marker,
                           ?H:el(path, [], [Cls], [{d, <<"M0,0 L7,3 L0,6 Z">>}]),
                           [], [{id, [Id, <<"-arrow">>, Suffix]},
                                {<<"markerWidth">>, 9}, {<<"markerHeight">>, 9},
                                {<<"refX">>, 7}, {<<"refY">>, 3}, {orient, auto},
                                {<<"markerUnits">>, <<"userSpaceOnUse">>}])
             end,
    Defs = ?H:el(defs, [Marker(<<>>, <<"ah-swimlane-flows__head">>),
                        Marker(<<"-active">>,
                               <<"ah-swimlane-flows__head ah-swimlane-flows__head--active">>)],
                 [], []),
    G = fun(#{from := F, to := T, label := Lbl, dashed := Dashed, arrow := Arrow}) ->
                case {maps:find(F, ById), maps:find(T, ById)} of
                    {{ok, Src}, {ok, Tgt}} ->
                        Active = Sel =/= undefined andalso (F =:= Sel orelse T =:= Sel),
                        Dim = Sel =/= undefined andalso not Active,
                        Pts = flow_points(Src, Tgt),
                        {Mx, My} = mid_point(Pts),
                        LW = max(30, 14 + string:length(Lbl) * 8),
                        [?H:el(g,
                               [?H:el(path, [], [<<"ah-swimlane-flows__line">>],
                                      [{d, points_path(Pts, 10)}, {fill, none},
                                       {data_dashed, Dashed andalso <<"true">>},
                                       {marker_end, case Arrow of
                                                        true -> [<<"url(#">>, Id, <<"-arrow">>,
                                                                 [<<"-active">> || Active], <<")">>];
                                                        false -> undefined
                                                    end}]),
                                [[?H:el(rect, [], [<<"ah-swimlane-flows__label-bg">>],
                                        [{x, num(Mx - LW / 2)}, {y, num(My - 9)}, {width, num(LW)},
                                         {height, 18}, {rx, 5}]),
                                  ?H:el(text, Lbl, [<<"ah-swimlane-flows__label">>],
                                        [{x, num(Mx)}, {y, num(My)}, {text_anchor, middle},
                                         {dominant_baseline, central}])] || Lbl =/= <<>>]],
                               [], [{data_from, F}, {data_to, T},
                                    {data_active, Active andalso <<"true">>},
                                    {data_dim, Dim andalso <<"true">>}])];
                    _ -> []
                end
        end,
    ?H:el(svg, [Defs, [G(Fl) || Fl <- Flows]], [<<"ah-swimlane-flows">>],
          [{width, num(W)}, {height, num(H)},
           {<<"viewBox">>, [<<"0 0 ">>, num(W), <<" ">>, num(H)]}, {aria_hidden, <<"true">>}]).

%% sigil's flow-points: an orthogonal polyline between two node boxes.
flow_points(#{x := SX0, y := SY0, w := SW, h := SH}, #{x := TX0, y := TY0, w := TW, h := TH}) ->
    Sx = SX0 + SW, Sy = SY0 + SH / 2,
    Tx = TX0, Ty = TY0 + TH / 2,
    Scx = SX0 + SW / 2, Sty = SY0, Sby = SY0 + SH,
    Tcx = TX0 + TW / 2, Tty = TY0, Tby = TY0 + TH,
    Overlap = abs(Scx - Tcx) < max(SW, TW),
    Below = Tty >= Sby,
    Above = Tby =< Sty,
    Gap = if Below -> Tty - Sby; Above -> Sty - Tby; true -> 0 end,
    Tight = Overlap andalso (Below orelse Above) andalso Gap < 24,
    if
        Tx >= Sx + 8 ->
            Mx = (Sx + Tx) / 2,
            [{Sx, Sy}, {Mx, Sy}, {Mx, Ty}, {Tx, Ty}];
        Tight ->
            Lx = min(SX0, TX0) - 24,
            [{SX0, Sy}, {Lx, Sy}, {Lx, Ty}, {TX0, Ty}];
        Overlap andalso Below ->
            My = (Sby + Tty) / 2,
            [{Scx, Sby}, {Scx, My}, {Tcx, My}, {Tcx, Tty}];
        Overlap andalso Above ->
            My = (Sty + Tby) / 2,
            [{Scx, Sty}, {Scx, My}, {Tcx, My}, {Tcx, Tby}];
        true ->
            Txr = TX0 + TW,
            Mx = (SX0 + Txr) / 2,
            [{SX0, Sy}, {Mx, Sy}, {Mx, Ty}, {Txr, Ty}]
    end.

mid_point(Pts) ->
    N = length(Pts),
    I = (N - 1) div 2,
    {Ax, Ay} = lists:nth(I + 1, Pts),
    {Bx, By} = lists:nth(min(I + 2, N), Pts),
    {(Ax + Bx) / 2, (Ay + By) / 2}.

%% sigil's points->path: a polyline with rounded corners.
points_path([{X0, Y0} | _] = Pts, Radius) ->
    T = list_to_tuple(Pts),
    N = tuple_size(T),
    lists:join(<<" ">>,
               [[<<"M">>, num2(X0), <<",">>, num2(Y0)]
                | [corner(element(I - 1, T), element(I, T),
                          case I of N -> last; _ -> element(I + 1, T) end, Radius)
                   || I <- lists:seq(2, N)]]).

corner(_, {X, Y}, last, _) -> [<<"L">>, num2(X), <<",">>, num2(Y)];
corner(Prev, {Px, Py} = P, Next, Radius) ->
    L1 = seg_len(Prev, P), L2 = seg_len(P, Next),
    R1 = min(Radius, L1 / 2), R2 = min(Radius, L2 / 2),
    {Ax, Ay} = case L1 > 0 of true -> lerp(Prev, P, (L1 - R1) / L1); false -> P end,
    {Bx, By} = case L2 > 0 of true -> lerp(P, Next, R2 / L2); false -> P end,
    [<<"L">>, num2(Ax), <<",">>, num2(Ay), <<" Q">>, num2(Px), <<",">>, num2(Py),
     <<" ">>, num2(Bx), <<",">>, num2(By)].

seg_len({Ax, Ay}, {Bx, By}) -> math:sqrt((Bx - Ax) * (Bx - Ax) + (By - Ay) * (By - Ay)).
lerp({Ax, Ay}, {Bx, By}, T) -> {Ax + (Bx - Ax) * T, Ay + (By - Ay) * T}.

%% @doc Answer an edit of a swimlane: render `Swimlane' (built from the
%% stored nodes) in place of the one that fired `Event', keeping its id
%% and selection. Sends one morph operation.
-spec swimlane_update(aihtml_action:ctx(), aihtml_action:event(), #ah_swimlane{}) -> ok.
swimlane_update(Ctx, #{id := Id, data := Data}, #ah_swimlane{} = S) ->
    Sel = case maps:get(<<"selected">>, Data, undefined) of
              undefined -> S#ah_swimlane.selected;
              <<>> -> undefined;
              V -> V
          end,
    aihtml_action:html(Ctx, {id, Id}, S#ah_swimlane{id = Id, selected = Sel}, morph).

%% @doc Functions besides the component that the aihtml facade re-exports.
-spec facade_extras() -> [{atom(), arity()}].
facade_extras() -> [{swimlane_update, 3}].

%%%===================================================================
%%% Internal
%%%===================================================================

%% A root without an id gets one: the behaviour and the update functions
%% refer to it. Returns the id and the record holding it, for root_attrs/2.
ensure_id(R) ->
    Id = case R#ah_swimlane.id of
             undefined -> <<"ah-s", (integer_to_binary(erlang:unique_integer([positive])))/binary>>;
             Id0 -> text(Id0)
         end,
    {Id, R#ah_swimlane{id = Id}}.

height_style(undefined) -> undefined;
height_style(H) when is_integer(H), H > 0 -> [<<"height:">>, integer_to_binary(H), <<"px;">>];
height_style(H) -> error({aihtml, {bad_option, height, H}}).

pos_int(_, N) when is_integer(N), N > 0 -> N;
pos_int(K, V) -> error({aihtml, {bad_option, K, V}}).

%% Merge custom texts over the defaults: unknown keys fail, lists of
%% months (12) and weekdays (7) are checked.
labels(Defaults, Custom, Comp) when is_map(Custom) ->
    maps:foreach(fun(K, _) -> maps:is_key(K, Defaults)
                                  orelse error({aihtml, {bad_label, Comp, K}})
                 end, Custom),
    M = maps:map(fun(_, V) when is_list(V), V =/= [], not is_integer(hd(V)) -> [text(X) || X <- V];
                    (_, V) -> text(V)
                 end, maps:merge(Defaults, Custom)),
    [length(maps:get(K, M)) =:= N orelse error({aihtml, {bad_label, Comp, K}})
     || {K, N} <- [{months, 12}, {months_short, 12}, {weekdays, 7}, {weekdays_short, 7}],
        maps:is_key(K, M)],
    M;
labels(_, Other, Comp) -> error({aihtml, {bad_label, Comp, Other}}).

%% A colour for a style attribute: nothing that could end the declaration.
color(C) ->
    B = text(C),
    case re:run(B, <<"^[#a-zA-Z0-9(),.%\\s-]+$">>) of
        {match, _} -> B;
        nomatch -> error({aihtml, {bad_color, C}})
    end.

%% A CSS number: integers as such, others with up to 3 decimals.
num(N) when is_integer(N) -> integer_to_binary(N);
num(F) when is_float(F) ->
    R = round(F),
    case abs(F - R) < 0.0005 of
        true -> integer_to_binary(R);
        false -> float_to_binary(F, [{decimals, 3}, compact])
    end.

%% sigil's fmt in points->path: 2 decimals.
num2(N) when is_integer(N) -> integer_to_binary(N);
num2(F) ->
    R = round(F),
    case abs(F - R) < 0.005 of
        true -> integer_to_binary(R);
        false -> float_to_binary(F, [{decimals, 2}, compact])
    end.

text(undefined) -> undefined;
text(B) when is_binary(B) -> B;
text(A) when is_atom(A) -> atom_to_binary(A);
text(I) when is_integer(I) -> integer_to_binary(I);
text(L) when is_list(L) -> unicode:characters_to_binary(L);
text(X) -> beamai_html_escape:to_binary(X, aihtml).

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => swimlane, category => data,
       signature => <<"ah_swimlane(Nodes, Css, Attrs)">>,
       root => <<"ah-swimlane">>,
       flags => [editable, legend],
       classes => #{editable => [], legend => []},
       options => [lanes, phases, flows, selected, axis, value_domain, value_ticks,
                   axis_width, lane_height, phase_width, node_width, node_height,
                   lane_label_width, height, labels],
       behavior => <<"swimlane">>,
       events => [<<"ah:node-change">>, <<"ah:select">>, <<"ah:node-click">>,
                  <<"ah:lane-click">>],
       doc => <<"A cross-functional flow chart: lanes by phases, nodes (start, task, decision, "
                "end) in the cells, orthogonal flow lines; selecting a node highlights its "
                "flows, dragging moves it to another cell (ah:node-change).">>,
       option_docs =>
           #{editable => <<"Drag nodes to another lane and phase; Shift+arrow keys move the "
                           "focused node.">>,
             legend => <<"Show a legend of the node types.">>,
             lanes => <<"Lanes (rows) #{id, name, color}.">>,
             phases => <<"Phases (columns) #{id, label}.">>,
             flows => <<"Flows #{from, to, label, dashed, arrow} between node ids.">>,
             selected => <<"Id of the selected node.">>,
             axis => <<"discrete (default: phase columns) or continuous (x from the nodes' "
                       "numeric value).">>,
             value_domain => <<"{Min, Max} of a continuous axis (default the values' range).">>,
             value_ticks => <<"Tick values of a continuous axis (default 5 even ticks).">>,
             axis_width => <<"Width of a continuous axis in px.">>,
             lane_height => <<"Lane height in px (default 110).">>,
             phase_width => <<"Phase width in px (default 190).">>,
             node_width => <<"Node width in px (default 132).">>,
             node_height => <<"Node height in px (default 52).">>,
             lane_label_width => <<"Width of the lane names in px (default 150).">>,
             height => <<"Height in px (default the content's).">>,
             labels => <<"Map of corner, start, task, decision and end (legend).">>},
       methods =>
           [#{name => select, args => <<"(NodeId | null)">>,
              doc => <<"Select a node without firing an event.">>},
            #{name => moveNode, args => <<"(NodeId, LaneId[, PhaseId])">>,
              doc => <<"Move a node to another cell without firing an event.">>}]}].
