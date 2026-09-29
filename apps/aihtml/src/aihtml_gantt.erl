%%%-------------------------------------------------------------------
%%% @doc A gantt chart, ported from sigil (data/gantt). See
%%% designs/04-components.md.
%%%
%%%   gantt(Tasks, Css, Attrs)             task bars on a day scale
%%%   gantt_update(Ctx, Event, G)          (in an action) re-render after an edit
%%%
%%% Everything is rendered here, on the server: the rows, bars, summary
%%% bars and the dependency lines (SVG paths). The behaviour
%%% (assets/js/components/gantt.ts) handles scrolling, keyboard, drag and
%%% drop and collapsing rows; after a local change it moves the existing
%%% elements and recomputes the lines, but never builds HTML.
%%%
%%% == Edits ==
%%%
%%% Dragging (with the `editable' modifier) moves a bar in place and fires
%%% `ah:task-change' on the root after writing its details (task, from,
%%% to, row, kind (move | resize), days) to data attributes, so a postback
%%% gets them in `Event.data'. The action stores the change and may answer
%%% with gantt_update/3, rendering the chart again from the stored data;
%%% the morph keeps the scroll position, and a refused change is undone
%%% the same way.
%%%
%%% gantt/3 builds an #ah_gantt{} (include/aihtml_gantt.hrl) and render/1
%%% turns it into HTML, so pages may also write the record directly
%%% (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_gantt).
-behaviour(aihtml_element).

-include("aihtml_gantt.hrl").

-export([gantt/3, gantt_update/3, render/1, fields/1, catalog/0, facade_extras/0]).

-export_type([element/0, task/0, row/0, label_key/0, labels/0]).

-define(H, aihtml_html).
-define(E, aihtml_element).
-define(D, aihtml_lib_date).

%% The record field types promise well-formed data, but builders pass
%% whatever the page gives: these clauses turn it into {aihtml, _} errors.
-dialyzer({no_match, [render/1, gantt_task/1, gantt_row/1, labels/3]}).

-define(DAY, 1440).
-define(PRIMARY, <<"var(--ah-color-primary)">>).
-define(MONTHS_SHORT, [<<"Jan">>, <<"Feb">>, <<"Mar">>, <<"Apr">>, <<"May">>, <<"Jun">>,
                       <<"Jul">>, <<"Aug">>, <<"Sep">>, <<"Oct">>, <<"Nov">>, <<"Dec">>]).

%% A gantt task. `end' is exclusive (a task from 2026-01-05 to 2026-01-12
%% lasts 7 days); `row' names its row when `rows' is given;
%% `dependencies' are ids of tasks that must finish first.
-type task() :: #{id := term(), name => unicode:chardata(),
                  start := aihtml_lib_date:time(), 'end' := aihtml_lib_date:time(),
                  row => term(), progress => number(),
                  color => unicode:chardata(),
                  dependencies => [term()] | term()}.
%% A gantt row (sidebar line); rows with a `parent' nest under it.
-type row() :: #{id := term(), label => unicode:chardata(), parent => term()}.
-type label_key() :: task | tasks | months_short.
%% Texts of the gantt: `task' (sidebar title), `tasks' (summary bar
%% label, "{n}" is the count), `months_short' (12).
-type labels() :: #{label_key() => unicode:chardata() | [unicode:chardata()]}.
-type element() :: #ah_gantt{}.

%% @doc A gantt chart (sigil's gantt). `Tasks' is a list of task maps
%% (see `task()': id, name, start, end (exclusive), row,
%% progress 0..100, color, dependencies).
%%
%% Css: `editable' (drag bars to move them, their ends to resize them, or
%% use the arrow keys), `no_dependencies' (no dependency lines).
%% Options (in Attrs): `rows' (sidebar rows `#{id, label, parent}'; by
%% default one row per task, labelled with its name), `collapsed' (ids of
%% parent rows shown collapsed), `height' (px, default 500),
%% `sidebar_width' (250), `column_width' (px per day, 60), `row_height'
%% (40), `today' (the date of the today marker, default the server's
%% date), `labels' (task, tasks, months_short).
-spec gantt([task()], aihtml_html:css(), aihtml_html:attrs()) -> #ah_gantt{}.
gantt(Tasks, Css, Attrs) ->
    ?E:build(?MODULE, #ah_gantt{items = Tasks}, Css, Attrs).

%% @doc The field names of #ah_gantt{}.
-spec fields(atom()) -> [atom()].
fields(ah_gantt) -> record_info(fields, ah_gantt).

-spec render(element()) -> aihtml_html:html().
render(#ah_gantt{items = Items, editable = Editable} = R0) ->
    {Id, R} = ensure_id(R0),
    Classes = ?E:classes(?MODULE, R),
    L = labels(#{task => <<"Task">>, tasks => <<"{n} tasks">>, months_short => ?MONTHS_SHORT},
                R#ah_gantt.labels, gantt),
    CW = pos_int(column_width, R#ah_gantt.column_width),
    RH = pos_int(row_height, R#ah_gantt.row_height),
    SW = pos_int(sidebar_width, R#ah_gantt.sidebar_width),
    is_list(Items) orelse error({aihtml, {bad_gantt_tasks, Items}}),
    Tasks0 = [gantt_task(T) || T <- Items],
    {Rows, Tasks} =
        case R#ah_gantt.rows of
            undefined ->
                {[#{id => I, label => N, parent => undefined}
                  || #{id := I, name := N} <- Tasks0],
                 [T#{row => I} || #{id := I} = T <- Tasks0]};
            Rs when is_list(Rs) ->
                {[gantt_row(X) || X <- Rs], Tasks0};
            Other -> error({aihtml, {bad_option, rows, Other}})
        end,
    {TodayDay, TodayMin} = ?D:today(R#ah_gantt.today),
    {From, To} = gantt_range(Tasks, TodayDay),
    Collapsed = [text(C) || C <- R#ah_gantt.collapsed],
    Tree = tree(Rows),
    Vis = [Row#{visible => V} || {Row, V} <- visibility(Tree, Collapsed)],
    VisRows = [Row || #{visible := true} = Row <- Vis],
    Index = maps:from_list(lists:zip([I || #{id := I} <- VisRows],
                                     lists:seq(0, length(VisRows) - 1))),
    Known = maps:from_list([{I, true} || #{id := I} <- Rows]),
    Shown = [T || #{row := Rw} = T <- Tasks, maps:is_key(Rw, Known)],
    Origin = From * ?DAY,
    Pos = fun(#{s := S, e := E}) ->
                  {(S - Origin) / ?DAY * CW, max((E - S) / ?DAY * CW, CW)}
          end,
    Days = lists:seq(From, To),
    TotalW = length(Days) * CW,
    TotalH = length(VisRows) * RH,
    Bar = fun(#{id := TId, row := Rw, color := C, progress := P, name := N} = T) ->
                  {Left, W} = Pos(T),
                  Top = maps:get(Rw, Index, 0) * RH + 4,
                  ?H:el('div',
                        [[?H:el('div', [], [<<"ah-gantt-task-progress">>],
                                [{style, [<<"width:">>, num(P), <<"%;">>]}]) || P > 0],
                         ?H:el('div', N, [<<"ah-gantt-task-label">>], []),
                         [[?H:el('div', [], [<<"ah-gantt-resize-handle ah-gantt-resize-left">>], []),
                           ?H:el('div', [], [<<"ah-gantt-resize-handle ah-gantt-resize-right">>], [])]
                          || Editable]],
                        [<<"ah-gantt-task-bar">>],
                        [{data_taskid, TId}, {data_rowid, Rw},
                         {data_start, maps:get(start, T)}, {data_end, maps:get('end', T)},
                         {data_deps, case maps:get(deps, T) of
                                         [] -> undefined;
                                         Ds -> lists:join(<<" ">>, Ds)
                                     end},
                         {hidden, not maps:is_key(Rw, Index)},
                         {tabindex, 0}, {role, button},
                         {aria_label, [N, <<": ">>, maps:get(start, T), <<" – "/utf8>>,
                                       maps:get('end', T),
                                       [[<<", ">>, num(P), <<"%">>] || P > 0]]},
                         {style, [<<"position:absolute;left:">>, num(Left), <<"px;top:">>,
                                  num(Top), <<"px;width:">>, num(W), <<"px;height:">>,
                                  num(RH - 8), <<"px;background:">>, C, <<";">>]}])
          end,
    Summary =
        [begin
             Desc = descendants(RowId, Tree),
             case [T || #{row := Rw} = T <- Shown, lists:member(Rw, Desc)] of
                 [] -> [];
                 Sub ->
                     S = lists:min([S0 || #{s := S0} <- Sub]),
                     E = lists:max([E0 || #{e := E0} <- Sub]),
                     {Left, W} = Pos(#{s => S, e => E}),
                     Show = maps:is_key(RowId, Index) andalso lists:member(RowId, Collapsed),
                     ?H:el('div',
                           [?H:el('div', [], [<<"ah-gantt-summary-cap ah-gantt-summary-cap-left">>], []),
                            ?H:el('div', replace(maps:get(tasks, L), <<"{n}">>,
                                                 integer_to_binary(length(Sub))),
                                  [<<"ah-gantt-summary-label">>], []),
                            ?H:el('div', [], [<<"ah-gantt-summary-cap ah-gantt-summary-cap-right">>], [])],
                           [<<"ah-gantt-summary-bar">>],
                           [{data_rowid, RowId}, {data_count, length(Sub)}, {hidden, not Show},
                            {style, [<<"position:absolute;left:">>, num(Left), <<"px;top:">>,
                                     num(maps:get(RowId, Index, 0) * RH + 4), <<"px;width:">>,
                                     num(W), <<"px;height:">>, num(RH - 8), <<"px;">>]}])
             end
         end || #{id := RowId, has_children := true} <- Tree],
    Grid = [?H:el('div', [], [<<"ah-gantt-grid-row">>],
                  [{data_rowid, I}, {hidden, not V}, {style, [<<"height:">>, num(RH), <<"px;">>]}])
            || #{id := I, visible := V} <- Vis],
    TaskMap = maps:from_list([{I, T} || #{id := I} = T <- Shown]),
    Deps = case R#ah_gantt.no_dependencies of
               true -> [];
               false ->
                   Paths = [dep_path(maps:get(D, TaskMap), T, Pos, Index, RH)
                            || #{deps := Ds} = T <- Shown, D <- Ds, maps:is_key(D, TaskMap)],
                   ?H:el(svg, Paths, [<<"ah-gantt-deps-layer">>],
                         [{<<"viewBox">>, [<<"0 0 ">>, num(TotalW), <<" ">>, num(TotalH)]},
                          {aria_hidden, <<"true">>},
                          {style, [<<"position:absolute;top:0;left:0;pointer-events:none;width:">>,
                                   num(TotalW), <<"px;height:">>, num(TotalH), <<"px;">>]}])
           end,
    MarkerLeft = (TodayMin - Origin) / ?DAY * CW,
    Marker = ?H:el('div', [], [<<"ah-gantt-today-marker">>],
                   [{style, case MarkerLeft >= 0 andalso MarkerLeft =< TotalW of
                                true -> [<<"display:block;left:">>, num(MarkerLeft), <<"px;">>];
                                false -> <<"display:none;">>
                            end}]),
    Sidebar = [gantt_sidebar_row(Row, RH, N0 =:= 1, Collapsed)
               || {N0, Row} <- lists:zip(lists:seq(1, length(Vis)), Vis)],
    ?H:el('div',
          [?H:el('div',
                 [?H:el('div', maps:get(task, L), [<<"ah-gantt-sidebar-header">>], []),
                  ?H:el('div', Sidebar, [<<"ah-gantt-sidebar-body">>],
                        [{role, tree}, {aria_label, maps:get(task, L)}])],
                 [<<"ah-gantt-sidebar">>], [{style, [<<"width:">>, num(SW), <<"px;">>]}]),
           ?H:el('div',
                 [?H:el('div',
                        [?H:el('div', gantt_months(Days, CW, L), [<<"ah-gantt-header-months">>], []),
                         ?H:el('div', [gantt_day(D, CW, TodayDay) || D <- Days],
                               [<<"ah-gantt-header-days">>], [])],
                        [<<"ah-gantt-timeline-header">>], [{aria_hidden, <<"true">>}]),
                  ?H:el('div',
                        [?H:el('div', [Grid, Summary, [Bar(T) || T <- Shown]],
                               [<<"ah-gantt-tasks-layer">>],
                               [{style, [<<"width:">>, num(TotalW), <<"px;height:">>,
                                         num(TotalH), <<"px;">>]}]),
                         Deps, Marker],
                        [<<"ah-gantt-timeline-body">>], [])],
                 [<<"ah-gantt-main">>], [])],
          Classes,
          [[{id, Id}, {data_ah, <<"gantt">>},
            {style, height_style(R#ah_gantt.height)},
            {data_origin, ?D:iso_date(From)},
            {data_column_width, CW}, {data_row_height, RH},
            {data_editable, Editable},
            {data_collapsed, aihtml_value:join(Collapsed)}],
           ?E:root_attrs(R, 'ah:task-change')]).

gantt_task(#{id := Id0, start := S0, 'end' := E0} = T) ->
    {S, SD} = ?D:parse_time(S0),
    {E1, ED} = ?D:parse_time(E0),
    E = max(S, E1),
    Id = text(Id0),
    Progress = case maps:get(progress, T, 0) of
                   P when is_number(P), P >= 0, P =< 100 -> P;
                   P -> error({aihtml, {bad_gantt_progress, P}})
               end,
    Deps = case maps:get(dependencies, T, []) of
               L when is_list(L), L =/= [], is_integer(hd(L)) -> [text(L)];
               L when is_list(L) -> [text(D) || D <- L];
               D -> [text(D)]
           end,
    #{id => Id, name => text(maps:get(name, T, Id)), s => S, e => E,
      start => ?D:iso_time(S, SD), 'end' => ?D:iso_time(E, ED),
      row => text(maps:get(row, T, undefined)),
      progress => Progress,
      color => color(maps:get(color, T, ?PRIMARY)),
      deps => Deps};
gantt_task(Other) ->
    error({aihtml, {bad_gantt_task, Other}}).

gantt_row(#{id := Id} = Row) ->
    #{id => text(Id), label => text(maps:get(label, Row, Id)),
      parent => case maps:get(parent, Row, undefined) of
                    undefined -> undefined;
                    P -> text(P)
                end};
gantt_row(Other) ->
    error({aihtml, {bad_gantt_row, Other}}).

%% Days shown: from a week before the first task's month to a week after
%% the last task's month (sigil's compute-timeline); the current month
%% without tasks.
gantt_range([], Today) ->
    {?D:first_of_month(Today), ?D:last_of_month(Today)};
gantt_range(Tasks, _) ->
    Min = lists:min([S || #{s := S} <- Tasks]) div ?DAY,
    Max = lists:max([E || #{e := E} <- Tasks]) div ?DAY,
    {?D:first_of_month(Min) - 7, ?D:last_of_month(Max) + 7}.

%% Rows in tree order (parents before children) with their level. A row
%% whose parent is unknown is a root.
tree(Rows) ->
    Ids = [I || #{id := I} <- Rows],
    Parent = fun(#{parent := P}) ->
                     case lists:member(P, Ids) of true -> P; false -> undefined end
             end,
    Kids = fun(P) -> [R || R <- Rows, Parent(R) =:= P] end,
    Walk = fun Walk(P, Level) ->
                   lists:append([[R#{level => Level, parent => P,
                                     has_children => Kids(I) =/= []}
                                  | Walk(I, Level + 1)]
                                 || #{id := I} = R <- Kids(P)])
           end,
    Walk(undefined, 0).

visibility(Tree, Collapsed) ->
    {Out, _} = lists:mapfoldl(
                 fun(#{id := I, parent := P} = Row, Open) ->
                         V = P =:= undefined orelse maps:get(P, Open, false),
                         {{Row, V}, Open#{I => V andalso not lists:member(I, Collapsed)}}
                 end, #{}, Tree),
    Out.

descendants(Id, Tree) ->
    Kids = [K || #{id := K, parent := P} <- Tree, P =:= Id],
    Kids ++ lists:append([descendants(K, Tree) || K <- Kids]).

gantt_sidebar_row(#{id := Id, label := Label, level := Level, parent := P,
                    has_children := Kids, visible := V}, RH, First, Collapsed) ->
    Open = not lists:member(Id, Collapsed),
    ?H:el('div',
          [[?H:el(span, <<"▶"/utf8>>,
                  [<<"ah-gantt-expand-icon">>, [<<" ah-gantt-expand-icon-expanded">> || Open]],
                  [{aria_hidden, <<"true">>}]) || Kids],
           ?H:el(span, Label, [<<"ah-gantt-sidebar-label">>], [])],
          [<<"ah-gantt-sidebar-row">>],
          [{data_rowid, Id}, {data_parent, P}, {data_level, Level},
           {role, treeitem}, {aria_level, Level + 1},
           {aria_expanded, case Kids of true -> atom_to_binary(Open); false -> undefined end},
           {tabindex, case First of true -> 0; false -> -1 end},
           {hidden, not V},
           {style, [<<"height:">>, num(RH), <<"px;padding-left:">>, num(16 + Level * 20),
                    <<"px;">>]}]).

gantt_months(Days, CW, L) ->
    Spans = lists:foldr(fun(D, Acc) ->
                                YM = ym(D),
                                case Acc of
                                    [{YM, N} | Rest] -> [{YM, N + 1} | Rest];
                                    _ -> [{YM, 1} | Acc]
                                end
                        end, [], Days),
    [?H:el('div', [lists:nth(M, maps:get(months_short, L)), <<" ">>, integer_to_binary(Y)],
           [<<"ah-gantt-month-cell">>], [{style, [<<"width:">>, num(N * CW), <<"px;">>]}])
     || {{Y, M}, N} <- Spans].

ym(D) -> {Y, M, _} = calendar:gregorian_days_to_date(D), {Y, M}.

gantt_day(D, CW, Today) ->
    {_, _, Dd} = calendar:gregorian_days_to_date(D),
    Wd = ?D:dow(D),
    ?H:el('div', integer_to_binary(Dd),
          [<<"ah-gantt-day-cell">>, [<<" ah-gantt-day-today">> || D =:= Today],
           [<<" ah-gantt-day-weekend">> || Wd =:= 0 orelse Wd =:= 6]],
          [{style, [<<"width:">>, num(CW), <<"px;">>]}]).

%% sigil's render-dependency-line: finish-to-start, a straight line in one
%% row, an S curve across rows, a loop out and back when the successor
%% starts before the predecessor ends. Empty when a row is hidden.
dep_path(From, To, Pos, Index, RH) ->
    D = case {maps:find(maps:get(row, From), Index), maps:find(maps:get(row, To), Index)} of
            {{ok, FI}, {ok, TI}} ->
                {FL, FW} = Pos(From),
                {TL, _} = Pos(To),
                dep_d(FL + FW, FI * RH + RH / 2, TL, TI * RH + RH / 2, FI =:= TI);
            _ -> <<>>
        end,
    ?H:el(path, [], [<<"ah-gantt-dep-line">>],
          [{d, D}, {fill, none}, {stroke, <<"var(--ah-color-grey-400)">>},
           {stroke_width, <<"1.5">>},
           {data_from, maps:get(id, From)}, {data_to, maps:get(id, To)}]).

dep_d(X1, Y1, X2, Y2, true) ->
    [<<"M">>, num(X1), <<",">>, num(Y1), <<" L">>, num(X2), <<",">>, num(Y2)];
dep_d(X1, Y1, X2, Y2, false) when X1 >= X2 ->
    Ext = 30 + (X1 - X2) / 3,
    [<<"M">>, num(X1), <<",">>, num(Y1), <<" C">>, num(X1 + Ext), <<",">>, num(Y1),
     <<" ">>, num(X2 - Ext), <<",">>, num(Y2), <<" ">>, num(X2), <<",">>, num(Y2)];
dep_d(X1, Y1, X2, Y2, false) ->
    Mx = (X1 + X2) / 2,
    [<<"M">>, num(X1), <<",">>, num(Y1), <<" C">>, num(Mx), <<",">>, num(Y1),
     <<" ">>, num(Mx), <<",">>, num(Y2), <<" ">>, num(X2), <<",">>, num(Y2)].

%% @doc Answer an edit of a gantt: render `Gantt' (built from the stored
%% tasks) in place of the one that fired `Event', keeping its id and the
%% rows the user collapsed. Sends one morph operation.
-spec gantt_update(aihtml_action:ctx(), aihtml_action:event(), #ah_gantt{}) -> ok.
gantt_update(Ctx, #{id := Id, data := Data}, #ah_gantt{} = G) ->
    Collapsed = case maps:get(<<"collapsed">>, Data, undefined) of
                    undefined -> G#ah_gantt.collapsed;
                    C -> aihtml_value:split(C)
                end,
    aihtml_action:html(Ctx, {id, Id}, G#ah_gantt{id = Id, collapsed = Collapsed}, morph).

%% @doc Functions besides the component that the aihtml facade re-exports.
-spec facade_extras() -> [{atom(), arity()}].
facade_extras() -> [{gantt_update, 3}].

%%%===================================================================
%%% Internal
%%%===================================================================

%% A root without an id gets one: the behaviour and the update functions
%% refer to it. Returns the id and the record holding it, for root_attrs/2.
ensure_id(R) ->
    Id = case R#ah_gantt.id of
             undefined -> <<"ah-s", (integer_to_binary(erlang:unique_integer([positive])))/binary>>;
             Id0 -> text(Id0)
         end,
    {Id, R#ah_gantt{id = Id}}.

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

replace(B, Pat, With) -> iolist_to_binary(string:replace(B, Pat, With, all)).

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

text(undefined) -> undefined;
text(B) when is_binary(B) -> B;
text(A) when is_atom(A) -> atom_to_binary(A);
text(I) when is_integer(I) -> integer_to_binary(I);
text(L) when is_list(L) -> unicode:characters_to_binary(L);
text(X) -> beamai_html_escape:to_binary(X, aihtml).

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => gantt, category => data,
       signature => <<"gantt(Tasks, Css, Attrs)">>,
       root => <<"ah-gantt">>,
       flags => [editable, no_dependencies],
       classes => #{editable => [], no_dependencies => []},
       options => [rows, collapsed, height, sidebar_width, column_width, row_height,
                   today, labels],
       behavior => <<"gantt">>,
       events => [<<"ah:task-change">>, <<"ah:task-click">>, <<"ah:row-click">>,
                  <<"ah:row-expand">>],
       doc => <<"Task bars on a day scale with a row tree, progress, dependency lines and a "
                "today marker; bars are dragged to move or resize them, firing ah:task-change.">>,
       option_docs =>
           #{editable => <<"Drag bars to move them and their ends to resize them; with a bar "
                           "focused, arrow keys move it by a day and Shift+arrows change its end.">>,
             no_dependencies => <<"Do not draw dependency lines.">>,
             rows => <<"Sidebar rows #{id, label, parent}; tasks go to the row named by their "
                       "row key. Default: one row per task.">>,
             collapsed => <<"Ids of parent rows shown collapsed (a summary bar spans their tasks).">>,
             height => <<"Height in px (default 500).">>,
             sidebar_width => <<"Sidebar width in px (default 250).">>,
             column_width => <<"Width of a day in px (default 60).">>,
             row_height => <<"Row height in px (default 40).">>,
             today => <<"Date of the today marker (default the server's date and time).">>,
             labels => <<"Map of task (sidebar title), tasks (summary label, {n}) and "
                         "months_short (12).">>},
       methods =>
           [#{name => expandRow, args => <<"(RowId)">>, doc => <<"Expand a parent row.">>},
            #{name => collapseRow, args => <<"(RowId)">>, doc => <<"Collapse a parent row.">>},
            #{name => scrollToDate, args => <<"(Iso)">>,
              doc => <<"Scroll the timeline to a date.">>},
            #{name => setTask, args => <<"(TaskId, From, To[, RowId])">>,
              doc => <<"Move a bar without firing an event (e.g. to undo a refused change).">>}]}].
