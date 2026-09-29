%%%-------------------------------------------------------------------
%%% @doc Schedule components, ported from sigil (data/gantt,
%%% data/scheduler, data/swimlane). See designs/04-components.md.
%%%
%%%   gantt(Tasks, Css, Attrs)             task bars on a day scale
%%%   scheduler(Events, Value, Css, Attrs) resource scheduler (day, week,
%%%                                        month, agenda, timeline views)
%%%   swimlane(Nodes, Css, Attrs)          lanes x phases flow chart
%%%   scheduler_update(Ctx, Event, S)      (in an action) render another range
%%%   scheduler_range(Event)               the view, date and range an event asks for
%%%   gantt_update(Ctx, Event, G), swimlane_update(Ctx, Event, S)
%%%                                        (in an action) re-render after an edit
%%%
%%% Everything is rendered here, on the server: the bars, the time grid,
%%% the month segments, the dependency and flow lines (SVG paths). The
%%% behaviours (assets/js/components/data_schedule*.js) handle scrolling,
%%% keyboard, drag and drop, collapsing gantt rows and selecting swimlane
%%% nodes; after a local change they move the existing elements and
%%% recompute the lines, but never build HTML.
%%%
%%% == Edits ==
%%%
%%% Dragging (with the `editable' modifier) moves the element in place and
%%% fires a component event on the root after writing its details to data
%%% attributes, so a postback gets them in `Event.data':
%%%
%%%   gantt      ah:task-change   task, from, to, row, kind (move | resize), days
%%%   scheduler  ah:event-change  event, source, from, to, resource, kind
%%%   swimlane   ah:node-change   node, lane, phase, oldLane, oldPhase
%%%
%%% The action stores the change and may answer with gantt_update/3 (etc.),
%%% rendering the component again from the stored data; the morph keeps
%%% the scroll position, and a refused change is undone the same way.
%%%
%%% == Scheduler navigation (remote) ==
%%%
%%% The scheduler's value is the date it shows. The toolbar (prev, today,
%%% next, the view buttons) updates `data-ah-value', `data-view',
%%% `data-start' and `data-end' (the visible range, end exclusive) and fires
%%% `change'; the `source' option binds an action to it, which loads that
%%% range from the database and answers with
%%% `scheduler_update(Ctx, Event, scheduler(Events, undefined, Css, Attrs))':
%%% the date and view come from the event, the new view is rendered here
%%% and morphed into the page. The browser never renders a range itself,
%%% so the page holds only what is visible. Without `source' (or a change
%%% postback) the toolbar shows only the title.
%%%
%%% Each component function builds an element record (#ah_gantt{},
%%% #ah_scheduler{}, #ah_swimlane{}, defined in
%%% include/aihtml_data_schedule.hrl) and render/1 turns it into HTML, so
%%% pages may also write the records directly (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_data_schedule).
-behaviour(aihtml_element).

-include("aihtml_data_schedule.hrl").

-export([gantt/3, scheduler/4, swimlane/3,
         scheduler_update/3, scheduler_range/1, gantt_update/3, swimlane_update/3,
         render/1, fields/1, catalog/0, facade_extras/0]).

-export_type([element/0, gantt_task/0, scheduler_event/0, swimlane_node/0]).

-define(H, aihtml_html).
-define(E, aihtml_element).

%% The record field types promise well-formed data, but builders pass
%% whatever the page gives: these clauses turn it into {aihtml, _} errors.
-dialyzer({no_match, [render_gantt/1, gantt_task/1, gantt_row/1, resource/1,
                      render_swimlane/1, swim_lane/1, swim_phase/1, swim_node/1,
                      swim_flow/1, labels/3]}).

-define(DAY, 1440).
-define(MAX_ITERS, 5000).
-define(VIEWS, [day, week, month, agenda, timeline_day, timeline_week, timeline_month]).
-define(PRIMARY, <<"var(--ah-color-primary)">>).
-define(MONTHS, [<<"January">>, <<"February">>, <<"March">>, <<"April">>, <<"May">>,
                 <<"June">>, <<"July">>, <<"August">>, <<"September">>, <<"October">>,
                 <<"November">>, <<"December">>]).
-define(MONTHS_SHORT, [<<"Jan">>, <<"Feb">>, <<"Mar">>, <<"Apr">>, <<"May">>, <<"Jun">>,
                       <<"Jul">>, <<"Aug">>, <<"Sep">>, <<"Oct">>, <<"Nov">>, <<"Dec">>]).
-define(WEEKDAYS, [<<"Sunday">>, <<"Monday">>, <<"Tuesday">>, <<"Wednesday">>,
                   <<"Thursday">>, <<"Friday">>, <<"Saturday">>]).
-define(WEEKDAYS_SHORT, [<<"Sun">>, <<"Mon">>, <<"Tue">>, <<"Wed">>, <<"Thu">>,
                         <<"Fri">>, <<"Sat">>]).

-type element() :: #ah_gantt{} | #ah_scheduler{} | #ah_swimlane{}.
-type gantt_task() :: ah_gantt_task().
-type scheduler_event() :: ah_sch_event().
-type swimlane_node() :: ah_swim_node().

%%%===================================================================
%%% gantt
%%%===================================================================

%% @doc A gantt chart (sigil's gantt). `Tasks' is a list of task maps
%% (see `ah_gantt_task()': id, name, start, end (exclusive), row,
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
-spec gantt([gantt_task()], aihtml_html:css(), aihtml_html:attrs()) -> #ah_gantt{}.
gantt(Tasks, Css, Attrs) ->
    build(#ah_gantt{items = Tasks}, Css, Attrs).

render_gantt(#ah_gantt{items = Items, editable = Editable} = R0) ->
    {Id, R} = ensure_id(R0),
    Classes = classes(R),
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
    {TodayDay, TodayMin} = today(R#ah_gantt.today),
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
            {data_origin, iso_date(From)},
            {data_column_width, CW}, {data_row_height, RH},
            {data_editable, Editable},
            {data_collapsed, lists:join(<<",">>, Collapsed)}],
           ?E:root_attrs(R, 'ah:task-change')]).

gantt_task(#{id := Id0, start := S0, 'end' := E0} = T) ->
    {S, SD} = parse_time(S0),
    {E1, ED} = parse_time(E0),
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
      start => iso_time(S, SD), 'end' => iso_time(E, ED),
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
    {first_of_month(Today), last_of_month(Today)};
gantt_range(Tasks, _) ->
    Min = lists:min([S || #{s := S} <- Tasks]) div ?DAY,
    Max = lists:max([E || #{e := E} <- Tasks]) div ?DAY,
    {first_of_month(Min) - 7, last_of_month(Max) + 7}.

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
    Wd = dow(D),
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
                    <<>> -> [];
                    C -> binary:split(C, <<",">>, [global])
                end,
    aihtml_action:html(Ctx, {id, Id}, G#ah_gantt{id = Id, collapsed = Collapsed}, morph).

%%%===================================================================
%%% scheduler
%%%===================================================================

%% @doc A resource scheduler (sigil's scheduler). `Events' is a list of
%% appointment maps (see `ah_sch_event()'); `Value' is the date shown (an
%% ISO date or `calendar:date()'; `undefined' is today).
%%
%% Css: `editable' (drag appointments to other times, days and resources,
%% resize them, drag over free time to select a range, context menu),
%% `no_all_day' (no all day row in the day and week views).
%% Options (in Attrs): `view' (day, week (default), month, agenda,
%% timeline_day, timeline_week, timeline_month), `views' (the view
%% buttons, default [day, week, month, agenda]), `resources' (a list of
%% `#{id, name, color}'), `first_day' (0 = Sunday .. 6, default 1),
%% `slot_duration' (minutes, a divisor of 60, default 30), `slot_height'
%% (px, 20), `day_start', `day_end' (hours shown, 0 and 24), `height' (px,
%% 600), `agenda_days' (30), `day_max_events' (month rows before "+n
%% more", 3), `hour_format' (12 or 24), `today' (the date highlighted,
%% default the server's date), `toolbar' (default true), `source' (the
%% action that loads another range, see the module doc), `labels',
%% `name' (a hidden input with the shown date).
-spec scheduler([scheduler_event()], ah_sch_date(), aihtml_html:css(), aihtml_html:attrs()) ->
          #ah_scheduler{}.
scheduler(Events, Value, Css, Attrs) ->
    build(#ah_scheduler{items = Events, value = Value}, Css, Attrs).

sch_label_defaults() ->
    #{today => <<"Today">>, prev => <<"Previous">>, next => <<"Next">>,
      day => <<"Day">>, week => <<"Week">>, month => <<"Month">>, agenda => <<"Agenda">>,
      timeline_day => <<"Timeline Day">>, timeline_week => <<"Timeline Week">>,
      timeline_month => <<"Timeline Month">>,
      all_day => <<"All day">>, all_day_short => <<"all-day">>, more => <<"+{n} more">>,
      no_events => <<"No appointments">>,
      hint_navigate => <<"Try navigating to a different date range">>,
      edit => <<"Edit">>, delete => <<"Delete">>, copy => <<"Copy">>,
      new => <<"New appointment">>, am => <<"AM">>, pm => <<"PM">>,
      months => ?MONTHS, months_short => ?MONTHS_SHORT,
      weekdays => ?WEEKDAYS, weekdays_short => ?WEEKDAYS_SHORT,
      title_day => <<"yyyy-MM-dd EEEE">>, title_month => <<"MMMM yyyy">>,
      range_start => <<"MMM d">>, range_end => <<"MMM d, yyyy">>,
      agenda_date => <<"MMMM d, yyyy">>, popover_date => <<"yyyy-MM-dd EEE">>}.

render_scheduler(#ah_scheduler{view = View, views = Views, editable = Editable} = R0) ->
    {Id, R} = ensure_id(R0),
    Classes = classes(R),
    lists:member(View, ?VIEWS) orelse error({aihtml, {bad_option, view, View}}),
    (is_list(Views) andalso Views =/= [] andalso lists:all(fun(V) -> lists:member(V, ?VIEWS) end,
                                                           Views))
        orelse error({aihtml, {bad_option, views, Views}}),
    First = in_range(first_day, R#ah_scheduler.first_day, 0, 6),
    SD = pos_int(slot_duration, R#ah_scheduler.slot_duration),
    60 rem SD =:= 0 orelse error({aihtml, {bad_option, slot_duration, SD}}),
    SH = pos_int(slot_height, R#ah_scheduler.slot_height),
    DS = in_range(day_start, R#ah_scheduler.day_start, 0, 23),
    DE = in_range(day_end, R#ah_scheduler.day_end, DS + 1, 24),
    Agenda = pos_int(agenda_days, R#ah_scheduler.agenda_days),
    Max = pos_int(day_max_events, R#ah_scheduler.day_max_events),
    HF = case R#ah_scheduler.hour_format of
             F when F =:= 12; F =:= 24 -> F;
             F -> error({aihtml, {bad_option, hour_format, F}})
         end,
    is_boolean(R#ah_scheduler.toolbar)
        orelse error({aihtml, {bad_option, toolbar, R#ah_scheduler.toolbar}}),
    L = labels(sch_label_defaults(), R#ah_scheduler.labels, scheduler),
    {Today, _} = today(R#ah_scheduler.today),
    Cur = case R#ah_scheduler.value of
              undefined -> Today;
              V -> days_of(V)
          end,
    {RS, RE} = profile(View, Cur, First, Agenda),
    Title = sch_title(View, Cur, RS, RE, L),
    Resources = [resource(X) || X <- R#ah_scheduler.resources],
    Items = R#ah_scheduler.items,
    is_list(Items) orelse error({aihtml, {bad_scheduler_events, Items}}),
    Events = [sch_event(E, N) || {N, E} <- lists:zip(lists:seq(1, length(Items)), Items)],
    Insts = instances(Events, RS * ?DAY, RE * ?DAY),
    Cfg = #{id => Id, today => Today, cur => Cur, rs => RS, re => RE, sd => SD, sh => SH,
            ds => DS, de => DE, hf => HF, labels => L, resources => Resources,
            editable => Editable, max => Max, first => First, view => View,
            all_day_row => not R#ah_scheduler.no_all_day},
    Body = case View of
               day -> timegrid(Insts, Cfg);
               week -> timegrid(Insts, Cfg);
               month -> month_view(Insts, Cfg);
               agenda -> agenda_view(Insts, Cfg);
               _ -> timeline_view(Insts, Cfg)
           end,
    #{postback := Postback} = ?E:base(R),
    Source = R#ah_scheduler.source,
    Navigable = Source =/= undefined orelse Postback =/= undefined,
    Iso = iso_date(Cur),
    ?H:el('div',
          [[sch_toolbar(Title, Navigable, Views, View, L) || R#ah_scheduler.toolbar],
           hidden(R#ah_scheduler.name, Iso),
           ?H:el('div', Body, [<<"ah-scheduler-view-container">>], []),
           [sch_menus(L) || Editable],
           ?H:el('div', [], [<<"ah-scheduler-live">>],
                 [{aria_live, polite}, {style, <<"position:absolute;width:1px;height:1px;"
                                                 "overflow:hidden;clip:rect(0 0 0 0);">>}])],
          Classes,
          [[{id, Id}, {data_ah, <<"scheduler">>},
            {style, height_style(R#ah_scheduler.height)},
            {data_ah_value, Iso}, {data_view, View},
            {data_start, iso_date(RS)}, {data_end, iso_date(RE)},
            {data_first_day, First}, {data_agenda_days, Agenda},
            {data_slot_duration, SD}, {data_slot_height, SH},
            {data_day_start, DS}, {data_day_end, DE},
            {data_hour_format, HF}, {data_am, maps:get(am, L)}, {data_pm, maps:get(pm, L)},
            {data_editable, Editable},
            {role, region}, {aria_label, Title}],
           case Source of
               undefined -> [];
               _ -> aihtml:on(change, Source, #{sync => queue})
           end,
           ?E:root_attrs(R, change)]).

%% The visible days [Start, End) of a view (sigil's compute-date-profile).
profile(View, D, _, _) when View =:= day; View =:= timeline_day -> {D, D + 1};
profile(View, D, First, _) when View =:= week; View =:= timeline_week ->
    S = sow(D, First), {S, S + 7};
profile(month, D, First, _) ->
    {sow(first_of_month(D), First), sow(last_of_month(D), First) + 7};
profile(timeline_month, D, _, _) -> {first_of_month(D), last_of_month(D) + 1};
profile(agenda, D, _, N) -> {D, D + N}.

sch_title(View, D, _, _, L) when View =:= day; View =:= timeline_day ->
    fmt(D * ?DAY, maps:get(title_day, L), L);
sch_title(View, D, _, _, L) when View =:= month; View =:= timeline_month ->
    fmt(D * ?DAY, maps:get(title_month, L), L);
sch_title(_, _, S, E, L) ->
    [fmt(S * ?DAY, maps:get(range_start, L), L), <<" – "/utf8>>,
     fmt((E - 1) * ?DAY, maps:get(range_end, L), L)].

sch_toolbar(Title, Navigable, Views, View, L) ->
    ?H:el('div',
          ?H:el('div',
                [?H:el('div',
                       [[?H:el(button, <<"‹"/utf8>>, [<<"ah-scheduler-btn ah-scheduler-btn-prev">>],
                               [{type, button}, {aria_label, maps:get(prev, L)}]),
                         ?H:el(button, maps:get(today, L), [<<"ah-scheduler-btn ah-scheduler-btn-today">>],
                               [{type, button}]),
                         ?H:el(button, <<"›"/utf8>>, [<<"ah-scheduler-btn ah-scheduler-btn-next">>],
                               [{type, button}, {aria_label, maps:get(next, L)}])]
                        || Navigable],
                       [<<"ah-scheduler-toolbar-left">>], []),
                 ?H:el('div', ?H:el(h2, Title, [<<"ah-scheduler-title">>], [{aria_live, polite}]),
                       [<<"ah-scheduler-toolbar-center">>], []),
                 ?H:el('div',
                       [[?H:el(button, maps:get(V, L),
                               [<<"ah-scheduler-view-btn">>,
                                [<<" ah-scheduler-view-btn-active">> || V =:= View]],
                               [{type, button}, {data_view, V},
                                {aria_pressed, atom_to_binary(V =:= View)}])
                         || V <- Views] || Navigable],
                       [<<"ah-scheduler-toolbar-right">>], [{role, group}])],
                [<<"ah-scheduler-toolbar-inner">>], []),
          [<<"ah-scheduler-toolbar">>], [{role, toolbar}]).

%% Context menus as inert templates, cloned by the behaviour.
sch_menus(L) ->
    Item = fun(Action, Key) ->
                   ?H:el('div', maps:get(Key, L), [<<"ah-scheduler-contextmenu-item">>],
                         [{data_action, Action}, {role, menuitem}, {tabindex, -1}])
           end,
    [?H:el(template, [Item(edit, edit), Item(delete, delete), Item(copy, copy)],
           [<<"ah-scheduler-menu-event">>], []),
     ?H:el(template, Item(create, new), [<<"ah-scheduler-menu-cell">>], [])].

resource(#{id := Id} = Res) ->
    #{id => text(Id), name => text(maps:get(name, Res, Id)),
      color => color(maps:get(color, Res, ?PRIMARY))};
resource(Other) -> error({aihtml, {bad_scheduler_resource, Other}}).

sch_event(#{start := S0} = E, N) ->
    {S, DateOnly} = parse_time(S0),
    Flag = maps:get(all_day, E, false) =:= true,
    End0 = case maps:get('end', E, undefined) of
               undefined when Flag; DateOnly -> S + ?DAY;
               undefined -> S + 60;
               E0 -> element(1, parse_time(E0))
           end,
    End = max(S, End0),
    AllDay = Flag orelse (S rem ?DAY =:= 0 andalso End rem ?DAY =:= 0 andalso S =/= End),
    #{id => case maps:get(id, E, undefined) of
                undefined -> <<"appt", (integer_to_binary(N))/binary>>;
                I -> text(I)
            end,
      title => text(maps:get(title, E, <<>>)),
      s => S, e => End, all_day => AllDay,
      resource => case maps:get(resource, E, undefined) of
                      undefined -> undefined;
                      Rs -> text(Rs)
                  end,
      status => status(maps:get(status, E, busy)),
      color => color(maps:get(color, E, ?PRIMARY)),
      rrule => case maps:get(rrule, E, undefined) of
                   undefined -> undefined;
                   Rule -> parse_rrule(text(Rule))
               end,
      exdates => [iso_date(days_of(D)) || D <- maps:get(exdates, E, [])]};
sch_event(Other, _) ->
    error({aihtml, {bad_scheduler_event, Other}}).

status(S) when S =:= free; S =:= busy; S =:= tentative; S =:= out_of_office -> S;
status(B) when is_binary(B); is_list(B) ->
    case string:replace(string:lowercase(text(B)), <<"-">>, <<"_">>, all) of
        X -> status_atom(iolist_to_binary(X), B)
    end;
status(Other) -> error({aihtml, {bad_scheduler_status, Other}}).

status_atom(<<"free">>, _) -> free;
status_atom(<<"busy">>, _) -> busy;
status_atom(<<"tentative">>, _) -> tentative;
status_atom(<<"out_of_office">>, _) -> out_of_office;
status_atom(_, B) -> error({aihtml, {bad_scheduler_status, B}}).

status_class(out_of_office) -> <<"ah-scheduler-status-out-of-office">>;
status_class(S) -> <<"ah-scheduler-status-", (atom_to_binary(S))/binary>>.

status_color(free) -> <<"var(--ah-color-success)">>;
status_color(busy) -> <<"var(--ah-color-error)">>;
status_color(tentative) -> <<"var(--ah-color-warning)">>;
status_color(out_of_office) -> <<"var(--ah-color-info, #6366f1)">>.

%% Instances overlapping [RS, RE) (minutes), recurring series expanded:
%% #{id, ev, s, e}; an occurrence's id is <series id>_<yyyyMMddTHHmmss>.
instances(Events, RS, RE) ->
    lists:append(
      [case Rule of
           undefined when S < RE, E > RS; S =:= E, S >= RS, S < RE -> [#{id => Id, ev => Ev, s => S, e => E}];
           undefined -> [];
           _ -> [#{id => <<Id/binary, "_", (stamp(Cs))/binary>>, ev => Ev, s => Cs, e => Ce}
                 || {Cs, Ce} <- expand(S, E, Rule, RS, RE, Ex)]
       end || #{id := Id, s := S, e := E, rrule := Rule, exdates := Ex} = Ev <- Events]).

%% The attributes every appointment element carries.
ev_attrs(#{id := IId, s := S, e := E, ev := #{id := Src, all_day := AD, resource := Res,
                                                title := T}}, Cfg) ->
    [{data_eventid, IId}, {data_source, Src}, {data_resourceid, Res},
     {data_start, iso_time(S, AD)}, {data_end, iso_time(E, AD)}, {data_all_day, AD},
     {tabindex, 0}, {role, button},
     {aria_label, [T, <<", ">>, time_range(S, E, AD, Cfg)]}].

time_range(_, _, true, #{labels := L}) -> maps:get(all_day, L);
time_range(S, E, false, Cfg) -> [clock(S, Cfg), <<" – "/utf8>>, clock(E, Cfg)].

clock(Min, #{hf := 24}) ->
    <<(pad(Min rem ?DAY div 60))/binary, ":", (pad(Min rem 60))/binary>>;
clock(Min, #{labels := L}) ->
    H = Min rem ?DAY div 60,
    H12 = case H rem 12 of 0 -> 12; X -> X end,
    <<(integer_to_binary(H12))/binary, ":", (pad(Min rem 60))/binary, " ",
      (maps:get(case H < 12 of true -> am; false -> pm end, L))/binary>>.

ev_style(#{ev := #{color := C, status := St}}) ->
    [<<"background:">>, C, <<";border-left:3px solid ">>, status_color(St), <<";">>].

ev_classes(Base, #{ev := #{status := St}}) ->
    [Base, <<" ">>, status_class(St)].

ev_title(#{ev := #{title := T, rrule := Rule}}, Tag, Class) ->
    ?H:el(Tag, [[?H:el(span, <<"↻ "/utf8>>, [<<"ah-scheduler-event-recurring-icon">>],
                       [{aria_hidden, <<"true">>}]) || Rule =/= undefined], T],
          [Class], []).

overlaps(#{s := S, e := E}, From, To) -> S < To andalso (E > From orelse (S =:= E andalso S >= From)).

res_matches(_, undefined) -> true;
res_matches(#{ev := #{resource := R}}, #{id := R}) -> true;
res_matches(_, _) -> false.

%%%-------------------------------------------------------------------
%%% Day and week views: a time grid, optionally a column per resource
%%%-------------------------------------------------------------------

timegrid(Insts, #{rs := RS, re := RE, resources := Res, ds := DS, de := DE,
                  sd := SD, sh := SH, today := Today, labels := L} = Cfg) ->
    Days = lists:seq(RS, RE - 1),
    HasRes = Res =/= [],
    Cols = case HasRes of true -> length(Days) * length(Res); false -> length(Days) end,
    MinW = [<<"min-width:">>, num(Cols * 80), <<"px;">>],
    PerDay = case HasRes of true -> Res; false -> [undefined] end,
    HourPx = SH * 60 div SD,
    Header = [case HasRes of
                  false -> day_header_cell(D, Today, L);
                  true ->
                      ?H:el('div',
                            [day_header_cell(D, Today, L),
                             ?H:el('div',
                                   [?H:el('div', N, [<<"ah-scheduler-dayview-resource-label">>],
                                          [{style, [<<"border-bottom:2px solid ">>, C, <<";">>]}])
                                    || #{name := N, color := C} <- Res],
                                   [<<"ah-scheduler-dayview-resource-header">>], [])],
                            [<<"ah-scheduler-dayview-header-day-group">>], [])
              end || D <- Days],
    AllDay = case maps:get(all_day_row, Cfg) of
                 false -> [];
                 true ->
                     ?H:el('div',
                           [?H:el('div', maps:get(all_day_short, L),
                                  [<<"ah-scheduler-dayview-gutter">>], []),
                            ?H:el('div',
                                  [?H:el('div',
                                         [allday_event(I, Cfg)
                                          || #{ev := #{all_day := true}} = I <- Insts,
                                             overlaps(I, D * ?DAY, (D + 1) * ?DAY),
                                             res_matches(I, Rr)],
                                         [<<"ah-scheduler-dayview-allday-cell">>],
                                         [{data_date, iso_date(D)}, {data_resourceid, res_id(Rr)}])
                                   || D <- Days, Rr <- PerDay],
                                  [<<"ah-scheduler-dayview-allday">>], [{style, MinW}])],
                           [<<"ah-scheduler-dayview-allday-row">>], [])
             end,
    Slots = [?H:el('div',
                   [?H:el('div', hour_label(H, Cfg), [<<"ah-scheduler-timegrid-slot-label">>], []),
                    ?H:el('div', [], [<<"ah-scheduler-timegrid-slot-line">>], [])],
                   [<<"ah-scheduler-timegrid-slot">>],
                   [{style, [<<"height:">>, num(HourPx), <<"px;">>]}])
             || H <- lists:seq(DS, DE - 1)],
    TotalH = (DE - DS) * HourPx,
    Columns = [day_column(D, Rr, Insts, TotalH, Cfg) || D <- Days, Rr <- PerDay],
    ?H:el('div',
          ?H:el('div',
                [?H:el('div',
                       [?H:el('div', [], [<<"ah-scheduler-dayview-gutter-header">>], []),
                        ?H:el('div', Header, [<<"ah-scheduler-dayview-header-cols">>],
                              [{style, MinW}])],
                       [<<"ah-scheduler-dayview-header">>], []),
                 AllDay,
                 ?H:el('div',
                       ?H:el('div',
                             [?H:el('div', Slots, [<<"ah-scheduler-dayview-gutter">>],
                                    [{aria_hidden, <<"true">>}]),
                              ?H:el('div', Columns, [<<"ah-scheduler-dayview-cols">>],
                                    [{style, MinW}]),
                              ?H:el('div', [], [<<"ah-scheduler-dayview-now-indicator">>],
                                    [{style, <<"display:none;">>}])],
                             [<<"ah-scheduler-dayview-body">>], []),
                       [<<"ah-scheduler-dayview-scroll">>], [])],
                [<<"ah-scheduler-dayview-hscroll">>], []),
          [<<"ah-scheduler-dayview">>], []).

res_id(undefined) -> undefined;
res_id(#{id := Id}) -> Id.

day_header_cell(D, Today, L) ->
    ?H:el('div',
          [?H:el(span, fmt(D * ?DAY, <<"EEE">>, L), [<<"ah-scheduler-dayview-header-day">>], []),
           ?H:el(span, fmt(D * ?DAY, <<"d">>, L), [<<"ah-scheduler-dayview-header-date">>], [])],
          [<<"ah-scheduler-dayview-header-cell">>,
           [<<" ah-scheduler-dayview-header-today">> || D =:= Today]], []).

hour_label(H, #{hf := 24}) -> <<(pad(H))/binary, ":00">>;
hour_label(H, #{labels := L}) ->
    H12 = case H rem 12 of 0 -> 12; X -> X end,
    <<(integer_to_binary(H12))/binary, " ",
      (maps:get(case H < 12 of true -> am; false -> pm end, L))/binary>>.

allday_event(I, Cfg) ->
    ?H:el('div', ev_title(I, span, <<"ah-scheduler-event-title">>),
          ev_classes(<<"ah-scheduler-event ah-scheduler-allday-event">>, I),
          [ev_attrs(I, Cfg), {style, ev_style(I)}]).

day_column(D, Res, Insts, TotalH, #{ds := DS, de := DE, sd := SD, sh := SH,
                                     today := Today, editable := Editable} = Cfg) ->
    W0 = D * ?DAY + DS * 60,
    W1 = D * ?DAY + DE * 60,
    Timed = [I#{cs => max(S, W0), ce => max(min(E, W1), max(S, W0) + SD)}
             || #{s := S, e := E, ev := #{all_day := false}} = I <- Insts,
                S < W1, E > W0 orelse (S =:= E andalso S >= W0),
                res_matches(I, Res)],
    Placed = assign_columns(Timed),
    Events = [?H:el('div',
                    [ev_title(I, 'div', <<"ah-scheduler-event-title">>),
                     ?H:el('div', time_range(S, E, false, Cfg), [<<"ah-scheduler-event-time">>], []),
                     [?H:el('div', [], [<<"ah-scheduler-timegrid-resize-handle">>], []) || Editable]],
                    ev_classes(<<"ah-scheduler-event ah-scheduler-timegrid-event">>, I),
                    [ev_attrs(I, Cfg),
                     {style, [<<"position:absolute;top:">>, num((Cs - W0) / SD * SH),
                              <<"px;height:">>, num((Ce - Cs) / SD * SH),
                              <<"px;left:">>, num(Col * 100 / Total), <<"%;width:">>,
                              num(100 / Total), <<"%;">>, ev_style(I)]}])
              || {#{s := S, e := E, cs := Cs, ce := Ce} = I, Col, Total} <- Placed],
    case Res of
        undefined ->
            ?H:el('div', Events,
                  [<<"ah-scheduler-dayview-col">>, [<<" ah-scheduler-dayview-col-today">> || D =:= Today]],
                  [{data_date, iso_date(D)},
                   {style, [<<"position:relative;height:">>, num(TotalH), <<"px;">>]}]);
        #{id := RId} ->
            ?H:el('div', Events, [<<"ah-scheduler-dayview-res-col">>],
                  [{data_date, iso_date(D)}, {data_resourceid, RId},
                   {style, [<<"position:relative;height:">>, num(TotalH),
                            <<"px;border-left:1px solid var(--ah-color-grey-100);">>]}])
    end.

%% sigil's assign-columns: by start, each item into the first column
%% where it does not overlap. Returns [{Item, Column, Columns}].
assign_columns([]) -> [];
assign_columns(Items) ->
    Sorted = lists:sort(fun(#{cs := A}, #{cs := B}) -> A =< B end, Items),
    {Cols, Placed} =
        lists:foldl(
          fun(#{cs := S, ce := E} = I, {Cs, Acc}) ->
                  Free = fun(Col) -> not lists:any(fun({S2, E2}) -> S < E2 andalso E > S2 end,
                                                   Col)
                         end,
                  case index_of(Free, Cs) of
                      0 -> {Cs ++ [[{S, E}]], [{I, length(Cs)} | Acc]};
                      N -> {setnth(N, Cs, [{S, E} | lists:nth(N, Cs)]), [{I, N - 1} | Acc]}
                  end
          end, {[], []}, Sorted),
    Total = length(Cols),
    [{I, C, Total} || {I, C} <- lists:reverse(Placed)].

index_of(F, L) -> index_of(F, L, 1).
index_of(_, [], _) -> 0;
index_of(F, [X | Xs], N) -> case F(X) of true -> N; false -> index_of(F, Xs, N + 1) end.

setnth(1, [_ | T], X) -> [X | T];
setnth(N, [H | T], X) -> [H | setnth(N - 1, T, X)].

%%%-------------------------------------------------------------------
%%% Month view
%%%-------------------------------------------------------------------

month_view(Insts, #{rs := RS, re := RE, cur := Cur, labels := L} = Cfg) ->
    Names = [fmt((RS + K) * ?DAY, <<"EEE">>, L) || K <- lists:seq(0, 6)],
    {_, CurMonth, _} = calendar:gregorian_days_to_date(Cur),
    Weeks = [lists:seq(W, W + 6) || W <- lists:seq(RS, RE - 1, 7)],
    ?H:el('div',
          [?H:el('div', [?H:el('div', N, [<<"ah-scheduler-monthview-header-cell">>], [])
                         || N <- Names],
                 [<<"ah-scheduler-monthview-header">>], [{aria_hidden, <<"true">>}]),
           ?H:el('div', [week_row(W, Insts, CurMonth, Cfg) || W <- Weeks],
                 [<<"ah-scheduler-monthview-body">>], [])],
          [<<"ah-scheduler-monthview">>], []).

week_row([WS | _] = Days, Insts, CurMonth, #{today := Today, max := Max, labels := L,
                                              resources := Res} = Cfg) ->
    Segs = week_segments(WS, Insts),
    Visible = [S || #{row := Rw} = S <- Segs, Rw < Max],
    Overflow = lists:foldl(fun(#{row := Rw, sc := Sc, ec := Ec}, Acc) when Rw >= Max ->
                                   lists:foldl(fun(C, A) -> maps:update_with(C, fun(X) -> X + 1 end,
                                                                             1, A)
                                               end, Acc, lists:seq(Sc, Ec - 1));
                              (_, Acc) -> Acc
                           end, #{}, Segs),
    Other = fun(D) -> element(2, calendar:gregorian_days_to_date(D)) =/= CurMonth end,
    ?H:el('div',
          [?H:el('div',
                 [?H:el('div', [], [<<"ah-scheduler-day">>,
                                    [<<" ah-scheduler-day-today">> || D =:= Today],
                                    [<<" ah-scheduler-day-other">> || Other(D)]],
                        [{data_date, iso_date(D)}]) || D <- Days],
                 [<<"ah-scheduler-week-bg">>], []),
           ?H:el('div',
                 [[?H:el('div', fmt(D * ?DAY, <<"d">>, L),
                         [<<"ah-scheduler-day-num">>,
                          [<<" ah-scheduler-day-num-today">> || D =:= Today],
                          [<<" ah-scheduler-day-num-other">> || Other(D)]],
                         [{style, [<<"grid-column:">>, integer_to_binary(K), <<";grid-row:1;">>]}])
                   || {K, D} <- lists:zip(lists:seq(1, 7), Days)],
                  [month_event(S, Res, Cfg) || S <- Visible],
                  [more_link(Col, lists:nth(Col, Days), N, Insts, Cfg)
                   || {Col, N} <- lists:sort(maps:to_list(Overflow))]],
                 [<<"ah-scheduler-week-content">>], [])],
          [<<"ah-scheduler-week-row">>], []).

%% sigil's compute-week-segments: every instance overlapping the week as
%% a bar from column sc to ec (1-based, ec exclusive) in the first free row.
week_segments(WS, Insts) ->
    W0 = WS * ?DAY, W1 = (WS + 7) * ?DAY,
    Segs = lists:filtermap(
             fun(#{s := S, e := E, ev := #{all_day := AD}} = I) ->
                     VS = S div ?DAY * ?DAY,
                     VE = case AD of true -> max(E, VS + ?DAY); false -> VS + ?DAY end,
                     case overlaps(I, W0, W1) of
                         false -> false;
                         true ->
                             Cs = max(VS, W0), Ce = min(VE, W1),
                             Sc = Cs div ?DAY - WS + 1,
                             Ec = (Ce + ?DAY - 1) div ?DAY - WS + 1,
                             Span = Ec - Sc,
                             Span > 0 andalso
                                 {true, #{inst => I, sc => Sc, ec => Ec, span => Span,
                                          multi => (VE - VS) > ?DAY,
                                          continuation => VS < W0, continues => VE > W1}}
                     end
             end, Insts),
    Sorted = lists:sort(fun(A, B) -> seg_key(A) =< seg_key(B) end, Segs),
    {_, Out} = lists:foldl(
                 fun(#{sc := Sc, ec := Ec} = Seg, {Rows, Acc}) ->
                         Free = fun(Rg) -> not lists:any(fun({A, B}) -> Sc < B andalso Ec > A end, Rg) end,
                         case index_of(Free, Rows) of
                             0 -> {Rows ++ [[{Sc, Ec}]], [Seg#{row => length(Rows)} | Acc]};
                             N -> {setnth(N, Rows, [{Sc, Ec} | lists:nth(N, Rows)]),
                                   [Seg#{row => N - 1} | Acc]}
                         end
                 end, {[], []}, Sorted),
    lists:reverse(Out).

seg_key(#{multi := M, span := Span, inst := #{s := S}}) ->
    {case M of true -> 0; false -> 1 end, -Span, S rem ?DAY}.

month_event(#{inst := #{s := S, ev := #{all_day := AD, resource := RId, title := T}} = I,
              sc := Sc, ec := Ec, row := Rw, multi := Multi,
              continuation := Cont, continues := Conts}, Res, Cfg) ->
    Dot = [?H:el(span, [], [<<"ah-scheduler-month-event-resource-dot">>],
                 [{style, [<<"background:">>, C, <<";">>]}])
           || #{id := Rid, color := C} <- Res, Rid =:= RId],
    ?H:el('div',
          [Dot,
           [?H:el(span, clock(S, Cfg), [<<"ah-scheduler-event-time">>], []) || not AD, not Multi],
           ev_title(I, span, <<"ah-scheduler-event-title">>)],
          [ev_classes(<<"ah-scheduler-event ah-scheduler-month-event">>, I),
           [<<" ah-scheduler-month-event-multi">> || Multi],
           [<<" ah-scheduler-month-event-start">> || Cont],
           [<<" ah-scheduler-month-event-end">> || Conts]],
          [ev_attrs(I, Cfg), {title, T},
           {style, [<<"grid-column:">>, integer_to_binary(Sc), <<"/">>, integer_to_binary(Ec),
                    <<";grid-row:">>, integer_to_binary(Rw + 2), <<";">>, ev_style(I)]}]).

%% "+n more" and, as an inert template, the day's popover.
more_link(Col, D, N, Insts, #{max := Max, labels := L} = Cfg) ->
    Day = lists:sort(fun(#{s := A}, #{s := B}) -> A =< B end,
                     [I || I <- Insts, overlaps(I, D * ?DAY, (D + 1) * ?DAY)]),
    Popover = [?H:el('div',
                     [?H:el(span, fmt(D * ?DAY, maps:get(popover_date, L), L), [], []),
                      ?H:el(span, <<"×"/utf8>>, [<<"ah-scheduler-more-popover-close">>],
                            [{role, button}, {tabindex, 0}, {aria_label, <<"Close">>}])],
                     [<<"ah-scheduler-more-popover-header">>], []),
               ?H:el('div',
                     [?H:el('div',
                            [?H:el(span, time_range(S, E, AD, Cfg), [<<"ah-scheduler-event-time">>], []),
                             ev_title(I, span, <<"ah-scheduler-event-title">>)],
                            ev_classes(<<"ah-scheduler-more-popover-item ah-scheduler-event">>, I),
                            [ev_attrs(I, Cfg), {style, ev_style(I)}])
                      || #{s := S, e := E, ev := #{all_day := AD}} = I <- Day],
                     [<<"ah-scheduler-more-popover-body">>], [])],
    ?H:el('div',
          [replace(maps:get(more, L), <<"{n}">>, integer_to_binary(N)),
           ?H:el(template, Popover, [], [])],
          [<<"ah-scheduler-day-more">>],
          [{data_date, iso_date(D)}, {role, button}, {tabindex, 0}, {aria_haspopup, dialog},
           {style, [<<"grid-column:">>, integer_to_binary(Col),
                    <<";grid-row:">>, integer_to_binary(Max + 2), <<";">>]}]).

%%%-------------------------------------------------------------------
%%% Agenda view
%%%-------------------------------------------------------------------

agenda_view(Insts, #{rs := RS, re := RE, labels := L, resources := Res} = Cfg) ->
    Groups = [{D, lists:sort(fun(#{s := A}, #{s := B}) -> A =< B end,
                             [I || I <- Insts, overlaps(I, D * ?DAY, (D + 1) * ?DAY)])}
              || D <- lists:seq(RS, RE - 1)],
    Body = case [G || {_, [_ | _]} = G <- Groups] of
               [] ->
                   ?H:el('div',
                   [?H:el('div', <<"📅"/utf8>>, [<<"ah-scheduler-agenda-empty-icon">>],
                          [{aria_hidden, <<"true">>}]),
                    ?H:el('div', maps:get(no_events, L), [], []),
                    ?H:el('div', maps:get(hint_navigate, L), [<<"ah-scheduler-agenda-empty-hint">>], [])],
                   [<<"ah-scheduler-agenda-empty">>], []);
               Gs ->
                   [?H:el('div',
                          [?H:el('div',
                                 [?H:el(span, fmt(D * ?DAY, <<"EEEE">>, L),
                                        [<<"ah-scheduler-agenda-day-name">>], []),
                                  ?H:el(span, fmt(D * ?DAY, maps:get(agenda_date, L), L),
                                        [<<"ah-scheduler-agenda-day-date">>], [])],
                                 [<<"ah-scheduler-agenda-day-header">>], []),
                           ?H:el('div', [agenda_row(I, Res, Cfg) || I <- Is],
                                 [<<"ah-scheduler-agenda-day-events">>], [])],
                          [<<"ah-scheduler-agenda-day-group">>], [])
                    || {D, Is} <- Gs]
           end,
    ?H:el('div', Body, [<<"ah-scheduler-agenda">>], []).

agenda_row(#{s := S, e := E, ev := #{all_day := AD, status := St, resource := RId}} = I, Res, Cfg) ->
    R = [X || #{id := Rid} = X <- Res, Rid =:= RId],
    ?H:el('div',
          [?H:el('div', [], [<<"ah-scheduler-agenda-event-status">>],
                 [{style, [<<"background:">>, status_color(St), <<";">>]}]),
           [?H:el('div', [], [<<"ah-scheduler-agenda-event-resource-dot">>],
                  [{style, [<<"background:">>, C, <<";">>]}]) || #{color := C} <- R],
           ?H:el('div', time_range(S, E, AD, Cfg), [<<"ah-scheduler-agenda-event-time">>], []),
           ev_title(I, 'div', <<"ah-scheduler-agenda-event-title">>),
           [?H:el('div', N, [<<"ah-scheduler-agenda-event-resource-name">>], []) || #{name := N} <- R]],
          [<<"ah-scheduler-agenda-event">>],
          ev_attrs(I, Cfg)).

%%%-------------------------------------------------------------------
%%% Timeline views: a row per resource on a horizontal time axis
%%%-------------------------------------------------------------------

timeline_view(Insts, #{view := View, cur := Cur, rs := RS, re := RE, ds := DS, de := DE,
                       today := Today, resources := Res, labels := L,
                       editable := Editable} = Cfg) ->
    {TS, TE, Slots} =
        case View of
            timeline_day ->
                {Cur * ?DAY + DS * 60, Cur * ?DAY + DE * 60,
                 [{Cur * ?DAY + H * 60, fmt(Cur * ?DAY + H * 60, <<"HH:mm">>, L), false}
                  || H <- lists:seq(DS, DE - 1)]};
            timeline_week ->
                {RS * ?DAY, RE * ?DAY,
                 [{D * ?DAY, fmt(D * ?DAY, <<"EEE d">>, L), D =:= Today}
                  || D <- lists:seq(RS, RE - 1)]};
            timeline_month ->
                {RS * ?DAY, RE * ?DAY,
                 [{D * ?DAY, fmt(D * ?DAY, <<"d">>, L), D =:= Today}
                  || D <- lists:seq(RS, RE - 1)]}
        end,
    Total = TE - TS,
    SlotW = case View of timeline_day -> 60; timeline_week -> 100; timeline_month -> 40 end,
    Width = max(100, length(Slots) * SlotW),
    Pct = 100 / length(Slots),
    Lines = [?H:el('div', [], [<<"ah-scheduler-timeline-grid-line">>],
                   [{style, [<<"left:">>, num((S - TS) * 100 / Total), <<"%;">>]}])
             || {S, _, _} <- Slots],
    Bar = fun(#{s := S, e := E} = I) ->
                  Left = max(0, S - TS) * 100 / Total,
                  Right = min(Total, E - TS) * 100 / Total,
                  ?H:el('div',
                        [ev_title(I, 'div', <<"ah-scheduler-timeline-event-title">>),
                         [[?H:el('div', [], [<<"ah-scheduler-timeline-resize-handle ah-scheduler-timeline-resize-left">>], []),
                           ?H:el('div', [], [<<"ah-scheduler-timeline-resize-handle ah-scheduler-timeline-resize-right">>], [])]
                          || Editable]],
                        ev_classes(<<"ah-scheduler-timeline-event">>, I),
                        [ev_attrs(I, Cfg),
                         {style, [<<"position:absolute;left:">>, num(Left), <<"%;width:">>,
                                  num(max(0.5, Right - Left)), <<"%;">>, ev_style(I)]}])
          end,
    InRange = [I || #{ev := #{all_day := AD}} = I <- Insts, overlaps(I, TS, TE),
                    not (AD andalso View =:= timeline_day)],
    Row = fun(R) ->
                  ?H:el('div',
                        ?H:el('div', [Lines, [Bar(I) || I <- InRange, res_matches(I, R)]],
                              [<<"ah-scheduler-timeline-row-events">>],
                              [{style, <<"position:relative;height:100%;">>}]),
                        [<<"ah-scheduler-timeline-row">>], [{data_resourceid, res_id(R)}])
          end,
    Panel = case Res of
                [] -> ?H:el('div', ?H:el('div', <<"—"/utf8>>, [<<"ah-scheduler-timeline-resource-name">>], []),
                            [<<"ah-scheduler-timeline-resource-cell">>], []);
                _ -> [?H:el('div',
                            [?H:el('div', [], [<<"ah-scheduler-timeline-resource-dot">>],
                                   [{style, [<<"background:">>, C, <<";">>]}]),
                             ?H:el('div', N, [<<"ah-scheduler-timeline-resource-name">>], [])],
                            [<<"ah-scheduler-timeline-resource-cell">>], [{data_resourceid, Id}])
                      || #{id := Id, name := N, color := C} <- Res]
            end,
    ?H:el('div',
          [?H:el('div',
                 [?H:el('div', [], [<<"ah-scheduler-timeline-resource-header">>], []),
                  ?H:el('div',
                        [?H:el('div', Label,
                               [<<"ah-scheduler-timeline-slot-header">>,
                                [<<" ah-scheduler-timeline-slot-today">> || IsToday]],
                               [{style, [<<"width:">>, num(Pct), <<"%;">>]}])
                         || {_, Label, IsToday} <- Slots],
                        [<<"ah-scheduler-timeline-slots-header">>],
                        [{style, [<<"min-width:">>, num(Width), <<"px;">>]}])],
                 [<<"ah-scheduler-timeline-header">>], [{aria_hidden, <<"true">>}]),
           ?H:el('div',
                 [?H:el('div', Panel, [<<"ah-scheduler-timeline-resource-panel">>], []),
                  ?H:el('div',
                        ?H:el('div', case Res of
                                         [] -> Row(undefined);
                                         _ -> [Row(R) || R <- Res]
                                     end,
                              [<<"ah-scheduler-timeline-grid">>],
                              [{data_from, iso_time(TS, false)}, {data_to, iso_time(TE, false)},
                               {style, [<<"min-width:">>, num(Width), <<"px;">>]}]),
                        [<<"ah-scheduler-timeline-scroll">>], [])],
                 [<<"ah-scheduler-timeline-body">>], [])],
          [<<"ah-scheduler-timeline">>], []).

%%%-------------------------------------------------------------------
%%% Navigation round trip
%%%-------------------------------------------------------------------

%% @doc The range a scheduler's navigation event asks for: its view, the
%% date shown and the visible days `start' .. `end' (exclusive), as ISO
%% dates, computed from `Event.value' and the view, first day and agenda
%% length in `Event.data'.
-spec scheduler_range(aihtml_action:event()) ->
          #{view := ah_sch_view(), date := binary(), start := binary(), 'end' := binary()}.
scheduler_range(#{data := Data} = Event) ->
    View = event_view(Data),
    D = event_date(Event),
    First = event_int(<<"firstDay">>, Data, 1, 0, 6),
    Agenda = event_int(<<"agendaDays">>, Data, 30, 1, 3660),
    {S, E} = profile(View, D, First, Agenda),
    #{view => View, date => iso_date(D), start => iso_date(S), 'end' => iso_date(E)}.

%% @doc Answer a scheduler's navigation (or an edit): render `Scheduler'
%% (built from the events of the requested range; its value and view are
%% taken from `Event') in place of the scheduler that fired `Event'.
%% Sends one morph operation, so the scroll position is kept.
-spec scheduler_update(aihtml_action:ctx(), aihtml_action:event(), #ah_scheduler{}) -> ok.
scheduler_update(Ctx, #{id := Id, data := Data} = Event, #ah_scheduler{} = S) ->
    S1 = S#ah_scheduler{id = Id, value = iso_date(event_date(Event)), view = event_view(Data)},
    aihtml_action:html(Ctx, {id, Id}, S1, morph).

event_view(Data) ->
    V = maps:get(<<"view">>, Data, <<"week">>),
    case [X || X <- ?VIEWS, atom_to_binary(X) =:= V] of
        [View] -> View;
        [] -> error({aihtml, {bad_scheduler_view, V}})
    end.

event_date(#{value := V}) when is_binary(V), V =/= <<>> -> days_of(V);
event_date(#{data := #{<<"date">> := V}}) -> days_of(V);
event_date(_) -> element(1, today(undefined)).

event_int(K, Data, Default, Lo, Hi) ->
    try binary_to_integer(maps:get(K, Data)) of
        N when N >= Lo, N =< Hi -> N;
        _ -> Default
    catch _:_ -> Default
    end.

%%%===================================================================
%%% swimlane
%%%===================================================================

-define(STACK_GAP, 10).
-define(HEADER_H, 46).

%% @doc A swimlane (sigil's swimlane): a grid of lanes (rows) by phases
%% (columns) with `Nodes' in the cells and flows between them. Nodes are
%% maps (see `ah_swim_node()': id, lane, phase, label, type (start, task,
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
-spec swimlane([swimlane_node()], aihtml_html:css(), aihtml_html:attrs()) -> #ah_swimlane{}.
swimlane(Nodes, Css, Attrs) ->
    build(#ah_swimlane{items = Nodes}, Css, Attrs).

-define(SWIM_COLORS, #{default => <<"#6B7280">>, blue => <<"#3B82F6">>, green => <<"#10B981">>,
                       red => <<"#EF4444">>, orange => <<"#F59E0B">>, purple => <<"#8B5CF6">>,
                       teal => <<"#14B8A6">>, pink => <<"#EC4899">>, indigo => <<"#6366F1">>,
                       yellow => <<"#EAB308">>}).

render_swimlane(#ah_swimlane{items = Items, editable = Editable} = R0) ->
    {Id, R} = ensure_id(R0),
    Classes = classes(R),
    L = labels(#{corner => <<"Lane / Phase">>, start => <<"Start">>, task => <<"Task">>,
                 decision => <<"Decision">>, 'end' => <<"End">>}, R#ah_swimlane.labels, swimlane),
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

%% @doc Functions besides the components that the aihtml facade re-exports.
-spec facade_extras() -> [{atom(), arity()}].
facade_extras() ->
    [{scheduler_update, 3}, {scheduler_range, 1}, {gantt_update, 3}, {swimlane_update, 3}].

%%%===================================================================
%%% Dates and times (minutes since gregorian day 0, local, no zone)
%%%===================================================================

%% ISO date or date-time -> {minutes, date only?}
parse_time({{_, _, _} = D, {H, Mi, _}}) when is_integer(H), is_integer(Mi), H >= 0, H < 24,
                                             Mi >= 0, Mi < 60 ->
    {days_of(D) * ?DAY + H * 60 + Mi, false};
parse_time({Y, M, D} = Date) when is_integer(Y), is_integer(M), is_integer(D) ->
    {days_of(Date) * ?DAY, true};
parse_time(L) when is_list(L) -> parse_time(unicode:characters_to_binary(L));
parse_time(<<Date:10/binary>>) -> {days_of(Date) * ?DAY, true};
parse_time(<<Date:10/binary, Sep, H:2/binary, ":", Mi:2/binary, _/binary>> = B)
  when Sep =:= $T; Sep =:= $\s ->
    try {binary_to_integer(H), binary_to_integer(Mi)} of
        {Hi, Mii} when Hi >= 0, Hi < 24, Mii >= 0, Mii < 60 ->
            {days_of(Date) * ?DAY + Hi * 60 + Mii, false};
        _ -> error({aihtml, {bad_date, B}})
    catch
        _:_ -> error({aihtml, {bad_date, B}})
    end;
parse_time(Other) -> error({aihtml, {bad_date, Other}}).

days_of({Y, M, D} = Date) when is_integer(Y), is_integer(M), is_integer(D) ->
    calendar:valid_date(Date) orelse error({aihtml, {bad_date, Date}}),
    calendar:date_to_gregorian_days(Date);
days_of(<<Y:4/binary, "-", M:2/binary, "-", D:2/binary>> = B) ->
    try {binary_to_integer(Y), binary_to_integer(M), binary_to_integer(D)} of
        Date -> calendar:valid_date(Date) orelse error({aihtml, {bad_date, B}}),
                calendar:date_to_gregorian_days(Date)
    catch
        _:_ -> error({aihtml, {bad_date, B}})
    end;
days_of(<<Date:10/binary, Sep, _/binary>>) when Sep =:= $T; Sep =:= $\s -> days_of(Date);
days_of(L) when is_list(L) -> days_of(unicode:characters_to_binary(L));
days_of(Other) -> error({aihtml, {bad_date, Other}}).

iso_date(Days) ->
    {Y, M, D} = calendar:gregorian_days_to_date(Days),
    <<(pad4(Y))/binary, "-", (pad(M))/binary, "-", (pad(D))/binary>>.

iso_time(Min, true) when Min rem ?DAY =:= 0 -> iso_date(Min div ?DAY);
iso_time(Min, _) ->
    <<(iso_date(Min div ?DAY))/binary, "T", (pad(Min rem ?DAY div 60))/binary, ":",
      (pad(Min rem 60))/binary>>.

%% {day, minute} of `today': a given date at noon, else the server's clock.
today(undefined) ->
    {Date, {H, M, _}} = calendar:local_time(),
    D = calendar:date_to_gregorian_days(Date),
    {D, D * ?DAY + H * 60 + M};
today(Date) ->
    D = days_of(Date),
    {D, D * ?DAY + 720}.

pad(N) when N < 10 -> <<"0", (integer_to_binary(N))/binary>>;
pad(N) -> integer_to_binary(N).

pad4(N) -> iolist_to_binary(io_lib:format("~4..0B", [N])).

stamp(Min) ->
    {Y, M, D} = calendar:gregorian_days_to_date(Min div ?DAY),
    <<(pad4(Y))/binary, (pad(M))/binary, (pad(D))/binary, "T",
      (pad(Min rem ?DAY div 60))/binary, (pad(Min rem 60))/binary, "00">>.

dow(Days) -> calendar:day_of_the_week(calendar:gregorian_days_to_date(Days)) rem 7.

sow(Days, First) -> Days - (dow(Days) - First + 7) rem 7.

first_of_month(Days) ->
    {Y, M, _} = calendar:gregorian_days_to_date(Days),
    calendar:date_to_gregorian_days(Y, M, 1).

last_of_month(Days) ->
    {Y, M, _} = calendar:gregorian_days_to_date(Days),
    calendar:date_to_gregorian_days(Y, M, calendar:last_day_of_the_month(Y, M)).

add_months(Days, N) ->
    {Y, M, D} = calendar:gregorian_days_to_date(Days),
    T = Y * 12 + (M - 1) + N,
    Ty = T div 12, Tm = T rem 12 + 1,
    calendar:date_to_gregorian_days(Ty, Tm, min(D, calendar:last_day_of_the_month(Ty, Tm))).

%% A display format: yyyy MMMM MMM MM M dd d EEEE EEE HH mm; other
%% characters are copied.
fmt(Min, Format, L) -> iolist_to_binary(fmt_tokens(Format, Min, L)).

fmt_tokens(<<>>, _, _) -> [];
fmt_tokens(<<"yyyy", R/binary>>, T, L) -> [integer_to_binary(y(T)) | fmt_tokens(R, T, L)];
fmt_tokens(<<"MMMM", R/binary>>, T, L) -> [lists:nth(m(T), maps:get(months, L)) | fmt_tokens(R, T, L)];
fmt_tokens(<<"MMM", R/binary>>, T, L) -> [lists:nth(m(T), maps:get(months_short, L)) | fmt_tokens(R, T, L)];
fmt_tokens(<<"MM", R/binary>>, T, L) -> [pad(m(T)) | fmt_tokens(R, T, L)];
fmt_tokens(<<"M", R/binary>>, T, L) -> [integer_to_binary(m(T)) | fmt_tokens(R, T, L)];
fmt_tokens(<<"dd", R/binary>>, T, L) -> [pad(d(T)) | fmt_tokens(R, T, L)];
fmt_tokens(<<"d", R/binary>>, T, L) -> [integer_to_binary(d(T)) | fmt_tokens(R, T, L)];
fmt_tokens(<<"EEEE", R/binary>>, T, L) -> [lists:nth(dow(T div ?DAY) + 1, maps:get(weekdays, L)) | fmt_tokens(R, T, L)];
fmt_tokens(<<"EEE", R/binary>>, T, L) -> [lists:nth(dow(T div ?DAY) + 1, maps:get(weekdays_short, L)) | fmt_tokens(R, T, L)];
fmt_tokens(<<"HH", R/binary>>, T, L) -> [pad(T rem ?DAY div 60) | fmt_tokens(R, T, L)];
fmt_tokens(<<"mm", R/binary>>, T, L) -> [pad(T rem 60) | fmt_tokens(R, T, L)];
fmt_tokens(<<C/utf8, R/binary>>, T, L) -> [<<C/utf8>> | fmt_tokens(R, T, L)].

y(T) -> element(1, calendar:gregorian_days_to_date(T div ?DAY)).
m(T) -> element(2, calendar:gregorian_days_to_date(T div ?DAY)).
d(T) -> element(3, calendar:gregorian_days_to_date(T div ?DAY)).

%%%===================================================================
%%% Recurrence (sigil's calendar/recurrence.cljs, as in aihtml_form_calendar)
%%%===================================================================

parse_rrule(<<"RRULE:", R/binary>>) -> parse_rrule(R);
parse_rrule(R) ->
    Rule = lists:foldl(
             fun(<<>>, Acc) -> Acc;
                (Part, Acc) ->
                     case binary:split(Part, <<"=">>) of
                         [K, V] -> rrule_part(string:uppercase(K), V, Acc);
                         _ -> error({aihtml, {bad_rrule, R}})
                     end
             end, #{}, binary:split(R, <<";">>, [global])),
    maps:is_key(freq, Rule) orelse error({aihtml, {bad_rrule, R}}),
    Rule.

rrule_part(<<"FREQ">>, V, Acc) ->
    case string:lowercase(V) of
        F when F =:= <<"daily">>; F =:= <<"weekly">>; F =:= <<"monthly">>; F =:= <<"yearly">> ->
            Acc#{freq => binary_to_atom(F)};
        _ -> error({aihtml, {bad_rrule, V}})
    end;
rrule_part(<<"INTERVAL">>, V, Acc) -> Acc#{interval => rrule_int(V)};
rrule_part(<<"COUNT">>, V, Acc) -> Acc#{count => rrule_int(V)};
rrule_part(<<"UNTIL">>, V, Acc) -> Acc#{until => rrule_until(V)};
rrule_part(<<"BYDAY">>, V, Acc) ->
    Days = [<<"SU">>, <<"MO">>, <<"TU">>, <<"WE">>, <<"TH">>, <<"FR">>, <<"SA">>],
    Index = maps:from_list(lists:zip(Days, lists:seq(0, 6))),
    Acc#{byday => [case maps:find(string:uppercase(D), Index) of
                       error -> error({aihtml, {bad_rrule, V}});
                       {ok, I} -> I
                   end || D <- binary:split(V, <<",">>, [global])]};
rrule_part(<<"BYMONTHDAY">>, V, Acc) ->
    Acc#{bymonthday => [rrule_int(X) || X <- binary:split(V, <<",">>, [global])]};
rrule_part(<<"BYMONTH">>, V, Acc) ->
    Acc#{bymonth => [rrule_int(X) || X <- binary:split(V, <<",">>, [global])]};
rrule_part(_, _, Acc) -> Acc.

rrule_int(V) ->
    try binary_to_integer(V) of
        N when N > 0 -> N;
        _ -> error({aihtml, {bad_rrule, V}})
    catch _:_ -> error({aihtml, {bad_rrule, V}})
    end.

rrule_until(<<Y:4/binary, M:2/binary, D:2/binary, Rest/binary>> = V) ->
    Day = days_of(<<Y/binary, "-", M/binary, "-", D/binary>>),
    case Rest of
        <<"T", H:2/binary, Mi:2/binary, _/binary>> ->
            Day * ?DAY + rrule_num(H, V) * 60 + rrule_num(Mi, V);
        _ -> Day * ?DAY
    end;
rrule_until(V) -> error({aihtml, {bad_rrule, V}}).

rrule_num(B, V) ->
    try binary_to_integer(B) catch _:_ -> error({aihtml, {bad_rrule, V}}) end.

%% The occurrences {Start, End} of a series overlapping [RS, RE), minutes.
expand(S, E, Rule, RS, RE, Ex) ->
    Ctx = #{s => S, dur => E - S, rs => RS, re => RE, ex => Ex,
            interval => maps:get(interval, Rule, 1), rule => Rule,
            until => maps:get(until, Rule, undefined),
            max => maps:get(count, Rule, undefined)},
    case {maps:get(freq, Rule), maps:get(byday, Rule, [])} of
        {weekly, [_ | _] = ByDay} ->
            Week = sow(S div ?DAY, 1),
            lists:reverse(weekly(Week, lists:sort(ByDay), Ctx, 0, 0, []));
        _ ->
            lists:reverse(generic(S, Ctx, 0, 0, []))
    end.

count_ok(_, #{max := undefined}) -> true;
count_ok(C, #{max := Max}) -> C < Max.

until_ok(_, #{until := undefined}) -> true;
until_ok(T, #{until := U}) -> T =< U.

occurrence(C, #{dur := Dur, rs := RS, re := RE, ex := Ex}, Acc) ->
    CE = C + Dur,
    case not lists:member(iso_date(C div ?DAY), Ex) andalso C < RE
        andalso (CE > RS orelse (Dur =:= 0 andalso C >= RS)) of
        true -> [{C, CE} | Acc];
        false -> Acc
    end.

weekly(Week, ByDay, #{re := RE, interval := I} = Ctx, Iter, Count, Acc) ->
    case Iter < ?MAX_ITERS andalso Week * ?DAY < RE andalso until_ok(Week * ?DAY, Ctx)
        andalso count_ok(Count, Ctx) of
        false -> Acc;
        true ->
            Offset = maps:get(s, Ctx) rem ?DAY,
            Wd = dow(Week),
            Cands = lists:sort([(Week + (D - Wd + 7) rem 7) * ?DAY + Offset || D <- ByDay]),
            {Iter1, Count1, Acc1} =
                lists:foldl(
                  fun(C, {It, Co, A}) ->
                          case count_ok(Co, Ctx) andalso It < ?MAX_ITERS
                              andalso C >= maps:get(s, Ctx) andalso until_ok(C, Ctx)
                              andalso C < RE of
                              true -> {It + 1, Co + 1, occurrence(C, Ctx, A)};
                              false -> {It, Co, A}
                          end
                  end, {Iter, Count, Acc}, Cands),
            weekly(Week + 7 * I, ByDay, Ctx, Iter1, Count1, Acc1)
    end.

generic(C, #{re := RE, rule := Rule, interval := I} = Ctx, Iter, Count, Acc) ->
    case Iter < ?MAX_ITERS andalso C < RE andalso until_ok(C, Ctx) andalso count_ok(Count, Ctx) of
        false -> Acc;
        true ->
            {Count1, Acc1} = case matches(C, Rule) of
                                 true -> {Count + 1, occurrence(C, Ctx, Acc)};
                                 false -> {Count, Acc}
                             end,
            generic(advance(C, maps:get(freq, Rule), I), Ctx, Iter + 1, Count1, Acc1)
    end.

matches(C, Rule) ->
    {_, M, D} = calendar:gregorian_days_to_date(C div ?DAY),
    lists:member(dow(C div ?DAY), maps:get(byday, Rule, [dow(C div ?DAY)]))
        andalso lists:member(D, maps:get(bymonthday, Rule, [D]))
        andalso lists:member(M, maps:get(bymonth, Rule, [M])).

advance(C, daily, I) -> C + I * ?DAY;
advance(C, weekly, I) -> C + 7 * I * ?DAY;
advance(C, monthly, I) -> add_months(C div ?DAY, I) * ?DAY + C rem ?DAY;
advance(C, yearly, I) -> add_months(C div ?DAY, 12 * I) * ?DAY + C rem ?DAY.

%%%===================================================================
%%% Shared
%%%===================================================================

build(R, Css, Attrs) ->
    Tag = element(1, R),
    ?E:build(R, fields(Tag), entry(?E:component_name(Tag)), Css, Attrs).

%% @doc The field names of one of this group's records.
-spec fields(atom()) -> [atom()].
fields(ah_gantt) -> record_info(fields, ah_gantt);
fields(ah_scheduler) -> record_info(fields, ah_scheduler);
fields(ah_swimlane) -> record_info(fields, ah_swimlane).

-spec render(element()) -> aihtml_html:html().
render(#ah_gantt{} = R) -> render_gantt(R);
render(#ah_scheduler{} = R) -> render_scheduler(R);
render(#ah_swimlane{} = R) -> render_swimlane(R).

classes(R) ->
    Tag = element(1, R),
    ?E:classes(R, fields(Tag), entry(?E:component_name(Tag))).

entry(Name) -> aihtml_catalog:entry(?MODULE, Name).

ensure_id(R) ->
    Id = case element(3, R) of
             undefined -> <<"ah-s", (integer_to_binary(erlang:unique_integer([positive])))/binary>>;
             Id0 -> text(Id0)
         end,
    {Id, setelement(3, R, Id)}.

hidden(undefined, _) -> [];
hidden(Name, Value) -> ?H:void(input, [], [{type, hidden}, {name, Name}, {value, Value}]).

height_style(undefined) -> undefined;
height_style(H) when is_integer(H), H > 0 -> [<<"height:">>, integer_to_binary(H), <<"px;">>];
height_style(H) -> error({aihtml, {bad_option, height, H}}).

pos_int(_, N) when is_integer(N), N > 0 -> N;
pos_int(K, V) -> error({aihtml, {bad_option, K, V}}).

in_range(_, N, Lo, Hi) when is_integer(N), N >= Lo, N =< Hi -> N;
in_range(K, V, _, _) -> error({aihtml, {bad_option, K, V}}).

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

%%%===================================================================
%%% Catalog
%%%===================================================================

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
              doc => <<"Move a bar without firing an event (e.g. to undo a refused change).">>}]},
     #{name => scheduler, category => data,
       signature => <<"scheduler(Events, Value, Css, Attrs)">>,
       root => <<"ah-scheduler">>,
       flags => [editable, no_all_day],
       classes => #{editable => [], no_all_day => []},
       options => [view, views, resources, first_day, slot_duration, slot_height,
                   day_start, day_end, height, agenda_days, day_max_events, hour_format,
                   today, toolbar, source, labels],
       behavior => <<"scheduler">>,
       events => [<<"change">>, <<"ah:event-click">>, <<"ah:event-change">>,
                  <<"ah:event-edit">>, <<"ah:event-delete">>, <<"ah:event-copy">>,
                  <<"ah:select">>, <<"ah:more-click">>],
       doc => <<"A resource scheduler: day and week time grids with a column per resource, "
                "month grid, agenda list and timeline rows, all rendered on the server; "
                "navigation loads the new range through an action.">>,
       option_docs =>
           #{editable => <<"Drag appointments (time, day, resource), resize them, drag over free "
                           "time to select a range (ah:select); right click or Shift+F10 opens a "
                           "menu (edit, delete, copy, new). Arrow keys move a focused "
                           "appointment, Shift+Up/Down resizes it.">>,
             no_all_day => <<"No all day row in the day and week views.">>,
             view => <<"day, week (default), month, agenda, timeline_day, timeline_week or "
                       "timeline_month.">>,
             views => <<"View buttons in the toolbar (default [day, week, month, agenda]).">>,
             resources => <<"Resources #{id, name, color}: columns of the day and week views, "
                            "rows of the timelines; appointments name theirs with resource.">>,
             first_day => <<"First day of the week, 0 = Sunday .. 6 (default 1).">>,
             slot_duration => <<"Minutes per time slot, a divisor of 60 (default 30).">>,
             slot_height => <<"Height of a slot in px (default 20).">>,
             day_start => <<"First hour shown in the time grid and timeline day (default 0).">>,
             day_end => <<"Hour the time grid ends (default 24).">>,
             height => <<"Height in px (default 600).">>,
             agenda_days => <<"Days in the agenda view (default 30).">>,
             day_max_events => <<"Rows of a month week before \"+n more\" (default 3).">>,
             hour_format => <<"12 (default) or 24.">>,
             today => <<"The date highlighted as today (default the server's date).">>,
             toolbar => <<"Show the toolbar (default true). Its navigation buttons appear only "
                          "with a source or a change postback.">>,
             source => <<"Action ref {Module, Action, Args} bound to change: it loads the range "
                         "the event names (scheduler_range/1) and answers with "
                         "scheduler_update/3.">>,
             labels => <<"Map of toolbar, view and menu texts, am, pm, months, months_short, "
                         "weekdays, weekdays_short (from Sunday) and the formats title_day, "
                         "title_month, range_start, range_end, agenda_date, popover_date.">>},
       methods =>
           [#{name => navigate, args => <<"(\"prev\" | \"next\" | \"today\")">>,
              doc => <<"Navigate as the toolbar buttons do (fires change).">>},
            #{name => setView, args => <<"(View)">>,
              doc => <<"Switch the view (fires change).">>},
            #{name => gotoDate, args => <<"(Iso)">>,
              doc => <<"Show another date (fires change).">>},
            #{name => getValue, args => <<"()">>, doc => <<"Return the shown date.">>}]},
     #{name => swimlane, category => data,
       signature => <<"swimlane(Nodes, Css, Attrs)">>,
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
