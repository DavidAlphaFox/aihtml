%%%-------------------------------------------------------------------
%%% @doc The tree grid, ported from sigil (data/treegrid). DOM and class
%%% names are sigil's, so the styles in priv/css/sigil apply unchanged.
%%%
%%%   ah_treegrid(Columns, Items, Css, Attrs)    rows in a tree: expand, sort, select
%%%   treegrid_children(Ctx, Event, Table)    (in an action) rows of a lazy node
%%%
%%% The tree grid is value-bearing: the root carries `data-ah-value' (the
%%% selected row keys, comma separated by aihtml_value) and fires `change' when the user
%%% changes the selection; a `name' in Attrs goes to a hidden input.
%%%
%%% Columns and rows are those of aihtml_datatable (the shared model is in
%%% aihtml_lib_table): a column is a field name, `{Field, Title}' or a map
%%% (see the type `column()'); rows are maps, keyed by `key_field'
%%% (default `id').
%%%
%%% Every row is rendered; rows under a collapsed node are `hidden'. The
%%% browser expands, collapses, sorts siblings and selects. A row whose
%%% children field is `lazy' shows an arrow; with a `load' action, the
%%% first expand POSTs it with Event.id = the row's id and Event.data =
%%% #{<<"key">>, <<"level">> (0-based), <<"treegrid">> (root id)}, and the
%%% action answers with `treegrid_children(Ctx, Event, Table)': `Table' is
%%% the same treegrid (columns and options) with the children as items.
%%% They are rendered here, appended to the body and moved under their
%%% parent by the behaviour method `childrenLoaded'.
%%%
%%% ah_treegrid/4 builds an element record (#ah_treegrid{}, defined in
%%% include/aihtml_treegrid.hrl) and render/1 turns it into HTML
%%% (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_treegrid).
-behaviour(aihtml_element).

-include("aihtml_treegrid.hrl").

-export([ah_treegrid/4, treegrid_children/3, render/1, fields/1, catalog/0, facade_extras/0]).

-export_type([element/0, column/0, row/0]).

-import(aihtml_lib_table, [cols/1, value/2, cell_content/3, cell_raw/2, align_style/1,
                            col_el/1, key_text/1, sel_keys/1, join/1, sort_opt/1, sort_key/1,
                            sort_by/3, col_key/2, header_cell/5, data_cell/4, one_of/3, bool/2,
                            field_name/2, check_ref/2, height_style/1, ensure_id/1, hidden/2,
                            text/1]).

-define(H, aihtml_html).
-define(E, aihtml_element).

-type html() :: aihtml_html:html().
-type css() :: aihtml_html:css().
-type attrs() :: aihtml_html:attrs().
-type column() :: aihtml_lib_table:column().
-type row() :: aihtml_lib_table:row().
-type element() :: #ah_treegrid{}.

%%%===================================================================
%%% Builder
%%%===================================================================

%% @doc A tree grid. `Columns' as in the module doc (the first one, or
%% `tree_column', holds the tree); `Items' nested rows (children under
%% `children_field') or flat rows (parent key under `parent_field'). Css:
%% `disabled'. Options: `value' (selected key or keys), `selection_mode',
%% `key_field', `children_field', `parent_field', `tree_column',
%% `expanded' (keys, or all), `sortable', `sort', `indent', `alt_rows',
%% `hover', `show_header', `resizable', `height', `empty_text', `load'.
-spec ah_treegrid([column()], [row()], css(), attrs()) -> #ah_treegrid{}.
ah_treegrid(Columns, Items, Css, Attrs) ->
    ?E:build(?MODULE, #ah_treegrid{columns = Columns, items = Items}, Css, Attrs).

%% @doc The field names of this component's record.
-spec fields(atom()) -> [atom()].
fields(ah_treegrid) -> record_info(fields, ah_treegrid).

%% @doc Functions besides the components that the aihtml facade re-exports.
-spec facade_extras() -> [{atom(), arity()}].
facade_extras() -> [{treegrid_children, 3}].

%%%===================================================================
%%% Rendering
%%%===================================================================

-spec render(element()) -> html().
render(#ah_treegrid{} = R) -> render_treegrid(R).

render_treegrid(#ah_treegrid{items = Items, value = Value, name = Name,
                             disabled = Disabled, selection_mode = Mode, sortable = Sortable,
                             sort = Sort0, show_header = ShowHeader, resizable = Resizable,
                             height = Height, empty_text = Empty, load = Load} = R0) ->
    Cfg0 = tg_config(R0),
    check_ref(load, Load),
    bool(show_header, ShowHeader),
    Sort = sort_opt(Sort0),
    {Id, R} = ensure_id(R0),
    Classes = ?E:classes(?MODULE, R),
    Cols = maps:get(cols, Cfg0),
    Sel = sel_keys(Value),
    Cfg = Cfg0#{root => Id, sel => Sel},
    Nodes = tg_sort(tg_nodes(Items, Cfg), Sort, Cfg),
    Rows = tg_alt(tg_flatten(Nodes, Cfg#{parent => <<>>, level => 0, prefix => Id,
                                         shown => true})),
    Visible = [Rw || #{visible := true} = Rw <- Rows],
    Focus = case [K || #{key := K} <- Visible, lists:member(K, Sel)] ++ [K || #{key := K} <- Visible] of
                [F | _] -> F;
                [] -> undefined
            end,
    NCols = length(Cols) + case Mode of checkbox -> 1; _ -> 0 end,
    Colgroup = ?H:el(colgroup,
                     [[col_el(<<"40px">>) || Mode =:= checkbox] | [col_el(W) || #{width := W} <- Cols]],
                     [], []),
    SortField = case Sort of undefined -> undefined; {SF, _} -> text(SF) end,
    Header = case ShowHeader of
                 false -> [];
                 true ->
                     ?H:el(table,
                           [Colgroup,
                            ?H:el(thead,
                                  ?H:el(tr,
                                        [[?H:el(th, ?H:void(input, [<<"ah-tg-header-checkbox">>],
                                                             [{type, checkbox}, {tabindex, -1},
                                                              {aria_label, <<"Select all rows">>}]),
                                                [<<"ah-tg-th">>, <<"ah-tg-th-checkbox">>],
                                                [{role, columnheader}])
                                          || Mode =:= checkbox],
                                         [header_cell(tg, C, Sortable, Sort, Resizable) || C <- Cols]],
                                        [<<"ah-tg-header-row">>], [{role, row}]),
                                  [], [{role, rowgroup}])],
                           [<<"ah-tg-table">>], [{role, presentation}])
             end,
    Body = ?H:el(tbody,
                 [[tg_row(Rw, Cfg#{focus => Focus}) || Rw <- Rows],
                  ?H:el(tr, ?H:el(td, Empty, [<<"ah-tg-cell-empty">>], [{colspan, NCols}]),
                        [<<"ah-tg-row-empty">>], [{hidden, Rows =/= []}])],
                 [], [{role, rowgroup}, {id, <<Id/binary, "-rows">>}]),
    Cur = join([K || K <- Sel, lists:any(fun(#{key := K1}) -> K1 =:= K end, Rows)]),
    ?H:el('div',
          [?H:el('div',
                 [?H:el('div', Header, [<<"ah-tg-header">>], [{hidden, not ShowHeader}]),
                  ?H:el('div', ?H:el(table, [Colgroup, Body], [<<"ah-tg-table">>],
                                     [{role, presentation}]),
                        [<<"ah-tg-body">>], [])],
                 [<<"ah-tg-content">>], []),
           hidden(Name, Cur)],
          Classes,
          [[{id, Id}, {role, treegrid},
            {aria_multiselectable, (Mode =:= multiple orelse Mode =:= checkbox) andalso <<"true">>},
            {aria_disabled, Disabled andalso <<"true">>},
            {data_ah, <<"treegrid">>}, {data_ah_value, Cur},
            {data_selection, Mode}, {data_sortable, Sortable andalso <<"true">>},
            {data_alt_rows, maps:get(alt, Cfg) andalso <<"true">>},
            {data_sort_field, SortField},
            {data_sort_dir, case Sort of undefined -> undefined; {_, D} -> D end},
            {data_load, case Load of
                            undefined -> undefined;
                            _ -> aihtml_action:token(Load)
                        end},
            {style, height_style(Height)}],
           ?E:root_attrs(R, change)]).

%% The options that shape rows, checked; shared with treegrid_children.
tg_config(#ah_treegrid{columns = Columns, selection_mode = Mode,
                       key_field = KF, children_field = CF, parent_field = PF,
                       tree_column = TC0, expanded = Exp0, sortable = Sortable, indent = Indent,
                       alt_rows = Alt, hover = Hover, resizable = Resizable}) ->
    one_of(selection_mode, Mode, [none, single, multiple, checkbox]),
    [field_name(K, V) || {K, V} <- [{key_field, KF}, {children_field, CF}, {parent_field, PF}]],
    [bool(K, V) || {K, V} <- [{sortable, Sortable}, {alt_rows, Alt}, {hover, Hover},
                              {resizable, Resizable}]],
    is_integer(Indent) andalso Indent >= 0 orelse error({aihtml, {bad_option, indent, Indent}}),
    Cols = cols(Columns),
    TreeCol = case {TC0, Cols} of
                  {undefined, [#{field := F} | _]} -> F;
                  {undefined, []} -> undefined;
                  _ ->
                      T = text(TC0),
                      lists:any(fun(#{field := F}) -> F =:= T end, Cols)
                          orelse error({aihtml, {bad_option, tree_column, TC0}}),
                      T
              end,
    Exp0 =:= all orelse is_list(Exp0) orelse error({aihtml, {bad_option, expanded, Exp0}}),
    Exp = case Exp0 of
              all -> all;
              L -> [key_text(K) || K <- L]
          end,
    #{cols => Cols, tree_col => TreeCol, mode => Mode, kf => KF, cf => CF, pf => PF,
      exp => Exp, indent => Indent, alt => Alt, hover => Hover}.

%% Items as nodes #{key, row, kids, lazy}: nested when a row has a list
%% under the children field, flat (parent keys) otherwise.
tg_nodes(Items, #{cf := CF} = Cfg) ->
    is_list(Items) orelse error({aihtml, {bad_option, items, Items}}),
    [is_map(I) orelse error({aihtml, {bad_row, I}}) || I <- Items],
    Nodes = case lists:any(fun(I) -> is_list(value(I, CF)) end, Items) of
                true -> [tg_nested(I, Cfg) || I <- Items];
                false -> tg_flat(Items, Cfg)
            end,
    Keys = tg_keys(Nodes),
    length(Keys) =:= length(lists:usort(Keys))
        orelse error({aihtml, {duplicate_row_keys, Keys -- lists:usort(Keys)}}),
    Nodes.

tg_keys(Nodes) -> lists:append([[K | tg_keys(Kids)] || #{key := K, kids := Kids} <- Nodes]).

tg_key(Row, KF) ->
    case value(Row, KF) of
        undefined -> error({aihtml, {row_without_key, Row}});
        K -> key_text(K)
    end.

tg_nested(Row, #{kf := KF, cf := CF} = Cfg) ->
    is_map(Row) orelse error({aihtml, {bad_row, Row}}),
    {Kids, Lazy} = case value(Row, CF) of
                       undefined -> {[], false};
                       lazy -> {[], true};
                       L when is_list(L) -> {[tg_nested(K, Cfg) || K <- L], false};
                       Other -> error({aihtml, {bad_children, Other}})
                   end,
    #{key => tg_key(Row, KF), row => Row, kids => Kids, lazy => Lazy}.

tg_flat(Items, #{kf := KF, cf := CF, pf := PF}) ->
    Keyed = [{tg_key(I, KF), I} || I <- Items],
    Known = maps:from_list(Keyed),
    Parent = fun(I) ->
                     case value(I, PF) of
                         P when P =:= undefined; P =:= null -> root;
                         P -> PT = key_text(P),
                              case maps:is_key(PT, Known) of
                                  true -> PT;
                                  false -> root
                              end
                     end
             end,
    ByParent = lists:foldr(fun({K, I}, Acc) ->
                                   maps:update_with(Parent(I), fun(L) -> [{K, I} | L] end,
                                                    [{K, I}], Acc)
                           end, #{}, Keyed),
    Build = fun Build(P) ->
                    [#{key => K, row => I, kids => Build(K), lazy => value(I, CF) =:= lazy}
                     || {K, I} <- maps:get(P, ByParent, [])]
            end,
    Build(root).

tg_sort(Nodes, undefined, _) -> Nodes;
tg_sort(Nodes, {F, _} = Sort, Cfg) ->
    Key = col_key(F, maps:get(cols, Cfg)),
    Sorted = sort_by(Nodes, fun(#{row := Row}) -> sort_key(value(Row, Key)) end, Sort),
    [N#{kids := tg_sort(Kids, Sort, Cfg)} || #{kids := Kids} = N <- Sorted].

%% Depth first, with level, parent, path, open and visible.
tg_flatten(Nodes, #{parent := Parent, level := Level, prefix := Prefix, shown := Shown,
                    exp := Exp} = Cfg) ->
    lists:append(
      [begin
           HasKids = Kids =/= [] orelse Lazy,
           Open = HasKids andalso not Lazy andalso (Exp =:= all orelse lists:member(K, Exp)),
           RowId = <<Prefix/binary, "-", (integer_to_binary(I))/binary>>,
           [N#{id => RowId, index => I, level => Level, parent => Parent, has_kids => HasKids,
               open => Open, visible => Shown}
            | tg_flatten(Kids, Cfg#{parent := K, level := Level + 1, prefix := RowId,
                                    shown := Shown andalso Open})]
       end
       || {I, #{key := K, kids := Kids, lazy := Lazy} = N}
              <- lists:zip(lists:seq(0, length(Nodes) - 1), Nodes)]).

%% Zebra stripes by position among the visible rows.
tg_alt(Rows) ->
    {Out, _} = lists:mapfoldl(fun(#{visible := true} = Rw, N) -> {Rw#{alt => N rem 2 =:= 1}, N + 1};
                                 (Rw, N) -> {Rw#{alt => false}, N}
                              end, 0, Rows),
    Out.

tg_row(#{id := RowId, key := K, row := Row, index := I, level := Level, parent := Parent,
         has_kids := HasKids, open := Open, visible := Visible, lazy := Lazy, alt := Alt},
       #{cols := Cols, tree_col := TreeCol, mode := Mode, indent := Indent, alt := AltRows,
         hover := Hover, root := Root, sel := Sel} = Cfg) ->
    Selected = Mode =/= none andalso lists:member(K, Sel),
    Toggle = case {HasKids, Open} of
                 {false, _} -> <<"ah-tg-toggle-leaf">>;
                 {true, true} -> <<"ah-tg-toggle-open">>;
                 {true, false} -> <<"ah-tg-toggle-closed">>
             end,
    Cells = [case F of
                 TreeCol ->
                     ?H:el(td,
                           ?H:el('div',
                                 [?H:el(span, case HasKids of
                                                  true -> <<"▶"/utf8>>;
                                                  false -> <<>>
                                              end,
                                        [<<"ah-tg-toggle">>, Toggle], [{aria_hidden, <<"true">>}]),
                                  ?H:el(span, cell_content(C, value(Row, Key), Row),
                                        [<<"ah-tg-cell-text">>], [])],
                                 [<<"ah-tg-tree-indent">>],
                                 [{style, [<<"padding-left:">>, integer_to_binary(Level * Indent),
                                           <<"px;">>]}]),
                           [<<"ah-tg-cell">>, <<"ah-tg-tree-cell">>, Class],
                           [{role, gridcell}, {data_field, F}, {style, align_style(Align)},
                            {data_value, cell_raw(C, value(Row, Key))}]);
                 _ ->
                     data_cell(tg, C, Row, false)
             end
             || #{field := F, key := Key, align := Align, class := Class} = C <- Cols],
    ?H:el(tr,
          [[?H:el(td, ?H:void(input, [<<"ah-tg-row-checkbox">>],
                              [{type, checkbox}, {tabindex, -1}, {checked, Selected},
                               {aria_label, <<"Select row">>}]),
                  [<<"ah-tg-cell">>, <<"ah-tg-checkbox-cell">>], [{role, gridcell}])
            || Mode =:= checkbox],
           Cells],
          [<<"ah-tg-row">>, [<<"ah-tg-row-leaf">> || not HasKids],
           [<<"ah-tg-row-alt">> || AltRows andalso Alt],
           [<<"ah-tg-row-hover">> || Hover], [<<"ah-tg-row-selected">> || Selected]],
          [{id, RowId}, {role, row}, {data_key, K}, {data_parent, Parent},
           {data_level, Level}, {data_i, I},
           {aria_level, Level + 1},
           {aria_expanded, HasKids andalso atom_to_binary(Open, utf8)},
           {aria_selected, Mode =/= none andalso atom_to_binary(Selected, utf8)},
           {tabindex, case maps:get(focus, Cfg, undefined) of K -> 0; _ -> -1 end},
           {hidden, not Visible},
           {data_lazy, Lazy andalso <<"true">>},
           {data_treegrid, Lazy andalso Root}]).

%% @doc Answer a treegrid's `load' action: render `Table's items as the
%% children of the lazy row that fired it and expand the row. `Table' is
%% the page's treegrid (columns and options) with the children as items;
%% its id is taken from the event. Sends the rendered rows (appended to
%% the table body) and a call of the behaviour method `childrenLoaded'.
%% An empty list turns the row into a leaf.
-spec treegrid_children(aihtml_action:ctx(), aihtml_action:event(), #ah_treegrid{}) -> ok.
treegrid_children(Ctx, #{id := RowId0, data := Data}, #ah_treegrid{items = Items} = T) ->
    RowId = text(RowId0),
    Root = text(maps:get(<<"treegrid">>, Data)),
    Parent = text(maps:get(<<"key">>, Data)),
    Level = binary_to_integer(text(maps:get(<<"level">>, Data))),
    Cfg0 = tg_config(T),
    Cfg = Cfg0#{root => Root, sel => sel_keys(maps:get(<<"value">>, Data, undefined))},
    Nodes = tg_sort(tg_nodes(Items, Cfg), sort_opt(T#ah_treegrid.sort), Cfg),
    Rows = [Rw#{alt => false}
            || Rw <- tg_flatten(Nodes, Cfg#{parent => Parent, level => Level + 1,
                                            prefix => RowId, shown => true})],
    aihtml_action:html(Ctx, {id, <<Root/binary, "-rows">>}, [tg_row(Rw, Cfg) || Rw <- Rows],
                       append),
    aihtml_action:call(Ctx, {id, Root}, childrenLoaded, [RowId]).

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => treegrid, category => data,
       signature => <<"ah_treegrid(Columns, Items, Css, Attrs)">>,
       root => <<"ah-tg">>, flags => [disabled],
       options => [value, selection_mode, key_field, children_field, parent_field, tree_column,
                   expanded, sortable, sort, indent, alt_rows, hover, show_header, resizable,
                   height, empty_text, load],
       behavior => <<"treegrid">>,
       events => [<<"change">>, <<"ah:expand">>, <<"ah:collapse">>, <<"ah:sort">>,
                  <<"ah:row-click">>, <<"ah:row-dblclick">>, <<"ah:load">>,
                  <<"ah:column-resize">>],
       doc => <<"A table whose rows form a tree: an indented tree column with expand arrows, "
                "sorting within each level, row selection and keyboard navigation; children "
                "of lazy rows come from the server.">>,
       option_docs =>
           maps:merge(aihtml_lib_table:option_docs(),
                      #{children_field => <<"Field of nested children (default children); "
                                            "the atom lazy there means: load them.">>,
                        parent_field => <<"Field of the parent key in flat items "
                                          "(default parent_id).">>,
                        tree_column => <<"Field of the column that holds the tree (default the "
                                         "first column).">>,
                        expanded => <<"Keys of the rows open at first, or all.">>,
                        indent => <<"Indent per level in pixels (default 24).">>,
                        load => <<"Action ref {Module, Action, Args} run when a lazy row is "
                                  "first expanded; it answers with treegrid_children/3.">>}),
       methods =>
           [#{name => getValue, args => <<"()">>, doc => <<"Return the selected keys, comma separated (a comma inside a key is escaped as \\,; aihtml_value:split/1 reads it).">>},
            #{name => setValue, args => <<"(Keys)">>,
              doc => <<"Select these keys (an array or comma separated text, see aihtml_value) without firing change.">>},
            #{name => clearSelection, args => <<"()">>, doc => <<"Select nothing (fires no change).">>},
            #{name => expand, args => <<"(Key)">>, doc => <<"Expand the row with this key.">>},
            #{name => collapse, args => <<"(Key)">>, doc => <<"Collapse the row with this key.">>},
            #{name => toggle, args => <<"(Key)">>, doc => <<"Expand or collapse the row.">>},
            #{name => expandAll, args => <<"()">>, doc => <<"Expand every row (lazy rows stay closed).">>},
            #{name => collapseAll, args => <<"()">>, doc => <<"Collapse every row.">>},
            #{name => ensureVisible, args => <<"(Key)">>,
              doc => <<"Expand the ancestors of the row and scroll it into view.">>},
            #{name => sort, args => <<"(Field, Dir)">>,
              doc => <<"Sort siblings by a column: \"asc\", \"desc\" or null for the original order.">>},
            #{name => childrenLoaded, args => <<"(RowId)">>,
              doc => <<"Called by treegrid_children/3: move the new rows under their parent "
                       "and expand it.">>}]}].
