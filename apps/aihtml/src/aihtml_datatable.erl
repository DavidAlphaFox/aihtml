%%%-------------------------------------------------------------------
%%% @doc The data table, ported from sigil (data/datatable). DOM and class
%%% names are sigil's, so the styles in priv/css/sigil apply unchanged.
%%%
%%%   datatable(Columns, Rows, Css, Attrs)    sort, filter, page, select, edit
%%%   datatable_rows(Ctx, Event, Table)       (in an action) answer a remote query
%%%   datatable_row(Ctx, Event, Table, Row)   (in an action) re-render one row
%%%   datatable_query(Event)                  the view state of a remote query
%%%
%%% The table is value-bearing: the root carries `data-ah-value' (the
%%% selected row keys, comma separated by aihtml_value) and fires `change' when the user
%%% changes the selection; a `name' in Attrs goes to a hidden input.
%%%
%%% == Columns and rows ==
%%%
%%% A column is a field name, `{Field, Title}' or a map (see the type
%%% `column()'). Rows are maps; a row's key is its `key_field' (default
%%% `id'). Keys are written as text and joined with commas in the value
%%% (aihtml_value: a comma inside a key is escaped as `\,'). The column model is shared with
%%% aihtml_treegrid (aihtml_lib_table).
%%%
%%% == datatable: local and remote ==
%%%
%%% Local (no `source'): every row is rendered (the initial sort and page
%%% applied here) and the browser sorts, filters, searches and pages.
%%%
%%% Remote (`source' = {Mod, Action, Args}): the table renders the rows it
%%% is given as the current page and `total' as the row count. Each view
%%% change (sort, filter, search, page, page size) writes the view state on
%%% the root and fires 'ah:query' there, which runs the source action.
%%% `datatable_query(Event)' reads that state (sort, page, page_size,
%%% offset, limit, search, filters); the action fetches the page and
%%% answers with `datatable_rows(Ctx, Event, Table)', `Table' being the
%%% same table (with its source) holding the page's rows and `total'. The
%%% table is rendered here with the view state of the event and morphed
%%% into the page. `datatable_page(Columns, Rows, Query)' applies a query
%%% to rows in memory.
%%%
%%% == Edits ==
%%%
%%% With `editable', a double click (or Enter / F2 on a row) edits a cell.
%%% Committing fires 'ah:cell-edit' on the cell; with an `edit' action ref
%%% the cell POSTs it with Event.data = #{<<"key">>, <<"field">>,
%%% <<"value">> (new), <<"old">>, <<"table">> (root id)}. The action may
%%% answer with `datatable_row(Ctx, Event, Table, Row)' to show the stored
%%% row (rendered here, with the column renderers).
%%%
%%% datatable/4 builds an element record (#ah_datatable{}, defined in
%%% include/aihtml_datatable.hrl) and render/1 turns it into HTML
%%% (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_datatable).
-behaviour(aihtml_element).

-include("aihtml_datatable.hrl").

-compile({parse_transform, beamai_mustache_transform}).
-mustache_template({tpl_datatable_pager, "../templates/datatable_pager.mustache"}).

-export([datatable/4, datatable_rows/3, datatable_row/4, datatable_query/1,
         render/1, fields/1, catalog/0, facade_extras/0]).
%% In-memory queries and the pager model, for pages and tests.
-export([datatable_page/3, pager_view/5]).

-export_type([element/0, column/0, row/0, query/0, condition/0, filter/0]).

-import(aihtml_lib_table, [col/1, cols/1, value/2, raw_text/1, num/1, cell_text/1,
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
-type element() :: #ah_datatable{}.
%% A column filter: a text (contains, case-insensitive) or an advanced
%% condition with its operand.
-type condition() :: contains | not_contains | equals | not_equals
                     | starts_with | ends_with | gt | gte | lt | lte
                     | empty | not_empty.
-type filter() :: iodata() | {condition(), iodata() | number()}.
%% The view state of a remote datatable query. `page' is 1-based;
%% `offset' and `limit' are the same for SQL (`limit' is undefined when
%% the table does not page).
-type query() :: #{sort := undefined | {binary(), asc | desc},
                   page := pos_integer(),
                   page_size := undefined | pos_integer(),
                   offset := non_neg_integer(),
                   limit := undefined | pos_integer(),
                   search := binary(),
                   filters := #{binary() => binary() | {condition(), binary()}}}.

-define(CONDITIONS, [contains, not_contains, equals, not_equals, starts_with, ends_with,
                     gt, gte, lt, lte, empty, not_empty]).
-define(TEXT_CONDITIONS, [contains, not_contains, equals, not_equals, starts_with,
                          ends_with, empty, not_empty]).
-define(NUMBER_CONDITIONS, [contains, equals, not_equals, gt, gte, lt, lte, empty, not_empty]).

%%%===================================================================
%%% Builder
%%%===================================================================

%% @doc A data table. `Columns' as in the module doc; `Rows' maps (in
%% remote mode, the current page). Css: `disabled'. Options: `value',
%% `selection_mode', `key_field', `sortable', `sort', `filter' (none | row
%% | search | advanced), `filters', `search', `page_size', `page',
%% `page_sizes', `total', `source', `editable', `edit', `row_details',
%% `expanded', `resizable', `column_chooser', `alt_rows', `hover',
%% `show_header', `height', `empty_text', `texts'.
-spec datatable([column()], [row()], css(), attrs()) -> #ah_datatable{}.
datatable(Columns, Rows, Css, Attrs) ->
    ?E:build(?MODULE, #ah_datatable{columns = Columns, rows = Rows}, Css, Attrs).

%% @doc The field names of this component's record.
-spec fields(atom()) -> [atom()].
fields(ah_datatable) -> record_info(fields, ah_datatable).

%% @doc Functions besides the components that the aihtml facade re-exports.
-spec facade_extras() -> [{atom(), arity()}].
facade_extras() ->
    [{datatable_rows, 3}, {datatable_row, 4}, {datatable_query, 1}].

%%%===================================================================
%%% Rendering
%%%===================================================================

-spec render(element()) -> html().
render(#ah_datatable{} = R) -> render_datatable(R).

render_datatable(#ah_datatable{columns = Columns, rows = Rows0, value = Value, name = Name,
                               disabled = Disabled, selection_mode = Mode, key_field = KF,
                               sortable = Sortable, sort = Sort0, filter = FilterMode,
                               filters = Filters0, search = Search0, page_size = PageSize,
                               page = Page0, page_sizes = Sizes, total = Total0,
                               source = Source, editable = Editable, edit = Edit,
                               row_details = Details, expanded = Expanded0,
                               resizable = Resizable, column_chooser = Chooser,
                               alt_rows = AltRows, hover = Hover, show_header = ShowHeader,
                               height = Height, empty_text = Empty, texts = Texts0} = R0) ->
    one_of(selection_mode, Mode, [none, single, multiple, checkbox]),
    one_of(filter, FilterMode, [none, row, search, advanced]),
    field_name(key_field, KF),
    [bool(K, V) || {K, V} <- [{sortable, Sortable}, {editable, Editable}, {resizable, Resizable},
                              {column_chooser, Chooser}, {alt_rows, AltRows}, {hover, Hover},
                              {show_header, ShowHeader}]],
    PageSize =:= undefined orelse (is_integer(PageSize) andalso PageSize > 0)
        orelse error({aihtml, {bad_option, page_size, PageSize}}),
    is_integer(Page0) andalso Page0 > 0 orelse error({aihtml, {bad_option, page, Page0}}),
    is_list(Sizes) andalso lists:all(fun(S) -> is_integer(S) andalso S > 0 end, Sizes)
        orelse error({aihtml, {bad_option, page_sizes, Sizes}}),
    Total0 =:= undefined orelse (is_integer(Total0) andalso Total0 >= 0)
        orelse error({aihtml, {bad_option, total, Total0}}),
    check_ref(source, Source),
    check_ref(edit, Edit),
    Details =:= undefined orelse is_function(Details, 1)
        orelse error({aihtml, {bad_option, row_details, Details}}),
    is_list(Expanded0) orelse error({aihtml, {bad_option, expanded, Expanded0}}),
    is_list(Rows0) orelse error({aihtml, {bad_option, rows, Rows0}}),
    Texts = texts(Texts0),
    Sort = sort_opt(Sort0),
    Filters = filters(Filters0),
    Search = text(Search0),
    {Id, R} = ensure_id(R0),
    Classes = ?E:classes(?MODULE, R),
    Cols = cols(Columns),
    Remote = Source =/= undefined,
    Sel = sel_keys(Value),
    Expanded = [key_text(K) || K <- Expanded0],
    %% [{Index, Key, Row}]
    Indexed = [begin
                   is_map(Row) orelse error({aihtml, {bad_row, Row}}),
                   Key = case value(Row, KF) of
                             undefined -> integer_to_binary(I);
                             K -> key_text(K)
                         end,
                   {I, Key, Row}
               end || {I, Row} <- lists:zip(lists:seq(0, length(Rows0) - 1), Rows0)],
    %% local: the initial view is computed here as the browser would
    {Ordered, Total, Page, Shown} =
        case Remote of
            true ->
                T = case Total0 of undefined -> length(Indexed); _ -> Total0 end,
                {Indexed, T, clamp_page(Page0, T, PageSize), [K || {_, K, _} <- Indexed]};
            false ->
                Match = [X || {_, _, Row} = X <- Indexed,
                              row_matches(Row, Cols, Search, Filters)],
                Sorted = sort_rows(Match, Cols, Sort),
                T = length(Sorted),
                P = clamp_page(Page0, T, PageSize),
                PageRows = page_slice(Sorted, P, PageSize),
                {Sorted ++ [X || X <- Indexed, not lists:member(X, Sorted)], T, P,
                 [K || {_, K, _} <- PageRows]}
        end,
    VisibleCols = [C || #{hidden := false} = C <- Cols],
    Extra = case Details of undefined -> 0; _ -> 1 end + case Mode of checkbox -> 1; _ -> 0 end,
    Span = length(VisibleCols) + Extra,
    Focus = case [K || {_, K, _} <- Ordered, lists:member(K, Shown), lists:member(K, Sel)] ++ Shown of
                [F | _] -> F;
                [] -> undefined
            end,
    Cfg = #{root => Id, cols => Cols, mode => Mode, sel => Sel, editable => Editable,
            details => Details, expanded => Expanded, span => Span, alt => AltRows,
            hover => Hover, focus => Focus, texts => Texts},
    {BodyRows, _} = lists:mapfoldl(
                      fun({I, K, Row}, N) ->
                              Vis = lists:member(K, Shown),
                              {dt_row(I, K, Row, Vis, Vis andalso N rem 2 =:= 1, Cfg),
                               case Vis of true -> N + 1; false -> N end}
                      end, 0, Ordered),
    Colgroup = ?H:el(colgroup,
                     [[col_el(<<"36px">>) || Details =/= undefined],
                      [col_el(<<"40px">>) || Mode =:= checkbox],
                      [case H of
                           true -> ?H:void(col, [], [{hidden, true}]);
                           false -> col_el(W)
                       end || #{width := W, hidden := H} <- Cols]],
                     [], []),
    SortField = case Sort of undefined -> undefined; {SF, _} -> text(SF) end,
    HeaderRow = ?H:el(tr,
                      [[?H:el(th, [], [<<"ah-dt-th">>, <<"ah-dt-th-expand">>], [{role, columnheader}])
                        || Details =/= undefined],
                       [?H:el(th, ?H:void(input, [<<"ah-dt-header-checkbox">>],
                                          [{type, checkbox},
                                           {aria_label, maps:get(select_all, Texts)}]),
                              [<<"ah-dt-th">>, <<"ah-dt-th-checkbox">>], [{role, columnheader}])
                        || Mode =:= checkbox],
                       [header_cell(dt, C, Sortable, Sort, Resizable) || C <- Cols]],
                      [<<"ah-dt-header-row">>], [{role, row}]),
    Pre = [[?H:el(td, [], [<<"ah-dt-filter-cell">>], []) || Details =/= undefined],
           [?H:el(td, [], [<<"ah-dt-filter-cell">>], []) || Mode =:= checkbox]],
    FilterRow = case FilterMode of
                    row ->
                        ?H:el(tr, [Pre, [filter_cell(C, Filters, Texts) || C <- Cols]],
                              [<<"ah-dt-filter-row">>], [{role, row}]);
                    advanced ->
                        ?H:el(tr, [Pre, [adv_filter_cell(C, Filters, Texts) || C <- Cols]],
                              [<<"ah-dt-filter-row">>, <<"ah-dt-filter-row-advanced">>],
                              [{role, row}]);
                    _ -> []
                end,
    Header = ?H:el('div',
                   [[?H:el('div', ?H:void(input, [<<"ah-dt-search-input">>],
                                          [{type, text}, {value, Search},
                                           {placeholder, maps:get(search, Texts)},
                                           {aria_label, maps:get(search, Texts)}]),
                           [<<"ah-dt-search-bar">>], [])
                     || FilterMode =:= search],
                    [?H:el('div', ?H:el(button, <<"☰"/utf8>>, [<<"ah-dt-chooser-btn">>],
                                        [{type, button}, {title, maps:get(columns, Texts)},
                                         {aria_label, maps:get(columns, Texts)},
                                         {aria_haspopup, <<"true">>},
                                         {aria_expanded, <<"false">>}]),
                           [<<"ah-dt-chooser-wrap">>], [])
                     || Chooser],
                    ?H:el(table, [Colgroup, ?H:el(thead, [HeaderRow, FilterRow], [],
                                                  [{role, rowgroup}])],
                          [<<"ah-dt-table">>], [{role, presentation}])],
                   [<<"ah-dt-header">>], [{hidden, not ShowHeader}]),
    Body = ?H:el('div',
                 ?H:el(table,
                       [Colgroup,
                        ?H:el(tbody,
                              [BodyRows,
                               ?H:el(tr, ?H:el(td, Empty, [<<"ah-dt-cell-empty">>],
                                               [{colspan, Span}]),
                                     [<<"ah-dt-row-empty">>], [{hidden, Shown =/= []}])],
                              [], [{role, rowgroup}, {id, <<Id/binary, "-rows">>}])],
                       [<<"ah-dt-table">>], [{role, presentation}]),
                 [<<"ah-dt-body">>], []),
    Pager = case PageSize of
                undefined -> [];
                _ ->
                    ?H:el('div',
                          aihtml_tpl:safe(tpl_datatable_pager(
                                            pager_view(Page, PageSize, Total, Sizes, Texts))),
                          [<<"ah-dt-pager-container">>],
                          [{data_info, maps:get(info, Texts)}, {data_prev, maps:get(prev, Texts)},
                           {data_next, maps:get(next, Texts)},
                           {data_size, maps:get(page_size, Texts)},
                           {data_sizes, join([integer_to_binary(S) || S <- Sizes])}])
            end,
    Panel = case Chooser of
                false -> [];
                true ->
                    ?H:el('div',
                          [?H:el(label,
                                 [?H:void(input, [<<"ah-dt-chooser-checkbox">>],
                                          [{type, checkbox}, {data_field, F}, {checked, not H}]),
                                  Title],
                                 [<<"ah-dt-chooser-item">>], [])
                           || #{field := F, title := Title, hidden := H} <- Cols],
                          [<<"ah-dt-chooser-panel">>],
                          [{role, group}, {aria_label, maps:get(columns, Texts)}])
            end,
    Cur = join(Sel),
    Query = case Source of
                undefined -> [];
                _ -> [aihtml:on('ah:query', Source), {data_ah_sync, replace}]
            end,
    ?H:el('div',
          [?H:el('div',
                 [Header, Body,
                  ?H:el('div', ?H:el('div', [], [<<"ah-dt-loading-spinner">>], []),
                        [<<"ah-dt-loading-overlay">>], [])],
                 [<<"ah-dt-content">>], []),
           Pager, Panel, hidden(Name, Cur)],
          Classes,
          [[{id, Id}, {role, grid},
            {aria_multiselectable, (Mode =:= multiple orelse Mode =:= checkbox) andalso <<"true">>},
            {aria_disabled, Disabled andalso <<"true">>},
            {data_ah, <<"datatable">>}, {data_ah_value, Cur},
            {data_selection, Mode},
            {data_mode, case Remote of true -> remote; false -> local end},
            {data_sortable, Sortable andalso <<"true">>},
            {data_alt_rows, AltRows andalso <<"true">>},
            {data_sort_field, SortField},
            {data_sort_dir, case Sort of undefined -> undefined; {_, D} -> D end},
            {data_filter, FilterMode},
            {data_search, Search =/= <<>> andalso Search},
            {data_page, Page}, {data_page_size, PageSize},
            {data_total, Remote andalso Total},
            {data_editable, Editable andalso <<"true">>},
            {data_edit, case Edit of
                            undefined -> undefined;
                            _ -> aihtml_action:token(Edit)
                        end},
            {data_hidden, case [F || #{field := F, hidden := true} <- Cols] of
                              [] -> undefined;
                              Hs -> join(Hs)
                          end},
            {data_expanded, Expanded =/= [] andalso join(Expanded)},
            {style, height_style(Height)},
            Query],
           ?E:root_attrs(R, change)]).

dt_row(I, K, Row, Visible, Alt, #{root := Root, cols := Cols, mode := Mode, sel := Sel,
                                  editable := Editable, details := Details,
                                  expanded := Expanded, span := Span, alt := AltRows,
                                  hover := Hover, texts := Texts} = Cfg) ->
    Selected = Mode =/= none andalso lists:member(K, Sel),
    Open = Details =/= undefined andalso lists:member(K, Expanded),
    RowId = row_id(Root, K),
    Tr = ?H:el(tr,
               [[?H:el(td, ?H:el(button, <<"›"/utf8>>,
                                 [<<"ah-dt-expand-btn">>, [<<"ah-dt-expand-btn-open">> || Open]],
                                 [{type, button}, {tabindex, -1},
                                  {aria_expanded, atom_to_binary(Open, utf8)},
                                  {aria_label, maps:get(details, Texts)},
                                  {aria_controls, <<RowId/binary, "-d">>}]),
                       [<<"ah-dt-cell">>, <<"ah-dt-expand-cell">>], [{role, gridcell}])
                 || Details =/= undefined],
                [?H:el(td, ?H:void(input, [<<"ah-dt-row-checkbox">>],
                                   [{type, checkbox}, {tabindex, -1}, {checked, Selected},
                                    {aria_label, maps:get(select_row, Texts)}]),
                       [<<"ah-dt-cell">>, <<"ah-dt-checkbox-cell">>], [{role, gridcell}])
                 || Mode =:= checkbox],
                [data_cell(dt, C, Row, Editable andalso CE) || #{editable := CE} = C <- Cols]],
               [<<"ah-dt-row">>, [<<"ah-dt-row-alt">> || AltRows andalso Alt],
                [<<"ah-dt-row-hover">> || Hover], [<<"ah-dt-row-selected">> || Selected]],
               [{id, RowId}, {role, row}, {data_key, K}, {data_i, I},
                {aria_selected, Mode =/= none andalso atom_to_binary(Selected, utf8)},
                {tabindex, case maps:get(focus, Cfg) of K -> 0; _ -> -1 end},
                {hidden, not Visible}]),
    case Details of
        undefined -> Tr;
        _ ->
            [Tr,
             ?H:el(tr, ?H:el(td, ?H:el('div', Details(Row), [<<"ah-dt-row-details-content">>], []),
                             [<<"ah-dt-row-details-cell">>], [{colspan, Span}]),
                   [<<"ah-dt-row-details">>, [<<"ah-dt-row-details-hidden">> || not Open]],
                   [{id, <<RowId/binary, "-d">>}, {data_key, K}, {hidden, not Visible}])]
    end.

%% A row's id: the root id and the key, with characters outside
%% [A-Za-z0-9_-] written as _hex.
row_id(Root, K) ->
    Safe = << <<(case C of
                     _ when C >= $a, C =< $z; C >= $A, C =< $Z; C >= $0, C =< $9; C =:= $- ->
                         <<C>>;
                     _ -> iolist_to_binary(io_lib:format("_~2.16.0b", [C]))
                 end)/binary>> || <<C>> <= K >>,
    <<Root/binary, "-r-", Safe/binary>>.

filter_cell(#{field := F, title := Title, filterable := Can, hidden := H}, Filters, Texts) ->
    Cur = case maps:get(F, Filters, <<>>) of
              {_, V} -> V;
              V -> V
          end,
    ?H:el(td,
          [?H:void(input, [<<"ah-dt-filter-input">>],
                   [{data_field, F}, {type, text}, {placeholder, maps:get(filter, Texts)},
                    {value, Cur}, {aria_label, label_text(Title, F)}])
           || Can],
          [<<"ah-dt-filter-cell">>], [{data_field, F}, {hidden, H}]).

adv_filter_cell(#{field := F, title := Title, type := Type, filterable := Can, hidden := H},
                Filters, Texts) ->
    {Cond, Cur} = case maps:get(F, Filters, <<>>) of
                      {C, V} -> {C, V};
                      V -> {contains, V}
                  end,
    Options = case Type of
                  number -> ?NUMBER_CONDITIONS;
                  _ -> ?TEXT_CONDITIONS
              end,
    ?H:el(td,
          [?H:el('div',
                 [?H:el(select,
                        [?H:el(option, maps:get(O, Texts), [],
                               [{value, O}, {selected, O =:= Cond}])
                         || O <- Options],
                        [<<"ah-dt-adv-filter-select">>],
                        [{data_field, F}, {aria_label, label_text(Title, F)}]),
                  ?H:void(input, [<<"ah-dt-adv-filter-input">>],
                          [{data_field, F}, {type, text}, {placeholder, maps:get(value, Texts)},
                           {value, Cur}, {aria_label, label_text(Title, F)},
                           {disabled, Cond =:= empty orelse Cond =:= not_empty}])],
                 [<<"ah-dt-adv-filter-wrap">>], [])
           || Can],
          [<<"ah-dt-filter-cell">>, <<"ah-dt-filter-cell-advanced">>],
          [{data_field, F}, {hidden, H}]).

label_text(Title, F) ->
    case raw_text(Title) of
        not_text -> F;
        T -> T
    end.

%% @doc The view data of templates/datatable_pager.mustache: the range
%% text, prev / next and the page buttons (1-based; the first and last
%% page, the current one and its neighbours, gaps between).
-spec pager_view(pos_integer(), pos_integer(), non_neg_integer(), [pos_integer()],
                 #{atom() => unicode:chardata()}) -> map().
pager_view(Page, PageSize, Total, Sizes, Texts) ->
    Pages = pages(Total, PageSize),
    Start = case Total of 0 -> 0; _ -> (Page - 1) * PageSize + 1 end,
    End = min(Page * PageSize, Total),
    Info = lists:foldl(fun({K, V}, Acc) -> binary:replace(Acc, K, integer_to_binary(V), [global]) end,
                       text(maps:get(info, Texts)),
                       [{<<"{start}">>, Start}, {<<"{end}">>, End}, {<<"{total}">>, Total},
                        {<<"{page}">>, Page}, {<<"{pages}">>, Pages}]),
    #{info => Info,
      prev_label => text(maps:get(prev, Texts)), next_label => text(maps:get(next, Texts)),
      size_label => text(maps:get(page_size, Texts)),
      prev_disabled => Page =< 1, next_disabled => Page >= Pages,
      buttons => [case B of
                      gap -> #{gap => true, page => 0, active => false};
                      N -> #{gap => false, page => N, active => N =:= Page}
                  end || B <- page_buttons(Page, Pages)],
      has_sizes => Sizes =/= [],
      sizes => [#{size => S, selected => S =:= PageSize} || S <- lists:usort([PageSize | Sizes])]}.

page_buttons(_, Pages) when Pages =< 7 -> lists:seq(1, Pages);
page_buttons(Page, Pages) ->
    Middle = lists:seq(max(2, Page - 1), min(Page + 1, Pages - 1)),
    [1] ++ [gap || Page > 3] ++ Middle ++ [gap || Page < Pages - 2] ++ [Pages].

pages(_, undefined) -> 1;
pages(Total, Size) -> max(1, (Total + Size - 1) div Size).

clamp_page(Page, Total, Size) -> max(1, min(Page, pages(Total, Size))).

page_slice(Rows, _, undefined) -> Rows;
page_slice(Rows, Page, Size) ->
    lists:sublist(Rows, (Page - 1) * Size + 1, Size).

%%%===================================================================
%%% Filtering and sorting (the browser's local pipeline, in Erlang)
%%%===================================================================

filters(M) when is_map(M) ->
    maps:from_list(
      [{text(F), case V of
                     {C, X} ->
                         lists:member(C, ?CONDITIONS)
                             orelse error({aihtml, {bad_option, filters, V}}),
                         {C, filter_text(X)};
                     _ -> filter_text(V)
                 end} || {F, V} <- maps:to_list(M)]);
filters(Other) -> error({aihtml, {bad_option, filters, Other}}).

filter_text(V) when is_number(V) -> num(V);
filter_text(V) -> text(V).

%% The text the browser matches on: the raw value (data-value) or the
%% cell's text.
match_text(C, Row) -> string:lowercase(cell_text(value(Row, maps:get(key, C)))).

row_matches(Row, Cols, Search, Filters) ->
    S = string:lowercase(string:trim(Search)),
    (S =:= <<>> orelse lists:any(fun(C) -> contains(match_text(C, Row), S) end, Cols))
        andalso lists:all(fun({F, Flt}) ->
                                  case [C || #{field := CF} = C <- Cols, CF =:= F] of
                                      [C | _] -> filter_matches(C, Row, Flt);
                                      [] -> true
                                  end
                          end, maps:to_list(Filters)).

filter_matches(C, Row, {Cond, _}) when Cond =:= empty; Cond =:= not_empty ->
    Empty = cell_text(value(Row, maps:get(key, C))) =:= <<>>,
    case Cond of empty -> Empty; not_empty -> not Empty end;
filter_matches(_, _, {_, <<>>}) -> true;
filter_matches(C, Row, {Cond, V}) -> condition(Cond, C, Row, V);
filter_matches(_, _, <<>>) -> true;
filter_matches(C, Row, V) -> contains(match_text(C, Row), string:lowercase(V)).

condition(Cond, #{type := Type, key := Key} = C, Row, V) ->
    T = match_text(C, Row),
    F = string:lowercase(V),
    Num = fun() -> {to_num(value(Row, Key)), to_num(V)} end,
    case Cond of
        contains -> contains(T, F);
        not_contains -> not contains(T, F);
        starts_with -> string:prefix(T, F) =/= nomatch;
        ends_with -> byte_size(T) >= byte_size(F) andalso
                         binary:part(T, byte_size(T), -byte_size(F)) =:= F;
        equals when Type =:= number -> case Num() of {A, B} when is_number(A), is_number(B) -> A == B; _ -> false end;
        equals -> T =:= F;
        not_equals when Type =:= number -> case Num() of {A, B} when is_number(A), is_number(B) -> A /= B; _ -> true end;
        not_equals -> T =/= F;
        _ ->
            case Num() of
                {A, B} when is_number(A), is_number(B) ->
                    case Cond of gt -> A > B; gte -> A >= B; lt -> A < B; lte -> A =< B end;
                _ -> false
            end
    end.

contains(Hay, Needle) -> binary:match(Hay, Needle) =/= nomatch.

to_num(V) when is_number(V) -> V;
to_num(V) ->
    T = string:trim(cell_text(V)),
    try binary_to_integer(T)
    catch error:badarg ->
            try binary_to_float(T)
            catch error:badarg -> undefined
            end
    end.

sort_rows(Rows, _, undefined) -> Rows;
sort_rows(Rows, Cols, {F, _} = Sort) ->
    Key = col_key(F, Cols),
    sort_by(Rows, fun({_, _, Row}) -> sort_key(value(Row, Key)) end, Sort).

%% @doc Apply a query (datatable_query/1) to rows in memory: search,
%% column filters and sort as the browser does in local mode, then the
%% page. Returns the page's rows and the number of rows that match.
-spec datatable_page([column()], [row()], query()) -> {[row()], non_neg_integer()}.
datatable_page(Columns, Rows, #{search := Search, filters := Filters, sort := Sort,
                                offset := Offset, limit := Limit}) ->
    Cols = cols(Columns),
    Match = [{0, <<>>, Row} || Row <- Rows, row_matches(Row, Cols, Search, filters(Filters))],
    Sorted = [Row || {_, _, Row} <- sort_rows(Match, Cols, Sort)],
    Page = case Limit of
               undefined -> Sorted;
               _ -> lists:sublist(Sorted, Offset + 1, Limit)
           end,
    {Page, length(Sorted)}.

%%%===================================================================
%%% Remote mode
%%%===================================================================

%% @doc The view state a datatable's 'ah:query' event carries: sort
%% (field and direction, or undefined), page (1-based), page_size,
%% offset and limit, search text and column filters (a text, or
%% `{Condition, Text}' from the advanced filter row).
-spec datatable_query(aihtml_action:event()) -> query().
datatable_query(#{data := Data}) ->
    Get = fun(K) -> case maps:get(K, Data, <<>>) of null -> <<>>; V -> text(V) end end,
    Sort = case {Get(<<"sortField">>), Get(<<"sortDir">>)} of
               {<<>>, _} -> undefined;
               {F, <<"asc">>} -> {F, asc};
               {F, <<"desc">>} -> {F, desc};
               _ -> undefined
           end,
    Int = fun(K, D) ->
                  try binary_to_integer(Get(K)) of
                      N when N > 0 -> N;
                      _ -> D
                  catch error:badarg -> D
                  end
          end,
    PageSize = Int(<<"pageSize">>, undefined),
    Page = Int(<<"page">>, 1),
    Filters = case Get(<<"filters">>) of
                  <<>> -> #{};
                  Json ->
                      try json:decode(Json) of
                          M when is_map(M) -> maps:filtermap(fun query_filter/2, M);
                          _ -> #{}
                      catch error:_ -> #{}
                      end
              end,
    #{sort => Sort, page => Page, page_size => PageSize,
      offset => case PageSize of undefined -> 0; _ -> (Page - 1) * PageSize end,
      limit => PageSize, search => Get(<<"search">>), filters => Filters}.

query_filter(_, V) when is_binary(V) -> V =/= <<>> andalso {true, V};
query_filter(_, #{<<"condition">> := C, <<"value">> := V}) when is_binary(C) ->
    case [A || A <- ?CONDITIONS, atom_to_binary(A, utf8) =:= C] of
        [A] when A =:= empty; A =:= not_empty -> {true, {A, <<>>}};
        [A] when is_binary(V), V =/= <<>> -> {true, {A, V}};
        _ -> false
    end;
query_filter(_, _) -> false.

%% @doc Answer a remote datatable's 'ah:query' (its `source' action):
%% render `Table' (the page's datatable, with its source, the rows of the
%% requested page and `total') in the view state of the event (sort,
%% filters, search, page, page size, selection, hidden columns, open row
%% details) and morph it into the page.
-spec datatable_rows(aihtml_action:ctx(), aihtml_action:event(), #ah_datatable{}) -> ok.
datatable_rows(Ctx, #{id := Id} = Event, #ah_datatable{source = Source} = T) ->
    Source =/= undefined orelse error({aihtml, {datatable_rows_needs_source, T#ah_datatable.id}}),
    #{data := Data} = Event,
    #{sort := Sort, page := Page, page_size := PageSize, search := Search, filters := Filters} =
        datatable_query(Event),
    Hidden = split_list(maps:get(<<"hidden">>, Data, <<>>)),
    Cols = [case col(C) of
                #{field := F} = N -> N#{hidden := lists:member(F, Hidden)}
            end || C <- T#ah_datatable.columns],
    T1 = T#ah_datatable{id = Id,
                        columns = [maps:without([key], C#{field := maps:get(key, C)}) || C <- Cols],
                        sort = Sort, search = Search, filters = Filters,
                        page = Page,
                        page_size = case PageSize of
                                        undefined -> T#ah_datatable.page_size;
                                        _ -> PageSize
                                    end,
                        value = maps:get(value, Event, undefined),
                        expanded = split_list(maps:get(<<"expanded">>, Data, <<>>))},
    aihtml_action:html(Ctx, {id, Id}, T1, morph).

split_list(null) -> [];
split_list(B) -> [K || K <- aihtml_value:split(text(B)), K =/= <<>>].

%% @doc Re-render one row of a datatable (after an edit, say): `Row' as
%% `Table' (the page's datatable) renders it, morphed over the row with
%% the same key, then the behaviour method `refresh' restores selection,
%% stripes and the local view. The table's id comes from `Table' or, when
%% it has none, from the event of an 'ah:cell-edit' (`data.table').
-spec datatable_row(aihtml_action:ctx(), aihtml_action:event(), #ah_datatable{}, row()) -> ok.
datatable_row(Ctx, Event, #ah_datatable{} = T, Row) ->
    Root = case T#ah_datatable.id of
               undefined -> text(maps:get(<<"table">>, maps:get(data, Event)));
               Id0 -> text(Id0)
           end,
    Cols = cols(T#ah_datatable.columns),
    Key = case value(Row, T#ah_datatable.key_field) of
              undefined -> error({aihtml, {row_without_key, Row}});
              K -> key_text(K)
          end,
    Details = T#ah_datatable.row_details,
    Cfg = #{root => Root, cols => Cols, mode => T#ah_datatable.selection_mode, sel => [],
            editable => T#ah_datatable.editable, details => Details, expanded => [],
            span => length([C || #{hidden := false} = C <- Cols])
                + case Details of undefined -> 0; _ -> 1 end
                + case T#ah_datatable.selection_mode of checkbox -> 1; _ -> 0 end,
            alt => false, hover => T#ah_datatable.hover, focus => undefined,
            texts => texts(T#ah_datatable.texts)},
    RowId = row_id(Root, Key),
    case dt_row(0, Key, Row, true, false, Cfg) of
        [Tr, Dr] ->
            aihtml_action:html(Ctx, {id, RowId}, Tr, morph),
            aihtml_action:html(Ctx, {id, <<RowId/binary, "-d">>}, Dr, morph);
        Tr ->
            aihtml_action:html(Ctx, {id, RowId}, Tr, morph)
    end,
    aihtml_action:call(Ctx, {id, Root}, refresh, []).

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => datatable, category => data,
       signature => <<"datatable(Columns, Rows, Css, Attrs)">>,
       root => <<"ah-dt">>, flags => [disabled],
       options => [value, selection_mode, key_field, sortable, sort, filter, filters, search,
                   page_size, page, page_sizes, total, source, editable, edit, row_details,
                   expanded, resizable, column_chooser, alt_rows, hover, show_header, height,
                   empty_text, texts],
       behavior => <<"datatable">>,
       events => [<<"change">>, <<"ah:sort">>, <<"ah:filter">>, <<"ah:page">>, <<"ah:query">>,
                  <<"ah:row-click">>, <<"ah:row-dblclick">>, <<"ah:row-expand">>,
                  <<"ah:row-collapse">>, <<"ah:cell-edit">>, <<"ah:column-resize">>,
                  <<"ah:columns">>],
       doc => <<"A data table: sorting, a filter row, search or advanced filters, paging, "
                "row selection, row details, inline editing, column resize and chooser; "
                "local (the browser does the work) or remote (the server renders each page).">>,
       option_docs =>
           maps:merge(aihtml_lib_table:option_docs(),
                      #{filter => <<"none (default), row (a text filter per column), search "
                                    "(one search field) or advanced (condition and value per "
                                    "column).">>,
                        filters => <<"Initial column filters: #{Field => Text | {Condition, Text}}.">>,
                        search => <<"Initial search text.">>,
                        page_size => <<"Rows per page; without it the table does not page.">>,
                        page => <<"The current page, 1-based (default 1).">>,
                        page_sizes => <<"Choices of the page size select (default [5, 10, 25, 50]; "
                                        "[] hides it).">>,
                        total => <<"Remote mode: the number of rows of the whole result.">>,
                        source => <<"Action ref {Module, Action, Args}: remote mode. Every view "
                                    "change runs it ('ah:query'); it answers with "
                                    "datatable_rows/3.">>,
                        editable => <<"Double click (or Enter / F2) edits a cell; a column may "
                                      "say editable => false.">>,
                        edit => <<"Action ref run when an edit is committed ('ah:cell-edit' with "
                                  "key, field, value, old); it may answer with datatable_row/4.">>,
                        row_details => <<"fun(Row) -> Html: a detail row under each row, opened "
                                         "with an arrow.">>,
                        expanded => <<"Keys of the rows whose details are open at first.">>,
                        column_chooser => <<"A button that shows and hides columns.">>,
                        texts => <<"Labels: filter, search, value, info (\"{start}-{end} of "
                                   "{total}\"), columns, prev, next, page_size, select_all, "
                                   "select_row, details and the condition names.">>}),
       methods =>
           [#{name => getValue, args => <<"()">>, doc => <<"Return the selected keys, comma separated (a comma inside a key is escaped as \\,; aihtml_value:split/1 reads it).">>},
            #{name => setValue, args => <<"(Keys)">>,
              doc => <<"Select these keys (an array or comma separated text, see aihtml_value) without firing change.">>},
            #{name => clearSelection, args => <<"()">>, doc => <<"Select nothing (fires no change).">>},
            #{name => sort, args => <<"(Field, Dir)">>,
              doc => <<"Sort by a column: \"asc\", \"desc\" or null.">>},
            #{name => goToPage, args => <<"(Page)">>, doc => <<"Show a page (1-based).">>},
            #{name => setPageSize, args => <<"(Size)">>, doc => <<"Change the page size.">>},
            #{name => setSearch, args => <<"(Text)">>, doc => <<"Set the search text.">>},
            #{name => clearFilters, args => <<"()">>, doc => <<"Clear the search and every column filter.">>},
            #{name => showColumn, args => <<"(Field)">>, doc => <<"Show a hidden column.">>},
            #{name => hideColumn, args => <<"(Field)">>, doc => <<"Hide a column.">>},
            #{name => expandRow, args => <<"(Key)">>, doc => <<"Open the details of a row.">>},
            #{name => collapseRow, args => <<"(Key)">>, doc => <<"Close the details of a row.">>},
            #{name => refresh, args => <<"()">>,
              doc => <<"Re-apply the view to the rows (after the server replaced some).">>}]}].

%%%===================================================================
%%% Internal
%%%===================================================================

default_texts() ->
    #{filter => <<"Filter...">>, search => <<"Search...">>, value => <<"Value...">>,
      info => <<"{start}-{end} of {total}">>, columns => <<"Columns">>,
      prev => <<"Previous page">>, next => <<"Next page">>, page_size => <<"Rows per page">>,
      select_all => <<"Select all rows">>, select_row => <<"Select row">>,
      details => <<"Details">>,
      contains => <<"Contains">>, not_contains => <<"Not Contains">>, equals => <<"Equals">>,
      not_equals => <<"Not Equals">>, starts_with => <<"Starts With">>,
      ends_with => <<"Ends With">>, gt => <<"Greater Than">>, gte => <<"Greater or Equal">>,
      lt => <<"Less Than">>, lte => <<"Less or Equal">>, empty => <<"Empty">>,
      not_empty => <<"Not Empty">>}.

texts(M) ->
    is_map(M) orelse error({aihtml, {bad_option, texts, M}}),
    D = default_texts(),
    [maps:is_key(K, D) orelse error({aihtml, {bad_option, texts, K}}) || K <- maps:keys(M)],
    maps:map(fun(_, V) -> text(V) end, maps:merge(D, M)).
