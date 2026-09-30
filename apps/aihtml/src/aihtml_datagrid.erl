%%%-------------------------------------------------------------------
%%% @doc The data grid, ported from sigil (data/datagrid). DOM and class
%%% names are sigil's, so the styles in priv/css/sigil/components/datagrid
%%% apply unchanged.
%%%
%%%   ah_datagrid(Columns, Rows, Css, Attrs)     the grid
%%%   datagrid_query(Event)                      (remote mode) the view the browser asks for
%%%   datagrid_rows(Ctx, Event, Rows, Total)     (remote mode) answer it
%%%   datagrid_row(Ctx, Event, Row)              re-render one row (after an edit)
%%%   datagrid_select(Query, Rows)               apply a query to rows in memory
%%%
%%% The grid is value-bearing: the root carries `data-ah-value' (the
%%% selected row keys, comma separated by aihtml_value) and fires `change' when the user
%%% changes the selection; a `name' in Attrs goes to a hidden input.
%%%
%%% == Events ==
%%%
%%% Besides `change', the root fires component events that pages bind
%%% with `aihtml:on/2' in Attrs. The root's data-* attributes arrive in
%%% `Event.data': the view state (`sort', a JSON list of [field, "asc" |
%%% "desc"]; `filter', a JSON object field -> text; `page'; `pageSize';
%%% `groupBy', a JSON list) and the details of the event:
%%%
%%%   ah:row-click, ah:row-dblclick   key, field
%%%   ah:edit                         key, field, value (the new text), old
%%%   ah:command                      key, field, name (a command button)
%%%   ah:toolbar                      name (a custom toolbar button)
%%%   ah:sort, ah:filter, ah:page     (the view state above)
%%%   ah:group-toggle                 value (the group id), expanded
%%%   ah:column-resize                field, value (the width in px)
%%%
%%% `render' holds a signed token of the grid's column set; datagrid_row/3
%%% and the remote helpers read it, so they render rows exactly like the
%%% first render did.
%%%
%%% == Local and remote mode ==
%%%
%%% Local mode (no `source'): all rows are rendered into the page and the
%%% browser sorts, filters, pages and groups them, computing aggregates on
%%% the fly. The initial `sort', `filters', `page' and `group_by' are
%%% applied here, so the first paint is already the final view.
%%%
%%% Remote mode (`{source, {Mod, Action, Args}}'): the page holds one page
%%% of rows, and the server always renders it: `Rows' are the page shown
%%% first (`page', `sort', `filters' say which) and `total' the number of
%%% matching rows (default: the number of `Rows'); no rows render the
%%% empty message. The grid does not ask for rows when it mounts, so the
%%% first page is in the HTML for crawlers and for the first paint; a page
%%% that wants a client-side first load calls the `refresh' method
%%% (`aihtml_action:call(Ctx, {id, Id}, refresh, [])' or `AH.invoke').
%%% Every view change (sort, filter row, search box, page, page size)
%%% POSTs the source action from a hidden element of the grid, with the
%%% view state in `Event.data' (sort, filter, search, page, pageSize,
%%% export, grid, render). The action reads it with `datagrid_query/1'
%%% and answers with `datagrid_rows(Ctx, Event, Rows, Total)', which
%%% renders the rows and the pager here and morphs them in.
%%%
%%% == Crawlable pages ==
%%%
%%% With `href' (a URL template) the pager's buttons are links: `{page}',
%%% `{size}', `{sort}' (`field:asc,field:desc') and `{search}' are filled
%%% in from the view, URL-encoded. The page serving that URL renders the
%%% grid in that state (reading the query into `page', `page_size',
%%% `sort' and the rows), so a crawler or a click without JavaScript gets
%%% that page from the server. With JavaScript a plain click stays in the
%%% page (the local re-page or the remote query as without links) and
%%% pushes the link's URL to the history; going back reloads it.
%%%
%%% Export in remote mode asks the same action with `export' set and
%%% `limit' infinity; datagrid_rows/4 then sends the formatted rows to the
%%% browser, which writes the file. Grouping and the status bar are local
%%% mode features.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_datagrid).
-behaviour(aihtml_element).

-include("aihtml_datagrid.hrl").

-compile({parse_transform, beamai_mustache_transform}).
-mustache_template({tpl_datagrid_pager, "../templates/datagrid_pager.mustache"}).
-mustache_template({tpl_datagrid_group_row, "../templates/datagrid_group_row.mustache"}).
-mustache_template({tpl_datagrid_column_menu, "../templates/datagrid_column_menu.mustache"}).

-export([ah_datagrid/4, datagrid_query/1, datagrid_rows/4, datagrid_row/3, datagrid_select/2,
         render/1, fields/1, catalog/0, facade_extras/0]).
%% The formatting and aggregation model, shared with the browser; for tests.
-export([format_value/3, aggregate/2, pager_view/4, default_labels/0]).

-export_type([element/0, column/0, row/0, query/0, key/0, cell_type/0, aggregate/0, tool/0]).

-define(H, aihtml_html).
-define(E, aihtml_element).
-define(CHECKBOX, <<"__checkbox">>).
-define(CHECKBOX_WIDTH, 40).
-define(GROUP_INDENT, 20).
-define(PAGER_BUTTONS, 7).

-type html() :: aihtml_html:html().
-type css() :: aihtml_html:css().
-type attrs() :: aihtml_html:attrs().
-type element() :: #ah_datagrid{}.
%% A column key: the key of the cell's value in each row map (an atom
%% also finds a binary key of the same name, and the other way round).
-type key() :: atom() | binary().
%% How a cell shows its value. text (default), number, date, bool (the
%% yes / no labels), select (the label of the matching option), textarea
%% are plain text and can be edited; progress, rating, image, link, badge
%% and command are display types.
-type cell_type() :: text | number | date | bool | select | textarea
                   | progress | rating | image | link | badge | command.
%% An aggregate of a column's numeric values (status bar, group rows).
-type aggregate() :: sum | avg | count | min | max.
%% A column: a key, `{Key, Title}', or a map. Map keys (all optional but
%% `key'): title, width (px, default 100), min_width (default 40), align
%% (left | center | right), type, format ("n2" number, "c2" currency,
%% "p1" percentage, "yyyy-MM-dd HH:mm" date), currency (the symbol of
%% "c" formats, default "¥"), sortable, filterable, resizable, groupable
%% (default true), editable, pinned, hidden (default false), options (of
%% a select column: values or {Value, Label}), aggregates, badges (badge
%% columns: #{Value => {Text, success | warning | danger | info |
%% default}}), max (progress: 100, rating: 5), link_text, target (link
%% columns, default "_blank"), commands (command columns: [{Name,
%% Label}], a click fires 'ah:command'), render ({Mod, Fun}: the cell
%% content is Mod:Fun(Value, Row), any html).
-type column() :: key()
                | {key(), unicode:chardata()}
                | #{key := key(), title => unicode:chardata(),
                    width => pos_integer(), min_width => pos_integer(),
                    align => left | center | right, type => cell_type(),
                    format => unicode:chardata(), currency => unicode:chardata(),
                    sortable => boolean(), filterable => boolean(),
                    resizable => boolean(), groupable => boolean(),
                    editable => boolean(), pinned => boolean(), hidden => boolean(),
                    options => [term() | {term(), unicode:chardata()}],
                    aggregates => [aggregate()],
                    badges => #{term() => {unicode:chardata(), atom()}},
                    max => number(), link_text => unicode:chardata(),
                    target => unicode:chardata(),
                    commands => [{atom() | binary(), unicode:chardata()}],
                    render => {module(), atom()}}.
%% A row: a map from column keys to values; the `key_field' value
%% identifies it (selection, edits, row ids).
-type row() :: #{term() => term()}.
%% A toolbar entry: export buttons (the export runs in the browser),
%% a search box over all columns, a separator, a flexible spacer, or a
%% custom button (`{Name, Label}' or a map) whose click fires
%% 'ah:toolbar' with its name.
-type tool() :: export_csv | export_xlsx | export_pdf | search | separator | spacer
              | {atom() | binary(), unicode:chardata()}
              | #{name := atom() | binary(), label := unicode:chardata(),
                  icon => unicode:chardata()}.
%% What the browser asks for in remote mode (see datagrid_query/1). The
%% keys in `sort' and `filters' are the columns' own keys; fields the grid
%% does not have are dropped, so they are safe to map to database columns.
-type query() :: #{sort := [{key(), asc | desc}],
                   filters := [{key(), binary()}],
                   search := binary(),
                   page := pos_integer(),
                   page_size := pos_integer(),
                   offset := non_neg_integer(),
                   limit := pos_integer() | infinity,
                   export := undefined | csv | xlsx | pdf}.

%%%===================================================================
%%% Builder
%%%===================================================================

%% @doc A data grid. `Columns' are keys, `{Key, Title}' or maps (see the
%% type `column()'); `Rows' are maps.
%% Css: `single' (default) | `multi' | `checkbox' | `none' (selection),
%% `filter_row', `pageable', `statusbar'. Options: `value' (selected row
%% keys), `key_field' (default id), `height', `page', `page_size',
%% `page_sizes', `sort', `filters', `group_by', `edit_mode', `column_menu',
%% `toolbar', `export_name', `labels', `source', `total', `href'; see
%% catalog/0.
-spec ah_datagrid([column()], [row()], css(), attrs()) -> #ah_datagrid{}.
ah_datagrid(Columns, Rows, Css, Attrs) ->
    ?E:build(?MODULE, #ah_datagrid{columns = Columns, rows = Rows}, Css, Attrs).

%% @doc The field names of this component's record.
-spec fields(atom()) -> [atom()].
fields(ah_datagrid) -> record_info(fields, ah_datagrid).

%% @doc Functions besides the components that the aihtml facade re-exports.
-spec facade_extras() -> [{atom(), arity()}].
facade_extras() ->
    [{datagrid_query, 1}, {datagrid_rows, 4}, {datagrid_row, 3}, {datagrid_select, 2}].

%%%===================================================================
%%% Rendering
%%%===================================================================

-spec render(element()) -> html().
render(#ah_datagrid{} = R0) ->
    check(R0),
    {Id, R} = ensure_id(R0),
    Classes = ?E:classes(?MODULE, R),
    #ah_datagrid{selection = Sel, filter_row = FilterRow, pageable = Pageable,
                 source = Source, column_menu = Menu, toolbar = Toolbar,
                 name = Name, height = Height} = R,
    Remote = Source =/= undefined,
    Cfg = cfg_of(Id, R),
    #{cols := Cols, labels := L} = Cfg,
    Fields = [F || #{field := F} <- Cols],
    Sort = [{F, D} || {K, D} <- R#ah_datagrid.sort, F <- [text(K)], lists:member(F, Fields)],
    Filters = [{F, text(V)} || {K, V} <- filters_list(R#ah_datagrid.filters),
                               F <- [text(K)], lists:member(F, Fields), text(V) =/= <<>>],
    GroupBy = case Remote of
                  true -> [];
                  false -> [F || K <- R#ah_datagrid.group_by, F <- [text(K)],
                                 lists:member(F, Fields)]
              end,
    Selected = [text(K) || K <- R#ah_datagrid.value],
    HeaderRows = 1 + bool_int(FilterRow),
    PageSize = R#ah_datagrid.page_size,
    Recs0 = [rec(Row, Cfg) || Row <- R#ah_datagrid.rows],
    Reordered = not Remote andalso (Sort =/= [] orelse GroupBy =/= [] orelse Filters =/= []),
    Recs = case Reordered of
               true -> [Rc#{index => I} || {I, Rc} <- lists:zip(lists:seq(0, length(Recs0) - 1), Recs0)];
               false -> Recs0
           end,
    {Items, Page, Total, Aggs} =
        case Remote of
            true ->
                T = case R#ah_datagrid.total of
                        undefined -> length(Recs);
                        T0 -> T0
                    end,
                {[{row, Rc, true} || Rc <- Recs], R#ah_datagrid.page, T, #{}};
            false ->
                local_view(Recs, Cols, Sort, Filters, GroupBy, Pageable,
                           R#ah_datagrid.page, PageSize)
        end,
    Offset = case Pageable of
                 true -> (Page - 1) * PageSize;
                 false -> 0
             end,
    Empty = not lists:any(fun({_, _, On}) -> On end, Items),
    Body = ?H:el('div', [body_items(Items, Cfg, Selected, HeaderRows + Offset),
                         empty_message(L, Empty)],
                 [<<"ah-dg-body">>, [<<"ah-dg-body-empty">> || Empty]],
                 [{id, sub_id(Id, <<"body">>)}, {role, rowgroup}]),
    Header = ?H:el('div',
                   [?H:el('div', [header_cell(C, Sort, Menu, Selected, Recs, L) || C <- Cols],
                          [<<"ah-dg-header-row">>], [{role, row}, {aria_rowindex, 1}]),
                    case FilterRow of
                        true -> filter_row(Cols, Filters, L);
                        false -> []
                    end],
                   [<<"ah-dg-header">>], [{role, rowgroup}]),
    Container =
        ?H:el('div',
              [case Toolbar of
                   [] -> [];
                   _ -> ?H:el('div', toolbar(Toolbar, L), [<<"ah-dg-toolbar-wrap">>], [])
               end,
               ?H:el('div', Header, [<<"ah-dg-header-wrap">>], []),
               ?H:el('div', Body, [<<"ah-dg-body-wrap">>], []),
               case R#ah_datagrid.statusbar andalso not Remote of
                   true -> ?H:el('div', statusbar(Cols, Aggs, L), [<<"ah-dg-statusbar-wrap">>], []);
                   false -> []
               end,
               case Pageable of
                   true -> ?H:el('div', pager(Page, PageSize, Total, Cfg,
                                             link_template(Cfg, PageSize, Sort, <<>>)),
                                 [<<"ah-dg-pager-wrap">>], [{id, sub_id(Id, <<"pager">>)}]);
                   false -> []
               end],
              [<<"ah-dg-container">>], []),
    Rows = Total + HeaderRows,
    ?H:el('div',
          [Container,
           case Menu of
               true -> ?H:el('div', [], [<<"ah-dg-column-menu">>],
                             [{id, sub_id(Id, <<"menu">>)}, {role, menu}]);
               false -> []
           end,
           ?H:el('div', [], [<<"ah-dg-resize-line">>], [{aria_hidden, <<"true">>}]),
           ?H:el('div', ?H:el('div', maps:get(loading, L), [<<"ah-dg-loading-message">>], []),
                 [<<"ah-dg-loading-overlay">>], [{style, <<"display:none;">>}]),
           case Source of
               undefined -> [];
               _ -> ?H:el('div', [], [<<"ah-dg-query">>],
                          [[{id, sub_id(Id, <<"q">>)}, {hidden, true}, {data_grid, Id},
                            {data_ah_sync, <<"replace">>}],
                           aihtml:on('ah:query', Source)])
           end,
           hidden_input(Name, join(Selected))],
          Classes,
          [[{id, Id}, {role, grid},
            {aria_multiselectable, (Sel =:= multi orelse Sel =:= checkbox) andalso <<"true">>},
            {aria_rowcount, Rows}, {aria_colcount, length(Cols)},
            {style, case Height of
                        undefined -> undefined;
                        _ when is_integer(Height) -> [<<"height:">>, integer_to_binary(Height), <<"px;">>];
                        _ -> [<<"height:">>, Height, <<";">>]
                    end},
            {data_ah, <<"datagrid">>}, {data_ah_value, join(Selected)},
            {data_ah_selection, Sel},
            {data_ah_edit_mode, R#ah_datagrid.edit_mode},
            {data_ah_remote, Remote},
            {data_ah_pageable, Pageable},
            {data_ah_header_rows, HeaderRows},
            {data_ah_export_name, text(R#ah_datagrid.export_name)},
            {data_ah_labels, json(L)},
            {data_ah_loaded, Remote andalso <<"true">>},
            {data_ah_href, maps:get(href, Cfg, undefined)},
            {data_sort, json([[F, D] || {F, D} <- Sort])},
            {data_filter, json(maps:from_list(Filters))},
            {data_page, Page},
            {data_page_size, PageSize},
            {data_group_by, json(GroupBy)},
            {data_render, aihtml_action:sign(maps:get(spec, Cfg))}],
           ?E:root_attrs(R, change)]).

check(#ah_datagrid{} = R) ->
    lists:member(R#ah_datagrid.edit_mode, [dblclick, click])
        orelse bad(edit_mode, R#ah_datagrid.edit_mode),
    pos_int(R#ah_datagrid.page) orelse bad(page, R#ah_datagrid.page),
    pos_int(R#ah_datagrid.page_size) orelse bad(page_size, R#ah_datagrid.page_size),
    is_list(R#ah_datagrid.page_sizes) andalso lists:all(fun pos_int/1, R#ah_datagrid.page_sizes)
        orelse bad(page_sizes, R#ah_datagrid.page_sizes),
    is_list(R#ah_datagrid.sort) andalso
        lists:all(fun({_, D}) -> D =:= asc orelse D =:= desc; (_) -> false end,
                  R#ah_datagrid.sort)
        orelse bad(sort, R#ah_datagrid.sort),
    is_list(R#ah_datagrid.filters) orelse is_map(R#ah_datagrid.filters)
        orelse bad(filters, R#ah_datagrid.filters),
    is_list(R#ah_datagrid.group_by) orelse bad(group_by, R#ah_datagrid.group_by),
    is_list(R#ah_datagrid.value) orelse bad(value, R#ah_datagrid.value),
    is_boolean(R#ah_datagrid.column_menu) orelse bad(column_menu, R#ah_datagrid.column_menu),
    is_list(R#ah_datagrid.toolbar) orelse bad(toolbar, R#ah_datagrid.toolbar),
    is_map(R#ah_datagrid.labels) orelse bad(labels, R#ah_datagrid.labels),
    case R#ah_datagrid.source of
        undefined -> ok;
        {M, A, _} when is_atom(M), is_atom(A) -> ok;
        S -> bad(source, S)
    end,
    case R#ah_datagrid.total of
        undefined -> ok;
        T when is_integer(T), T >= 0 -> ok;
        T -> bad(total, T)
    end,
    case R#ah_datagrid.href of
        undefined -> ok;
        U when is_binary(U); is_list(U) -> ok;
        U -> bad(href, U)
    end,
    case R#ah_datagrid.height of
        undefined -> ok;
        Hh when is_integer(Hh), Hh > 0 -> ok;
        Hh when is_binary(Hh); is_list(Hh) -> ok;
        Hh -> bad(height, Hh)
    end,
    is_list(R#ah_datagrid.columns) orelse bad(columns, R#ah_datagrid.columns),
    is_list(R#ah_datagrid.rows) orelse bad(rows, R#ah_datagrid.rows),
    ok.

-spec bad(atom(), term()) -> no_return().
bad(K, V) -> error({aihtml, {bad_option, K, V}}).

pos_int(N) -> is_integer(N) andalso N > 0.

filters_list(M) when is_map(M) -> lists:sort(maps:to_list(M));
filters_list(L) -> L.

%%%===================================================================
%%% Columns and the render config
%%%===================================================================

%% The render config: what rendering a row needs. Its source (`spec')
%% goes into the page signed (data-render), so the action helpers render
%% rows like this render did.
cfg_of(Id, #ah_datagrid{columns = Columns, selection = Sel, key_field = KeyField,
                        labels = Labels, page_sizes = Sizes, page_size = PageSize,
                        pageable = Pageable, href = Href}) ->
    Spec = #{id => Id, columns => Columns, key_field => KeyField, selection => Sel,
             labels => Labels, page_sizes => lists:usort([PageSize | Sizes]),
             page_size => PageSize, pageable => Pageable},
    cfg_from(case Href of
                 undefined -> Spec;
                 _ -> Spec#{href => text(Href)}
             end).

cfg_from(#{columns := Columns, selection := Sel, labels := Labels} = Spec) ->
    Cols0 = [col(C) || C <- Columns],
    Keys = [F || #{field := F} <- Cols0],
    case Keys -- lists:usort(Keys) of
        [] -> ok;
        Dups -> error({aihtml, {duplicate_column, hd(Dups)}})
    end,
    AnyPinned = lists:any(fun(#{pinned := P}) -> P end, Cols0),
    Cols1 = case Sel of
                checkbox -> [checkbox_col(AnyPinned) | Cols0];
                _ -> Cols0
            end,
    Spec#{cols => pin_offsets(Cols1), labels := labels(Labels), spec => Spec}.

checkbox_col(Pinned) ->
    #{key => ?CHECKBOX, field => ?CHECKBOX, title => <<>>, width => ?CHECKBOX_WIDTH,
      min_width => ?CHECKBOX_WIDTH, align => center, type => checkbox, format => undefined,
      currency => <<>>, sortable => false, filterable => false, resizable => false,
      groupable => false, editable => false, pinned => Pinned, hidden => false,
      options => [], aggregates => [], badges => #{}, max => undefined,
      link_text => undefined, target => <<"_blank">>, commands => [], render => undefined}.

-define(TYPES, [text, number, date, bool, select, textarea,
                progress, rating, image, link, badge, command]).
-define(EDITABLE_TYPES, [text, number, date, bool, select, textarea]).
-define(AGGS, [sum, avg, count, min, max]).
-define(COL_KEYS, [key, title, width, min_width, align, type, format, currency, sortable,
                   filterable, resizable, groupable, editable, pinned, hidden, options,
                   aggregates, badges, max, link_text, target, commands, render]).

col(#{key := K} = M) ->
    (is_atom(K) orelse is_binary(K)) andalso K =/= undefined
        orelse error({aihtml, {bad_column, M}}),
    [error({aihtml, {unknown_column_key, X}}) || X <- maps:keys(M), not lists:member(X, ?COL_KEYS)],
    Field = text(K),
    Field =:= ?CHECKBOX andalso error({aihtml, {bad_column, M}}),
    Type = maps:get(type, M, text),
    lists:member(Type, ?TYPES) orelse error({aihtml, {bad_column_type, Field, Type}}),
    Align = maps:get(align, M, left),
    lists:member(Align, [left, center, right]) orelse error({aihtml, {bad_column_align, Field, Align}}),
    Width = maps:get(width, M, 100),
    pos_int(Width) orelse error({aihtml, {bad_column_width, Field, Width}}),
    MinWidth = maps:get(min_width, M, min(40, Width)),
    pos_int(MinWidth) orelse error({aihtml, {bad_column_width, Field, MinWidth}}),
    Aggs = maps:get(aggregates, M, []),
    is_list(Aggs) andalso lists:all(fun(A) -> lists:member(A, ?AGGS) end, Aggs)
        orelse error({aihtml, {bad_column_aggregates, Field, Aggs}}),
    Editable = cbool(Field, editable, maps:get(editable, M, false)),
    Editable andalso not lists:member(Type, ?EDITABLE_TYPES)
        andalso error({aihtml, {not_editable_type, Field, Type}}),
    Render = case maps:get(render, M, undefined) of
                 undefined -> undefined;
                 {RM, RF} = RR when is_atom(RM), is_atom(RF) -> RR;
                 Other -> error({aihtml, {bad_column_render, Field, Other}})
             end,
    Format = case maps:get(format, M, undefined) of
                 undefined -> undefined;
                 F -> text(F)
             end,
    Options = [case O of
                   {V, Lb} -> {text(V), text(Lb)};
                   V -> {text(V), text(V)}
               end || O <- maps:get(options, M, [])],
    Commands = [case C of
                    {N, Lb} -> {text(N), text(Lb)};
                    _ -> error({aihtml, {bad_column_command, Field, C}})
                end || C <- maps:get(commands, M, [])],
    Badges = maps:get(badges, M, #{}),
    is_map(Badges) orelse error({aihtml, {bad_column_badges, Field, Badges}}),
    #{key => K, field => Field,
      title => title_text(maps:get(title, M, Field)),
      width => Width, min_width => MinWidth, align => Align, type => Type,
      format => Format, currency => text(maps:get(currency, M, <<"¥"/utf8>>)),
      sortable => cbool(Field, sortable, maps:get(sortable, M, Type =/= command)),
      filterable => cbool(Field, filterable, maps:get(filterable, M, Type =/= command)),
      resizable => cbool(Field, resizable, maps:get(resizable, M, true)),
      groupable => cbool(Field, groupable, maps:get(groupable, M, Type =/= command)),
      editable => Editable,
      pinned => cbool(Field, pinned, maps:get(pinned, M, false)),
      hidden => cbool(Field, hidden, maps:get(hidden, M, false)),
      options => Options, aggregates => Aggs,
      badges => maps:from_list([{text(V), badge(Field, B)} || V := B <- Badges]),
      max => case maps:get(max, M, undefined) of
                 undefined when Type =:= rating -> 5;
                 undefined when Type =:= progress -> 100;
                 Mx when is_number(Mx), Mx > 0 -> Mx;
                 undefined -> undefined;
                 Mx -> error({aihtml, {bad_column_max, Field, Mx}})
             end,
      link_text => case maps:get(link_text, M, undefined) of
                       undefined -> undefined;
                       LT -> text(LT)
                   end,
      target => text(maps:get(target, M, <<"_blank">>)),
      commands => Commands, render => Render};
col({K, Title}) -> col(#{key => K, title => Title});
col(K) when is_atom(K); is_binary(K) -> col(#{key => K});
col(Other) -> error({aihtml, {bad_column, Other}}).

cbool(_, _, B) when is_boolean(B) -> B;
cbool(Field, K, V) -> error({aihtml, {bad_column_option, Field, K, V}}).

badge(_, {T, C}) when is_atom(C) ->
    lists:member(C, [success, warning, danger, info, default])
        orelse error({aihtml, {bad_badge_class, C}}),
    {text(T), atom_to_binary(C)};
badge(Field, B) -> error({aihtml, {bad_column_badges, Field, B}}).

%% Titles are text: they are signed into the page and used as export headers.
title_text(T) when is_binary(T); is_list(T); is_atom(T); is_integer(T) -> text(T);
title_text(T) -> error({aihtml, {bad_column_title, T}}).

%% Sticky left offsets of the pinned (visible) columns, in order.
pin_offsets(Cols) ->
    {Out, _} = lists:mapfoldl(
                 fun(#{pinned := true, hidden := false, width := W} = C, Acc) ->
                         {C#{left => Acc}, Acc + W};
                    (C, Acc) -> {C#{left => undefined}, Acc}
                 end, 0, Cols),
    Last = case [F || #{left := Lf, field := F} <- Out, Lf =/= undefined] of
               [] -> undefined;
               Ps -> lists:last(Ps)
           end,
    [C#{last_pinned => F =:= Last} || #{field := F} = C <- Out].

%%%===================================================================
%%% Labels
%%%===================================================================

%% @doc The texts of the grid in the current language (aihtml_i18n, scope
%% datagrid); `labels' in Attrs overrides any of them. `total' and
%% `per_page' take the number for {0}.
-spec default_labels() -> #{atom() => binary()}.
default_labels() ->
    aihtml_i18n:texts(datagrid).

labels(Over) ->
    D = default_labels(),
    maps:fold(fun(K, V, Acc) ->
                      is_map_key(K, D) orelse error({aihtml, {unknown_label, K}}),
                      Acc#{K => text(V)}
              end, D, Over).

subst(Pattern, N) -> binary:replace(Pattern, <<"{0}">>, text(N), [global]).

%%%===================================================================
%%% Rows: values, texts and the local view
%%%===================================================================

%% A row with its key and, per column, {Raw, Text}: the raw text is what
%% the browser sorts, filters and edits (data-v), the text what it shows.
rec(Row, #{cols := Cols, key_field := KeyField, labels := L}) ->
    is_map(Row) orelse error({aihtml, {bad_row, Row}}),
    Key = case value(Row, KeyField) of
              undefined -> error({aihtml, {row_without_key, KeyField, Row}});
              K -> text(K)
          end,
    #{row => Row, key => Key,
      cells => maps:from_list([{F, cell_texts(C, value(Row, K), L)}
                               || #{field := F, key := K} = C <- Cols, F =/= ?CHECKBOX])}.

%% The value of a row under a key: an atom key also finds a binary key of
%% the same name, and a binary key an existing atom.
value(Row, K) ->
    case Row of
        #{K := V} -> V;
        _ when is_atom(K) -> maps:get(atom_to_binary(K), Row, undefined);
        _ when is_binary(K) ->
            try binary_to_existing_atom(K) of
                A -> maps:get(A, Row, undefined)
            catch error:badarg -> undefined
            end;
        _ -> undefined
    end.

raw(undefined) -> <<>>;
raw(null) -> <<>>;
raw(true) -> <<"true">>;
raw(false) -> <<"false">>;
raw(V) when is_float(V) -> num_text(V);
raw({{Y, M, D}, {H, Mi, S}}) -> iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B",
                                                               [Y, M, D, H, Mi, trunc(S)]));
raw({Y, M, D}) when is_integer(Y), is_integer(M), is_integer(D) ->
    iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0B", [Y, M, D]));
raw(V) -> text(V).

cell_texts(#{type := Type} = C, V, L) ->
    Raw = raw(V),
    Text = case Type of
               _ when V =:= undefined; V =:= null -> <<>>;
               bool -> bool_text(V, L);
               select -> proplists:get_value(Raw, maps:get(options, C), Raw);
               progress -> <<>>;
               image -> <<>>;
               rating -> stars(V, C);
               link -> case maps:get(link_text, C) of undefined -> Raw; LT -> LT end;
               badge -> element(1, maps:get(Raw, maps:get(badges, C), {Raw, x}));
               command -> <<>>;
               _ -> format_value(V, C, L)
           end,
    {Raw, Text}.

bool_text(V, L) when V =:= true; V =:= <<"true">> -> maps:get(yes, L);
bool_text(V, L) when V =:= false; V =:= <<"false">> -> maps:get(no, L);
bool_text(V, _) -> raw(V).

stars(V, #{max := Max}) ->
    N = case to_number(raw(V)) of
            undefined -> 0;
            X -> max(0, min(trunc(Max), trunc(X)))
        end,
    unicode:characters_to_binary([lists:duplicate(N, $★), lists:duplicate(trunc(Max) - N, $☆)]).

%% Initial local view: filter, aggregate, sort, group, page. Returns the
%% body items in DOM order ({row | group, _, InPage}), the clamped page,
%% the number of pageable items and the status bar aggregates.
local_view(Recs, Cols, Sort, Filters, GroupBy, Pageable, Page0, PageSize) ->
    {In, Out} = lists:partition(fun(Rc) -> matches(Rc, Filters) end, Recs),
    Aggs = maps:from_list([{F, aggregates(In, C)} || #{field := F, aggregates := [_ | _]} = C <- Cols]),
    Sorted = sort_recs(In, Sort, Cols),
    Flat = group(Sorted, GroupBy, Cols, 0, []),
    Total = length(Flat),
    Pages = max(1, ceil_div(Total, PageSize)),
    Page = case Pageable of
               true -> min(Page0, Pages);
               false -> 1
           end,
    {Lo, Hi} = case Pageable of
                   true -> {(Page - 1) * PageSize, Page * PageSize};
                   false -> {0, Total}
               end,
    Indexed = lists:zip(lists:seq(0, Total - 1), Flat),
    Items = [{Kind, X, I >= Lo andalso I < Hi} || {I, {Kind, X}} <- Indexed,
                                                   Kind =:= row orelse (I >= Lo andalso I < Hi)]
        ++ [{row, Rc, false} || Rc <- Out],
    {Items, Page, Total, Aggs}.

ceil_div(A, B) -> (A + B - 1) div B.

matches(_, []) -> true;
matches(#{cells := Cells}, Filters) ->
    lists:all(fun({F, Needle}) ->
                      {Raw, Text} = maps:get(F, Cells, {<<>>, <<>>}),
                      N = string:lowercase(Needle),
                      contains(string:lowercase(Raw), N) orelse contains(string:lowercase(Text), N)
              end, Filters).

contains(Hay, Needle) -> binary:match(Hay, Needle) =/= nomatch.

%% Stable multi-column sort on the raw texts: numbers numerically when
%% both sides are numbers, text case-insensitively (as the browser does).
sort_recs(Recs, [], _) -> Recs;
sort_recs(Recs, Sort, _Cols) ->
    Keyed = [{[sort_key(Rc, F) || {F, _} <- Sort], I, Rc}
             || {I, Rc} <- lists:zip(lists:seq(1, length(Recs)), Recs)],
    Dirs = [D || {_, D} <- Sort],
    [Rc || {_, _, Rc} <- lists:sort(fun({Ka, Ia, _}, {Kb, Ib, _}) ->
                                            case cmp_keys(Ka, Kb, Dirs) of
                                                0 -> Ia =< Ib;
                                                X -> X < 0
                                            end
                                    end, Keyed)].

sort_key(#{cells := Cells}, F) ->
    {Raw, _} = maps:get(F, Cells, {<<>>, <<>>}),
    Raw.

cmp_keys([], [], []) -> 0;
cmp_keys([A | As], [B | Bs], [D | Ds]) ->
    case compare(A, B) of
        0 -> cmp_keys(As, Bs, Ds);
        X when D =:= desc -> -X;
        X -> X
    end.

%% @private The comparison both sides use: -1, 0 or 1.
compare(A, B) ->
    case {to_number(A), to_number(B)} of
        {Na, Nb} when Na =/= undefined, Nb =/= undefined -> sign(Na - Nb);
        _ ->
            La = string:lowercase(A), Lb = string:lowercase(B),
            if La < Lb -> -1; La > Lb -> 1; true -> 0 end
    end.

sign(X) when X < 0 -> -1;
sign(X) when X > 0 -> 1;
sign(_) -> 0.

%% A number written as -?digits(.digits)?(e[+-]?digits)?, the form the
%% browser accepts too.
to_number(B) when is_binary(B) ->
    case re:run(B, <<"^-?[0-9]+(\\.[0-9]+)?([eE][+-]?[0-9]+)?$">>, [{capture, all, binary}]) of
        {match, [_]} -> binary_to_integer(B);
        {match, [_, <<>>, _]} ->
            [M, E] = binary:split(B, [<<"e">>, <<"E">>]),
            binary_to_float(<<M/binary, ".0e", E/binary>>);
        {match, _} -> binary_to_float(B);
        nomatch -> undefined
    end;
to_number(N) when is_number(N) -> N;
to_number(_) -> undefined.

%% Group rows (sigil grouping.cljs): groups ordered by key, a group row
%% then its rows (or sub-groups). The initial render has nothing
%% collapsed.
group(Recs, [], _, _, _) -> [{row, Rc} || Rc <- Recs];
group(Recs, [F | Rest], Cols, Level, Parent) ->
    Keys = lists:foldl(fun(Rc, Acc) ->
                               K = sort_key(Rc, F),
                               case lists:member(K, Acc) of
                                   true -> Acc;
                                   false -> Acc ++ [K]
                               end
                       end, [], Recs),
    Ordered = lists:sort(fun(A, B) -> compare(A, B) =< 0 end, Keys),
    lists:append(
      [begin
           Members = [Rc || Rc <- Recs, sort_key(Rc, F) =:= K],
           Path = Parent ++ [{F, K}],
           #{cells := Cells} = hd(Members),
           {_, Title} = maps:get(F, Cells),
           G = #{id => group_id(Path), level => Level, title => Title,
                 count => length(Members), recs => Members},
           [{group, G} | group(Members, Rest, Cols, Level + 1, Path)]
       end || K <- Ordered]).

group_id(Path) ->
    iolist_to_binary(lists:join($|, [[F, $:, K] || {F, K} <- Path])).

%%%===================================================================
%%% Aggregates
%%%===================================================================

aggregates(Recs, #{field := F, aggregates := Aggs}) ->
    Nums = [N || #{cells := Cells} <- Recs,
                 N <- [to_number(element(1, maps:get(F, Cells)))], N =/= undefined],
    [{A, case Nums =/= [] orelse A =:= count of
             true -> aggregate(A, Nums);
             false -> undefined
         end} || A <- Aggs].

%% @doc One aggregate of a list of numbers (sum and count of none are 0).
-spec aggregate(sum | avg | count | min | max, [number()]) -> number().
aggregate(sum, Ns) -> lists:foldl(fun(N, Acc) -> Acc + N end, 0, Ns);
aggregate(count, Ns) -> length(Ns);
aggregate(avg, Ns) -> aggregate(sum, Ns) / length(Ns);
aggregate(min, Ns) -> lists:min(Ns);
aggregate(max, Ns) -> lists:max(Ns).

%% Status bar: count as an integer, avg with 2 decimals, others as an
%% integer when integral, else 2 decimals.
status_text(_, undefined) -> <<>>;
status_text(count, V) -> integer_to_binary(trunc(V));
status_text(avg, V) -> fixed(V, 2);
status_text(_, V) -> int_or_fixed(V, 2).

%% Group rows: integral values as integers, others with 1 decimal.
group_agg_text(V) -> int_or_fixed(V, 1).

int_or_fixed(V, _) when is_integer(V) -> integer_to_binary(V);
int_or_fixed(V, D) ->
    case V == trunc(V) of
        true -> integer_to_binary(trunc(V));
        false -> fixed(V, D)
    end.

fixed(V, D) -> float_to_binary(float(V), [{decimals, D}]).

%%%===================================================================
%%% Formatting (sigil datagrid/format.cljs)
%%%===================================================================

%% @doc The text of a value in a column: its `format' ("n2" number with
%% thousands separators, "c2" currency, "p1" percentage, a date pattern
%% with yyyy MM dd HH mm ss), else booleans as the yes / no labels and
%% everything else as text. The browser formats edited values the same way.
-spec format_value(term(), #{format := binary() | undefined, currency := binary(),
                             atom() => term()},
                   #{atom() => binary()}) -> binary().
format_value(V, _, _) when V =:= undefined; V =:= null -> <<>>;
format_value(V, #{format := undefined}, L) when is_boolean(V) -> bool_text(V, L);
format_value(V, #{format := undefined}, _) -> raw(V);
format_value(V, #{format := <<C, Digits/binary>> = Fmt, currency := Cur}, L)
  when C =:= $n; C =:= $N; C =:= $c; C =:= $C; C =:= $p; C =:= $P ->
    D = case string:to_integer(Digits) of
            {N, _} when is_integer(N), N >= 0 -> N;
            _ -> 0
        end,
    case to_number(raw(V)) of
        undefined -> maybe_date(V, Fmt, L);
        Num when C =:= $n; C =:= $N -> thousands(Num, D);
        Num when C =:= $c; C =:= $C -> <<Cur/binary, (thousands(Num, D))/binary>>;
        Num -> <<(fixed(Num * 100, D))/binary, "%">>
    end;
format_value(V, #{format := Fmt}, L) -> maybe_date(V, Fmt, L).

maybe_date(V, Fmt, L) ->
    case re:run(Fmt, <<"yyyy|MM|dd|HH|mm|ss">>) of
        nomatch -> format_value(V, #{format => undefined, currency => <<>>}, L);
        _ ->
            case parse_datetime(V) of
                undefined -> raw(V);
                {{Y, Mo, D}, {H, Mi, S}} ->
                    lists:foldl(fun({Tok, N, W}, Acc) ->
                                        binary:replace(Acc, Tok, pad(N, W), [global])
                                end, Fmt,
                                [{<<"yyyy">>, Y, 4}, {<<"MM">>, Mo, 2}, {<<"dd">>, D, 2},
                                 {<<"HH">>, H, 2}, {<<"mm">>, Mi, 2}, {<<"ss">>, S, 2}])
            end
    end.

pad(N, W) -> iolist_to_binary(io_lib:format("~*..0B", [W, N])).

%% Dates and times as written: calendar tuples, "YYYY-MM-DD" and
%% "YYYY-MM-DD[T ]HH:MM[:SS]" (local fields, no time zone arithmetic).
parse_datetime({{Y, M, D}, {H, Mi, S}}) -> {{Y, M, D}, {H, Mi, trunc(S)}};
parse_datetime({Y, M, D} = Date) when is_integer(Y), is_integer(M), is_integer(D) -> {Date, {0, 0, 0}};
parse_datetime(B) when is_binary(B) ->
    case re:run(B, <<"^(\\d{4})-(\\d{2})-(\\d{2})(?:[T ](\\d{2}):(\\d{2})(?::(\\d{2}))?)?">>,
                [{capture, all_but_first, binary}]) of
        {match, Parts} ->
            [Y, M, D, H, Mi, S] = [case P of <<>> -> 0; _ -> binary_to_integer(P) end
                                   || P <- Parts ++ lists:duplicate(6 - length(Parts), <<>>)],
            {{Y, M, D}, {H, Mi, S}};
        nomatch -> undefined
    end;
parse_datetime(L) when is_list(L) -> parse_datetime(text(L));
parse_datetime(_) -> undefined.

thousands(Num, D) ->
    Fixed = case D of
                0 -> integer_to_binary(round(Num));
                _ -> fixed(Num, D)
            end,
    {Sign, Abs} = case Fixed of
                      <<"-", Rest/binary>> -> {<<"-">>, Rest};
                      _ -> {<<>>, Fixed}
                  end,
    {Int, Frac} = case binary:split(Abs, <<".">>) of
                      [I, F] -> {I, <<".", F/binary>>};
                      [I] -> {I, <<>>}
                  end,
    <<Sign/binary, (group3(Int))/binary, Frac/binary>>.

group3(Int) ->
    N = byte_size(Int),
    First = case N rem 3 of 0 -> 3; R -> R end,
    <<Head:First/binary, Tail/binary>> = Int,
    iolist_to_binary([Head | [[$,, G] || <<G:3/binary>> <= Tail]]).

%%%===================================================================
%%% HTML: header, filter row, body, status bar, toolbar, pager
%%%===================================================================

header_cell(#{field := ?CHECKBOX} = C, _, _, Selected, Recs, L) ->
    Keys = [K || #{key := K} <- Recs],
    N = length([K || K <- Keys, lists:member(K, Selected)]),
    All = Keys =/= [] andalso N =:= length(Keys),
    ?H:el('div',
          ?H:void(input, [<<"ah-dg-select-all">>, <<"ah-dg-header-checkbox">>],
                  [{type, checkbox}, {tabindex, -1}, {checked, All},
                   {data_indeterminate, N > 0 andalso not All andalso <<"true">>},
                   {aria_label, maps:get(select_all, L)}]),
          [<<"ah-dg-header-cell">>, <<"ah-dg-header-cell-checkbox">>, <<"ah-dg-align-center">>
           | pin_class(C)],
          [{role, columnheader}, {data_field, ?CHECKBOX}, {style, cell_style(C)}]);
header_cell(#{field := F, title := Title, align := Align, sortable := Sortable,
              resizable := Resizable, type := Type} = C, Sort, Menu, _, _, _L) ->
    {Dir, Priority} = case lists:keyfind(F, 1, Sort) of
                          {F, D} -> {D, index_of({F, D}, Sort)};
                          false -> {undefined, 0}
                      end,
    Icon = case Dir of asc -> <<"▲"/utf8>>; desc -> <<"▼"/utf8>>; undefined -> <<>> end,
    ?H:el('div',
          [?H:el(span, Title, [<<"ah-dg-header-cell-content">>], []),
           case Sortable of
               false -> [];
               true ->
                   ?H:el(span, [Icon, case length(Sort) > 1 andalso Dir =/= undefined of
                                          true -> ?H:el(span, integer_to_binary(Priority),
                                                        [<<"ah-dg-header-sort-badge">>], []);
                                          false -> []
                                      end],
                         [<<"ah-dg-header-sort-icon">>], [{aria_hidden, <<"true">>}])
           end,
           case Menu of
               true -> ?H:el(span, <<"⋮"/utf8>>, [<<"ah-dg-column-menu-btn">>],
                             [{data_field, F}, {aria_hidden, <<"true">>}]);
               false -> []
           end,
           case Resizable of
               true -> ?H:el('div', [], [<<"ah-dg-resize-handle">>], [{aria_hidden, <<"true">>}]);
               false -> []
           end],
          [<<"ah-dg-header-cell">>, align_class(Align),
           [<<"ah-dg-header-cell-sorted">> || Dir =/= undefined],
           [<<"ah-dg-header-cell-resizable">> || Resizable],
           hidden_class(C) | pin_class(C)],
          [{role, columnheader}, {data_field, F},
           {aria_sort, case Dir of
                           asc -> <<"ascending">>;
                           desc -> <<"descending">>;
                           undefined when Sortable -> <<"none">>;
                           undefined -> undefined
                       end},
           {style, cell_style(C)},
           {data_type, Type =/= text andalso Type},
           {data_editable, maps:get(editable, C) andalso <<"true">>},
           {data_sortable, not Sortable andalso <<"false">>},
           {data_groupable, not maps:get(groupable, C) andalso <<"false">>},
           {data_pinned, maps:get(pinned, C) andalso <<"true">>},
           {data_min_width, maps:get(min_width, C)},
           {data_format, maps:get(format, C)},
           {data_currency, maps:get(format, C) =/= undefined andalso maps:get(currency, C)},
           {data_options, case maps:get(options, C) of
                              [] -> undefined;
                              Os -> json([[V, Lb] || {V, Lb} <- Os])
                          end},
           {data_aggs, case maps:get(aggregates, C) of
                           [] -> undefined;
                           As -> iolist_to_binary(lists:join($,, [atom_to_binary(A) || A <- As]))
                       end}]).

index_of(X, L) -> length(lists:takewhile(fun(Y) -> Y =/= X end, L)) + 1.

filter_row(Cols, Filters, L) ->
    ?H:el('div',
          [begin
               V = proplists:get_value(F, Filters, <<>>),
               ?H:el('div',
                     case Filterable andalso F =/= ?CHECKBOX of
                         true -> ?H:void(input, [<<"ah-dg-filter-input">>],
                                         [{type, text}, {data_field, F}, {value, V},
                                          {placeholder, maps:get(filter, L)},
                                          {autocomplete, off},
                                          {aria_label, T}]);
                         false -> []
                     end,
                     [<<"ah-dg-filter-cell">>, [<<"ah-dg-filter-cell-active">> || V =/= <<>>],
                      hidden_class(C) | pin_class(C)],
                     [{data_field, F}, {style, cell_style(C)}])
           end || #{field := F, filterable := Filterable, title := T} = C <- Cols],
          [<<"ah-dg-header-filter-row">>], [{role, row}, {aria_rowindex, 2}]).

body_items(Items, Cfg, Selected, RowIndex0) ->
    {Html, _} = lists:mapfoldl(
                  fun({row, Rc, true}, {Idx, RI}) ->
                          {row_html(Rc, Cfg, Selected, Idx, RI, false), {Idx + 1, RI + 1}};
                     ({row, Rc, false}, Acc) ->
                          {row_html(Rc, Cfg, Selected, 0, undefined, true), Acc};
                     ({group, G, true}, {Idx, RI}) ->
                          {group_row(G, Cfg), {Idx + 1, RI}};
                     ({group, _, false}, Acc) -> {[], Acc}
                  end, {0, RowIndex0 + 1}, Items),
    Html.

row_html(#{key := Key, row := Row, cells := Cells} = Rc, #{id := Id, cols := Cols, selection := Sel} = Cfg,
         Selected, Idx, RowIndex, Off) ->
    IsSel = lists:member(Key, Selected),
    ?H:el('div',
          [cell_html(C, Row, Cells, IsSel, Cfg) || C <- Cols],
          [<<"ah-dg-row">>, [<<"ah-dg-row-selected">> || IsSel],
           case Idx rem 2 of 0 -> <<"ah-dg-row-even">>; 1 -> <<"ah-dg-row-odd">> end,
           [<<"ah-dg-row-off">> || Off]],
          [{id, row_id(Id, Key)}, {role, row}, {data_key, Key},
           {aria_selected, Sel =/= none andalso atom_to_binary(IsSel)},
           {aria_rowindex, RowIndex},
           {data_i, maps:get(index, Rc, undefined)}]).

cell_html(#{field := ?CHECKBOX} = C, _, _, IsSel, #{labels := L}) ->
    ?H:el('div',
          ?H:void(input, [<<"ah-dg-row-checkbox">>],
                  [{type, checkbox}, {tabindex, -1}, {checked, IsSel},
                   {aria_label, maps:get(select_row, L)}]),
          [<<"ah-dg-cell">>, <<"ah-dg-cell-checkbox">>, <<"ah-dg-align-center">> | pin_class(C)],
          [{role, gridcell}, {data_field, ?CHECKBOX}, {style, cell_style(C)}]);
cell_html(#{field := F, key := K, align := Align, editable := Editable} = C, Row, Cells, _, _) ->
    {Raw, Text} = maps:get(F, Cells),
    V = value(Row, K),
    ?H:el('div',
          cell_content(C, V, Raw, Text, Row),
          [<<"ah-dg-cell">>, align_class(Align), [<<"ah-dg-cell-editable">> || Editable],
           hidden_class(C) | pin_class(C)],
          [{role, gridcell}, {data_field, F},
           {data_v, Raw =/= Text andalso Raw},
           {aria_readonly, Editable andalso <<"false">>},
           {style, cell_style(C)}]).

cell_content(#{render := {M, Fn}}, V, _, _, Row) -> M:Fn(V, Row);
cell_content(C, V, Raw, Text, Row) when map_get(type, C) =:= command ->
    command_group(C, V, Raw, Text, Row);
cell_content(_, V, _, _, _) when V =:= undefined; V =:= null ->
    ?H:el(span, <<>>, [<<"ah-dg-cell-content">>], []);
cell_content(#{type := progress, max := Max}, _, Raw, _, _) ->
    N = case to_number(Raw) of undefined -> 0; X -> X end,
    Pct = min(100, max(0, N / Max * 100)),
    ?H:el('div', ?H:el('div', [], [<<"ah-dg-progress-bar">>],
                       [{style, [<<"width:">>, num_text(round2(Pct)), <<"%">>]}]),
          [<<"ah-dg-progress">>],
          [{role, progressbar}, {aria_valuenow, num_text(N)}, {aria_valuemin, 0},
           {aria_valuemax, num_text(Max)}]);
cell_content(#{type := rating}, _, _, Text, _) ->
    ?H:el(span, Text, [<<"ah-dg-rating">>], []);
cell_content(#{type := image}, _, Raw, _, _) ->
    ?H:void(img, [<<"ah-dg-cell-image">>],
            [{src, Raw}, {alt, <<>>}, {style, <<"width:30px;height:30px">>}]);
cell_content(#{type := link, target := Target}, _, Raw, Text, _) ->
    ?H:el(a, Text, [<<"ah-dg-cell-link">>],
          [{href, Raw}, {target, Target},
           {rel, Target =:= <<"_blank">> andalso <<"noopener noreferrer">>}]);
cell_content(#{type := badge, badges := Badges}, _, Raw, Text, _) ->
    Cls = case Badges of
              #{Raw := {_, Cl}} -> Cl;
              _ -> <<"default">>
          end,
    ?H:el(span, Text, [<<"ah-dg-badge">>, <<"ah-dg-badge-", Cls/binary>>], []);
cell_content(_, _, _, Text, _) ->
    ?H:el(span, Text, [<<"ah-dg-cell-content">>], []).

command_group(#{field := F, commands := Cmds}, _, _, _, _) ->
    ?H:el(span, [?H:el(button, Lb, [<<"ah-dg-command-btn">>],
                       [{type, button}, {tabindex, -1}, {data_field, F}, {data_command, N}])
                 || {N, Lb} <- Cmds],
          [<<"ah-dg-command-group">>], []).

round2(X) -> round(X * 100) / 100.

group_row(#{id := GId, level := Level, title := Title, count := Count, recs := Recs},
          #{cols := Cols}) ->
    Aggs = [{T, iolist_to_binary(lists:join(<<", ">>, Parts))}
            || #{aggregates := [_ | _], title := T, hidden := false} = C <- Cols,
               Parts <- [[[atom_to_binary(A), $=, group_agg_text(V)]
                          || {A, V} <- aggregates(Recs, C), V =/= undefined]],
               Parts =/= []],
    aihtml_tpl:safe(tpl_datagrid_group_row(
                      #{id => GId, level => Level, aria_level => Level + 1,
                        expanded => <<"true">>, open => true,
                        indent => Level * ?GROUP_INDENT,
                        colspan => length([x || #{hidden := false} <- Cols]),
                        title => Title, count => Count,
                        has_aggs => Aggs =/= [],
                        aggs => [#{label => T, text => X} || {T, X} <- Aggs]})).

empty_message(L, Empty) ->
    ?H:el('div', maps:get(empty, L), [<<"ah-dg-empty-message">>], [{hidden, not Empty}]).

statusbar(Cols, Aggs, L) ->
    ?H:el('div',
          ?H:el('div',
                [?H:el('div',
                       [?H:el(span,
                              [?H:el(span, [maps:get(A, L), <<": ">>], [<<"ah-dg-statusbar-label">>], []),
                               ?H:el(span, status_text(A, V), [<<"ah-dg-statusbar-value">>], [])],
                              [<<"ah-dg-statusbar-item">>], [{data_agg, A}])
                        || {A, V} <- maps:get(F, Aggs, [])],
                       [<<"ah-dg-statusbar-cell">>, hidden_class(C) | pin_class(C)],
                       [{data_field, F}, {style, cell_style(C)}])
                 || #{field := F} = C <- Cols],
                [<<"ah-dg-statusbar-row">>], []),
          [<<"ah-dg-statusbar">>], [{role, status}]).

toolbar(Items, L) ->
    ?H:el('div', [tool(I, L) || I <- Items], [<<"ah-dg-toolbar">>], [{role, toolbar}]).

tool(separator, _) -> ?H:el('div', [], [<<"ah-dg-toolbar-separator">>], [{role, separator}]);
tool(spacer, _) -> ?H:el('div', [], [<<"ah-dg-toolbar-spacer">>], []);
tool(search, L) ->
    ?H:void(input, [<<"ah-dg-search-input">>],
            [{type, search}, {placeholder, maps:get(search, L)}, {aria_label, maps:get(search, L)},
             {autocomplete, off}]);
tool(E, L) when E =:= export_csv; E =:= export_xlsx; E =:= export_pdf ->
    <<"export_", Fmt/binary>> = atom_to_binary(E),
    tool_button(maps:get(E, L), <<"⤓"/utf8>>, [{data_export, Fmt}]);
tool({N, Label}, _) -> tool_button(Label, undefined, [{data_name, text(N)}]);
tool(#{name := N, label := Label} = M, _) ->
    tool_button(Label, maps:get(icon, M, undefined), [{data_name, text(N)}]);
tool(Other, _) -> error({aihtml, {bad_toolbar_item, Other}}).

tool_button(Label, Icon, Attrs) ->
    ?H:el(button,
          [case Icon of
               undefined -> [];
               _ -> ?H:el(span, Icon, [<<"ah-dg-toolbar-btn-icon">>], [{aria_hidden, <<"true">>}])
           end,
           ?H:el(span, Label, [<<"ah-dg-toolbar-btn-text">>], [])],
          [<<"ah-dg-toolbar-btn">>], [{type, button} | Attrs]).

pager(Page, PageSize, Total, #{page_sizes := Sizes, labels := L}, Link) ->
    Opts = #{sizes => Sizes, labels => L},
    aihtml_tpl:safe(tpl_datagrid_pager(
                      pager_view(Page, PageSize, Total, case Link of
                                                            undefined -> Opts;
                                                            _ -> Opts#{href => Link}
                                                        end))).

%% @doc The view data of templates/datagrid_pager.mustache (the browser
%% builds the same). Pages are 1-based; at most 7 page buttons around the
%% current page. With `href' (a link template whose only placeholder
%% left is `{page}', see link_template/4) the buttons are links.
-spec pager_view(pos_integer(), pos_integer(), non_neg_integer(),
                 #{sizes := [pos_integer()], labels := #{atom() => binary()},
                   href => binary()}) -> map().
pager_view(Page0, PageSize, Total, #{sizes := Sizes, labels := L} = Opts) ->
    Pages = ceil_div(Total, PageSize),
    Page = max(1, min(Page0, max(1, Pages))),
    Half = ?PAGER_BUTTONS div 2,
    End = min(Pages, max(1, Page - Half) + ?PAGER_BUTTONS - 1),
    Start = max(1, End - ?PAGER_BUTTONS + 1),
    Prev = max(1, Page - 1),
    Next = min(max(1, Pages), Page + 1),
    Last = max(1, Pages),
    AtStart = Page =< 1,
    AtEnd = Page >= Pages,
    View = #{label => maps:get(pages, L), info => subst(maps:get(total, L), Total),
             prev => Prev, next => Next, last => Last, at_start => AtStart, at_end => AtEnd,
             first_label => maps:get(first_page, L), prev_label => maps:get(prev_page, L),
             next_label => maps:get(next_page, L), last_label => maps:get(last_page, L),
             pages => [#{page => P, active => P =:= Page} || P <- lists:seq(Start, End), Pages > 0],
             size_label => maps:get(page_size, L),
             sizes => [#{size => S, text => subst(maps:get(per_page, L), S), selected => S =:= PageSize}
                       || S <- lists:usort([PageSize | Sizes])]},
    case Opts of
        #{href := T} ->
            Url = fun(true, _) -> <<>>;
                     (false, P) -> binary:replace(T, <<"{page}">>, integer_to_binary(P), [global])
                  end,
            View#{link => true,
                  first_href => Url(AtStart, 1), prev_href => Url(AtStart, Prev),
                  next_href => Url(AtEnd, Next), last_href => Url(AtEnd, Last),
                  pages => [M#{link => true, href => Url(false, P)}
                            || #{page := P} = M <- maps:get(pages, View)]};
        _ -> View
    end.

%% The grid's `href' with the view filled in but `{page}': `{size}', `{sort}'
%% (`field:dir,...') and `{search}', URL-encoded as encodeURIComponent
%% does (datagrid.ts linkTemplate/1 builds the same).
link_template(#{href := T}, PageSize, Sort, Search) ->
    SortText = lists:join(<<",">>, [[uri(F), <<":">>, atom_to_binary(D)] || {F, D} <- Sort]),
    lists:foldl(fun({K, V}, Acc) -> binary:replace(Acc, K, iolist_to_binary(V), [global]) end, T,
                [{<<"{size}">>, integer_to_binary(PageSize)}, {<<"{sort}">>, SortText},
                 {<<"{search}">>, uri(Search)}]);
link_template(_, _, _, _) -> undefined.

%% encodeURIComponent: UTF-8 bytes, all but A-Z a-z 0-9 - _ . ! ~ * ' ( )
%% as %XX.
uri(B) ->
    << <<(case C of
              _ when C >= $a, C =< $z; C >= $A, C =< $Z; C >= $0, C =< $9 -> <<C>>;
              _ -> case lists:member(C, "-_.!~*'()") of
                       true -> <<C>>;
                       false -> iolist_to_binary(io_lib:format("%~2.16.0B", [C]))
                   end
          end)/binary>> || <<C>> <= text(B) >>.

align_class(left) -> <<"ah-dg-align-left">>;
align_class(center) -> <<"ah-dg-align-center">>;
align_class(right) -> <<"ah-dg-align-right">>.

hidden_class(#{hidden := true}) -> <<"ah-dg-col-hidden">>;
hidden_class(_) -> [].

pin_class(#{last_pinned := true}) -> [<<"ah-dg-cell-pinned-last">>];
pin_class(_) -> [].

cell_style(#{width := W} = C) ->
    [<<"width:">>, integer_to_binary(W), <<"px">>,
     case maps:get(left, C, undefined) of
         undefined -> [];
         Left -> [<<";position:sticky;left:">>, integer_to_binary(Left), <<"px;z-index:2">>]
     end].

%% Row ids: the grid id and the key, or a hex form of a key that is not
%% a plain name.
row_id(GridId, Key) ->
    case re:run(Key, <<"^[A-Za-z0-9_-]+$">>) of
        {match, _} -> <<GridId/binary, "-r-", Key/binary>>;
        nomatch -> <<GridId/binary, "-rx-", (binary:encode_hex(Key, lowercase))/binary>>
    end.

%%%===================================================================
%%% Remote mode and row updates
%%%===================================================================

%% @doc The view a remote grid's `source' action is asked for (its
%% Event): `sort' and `filters' by column key (unknown fields dropped),
%% the `search' box text, the 1-based `page' and `page_size' (one of the
%% grid's page sizes), `offset' and `limit' for a database query, and
%% `export' (csv | xlsx | pdf when the user exports: then offset is 0 and
%% limit infinity, the answer should hold every matching row).
-spec datagrid_query(aihtml_action:event()) -> query().
datagrid_query(#{data := Data}) ->
    #{cols := Cols, page_sizes := Sizes, page_size := DefSize, pageable := Pageable} = cfg(Data),
    ByField = maps:from_list([{F, K} || #{field := F, key := K} <- Cols, F =/= ?CHECKBOX]),
    Sort = [{maps:get(F, ByField), binary_to_existing_atom(D)}
            || [F, D] <- json_field(Data, <<"sort">>, []),
               is_binary(F), is_map_key(F, ByField), D =:= <<"asc">> orelse D =:= <<"desc">>],
    Filters = case json_field(Data, <<"filter">>, #{}) of
                  M when is_map(M) ->
                      [{maps:get(F, ByField), V} || F := V <- M, is_map_key(F, ByField),
                                                    is_binary(V), V =/= <<>>];
                  _ -> []
              end,
    Export = case maps:get(<<"export">>, Data, <<>>) of
                 <<"csv">> -> csv;
                 <<"xlsx">> -> xlsx;
                 <<"pdf">> -> pdf;
                 _ -> undefined
             end,
    PageSize = case int_field(Data, <<"pageSize">>) of
                   N when is_integer(N) -> case lists:member(N, Sizes) of
                                               true -> N;
                                               false -> DefSize
                                           end;
                   _ -> DefSize
               end,
    Page = case int_field(Data, <<"page">>) of
               P when is_integer(P), P > 0 -> P;
               _ -> 1
           end,
    {Offset, Limit} = case Export =:= undefined andalso Pageable of
                          true -> {(Page - 1) * PageSize, PageSize};
                          false -> {0, infinity}
                      end,
    Search = case maps:get(<<"search">>, Data, <<>>) of
                 S when is_binary(S) -> S;
                 _ -> <<>>
             end,
    #{sort => Sort, filters => lists:sort(Filters), search => Search, page => Page,
      page_size => PageSize, offset => Offset, limit => Limit, export => Export}.

%% @doc Answer a remote grid's `source' action: render `Rows' (this page
%% of the result; `Total' is the number of matching rows) with the grid's
%% columns, morph them into the body and the pager, and call the
%% behaviour method `rowsLoaded'. For an export request (see
%% datagrid_query/1) `Rows' are all matching rows: they are formatted
%% here and sent to the browser, which writes the file.
-spec datagrid_rows(aihtml_action:ctx(), aihtml_action:event(), [row()], non_neg_integer()) -> ok.
datagrid_rows(Ctx, #{data := Data} = Ev, Rows, Total) when is_integer(Total), Total >= 0 ->
    #{id := Id, labels := L, pageable := Pageable} = Cfg = cfg(Data),
    #{page := Page, page_size := PageSize, export := Export, sort := Sort,
      search := Search} = datagrid_query(Ev),
    Recs = [rec(Row, Cfg) || Row <- Rows],
    case Export of
        undefined ->
            HeaderRows = case maps:get(<<"headerRows">>, Data, <<"1">>) of
                             <<"2">> -> 2;
                             _ -> 1
                         end,
            Offset = case Pageable of true -> (Page - 1) * PageSize; false -> 0 end,
            Items = [{row, Rc, true} || Rc <- Recs],
            aihtml_action:html(Ctx, {id, sub_id(Id, <<"body">>)},
                               [body_items(Items, Cfg, [], HeaderRows + Offset),
                                empty_message(L, Recs =:= [])], morph_inner),
            case Pageable of
                true -> aihtml_action:html(Ctx, {id, sub_id(Id, <<"pager">>)},
                                           pager(Page, PageSize, Total, Cfg,
                                                 link_template(Cfg, PageSize,
                                                               [{text(K), D} || {K, D} <- Sort],
                                                               Search)),
                                           morph_inner);
                false -> ok
            end,
            aihtml_action:call(Ctx, {id, Id}, rowsLoaded, [Total, Page]);
        _ ->
            {Headers, Body} = export_table(Cfg, Recs),
            aihtml_action:call(Ctx, {id, Id}, exportData, [atom_to_binary(Export), Headers, Body])
    end.

%% The visible data columns' titles and the rows' texts (raw when a cell
%% shows no text, like a progress bar).
export_table(#{cols := Cols}, Recs) ->
    ECols = [C || #{field := F, hidden := false, type := T} = C <- Cols,
                  F =/= ?CHECKBOX, T =/= command],
    {[T || #{title := T} <- ECols],
     [[case maps:get(F, Cells) of
           {Raw, <<>>} -> Raw;
           {_, Text} -> Text
       end || #{field := F} <- ECols] || #{cells := Cells} <- Recs]}.

%% @doc Re-render one row of a grid from inside an action, typically the
%% one an `ah:edit' postback saved (`Event' is any event of the grid): the
%% row is morphed in place and the behaviour method `rowUpdated' restores
%% its selection, widths and position in the view.
-spec datagrid_row(aihtml_action:ctx(), aihtml_action:event(), row()) -> ok.
datagrid_row(Ctx, #{data := Data}, Row) ->
    #{id := Id} = Cfg = cfg(Data),
    #{key := Key} = Rc = rec(Row, Cfg),
    RowId = row_id(Id, Key),
    aihtml_action:html(Ctx, {id, RowId}, row_html(Rc, Cfg, [], 0, undefined, false), morph),
    aihtml_action:call(Ctx, {id, Id}, rowUpdated, [RowId]).

%% @doc Apply a query to rows held in memory: filter (case-insensitive
%% "contains" on the value's text), search (any value), sort, then take
%% the page. Returns the page and the number of matching rows. For demos,
%% tests and small tables; a database does this in SQL.
-spec datagrid_select(query(), [row()]) -> {[row()], non_neg_integer()}.
datagrid_select(#{sort := Sort, filters := Filters, search := Search,
                  offset := Offset, limit := Limit}, Rows) ->
    Needle = string:lowercase(Search),
    Matching = [R || R <- Rows,
                     lists:all(fun({K, V}) ->
                                       contains(string:lowercase(raw(value(R, K))),
                                                string:lowercase(V))
                               end, Filters),
                     Needle =:= <<>> orelse
                         lists:any(fun(V) -> contains(string:lowercase(raw(V)), Needle) end,
                                   maps:values(R))],
    Indexed = lists:zip(lists:seq(1, length(Matching)), Matching),
    Sorted = [R || {_, R} <- lists:sort(
                               fun({Ia, A}, {Ib, B}) ->
                                       case cmp_keys([raw(value(A, K)) || {K, _} <- Sort],
                                                     [raw(value(B, K)) || {K, _} <- Sort],
                                                     [D || {_, D} <- Sort]) of
                                           0 -> Ia =< Ib;
                                           X -> X < 0
                                       end
                               end, Indexed)],
    Page = case Limit of
               infinity -> lists:nthtail(min(Offset, length(Sorted)), Sorted);
               _ -> lists:sublist(Sorted, Offset + 1, Limit)
           end,
    {Page, length(Matching)}.

cfg(#{<<"render">> := Token}) ->
    case aihtml_action:unsign(Token) of
        {ok, #{id := _, columns := _, selection := _, labels := _} = Spec} -> cfg_from(Spec);
        _ -> error({aihtml, bad_datagrid_token})
    end;
cfg(_) -> error({aihtml, bad_datagrid_token}).

json_field(Data, K, Default) ->
    case maps:get(K, Data, undefined) of
        B when is_binary(B), B =/= <<>> ->
            try json:decode(B) catch _:_ -> Default end;
        _ -> Default
    end.

int_field(Data, K) ->
    case maps:get(K, Data, undefined) of
        B when is_binary(B) -> try binary_to_integer(B) catch error:badarg -> undefined end;
        I when is_integer(I) -> I;
        _ -> undefined
    end.

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => datagrid, category => data,
       signature => <<"ah_datagrid(Columns, Rows, Css, Attrs)">>,
       root => <<"ah-dg">>,
       groups => #{selection => {[none, single, multi, checkbox], single}},
       flags => [filter_row, pageable, statusbar],
       classes => #{none => [], single => [], multi => [], checkbox => [],
                    filter_row => [], pageable => [], statusbar => []},
       options => [value, key_field, height, page, page_size, page_sizes, sort, filters,
                   group_by, edit_mode, column_menu, toolbar, export_name, labels,
                   source, total, href],
       behavior => <<"datagrid">>,
       events => [<<"change">>, <<"ah:row-click">>, <<"ah:row-dblclick">>, <<"ah:edit">>,
                  <<"ah:command">>, <<"ah:toolbar">>, <<"ah:sort">>, <<"ah:filter">>,
                  <<"ah:page">>, <<"ah:group-toggle">>, <<"ah:column-resize">>,
                  <<"ah:query">>],
       doc => <<"A data grid: sort, filter, page and select rows, navigate cells with the "
                "keyboard, resize, pin and hide columns, edit cells, group rows with "
                "aggregates and export to CSV, Excel or PDF; rows may come from the server "
                "one page at a time.">>,
       option_docs =>
           #{none => <<"No row selection.">>,
             single => <<"Selection: one row at a time (default).">>,
             multi => <<"Selection: Ctrl+click toggles, Shift+click selects a range.">>,
             checkbox => <<"Selection: a check box column with a select-all box.">>,
             filter_row => <<"A row of filter boxes under the header (contains, ignoring case).">>,
             pageable => <<"Show a pager; page_size rows per page.">>,
             statusbar => <<"A bottom bar with the columns' aggregates over the filtered rows "
                            "(local mode).">>,
             value => <<"The selected row keys.">>,
             key_field => <<"The row key that identifies a row (default id).">>,
             height => <<"The grid's height (px or a CSS length); the body scrolls.">>,
             page => <<"The 1-based page shown first (default 1).">>,
             page_size => <<"Rows per page (default 10).">>,
             page_sizes => <<"Choices of the page size box (default [10, 20, 50, 100]).">>,
             sort => <<"Initial sort: [{Key, asc | desc}], the first key first.">>,
             filters => <<"Initial filter row texts: [{Key, Text}] or a map.">>,
             group_by => <<"Group rows by these column keys (local mode), with a row per "
                           "group showing the count and the columns' aggregates.">>,
             edit_mode => <<"dblclick (default) or click: what starts editing an editable cell "
                            "(Enter and F2 always do).">>,
             column_menu => <<"The ⋮ menu of each column: sort, pin, hide, group, show "
                              "columns (default true).">>,
             toolbar => <<"Toolbar entries: export_csv, export_xlsx, export_pdf, search, "
                          "separator, spacer, {Name, Label} (fires ah:toolbar).">>,
             export_name => <<"File name of exports, without the extension (default data).">>,
             labels => <<"Texts to replace, e.g. #{empty => <<\"暂无数据\">>, total => "
                         "<<\"共 {0} 条\">>}; see aihtml_datagrid:default_labels/0.">>,
             source => <<"Action ref {Module, Action, Args}: remote mode. Rows are the first "
                         "page, rendered here; each view change runs the action, which reads "
                         "datagrid_query(Event) and answers with "
                         "datagrid_rows(Ctx, Event, Rows, Total).">>,
             total => <<"Remote mode: the number of matching rows; Rows are the first page "
                        "(default: their count). The server always renders that page; the grid "
                        "does not load on mount (call refresh for that).">>,
             href => <<"Link template for the pager: {page}, {size}, {sort} (field:asc,...) and "
                       "{search} are filled in, so pages are crawlable links; with JavaScript a "
                       "click stays in the page and pushes the URL.">>},
       methods =>
           [#{name => setValue, args => <<"(Keys)">>,
              doc => <<"Select the rows with these keys (a list or comma separated text, "
                       "see aihtml_value), without firing change.">>},
            #{name => getValue, args => <<"()">>,
              doc => <<"Return the selected keys, comma separated (a comma inside a key "
                       "is escaped as \\,; aihtml_value:split/1 reads it).">>},
            #{name => sort, args => <<"(Field, Dir)">>,
              doc => <<"Sort by a column (\"asc\", \"desc\" or null to clear).">>},
            #{name => filter, args => <<"(Field, Text)">>, doc => <<"Set a column filter.">>},
            #{name => search, args => <<"(Text)">>, doc => <<"Filter on all columns.">>},
            #{name => goToPage, args => <<"(Page)">>, doc => <<"Show a page (1-based).">>},
            #{name => groupBy, args => <<"(Fields)">>,
              doc => <<"Group by these fields (local mode); [] ungroups.">>},
            #{name => showColumn, args => <<"(Field)">>, doc => <<"Show a hidden column.">>},
            #{name => hideColumn, args => <<"(Field)">>, doc => <<"Hide a column.">>},
            #{name => pinColumn, args => <<"(Field, Pinned)">>,
              doc => <<"Pin a column to the left, or unpin it.">>},
            #{name => setColumnWidth, args => <<"(Field, Px)">>, doc => <<"Resize a column.">>},
            #{name => exportData, args => <<"(Format, Headers, Rows)">>,
              doc => <<"Write csv, xlsx or pdf from these texts; with only a format, export "
                       "the grid's filtered and sorted rows (local) or ask the server (remote).">>},
            #{name => refresh, args => <<"()">>,
              doc => <<"Remote mode: ask the source action for the current view again.">>},
            #{name => rowsLoaded, args => <<"(Total, Page)">>,
              doc => <<"Called by datagrid_rows/4 after the rows were morphed in.">>},
            #{name => rowUpdated, args => <<"(RowId)">>,
              doc => <<"Called by datagrid_row/3 after a row was morphed in.">>}]}].

%%%===================================================================
%%% Internal
%%%===================================================================

%% The root needs an id: row ids, the body and the pager derive from it.
ensure_id(#ah_datagrid{id = undefined} = R) ->
    Id = <<"ah-dg", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
    {Id, R#ah_datagrid{id = Id}};
ensure_id(#ah_datagrid{id = Id0} = R) ->
    Id = text(Id0),
    {Id, R#ah_datagrid{id = Id}}.

sub_id(Id, Suffix) -> <<Id/binary, "-", Suffix/binary>>.

hidden_input(undefined, _) -> [];
hidden_input(Name, Value) ->
    ?H:void(input, [], [{type, hidden}, {name, Name}, {value, Value}, {data_ah_input, true}]).

join(Vs) -> aihtml_value:join(Vs).

bool_int(true) -> 1;
bool_int(false) -> 0.

json(T) -> iolist_to_binary(aihtml_json:encode(T)).

num_text(V) when is_integer(V) -> integer_to_binary(V);
num_text(V) when is_float(V) ->
    case V == trunc(V) of
        true -> integer_to_binary(trunc(V));
        false -> float_to_binary(V, [short])
    end.

text(undefined) -> <<>>;
text(B) when is_binary(B) -> B;
text(L) when is_list(L) ->
    case unicode:characters_to_binary(L) of
        B when is_binary(B) -> B;
        _ -> error({aihtml, {bad_text, L}})
    end;
text(F) when is_float(F) -> num_text(F);
text(X) -> beamai_html_escape:to_binary(X, aihtml).
