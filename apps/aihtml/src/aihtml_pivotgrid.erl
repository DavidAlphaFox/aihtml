%%%-------------------------------------------------------------------
%%% @doc The pivot table, ported from sigil (data/pivotgrid). See
%%% designs/04-components.md.
%%%
%%%   pivotgrid(Rows, Layout, Css, Attrs)   a pivot table of data rows
%%%   pivotgrid_rows(Ctx, Event, Rows)      (in a `source' action) re-render it
%%%   pivotgrid_view(Event)                 the layout and view an event carries
%%%   pivotgrid_cell(Event)                 the cell an 'ah:cell-click' names
%%%
%%% `Rows' are the data (maps from field names to values), `Layout' the
%%% component's value: `#{rows => [country, city], columns => [year],
%%% values => [{sales, sum}]}'. The records are grouped by the row and
%%% the column dimensions, every crossing shows the measures (sum, count,
%%% avg, min, max, product) and expanded members show their subtotals,
%%% with grand totals at the end. The aggregation runs here, in Erlang,
%%% for the first render; the DOM and class names are sigil's four
%%% quadrants (corner, column headers, row headers, body), so its
%%% stylesheet applies unchanged.
%%%
%%% == Local and remote mode ==
%%%
%%% By default (local mode) the rows are also written into the root as a
%%% JSON data island (`<script type="application/json" class="ah-pg-data">')
%%% and the `pivotgrid' behaviour (assets/js/components/pivotgrid.js)
%%% re-aggregates in the browser when the user expands or collapses a
%%% member, sorts, or moves fields between the areas of the field list.
%%% The browser runs the same algorithm and renders the same shared
%%% templates (templates/pivotgrid_grid.mustache and
%%% templates/pivotgrid_fields.mustache), so its tables are byte for byte
%%% what this module renders.
%%%
%%% With `{source, {Mod, Action, Args}}' (remote mode) no data goes to the
%%% page. Every view change fires the component event 'ah:view' on the
%%% root, bound to the action, with
%%%
%%%   Event.id                the root id
%%%   Event.value             the layout (JSON, also the root's data-ah-value)
%%%   Event.data "view"       expanded members and sort order (JSON)
%%%   Event.data "config"     fields, labels, formats and totals (JSON)
%%%
%%% `pivotgrid_view(Event)' decodes the first two, so the action can load
%%% the rows it needs (only the fields of the layout, say), and it answers
%%% with `pivotgrid_rows(Ctx, Event, Rows)', which aggregates and renders
%%% here and morphs the tables into the page, then calls the behaviour
%%% method `viewLoaded'. The server keeps nothing between requests.
%%%
%%% == Events ==
%%%
%%% `postback' (and `on('ah:cell-click', ...)') fires when a value cell is
%%% clicked or Enter is pressed on it; `pivotgrid_cell(Event)' gives the
%%% row and column members, a field => key filter for a drill-through
%%% query, the measure and the value. `change' fires when the layout
%%% changes (fields moved in the field list, another aggregate).
%%%
%%% Each component function builds an element record (#ah_pivotgrid{},
%%% include/aihtml_pivotgrid.hrl) and render/1 turns it into HTML.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_pivotgrid).
-behaviour(aihtml_element).

-include("aihtml_pivotgrid.hrl").

-export([pivotgrid/4, pivotgrid_rows/3, pivotgrid_view/1, pivotgrid_cell/1,
         render/1, fields/1, catalog/0, facade_extras/0]).

-export_type([element/0, field_name/0, row/0, agg/0, value_spec/0, layout/0, format/0,
              field/0, path/0, view/0, locale/0]).

-define(H, aihtml_html).
-define(E, aihtml_element).

%% Shared templates (see aihtml_tpl): also compiled to AH.tpl.* for the browser.
-compile({parse_transform, beamai_mustache_transform}).
-mustache_template({tpl_pivotgrid_grid, "../templates/pivotgrid_grid.mustache"}).
-mustache_template({tpl_pivotgrid_fields, "../templates/pivotgrid_fields.mustache"}).

-type element() :: #ah_pivotgrid{}.
%% A field name: an atom or a binary (a map key of the data rows).
-type field_name() :: atom() | binary().
%% One data row: a map from field names to values. Numbers are
%% aggregated; texts, atoms, dates ({Y, M, D}) and numbers group; `null'
%% and `undefined' are blanks.
-type row() :: #{field_name() => term()} | [{field_name(), term()}].
-type agg() :: sum | count | avg | min | max | product.
%% A measure: a field (summed, or the field's default `agg'), `{Field,
%% Agg}', or a map with `field', `agg' and an optional `label'. A map
%% without `field' and with `agg => count' counts records.
-type value_spec() :: field_name() | {field_name(), agg()}
                      | #{field => field_name(), agg => agg(), label => unicode:chardata()}.
%% The pivot layout, the component's value: the row dimensions, the
%% column dimensions and the measures, each in order.
-type layout() :: #{rows => [field_name()], columns => [field_name()],
                    values => [value_spec()]}.
%% Number display: fixed `decimals' (default: integers as they are, other
%% numbers with 2), a `thousands' separator (default none), the `decimal'
%% separator (default "."), a `prefix' and a `suffix'.
-type format() :: #{decimals => non_neg_integer(), thousands => unicode:chardata(),
                    decimal => unicode:chardata(), prefix => unicode:chardata(),
                    suffix => unicode:chardata()}.
%% A field of the data: its name, and optionally a `label', the number
%% `format' used when it is a measure and its default `agg' when it is
%% dropped on the values.
-type field() :: field_name() | {field_name(), unicode:chardata()}
               | #{name := field_name(), label => unicode:chardata(),
                   format => format(), agg => agg()}.
%% A member path: the keys of the dimensions from the outermost.
-type path() :: [number() | binary() | null].
%% The view state: expanded row and column members, row order (by key,
%% or by the values of one column: `col' is the member path the column
%% aggregates, [] for the grand total, `vi' the measure) and column order.
-type view() :: #{expanded_rows => [path()] | all,
                  expanded_cols => [path()] | all,
                  row_sort => null | #{by := key, dir := asc | desc}
                             | #{by := value, dir := asc | desc,
                                 col := path(), vi := non_neg_integer()},
                  col_sort => asc | desc}.
-type locale() :: en | zh.

-define(AGGS, [<<"sum">>, <<"count">>, <<"avg">>, <<"min">>, <<"max">>, <<"product">>]).

%%%===================================================================
%%% pivotgrid
%%%===================================================================

%% @doc A pivot table of `Rows' (maps from field names to values) laid out
%% by `Layout' (`#{rows => [F], columns => [F], values => [Spec]}', see
%% the type value_spec() for the measures).
%%
%% Css: `expand_all' (every member starts expanded), `values_on_rows'
%% (with several measures, one row per measure instead of one column),
%% `field_list' (a bar of the fields and the rows, columns and values
%% areas; drag the chips or open their menu to re-pivot).
%% Options (in Attrs): `fields' (the fields with labels, formats and
%% default aggregates; default: every key of the rows), `view' (the
%% initial expanded members and sort order, as pivotgrid_view/1 returns
%% it), `row_subtotals', `col_subtotals', `grand_totals' (default true),
%% `format' (the default number format), `height' (px or a CSS length;
%% the body scrolls under fixed headers), `locale' (en | zh), `labels'
%% (texts), `source' (an action ref: remote mode, see the module doc).
-spec pivotgrid([row()], layout(), aihtml_html:css(), aihtml_html:attrs()) -> #ah_pivotgrid{}.
pivotgrid(Rows, Layout, Css, Attrs) ->
    ?E:build(?MODULE, #ah_pivotgrid{items = Rows, value = Layout}, Css, Attrs).

render_pivotgrid(#ah_pivotgrid{items = Items, name = Name, source = Source} = R0) ->
    {Id, R} = ensure_id(R0),
    Classes = ?E:classes(?MODULE, R),
    M0 = model(R, Id),
    Data = data(Items, field_names(M0)),
    {Grid, FieldsView, M} = views(M0, Data),
    Layout = layout_json(M),
    Remote = case Source of
                 undefined -> [];
                 {Mod, Act, _} = Ref when is_atom(Mod), is_atom(Act) -> aihtml:on('ah:view', Ref);
                 Other -> error({aihtml, {bad_option, source, Other}})
             end,
    Labels = maps:get(labels, M),
    ?H:el('div',
          [[?H:el('div', aihtml_tpl:safe(tpl_pivotgrid_fields(FieldsView)),
                  [<<"ah-pg-fields">>], [{id, sub_id(Id, <<"fields">>)}])
            || maps:get(field_list, M)],
           ?H:el('div', aihtml_tpl:safe(tpl_pivotgrid_grid(Grid)),
                 [<<"ah-pg-content">>],
                 [{id, sub_id(Id, <<"content">>)}, {tabindex, 0}, {role, grid},
                  {aria_label, maps:get(<<"pivot">>, Labels)}]),
           menu(Id, Labels),
           ?H:el('div', [], [<<"ah-pg-resize-line">>], [{aria_hidden, <<"true">>}]),
           hidden(Name, Layout),
           [island(M, Data) || Remote =:= []]],
          Classes,
          [[{id, Id}, {data_ah, <<"pivotgrid">>}, {data_ah_value, Layout},
            {data_view, json(view_json(maps:get(view, M)))},
            {data_config, json(config_json(M))},
            {data_ah_remote, Remote =/= []},
            {style, height_style(R#ah_pivotgrid.height)}],
           Remote,
           ?E:root_attrs(R, 'ah:cell-click')]).

height_style(undefined) -> undefined;
height_style(N) when is_integer(N), N > 0 -> <<"height:", (integer_to_binary(N))/binary, "px">>;
height_style(B) when is_binary(B); is_list(B) ->
    V = text(B),
    re:run(V, <<"^[0-9.]+(px|em|rem|vh|%)$">>) =/= nomatch
        orelse error({aihtml, {bad_option, height, B}}),
    <<"height:", V/binary>>;
height_style(Other) -> error({aihtml, {bad_option, height, Other}}).

%% The rows as JSON for the browser; every "<" is written <, so the
%% data can never close the script element (or open a comment).
island(M, Rows) ->
    Json = json(#{fields => field_names(M), rows => [tuple_to_list(T) || T <- Rows]}),
    Safe = binary:replace(Json, <<"<">>, <<"\\u003c">>, [global]),
    ?H:el(script, aihtml:safe(Safe), [<<"ah-pg-data">>], [{type, <<"application/json">>}]).

hidden(undefined, _) -> [];
hidden(Name, Value) -> ?H:void(input, [], [{type, hidden}, {name, Name}, {value, Value}]).

%% The context menu of the headers, cells and field chips; the behaviour
%% shows the items whose data-ctx names the context it opens for.
menu(Id, L) ->
    Item = fun(Action, Ctx, Label, Extra) ->
                   ?H:el('div', maps:get(Label, L), [<<"ah-pg-context-menu-item">>],
                         [{role, menuitem}, {tabindex, <<"-1">>}, {data_action, Action},
                          {data_ctx, Ctx} | Extra])
           end,
    Grid = <<"row col cell">>,
    ?H:el('div',
          [Item(<<"sort-asc">>, <<"row cell">>, <<"sort_asc">>, []),
           Item(<<"sort-desc">>, <<"row cell">>, <<"sort_desc">>, []),
           Item(<<"sort-value-asc">>, <<"col cell">>, <<"sort_value_asc">>, []),
           Item(<<"sort-value-desc">>, <<"col cell">>, <<"sort_value_desc">>, []),
           Item(<<"sort-cols-asc">>, <<"col">>, <<"sort_cols_asc">>, []),
           Item(<<"sort-cols-desc">>, <<"col">>, <<"sort_cols_desc">>, []),
           Item(<<"sort-clear">>, Grid, <<"sort_clear">>, []),
           Item(<<"expand-all">>, Grid, <<"expand_all">>, []),
           Item(<<"collapse-all">>, Grid, <<"collapse_all">>, []),
           Item(<<"export-xlsx">>, Grid, <<"export_xlsx">>, []),
           Item(<<"export-csv">>, Grid, <<"export_csv">>, []),
           Item(<<"move-rows">>, <<"fields columns">>, <<"move_rows">>, []),
           Item(<<"move-columns">>, <<"fields rows">>, <<"move_columns">>, []),
           Item(<<"move-values">>, <<"fields rows columns">>, <<"move_values">>, []),
           Item(<<"move-left">>, <<"rows columns values">>, <<"move_left">>, []),
           Item(<<"move-right">>, <<"rows columns values">>, <<"move_right">>, []),
           [Item(<<"agg">>, <<"values">>, A, [{role, menuitemradio}, {data_agg, A}])
            || A <- ?AGGS],
           Item(<<"remove">>, <<"rows columns values">>, <<"remove">>, [])],
          [<<"ah-pg-context-menu">>],
          [{id, sub_id(Id, <<"menu">>)}, {role, menu}]).

%%%===================================================================
%%% Model: the normalised options, as the browser gets them
%%%===================================================================

model(#ah_pivotgrid{items = Items, value = Layout, fields = Fields0, view = View0} = R, Id) ->
    Fields = case Fields0 of
                 undefined -> [field(K) || K <- data_keys(Items)];
                 L -> [field(F) || F <- L]
             end,
    Names = [N || #{name := N} <- Fields],
    length(lists:usort(Names)) =:= length(Names) orelse error({aihtml, {bad_option, fields, Fields0}}),
    Bool = fun(K, V) -> is_boolean(V) orelse error({aihtml, {bad_option, K, V}}), V end,
    View = case R#ah_pivotgrid.expand_all of
               true -> maps:merge(#{expanded_rows => all, expanded_cols => all}, View0);
               false -> View0
           end,
    Locale = R#ah_pivotgrid.locale,
    lists:member(Locale, [en, zh]) orelse error({aihtml, {bad_option, locale, Locale}}),
    (layout(Layout, Fields))#{
      id => Id,
      fields => Fields,
      view => view(View),
      row_subtotals => Bool(row_subtotals, R#ah_pivotgrid.row_subtotals),
      col_subtotals => Bool(col_subtotals, R#ah_pivotgrid.col_subtotals),
      grand_totals => Bool(grand_totals, R#ah_pivotgrid.grand_totals),
      values_on_rows => R#ah_pivotgrid.values_on_rows,
      field_list => R#ah_pivotgrid.field_list,
      format => format(R#ah_pivotgrid.format),
      labels => labels(Locale, R#ah_pivotgrid.labels)}.

%% The model from the JSON a remote event carries (config_json/1,
%% layout_json/1 and view_json/1 of the first render).
model_from_json(Id, Conf, Layout, View) ->
    Fields = [#{name => N, label => L, format => json_format(F), agg => agg(A)}
              || #{<<"name">> := N, <<"label">> := L, <<"format">> := F, <<"agg">> := A}
                     <- maps:get(<<"fields">>, Conf)],
    (layout(#{rows => maps:get(<<"rows">>, Layout, []),
              columns => maps:get(<<"columns">>, Layout, []),
              values => [value_spec_json(V) || V <- maps:get(<<"values">>, Layout, [])]},
            Fields))#{
      id => text(Id),
      fields => Fields,
      view => view(#{expanded_rows => maps:get(<<"expanded_rows">>, View, []),
                     expanded_cols => maps:get(<<"expanded_cols">>, View, []),
                     row_sort => json_sort(maps:get(<<"row_sort">>, View, null)),
                     col_sort => maps:get(<<"col_sort">>, View, <<"asc">>)}),
      row_subtotals => maps:get(<<"row_subtotals">>, Conf, true) =:= true,
      col_subtotals => maps:get(<<"col_subtotals">>, Conf, true) =:= true,
      grand_totals => maps:get(<<"grand_totals">>, Conf, true) =:= true,
      values_on_rows => maps:get(<<"values_on_rows">>, Conf, false) =:= true,
      field_list => maps:get(<<"field_list">>, Conf, false) =:= true,
      format => json_format(maps:get(<<"format">>, Conf, null)),
      labels => maps:get(<<"labels">>, Conf)}.

value_spec_json(#{<<"agg">> := A} = V) ->
    L = maps:get(<<"label">>, V, null),
    case maps:get(<<"field">>, V, null) of
        null -> #{agg => A, label => L};
        F -> #{field => F, agg => A, label => L}
    end;
value_spec_json(Other) -> error({aihtml, {bad_option, value, Other}}).

json_sort(null) -> null;
json_sort(#{<<"by">> := <<"value">>} = S) ->
    #{by => value, dir => maps:get(<<"dir">>, S), col => maps:get(<<"col">>, S, []),
      vi => maps:get(<<"vi">>, S, 0)};
json_sort(#{<<"by">> := _} = S) -> #{by => key, dir => maps:get(<<"dir">>, S)};
json_sort(Other) -> error({aihtml, {bad_option, view, Other}}).

json_format(null) -> format(#{});
json_format(F) when is_map(F) ->
    format(maps:from_list([{binary_to_existing_atom(K), V} || K := V <- F, V =/= null]));
json_format(Other) -> error({aihtml, {bad_option, format, Other}}).

field(#{name := N} = F) ->
    maps:foreach(fun(K, _) -> lists:member(K, [name, label, format, agg])
                                  orelse error({aihtml, {bad_option, fields, F}})
                 end, F),
    #{name => name(N), label => text(maps:get(label, F, name(N))),
      format => case maps:find(format, F) of
                    {ok, Fmt} -> format(Fmt);
                    error -> null
                end,
      agg => agg(maps:get(agg, F, sum))};
field({N, L}) -> field(#{name => N, label => L});
field(N) when is_atom(N); is_binary(N) -> field(#{name => N});
field(Other) -> error({aihtml, {bad_option, fields, Other}}).

%% the keys of the data rows, when no `fields' are given
data_keys(Items) ->
    lists:usort([name(K) || Row <- Items, K <- maps:keys(row_map(Row))]).

field_names(#{fields := Fields}) -> [N || #{name := N} <- Fields].

layout(L, Fields) when is_map(L) ->
    Names = [N || #{name := N} <- Fields],
    maps:foreach(fun(K, _) -> lists:member(K, [rows, columns, values])
                                  orelse error({aihtml, {bad_option, value, L}})
                 end, L),
    Dim = fun(K) ->
                  Fs = [name(F) || F <- list(maps:get(K, L, []), value)],
                  [lists:member(F, Names) orelse error({aihtml, {bad_option, value, F}})
                   || F <- Fs],
                  Fs
          end,
    Rows = Dim(rows),
    Cols = Dim(columns),
    [error({aihtml, {bad_option, value, F}}) || F <- Rows, lists:member(F, Cols)],
    (length(lists:usort(Rows ++ Cols)) =:= length(Rows ++ Cols))
        orelse error({aihtml, {bad_option, value, L}}),
    #{rows => Rows, columns => Cols,
      values => [value_spec(V, Fields) || V <- list(maps:get(values, L, []), value)]}.

list(L, _) when is_list(L) -> L;
list(Other, K) -> error({aihtml, {bad_option, K, Other}}).

value_spec(#{} = V, Fields) ->
    maps:foreach(fun(K, _) -> lists:member(K, [field, agg, label])
                                  orelse error({aihtml, {bad_option, value, V}})
                 end, V),
    Field = case maps:find(field, V) of
                {ok, F} when F =/= null, F =/= undefined ->
                    N = name(F),
                    lists:member(N, [Nm || #{name := Nm} <- Fields])
                        orelse error({aihtml, {bad_option, value, F}}),
                    N;
                _ -> null
            end,
    Agg = case {maps:find(agg, V), Field} of
              {{ok, A}, _} -> agg(A);
              {error, null} -> <<"count">>;
              {error, _} -> hd([Ag || #{name := N, agg := Ag} <- Fields, N =:= Field])
          end,
    (Field =/= null orelse Agg =:= <<"count">>) orelse error({aihtml, {bad_option, value, V}}),
    #{field => Field, agg => Agg,
      label => case maps:get(label, V, null) of
                   null -> null;
                   L -> text(L)
               end};
value_spec({F, A}, Fields) -> value_spec(#{field => F, agg => A}, Fields);
value_spec(F, Fields) when is_atom(F); is_binary(F) -> value_spec(#{field => F}, Fields);
value_spec(Other, _) -> error({aihtml, {bad_option, value, Other}}).

agg(A) when is_atom(A) -> agg(atom_to_binary(A));
agg(A) when is_binary(A) ->
    lists:member(A, ?AGGS) orelse error({aihtml, {bad_option, agg, A}}),
    A;
agg(Other) -> error({aihtml, {bad_option, agg, Other}}).

format(F) when is_map(F) ->
    maps:foreach(fun(K, _) -> lists:member(K, [decimals, thousands, decimal, prefix, suffix])
                                  orelse error({aihtml, {bad_option, format, F}})
                 end, F),
    D = maps:get(decimals, F, null),
    (D =:= null orelse (is_integer(D) andalso D >= 0 andalso D =< 20))
        orelse error({aihtml, {bad_option, format, F}}),
    #{decimals => D, thousands => text(maps:get(thousands, F, <<>>)),
      decimal => text(maps:get(decimal, F, <<".">>)),
      prefix => text(maps:get(prefix, F, <<>>)), suffix => text(maps:get(suffix, F, <<>>))};
format(Other) -> error({aihtml, {bad_option, format, Other}}).

view(V) when is_map(V) ->
    maps:foreach(fun(K, _) ->
                         lists:member(K, [expanded_rows, expanded_cols, row_sort, col_sort])
                             orelse error({aihtml, {bad_option, view, V}})
                 end, V),
    Paths = fun(all) -> all;
               (<<"all">>) -> all;
               (Ps) when is_list(Ps) -> [path(P) || P <- Ps];
               (Other) -> error({aihtml, {bad_option, view, Other}})
            end,
    #{expanded_rows => Paths(maps:get(expanded_rows, V, [])),
      expanded_cols => Paths(maps:get(expanded_cols, V, [])),
      row_sort => sort_spec(maps:get(row_sort, V, null)),
      col_sort => dir(maps:get(col_sort, V, asc))}.

path(P) when is_list(P) -> [cell(K) || K <- P];
path(Other) -> error({aihtml, {bad_option, view, Other}}).

sort_spec(null) -> null;
sort_spec(undefined) -> null;
sort_spec(#{by := key} = S) -> #{by => <<"key">>, dir => dir(maps:get(dir, S, asc))};
sort_spec(#{by := value} = S) ->
    Vi = maps:get(vi, S, 0),
    (is_integer(Vi) andalso Vi >= 0) orelse error({aihtml, {bad_option, view, S}}),
    #{by => <<"value">>, dir => dir(maps:get(dir, S, asc)), col => path(maps:get(col, S, [])),
      vi => Vi};
sort_spec(Other) -> error({aihtml, {bad_option, view, Other}}).

dir(asc) -> <<"asc">>;
dir(desc) -> <<"desc">>;
dir(<<"asc">>) -> <<"asc">>;
dir(<<"desc">>) -> <<"desc">>;
dir(Other) -> error({aihtml, {bad_option, view, Other}}).

-define(LABELS_EN,
        #{pivot => <<"Pivot table">>, subtotal => <<"Subtotal">>,
          grand_total => <<"Grand Total">>, empty => <<"No data to display">>,
          blank => <<"(blank)">>, values => <<"Values">>, fields => <<"Fields">>,
          rows => <<"Rows">>, columns => <<"Columns">>, drop => <<"Drop fields here">>,
          sort_asc => <<"Sort rows A to Z">>, sort_desc => <<"Sort rows Z to A">>,
          sort_value_asc => <<"Sort rows by this column, ascending">>,
          sort_value_desc => <<"Sort rows by this column, descending">>,
          sort_cols_asc => <<"Sort columns A to Z">>, sort_cols_desc => <<"Sort columns Z to A">>,
          sort_clear => <<"Clear sort">>, expand_all => <<"Expand all">>,
          collapse_all => <<"Collapse all">>, export_xlsx => <<"Export to Excel">>,
          export_csv => <<"Export to CSV">>, move_rows => <<"Move to rows">>,
          move_columns => <<"Move to columns">>, move_values => <<"Add to values">>,
          move_left => <<"Move left">>, move_right => <<"Move right">>, remove => <<"Remove">>,
          sum => <<"Sum">>, count => <<"Count">>, avg => <<"Average">>, min => <<"Min">>,
          max => <<"Max">>, product => <<"Product">>}).

-define(LABELS_ZH,
        #{pivot => <<"透视表"/utf8>>, subtotal => <<"小计"/utf8>>,
          grand_total => <<"合计"/utf8>>, empty => <<"暂无数据"/utf8>>,
          blank => <<"（空白）"/utf8>>, values => <<"值"/utf8>>, fields => <<"字段"/utf8>>,
          rows => <<"行"/utf8>>, columns => <<"列"/utf8>>, drop => <<"拖入字段"/utf8>>,
          sort_asc => <<"行升序排列"/utf8>>, sort_desc => <<"行降序排列"/utf8>>,
          sort_value_asc => <<"按此列升序排列行"/utf8>>,
          sort_value_desc => <<"按此列降序排列行"/utf8>>,
          sort_cols_asc => <<"列升序排列"/utf8>>, sort_cols_desc => <<"列降序排列"/utf8>>,
          sort_clear => <<"清除排序"/utf8>>, expand_all => <<"全部展开"/utf8>>,
          collapse_all => <<"全部折叠"/utf8>>, export_xlsx => <<"导出 Excel"/utf8>>,
          export_csv => <<"导出 CSV"/utf8>>, move_rows => <<"移到行"/utf8>>,
          move_columns => <<"移到列"/utf8>>, move_values => <<"添加到值"/utf8>>,
          move_left => <<"左移"/utf8>>, move_right => <<"右移"/utf8>>, remove => <<"移除"/utf8>>,
          sum => <<"求和"/utf8>>, count => <<"计数"/utf8>>, avg => <<"平均值"/utf8>>,
          min => <<"最小值"/utf8>>, max => <<"最大值"/utf8>>, product => <<"乘积"/utf8>>}).

labels(Locale, Custom) when is_map(Custom) ->
    Defaults = case Locale of
                   en -> ?LABELS_EN;
                   zh -> ?LABELS_ZH
               end,
    maps:foreach(fun(K, _) -> maps:is_key(K, Defaults)
                                  orelse error({aihtml, {bad_option, labels, K}})
                 end, Custom),
    #{atom_to_binary(K) => text(V) || K := V <- maps:merge(Defaults, Custom)}.

%% What the root carries for the browser (and for a remote event).
config_json(M) ->
    maps:with([fields, row_subtotals, col_subtotals, grand_totals, values_on_rows,
               field_list, format, labels], M).

layout_json(M) ->
    json(maps:with([rows, columns, values], M)).

view_json(V) -> V.

json(T) -> iolist_to_binary(aihtml_json:encode(T)).

%%%===================================================================
%%% Data
%%%===================================================================

%% The rows as tuples of normalised cells, in the order of `Names'.
data(Items, Names) when is_list(Items) ->
    Keys = [{N, try binary_to_existing_atom(N) catch error:badarg -> undefined end}
            || N <- Names],
    [list_to_tuple([cell(lookup(K, row_map(Row))) || K <- Keys]) || Row <- Items];
data(Other, _) -> error({aihtml, {bad_option, items, Other}}).

row_map(M) when is_map(M) -> M;
row_map(L) when is_list(L) -> maps:from_list(L);
row_map(Other) -> error({aihtml, {bad_pivot_row, Other}}).

lookup({Bin, Atom}, M) ->
    case M of
        #{Bin := V} -> V;
        #{Atom := V} when Atom =/= undefined -> V;
        _ -> null
    end.

%% A value as the browser sees it after JSON: integers, non-integral
%% floats, texts and null.
cell(null) -> null;
cell(undefined) -> null;
cell(I) when is_integer(I) -> I;
cell(F) when is_float(F) ->
    case F == trunc(F) andalso abs(F) < 9.0e15 of
        true -> trunc(F);
        false -> F
    end;
cell(B) when is_binary(B) -> B;
cell(true) -> <<"true">>;
cell(false) -> <<"false">>;
cell(A) when is_atom(A) -> atom_to_binary(A);
cell({Y, M, D} = Date) when is_integer(Y), is_integer(M), is_integer(D) ->
    calendar:valid_date(Date) orelse error({aihtml, {bad_pivot_value, Date}}),
    iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0B", [Y, M, D]));
cell(L) when is_list(L) ->
    case unicode:characters_to_binary(L) of
        B when is_binary(B) -> B;
        _ -> error({aihtml, {bad_pivot_value, L}})
    end;
cell(Other) -> error({aihtml, {bad_pivot_value, Other}}).

%%%===================================================================
%%% Engine: the same steps as pgEngine in pivotgrid.js
%%%===================================================================

%% The measures shown: the layout's, or a count of the records when
%% there are none.
eff_values(#{values := []}) -> [#{field => null, agg => <<"count">>, label => null}];
eff_values(#{values := Vs}) -> Vs.

%% One pass over the rows: the accumulators of every (row prefix, column
%% prefix) pair and the children of every member of both axes.
aggregate(M, Rows, Vs) ->
    Idx = maps:from_list(lists:zip(field_names(M), lists:seq(1, length(field_names(M))))),
    RI = [maps:get(F, Idx) || F <- maps:get(rows, M)],
    CI = [maps:get(F, Idx) || F <- maps:get(columns, M)],
    VI = [case F of null -> row; _ -> maps:get(F, Idx) end || #{field := F} <- Vs],
    Empty = [{0, 0, 0, null, null, 1} || _ <- Vs],
    lists:foldl(
      fun(T, {Accs, RK, CK}) ->
              RKey = [element(I, T) || I <- RI],
              CKey = [element(I, T) || I <- CI],
              Vals = [case I of row -> row; _ -> element(I, T) end || I <- VI],
              Accs1 = lists:foldl(
                        fun(Key, A) ->
                                A#{Key => lists:zipwith(fun add/2, maps:get(Key, A, Empty), Vals)}
                        end, Accs, [{RP, CP} || RP <- prefixes(RKey), CP <- prefixes(CKey)]),
              {Accs1, kids(RKey, RK), kids(CKey, CK)}
      end, {#{}, #{}, #{}}, Rows).

prefixes(Key) -> [lists:sublist(Key, N) || N <- lists:seq(0, length(Key))].

kids(Key, K) ->
    lists:foldl(fun(N, Acc) ->
                        P = lists:sublist(Key, N),
                        Acc#{P => (maps:get(P, Acc, #{}))#{lists:nth(N + 1, Key) => true}}
                end, K, lists:seq(0, length(Key) - 1)).

add({S, NN, N, Mi, Ma, P}, row) -> {S, NN, N + 1, Mi, Ma, P};
add(A, null) -> A;
add({S, NN, N, Mi, Ma, P}, V) when is_number(V) ->
    {S + V, NN + 1, N + 1, pick_min(Mi, V), pick_max(Ma, V), P * V};
add({S, NN, N, Mi, Ma, P}, _) -> {S, NN, N + 1, Mi, Ma, P}.

pick_min(null, V) -> V;
pick_min(M, V) when V < M -> V;
pick_min(M, _) -> M.

pick_max(null, V) -> V;
pick_max(M, V) when V > M -> V;
pick_max(M, _) -> M.

result(<<"count">>, {_, _, N, _, _, _}) when N > 0 -> N;
result(_, {_, 0, _, _, _, _}) -> null;
result(<<"sum">>, {S, _, _, _, _, _}) -> S;
result(<<"avg">>, {S, NN, _, _, _, _}) -> S / NN;
result(<<"min">>, {_, _, _, Mi, _, _}) -> Mi;
result(<<"max">>, {_, _, _, _, Ma, _}) -> Ma;
result(<<"product">>, {_, _, _, _, _, P}) -> P;
result(_, _) -> null.

value_at(Accs, Vs, RP, CP, Vi) ->
    case maps:find({RP, CP}, Accs) of
        {ok, As} -> result(maps:get(agg, lists:nth(Vi + 1, Vs)), lists:nth(Vi + 1, As));
        error -> null
    end.

%% Keys order: numbers, then texts (by code point), then blanks.
rank(K) when is_number(K) -> 0;
rank(null) -> 2;
rank(_) -> 1.

kcmp(A, B) ->
    case {rank(A), rank(B)} of
        {RA, RB} when RA < RB -> -1;
        {RA, RB} when RA > RB -> 1;
        _ when A < B -> -1;
        _ when A > B -> 1;
        _ -> 0
    end.

sort_keys(Keys, Cmp) -> lists:sort(fun(A, B) -> Cmp(A, B) =< 0 end, Keys).

%% The comparator of the children of `Parent' on one axis.
key_cmp(<<"desc">>) -> fun(A, B) -> kcmp(B, A) end;
key_cmp(_) -> fun kcmp/2.

value_cmp(Accs, Vs, Parent, #{dir := Dir, col := Col, vi := Vi}) ->
    Vi1 = min(Vi, length(Vs) - 1),
    fun(A, B) ->
            VA = value_at(Accs, Vs, Parent ++ [A], Col, Vi1),
            VB = value_at(Accs, Vs, Parent ++ [B], Col, Vi1),
            if VA =:= null, VB =:= null -> kcmp(A, B);
               VA =:= null -> 1;
               VB =:= null -> -1;
               VA < VB, Dir =:= <<"asc">> -> -1;
               VA < VB -> 1;
               VA > VB, Dir =:= <<"asc">> -> 1;
               VA > VB -> -1;
               true -> kcmp(A, B)
            end
    end.

%% The visible members of an axis, depth first: #{path, depth, kids, exp}.
flatten(Kids, Exp, SortFun, Parent, Depth) ->
    case maps:find(Parent, Kids) of
        error -> [];
        {ok, Set} ->
            lists:append(
              [begin
                   P = Parent ++ [K],
                   Has = maps:is_key(P, Kids),
                   Open = Has andalso maps:is_key(P, Exp),
                   [#{path => P, key => K, depth => Depth, kids => Has, exp => Open}
                    | case Open of
                          true -> flatten(Kids, Exp, SortFun, P, Depth + 1);
                          false -> []
                      end]
               end || K <- SortFun(Parent, maps:keys(Set))])
    end.

expanded(all, Kids) -> maps:from_list([{P, true} || P <- maps:keys(Kids), P =/= []]);
expanded(Paths, _) -> maps:from_list([{P, true} || P <- Paths]).

%% The views of both templates, and the model with `all' expansions
%% resolved (what the root's data-view keeps).
views(M, Rows) ->
    Vs = eff_values(M),
    {Accs, RKids, CKids} = aggregate(M, Rows, Vs),
    #{view := View} = M,
    RExp = expanded(maps:get(expanded_rows, View), RKids),
    CExp = expanded(maps:get(expanded_cols, View), CKids),
    View1 = View#{expanded_rows => lists:sort(maps:keys(RExp)),
                  expanded_cols => lists:sort(maps:keys(CExp))},
    M1 = M#{view => View1},
    RSort = case maps:get(row_sort, View) of
                #{by := <<"value">>} = S when Vs =/= [] ->
                    fun(Parent, Keys) -> sort_keys(Keys, value_cmp(Accs, Vs, Parent, S)) end;
                #{dir := Dir} -> fun(_, Keys) -> sort_keys(Keys, key_cmp(Dir)) end;
                null -> fun(_, Keys) -> sort_keys(Keys, key_cmp(<<"asc">>)) end
            end,
    CSort = fun(_, Keys) -> sort_keys(Keys, key_cmp(maps:get(col_sort, View))) end,
    RowList = flatten(RKids, RExp, RSort, [], 0),
    ColList = flatten(CKids, CExp, CSort, [], 0),
    Grid = case Rows of
               [] -> #{empty => maps:get(<<"empty">>, maps:get(labels, M))};
               _ -> grid_view(M1, Vs, Accs, RowList, ColList, CKids, CExp, CSort)
           end,
    {Grid, fields_view(M1), M1}.

%%%===================================================================
%%% Views (the data of the shared templates; pivotgrid.js builds the same)
%%%===================================================================

grid_view(M, Vs, Accs, RowList, ColList, CKids, CExp, CSort) ->
    #{id := Id, labels := L, values_on_rows := VOR0} = M,
    Vc = length(Vs),
    VOR = VOR0 andalso Vc > 1,
    VcEff = case VOR of true -> 1; false -> Vc end,
    GT = maps:get(grand_totals, M),
    HasCols = maps:get(columns, M) =/= [],
    HasRows = maps:get(rows, M) =/= [],
    Grand = maps:get(<<"grand_total">>, L),
    RowSort = maps:get(row_sort, maps:get(view, M)),
    D = lists:max([0 | [length(P) || #{path := P} <- ColList]]),
    %% column leaves and the header cells, depth first
    {Leaves0, Cells0} = case HasCols of
                            true -> col_walk(M, CKids, CExp, CSort, [], 1, D, VcEff);
                            false -> {[], []}
                        end,
    Leaves = Leaves0 ++ [#{kind => grand, agg => []} || GT orelse not HasCols],
    Phys = [{Leaf, Vi} || Leaf <- Leaves, Vi <- lists:seq(0, VcEff - 1)],
    Indexed = lists:zip(lists:seq(0, length(Phys) - 1), Phys),
    SortOf = fun(#{agg := AggPath}, Vi) ->
                     S = json([AggPath, Vi]),
                     Icon = case RowSort of
                                #{by := <<"value">>, col := AggPath, vi := Vi, dir := Dir} ->
                                    Dir;
                                _ -> <<>>
                            end,
                     {S, Icon}
             end,
    SortAttrs = fun(Leaf, Vi, Ci) ->
                        {S, Dir} = SortOf(Leaf, Vi),
                        #{sort => S, ci => integer_to_binary(Ci),
                          sort_icon => case Dir of
                                           <<"asc">> -> <<"▲"/utf8>>;
                                           <<"desc">> -> <<"▼"/utf8>>;
                                           _ -> <<>>
                                       end,
                          aria_sort => case Dir of
                                           <<"asc">> -> <<"ascending">>;
                                           <<"desc">> -> <<"descending">>;
                                           _ -> <<>>
                                       end}
                end,
    ValueRow = VcEff > 1 orelse not HasCols,
    %% the header cells that carry a physical column (data-ci) when there
    %% is no value row: every leaf cell
    LeafCells = fun(Level) ->
                        [begin
                             Extra = case {ValueRow, C} of
                                         {false, #{leaf := _}} ->
                                             Ci = leaf_index(C, Leaves),
                                             SortAttrs(lists:nth(Ci + 1, Leaves), 0, Ci);
                                         _ -> no_sort()
                                     end,
                             maps:merge(maps:without([leaf, lvl], C), Extra)
                         end || #{lvl := Lv} = C <- Cells0, Lv =:= Level]
                end,
    GrandCell = fun() ->
                        Base = #{cls => <<"ah-pg-col-th ah-pg-grand-total">>,
                                 colspan => span(VcEff), rowspan => span(D), path => <<>>,
                                 toggle => <<>>, expanded => <<>>, label => Grand},
                        case ValueRow of
                            true -> maps:merge(Base, no_sort());
                            false ->
                                Ci = length(Leaves) - 1,
                                maps:merge(Base, SortAttrs(lists:last(Leaves), 0, Ci))
                        end
                end,
    DimRows = [#{cls => <<"ah-pg-col-header-row">>,
                 cells => LeafCells(Lv) ++ [GrandCell() || Lv =:= 1, GT]}
               || Lv <- lists:seq(1, D)],
    ValRows = case ValueRow of
                  false -> [];
                  true ->
                      [#{cls => <<"ah-pg-col-header-row ah-pg-value-label-row">>,
                         cells => [maps:merge(
                                     #{cls => <<"ah-pg-col-th ah-pg-value-label",
                                                (leaf_cls(Leaf))/binary>>,
                                       colspan => <<>>, rowspan => <<>>, path => <<>>,
                                       toggle => <<>>, expanded => <<>>,
                                       label => case {HasCols, VOR} of
                                                    {false, true} -> Grand;
                                                    _ -> value_label(lists:nth(Vi + 1, Vs), M)
                                                end},
                                     SortAttrs(Leaf, Vi, Ci))
                                   || {Ci, {Leaf, Vi}} <- Indexed]}]
              end,
    %% rows
    RowEntries = case HasRows of
                     true -> RowList ++ [#{grand => true} || GT];
                     false -> [#{grand => true}]
                 end,
    VisRows = [{E, Vi} || E <- RowEntries,
                          Vi <- case VOR of true -> lists:seq(0, Vc - 1); false -> [0] end],
    RSub = maps:get(row_subtotals, M),
    IsTotalRow = fun(#{grand := true}) -> true;
                    (#{exp := Exp}) -> Exp andalso RSub
                 end,
    RowHeads = [row_head(E, Vi, VOR, Vc, IsTotalRow(E), Grand, M, Vs) || {E, Vi} <- VisRows],
    Body = [#{cls => <<"ah-pg-body-row", (tot(IsTotalRow(E)))/binary>>,
              cells => [body_cell(Id, Ri, Ci, E, Leaf, case VOR of true -> RVi; false -> PVi end,
                                  IsTotalRow(E), RSub, Accs, Vs, M)
                        || {Ci, {Leaf, PVi}} <- Indexed]}
            || {Ri, {E, RVi}} <- lists:zip(lists:seq(0, length(VisRows) - 1), VisRows)],
    #{empty => <<>>,
      corner => iolist_to_binary(lists:join(<<" / ">>, [field_label(F, M)
                                                        || F <- maps:get(rows, M)])),
      cols => [#{} || _ <- Phys],
      hrows => DimRows ++ ValRows,
      rows => RowHeads,
      body => Body}.

no_sort() -> #{sort => <<>>, ci => <<>>, sort_icon => <<>>, aria_sort => <<>>}.

span(1) -> <<>>;
span(0) -> <<>>;
span(N) -> integer_to_binary(N).

tot(true) -> <<" ah-pg-total">>;
tot(false) -> <<>>.

leaf_cls(#{kind := grand}) -> <<" ah-pg-grand-total">>;
leaf_cls(#{kind := subtotal}) -> <<" ah-pg-total">>;
leaf_cls(_) -> <<>>.

leaf_index(#{leaf := Leaf}, Leaves) -> index_of(Leaf, Leaves, 0).

index_of(X, [X | _], N) -> N;
index_of(X, [_ | T], N) -> index_of(X, T, N + 1).

%% The column tree below `Parent': its leaves (members that are not
%% expanded and the subtotals of those that are) and its header cells,
%% each tagged with its level (and, for a leaf cell, its leaf).
col_walk(M, Kids, Exp, SortFun, Parent, Level, D, VcEff) ->
    Keys = SortFun(Parent, maps:keys(maps:get(Parent, Kids))),
    CSub = maps:get(col_subtotals, M),
    L = maps:get(labels, M),
    lists:foldl(
      fun(K, {LeavesAcc, CellsAcc}) ->
              P = Parent ++ [K],
              Has = maps:is_key(P, Kids),
              Open = Has andalso maps:is_key(P, Exp),
              Label = key_label(K, M),
              Base = #{path => json(P), label => Label, lvl => Level,
                       toggle => case {Has, Open} of
                                     {false, _} -> <<>>;
                                     {true, true} -> <<"ah-pg-toggle ah-pg-toggle-open">>;
                                     {true, false} -> <<"ah-pg-toggle ah-pg-toggle-closed">>
                                 end,
                       expanded => case {Has, Open} of
                                       {false, _} -> <<>>;
                                       {true, true} -> <<"true">>;
                                       {true, false} -> <<"false">>
                                   end},
              case Open of
                  true ->
                      {SubLeaves, SubCells} = col_walk(M, Kids, Exp, SortFun, P, Level + 1, D,
                                                       VcEff),
                      Sub = #{kind => subtotal, agg => P},
                      SubCell = [#{cls => <<"ah-pg-col-th ah-pg-total">>, colspan => span(VcEff),
                                   rowspan => span(D - Level), path => <<>>, toggle => <<>>,
                                   expanded => <<>>, lvl => Level + 1, leaf => Sub,
                                   label => <<Label/binary, " ",
                                              (maps:get(<<"subtotal">>, L))/binary>>}
                                 || CSub],
                      Leaves = SubLeaves ++ [Sub || CSub],
                      Cell = Base#{cls => <<"ah-pg-col-th">>,
                                   colspan => span(length(Leaves) * VcEff), rowspan => <<>>},
                      {LeavesAcc ++ Leaves, CellsAcc ++ [Cell] ++ SubCells ++ SubCell};
                  false ->
                      Leaf = #{kind => member, agg => P},
                      Cell = Base#{cls => <<"ah-pg-col-th">>, colspan => span(VcEff),
                                   rowspan => span(D - Level + 1), leaf => Leaf},
                      {LeavesAcc ++ [Leaf], CellsAcc ++ [Cell]}
              end
      end, {[], []}, Keys).

row_head(#{grand := true}, Vi, VOR, Vc, _, Grand, M, Vs) ->
    #{cls => <<"ah-pg-row-header ah-pg-total">>, path => <<"[]">>,
      vi => vi_attr(VOR, Vi),
      head => [#{rowspan => rowspan(VOR, Vc), expanded => <<>>, indent => <<"0">>,
                 toggle => <<>>, label => Grand} || Vi =:= 0],
      vlabel => vlabel(VOR, Vi, Vs, M)};
row_head(#{path := P, key := K, depth := Depth, kids := Has, exp := Open}, Vi, VOR, Vc, Total,
         _, M, Vs) ->
    #{cls => <<"ah-pg-row-header", (tot(Total))/binary>>, path => json(P),
      vi => vi_attr(VOR, Vi),
      head => [#{rowspan => rowspan(VOR, Vc),
                 expanded => case {Has, Open} of
                                 {false, _} -> <<>>;
                                 {true, true} -> <<"true">>;
                                 {true, false} -> <<"false">>
                             end,
                 indent => integer_to_binary(Depth * 20),
                 toggle => case {Has, Open} of
                               {false, _} -> <<"ah-pg-toggle ah-pg-toggle-leaf">>;
                               {true, true} -> <<"ah-pg-toggle ah-pg-toggle-open">>;
                               {true, false} -> <<"ah-pg-toggle ah-pg-toggle-closed">>
                           end,
                 label => key_label(K, M)} || Vi =:= 0],
      vlabel => vlabel(VOR, Vi, Vs, M)}.

vi_attr(true, Vi) -> integer_to_binary(Vi);
vi_attr(false, _) -> <<>>.

rowspan(true, Vc) -> span(Vc);
rowspan(false, _) -> <<>>.

vlabel(true, Vi, Vs, M) -> value_label(lists:nth(Vi + 1, Vs), M);
vlabel(false, _, _, _) -> <<>>.

body_cell(Id, Ri, Ci, E, Leaf, Vi, TotalRow, RSub, Accs, Vs, M) ->
    RP = maps:get(path, E, []),
    Blank = maps:get(exp, E, false) andalso not RSub,
    V = case Blank of
            true -> null;
            false -> value_at(Accs, Vs, RP, maps:get(agg, Leaf), Vi)
        end,
    Cls = case Leaf of
              #{kind := grand} -> <<" ah-pg-grand-total">>;
              #{kind := subtotal} -> <<" ah-pg-total">>;
              _ -> tot(TotalRow)
          end,
    #{id => <<Id/binary, "-c", (integer_to_binary(Ri))/binary, "-",
              (integer_to_binary(Ci))/binary>>,
      cls => <<"ah-pg-cell", Cls/binary>>,
      v => raw(V),
      text => format_value(V, lists:nth(Vi + 1, Vs), M)}.

fields_view(M) ->
    #{id := Id, labels := L, fields := Fields, rows := Rows, columns := Cols, values := Vs} = M,
    Chip = fun(Zone, I, Field, Label) ->
                   #{id => <<Id/binary, "-chip-", Zone/binary, "-", (integer_to_binary(I))/binary>>,
                     zone => Zone, index => integer_to_binary(I), field => Field, label => Label}
           end,
    Zone = fun(Zone, Chips) ->
                   #{zone => Zone, label => maps:get(Zone, L), empty => maps:get(<<"drop">>, L),
                     chips => [Chip(Zone, I, F, Lb)
                               || {I, {F, Lb}} <- lists:zip(lists:seq(0, length(Chips) - 1), Chips)]}
           end,
    Used = Rows ++ Cols,
    #{zones => [Zone(<<"fields">>, [{N, Lb} || #{name := N, label := Lb} <- Fields,
                                              not lists:member(N, Used)]),
                Zone(<<"rows">>, [{F, field_label(F, M)} || F <- Rows]),
                Zone(<<"columns">>, [{F, field_label(F, M)} || F <- Cols]),
                Zone(<<"values">>, [{case F of null -> <<>>; _ -> F end, value_label(V, M)}
                                    || #{field := F} = V <- Vs])]}.

field_label(F, #{fields := Fields}) ->
    hd([L || #{name := N, label := L} <- Fields, N =:= F]).

value_label(#{label := L}, _) when L =/= null -> L;
value_label(#{field := null}, #{labels := L}) -> maps:get(<<"count">>, L);
value_label(#{field := F, agg := A}, #{labels := L} = M) ->
    <<(field_label(F, M))/binary, " (", (maps:get(A, L))/binary, ")">>.

key_label(null, #{labels := L}) -> maps:get(<<"blank">>, L);
key_label(K, _) when is_number(K) -> num(K);
key_label(K, _) -> K.

%% A number as JavaScript's String() writes it (for the usual range).
num(I) when is_integer(I) -> integer_to_binary(I);
num(F) -> float_to_binary(F, [short]).

raw(null) -> <<>>;
raw(V) when is_integer(V) -> integer_to_binary(V);
raw(V) ->
    case V == trunc(V) andalso abs(V) < 9.0e15 of
        true -> integer_to_binary(trunc(V));
        false -> num(V)
    end.

%%%===================================================================
%%% Number format (formatValue in pivotgrid.js)
%%%===================================================================

format_value(null, _, _) -> <<>>;
format_value(V, #{field := F, agg := A}, #{fields := Fields, format := Default}) ->
    Fmt = case A of
              <<"count">> -> Default#{decimals => null, prefix => <<>>, suffix => <<>>};
              _ ->
                  case [Fo || #{name := N, format := Fo} <- Fields, N =:= F, Fo =/= null] of
                      [Fo | _] -> Fo;
                      [] -> Default
                  end
          end,
    format_number(V, Fmt).

format_number(V, #{decimals := D, thousands := T, decimal := DS, prefix := P, suffix := S}) ->
    A = abs(V),
    Str = case D of
              null ->
                  case A == trunc(A) of
                      true -> integer_to_binary(trunc(A));
                      false -> to_fixed(A, 2)
                  end;
              _ -> to_fixed(A, D)
          end,
    {Int, Frac} = case binary:split(Str, <<".">>) of
                      [I, F] -> {I, [DS, F]};
                      [I] -> {I, []}
                  end,
    iolist_to_binary([P, [<<"-">> || V < 0], thousands(Int, T), Frac, S]).

thousands(Int, <<>>) -> Int;
thousands(Int, Sep) ->
    N = byte_size(Int),
    case N =< 3 of
        true -> Int;
        false ->
            First = case N rem 3 of 0 -> 3; R -> R end,
            <<H:First/binary, Rest/binary>> = Int,
            iolist_to_binary([H | [[Sep, G] || <<G:3/binary>> <= Rest]])
    end.

%% Number.prototype.toFixed for a non-negative number: the decimal
%% expansion of the exact binary value, rounded half up.
to_fixed(I, D) when is_integer(I) ->
    iolist_to_binary([integer_to_binary(I), [[$. | lists:duplicate(D, $0)] || D > 0]]);
to_fixed(F, D) ->
    <<0:1, E:11, Frac:52>> = <<F/float>>,
    {Mant, Exp} = case E of
                      0 -> {Frac, -1074};
                      _ -> {Frac + (1 bsl 52), E - 1075}
                  end,
    Scaled = case Exp >= 0 of
                 true -> (Mant bsl Exp) * pow10(D);
                 false ->
                     Num = Mant * pow10(D),
                     Den = 1 bsl (-Exp),
                     (2 * Num + Den) div (2 * Den)
             end,
    Digits = integer_to_binary(Scaled),
    case D of
        0 -> Digits;
        _ ->
            Padded = case byte_size(Digits) =< D of
                         true -> <<(binary:copy(<<"0">>, D + 1 - byte_size(Digits)))/binary,
                                   Digits/binary>>;
                         false -> Digits
                     end,
            K = byte_size(Padded) - D,
            <<IntPart:K/binary, FracPart/binary>> = Padded,
            <<IntPart/binary, ".", FracPart/binary>>
    end.

pow10(0) -> 1;
pow10(N) -> 10 * pow10(N - 1).

%%%===================================================================
%%% Remote mode
%%%===================================================================

%% @doc Answer a `source' action: aggregate `Rows' (maps, as for
%% pivotgrid/4) with the layout, view and options the event carries,
%% render the tables on the server and morph them into the page
%% (`<root id>-content' and, with a field list, `<root id>-fields'), then
%% call the behaviour method `viewLoaded'.
-spec pivotgrid_rows(aihtml_action:ctx(), aihtml_action:event(), [row()]) -> ok.
pivotgrid_rows(Ctx, #{id := Id, value := Layout, data := #{<<"view">> := View,
                                                            <<"config">> := Conf}}, Rows) ->
    M0 = model_from_json(Id, json:decode(Conf), json:decode(Layout), json:decode(View)),
    Data = data(Rows, field_names(M0)),
    {Grid, FieldsView, M} = views(M0, Data),
    RootId = maps:get(id, M),
    aihtml_action:html(Ctx, {id, sub_id(RootId, <<"content">>)},
                       aihtml_tpl:safe(tpl_pivotgrid_grid(Grid)), morph_inner),
    [aihtml_action:html(Ctx, {id, sub_id(RootId, <<"fields">>)},
                        aihtml_tpl:safe(tpl_pivotgrid_fields(FieldsView)), morph_inner)
     || maps:get(field_list, M)],
    aihtml_action:attr(Ctx, {id, RootId}, <<"data-view">>, json(view_json(maps:get(view, M)))),
    aihtml_action:call(Ctx, {id, RootId}, viewLoaded, []).

%% @doc The layout and view of an 'ah:view' (or `change') event, decoded:
%% `#{layout => #{rows, columns, values => [#{field, agg, label}]},
%% view => #{expanded_rows, expanded_cols, row_sort, col_sort}}', field
%% names and keys as binaries. The view may be given back as the `view'
%% option to restore it.
-spec pivotgrid_view(aihtml_action:event()) -> #{layout := map(), view := map()}.
pivotgrid_view(#{value := Layout, data := Data}) ->
    L = json:decode(Layout),
    V = case Data of
            #{<<"view">> := VJ} -> json:decode(VJ);
            _ -> #{}
        end,
    Dir = fun(<<"desc">>) -> desc; (_) -> asc end,
    #{layout => #{rows => maps:get(<<"rows">>, L, []),
                  columns => maps:get(<<"columns">>, L, []),
                  values => [#{field => maps:get(<<"field">>, X, null),
                               agg => binary_to_existing_atom(maps:get(<<"agg">>, X)),
                               label => maps:get(<<"label">>, X, null)}
                             || X <- maps:get(<<"values">>, L, [])]},
      view => #{expanded_rows => maps:get(<<"expanded_rows">>, V, []),
                expanded_cols => maps:get(<<"expanded_cols">>, V, []),
                row_sort => case maps:get(<<"row_sort">>, V, null) of
                                null -> null;
                                #{<<"by">> := <<"value">>} = S ->
                                    #{by => value, dir => Dir(maps:get(<<"dir">>, S)),
                                      col => maps:get(<<"col">>, S, []),
                                      vi => maps:get(<<"vi">>, S, 0)};
                                S -> #{by => key, dir => Dir(maps:get(<<"dir">>, S))}
                            end,
                col_sort => Dir(maps:get(<<"col_sort">>, V, <<"asc">>))}}.

%% @doc The cell of an 'ah:cell-click' event: `row' and `col' (member
%% paths, [] for a grand total), `filter' (field => key of both paths:
%% the records behind the cell), `field' and `agg' (the measure; field
%% is null for a record count), `value' (null when empty) and `text' (as
%% shown).
-spec pivotgrid_cell(aihtml_action:event()) ->
          #{row := list(), col := list(), filter := map(), field := binary() | null,
            agg := atom(), value := number() | null, text := binary()}.
pivotgrid_cell(#{data := #{<<"cell">> := Json}}) ->
    C = json:decode(Json),
    #{row => maps:get(<<"row">>, C), col => maps:get(<<"col">>, C),
      filter => maps:get(<<"filter">>, C), field => maps:get(<<"field">>, C),
      agg => binary_to_existing_atom(maps:get(<<"agg">>, C)),
      value => maps:get(<<"value">>, C), text => maps:get(<<"text">>, C)}.

%% @doc Functions besides the components that the aihtml facade re-exports.
-spec facade_extras() -> [{atom(), arity()}].
facade_extras() -> [{pivotgrid_rows, 3}, {pivotgrid_view, 1}, {pivotgrid_cell, 1}].

%%%===================================================================
%%% Records
%%%===================================================================

%% @doc The field names of the record.
-spec fields(atom()) -> [atom()].
fields(ah_pivotgrid) -> record_info(fields, ah_pivotgrid).

-spec render(element()) -> aihtml_html:html().
render(#ah_pivotgrid{} = R) -> render_pivotgrid(R).

ensure_id(R) ->
    Id = case element(3, R) of
             undefined -> <<"ah-pg", (integer_to_binary(erlang:unique_integer([positive])))/binary>>;
             Id0 -> text(Id0)
         end,
    {Id, setelement(3, R, Id)}.

sub_id(Id, Part) -> <<Id/binary, "-", Part/binary>>.

name(A) when is_atom(A) -> atom_to_binary(A);
name(B) when is_binary(B) -> B;
name(L) when is_list(L) -> unicode:characters_to_binary(L);
name(Other) -> error({aihtml, {bad_option, fields, Other}}).

text(undefined) -> <<>>;
text(B) when is_binary(B) -> B;
text(L) when is_list(L) -> unicode:characters_to_binary(L);
text(X) -> beamai_html_escape:to_binary(X, aihtml).

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => pivotgrid, category => data,
       signature => <<"pivotgrid(Rows, Layout, Css, Attrs)">>,
       root => <<"ah-pg">>,
       flags => [expand_all, values_on_rows, field_list],
       classes => #{expand_all => [], values_on_rows => [], field_list => []},
       options => [fields, view, row_subtotals, col_subtotals, grand_totals, format,
                   height, locale, labels, source],
       behavior => <<"pivotgrid">>,
       events => [<<"ah:cell-click">>, <<"change">>, <<"ah:view">>, <<"ah:selection">>],
       doc => <<"A pivot table: data rows grouped by row and column dimensions with sums, "
                "counts, averages, minima or maxima, subtotals and grand totals, aggregated on "
                "the server; expand, sort and re-pivot in the browser or through an action, "
                "export to Excel or CSV.">>,
       option_docs =>
           #{expand_all => <<"Every row and column member starts expanded.">>,
             values_on_rows => <<"With several measures, one row per measure under each member "
                                 "instead of one column per measure.">>,
             field_list => <<"A bar above the table with the unused fields and the rows, columns "
                             "and values areas: drag the chips, or open their menu (click, "
                             "Enter) to move them or change the aggregate; fires change.">>,
             fields => <<"The fields: names, {Name, Label} or #{name, label, format, agg} "
                         "(format: the number format as a measure; agg: the default aggregate). "
                         "Default: every key of the rows, labelled by its name.">>,
             view => <<"Initial view: #{expanded_rows => [Path] | all, expanded_cols => ..., "
                       "row_sort => #{by => key | value, dir => asc | desc, col => Path, vi => N}, "
                       "col_sort => asc | desc}, as pivotgrid_view/1 returns it.">>,
             row_subtotals => <<"Expanded row members show their totals (default true).">>,
             col_subtotals => <<"Expanded column members get a total column (default true).">>,
             grand_totals => <<"A grand total row and column (default true).">>,
             format => <<"Default number format: #{decimals, thousands, decimal, prefix, "
                         "suffix}; without decimals integers show as they are, others with 2.">>,
             height => <<"Height (px or a CSS length): the body scrolls under fixed headers.">>,
             locale => <<"en (default) or zh: the texts of totals, menus and the field list.">>,
             labels => <<"Map overriding texts: subtotal, grand_total, empty, blank, values, "
                         "fields, rows, columns, drop, sort_*, expand_all, collapse_all, "
                         "export_xlsx, export_csv, move_*, remove, sum, count, avg, min, max, "
                         "product, pivot.">>,
             source => <<"Action ref {Module, Action, Args}: remote mode. Every view change fires "
                         "'ah:view' with the layout, view and options; the action answers with "
                         "pivotgrid_rows(Ctx, Event, Rows). No data island is written.">>},
       methods =>
           [#{name => expandAll, args => <<"()">>, doc => <<"Expand every row and column member.">>},
            #{name => collapseAll, args => <<"()">>, doc => <<"Collapse every member.">>},
            #{name => expandRow, args => <<"(Path)">>, doc => <<"Expand a row member.">>},
            #{name => collapseRow, args => <<"(Path)">>, doc => <<"Collapse a row member.">>},
            #{name => expandColumn, args => <<"(Path)">>, doc => <<"Expand a column member.">>},
            #{name => collapseColumn, args => <<"(Path)">>, doc => <<"Collapse a column member.">>},
            #{name => sortRows, args => <<"(\"asc\" | \"desc\" | null)">>,
              doc => <<"Order the row members by key (null: default order).">>},
            #{name => sortByValue, args => <<"(ColPath, Vi, \"asc\" | \"desc\")">>,
              doc => <<"Order the row members by the values of a column.">>},
            #{name => sortColumns, args => <<"(\"asc\" | \"desc\")">>,
              doc => <<"Order the column members by key.">>},
            #{name => setLayout, args => <<"(Layout)">>,
              doc => <<"Re-pivot: {rows, columns, values: [{field, agg}]}; without change.">>},
            #{name => getLayout, args => <<"()">>, doc => <<"The layout (parsed data-ah-value).">>},
            #{name => getView, args => <<"()">>, doc => <<"Expanded members and sort order.">>},
            #{name => setData, args => <<"(Rows)">>,
              doc => <<"Local mode: replace the data (objects) and re-aggregate.">>},
            #{name => getSelection, args => <<"()">>,
              doc => <<"The selected cells: [{row, col, vi, value}].">>},
            #{name => clearSelection, args => <<"()">>, doc => <<"Clear the selection.">>},
            #{name => exportXlsx, args => <<"({filename, sheetName})">>,
              doc => <<"Download the table as it is shown as .xlsx (imports the xlsx chunk on demand), "
                       "with merged headers and numbers as numbers.">>},
            #{name => exportCsv, args => <<"({filename, separator})">>,
              doc => <<"Download the table as CSV (UTF-8 with BOM).">>},
            #{name => exportData, args => <<"()">>,
              doc => <<"The table as {aoa, merges}: rows of cells and header merges.">>},
            #{name => refresh, args => <<"()">>, doc => <<"Re-render (local) or re-request (remote).">>},
            #{name => viewLoaded, args => <<"()">>,
              doc => <<"Remote mode: end the loading state after pivotgrid_rows/3 morphed the "
                       "tables in; called by pivotgrid_rows itself.">>}]}].
