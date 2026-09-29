%%%-------------------------------------------------------------------
%%% @doc Internal: the column model, cells, headers and option checks
%%% shared by aihtml_treegrid and aihtml_datatable (ported from sigil's
%%% data/treegrid and data/datatable). Not part of the public API.
%%%
%%% A column is a field name, `{Field, Title}' or a map (see the type
%%% `column()'). Rows are maps; a row's key is its `key_field' (default
%%% `id'). Keys are written as text and joined with commas in the value,
%%% so they should not contain commas.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_lib_table).

-export([col/1, cols/1, value/2, raw_text/1, num/1, cell_text/1, cell_content/3, cell_raw/2,
         align_style/1, col_el/1, key_text/1, sel_keys/1, join/1, sort_opt/1, sort_key/1,
         sort_by/3, col_key/2, header_cell/5, data_cell/4, one_of/3, bool/2, field_name/2,
         check_ref/2, height_style/1, ensure_id/1, hidden/2, text/1, option_docs/0]).

-export_type([column/0, row/0, selection/0, sort/0, css_length/0, col/0]).

-define(H, aihtml_html).
-define(COLUMN_KEYS, [field, title, width, align, sortable, filterable, editable, type,
                      render, class, hidden]).

%% A column: a field name alone, `{Field, Title}', or a map. `field' is
%% the key of the value in each row (an atom key also finds the same name
%% as a binary key); `render' turns a value into HTML (fun(Value, Row));
%% `type' drives sorting, the advanced filter and the cell editor.
-type column() :: atom() | binary()
              | {atom() | binary(), aihtml_html:html()}
              | #{field := atom() | binary(),
                  title => aihtml_html:html(),
                  width => pos_integer() | binary(),
                  align => left | center | right,
                  sortable => boolean(),
                  filterable => boolean(),
                  editable => boolean(),
                  type => text | number | date | checkbox,
                  render => fun((term(), map()) -> aihtml_html:html()),
                  class => binary(),
                  hidden => boolean()}.
%% A row: a map from field names to values (binaries, numbers, atoms,
%% or HTML).
-type row() :: map().
%% Selection mode of both tables.
-type selection() :: none | single | multiple | checkbox.
%% A sort: the column's field and the direction.
-type sort() :: undefined | {atom() | binary(), asc | desc}.
%% A CSS length: pixels or a literal such as <<"60vh">>.
-type css_length() :: undefined | pos_integer() | binary().
%% A column as col/1 normalises it.
-type col() :: #{key := atom() | binary(), field := binary(), title := aihtml_html:html(),
                 width := binary() | undefined, align := left | center | right,
                 type := text | number | date | checkbox,
                 render := undefined | fun((term(), map()) -> aihtml_html:html()),
                 class := binary() | [], sortable := boolean(), filterable := boolean(),
                 editable := boolean(), hidden := boolean()}.

%%%===================================================================
%%% Columns and values
%%%===================================================================

-spec col(column()) -> col().
col(F) when is_atom(F); is_binary(F) -> col(#{field => F});
col({F, Title}) when is_atom(F); is_binary(F) -> col(#{field => F, title => Title});
col(#{field := F} = M) when is_atom(F); is_binary(F) ->
    [error({aihtml, {bad_column_key, K, M}}) || K <- maps:keys(M), not lists:member(K, ?COLUMN_KEYS)],
    Align = maps:get(align, M, left),
    lists:member(Align, [left, center, right]) orelse error({aihtml, {bad_column, align, Align}}),
    Type = maps:get(type, M, text),
    lists:member(Type, [text, number, date, checkbox]) orelse error({aihtml, {bad_column, type, Type}}),
    Width = case maps:get(width, M, undefined) of
                undefined -> undefined;
                W when is_integer(W), W > 0 -> <<(integer_to_binary(W))/binary, "px">>;
                W when is_binary(W) -> W;
                W -> error({aihtml, {bad_column, width, W}})
            end,
    Render = case maps:get(render, M, undefined) of
                 undefined -> undefined;
                 Fun when is_function(Fun, 2) -> Fun;
                 Fun -> error({aihtml, {bad_column, render, Fun}})
             end,
    Bool = fun(K, D) ->
                   case maps:get(K, M, D) of
                       B when is_boolean(B) -> B;
                       B -> error({aihtml, {bad_column, K, B}})
                   end
           end,
    #{key => F, field => text(F), title => maps:get(title, M, text(F)), width => Width,
      align => Align, type => Type, render => Render, class => maps:get(class, M, []),
      sortable => Bool(sortable, true), filterable => Bool(filterable, true),
      editable => Bool(editable, true), hidden => Bool(hidden, false)};
col(Other) -> error({aihtml, {bad_column, Other}}).

-spec cols([column()]) -> [col()].
cols(Columns) when is_list(Columns) -> [col(C) || C <- Columns];
cols(Other) -> error({aihtml, {bad_option, columns, Other}}).

%% The value of a field in a row; an atom field also finds the binary key.
-spec value(row(), atom() | binary()) -> term().
value(Row, Key) when is_map(Row) ->
    case Row of
        #{Key := V} -> V;
        _ when is_atom(Key) -> maps:get(atom_to_binary(Key, utf8), Row, undefined);
        _ ->
            try binary_to_existing_atom(Key, utf8) of
                A -> maps:get(A, Row, undefined)
            catch error:badarg -> undefined
            end
    end;
value(Row, _) -> error({aihtml, {bad_row, Row}}).

%% A value as text, or not_text for HTML.
-spec raw_text(term()) -> binary() | not_text.
raw_text(V) when V =:= undefined; V =:= null -> <<>>;
raw_text(V) when is_binary(V) -> V;
raw_text(V) when is_integer(V) -> integer_to_binary(V);
raw_text(V) when is_float(V) -> num(V);
raw_text(V) when is_atom(V) -> atom_to_binary(V, utf8);
raw_text(V) when is_list(V) ->
    case io_lib:printable_unicode_list(V) of
        true -> unicode:characters_to_binary(V);
        false -> not_text
    end;
raw_text(_) -> not_text.

-spec num(number()) -> binary().
num(V) when is_integer(V) -> integer_to_binary(V);
num(V) when is_float(V) ->
    case V == trunc(V) of
        true -> integer_to_binary(trunc(V));
        false -> float_to_binary(V, [short])
    end.

-spec cell_text(term()) -> binary().
cell_text(V) ->
    case raw_text(V) of
        not_text -> <<>>;
        T -> T
    end.

%% The content of a cell: the column's renderer, or the value as text.
-spec cell_content(col(), term(), row()) -> aihtml_html:html().
cell_content(#{render := undefined}, V, _Row) ->
    case raw_text(V) of
        not_text -> V;
        T -> ?H:el(span, T, [], [])
    end;
cell_content(#{render := Fun}, V, Row) -> Fun(V, Row).

%% The raw value the browser sorts, filters and edits on, when the cell's
%% text is not it (a renderer, a number, a typed column).
-spec cell_raw(col(), term()) -> binary() | undefined.
cell_raw(#{render := Render, type := Type}, V) ->
    case raw_text(V) of
        not_text -> undefined;
        T when Render =/= undefined; Type =/= text; is_number(V) -> T;
        _ -> undefined
    end.

-spec align_style(left | center | right) -> iodata().
align_style(Align) -> [<<"text-align:">>, atom_to_binary(Align, utf8), <<";">>].

-spec col_el(binary() | undefined) -> aihtml_html:html().
col_el(undefined) -> ?H:void(col, [], []);
col_el(W) -> ?H:void(col, [], [{style, [<<"width:">>, W, <<";min-width:">>, W, <<";">>]}]).

-spec key_text(term()) -> binary().
key_text(K) ->
    case raw_text(K) of
        not_text -> error({aihtml, {bad_key, K}});
        T -> T
    end.

%% Selected keys: a key, a list of keys or a comma separated text.
-spec sel_keys(term()) -> [binary()].
sel_keys(undefined) -> [];
sel_keys(null) -> [];
sel_keys(<<>>) -> [];
sel_keys(B) when is_binary(B) -> binary:split(B, <<",">>, [global, trim_all]);
sel_keys(L) when is_list(L) ->
    case io_lib:printable_unicode_list(L) andalso L =/= [] of
        true -> sel_keys(unicode:characters_to_binary(L));
        false -> [key_text(K) || K <- L]
    end;
sel_keys(K) -> [key_text(K)].

-spec join([iodata()]) -> binary().
join(Keys) -> iolist_to_binary(lists:join(<<",">>, Keys)).

-spec sort_opt(term()) -> sort().
sort_opt(undefined) -> undefined;
sort_opt({F, Dir} = S) when (is_atom(F) orelse is_binary(F)) andalso (Dir =:= asc orelse Dir =:= desc) ->
    S;
sort_opt(Other) -> error({aihtml, {bad_option, sort, Other}}).

%% Sort values: numbers before texts, texts without case.
-spec sort_key(term()) -> {0, number()} | {1, unicode:chardata()}.
sort_key(V) when is_number(V) -> {0, V};
sort_key(V) -> {1, string:lowercase(cell_text(V))}.

-spec sort_by([T], fun((T) -> term()), {term(), asc | desc}) -> [T].
sort_by(Items, Fun, {_, Dir}) ->
    Keyed = lists:zip([Fun(I) || I <- Items], lists:seq(1, length(Items))),
    Sorted = [lists:nth(N, Items) || {_, N} <- lists:sort(Keyed)],
    case Dir of
        asc -> Sorted;
        desc -> lists:reverse(Sorted)
    end.

%% The row key of a column field (the column's own key when it is one).
-spec col_key(atom() | binary(), [col()]) -> atom() | binary().
col_key(F, Cols) ->
    T = text(F),
    case [K || #{field := CF, key := K} <- Cols, CF =:= T] of
        [K | _] -> K;
        [] -> F
    end.

%%%===================================================================
%%% Header and cells
%%%===================================================================

%% A column header: sort state, sort icon, resize handle.
-spec header_cell(tg | dt, col(), boolean(), sort(), boolean()) -> aihtml_html:html().
header_cell(P, #{field := F, title := Title, align := Align, type := Type, sortable := ColSort,
                 hidden := Hidden},
            Sortable, Sort, Resizable) ->
    Pre = prefix(P),
    CanSort = Sortable andalso ColSort,
    Dir = case Sort of
              {SF, D} -> case text(SF) of F -> D; _ -> none end;
              _ -> none
          end,
    ?H:el(th,
          [?H:el('div',
                 [?H:el(span, Title, [<<Pre/binary, "-th-text">>], []),
                  [?H:el(span, [], [<<Pre/binary, "-sort-icon">>], [{aria_hidden, <<"true">>}])
                   || CanSort]],
                 [<<Pre/binary, "-th-content">>], []),
           [?H:el('div', [], [<<Pre/binary, "-resize-handle">>], [{aria_hidden, <<"true">>}])
            || Resizable]],
          [<<Pre/binary, "-th">>, [<<Pre/binary, "-th-sortable">> || CanSort],
           case Dir of
               asc -> <<Pre/binary, "-sort-asc">>;
               desc -> <<Pre/binary, "-sort-desc">>;
               none -> []
           end],
          [{role, columnheader}, {data_field, F}, {data_type, Type},
           {style, align_style(Align)},
           {aria_sort, case Dir of
                           asc -> <<"ascending">>;
                           desc -> <<"descending">>;
                           none -> undefined
                       end},
           {tabindex, CanSort andalso 0},
           {hidden, Hidden}]).

-spec data_cell(tg | dt, col(), row(), boolean()) -> aihtml_html:html().
data_cell(P, #{field := F, key := Key, align := Align, class := Class, hidden := Hidden} = C,
          Row, Editable) ->
    Pre = prefix(P),
    V = value(Row, Key),
    ?H:el(td, cell_content(C, V, Row),
          [<<Pre/binary, "-cell">>, [<<Pre/binary, "-cell-editable">> || Editable], Class],
          [{role, gridcell}, {data_field, F}, {style, align_style(Align)},
           {data_value, cell_raw(C, V)}, {hidden, Hidden}]).

prefix(tg) -> <<"ah-tg">>;
prefix(dt) -> <<"ah-dt">>.

%%%===================================================================
%%% Options and helpers
%%%===================================================================

%% @doc The option docs treegrid and datatable share.
-spec option_docs() -> #{atom() => binary()}.
option_docs() ->
    #{value => <<"Selected row key, or a list of keys (multiple / checkbox).">>,
      disabled => <<"Disable the whole table.">>,
      selection_mode => <<"none, single (default: a click selects, again deselects), "
                          "multiple (Ctrl / Shift click) or checkbox (a checkbox column).">>,
      key_field => <<"The row field that holds its key (default id).">>,
      sortable => <<"Click a column header to sort: ascending, descending, none "
                    "(default true; a column may say sortable => false).">>,
      sort => <<"Initial sort, {Field, asc | desc}.">>,
      alt_rows => <<"Zebra stripes on the visible rows (default true).">>,
      hover => <<"Highlight the row under the pointer (default true).">>,
      show_header => <<"Show the header row (default true).">>,
      resizable => <<"Drag the header edges to resize columns.">>,
      height => <<"Height of the table (pixels or a CSS length); the body scrolls.">>,
      empty_text => <<"Shown when there are no rows (default \"No data to display\").">>}.

-spec one_of(atom(), term(), [term()]) -> true.
one_of(K, V, L) -> lists:member(V, L) orelse error({aihtml, {bad_option, K, V}}).

-spec bool(atom(), term()) -> true.
bool(K, V) -> is_boolean(V) orelse error({aihtml, {bad_option, K, V}}).

-spec field_name(atom(), term()) -> ok.
field_name(_, F) when is_atom(F); is_binary(F) -> ok;
field_name(K, F) -> error({aihtml, {bad_option, K, F}}).

-spec check_ref(atom(), term()) -> ok.
check_ref(_, undefined) -> ok;
check_ref(_, {M, A, _}) when is_atom(M), is_atom(A) -> ok;
check_ref(K, V) -> error({aihtml, {bad_option, K, V}}).

-spec height_style(css_length()) -> iodata() | undefined.
height_style(undefined) -> undefined;
height_style(H) when is_integer(H), H > 0 -> [<<"height:">>, integer_to_binary(H), <<"px;">>];
height_style(H) when is_binary(H) -> [<<"height:">>, H, <<";">>];
height_style(H) -> error({aihtml, {bad_option, height, H}}).

%% The root needs an id: rows and the body derive theirs from it.
-spec ensure_id(T) -> {binary(), T} when T :: tuple().
ensure_id(R) ->
    Id = case element(3, R) of
             undefined -> <<"ah-dtb", (integer_to_binary(erlang:unique_integer([positive])))/binary>>;
             Id0 -> text(Id0)
         end,
    {Id, setelement(3, R, Id)}.

-spec hidden(undefined | atom() | iodata(), iodata()) -> aihtml_html:html().
hidden(undefined, _) -> [];
hidden(Name, Value) ->
    ?H:void(input, [], [{type, hidden}, {name, Name}, {value, Value}, {data_ah_input, true}]).

-spec text(term()) -> binary().
text(undefined) -> <<>>;
text(B) when is_binary(B) -> B;
text(L) when is_list(L) ->
    case unicode:characters_to_binary(L) of
        B when is_binary(B) -> B;
        _ -> error({aihtml, {bad_text, L}})
    end;
text(X) -> beamai_html_escape:to_binary(X, aihtml).
