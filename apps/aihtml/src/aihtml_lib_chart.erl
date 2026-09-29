%%%-------------------------------------------------------------------
%%% @doc Internal: what the echarts components share (chart, area_chart,
%%% bar_chart, donut_chart, radar_chart, relation_graph): the root with
%%% the option's JSON data island and its readable data table (see
%%% "Readable data" below), sizes, the common option parts of the
%%% convenience charts (title, legend, tooltip, grid, palette), series
%%% normalisation, the catalog docs and methods, and small checks. The
%%% browser side is assets/js/components/_lib_chart.ts (the `chart'
%%% behaviour every one of them mounts).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_lib_chart).

-export([chart_root/8, island/1, data_text/2, data_text_update/2, renderer/1, size_style/2, check_option/1,
         value_axis/2, name_side/2, axis_chart/8, common/7, legend_ok/1, color_kv/1,
         norm_series/1, categories/2, events/0, methods/0, size_docs/0, axis_docs/0,
         bool/2, list/2, text/1]).

-export_type([option/0, text/0, size/0, series/0, legend/0]).

-define(H, aihtml_html).
-define(E, aihtml_element).

-type css() :: aihtml_html:css().
-type html() :: aihtml_html:html().
%% An echarts option as echarts documents it: maps with atom or binary
%% keys, binaries, numbers, booleans, null and lists (no tuples, and text
%% as binaries, not Erlang strings). A string "--ah-color-primary" or
%% "var(--ah-color-primary)" anywhere in it is replaced in the browser by
%% the current theme's value, again after every theme change.
-type option() :: #{atom() | binary() => term()}.
%% A name or label: a binary, an atom, a number or an Erlang string.
-type text() :: binary() | atom() | number() | string().
%% A CSS size: pixels, or any CSS length as a binary (<<"50vh">>).
-type size() :: undefined | pos_integer() | binary().
%% A data series of the axis and radar charts: `{Name, Values}' or a map
%% (`color' is a CSS colour or an "--ah-color-*" token).
-type series() :: {text(), [number() | null]}
                | #{name => text(), data := [number() | null], color => binary()}.
%% A legend position.
-type legend() :: top | bottom | left | right | none.

%%%===================================================================
%%% Root and data island
%%%===================================================================

%% @doc Internal: the root of a chart component (chart, area_chart,
%% bar_chart, donut_chart, radar_chart), `Option' drawn from its data
%% island, its data readable in a visually hidden table (data_text/2)
%% that the root names with aria-describedby. The root is a figure, not
%% an img: the children of an img are presentational, so a screen reader
%% could not move through the table's cells. The caller computes
%% `Classes' before `Option': they check the modifier fields before the
%% option does.
-spec chart_root(aihtml_element:element(), css(), option(), boolean(), boolean(), size(),
                 size(), canvas | svg) -> html().
chart_root(R, Classes, Option, Loading, Disabled, W, H, Renderer) ->
    bool(loading, Loading),
    bool(disabled, Disabled),
    {Text, TextId} = data_text(R, Option),
    ?H:el('div',
          [island(Option),
           Text,
           case Disabled of
               true -> ?H:el('div', [], [<<"ah-chart-overlay">>], []);
               false -> []
           end],
          Classes,
          [[{role, figure}, {data_ah, <<"chart">>},
            {data_ah_renderer, renderer(Renderer)},
            {data_ah_loading, Loading andalso <<"true">>},
            {aria_disabled, Disabled andalso <<"true">>},
            {aria_describedby, TextId},
            {style, size_style(W, H)}],
           ?E:root_attrs(R, 'ah:chart-click')]).

%% @doc Internal: the option as JSON in a script element. No "<" is left
%% in it (JSON allows < in strings, the only place "<" can appear), so the
%% data can neither close the script element nor open a comment in it.
-spec island(option()) -> html().
island(Option) ->
    Json = try iolist_to_binary(aihtml_json:encode(Option))
           catch error:_ -> error({aihtml, {bad_option, option, Option}})
           end,
    ?H:el(script, {safe, binary:replace(Json, <<"<">>, <<"\\u003c">>, [global])},
          [<<"ah-chart-data">>], [{type, <<"application/json">>}]).

%% @doc Internal: the data-ah-renderer value (undefined for canvas).
-spec renderer(canvas | svg) -> binary() | undefined.
renderer(R) ->
    lists:member(R, [canvas, svg]) orelse error({aihtml, {bad_option, renderer, R}}),
    case R of
        svg -> <<"svg">>;
        _ -> undefined
    end.

%% @doc Internal: the style attribute of a width and a height.
-spec size_style(size(), size()) -> binary() | undefined.
size_style(W, H) ->
    case [[P, size(K, V), $;] || {P, K, V} <- [{<<"width:">>, width, W},
                                               {<<"height:">>, height, H}],
                                  V =/= undefined] of
        [] -> undefined;
        L -> iolist_to_binary(L)
    end.

size(_, N) when is_integer(N), N > 0 -> [integer_to_binary(N), <<"px">>];
size(K, B) when is_binary(B), B =/= <<>> ->
    %% a CSS length, not a way to smuggle more declarations into style
    case binary:match(B, [<<";">>, <<"\"">>, <<"<">>, <<"{">>]) of
        nomatch -> B;
        _ -> error({aihtml, {bad_option, K, B}})
    end;
size(K, V) -> error({aihtml, {bad_option, K, V}}).

%% @doc Internal: `O' if it is an option map, else a bad_option error.
-spec check_option(term()) -> option().
check_option(O) ->
    is_map(O) orelse error({aihtml, {bad_option, option, O}}),
    O.

%%%===================================================================
%%% Readable data
%%%===================================================================

%% The classes of the readable data node (ah-sr-only: visually hidden,
%% still read by screen readers; extra/chart.css).
-define(TEXT_CSS, [<<"ah-chart-text">>, <<"ah-sr-only">>]).
%% Rows past this many are left out of the table; a last row says how many.
-define(MAX_ROWS, 500).
%% Series types drawn on a category axis whose data the axis table reads.
-define(CARTESIAN, [<<"line">>, <<"bar">>, <<"scatter">>, <<"effectScatter">>,
                    <<"pictorialBar">>]).

%% @doc Internal: the data of a chart as text that search engines and
%% screen readers can read: `{Html, Id}'. Html is a table in a visually
%% hidden div (`class="ah-chart-text ah-sr-only"'), captioned with the option's title
%% or else the root's aria-label, rows and columns read off simple option
%% shapes: a dataset source; series on a category axis (categories x
%% series); points on two value axes (x, y); a heatmap on two category
%% axes (y x x); pie and funnel data (name, value, share); radar
%% (indicators x series); a graph (nodes, their category and the nodes
%% they link to); a tree (nodes and their children). An option of another shape gives
%% just the caption, in a paragraph; without a caption either, nothing
%% (`{[], undefined}'). `Id' is the root's id plus "-data" (or a
%% generated one), for the root's aria-describedby. The browser applies
%% the same rules (dataText in _lib_chart.ts) when data changes there.
-spec data_text(aihtml_element:element(), option()) -> {html(), binary() | undefined}.
data_text(R, Option) ->
    #{id := RootId, attrs := Attrs} = ?E:base(R),
    case text_node(Option, caption(Option, Attrs)) of
        none -> {[], undefined};
        Node ->
            Id = case RootId of
                     undefined ->
                         N = erlang:unique_integer([positive]),
                         <<"ah-chart-text-", (integer_to_binary(N))/binary>>;
                     _ -> <<(text(RootId))/binary, "-data">>
                 end,
            {Node(Id), Id}
    end.

%% @doc Internal: data_text/2's node as HTML without an id (<<>> when
%% there is none), for chart_update/3: the browser puts it in place of
%% the chart's current one, keeping that one's id (and its caption when
%% the new node has none, since an update record seldom repeats the
%% aria-label).
-spec data_text_update(aihtml_element:element(), option()) -> binary().
data_text_update(R, Option) ->
    #{attrs := Attrs} = ?E:base(R),
    case text_node(Option, caption(Option, Attrs)) of
        none -> <<>>;
        Node -> ?H:render_binary(Node(undefined))
    end.

text_node(Option, Caption) ->
    case table(Option) of
        {Head0, Rows0} ->
            W = lists:max([length(Head0) | [length(Rw) || Rw <- Rows0]]),
            Head = pad(Head0, W),
            Rows = [pad(Rw, W) || Rw <- lists:sublist(Rows0, ?MAX_ROWS)],
            More = length(Rows0) - length(Rows),
            %% in a div: a table grows to its content whatever its width,
            %% so the div is what stays 1px x 1px (the overflow is clipped)
            fun(Id) ->
                    ?H:el('div', ?H:el(table,
                          [[?H:el(caption, Caption, [], []) || Caption =/= undefined],
                           ?H:el(thead, ?H:el(tr, [?H:el(th, H, [], [{scope, col}]) || H <- Head],
                                              [], []), [], []),
                           ?H:el(tbody,
                                 [[?H:el(tr, [?H:el(th, First, [], [{scope, row}])
                                              | [?H:el(td, C, [], []) || C <- Cells]], [], [])
                                   || [First | Cells] <- Rows],
                                  [?H:el(tr, ?H:el(td, <<"And ", (integer_to_binary(More))/binary,
                                                         " more rows.">>, [], [{colspan, W}]),
                                         [], []) || More > 0]],
                                 [], [])],
                          [], []), ?TEXT_CSS, [{id, Id}])
            end;
        none when Caption =/= undefined ->
            fun(Id) -> ?H:el(p, Caption, ?TEXT_CSS, [{id, Id}]) end;
        none -> none
    end.

pad(Row, W) -> Row ++ lists:duplicate(W - length(Row), <<>>).

%% The option's title, or else the root's aria-label.
caption(Option, Attrs) ->
    case [T || M <- all(get(title, Option)), T <- [str(get(text, M))], T =/= <<>>] of
        [T | _] -> T;
        [] ->
            case proplists:get_value(<<"aria-label">>, ?H:attrs(Attrs)) of
                B when is_binary(B), B =/= <<>> -> B;
                _ -> undefined
            end
    end.

%% {Head, Rows} of an option, or none.
table(O) ->
    case dataset_table(first(get(dataset, O))) of
        none -> series_table(O, all(get(series, O)));
        T -> T
    end.

dataset_table(D) ->
    case get(source, D) of
        [First | _] = Src when is_list(First) ->
            case lists:all(fun is_list/1, Src) of
                true -> {[str(C) || C <- First], [[str(C) || C <- Rw] || Rw <- tl(Src)]};
                false -> none
            end;
        [First | _] = Src when is_map(First) ->
            case lists:all(fun is_map/1, Src) of
                true ->
                    Dims = case all(get(dimensions, D)) of
                               [] -> lists:usort([str(K) || K <- maps:keys(First)]);
                               Ds -> [case is_map(Dm) of true -> str(get(name, Dm));
                                                         false -> str(Dm) end || Dm <- Ds]
                           end,
                    {Dims, [[str(field(K, Rw)) || K <- Dims] || Rw <- Src]};
                false -> none
            end;
        _ -> none
    end.

series_table(_, []) -> none;
series_table(O, Series) ->
    Types = lists:usort([str(get(type, S)) || S <- Series]),
    Is = fun(Allowed) -> lists:all(fun(T) -> lists:member(T, Allowed) end, Types) end,
    case Is([<<"pie">>, <<"funnel">>]) of
        true -> pie_table(Series);
        false ->
            case {Types, Series} of
                {[<<"radar">>], _} -> radar_table(O, Series);
                {[<<"graph">>], [S]} -> graph_table(S);
                {[<<"tree">>], [S]} -> tree_table(S);
                {[<<"heatmap">>], [S]} -> heatmap_table(O, S);
                _ ->
                    case Is(?CARTESIAN) of
                        true -> axis_table(O, Series);
                        false -> none
                    end
            end
    end.

%% Categories x series.
axis_table(O, Series) ->
    Axes = [A || A <- [first(get(xAxis, O)), first(get(yAxis, O))], is_category(A)],
    Cols = [cells(get(data, S)) || S <- Series],
    case Axes =/= [] andalso not lists:member(error, Cols) of
        false when Axes =:= [] -> xy_table(O, Series);
        false -> none;
        true ->
            [Axis | _] = Axes,
            Labels = [str(C) || C <- all(get(data, Axis))],
            N = lists:max([length(Labels) | [length(C) || C <- Cols]]),
            Name = case str(get(name, Axis)) of
                       <<>> -> <<"Category">>;
                       Nm -> Nm
                   end,
            {[Name | [series_name(S, I) || {I, S} <- lists:enumerate(Series)]],
             [[nth(I, Labels, integer_to_binary(I)) | [nth(I, C, <<>>) || C <- Cols]]
              || I <- lists:seq(1, N)]}
    end.

%% Points on two value axes: x, y (and the series, with more than one).
xy_table(O, Series) ->
    Parts = [[xy(D) || D <- all(get(data, S))] || S <- Series],
    case lists:member(error, lists:append(Parts)) of
        true -> none;
        false ->
            Multi = length(Series) > 1,
            {[<<"Series">> || Multi] ++ [axis_name(get(xAxis, O), <<"X">>),
                                         axis_name(get(yAxis, O), <<"Y">>)],
             lists:append([[[series_name(S, I) || Multi] ++ P || P <- Ps]
                           || {I, {S, Ps}} <- lists:enumerate(lists:zip(Series, Parts))])}
    end.

xy(D) when is_map(D) -> xy(get(value, D));
xy([X, Y | _]) ->
    case cells([X, Y]) of
        error -> error;
        Cs -> Cs
    end;
xy(_) -> error.

axis_name(Axis, Default) ->
    case str(get(name, first(Axis))) of
        <<>> -> Default;
        N -> N
    end.

%% A heatmap on two category axes: y categories x x categories.
heatmap_table(O, S) ->
    X = first(get(xAxis, O)),
    Y = first(get(yAxis, O)),
    Xs = [str(C) || C <- all(get(data, X))],
    Ys = [str(C) || C <- all(get(data, Y))],
    Cells = [heat_cell(D, Xs, Ys) || D <- all(get(data, S))],
    case is_category(X) andalso is_category(Y) andalso Xs =/= [] andalso Ys =/= []
        andalso not lists:member(error, Cells) of
        false -> none;
        true ->
            M = maps:from_list(Cells),
            {[axis_name(Y, <<"Category">>) | Xs],
             [[Yl | [maps:get({I, J}, M, <<>>) || I <- lists:seq(1, length(Xs))]]
              || {J, Yl} <- lists:enumerate(Ys)]}
    end.

heat_cell(D, Xs, Ys) when is_map(D) -> heat_cell(get(value, D), Xs, Ys);
heat_cell([Xi, Yi, V | _], Xs, Ys) ->
    case {axis_pos(Xi, Xs), axis_pos(Yi, Ys), cells([V])} of
        {none, _, _} -> error;
        {_, none, _} -> error;
        {_, _, error} -> error;
        {I, J, [C]} -> {{I, J}, C}
    end;
heat_cell(_, _, _) -> error.

%% The 1-based position on a category axis of a 0-based index or a label.
axis_pos(V, Labels) when is_integer(V), V >= 0, V < length(Labels) -> V + 1;
axis_pos(V, Labels) -> index(str(V), Labels).

is_category(A) when is_map(A) ->
    case str(get(type, A)) of
        <<"category">> -> true;
        <<>> -> is_list(get(data, A));
        _ -> false
    end;
is_category(_) -> false.

%% Name, value, share (and the series, with more than one).
pie_table(Series) ->
    Parts = [pie_rows(get(data, S)) || S <- Series],
    case lists:member(error, Parts) of
        true -> none;
        false ->
            Multi = length(Series) > 1,
            {[<<"Series">> || Multi] ++ [<<"Name">>, <<"Value">>, <<"Share">>],
             lists:append([[[series_name(S, I) || Multi] ++ Rw || Rw <- Rows]
                           || {I, {S, Rows}} <- lists:enumerate(lists:zip(Series, Parts))])}
    end.

pie_rows(Data) when is_list(Data) ->
    Items = [case D of
                 _ when is_map(D) -> {str(get(name, D)), get(value, D)};
                 _ -> {<<>>, D}
             end || D <- Data],
    case lists:all(fun({_, V}) -> is_number(V) orelse blank(V) end, Items) of
        false -> error;
        true ->
            Total = lists:sum([V || {_, V} <- Items, is_number(V)]),
            [[N, str(V), share(V, Total)] || {N, V} <- Items]
    end;
pie_rows(_) -> error.

share(V, Total) when is_number(V), Total > 0 ->
    <<(float_to_binary(V * 100 / Total, [{decimals, 1}]))/binary, "%">>;
share(_, _) -> <<>>.

%% Indicators x series.
radar_table(O, Series) ->
    Inds = [case is_map(I) of true -> str(get(name, I)); false -> str(I) end
            || I <- all(get(indicator, first(get(radar, O))))],
    Items = [case D of
                 _ when is_map(D) -> {str(get(name, D)), cells(get(value, D))};
                 _ -> {<<>>, cells(D)}
             end || S <- Series, D <- all(get(data, S))],
    case Inds =/= [] andalso not lists:any(fun({_, V}) -> V =:= error end, Items) of
        false -> none;
        true ->
            {[<<"Indicator">> | [case N of <<>> -> series_name(#{}, I); _ -> N end
                                 || {I, {N, _}} <- lists:enumerate(Items)]],
             [[Ind | [nth(J, V, <<>>) || {_, V} <- Items]] || {J, Ind} <- lists:enumerate(Inds)]}
    end.

%% Nodes, their category and the nodes they link to.
graph_table(S) ->
    Nodes = all(first([get(data, S), get(nodes, S)])),
    Links = all(first([get(links, S), get(edges, S)])),
    Cats = [case is_map(C) of true -> str(get(name, C)); false -> str(C) end
            || C <- all(get(categories, S))],
    case Nodes =/= [] andalso lists:all(fun is_map/1, Nodes ++ Links) of
        false -> none;
        true ->
            Ids = [{str(first([get(id, N), get(name, N)])), str(get(name, N))} || N <- Nodes],
            Names = [case Nm of <<>> -> Id; _ -> Nm end || {Id, Nm} <- Ids],
            Ends = [{node_index(get(source, L), Ids), node_index(get(target, L), Ids),
                     str(get(target, L)), str(get(value, L))} || L <- Links],
            Out = fun(I) ->
                          join([case Lb of <<>> -> T; _ -> <<T/binary, " (", Lb/binary, ")">> end
                                || {Src, Tg, Raw, Lb} <- Ends, Src =:= I,
                                   T <- [case Tg of none -> Raw; _ -> lists:nth(Tg, Names) end]])
                  end,
            {[<<"Node">>] ++ [<<"Category">> || Cats =/= []] ++ [<<"Links to">>],
             [[Name] ++ [case get(category, N) of
                             C when is_integer(C) -> nth(C + 1, Cats, <<>>);
                             C -> str(C)
                         end || Cats =/= []] ++ [Out(I)]
              || {I, {N, Name}} <- lists:enumerate(lists:zip(Nodes, Names))]}
    end.

%% The 1-based position of the node a link end names (a 0-based index,
%% an id or a name), or none.
node_index(Ref, Ids) when is_integer(Ref), Ref >= 0, Ref < length(Ids) -> Ref + 1;
node_index(Ref, Ids) ->
    R = str(Ref),
    case {index(R, [Id || {Id, _} <- Ids]), index(R, [Nm || {_, Nm} <- Ids])} of
        {none, I} -> I;
        {I, _} -> I
    end.

index(X, L) -> index(X, L, 1).
index(_, [], _) -> none;
index(X, [X | _], I) -> I;
index(X, [_ | T], I) -> index(X, T, I + 1).

%% Nodes and their children, depth first.
tree_table(S) ->
    case tree_rows(all(get(data, S))) of
        [] -> none;
        Rows -> {[<<"Node">>, <<"Children">>], Rows}
    end.

tree_rows(Nodes) ->
    lists:append(
      [begin
           Kids = [K || K <- all(get(children, N)), is_map(K)],
           Own = case str(get(id, N)) of
                     <<"__root__">> -> [];
                     _ -> [[str(get(name, N)), join([str(get(name, K)) || K <- Kids])]]
                 end,
           Own ++ tree_rows(Kids)
       end || N <- Nodes, is_map(N)]).

series_name(S, I) ->
    case str(get(name, S)) of
        <<>> -> <<"Series ", (integer_to_binary(I))/binary>>;
        N -> N
    end.

%% The cells of a data list (a value, or a map with a value), or error
%% when one is not a scalar.
cells(L) when is_list(L) ->
    Vs = [case D of
              _ when is_map(D) -> get(value, D);
              _ -> D
          end || D <- L],
    case lists:all(fun(V) -> is_number(V) orelse is_binary(V) orelse is_atom(V) end, Vs) of
        true -> [str(V) || V <- Vs];
        false -> error
    end;
cells(_) -> error.

nth(I, L, _) when I >= 1, I =< length(L) -> lists:nth(I, L);
nth(_, _, Default) -> Default.

join(L) -> iolist_to_binary(lists:join(<<", ">>, L)).

blank(V) -> V =:= undefined orelse V =:= null orelse V =:= <<"-">>.

%% A scalar as table text: numbers as JavaScript writes them, missing
%% values ("-", null) empty, anything else (lists, maps) empty too.
str(V) when is_binary(V) -> case V of <<"-">> -> <<>>; _ -> V end;
str(V) when is_integer(V) -> integer_to_binary(V);
str(V) when is_float(V), V == trunc(V), abs(V) < 1.0e15 -> integer_to_binary(trunc(V));
str(V) when is_float(V) -> float_to_binary(V, [short]);
str(V) when V =:= undefined; V =:= null -> <<>>;
str(V) when is_atom(V) -> atom_to_binary(V);
str(_) -> <<>>.

%% A key of an option map, written as an atom or a binary.
get(K, M) when is_map(M) ->
    case M of
        #{K := V} -> V;
        _ -> maps:get(atom_to_binary(K), M, undefined)
    end;
get(_, _) -> undefined.

%% A dataset row's field named K (a binary), whatever its key's type.
field(K, Row) ->
    case [V || {Kx, V} <- maps:to_list(Row), str(Kx) =:= K] of
        [V | _] -> V;
        [] -> undefined
    end.

all(undefined) -> [];
all(null) -> [];
all(L) when is_list(L) -> L;
all(X) -> [X].

%% The first defined of a list of values, or the first of a list.
first([]) -> undefined;
first([X | Rest]) when X =:= undefined; X =:= null -> first(Rest);
first([X | _]) -> X;
first(X) -> X.

%%%===================================================================
%%% Common option parts (sigil's chart helpers)
%%%===================================================================

%% @doc Internal: the value axis of area and bar charts.
-spec value_axis(boolean(), undefined | text()) -> map().
value_axis(Grid, YName) ->
    maps:merge(#{type => value, axisLine => #{show => false}, axisTick => #{show => false},
                 splitLine => #{show => Grid, lineStyle => #{type => dashed}}},
               maps:from_list([{name, text(YName)} || YName =/= undefined])).

%% @doc Internal: where the value axis name goes (none without one).
-spec name_side(undefined | text(), top | right) -> none | top | right.
name_side(undefined, _) -> none;
name_side(_, Side) -> Side.

%% @doc Internal: title, tooltip, legend, grid and palette shared by area
%% and bar charts. The grid leaves room for them and for the value axis
%% name (NameSide).
-spec axis_chart(map(), map(), [#{name := binary(), _ => _}], undefined | text(),
                 undefined | [binary()], legend(), boolean(), none | top | right) -> option().
axis_chart(Base, TooltipCfg, Series, Title, Colors, Legend, Tooltip, NameSide) ->
    legend_ok(Legend),
    HasTitle = Title =/= undefined,
    Top = 16 + case HasTitle of true -> 28; false -> 0 end
             + case Legend of top -> 28; _ -> 0 end
             + case NameSide of top -> 20; _ -> 0 end,
    Grid = #{left => 12, top => Top,
             right => case Legend of right -> 120; _ -> 20 end
                 + case NameSide of right -> 40; _ -> 0 end,
             bottom => case Legend of bottom -> 40; _ -> 12 end,
             containLabel => true},
    Grid1 = case Legend of left -> Grid#{left => 120}; _ -> Grid end,
    common(Base#{grid => Grid1}, TooltipCfg, [N || #{name := N} <- Series],
           Title, Colors, Legend, Tooltip).

%% @doc Internal: `Opt' with the title, tooltip, legend and palette of
%% any convenience chart.
-spec common(map(), map(), [binary()], undefined | text(), undefined | [binary()],
             legend(), boolean()) -> option().
common(Opt, TooltipCfg, Names, Title, Colors, Legend, Tooltip) ->
    maps:from_list(
      maps:to_list(Opt)
      ++ [{title, title(Title)} || Title =/= undefined]
      ++ [{tooltip, TooltipCfg} || Tooltip]
      ++ [{legend, legend(Legend, Names, Title =/= undefined)} || Legend =/= none]
      ++ [{color, colors(Colors)} || Colors =/= undefined]).

title(T) -> #{text => text(T), left => center}.

legend(Pos, Names, HasTitle) ->
    Place = case Pos of
                bottom -> #{bottom => 0, left => center};
                top -> #{top => case HasTitle of true -> 28; false -> 0 end, left => center};
                right -> #{right => 10, top => middle, orient => vertical};
                left -> #{left => 10, top => middle, orient => vertical}
            end,
    Place#{data => Names, type => scroll}.

%% @doc Internal: a legend position, or a bad_option error.
-spec legend_ok(term()) -> true.
legend_ok(L) ->
    lists:member(L, [top, bottom, left, right, none])
        orelse error({aihtml, {bad_option, legend, L}}).

colors(Cs) ->
    is_list(Cs) andalso lists:all(fun is_binary/1, Cs)
        orelse error({aihtml, {bad_option, colors, Cs}}),
    Cs.

%% @doc Internal: the `color' of a normalised series as a map entry.
-spec color_kv(map()) -> [{color, binary()}].
color_kv(#{color := C}) when is_binary(C) -> [{color, C}];
color_kv(#{color := C}) -> error({aihtml, {bad_color, C}});
color_kv(_) -> [].

%% @doc Internal: a series as `#{name, data}' (and `color').
-spec norm_series(series()) -> #{name := binary(), data := [number() | null],
                                 color => binary()}.
norm_series({Name, Data} = S) -> norm_series(S, #{name => Name, data => Data});
norm_series(#{data := _} = M) -> norm_series(M, M);
norm_series(Other) -> error({aihtml, {bad_series, Other}}).

norm_series(Orig, #{data := Data} = M) ->
    is_list(Data) andalso lists:all(fun(V) -> is_number(V) orelse V =:= null end, Data)
        orelse error({aihtml, {bad_series, Orig}}),
    Name = case M of
               #{name := N} -> text(N);
               _ -> <<>>
           end,
    maps:merge(maps:with([color], M), #{name => Name, data => Data}).

%% @doc Internal: the category labels; without categories the x axis
%% counts 1..N.
-spec categories([text()], [#{data := list(), _ => _}]) -> [binary()].
categories([], Series) ->
    N = lists:max([0 | [length(D) || #{data := D} <- Series]]),
    [integer_to_binary(I) || I <- lists:seq(1, N)];
categories(Cats, _) ->
    is_list(Cats) orelse error({aihtml, {bad_option, categories, Cats}}),
    [text(C) || C <- Cats].

%%%===================================================================
%%% Catalog
%%%===================================================================

%% @doc Internal: the chart events every chart component fires.
-spec events() -> [binary()].
events() ->
    [<<"ah:chart-click">>, <<"ah:chart-dblclick">>, <<"ah:chart-mouseover">>,
     <<"ah:chart-mouseout">>, <<"ah:chart-legendselectchanged">>,
     <<"ah:chart-datazoom">>, <<"ah:chart-restore">>].

%% @doc Internal: the behaviour methods of the chart components.
-spec methods() -> [#{name := atom(), args := binary(), doc := binary()}].
methods() ->
    [#{name => setOption, args => <<"(Option, NotMerge)">>,
       doc => <<"Apply an echarts option: merged into the current one, or replacing it when "
                "NotMerge is true (a list of component names replaces just those).">>},
     #{name => setData, args => <<"([Data, ...])">>,
       doc => <<"Replace the data of the series, one data list per series, in order.">>},
     #{name => resize, args => <<"()">>, doc => <<"Fit the chart to its container again.">>},
     #{name => showLoading, args => <<"()">>, doc => <<"Show the loading animation.">>},
     #{name => hideLoading, args => <<"()">>, doc => <<"Hide the loading animation.">>},
     #{name => dispatchAction, args => <<"(Action)">>,
       doc => <<"Run an echarts action, e.g. #{type => highlight, seriesIndex => 0}.">>},
     #{name => toggleSeries, args => <<"(Name)">>,
       doc => <<"Show or hide the series of this legend name.">>},
     #{name => getOption, args => <<"()">>,
       doc => <<"Return echarts' current option (browser side).">>},
     #{name => getDataURL, args => <<"(Opts)">>,
       doc => <<"Return the chart as an image data URL (browser side).">>},
     #{name => saveAsImage, args => <<"(Filename)">>,
       doc => <<"Download the chart as a PNG file.">>}].

%% @doc Internal: the option docs of the flags and size options every chart has.
-spec size_docs() -> #{atom() => binary()}.
size_docs() ->
    #{loading => <<"Show echarts' loading animation until hideLoading is called.">>,
      disabled => <<"Grey the chart out and ignore the pointer.">>,
      width => <<"Width in pixels or a CSS length (default: the container's).">>,
      height => <<"Height in pixels or a CSS length (default 400px).">>,
      renderer => <<"canvas (default) or svg.">>}.

%% @doc Internal: the option docs of the area and bar chart options.
-spec axis_docs() -> #{atom() => binary()}.
axis_docs() ->
    #{categories => <<"The category labels (default 1, 2, 3 ...).">>,
      title => <<"A title above the chart.">>,
      colors => <<"Series colours: CSS colours or \"--ah-color-*\" tokens "
                  "(default the theme palette).">>,
      y_name => <<"The name of the value axis.">>,
      legend => <<"Legend position: bottom (default), top, left, right or none.">>,
      grid => <<"Dashed grid lines on the value axis (default true).">>,
      tooltip => <<"Show a tooltip on hover (default true).">>,
      stack => <<"Stack the series on each other.">>}.

%%%===================================================================
%%% Checks
%%%===================================================================

%% @doc Internal: `V' if it is a boolean, else a bad_option error for `K'.
-spec bool(atom(), term()) -> boolean().
bool(K, V) ->
    is_boolean(V) orelse error({aihtml, {bad_option, K, V}}),
    V.

%% @doc Internal: `L' if it is a list, else a bad_option error for `K'.
-spec list(atom(), term()) -> list().
list(_, L) when is_list(L) -> L;
list(K, V) -> error({aihtml, {bad_option, K, V}}).

%% @doc Internal: a name or label as a binary.
-spec text(term()) -> binary().
text(B) when is_binary(B) -> B;
text(A) when is_atom(A) -> atom_to_binary(A);
text(I) when is_integer(I) -> integer_to_binary(I);
text(F) when is_float(F) -> float_to_binary(F, [short]);
text(L) when is_list(L) ->
    case unicode:characters_to_binary(L) of
        B when is_binary(B) -> B;
        _ -> error({aihtml, {bad_text, L}})
    end;
text(X) -> error({aihtml, {bad_text, X}}).
