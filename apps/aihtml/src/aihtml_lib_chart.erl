%%%-------------------------------------------------------------------
%%% @doc Internal: what the echarts components share (chart, area_chart,
%%% bar_chart, donut_chart, radar_chart, relation_graph): the root with
%%% the option's JSON data island, sizes, the common option parts of the
%%% convenience charts (title, legend, tooltip, grid, palette), series
%%% normalisation, the catalog docs and methods, and small checks. The
%%% browser side is assets/js/components/_lib_chart.js (the `chart'
%%% behaviour every one of them mounts).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_lib_chart).

-export([chart_root/8, island/1, renderer/1, size_style/2, check_option/1,
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
%% island. The caller computes `Classes' before `Option': they check the
%% modifier fields before the option does.
-spec chart_root(aihtml_element:element(), css(), option(), boolean(), boolean(), size(),
                 size(), canvas | svg) -> html().
chart_root(R, Classes, Option, Loading, Disabled, W, H, Renderer) ->
    bool(loading, Loading),
    bool(disabled, Disabled),
    ?H:el('div',
          [island(Option),
           case Disabled of
               true -> ?H:el('div', [], [<<"ah-chart-overlay">>], []);
               false -> []
           end],
          Classes,
          [[{role, img}, {data_ah, <<"chart">>},
            {data_ah_renderer, renderer(Renderer)},
            {data_ah_loading, Loading andalso <<"true">>},
            {aria_disabled, Disabled andalso <<"true">>},
            {style, size_style(W, H)}],
           ?E:root_attrs(R, 'ah:chart-click')]).

%% @doc Internal: the option as JSON in a script element. No "<" is left
%% in it (JSON allows < in strings, the only place "<" can appear), so the
%% data can neither close the script element nor open a comment in it.
-spec island(option()) -> html().
island(Option) ->
    Json = try iolist_to_binary(json:encode(Option))
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
