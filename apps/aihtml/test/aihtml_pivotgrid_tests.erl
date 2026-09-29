%% Tests for aihtml_pivotgrid. The module is also the fake action module
%% of the remote-mode round trip.
-module(aihtml_pivotgrid_tests).
-behaviour(aihtml_action).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_pivotgrid.hrl").

-export([action/4]).

-define(M, aihtml_pivotgrid).

r(Html) -> aihtml_html:render_binary(Html).

hasnt(Needle, Hay) -> binary:match(Hay, Needle) =:= nomatch.

has(Needle, Hay) ->
    case binary:match(Hay, Needle) of
        nomatch -> ?debugFmt("~ts not in~n~ts", [Needle, Hay]), false;
        _ -> true
    end.

count(Needle, Hay) -> length(binary:matches(Hay, Needle)).

%% an attribute of the root, unescaped
attr(Name, Html) ->
    {match, [V]} = re:run(Html, <<" ", Name/binary, "=\"([^\"]*)\"">>,
                          [{capture, all_but_first, binary}]),
    unescape(V).

unescape(V) ->
    lists:foldl(fun({A, B}, Acc) -> binary:replace(Acc, A, B, [global]) end, V,
                [{<<"&quot;">>, <<"\"">>}, {<<"&#39;">>, <<"'">>}, {<<"&lt;">>, <<"<">>},
                 {<<"&gt;">>, <<">">>}, {<<"&amp;">>, <<"&">>}]).

%% the text of the body cells, row by row
cells(Html) ->
    {match, Rows} = re:run(Html, <<"<tr class=\"ah-pg-body-row[^\"]*\" role=\"row\">(.*?)</tr>">>,
                           [global, {capture, all_but_first, binary}]),
    [[T || [T] <- element(2, re:run(Row, <<">([^<]*)</td>">>,
                                    [global, {capture, all_but_first, binary}]))]
     || [Row] <- Rows].

row_labels(Html) ->
    {match, Ls} = re:run(Html, <<"class=\"ah-pg-row-label\">([^<]*)<">>,
                         [global, {capture, all_but_first, binary}]),
    [L || [L] <- Ls].

sales() ->
    [#{country => <<"CN">>, city => <<"Beijing">>, year => 2023, sales => 10, units => 1},
     #{country => <<"CN">>, city => <<"Shanghai">>, year => 2023, sales => 20, units => 2},
     #{country => <<"CN">>, city => <<"Beijing">>, year => 2024, sales => 5, units => 3},
     #{country => <<"US">>, city => <<"NYC">>, year => 2024, sales => 7.5, units => 4},
     #{country => <<"US">>, city => <<"NYC">>, year => 2023, sales => null, units => 5}].

layout() -> #{rows => [country, city], columns => [year], values => [sales]}.

%%%===================================================================
%%% Rendering and aggregation
%%%===================================================================

basic_test() ->
    H = r(?M:pivotgrid(sales(), layout(), [<<"mt-2">>], [{id, pg}, {title, <<"t">>}])),
    ?assert(has(<<"<div class=\"ah-pg mt-2\" id=\"pg\" data-ah=\"pivotgrid\"">>, H)),
    ?assert(has(<<"title=\"t\"">>, H)),
    ?assert(has(<<"id=\"pg-content\" tabindex=\"0\" role=\"grid\"">>, H)),
    ?assert(has(<<"<script class=\"ah-pg-data\" type=\"application/json\">">>, H)),
    ?assert(has(<<"<th class=\"ah-pg-corner-th\">country / city</th>">>, H)),
    ?assertEqual(#{<<"rows">> => [<<"country">>, <<"city">>], <<"columns">> => [<<"year">>],
                   <<"values">> => [#{<<"field">> => <<"sales">>, <<"agg">> => <<"sum">>,
                                      <<"label">> => null}]},
                 json:decode(attr(<<"data-ah-value">>, H))),
    %% collapsed: one row per country, then the grand total; 2023, 2024, total
    ?assertEqual([<<"CN">>, <<"US">>, <<"Grand Total">>], row_labels(H)),
    ?assertEqual([[<<"30">>, <<"5">>, <<"35">>],
                  [<<>>, <<"7.50">>, <<"7.50">>],
                  [<<"30">>, <<"12.50">>, <<"42.50">>]], cells(H)),
    ?assert(has(<<"data-v=\"42.5\"">>, H)),
    ?assert(has(<<"id=\"pg-c2-2\" role=\"gridcell\"">>, H)),
    %% the context menu and the resize line
    ?assert(has(<<"id=\"pg-menu\" role=\"menu\"">>, H)),
    ?assert(has(<<"data-action=\"export-xlsx\"">>, H)),
    ?assert(has(<<"ah-pg-resize-line">>, H)).

expand_and_subtotals_test() ->
    H = r(?M:pivotgrid(sales(), layout(), [expand_all], [{id, pg}])),
    ?assertEqual([<<"CN">>, <<"Beijing">>, <<"Shanghai">>, <<"US">>, <<"NYC">>,
                  <<"Grand Total">>], row_labels(H)),
    ?assertEqual([[<<"30">>, <<"5">>, <<"35">>], [<<"10">>, <<"5">>, <<"15">>],
                  [<<"20">>, <<>>, <<"20">>], [<<>>, <<"7.50">>, <<"7.50">>],
                  [<<>>, <<"7.50">>, <<"7.50">>], [<<"30">>, <<"12.50">>, <<"42.50">>]],
                 cells(H)),
    %% expanded parents are total rows, with an open toggle and aria-expanded
    ?assert(has(<<"<tr class=\"ah-pg-row-header ah-pg-total\" role=\"row\" "
                  "data-path=\"[&quot;CN&quot;]\"><td class=\"ah-pg-row-th\" role=\"rowheader\" "
                  "aria-expanded=\"true\">">>, H)),
    ?assert(has(<<"ah-pg-toggle ah-pg-toggle-leaf">>, H)),
    ?assert(has(<<"padding-left:20px;">>, H)),
    ?assertEqual(#{<<"expanded_rows">> => [[<<"CN">>], [<<"US">>]], <<"expanded_cols">> => [],
                   <<"row_sort">> => null, <<"col_sort">> => <<"asc">>},
                 json:decode(attr(<<"data-view">>, H))),
    %% without row subtotals the expanded parents are blank
    H2 = r(?M:pivotgrid(sales(), layout(), [expand_all], [{row_subtotals, false}])),
    ?assertEqual([<<>>, <<>>, <<>>], hd(cells(H2))),
    ?assert(hasnt(<<"ah-pg-row-header ah-pg-total\" role=\"row\" data-path=\"[&quot;CN">>, H2)).

column_tree_test() ->
    L = #{rows => [country], columns => [year, city], values => [{units, count}]},
    H = r(?M:pivotgrid(sales(), L, [], [{view, #{expanded_cols => [[2023]]}}])),
    %% 2023 spans Beijing, NYC, Shanghai and its subtotal; 2024 stays a leaf
    ?assert(has(<<"colspan=\"4\" data-path=\"[2023]\" aria-expanded=\"true\"">>, H)),
    ?assert(has(<<"rowspan=\"2\" data-path=\"[2024]\" data-sort=\"[[2024],0]\" data-ci=\"4\" "
                  "aria-expanded=\"false\"">>, H)),
    ?assert(has(<<">2023 Subtotal<">>, H)),
    ?assert(has(<<"data-sort=\"[[],0]\" data-ci=\"5\"">>, H)),
    ?assertEqual([[<<"1">>, <<>>, <<"1">>, <<"2">>, <<"1">>, <<"3">>],
                  [<<>>, <<"1">>, <<>>, <<"1">>, <<"1">>, <<"2">>],
                  [<<"1">>, <<"1">>, <<"1">>, <<"3">>, <<"2">>, <<"5">>]], cells(H)),
    %% no column subtotals, no grand totals
    H2 = r(?M:pivotgrid(sales(), L, [], [{view, #{expanded_cols => [[2023]]}},
                                         {col_subtotals, false}, {grand_totals, false}])),
    ?assertEqual([[<<"1">>, <<>>, <<"1">>, <<"1">>], [<<>>, <<"1">>, <<>>, <<"1">>]], cells(H2)),
    ?assert(hasnt(<<">Grand Total<">>, H2)).

aggregates_test() ->
    Aggs = [sum, count, avg, min, max, product],
    L = #{rows => [country], columns => [], values => [{units, A} || A <- Aggs]},
    H = r(?M:pivotgrid(sales(), L, [], [])),
    ?assertEqual([[<<"6">>, <<"3">>, <<"2">>, <<"1">>, <<"3">>, <<"6">>],
                  [<<"9">>, <<"2">>, <<"4.50">>, <<"4">>, <<"5">>, <<"20">>],
                  [<<"15">>, <<"5">>, <<"3">>, <<"1">>, <<"5">>, <<"120">>]], cells(H)),
    %% without column fields the headers are the measure labels
    ?assert(has(<<">units (Average)<">>, H)),
    %% count of a field counts non-blank values; no measure counts records
    H2 = r(?M:pivotgrid(sales(), #{rows => [country], values => [{sales, count}]}, [], [])),
    ?assertEqual([[<<"3">>], [<<"1">>], [<<"4">>]], cells(H2)),
    H3 = r(?M:pivotgrid(sales(), #{rows => [country]}, [], [])),
    ?assertEqual([[<<"3">>], [<<"2">>], [<<"5">>]], cells(H3)),
    ?assert(has(<<">Count<">>, H3)).

values_on_rows_test() ->
    L = #{rows => [country], columns => [year], values => [sales, {units, max}]},
    H = r(?M:pivotgrid(sales(), L, [values_on_rows], [])),
    ?assertEqual(6, count(<<"<tr class=\"ah-pg-body-row">>, H)),
    ?assert(has(<<"data-vi=\"1\"><td class=\"ah-pg-row-th ah-pg-value-label-cell\">"
                  "units (Max)</td>">>, H)),
    ?assert(has(<<"rowspan=\"2\"><div class=\"ah-pg-row-indent\"">>, H)),
    ?assertEqual([[<<"30">>, <<"5">>, <<"35">>], [<<"2">>, <<"3">>, <<"3">>]],
                 lists:sublist(cells(H), 2)),
    %% one measure: the flag changes nothing
    Content = fun(X) -> hd(binary:split(tl_after(X, <<"id=\"a-content\"">>), <<"id=\"a-menu\"">>)) end,
    ?assertEqual(Content(r(?M:pivotgrid(sales(), layout(), [], [{id, a}]))),
                 Content(r(?M:pivotgrid(sales(), layout(), [values_on_rows], [{id, a}])))).

tl_after(B, Sep) -> lists:last(binary:split(B, Sep)).

key_order_and_blanks_test() ->
    Rows = [#{k => <<"b">>, v => 1}, #{k => 10, v => 1}, #{k => null, v => 1},
            #{k => 9, v => 1}, #{k => <<"a">>, v => 1}, #{k => 2.5, v => 1},
            #{v => 1}, #{k => true, v => 1}, #{k => {2026, 9, 29}, v => 1}],
    H = r(?M:pivotgrid(Rows, #{rows => [k], values => [v]}, [], [])),
    ?assertEqual([<<"2.5">>, <<"9">>, <<"10">>, <<"2026-09-29">>, <<"a">>, <<"b">>,
                  <<"true">>, <<"(blank)">>, <<"Grand Total">>], row_labels(H)),
    ?assertEqual([<<"2">>], lists:nth(8, cells(H))),
    %% descending
    Hd = r(?M:pivotgrid(Rows, #{rows => [k], values => [v]}, [],
                        [{view, #{row_sort => #{by => key, dir => desc}}}])),
    ?assertEqual(<<"(blank)">>, hd(row_labels(Hd))).

sort_by_value_test() ->
    L = #{rows => [city], columns => [year], values => [units]},
    H = r(?M:pivotgrid(sales(), L, [],
                       [{view, #{row_sort => #{by => value, col => [2024], vi => 0,
                                               dir => desc}}}])),
    %% NYC 4, Beijing 3, Shanghai (blank in 2024) last
    ?assertEqual([<<"NYC">>, <<"Beijing">>, <<"Shanghai">>, <<"Grand Total">>], row_labels(H)),
    ?assert(has(<<"data-sort=\"[[2024],0]\" data-ci=\"1\" aria-sort=\"descending\"">>, H)),
    ?assert(has(<<"<span class=\"ah-pg-sort-icon\">▼"/utf8>>, H)).

format_test() ->
    Rows = [#{k => a, v => 1234567.125}, #{k => b, v => -0.125}, #{k => c, v => 1.005},
            #{k => d, v => 1000}],
    F = fun(Fmt) ->
                H = r(?M:pivotgrid(Rows, #{rows => [k], values => [v]}, [],
                                   [{format, Fmt}, {grand_totals, false}])),
                [C || [C] <- cells(H)]
        end,
    ?assertEqual([<<"1234567.13">>, <<"-0.13">>, <<"1.00">>, <<"1000">>], F(#{})),
    ?assertEqual([<<"$1,234,567.13 USD">>, <<"$-0.13 USD">>, <<"$1.00 USD">>,
                  <<"$1,000.00 USD">>],
                 F(#{decimals => 2, thousands => <<",">>, prefix => <<"$">>,
                     suffix => <<" USD">>})),
    ?assertEqual([<<"1.234.567">>, <<"-0">>, <<"1">>, <<"1.000">>],
                 F(#{decimals => 0, thousands => <<".">>, decimal => <<",">>})),
    ?assertEqual([<<"1234567,1">>, <<"-0,1">>, <<"1,0">>, <<"1000,0">>],
                 F(#{decimals => 1, decimal => <<",">>})),
    %% a field's own format wins; counts ignore prefix and decimals
    H = r(?M:pivotgrid(Rows, #{rows => [k], values => [v, {v, count}]}, [],
                       [{fields, [k, #{name => v, label => <<"V">>,
                                       format => #{prefix => <<"¥"/utf8>>, decimals => 1}}]},
                        {format, #{prefix => <<"X">>}}])),
    ?assertEqual([<<"¥1000.0"/utf8>>, <<"1">>], lists:nth(4, cells(H))),
    ?assert(has(<<">V (Sum)<">>, H)).

field_list_and_labels_test() ->
    Fields = [{country, <<"国家"/utf8>>}, {city, <<"城市"/utf8>>}, year,
              #{name => sales, label => <<"销售额"/utf8>>, agg => avg}, units],
    H = r(?M:pivotgrid(sales(), #{rows => [country], columns => [year], values => [sales]},
                       [field_list], [{id, pg}, {fields, Fields}, {locale, zh},
                                      {labels, #{grand_total => <<"总计"/utf8>>}}])),
    ?assert(has(<<"<div class=\"ah-pg-fields\" id=\"pg-fields\">">>, H)),
    ?assert(has(<<"id=\"pg-chip-fields-0\" role=\"button\" tabindex=\"0\" draggable=\"true\" "
                  "aria-haspopup=\"menu\" data-zone=\"fields\" data-index=\"0\" "
                  "data-field=\"city\">城市</span>"/utf8>>, H)),
    ?assert(has(<<"data-field=\"sales\">销售额 (平均值)</span>"/utf8>>, H)),
    ?assert(has(<<"<span class=\"ah-pg-zone-label\">行</span>"/utf8>>, H)),
    ?assert(has(<<">总计<"/utf8>>, H)),
    ?assert(has(<<">按此列升序排列行<"/utf8>>, H)),
    ?assert(has(<<"<th class=\"ah-pg-corner-th\">国家</th>"/utf8>>, H)),
    Conf = json:decode(attr(<<"data-config">>, H)),
    ?assertMatch(#{<<"field_list">> := true, <<"grand_totals">> := true,
                   <<"fields">> := [#{<<"name">> := <<"country">>, <<"label">> := _,
                                      <<"agg">> := <<"sum">>, <<"format">> := null} | _]}, Conf),
    ?assertEqual(<<"总计"/utf8>>, maps:get(<<"grand_total">>, maps:get(<<"labels">>, Conf))).

empty_and_height_test() ->
    H = r(?M:pivotgrid([], #{rows => [a]}, [], [{fields, [a]}, {height, 300}])),
    ?assert(has(<<"<div class=\"ah-pg-empty\">No data to display</div>">>, H)),
    ?assert(has(<<"style=\"height:300px\"">>, H)),
    ?assert(has(<<"style=\"height:50vh\"">>,
                r(?M:pivotgrid([], #{}, [], [{height, <<"50vh">>}])))).

island_escape_test() ->
    Rows = [#{k => <<"</script><!--x">>, v => 1}],
    H = r(?M:pivotgrid(Rows, #{rows => [k], values => [v]}, [], [])),
    {match, [Island]} = re:run(H, <<"<script class=\"ah-pg-data\"[^>]*>(.*?)</script>">>,
                               [{capture, all_but_first, binary}]),
    ?assertEqual(nomatch, binary:match(Island, <<"<">>)),
    ?assertEqual(#{<<"fields">> => [<<"k">>, <<"v">>],
                   <<"rows">> => [[<<"</script><!--x">>, 1]]}, json:decode(Island)),
    %% the label is escaped as text
    ?assert(has(<<"&lt;/script&gt;&lt;!--x">>, H)).

%%%===================================================================
%%% Remote mode
%%%===================================================================

remote_test() ->
    H = r(?M:pivotgrid(sales(), layout(), [field_list],
                       [{id, rp}, {source, {?MODULE, pivot, #{}}}, {fields, [country, city, year, sales]}])),
    ?assert(hasnt(<<"ah-pg-data">>, H)),
    ?assert(has(<<"data-ah-remote">>, H)),
    ?assert(has(<<"data-ah-on=\"ah:view:">>, H)),
    %% the browser expands CN and sorts by the grand total: the event carries it
    View = #{<<"expanded_rows">> => [[<<"CN">>]], <<"expanded_cols">> => [],
             <<"row_sort">> => #{<<"by">> => <<"value">>, <<"dir">> => <<"asc">>,
                                 <<"col">> => [], <<"vi">> => 0},
             <<"col_sort">> => <<"desc">>},
    Event = #{type => <<"ah:view">>, id => <<"rp">>, value => attr(<<"data-ah-value">>, H),
              checked => null, key => null, form => #{}, values => #{},
              data => #{<<"view">> => iolist_to_binary(json:encode(View)),
                        <<"config">> => attr(<<"data-config">>, H)}},
    Ops = aihtml_action:render_ops(fun(Ctx) -> action(pivot, #{}, Event, Ctx) end),
    [#{op := html, id := <<"rp-content">>, swap := morph_inner, html := Grid},
     #{op := html, id := <<"rp-fields">>},
     #{op := attr, id := <<"rp">>, name := <<"data-view">>},
     #{op := call, id := <<"rp">>, method := <<"viewLoaded">>}] = [atomize(O) || O <- Ops],
    %% same markup as a first render of that view
    Local = r(?M:pivotgrid(sales(), layout(), [field_list],
                           [{id, rp}, {fields, [country, city, year, sales]},
                            {view, #{expanded_rows => [[<<"CN">>]],
                                     row_sort => #{by => value, col => [], vi => 0, dir => asc},
                                     col_sort => desc}}])),
    ?assert(has(Grid, Local)),
    ?assertEqual([<<"US">>, <<"CN">>, <<"Beijing">>, <<"Shanghai">>, <<"Grand Total">>],
                 row_labels(Grid)),
    ?assert(has(<<">2024<">>, hd(binary:split(Grid, <<">2023<">>)))),
    %% pivotgrid_view/1
    ?assertMatch(#{layout := #{rows := [<<"country">>, <<"city">>], columns := [<<"year">>],
                               values := [#{field := <<"sales">>, agg := sum}]},
                   view := #{expanded_rows := [[<<"CN">>]], col_sort := desc,
                             row_sort := #{by := value, dir := asc, col := [], vi := 0}}},
                 ?M:pivotgrid_view(Event)),
    %% expand all through the server
    Ops2 = aihtml_action:render_ops(
             fun(Ctx) ->
                     action(pivot, #{}, Event#{data => #{<<"view">> => <<"{\"expanded_rows\":\"all\"}">>,
                                                         <<"config">> => attr(<<"data-config">>, H)}},
                            Ctx)
             end),
    [#{html := Grid2} | _] = [atomize(O) || O <- Ops2],
    ?assertEqual([<<"CN">>, <<"Beijing">>, <<"Shanghai">>, <<"US">>, <<"NYC">>, <<"Grand Total">>],
                 row_labels(Grid2)).

atomize(M) -> maps:fold(fun(K, V, Acc) when is_binary(K) -> Acc#{binary_to_atom(K) => V};
                           (K, V, Acc) -> Acc#{K => V} end, #{}, M).

-spec action(atom(), term(), aihtml_action:event(), aihtml_action:ctx()) -> ok.
action(pivot, _, Event, Ctx) ->
    ?M:pivotgrid_rows(Ctx, Event, sales()).

cell_event_test() ->
    Cell = #{row => [<<"CN">>], col => [2023], filter => #{country => <<"CN">>, year => 2023},
             field => <<"sales">>, agg => <<"sum">>, value => 30, text => <<"30">>},
    E = #{data => #{<<"cell">> => iolist_to_binary(json:encode(Cell))}},
    ?assertEqual(#{row => [<<"CN">>], col => [2023],
                   filter => #{<<"country">> => <<"CN">>, <<"year">> => 2023},
                   field => <<"sales">>, agg => sum, value => 30, text => <<"30">>},
                 ?M:pivotgrid_cell(E)).

%%%===================================================================
%%% Validation
%%%===================================================================

validation_test() ->
    R = fun(L, Css, A) -> r(?M:pivotgrid(sales(), L, Css, A)) end,
    ?assertError({aihtml, {bad_option, value, <<"region">>}}, R(#{rows => [region]}, [], [])),
    ?assertError({aihtml, {bad_option, value, <<"country">>}},
                 R(#{rows => [country], columns => [country]}, [], [])),
    ?assertError({aihtml, {bad_option, agg, <<"median">>}},
                 R(#{values => [{sales, median}]}, [], [])),
    ?assertError({aihtml, {bad_option, value, _}}, R(#{cols => [year]}, [], [])),
    ?assertError({aihtml, {bad_option, height, <<"tall">>}}, R(#{}, [], [{height, <<"tall">>}])),
    ?assertError({aihtml, {bad_option, locale, fr}}, R(#{}, [], [{locale, fr}])),
    ?assertError({aihtml, {bad_option, labels, totl}}, R(#{}, [], [{labels, #{totl => <<"x">>}}])),
    ?assertError({aihtml, {bad_option, format, _}}, R(#{}, [], [{format, #{decimals => -1}}])),
    ?assertError({aihtml, {bad_option, view, _}}, R(#{}, [], [{view, #{row_sort => up}}])),
    ?assertError({aihtml, {bad_option, grand_totals, no}}, R(#{}, [], [{grand_totals, no}])),
    ?assertError({aihtml, {bad_option, source, _}}, R(#{}, [], [{source, fun() -> ok end}])),
    ?assertError({aihtml, {bad_pivot_value, {1, 2}}},
                 r(?M:pivotgrid([#{a => {1, 2}}], #{rows => [a]}, [], []))),
    ?assertError({aihtml, {bad_flag, pivotgrid, field_list, yes}},
                 r(#ah_pivotgrid{field_list = yes})),
    ?assertError({aihtml, {unknown_modifier, pivotgrid, big, _}},
                 ?M:pivotgrid([], #{}, [big], [])).

%%%===================================================================
%%% Catalog and records (designs/05-records.md)
%%%===================================================================

catalog_test() ->
    [#{name := pivotgrid}] = ?M:catalog(),
    E = aihtml_catalog:entry(?M, pivotgrid),
    #{flags := Fl, options := Op, option_docs := Docs, methods := Ms} = E,
    [<<"ah-pg">>] = aihtml_catalog:classes(E, Fl),
    ?assertEqual(lists:sort(Fl ++ Op), lists:sort(maps:keys(Docs))),
    ?assert(lists:member(viewLoaded, [N || #{name := N} <- Ms])),
    [?assert(erlang:function_exported(?M, F, A)) || {F, A} <- ?M:facade_extras()].

record_equals_builder_test() ->
    Fields = [country, city, year, #{name => sales, format => #{decimals => 1}}],
    ?assertEqual(r(?M:pivotgrid(sales(), layout(), [expand_all, field_list, <<"w-full">>],
                                [{id, pg}, {name, layout}, {fields, Fields}, {height, 200},
                                 {locale, zh}, {grand_totals, false},
                                 {view, #{col_sort => desc}}, {title, <<"t">>}])),
                 r(#ah_pivotgrid{items = sales(), value = layout(), expand_all = true,
                                 field_list = true, css = [<<"w-full">>], id = pg, name = layout,
                                 fields = Fields, height = 200, locale = zh,
                                 grand_totals = false, view = #{col_sort => desc},
                                 attrs = [{title, <<"t">>}]})).

builder_fills_fields_test() ->
    P = ?M:pivotgrid([], #{}, [values_on_rows, <<"x">>],
                     [{id, p}, {row_subtotals, false}, {labels, #{empty => <<"-">>}},
                      {title, <<"t">>}]),
    ?assertMatch(#ah_pivotgrid{items = [], value = #{}, values_on_rows = true, id = p,
                               row_subtotals = false, labels = #{empty := <<"-">>},
                               css = [<<"x">>], attrs = [{title, <<"t">>}]}, P),
    ?assertError({aihtml, {record_only_field, ah_pivotgrid, postback}},
                 ?M:pivotgrid([], #{}, [], [{postback, x}])).

generated_id_and_hidden_input_test() ->
    H = r(#ah_pivotgrid{items = sales(), value = layout(), name = pv}),
    {match, [Id]} = re:run(H, <<"^<div class=\"ah-pg\" id=\"(ah-pg[0-9]+)\"">>,
                           [{capture, all_but_first, binary}]),
    ?assert(has(<<"id=\"", Id/binary, "-content\"">>, H)),
    ?assert(has(<<"<input type=\"hidden\" name=\"pv\" value=\"{&quot;">>, H)),
    ?assertNotEqual(r(#ah_pivotgrid{}), r(#ah_pivotgrid{})).

postback_test() ->
    H = r(#ah_pivotgrid{id = p, postback = {cell, #{n => 1}}}),
    {match, [T]} = re:run(H, <<"data-ah-on=\"([^\"]+)\"">>, [{capture, all_but_first, binary}]),
    [<<"ah">>, <<"cell-click">>, Tok] = binary:split(T, <<":">>, [global]),
    ?assertEqual({ok, {?MODULE, cell, #{n => 1}}}, aihtml_action:unsign(Tok)).

records_match_catalog_test() ->
    Base = [module, id, css, attrs, postback, delegate],
    [begin
         Tag = list_to_atom("ah_" ++ atom_to_list(N)),
         Fields = ?M:fields(Tag),
         ?assertEqual(Base, lists:sublist(Fields, 6)),
         Defaults = maps:from_list(lists:zip(Fields, tl(tuple_to_list(#ah_pivotgrid{})))),
         [?assertEqual({N, F, false}, {N, F, maps:get(F, Defaults)})
          || F <- maps:get(flags, E, [])],
         [?assert(lists:member(O, Fields)) || O <- maps:get(options, E, [])],
         ?assertEqual(?M, maps:get(module, Defaults))
     end || #{name := N} = E <- ?M:catalog()].
