%% Tests for aihtml_datagrid. The module is also the fake action module
%% of the remote round trip.
-module(aihtml_datagrid_tests).
-behaviour(aihtml_action).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_datagrid.hrl").

-export([action/4, cell/2]).

-define(M, aihtml_datagrid).

r(Html) -> aihtml_html:render_binary(Html).

has(Needle, Hay) ->
    case binary:match(Hay, Needle) of
        nomatch -> ?debugFmt("~ts not in~n~ts", [Needle, Hay]), false;
        _ -> true
    end.

has_quiet(Needle, Hay) -> binary:match(Hay, Needle) =/= nomatch.

count(Needle, Hay) -> length(binary:matches(Hay, Needle)).

cols() ->
    [#{key => id, title => <<"ID">>, width => 50},
     #{key => name, title => <<"Name">>, editable => true},
     #{key => dept, title => <<"Dept">>, type => select, editable => true,
       options => [{eng, <<"Engineering">>}, {ops, <<"Operations">>}]},
     #{key => age, title => <<"Age">>, type => number, align => right, aggregates => [sum, avg]},
     #{key => active, title => <<"Active">>, type => bool}].

rows() ->
    [#{id => I, name => N, dept => D, age => A, active => I rem 2 =:= 1}
     || {I, N, D, A} <- [{1, <<"Ann">>, eng, 30}, {2, <<"bob">>, ops, 25}, {3, <<"Cy">>, eng, 41},
                         {4, <<"Dee">>, ops, 35}, {5, <<"Eve">>, eng, 28}]].

%% The keys of the rows on the page, in order.
shown(H) ->
    {match, Ms} = re:run(H, <<"<div class=\"ah-dg-row([^\"]*)\" id=\"[^\"]+\" role=\"row\" data-key=\"([^\"]+)\"">>,
                         [global, {capture, all_but_first, binary}]),
    [K || [Cls, K] <- Ms, binary:match(Cls, <<"ah-dg-row-off">>) =:= nomatch].

%%%===================================================================
%%% Structure
%%%===================================================================

structure_test() ->
    H = r(?M:datagrid(cols(), rows(), [<<"mt-2">>], [{id, g}, {aria_label, <<"People">>}])),
    ?assert(has(<<"<div class=\"ah-dg mt-2\" id=\"g\" role=\"grid\" aria-rowcount=\"6\" "
                  "aria-colcount=\"5\" data-ah=\"datagrid\" data-ah-value=\"\" "
                  "data-ah-selection=\"single\" data-ah-edit-mode=\"dblclick\" "
                  "data-ah-header-rows=\"1\"">>, H)),
    ?assert(has(<<"aria-label=\"People\"">>, H)),
    ?assert(has(<<"<div class=\"ah-dg-container\"><div class=\"ah-dg-header-wrap\">"
                  "<div class=\"ah-dg-header\" role=\"rowgroup\">"
                  "<div class=\"ah-dg-header-row\" role=\"row\" aria-rowindex=\"1\">">>, H)),
    %% header cells: sort state, menu button, resize handle, column config
    ?assert(has(<<"<div class=\"ah-dg-header-cell ah-dg-align-left ah-dg-header-cell-resizable\" "
                  "role=\"columnheader\" data-field=\"id\" aria-sort=\"none\" style=\"width:50px\" "
                  "data-min-width=\"40\"><span class=\"ah-dg-header-cell-content\">ID</span>"
                  "<span class=\"ah-dg-header-sort-icon\" aria-hidden=\"true\"></span>"
                  "<span class=\"ah-dg-column-menu-btn\" data-field=\"id\" aria-hidden=\"true\">⋮</span>"
                  "<div class=\"ah-dg-resize-handle\" aria-hidden=\"true\"></div></div>"/utf8>>, H)),
    ?assert(has(<<"data-field=\"dept\" aria-sort=\"none\" style=\"width:100px\" data-type=\"select\" "
                  "data-editable=\"true\" data-min-width=\"40\" "
                  "data-options=\"[[&quot;eng&quot;,&quot;Engineering&quot;],"
                  "[&quot;ops&quot;,&quot;Operations&quot;]]\"">>, H)),
    ?assert(has(<<"data-aggs=\"sum,avg\"">>, H)),
    %% rows: id from the key, stripes, cells with the raw value when it differs
    ?assert(has(<<"<div class=\"ah-dg-body\" id=\"g-body\" role=\"rowgroup\">"
                  "<div class=\"ah-dg-row ah-dg-row-even\" id=\"g-r-1\" role=\"row\" data-key=\"1\" "
                  "aria-selected=\"false\" aria-rowindex=\"2\">">>, H)),
    ?assert(has(<<"id=\"g-r-2\" role=\"row\" data-key=\"2\"">>, H)),
    ?assert(has(<<"<div class=\"ah-dg-row ah-dg-row-odd\" id=\"g-r-2\"">>, H)),
    ?assert(has(<<"<div class=\"ah-dg-cell ah-dg-align-left ah-dg-cell-editable\" role=\"gridcell\" "
                  "data-field=\"dept\" data-v=\"eng\" aria-readonly=\"false\" style=\"width:100px\">"
                  "<span class=\"ah-dg-cell-content\">Engineering</span></div>">>, H)),
    ?assert(has(<<"data-field=\"active\" data-v=\"true\" style=\"width:100px\">"
                  "<span class=\"ah-dg-cell-content\">Yes</span>">>, H)),
    ?assert(has(<<"<div class=\"ah-dg-cell ah-dg-align-right\" role=\"gridcell\" data-field=\"age\" "
                  "style=\"width:100px\"><span class=\"ah-dg-cell-content\">30</span></div>">>, H)),
    ?assert(has(<<"<div class=\"ah-dg-empty-message\" hidden>No data</div></div>">>, H)),
    %% the menu, the resize line, the loading veil; no pager, status bar, query element
    ?assert(has(<<"<div class=\"ah-dg-column-menu\" id=\"g-menu\" role=\"menu\"></div>"
                  "<div class=\"ah-dg-resize-line\" aria-hidden=\"true\"></div>"
                  "<div class=\"ah-dg-loading-overlay\" style=\"display:none;\">"
                  "<div class=\"ah-dg-loading-message\">Loading...</div></div>">>, H)),
    [?assertNot(has_quiet(X, H)) || X <- [<<"ah-dg-pager">>, <<"ah-dg-statusbar">>,
                                          <<"ah-dg-query">>, <<"data-i=">>, <<"ah-dg-toolbar">>,
                                          <<"data-ah-remote">>, <<"ah-dg-row-off">>]],
    ?assertEqual([<<"1">>, <<"2">>, <<"3">>, <<"4">>, <<"5">>], shown(H)).

selection_test() ->
    H = r(?M:datagrid(cols(), rows(), [checkbox], [{id, g}, {value, [2, 4]}, {name, ids}])),
    ?assert(has(<<"aria-multiselectable=\"true\"">>, H)),
    ?assert(has(<<"data-ah-value=\"2,4\" data-ah-selection=\"checkbox\"">>, H)),
    ?assert(has(<<"<input type=\"hidden\" name=\"ids\" value=\"2,4\" data-ah-input>">>, H)),
    ?assert(has(<<"<div class=\"ah-dg-header-cell ah-dg-header-cell-checkbox ah-dg-align-center\" "
                  "role=\"columnheader\" data-field=\"__checkbox\" style=\"width:40px\">"
                  "<input class=\"ah-dg-select-all ah-dg-header-checkbox\" type=\"checkbox\" "
                  "tabindex=\"-1\" data-indeterminate=\"true\" aria-label=\"Select all rows\">">>, H)),
    ?assert(has(<<"<div class=\"ah-dg-row ah-dg-row-selected ah-dg-row-odd\" id=\"g-r-2\" role=\"row\" "
                  "data-key=\"2\" aria-selected=\"true\"">>, H)),
    ?assert(has(<<"<input class=\"ah-dg-row-checkbox\" type=\"checkbox\" tabindex=\"-1\" checked "
                  "aria-label=\"Select row\">">>, H)),
    ?assertEqual(2, count(<<"checked aria-label=\"Select row\"">>, H)),
    %% all selected: the header box is checked
    Hall = r(?M:datagrid(cols(), rows(), [checkbox], [{value, [1, 2, 3, 4, 5]}])),
    ?assert(has(<<"type=\"checkbox\" tabindex=\"-1\" checked aria-label=\"Select all rows\"">>, Hall)),
    %% none: no aria-selected
    Hn = r(?M:datagrid(cols(), rows(), [none], [])),
    ?assertNot(has_quiet(<<"aria-selected">>, Hn)),
    ?assertNot(has_quiet(<<"aria-multiselectable">>, Hn)).

local_view_test() ->
    H = r(?M:datagrid(cols(), rows(), [filter_row, pageable, statusbar],
                      [{id, g}, {page_size, 2}, {page, 2}, {sort, [{age, desc}]},
                       {filters, #{dept => <<"ENG">>}}])),
    %% eng rows by age desc: Cy 41, Ann 30, Eve 28; page 2 of size 2 is Eve
    ?assertEqual([<<"5">>], shown(H)),
    ?assert(has(<<"data-sort=\"[[&quot;age&quot;,&quot;desc&quot;]]\" "
                  "data-filter=\"{&quot;dept&quot;:&quot;ENG&quot;}\" data-page=\"2\" "
                  "data-page-size=\"2\"">>, H)),
    %% DOM order: sorted page rows first, filtered-out rows last, source index kept
    {match, Order} = re:run(H, <<"data-key=\"([0-9])\"">>, [global, {capture, all_but_first, binary}]),
    ?assertEqual([<<"3">>, <<"1">>, <<"5">>, <<"2">>, <<"4">>], lists:append(Order)),
    ?assert(has(<<"data-key=\"3\" aria-selected=\"false\" data-i=\"2\"">>, H)),
    ?assert(has(<<"aria-sort=\"descending\"">>, H)),
    ?assert(has(<<"ah-dg-header-cell-sorted">>, H)),
    ?assert(has(<<"<span class=\"ah-dg-header-sort-icon\" aria-hidden=\"true\">▼</span>"/utf8>>, H)),
    %% the filter row keeps the text; aria-rowindex counts the two header rows
    ?assert(has(<<"<div class=\"ah-dg-filter-cell ah-dg-filter-cell-active\" data-field=\"dept\" "
                  "style=\"width:100px\"><input class=\"ah-dg-filter-input\" type=\"text\" "
                  "data-field=\"dept\" value=\"ENG\" placeholder=\"Filter...\" autocomplete=\"off\" "
                  "aria-label=\"Dept\">">>, H)),
    ?assert(has(<<"data-key=\"5\" aria-selected=\"false\" aria-rowindex=\"5\"">>, H)),
    ?assert(has(<<"data-ah-header-rows=\"2\"">>, H)),
    %% status bar over the filtered rows: 30 + 41 + 28
    ?assert(has(<<"<span class=\"ah-dg-statusbar-item\" data-agg=\"sum\">"
                  "<span class=\"ah-dg-statusbar-label\">Sum: </span>"
                  "<span class=\"ah-dg-statusbar-value\">99</span></span>">>, H)),
    ?assert(has(<<"<span class=\"ah-dg-statusbar-value\">33.00</span>">>, H)),
    %% pager: 3 items, page 2 of 2
    ?assert(has(<<"<div class=\"ah-dg-pager-wrap\" id=\"g-pager\"><div class=\"ah-dg-pager\" "
                  "role=\"navigation\" aria-label=\"Pages\"><div class=\"ah-dg-pager-info\" "
                  "aria-live=\"polite\">Total 3</div>">>, H)),
    ?assert(has(<<"class=\"ah-dg-pager-button ah-dg-pager-button-active\" data-page=\"2\" "
                  "aria-current=\"page\">2</button>">>, H)),
    ?assert(has(<<"<option value=\"2\" selected>2 / page</option><option value=\"10\">">>, H)),
    %% a page past the end shows the last one
    H2 = r(?M:datagrid(cols(), rows(), [pageable], [{page_size, 2}, {page, 9}])),
    ?assertEqual([<<"5">>], shown(H2)).

grouping_test() ->
    H = r(?M:datagrid(cols(), rows(), [], [{id, g}, {group_by, [dept]}, {sort, [{name, asc}]}])),
    ?assert(has(<<"<div class=\"ah-dg-group-row\" role=\"row\" data-group-id=\"dept:eng\" "
                  "data-level=\"0\" aria-level=\"1\" aria-expanded=\"true\">"
                  "<span class=\"ah-dg-group-indent\" style=\"width:0px\"></span>"
                  "<span class=\"ah-dg-group-toggle ah-dg-group-toggle-open\" aria-hidden=\"true\">▶</span>"
                  "<span class=\"ah-dg-group-title\" role=\"gridcell\" aria-colspan=\"5\">"
                  "Engineering (3)</span><span class=\"ah-dg-group-aggregates\">"
                  "<span class=\"ah-dg-group-agg-item\">Age: sum=99, avg=33</span></span></div>"/utf8>>, H)),
    ?assert(has(<<"data-group-id=\"dept:ops\"">>, H)),
    ?assert(has(<<"Age: sum=60, avg=30</span>">>, H)),
    ?assertEqual([<<"1">>, <<"3">>, <<"5">>, <<"2">>, <<"4">>], shown(H)),
    ?assert(has(<<"data-group-by=\"[&quot;dept&quot;]\"">>, H)),
    %% nested groups
    H2 = r(?M:datagrid(cols(), rows(), [], [{group_by, [dept, active]}])),
    ?assert(has(<<"data-group-id=\"dept:eng|active:true\" data-level=\"1\" aria-level=\"2\"">>, H2)),
    ?assert(has(<<"style=\"width:20px\"">>, H2)).

column_types_test() ->
    Cols = [#{key => id},
            #{key => p, type => progress, max => 50},
            #{key => s, type => rating},
            #{key => b, type => badge, badges => #{ok => {<<"Fine">>, success}}},
            #{key => l, type => link, link_text => <<"Open">>},
            #{key => i, type => image},
            #{key => c, type => command, commands => [{edit, <<"Edit">>}]},
            #{key => d, format => <<"yyyy/MM/dd">>},
            #{key => x, render => {?MODULE, cell}},
            #{key => h, hidden => true, pinned => true}],
    Row = #{id => 1, p => 10, s => 3, b => ok, l => <<"https://e.x/?a=1&b=2">>, i => <<"/a.png">>,
            d => {2026, 9, 29}, x => <<"raw">>, h => 1},
    H = r(?M:datagrid(Cols, [Row], [], [{id, g}])),
    ?assert(has(<<"<div class=\"ah-dg-progress\" role=\"progressbar\" aria-valuenow=\"10\" "
                  "aria-valuemin=\"0\" aria-valuemax=\"50\"><div class=\"ah-dg-progress-bar\" "
                  "style=\"width:20%\"></div></div>">>, H)),
    ?assert(has(<<"<span class=\"ah-dg-rating\">★★★☆☆</span>"/utf8>>, H)),
    ?assert(has(<<"<span class=\"ah-dg-badge ah-dg-badge-success\">Fine</span>">>, H)),
    ?assert(has(<<"<a class=\"ah-dg-cell-link\" href=\"https://e.x/?a=1&amp;b=2\" target=\"_blank\" "
                  "rel=\"noopener noreferrer\">Open</a>">>, H)),
    ?assert(has(<<"<img class=\"ah-dg-cell-image\" src=\"/a.png\" alt=\"\"">>, H)),
    ?assert(has(<<"<span class=\"ah-dg-command-group\"><button class=\"ah-dg-command-btn\" "
                  "type=\"button\" tabindex=\"-1\" data-field=\"c\" data-command=\"edit\">Edit</button>">>, H)),
    ?assert(has(<<"data-field=\"d\" data-v=\"2026-09-29\"">>, H)),
    ?assert(has(<<"<span class=\"ah-dg-cell-content\">2026/09/29</span>">>, H)),
    ?assert(has(<<"<b>custom raw 1</b>">>, H)),
    %% command columns do not sort; a hidden pinned column takes no sticky offset
    ?assert(has(<<"data-field=\"c\" style=\"width:100px\" data-type=\"command\" data-sortable=\"false\" "
                  "data-groupable=\"false\"">>, H)),
    ?assert(has(<<"<div class=\"ah-dg-cell ah-dg-align-left ah-dg-col-hidden\" role=\"gridcell\" "
                  "data-field=\"h\" style=\"width:100px\">">>, H)).

-spec cell(term(), map()) -> aihtml_html:html().
cell(V, #{id := Id}) ->
    aihtml_html:el(b, [<<"custom ">>, V, <<" ">>, integer_to_binary(Id)], [], []).

pinned_test() ->
    Cols = [#{key => id, width => 60, pinned => true}, #{key => name, width => 90, pinned => true},
            #{key => age}],
    H = r(?M:datagrid(Cols, rows(), [checkbox], [{id, g}])),
    %% the check box column is pinned with them
    ?assert(has(<<"data-field=\"__checkbox\" style=\"width:40px;position:sticky;left:0px;z-index:2\"">>, H)),
    ?assert(has(<<"data-field=\"id\" style=\"width:60px;position:sticky;left:40px;z-index:2\"">>, H)),
    ?assert(has(<<"<div class=\"ah-dg-cell ah-dg-align-left ah-dg-cell-pinned-last\" role=\"gridcell\" "
                  "data-field=\"name\" style=\"width:90px;position:sticky;left:100px;z-index:2\">">>, H)),
    ?assert(has(<<"data-pinned=\"true\"">>, H)),
    ?assert(has(<<"data-field=\"age\" style=\"width:100px\">">>, H)).

toolbar_labels_test() ->
    H = r(?M:datagrid(cols(), [], [pageable],
                      [{toolbar, [export_csv, separator, {archive, <<"Archive">>}, spacer, search,
                                  #{name => del, label => <<"Delete">>, icon => <<"x">>}]},
                       {labels, #{empty => <<"暂无数据"/utf8>>, total => <<"共 {0} 条"/utf8>>}},
                       {export_name, <<"people">>}])),
    ?assert(has(<<"<div class=\"ah-dg-toolbar\" role=\"toolbar\"><button class=\"ah-dg-toolbar-btn\" "
                  "type=\"button\" data-export=\"csv\"><span class=\"ah-dg-toolbar-btn-icon\" "
                  "aria-hidden=\"true\">⤓</span><span class=\"ah-dg-toolbar-btn-text\">CSV</span>"
                  "</button><div class=\"ah-dg-toolbar-separator\" role=\"separator\"></div>"/utf8>>, H)),
    ?assert(has(<<"data-name=\"archive\"><span class=\"ah-dg-toolbar-btn-text\">Archive</span>">>, H)),
    ?assert(has(<<"<div class=\"ah-dg-toolbar-spacer\"></div><input class=\"ah-dg-search-input\" "
                  "type=\"search\" placeholder=\"Search...\"">>, H)),
    ?assert(has(<<"data-name=\"del\"><span class=\"ah-dg-toolbar-btn-icon\" aria-hidden=\"true\">x</span>">>, H)),
    ?assert(has(<<"<div class=\"ah-dg-body ah-dg-body-empty\"">>, H)),
    ?assert(has(<<"<div class=\"ah-dg-empty-message\">暂无数据</div>"/utf8>>, H)),
    ?assert(has(<<"共 0 条"/utf8>>, H)),
    ?assert(has(<<"data-ah-export-name=\"people\"">>, H)),
    ?assert(has(<<"class=\"ah-dg-pager-button\" data-page=\"1\" aria-label=\"Last page\" disabled>">>, H)).

escaping_test() ->
    H = r(?M:datagrid([#{key => <<"a\"b">>, title => <<"<T>">>}],
                      [#{id => <<"k 1">>, <<"a\"b">> => <<"<script>">>}], [], [{id, g}])),
    ?assert(has(<<"&lt;script&gt;">>, H)),
    ?assert(has(<<"<span class=\"ah-dg-header-cell-content\">&lt;T&gt;</span>">>, H)),
    ?assert(has(<<"data-field=\"a&quot;b\"">>, H)),
    %% a key that is not a plain name gets a hex row id
    ?assert(has(<<"id=\"g-rx-6b2031\" role=\"row\" data-key=\"k 1\"">>, H)),
    %% the atom key finds a binary key and the other way round
    H2 = r(?M:datagrid([name], [#{id => 1, <<"name">> => <<"bin">>}], [], [])),
    ?assert(has(<<">bin</span>">>, H2)).

generated_id_test() ->
    H = r(#ah_datagrid{columns = [id], rows = [#{id => 1}]}),
    {match, [Id]} = re:run(H, <<"^<div class=\"ah-dg\" id=\"(ah-dg[0-9]+)\"">>,
                           [{capture, all_but_first, binary}]),
    ?assert(has(<<"id=\"", Id/binary, "-r-1\"">>, H)),
    ?assertNotEqual(r(#ah_datagrid{}), r(#ah_datagrid{})).

%%%===================================================================
%%% Formatting, aggregates, pager model
%%%===================================================================

format_test() ->
    L = ?M:default_labels(),
    F = fun(V, Fmt) -> ?M:format_value(V, #{format => Fmt, currency => <<"$">>}, L) end,
    ?assertEqual(<<"1,234,567.89">>, F(1234567.891, <<"n2">>)),
    ?assertEqual(<<"-1,234">>, F(-1234, <<"n0">>)),
    ?assertEqual(<<"999">>, F(999, <<"n">>)),
    ?assertEqual(<<"$1,200.50">>, F(<<"1200.5">>, <<"c2">>)),
    ?assertEqual(<<"85.3%">>, F(0.853, <<"p1">>)),
    ?assertEqual(<<"100%">>, F(1, <<"p0">>)),
    ?assertEqual(<<"2026/09/29 08:05">>, F(<<"2026-09-29T08:05:00">>, <<"yyyy/MM/dd HH:mm">>)),
    ?assertEqual(<<"02.01.2026 00:00:00">>, F({2026, 1, 2}, <<"dd.MM.yyyy HH:mm:ss">>)),
    ?assertEqual(<<"13:04">>, F({{2026, 1, 2}, {13, 4, 5}}, <<"HH:mm">>)),
    ?assertEqual(<<"abc">>, F(<<"abc">>, <<"n2">>)),
    ?assertEqual(<<"not a date">>, F(<<"not a date">>, <<"yyyy">>)),
    ?assertEqual(<<"1.0e5">>, F(<<"1.0e5">>, undefined)),
    ?assertEqual(<<"100,000.0">>, F(<<"1e5">>, <<"n1">>)),
    ?assertEqual(<<"Yes">>, F(true, undefined)),
    ?assertEqual(<<>>, F(undefined, <<"n2">>)),
    ?assertEqual(<<"2.5">>, F(2.5, undefined)).

aggregate_test() ->
    ?assertEqual(6, ?M:aggregate(sum, [1, 2, 3])),
    ?assertEqual(2.0, ?M:aggregate(avg, [1, 2, 3])),
    ?assertEqual(3, ?M:aggregate(count, [1, 2, 3])),
    ?assertEqual(1, ?M:aggregate(min, [3, 1, 2])),
    ?assertEqual(0, ?M:aggregate(sum, [])),
    %% no numbers: count 0, the others empty
    H = r(?M:datagrid([#{key => v, aggregates => [sum, count]}], [#{id => 1, v => <<"x">>}],
                      [statusbar], [])),
    ?assert(has(<<"<span class=\"ah-dg-statusbar-value\"></span>">>, H)),
    ?assert(has(<<"<span class=\"ah-dg-statusbar-value\">0</span>">>, H)).

pager_view_test() ->
    L = ?M:default_labels(),
    V = ?M:pager_view(5, 10, 200, #{sizes => [10, 20], labels => L}),
    ?assertMatch(#{info := <<"Total 200">>, prev := 4, next := 6, last := 20,
                   at_start := false, at_end := false}, V),
    ?assertEqual([2, 3, 4, 5, 6, 7, 8], [P || #{page := P} <- maps:get(pages, V)]),
    ?assertEqual([5], [P || #{page := P, active := true} <- maps:get(pages, V)]),
    V2 = ?M:pager_view(1, 10, 0, #{sizes => [10], labels => L}),
    ?assertMatch(#{pages := [], at_start := true, at_end := true, last := 1}, V2),
    ?assertEqual([14, 15, 16, 17, 18, 19, 20],
                 [P || #{page := P} <- maps:get(pages, ?M:pager_view(20, 10, 200,
                                                                     #{sizes => [10], labels => L}))]).

%%%===================================================================
%%% Remote mode: render, query, answer; row updates; export
%%%===================================================================

action(people, _, Event, Ctx) ->
    {Rows, Total} = ?M:datagrid_select(?M:datagrid_query(Event), rows()),
    ?M:datagrid_rows(Ctx, Event, Rows, Total).

remote_html() ->
    r(?M:datagrid(cols(), lists:sublist(rows(), 2), [pageable, filter_row],
                  [{id, rg}, {page_size, 2}, {page_sizes, [2, 4]}, {total, 5},
                   {source, {?MODULE, people, #{}}}])).

token(H, Attr) ->
    {match, [T]} = re:run(H, <<Attr/binary, "=\"([^\"]+)\"">>, [{capture, all_but_first, binary}]),
    T.

remote_render_test() ->
    H = remote_html(),
    ?assert(has(<<"aria-rowcount=\"7\"">>, H)),
    ?assert(has(<<"data-ah-remote">>, H)),
    ?assert(has(<<"data-ah-loaded=\"true\"">>, H)),
    ?assertEqual([<<"1">>, <<"2">>], shown(H)),
    ?assert(has(<<"<div class=\"ah-dg-query\" id=\"rg-q\" hidden data-grid=\"rg\" "
                  "data-ah-sync=\"replace\" data-ah-on=\"ah:query:">>, H)),
    {match, [Q]} = re:run(H, <<"data-ah-on=\"ah:query:([^\"]+)\"">>, [{capture, all_but_first, binary}]),
    ?assertEqual({ok, {?MODULE, people, #{}}}, aihtml_action:verify(Q)),
    ?assert(has(<<"Total 5">>, H)),
    ?assertNot(has_quiet(<<"data-i=">>, H)),
    %% without a total the rows given are the whole count; no rows is the
    %% empty state, rendered here (the grid does not load on mount)
    H2 = r(?M:datagrid(cols(), [], [pageable], [{source, {?MODULE, people, #{}}}])),
    ?assert(has(<<"data-ah-loaded=\"true\"">>, H2)),
    ?assert(has(<<"ah-dg-body ah-dg-body-empty">>, H2)),
    ?assert(has(<<"<div class=\"ah-dg-empty-message\">No data</div>">>, H2)),
    ?assert(has(<<"Total 0">>, H2)),
    H3 = r(?M:datagrid(cols(), lists:sublist(rows(), 3), [pageable],
                       [{source, {?MODULE, people, #{}}}])),
    ?assertEqual([<<"1">>, <<"2">>, <<"3">>], shown(H3)),
    ?assert(has(<<"Total 3">>, H3)).

%%%===================================================================
%%% Pager links (href)
%%%===================================================================

linked_html() ->
    r(?M:datagrid(cols(), lists:sublist(rows(), 2), [pageable],
                  [{id, lg}, {page_size, 2}, {page_sizes, [2, 4]}, {total, 5}, {page, 2},
                   {sort, [{name, asc}, {age, desc}]}, {source, {?MODULE, people, #{}}},
                   {href, <<"/p?page={page}&size={size}&sort={sort}&q={search}">>}])).

pager_links_test() ->
    H = linked_html(),
    ?assert(has(<<"data-ah-href=\"/p?page={page}&amp;size={size}&amp;sort={sort}&amp;q={search}\"">>, H)),
    ?assert(has(<<"<a class=\"ah-dg-pager-button\" href=\"/p?page=1&amp;size=2&amp;sort=name:asc,age:desc&amp;q=\" "
                  "data-page=\"1\" aria-label=\"First page\">|&lt;</a>">>, H)),
    ?assert(has(<<"<a class=\"ah-dg-pager-button ah-dg-pager-button-active\" "
                  "href=\"/p?page=2&amp;size=2&amp;sort=name:asc,age:desc&amp;q=\" data-page=\"2\" "
                  "aria-current=\"page\">2</a>">>, H)),
    ?assert(has(<<"<a class=\"ah-dg-pager-button\" href=\"/p?page=3&amp;size=2&amp;sort=name:asc,age:desc&amp;q=\" "
                  "data-page=\"3\" aria-label=\"Last page\">&gt;|</a>">>, H)),
    ?assertNot(has_quiet(<<"<button type=\"button\" class=\"ah-dg-pager-button">>, H)),
    %% on the first page first / prev stay disabled buttons
    H1 = r(?M:datagrid(cols(), rows(), [pageable], [{page_size, 2}, {href, <<"?p={page}">>}])),
    ?assert(has(<<"<button type=\"button\" class=\"ah-dg-pager-button\" data-page=\"1\" "
                  "aria-label=\"First page\" disabled>|&lt;</button>">>, H1)),
    ?assert(has(<<"<a class=\"ah-dg-pager-button\" href=\"?p=2\" data-page=\"2\" "
                  "aria-label=\"Next page\">&gt;</a>">>, H1)),
    %% without href the pager has no links and the root no data-ah-href
    H0 = r(?M:datagrid(cols(), rows(), [pageable], [{page_size, 2}])),
    ?assertNot(has_quiet(<<"<a class=\"ah-dg-pager-button">>, H0)),
    ?assertNot(has_quiet(<<"data-ah-href">>, H0)),
    ?assertError({aihtml, {bad_option, href, 7}},
                 r(?M:datagrid(cols(), rows(), [], [{href, 7}]))).

pager_view_links_test() ->
    L = ?M:default_labels(),
    V = ?M:pager_view(1, 10, 25, #{sizes => [10], labels => L, href => <<"/x/{page}">>}),
    ?assertMatch(#{link := true, first_href := <<>>, prev_href := <<>>,
                   next_href := <<"/x/2">>, last_href := <<"/x/3">>}, V),
    ?assertEqual([<<"/x/1">>, <<"/x/2">>, <<"/x/3">>], [U || #{href := U} <- maps:get(pages, V)]),
    ?assertNot(maps:is_key(link, ?M:pager_view(1, 10, 25, #{sizes => [10], labels => L}))).

link_rows_test() ->
    %% datagrid_rows/4 renders the links with the view of the query,
    %% the search and sort URL-encoded like encodeURIComponent
    Tok = token(linked_html(), <<"data-render">>),
    Ev = #{type => <<"ah:query">>, id => <<"lg-q">>, value => null,
           data => #{<<"render">> => Tok, <<"page">> => <<"1">>, <<"pageSize">> => <<"4">>,
                     <<"sort">> => <<"[[\"age\",\"desc\"]]">>,
                     <<"search">> => <<"a b&é/"/utf8>>}},
    Ops = aihtml_action:render_ops(fun(Ctx) -> ?M:datagrid_rows(Ctx, Ev, rows(), 5) end),
    [#{id := <<"lg-pager">>, html := P}] = [O || #{id := <<"lg-pager">>} = O <- Ops],
    ?assert(has(<<"href=\"/p?page=2&amp;size=4&amp;sort=age:desc&amp;q=a%20b%26%C3%A9%2F\"">>, P)).

query_event(Data) ->
    #{type => <<"ah:query">>, id => <<"rg-q">>, value => null, checked => null, key => null,
      form => #{}, values => #{}, data => Data#{<<"render">> => token(remote_html(), <<"data-render">>)}}.

query_test() ->
    Q = ?M:datagrid_query(query_event(#{<<"sort">> => <<"[[\"age\",\"desc\"],[\"nope\",\"asc\"],[\"name\",\"up\"]]">>,
                                        <<"filter">> => <<"{\"dept\":\"en\",\"evil\":\"x\",\"name\":\"\"}">>,
                                        <<"search">> => <<"a">>,
                                        <<"page">> => <<"2">>, <<"pageSize">> => <<"4">>})),
    ?assertEqual(#{sort => [{age, desc}], filters => [{dept, <<"en">>}], search => <<"a">>,
                   page => 2, page_size => 4, offset => 4, limit => 4, export => undefined}, Q),
    %% an unknown page size falls back; junk is ignored; export takes everything
    Q2 = ?M:datagrid_query(query_event(#{<<"sort">> => <<"{">>, <<"pageSize">> => <<"1000">>,
                                         <<"page">> => <<"-3">>, <<"export">> => <<"xlsx">>})),
    ?assertEqual(#{sort => [], filters => [], search => <<>>, page => 1, page_size => 2,
                   offset => 0, limit => infinity, export => xlsx}, Q2),
    ?assertError({aihtml, bad_datagrid_token},
                 ?M:datagrid_query(#{data => #{<<"render">> => <<"forged.token">>}})).

remote_round_trip_test() ->
    Ev = query_event(#{<<"sort">> => <<"[[\"age\",\"desc\"]]">>, <<"page">> => <<"2">>,
                       <<"pageSize">> => <<"2">>, <<"headerRows">> => <<"2">>}),
    {ok, [Body, Pager, Call]} = aihtml_action:execute({?MODULE, people, #{}}, json_event(Ev),
                                                      #{send => fun(_) -> error(unexpected_flush) end}),
    #{op := html, swap := morph_inner, id := <<"rg-body">>, html := Rows} = Body,
    %% ages desc: 41 35 30 28 25; page 2 is Ann (30), Eve (28)
    ?assertEqual([<<"1">>, <<"5">>], shown(Rows)),
    ?assert(has(<<"id=\"rg-r-1\" role=\"row\" data-key=\"1\" aria-selected=\"false\" aria-rowindex=\"5\"">>,
                Rows)),
    ?assert(has(<<"<div class=\"ah-dg-empty-message\" hidden>">>, Rows)),
    #{op := html, id := <<"rg-pager">>, html := PagerHtml} = Pager,
    ?assert(has(<<"data-page=\"2\" aria-current=\"page\"">>, PagerHtml)),
    ?assertEqual(#{op => call, id => <<"rg">>, method => <<"rowsLoaded">>, args => [5, 2]}, Call),
    _ = iolist_to_binary(json:encode([Body, Pager, Call])).

remote_export_test() ->
    Ev = query_event(#{<<"export">> => <<"csv">>, <<"sort">> => <<"[[\"name\",\"asc\"]]">>}),
    Ops = aihtml_action:render_ops(
            fun(Ctx) ->
                    {Rows, Total} = ?M:datagrid_select(?M:datagrid_query(Ev), rows()),
                    5 = length(Rows),
                    ?M:datagrid_rows(Ctx, Ev, Rows, Total)
            end),
    [#{op := call, id := <<"rg">>, method := <<"exportData">>,
       args := [<<"csv">>, Headers, [First | _] = Body]}] = Ops,
    ?assertEqual([<<"ID">>, <<"Name">>, <<"Dept">>, <<"Age">>, <<"Active">>], Headers),
    ?assertEqual([<<"1">>, <<"Ann">>, <<"Engineering">>, <<"30">>, <<"Yes">>], First),
    ?assertEqual(5, length(Body)).

datagrid_row_test() ->
    H = r(?M:datagrid(cols(), rows(), [], [{id, g}])),
    Ev = #{type => <<"ah:edit">>, id => <<"g">>, value => <<>>,
           data => #{<<"render">> => token(H, <<"data-render">>), <<"key">> => <<"2">>}},
    Ops = aihtml_action:render_ops(
            fun(Ctx) -> ?M:datagrid_row(Ctx, Ev, #{id => 2, name => <<"Bobby">>, dept => eng,
                                                   age => 26, active => true}) end),
    [#{op := html, swap := morph, id := <<"g-r-2">>, html := Row},
     #{op := call, id := <<"g">>, method := <<"rowUpdated">>, args := [<<"g-r-2">>]}] = Ops,
    ?assert(has(<<"<div class=\"ah-dg-row ah-dg-row-even\" id=\"g-r-2\" role=\"row\" data-key=\"2\"">>, Row)),
    ?assert(has(<<">Bobby</span>">>, Row)),
    ?assert(has(<<">Engineering</span>">>, Row)).

select_test() ->
    Q = #{sort => [{age, asc}], filters => [{dept, <<"EN">>}], search => <<>>, page => 1,
          page_size => 2, offset => 0, limit => 2, export => undefined},
    {Rows, Total} = ?M:datagrid_select(Q, rows()),
    ?assertEqual(3, Total),
    ?assertEqual([5, 1], [I || #{id := I} <- Rows]),
    {R2, 2} = ?M:datagrid_select(Q#{filters => [], search => <<"o">>, limit => infinity,
                                    sort => [{name, desc}]}, rows()),
    ?assertEqual([<<"Dee">>, <<"bob">>], [N || #{name := N} <- R2]),
    {R3, 5} = ?M:datagrid_select(Q#{filters => [], offset => 4, limit => 2}, rows()),
    ?assertEqual([3], [I || #{id := I} <- R3]).

json_event(Ev) ->
    maps:from_list([{atom_to_binary(K), V} || K := V <- Ev]).


%%%===================================================================
%%% Validation
%%%===================================================================

field_validation_test() ->
    ?assertError({aihtml, {bad_option, edit_mode, twice}}, r(#ah_datagrid{edit_mode = twice})),
    ?assertError({aihtml, {bad_option, page_size, 0}}, r(#ah_datagrid{page_size = 0})),
    ?assertError({aihtml, {bad_option, sort, [{a, up}]}}, r(#ah_datagrid{sort = [{a, up}]})),
    ?assertError({aihtml, {bad_option, source, nope}}, r(#ah_datagrid{source = nope})),
    ?assertError({aihtml, {bad_option, total, -1}}, r(#ah_datagrid{total = -1})),
    ?assertError({aihtml, {bad_option, height, 1.5}}, r(#ah_datagrid{height = 1.5})),
    ?assertError({aihtml, {bad_modifier, datagrid, selection, many, _}},
                 r(#ah_datagrid{selection = many})),
    ?assertError({aihtml, {bad_flag, datagrid, pageable, yes}}, r(#ah_datagrid{pageable = yes})),
    ?assertError({aihtml, {unknown_column_key, witdh}},
                 r(#ah_datagrid{columns = [#{key => a, witdh => 3}]})),
    ?assertError({aihtml, {bad_column_type, <<"a">>, chart}},
                 r(#ah_datagrid{columns = [#{key => a, type => chart}]})),
    ?assertError({aihtml, {not_editable_type, <<"a">>, progress}},
                 r(#ah_datagrid{columns = [#{key => a, type => progress, editable => true}]})),
    ?assertError({aihtml, {duplicate_column, <<"a">>}}, r(#ah_datagrid{columns = [a, <<"a">>]})),
    ?assertError({aihtml, {row_without_key, id, _}},
                 r(#ah_datagrid{columns = [a], rows = [#{a => 1}]})),
    ?assertError({aihtml, {unknown_label, nope}}, r(#ah_datagrid{labels = #{nope => <<"x">>}})),
    ?assertError({aihtml, {bad_toolbar_item, print}}, r(#ah_datagrid{toolbar = [print]})),
    ?assertError({aihtml, {bad_badge_class, pink}},
                 r(#ah_datagrid{columns = [#{key => a, type => badge, badges => #{x => {<<"X">>, pink}}}]})),
    ?assertError({aihtml, {unknown_modifier, datagrid, big, _}}, ?M:datagrid([], [], [big], [])).

%%%===================================================================
%%% Catalog and records
%%%===================================================================

catalog_test() ->
    [#{name := datagrid, category := data}] = ?M:catalog(),
    [?assert(erlang:function_exported(?M, F, A)) || {F, A} <- ?M:facade_extras()],
    #{flags := Fl, options := Op, option_docs := Docs, methods := Ms, groups := Gs} = E =
        aihtml_catalog:entry(?M, datagrid),
    Mods = lists:append([Vs || {Vs, _} <- maps:values(Gs)]),
    ?assertEqual(lists:sort(Fl ++ Op ++ Mods), lists:sort(maps:keys(Docs))),
    ?assertEqual([<<"ah-dg">>], aihtml_catalog:classes(E, [multi | Fl])),
    [?assert(is_binary(D)) || #{doc := D} <- Ms].

catalog_docs_test() ->
    [begin
         ?assert(byte_size(maps:get(doc, E)) > 0),
         [?assert(byte_size(maps:get(K, maps:get(option_docs, E))) > 0)
          || K <- maps:get(options, E, []) ++ maps:get(flags, E, [])]
     end || E <- ?M:catalog()].

record_equals_builder_test() ->
    Src = {?MODULE, people, #{}},
    ?assertEqual(r(?M:datagrid(cols(), rows(), [multi, pageable, <<"w-full">>],
                               [{id, g}, {page_size, 3}, {sort, [{age, asc}]}, {value, [1]},
                                {source, Src}, {total, 5}, {name, n}, {title, <<"t">>}])),
                 r(#ah_datagrid{columns = cols(), rows = rows(), selection = multi, pageable = true,
                                css = [<<"w-full">>], id = g, page_size = 3, sort = [{age, asc}],
                                value = [1], source = Src, total = 5, name = n,
                                attrs = [{title, <<"t">>}]})).

builder_fills_fields_test() ->
    G = ?M:datagrid([a], [], [checkbox, statusbar, <<"x">>], [{height, 300}, {role, x}]),
    ?assertMatch(#ah_datagrid{columns = [a], selection = checkbox, statusbar = true,
                              height = 300, css = [<<"x">>], attrs = [{role, x}]}, G),
    ?assertError({aihtml, {record_only_field, ah_datagrid, postback}},
                 ?M:datagrid([], [], [], [{postback, pick}])).

postback_test() ->
    {match, [Ev, Tok]} = re:run(r(#ah_datagrid{columns = [a], postback = {picked, #{n => 1}}}),
                                <<"data-ah-on=\"([a-z:]+):([^\"]+)\"">>,
                                [{capture, all_but_first, binary}]),
    ?assertEqual(<<"change">>, Ev),
    ?assertEqual({ok, {?MODULE, picked, #{n => 1}}}, aihtml_action:unsign(Tok)).

records_match_catalog_test() ->
    Base = [module, id, css, attrs, postback, delegate],
    [begin
         Tag = list_to_atom("ah_" ++ atom_to_list(N)),
         Fields = ?M:fields(Tag),
         ?assertEqual(Base, lists:sublist(Fields, 6)),
         Defaults = maps:from_list(lists:zip(Fields, tl(tuple_to_list(#ah_datagrid{})))),
         [?assertEqual({N, G, case D of none -> undefined; _ -> D end},
                       {N, G, maps:get(G, Defaults)})
          || {G, {_, D}} <- maps:to_list(maps:get(groups, E, #{}))],
         [?assertEqual({N, F, false}, {N, F, maps:get(F, Defaults)})
          || F <- maps:get(flags, E, [])],
         [?assert(lists:member(O, Fields)) || O <- maps:get(options, E, [])],
         ?assertEqual(?M, maps:get(module, Defaults))
     end || #{name := N} = E <- ?M:catalog()].

%% A value containing a comma is escaped in data-ah-value (aihtml_value).
vhas(Sub, Bin) -> binary:match(Bin, Sub) =/= nomatch.

comma_values_test() ->
    Rows = [#{id => <<"1,5">>, name => <<"A">>}, #{id => 2, name => <<"B">>}],
    H = r(?M:datagrid([#{key => name}], Rows, [multi],
                      [{id, g}, {name, sel}, {value, [<<"1,5">>, 2]}])),
    ?assert(vhas(<<"data-ah-value=\"1\\,5,2\"">>, H)),
    ?assert(vhas(<<"value=\"1\\,5,2\"">>, H)).
