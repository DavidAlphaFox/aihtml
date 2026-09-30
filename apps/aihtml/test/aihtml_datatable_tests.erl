%% Tests for aihtml_datatable. The module is also the fake action module
%% of the remote data table round trips.
-module(aihtml_datatable_tests).
-behaviour(aihtml_action).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_datatable.hrl").

-export([action/4]).

-define(M, aihtml_datatable).

r(Html) -> iolist_to_binary(aihtml_html:render(Html)).

has(Needle, Hay) ->
    case binary:match(Hay, Needle) of
        nomatch -> ?debugFmt("~s not in~n~s", [Needle, Hay]), false;
        _ -> true
    end.

has_quiet(Needle, Hay) -> binary:match(Hay, Needle) =/= nomatch.

count(Needle, Hay) -> length(binary:matches(Hay, Needle)).

%% The keys of the rows in document order, and of the visible ones.
keys(H) ->
    {match, Ks} = re:run(H, <<"<tr class=\"ah-(?:tg|dt)-row[^\"]*\" id=\"[^\"]+\" role=\"row\" data-key=\"([^\"]+)\"">>,
                         [global, {capture, all_but_first, binary}]),
    [K || [K] <- Ks].

shown(H) ->
    {match, Ks} = re:run(H, <<"<tr class=\"ah-(?:tg|dt)-row[^\"]*\" id=\"[^\"]+\" role=\"row\" data-key=\"([^\"]+)\"[^>]*>">>,
                         [global, {capture, [0, 1], binary}]),
    [K || [Tag, K] <- Ks, not has_quiet(<<" hidden">>, Tag)].

%%%===================================================================
%%% datatable
%%%===================================================================

people() ->
    [#{id => 1, name => <<"Ann">>, age => 31, city => <<"Oslo">>},
     #{id => 2, name => <<"Bob">>, age => 25, city => <<"Rome">>},
     #{id => 3, name => <<"Cid">>, age => 47, city => <<"Oslo">>},
     #{id => 4, name => <<"Dan">>, age => 19, city => <<>>},
     #{id => 5, name => <<"Eve">>, age => 38, city => <<"Lima">>}].

pcols() ->
    [#{field => name, title => <<"Name">>, width => 120},
     #{field => age, title => <<"Age">>, type => number, align => right},
     #{field => city, title => <<"City">>}].

datatable_structure_test() ->
    H = r(?M:ah_datatable(pcols(), people(), [], [{id, dt}])),
    ?assert(has(<<"<div class=\"ah-dt\" id=\"dt\" role=\"grid\" data-ah=\"datatable\" data-ah-value=\"\" "
                  "data-selection=\"single\" data-mode=\"local\" data-sortable=\"true\" "
                  "data-alt-rows=\"true\" data-filter=\"none\" data-page=\"1\">">>, H)),
    ?assert(has(<<"<div class=\"ah-dt-content\"><div class=\"ah-dt-header\"><table class=\"ah-dt-table\" "
                  "role=\"presentation\"><colgroup><col style=\"width:120px;min-width:120px;\"><col><col>"
                  "</colgroup><thead role=\"rowgroup\"><tr class=\"ah-dt-header-row\" role=\"row\">">>, H)),
    ?assert(has(<<"<th class=\"ah-dt-th ah-dt-th-sortable\" role=\"columnheader\" data-field=\"age\" "
                  "data-type=\"number\" style=\"text-align:right;\" tabindex=\"0\">">>, H)),
    ?assertEqual([<<"1">>, <<"2">>, <<"3">>, <<"4">>, <<"5">>], keys(H)),
    ?assertEqual(keys(H), shown(H)),
    ?assert(has(<<"<tr class=\"ah-dt-row ah-dt-row-hover\" id=\"dt-r-1\" role=\"row\" data-key=\"1\" "
                  "data-i=\"0\" aria-selected=\"false\" tabindex=\"0\"><td class=\"ah-dt-cell\" "
                  "role=\"gridcell\" data-field=\"name\" style=\"text-align:left;\"><span>Ann</span></td>"
                  "<td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"age\" style=\"text-align:right;\" "
                  "data-value=\"31\"><span>31</span></td>">>, H)),
    ?assert(has(<<"<tr class=\"ah-dt-row ah-dt-row-alt ah-dt-row-hover\" id=\"dt-r-2\"">>, H)),
    ?assert(has(<<"<tr class=\"ah-dt-row-empty\" hidden><td class=\"ah-dt-cell-empty\" colspan=\"3\">">>, H)),
    ?assert(has(<<"<div class=\"ah-dt-loading-overlay\"><div class=\"ah-dt-loading-spinner\"></div></div>">>, H)),
    ?assertNot(has_quiet(<<"ah-dt-pager">>, H)),
    ?assertNot(has_quiet(<<"data-ah-on">>, H)).

datatable_local_view_test() ->
    %% initial sort, filter and page are applied here as the browser would
    H = r(?M:ah_datatable(pcols(), people(), [],
                          [{id, v}, {sort, {age, desc}}, {page_size, 2}, {page, 2},
                           {filter, row}, {filters, #{city => <<"o">>}}])),
    %% Oslo, Rome, Oslo match "o"; by age desc: 3 (47), 1 (31), 2 (25)
    ?assertEqual([<<"3">>, <<"1">>, <<"2">>, <<"4">>, <<"5">>], keys(H)),
    ?assertEqual([<<"2">>], shown(H)),
    ?assert(has(<<"data-sort-field=\"age\" data-sort-dir=\"desc\" data-filter=\"row\" data-page=\"2\" "
                  "data-page-size=\"2\"">>, H)),
    ?assert(has(<<"<input class=\"ah-dt-filter-input\" data-field=\"city\" type=\"text\" "
                  "placeholder=\"Filter...\" value=\"o\" aria-label=\"City\">">>, H)),
    ?assert(has(<<"<div class=\"ah-dt-pager-info\">3-3 of 3</div>">>, H)),
    ?assert(has(<<"<button class=\"ah-dt-pager-btn ah-dt-pager-btn-num ah-dt-pager-btn-active\" "
                  "type=\"button\" data-page=\"2\" aria-current=\"page\">2</button>">>, H)),
    ?assert(has(<<"data-sizes=\"5,10,25,50\"">>, H)),
    ?assert(has(<<"<option value=\"2\" selected>2</option><option value=\"5\">5</option>">>, H)),
    %% a page past the end is clamped; search mode
    S = r(?M:ah_datatable(pcols(), people(), [],
                          [{id, s}, {filter, search}, {search, <<" LI ">>}, {page_size, 10}, {page, 9}])),
    ?assertEqual([<<"5">>], shown(S)),
    ?assert(has(<<"data-search=\" LI \" data-page=\"1\"">>, S)),
    ?assert(has(<<"<div class=\"ah-dt-search-bar\"><input class=\"ah-dt-search-input\" type=\"text\" "
                  "value=\" LI \" placeholder=\"Search...\" aria-label=\"Search...\"></div>">>, S)),
    %% advanced conditions
    A = r(?M:ah_datatable(pcols(), people(), [],
                          [{id, a}, {filter, advanced}, {filters, #{age => {gt, 30}, city => {empty, <<>>}}}])),
    ?assertEqual([], shown(A)),
    ?assert(has(<<"<tr class=\"ah-dt-row-empty\"><td">>, A)),
    ?assert(has(<<"<option value=\"gt\" selected>Greater Than</option>">>, A)),
    ?assert(has(<<"<input class=\"ah-dt-adv-filter-input\" data-field=\"city\" type=\"text\" "
                  "placeholder=\"Value...\" value=\"\" aria-label=\"City\" disabled>">>, A)),
    %% text columns offer no gt
    ?assertEqual(1, count(<<"value=\"gt\"">>, A)),
    A2 = r(?M:ah_datatable(pcols(), people(), [],
                           [{filter, advanced}, {filters, #{age => {lte, <<"31">>}, name => {not_contains, <<"b">>}}}])),
    ?assertEqual([<<"1">>, <<"4">>], shown(A2)).

datatable_features_test() ->
    Details = fun(#{name := N}) -> [<<"About ">>, N] end,
    Cols = pcols() ++ [#{field => note, hidden => true, editable => false}],
    H = r(?M:ah_datatable(Cols, lists:sublist(people(), 2), [<<"w-full">>],
                          [{id, f}, {selection_mode, checkbox}, {value, [2]}, {row_details, Details},
                           {expanded, [1]}, {editable, true}, {edit, {?MODULE, edit, #{}}},
                           {resizable, true}, {column_chooser, true}, {name, sel},
                           {texts, #{columns => <<"Cols">>}}])),
    ?assert(has(<<"<colgroup><col style=\"width:36px;min-width:36px;\"><col style=\"width:40px;min-width:40px;\">"
                  "<col style=\"width:120px;min-width:120px;\"><col><col><col hidden></colgroup>">>, H)),
    ?assert(has(<<"<th class=\"ah-dt-th ah-dt-th-expand\" role=\"columnheader\"></th>">>, H)),
    ?assert(has(<<"<input class=\"ah-dt-header-checkbox\" type=\"checkbox\" aria-label=\"Select all rows\">">>, H)),
    ?assert(has(<<"<button class=\"ah-dt-expand-btn ah-dt-expand-btn-open\" type=\"button\" tabindex=\"-1\" "
                  "aria-expanded=\"true\" aria-label=\"Details\" aria-controls=\"f-r-1-d\">›</button>"/utf8>>, H)),
    ?assert(has(<<"<tr class=\"ah-dt-row-details\" id=\"f-r-1-d\" data-key=\"1\"><td "
                  "class=\"ah-dt-row-details-cell\" colspan=\"5\"><div class=\"ah-dt-row-details-content\">"
                  "About Ann</div></td></tr>">>, H)),
    ?assert(has(<<"<tr class=\"ah-dt-row-details ah-dt-row-details-hidden\" id=\"f-r-2-d\"">>, H)),
    ?assert(has(<<"<td class=\"ah-dt-cell ah-dt-cell-editable\" role=\"gridcell\" data-field=\"name\"">>, H)),
    %% the hidden, non-editable column
    ?assert(has(<<"<td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"note\" style=\"text-align:left;\" "
                  "hidden><span></span></td>">>, H)),
    ?assert(has(<<"data-editable=\"true\" data-edit=\"">>, H)),
    ?assert(has(<<"data-hidden=\"note\" data-expanded=\"1\"">>, H)),
    ?assert(has(<<"<div class=\"ah-dt-resize-handle\" aria-hidden=\"true\"></div>">>, H)),
    ?assert(has(<<"<div class=\"ah-dt-chooser-wrap\"><button class=\"ah-dt-chooser-btn\" type=\"button\" "
                  "title=\"Cols\" aria-label=\"Cols\" aria-haspopup=\"true\" aria-expanded=\"false\">☰</button>"/utf8>>, H)),
    ?assert(has(<<"<div class=\"ah-dt-chooser-panel\" role=\"group\" aria-label=\"Cols\"><label "
                  "class=\"ah-dt-chooser-item\"><input class=\"ah-dt-chooser-checkbox\" type=\"checkbox\" "
                  "data-field=\"name\" checked>Name</label>">>, H)),
    ?assert(has(<<"data-field=\"note\">note</label>">>, H)),
    ?assert(has(<<"<tr class=\"ah-dt-row ah-dt-row-alt ah-dt-row-hover ah-dt-row-selected\" id=\"f-r-2\" "
                  "role=\"row\" data-key=\"2\" data-i=\"1\" aria-selected=\"true\" tabindex=\"0\">">>, H)),
    ?assert(has(<<"<input type=\"hidden\" name=\"sel\" value=\"2\" data-ah-input>">>, H)),
    {match, [Token]} = re:run(H, <<"data-edit=\"([^\"]+)\"">>, [{capture, all_but_first, binary}]),
    ?assertEqual({ok, {?MODULE, edit, #{}}}, aihtml_action:verify(Token)),
    %% rows without a key field are keyed by position
    K = r(?M:ah_datatable([a], [#{a => <<"x">>}, #{a => <<"y">>}], [], [{id, k}])),
    ?assertEqual([<<"0">>, <<"1">>], keys(K)).

row_id_test() ->
    H = r(?M:ah_datatable([k], [#{id => <<"a b/é"/utf8>>, k => 1}], [], [{id, t}])),
    ?assert(has(<<"id=\"t-r-a_20b_2f_c3_a9\" role=\"row\" data-key=\"a b/é\""/utf8>>, H)).

pager_test() ->
    T = #{info => <<"{start}-{end} of {total} ({page}/{pages})">>, prev => <<"P">>, next => <<"N">>,
          page_size => <<"S">>},
    Btns = fun(Page, Pages) ->
                   #{buttons := Bs} = ?M:pager_view(Page, 1, Pages, [], T),
                   [case B of #{gap := true} -> gap; #{page := P} -> P end || B <- Bs]
           end,
    ?assertEqual([1, 2, 3, 4, 5, 6, 7], Btns(1, 7)),
    ?assertEqual([1, 2, gap, 20], Btns(1, 20)),
    ?assertEqual([1, 2, 3, 4, gap, 20], Btns(3, 20)),
    ?assertEqual([1, gap, 9, 10, 11, gap, 20], Btns(10, 20)),
    ?assertEqual([1, gap, 19, 20], Btns(20, 20)),
    #{info := I, prev_disabled := true, next_disabled := false, has_sizes := false} =
        ?M:pager_view(1, 10, 25, [], T),
    ?assertEqual(<<"1-10 of 25 (1/3)">>, I),
    #{info := I0, next_disabled := true, sizes := Sizes} = ?M:pager_view(1, 10, 0, [5, 10], T),
    ?assertEqual(<<"0-0 of 0 (1/1)">>, I0),
    ?assertEqual([#{size => 5, selected => false}, #{size => 10, selected => true}], Sizes).

%%%===================================================================
%%% remote datatable
%%%===================================================================

query_event(Data) ->
    #{type => <<"ah:query">>, id => <<"rq">>, value => <<"3">>, checked => null, key => null,
      form => #{}, values => #{}, data => Data}.

query_test() ->
    Q = ?M:datatable_query(query_event(#{<<"sortField">> => <<"age">>, <<"sortDir">> => <<"desc">>,
                                         <<"page">> => <<"3">>, <<"pageSize">> => <<"10">>,
                                         <<"search">> => <<"x">>,
                                         <<"filters">> => <<"{\"city\":\"os\",\"age\":{\"condition\":\"gte\","
                                                            "\"value\":\"30\"},\"name\":{\"condition\":"
                                                            "\"empty\",\"value\":\"\"},\"bad\":{\"condition\":"
                                                            "\"drop_table\",\"value\":\"1\"},\"none\":\"\"}">>})),
    ?assertEqual(#{sort => {<<"age">>, desc}, page => 3, page_size => 10, offset => 20, limit => 10,
                   search => <<"x">>,
                   filters => #{<<"city">> => <<"os">>, <<"age">> => {gte, <<"30">>},
                                <<"name">> => {empty, <<>>}}}, Q),
    ?assertEqual(#{sort => undefined, page => 1, page_size => undefined, offset => 0,
                   limit => undefined, search => <<>>, filters => #{}},
                 ?M:datatable_query(query_event(#{<<"page">> => <<"x">>, <<"filters">> => <<"{">>}))).

datatable_page_test() ->
    Q = #{sort => {<<"age">>, asc}, page => 1, page_size => 2, offset => 0, limit => 2,
          search => <<>>, filters => #{<<"city">> => <<"o">>}},
    {Rows, Total} = ?M:datatable_page(pcols(), people(), Q),
    ?assertEqual(3, Total),
    ?assertEqual([2, 1], [I || #{id := I} <- Rows]),
    {Rows2, 5} = ?M:datatable_page(pcols(), people(),
                                   Q#{filters := #{}, sort := {<<"name">>, desc}, offset := 4,
                                      limit := undefined}),
    ?assertEqual(5, length(Rows2)),
    ?assertMatch({[#{id := 4}], 1},
                 ?M:datatable_page(pcols(), people(), Q#{filters := #{<<"city">> => {empty, <<>>}}})).

remote_render_test() ->
    Src = {?MODULE, query, #{}},
    H = r(?M:ah_datatable(pcols(), lists:sublist(people(), 2), [],
                          [{id, rq}, {source, Src}, {total, 42}, {page_size, 2}, {page, 3}])),
    ?assert(has(<<"data-mode=\"remote\"">>, H)),
    ?assert(has(<<"data-page=\"3\" data-page-size=\"2\" data-total=\"42\"">>, H)),
    ?assert(has(<<"data-ah-on=\"ah:query:">>, H)),
    ?assert(has(<<"data-ah-sync=\"replace\"">>, H)),
    %% the rows given are the page, all shown
    ?assertEqual([<<"1">>, <<"2">>], shown(H)),
    ?assert(has(<<"<div class=\"ah-dt-pager-info\">5-6 of 42</div>">>, H)).

%% The actions of the round trips: the remote table's source and its edit.
action(query, _, Event, Ctx) ->
    {Rows, Total} = ?M:datatable_page(pcols(), people(), ?M:datatable_query(Event)),
    ?M:datatable_rows(Ctx, Event, remote(Rows, Total));
action(edit, _, #{data := #{<<"key">> := K, <<"value">> := V}} = Event, Ctx) ->
    [Row] = [P || #{id := I} = P <- people(), integer_to_binary(I) =:= K],
    ?M:datatable_row(Ctx, Event, ?M:ah_datatable(pcols(), [], [], [{editable, true}]), Row#{name => V}).

remote(Rows, Total) ->
    ?M:ah_datatable(pcols(), Rows, [], [{source, {?MODULE, query, #{}}}, {total, Total},
                                        {page_size, 5}, {filter, row}, {selection_mode, multiple}]).

remote_round_trip_test() ->
    Event = query_event(#{<<"sortField">> => <<"age">>, <<"sortDir">> => <<"asc">>,
                          <<"page">> => <<"1">>, <<"pageSize">> => <<"2">>,
                          <<"filters">> => <<"{\"city\":\"o\"}">>, <<"hidden">> => <<"city">>,
                          <<"expanded">> => <<>>}),
    [#{op := html, id := <<"rq">>, swap := morph, html := H}] =
        aihtml_action:render_ops(fun(Ctx) -> action(query, #{}, Event, Ctx) end),
    ?assertEqual([<<"2">>, <<"1">>], keys(H)),
    ?assert(has(<<"<div class=\"ah-dt\" id=\"rq\" role=\"grid\" aria-multiselectable=\"true\" "
                  "data-ah=\"datatable\" data-ah-value=\"3\" data-selection=\"multiple\" data-mode=\"remote\" "
                  "data-sortable=\"true\" data-alt-rows=\"true\" data-sort-field=\"age\" data-sort-dir=\"asc\" "
                  "data-filter=\"row\" data-page=\"1\" data-page-size=\"2\" data-total=\"3\" "
                  "data-hidden=\"city\"">>, H)),
    ?assert(has(<<"<input class=\"ah-dt-filter-input\" data-field=\"city\" type=\"text\" "
                  "placeholder=\"Filter...\" value=\"o\" aria-label=\"City\">">>, H)),
    ?assert(has(<<"<th class=\"ah-dt-th ah-dt-th-sortable\" role=\"columnheader\" data-field=\"city\" "
                  "data-type=\"text\" style=\"text-align:left;\" tabindex=\"0\" hidden>">>, H)),
    ?assert(has(<<"<div class=\"ah-dt-pager-info\">1-2 of 3</div>">>, H)),
    ?assertError({aihtml, {datatable_rows_needs_source, _}},
                 aihtml_action:render_ops(fun(Ctx) ->
                                                  ?M:datatable_rows(Ctx, Event, ?M:ah_datatable(pcols(), [], [], []))
                                          end)).

%%%===================================================================
%%% Links (href)
%%%===================================================================

-define(HREF, <<"/t?p={page}&s={size}&o={sort}&q={search}">>).

href_local_test() ->
    H = r(?M:ah_datatable(pcols(), people(), [],
                          [{id, hl}, {page_size, 2}, {page, 2}, {sort, {age, desc}},
                           {search, <<"a & b">>}, {filter, search}, {href, ?HREF}])),
    %% nothing matches "a & b": one empty page, prev/next disabled buttons
    ?assert(has(<<"data-href=\"/t?p={page}&amp;s={size}&amp;o={sort}&amp;q={search}\"">>, H)),
    H2 = r(?M:ah_datatable(pcols(), people(), [],
                           [{id, hl}, {page_size, 2}, {page, 2}, {sort, {age, desc}},
                            {href, ?HREF}])),
    U = fun(P) -> <<"/t?p=", P/binary, "&amp;s=2&amp;o=age%3Adesc&amp;q=">> end,
    ?assert(has(<<"<a class=\"ah-dt-pager-btn ah-dt-pager-btn-prev\" href=\"", (U(<<"1">>))/binary,
                  "\" aria-label=\"Previous page\">">>, H2)),
    ?assert(has(<<"<a class=\"ah-dt-pager-btn ah-dt-pager-btn-num\" href=\"", (U(<<"3">>))/binary,
                  "\" data-page=\"3\">3</a>">>, H2)),
    ?assert(has(<<"<a class=\"ah-dt-pager-btn ah-dt-pager-btn-next\" href=\"", (U(<<"3">>))/binary, "\"">>, H2)),
    %% the current page stays a button
    ?assert(has(<<"<button class=\"ah-dt-pager-btn ah-dt-pager-btn-num ah-dt-pager-btn-active\" "
                  "type=\"button\" data-page=\"2\" aria-current=\"page\">2</button>">>, H2)),
    %% the first page: prev is a disabled button, not a link
    H3 = r(?M:ah_datatable(pcols(), people(), [], [{id, hl}, {page_size, 2}, {href, ?HREF}])),
    ?assert(has(<<"<button class=\"ah-dt-pager-btn ah-dt-pager-btn-prev\" type=\"button\" "
                  "aria-label=\"Previous page\" disabled>">>, H3)),
    ?assert(has(<<"href=\"/t?p=2&amp;s=2&amp;o=&amp;q=\"">>, H3)).

href_encoding_test() ->
    V = ?M:pager_view(1, 10, 25, [], #{info => <<>>, prev => <<>>, next => <<>>, page_size => <<>>},
                      <<"/x?n={page}&s={size}">>),
    ?assertMatch(#{prev_link := false, next_link := true, next_href := <<"/x?n=2&s=10">>}, V),
    Odd = <<"Ö &'()*!~ /"/utf8>>,
    Rows = [#{id => I, name => Odd, age => I, city => <<>>} || I <- [1, 2, 3]],
    H = r(?M:ah_datatable(pcols(), Rows, [],
                          [{id, he}, {page_size, 1}, {filter, search}, {search, Odd},
                           {href, <<"/y?q={search}&p={page}">>}])),
    %% as encodeURIComponent: ' ( ) * ! ~ stay, the rest %XX (UTF-8)
    ?assert(has(<<"href=\"/y?q=%C3%96%20%26&#39;()*!~%20%2F&amp;p=2\"">>, H)).

href_absent_test() ->
    %% without href: no links, no data-href
    H = r(?M:ah_datatable(pcols(), people(), [], [{id, hn}, {page_size, 2}, {page, 2}])),
    ?assertNot(has_quiet(<<"<a ">>, H)),
    ?assertNot(has_quiet(<<"data-href">>, H)),
    ?assertEqual(?M:pager_view(2, 2, 5, [5], #{info => <<>>, prev => <<>>, next => <<>>, page_size => <<>>}),
                 ?M:pager_view(2, 2, 5, [5], #{info => <<>>, prev => <<>>, next => <<>>, page_size => <<>>},
                               undefined)).

href_round_trip_test() ->
    %% datatable_rows re-renders the table the action passes, links included,
    %% with the event's sort and page
    Event = query_event(#{<<"sortField">> => <<"name">>, <<"sortDir">> => <<"desc">>,
                          <<"page">> => <<"2">>, <<"pageSize">> => <<"2">>}),
    Table = ?M:ah_datatable(pcols(), lists:sublist(people(), 2), [],
                            [{source, {?MODULE, query, #{}}}, {total, 5}, {page_size, 2},
                             {href, ?HREF}]),
    [#{op := html, html := H}] =
        aihtml_action:render_ops(fun(Ctx) -> ?M:datatable_rows(Ctx, Event, Table) end),
    ?assert(has(<<"href=\"/t?p=3&amp;s=2&amp;o=name%3Adesc&amp;q=\"">>, H)),
    ?assert(has(<<"href=\"/t?p=1&amp;s=2&amp;o=name%3Adesc&amp;q=\"">>, H)).

remote_no_total_test() ->
    %% remote mode renders what it is given: total defaults to the rows,
    %% none shows the empty text (the table never loads on mount)
    H = r(?M:ah_datatable(pcols(), [], [], [{id, re}, {source, {?MODULE, query, #{}}}, {page_size, 2}])),
    ?assert(has(<<"data-total=\"0\"">>, H)),
    ?assert(has(<<"<tr class=\"ah-dt-row-empty\"><td class=\"ah-dt-cell-empty\"">>, H)).

edit_round_trip_test() ->
    Event = #{type => <<"ah:cell-edit">>, id => <<"ah-e1">>, value => null,
              data => #{<<"key">> => <<"2">>, <<"field">> => <<"name">>, <<"value">> => <<"Bo">>,
                        <<"old">> => <<"Bob">>, <<"table">> => <<"ed">>}},
    [#{op := html, id := <<"ed-r-2">>, swap := morph, html := Tr},
     #{op := call, id := <<"ed">>, method := <<"refresh">>, args := []}] =
        aihtml_action:render_ops(fun(Ctx) -> action(edit, #{}, Event, Ctx) end),
    ?assert(has(<<"<tr class=\"ah-dt-row ah-dt-row-hover\" id=\"ed-r-2\" role=\"row\" data-key=\"2\"">>, Tr)),
    ?assert(has(<<"<td class=\"ah-dt-cell ah-dt-cell-editable\" role=\"gridcell\" data-field=\"name\" "
                  "style=\"text-align:left;\"><span>Bo</span></td>">>, Tr)).

%%%===================================================================
%%% Catalog
%%%===================================================================

catalog_test() ->
    [#{name := datatable, category := data}] = ?M:catalog(),
    ?assertEqual([{datatable_rows, 3}, {datatable_row, 4}, {datatable_query, 1}], ?M:facade_extras()),
    [?assert(erlang:function_exported(?M, F, A)) || {F, A} <- ?M:facade_extras()],
    [begin
         #{flags := Fl, options := Op, option_docs := Docs, methods := Ms} = E =
             aihtml_catalog:entry(?M, N),
         ?assertEqual(lists:sort(Fl ++ Op), lists:sort(maps:keys(Docs))),
         [_ | _] = aihtml_catalog:classes(E, Fl),
         [?assert(is_binary(D)) || #{doc := D} <- Ms]
     end || #{name := N} <- ?M:catalog()].

catalog_docs_test() ->
    [begin
         ?assert(byte_size(maps:get(doc, E)) > 0),
         [?assert(byte_size(maps:get(K, maps:get(option_docs, E))) > 0)
          || K <- maps:get(options, E, []) ++ maps:get(flags, E, [])]
     end || E <- ?M:catalog()].

%%%===================================================================
%%% element record (designs/05-records.md)
%%%===================================================================

record_equals_builder_test() ->
    ?assertEqual(r(?M:ah_datatable(pcols(), people(), [],
                                   [{id, d}, {page_size, 2}, {filter, row}, {sort, {age, asc}},
                                    {texts, #{info => <<"{total}">>}}])),
                 r(#ah_datatable{columns = pcols(), rows = people(), id = d, page_size = 2,
                                 filter = row, sort = {age, asc}, texts = #{info => <<"{total}">>}})).

builder_fills_fields_test() ->
    ?assertMatch(#ah_datatable{columns = [a], rows = [], page_size = 5, total = 9,
                               source = {m, a, []}},
                 ?M:ah_datatable([a], [], [], [{page_size, 5}, {total, 9}, {source, {m, a, []}}])),
    ?assertError({aihtml, {record_only_field, ah_datatable, postback}},
                 ?M:ah_datatable([], [], [], [{postback, pick}])).

postback_test() ->
    Token = fun(Html) ->
                    {match, [Ev, Tok]} = re:run(r(Html), <<"data-ah-on=\"([a-z:]+):([^\" ]+)\"">>,
                                                [{capture, all_but_first, binary}]),
                    {ok, Ref} = aihtml_action:unsign(Tok),
                    {Ev, Ref}
            end,
    ?assertEqual({<<"change">>, {other, go, #{}}},
                 Token(#ah_datatable{columns = [a], postback = go, delegate = other})).

field_validation_test() ->
    ?assertError({aihtml, {bad_row, x}}, r(#ah_datatable{columns = [a], rows = [x]})),
    ?assertError({aihtml, {bad_column_key, widht, _}},
                 r(#ah_datatable{columns = [#{field => a, widht => 3}]})),
    ?assertError({aihtml, {bad_column, align, middle}},
                 r(#ah_datatable{columns = [#{field => a, align => middle}]})),
    ?assertError({aihtml, {bad_column, {1, 2, 3}}}, r(#ah_datatable{columns = [{1, 2, 3}]})),
    ?assertError({aihtml, {bad_option, filter, all}}, r(#ah_datatable{filter = all})),
    ?assertError({aihtml, {bad_option, filters, {near, 1}}},
                 r(#ah_datatable{filters = #{a => {near, 1}}})),
    ?assertError({aihtml, {bad_option, page_size, 0}}, r(#ah_datatable{page_size = 0})),
    ?assertError({aihtml, {bad_option, page_sizes, [a]}}, r(#ah_datatable{page_sizes = [a]})),
    ?assertError({aihtml, {bad_option, total, -1}}, r(#ah_datatable{total = -1})),
    ?assertError({aihtml, {bad_option, row_details, x}}, r(#ah_datatable{row_details = x})),
    ?assertError({aihtml, {bad_option, texts, nope}}, r(#ah_datatable{texts = #{nope => <<"x">>}})),
    ?assertError({aihtml, {bad_option, height, -3}}, r(#ah_datatable{height = -3})),
    ?assertError({aihtml, {bad_option, source, {1, 2}}}, r(#ah_datatable{source = {1, 2}})),
    ?assertError({aihtml, {bad_flag, datatable, disabled, yes}}, r(#ah_datatable{disabled = yes})).

records_match_catalog_test() ->
    Base = [module, id, css, attrs, postback, delegate],
    [begin
         Tag = list_to_atom("ah_" ++ atom_to_list(N)),
         Fields = ?M:fields(Tag),
         ?assertEqual(Base, lists:sublist(Fields, 6)),
         Defaults = maps:from_list(lists:zip(Fields, tl(tuple_to_list(default(Tag))))),
         [?assertEqual({N, G, case D of none -> undefined; _ -> D end},
                       {N, G, maps:get(G, Defaults)})
          || {G, {_, D}} <- maps:to_list(maps:get(groups, E, #{}))],
         [?assertEqual({N, F, false}, {N, F, maps:get(F, Defaults)})
          || F <- maps:get(flags, E, [])],
         [?assert(lists:member(O, Fields)) || O <- maps:get(options, E, [])],
         ?assertEqual(?M, maps:get(module, Defaults))
     end || #{name := N} = E <- ?M:catalog()].

default(ah_datatable) -> #ah_datatable{}.

%% A value containing a comma is escaped in data-ah-value (aihtml_value).
vhas(Sub, Bin) -> binary:match(Bin, Sub) =/= nomatch.

comma_values_test() ->
    Rows = [#{id => <<"a,b">>, name => <<"A">>}, #{id => <<"c">>, name => <<"C">>}],
    H = r(?M:ah_datatable([name], Rows, [], [{id, dt}, {selection_mode, multiple},
                                             {name, sel}, {expanded, [<<"a,b">>]},
                                             {value, [<<"a,b">>, <<"c">>]}])),
    ?assert(vhas(<<"data-ah-value=\"a\\,b,c\"">>, H)),
    ?assert(vhas(<<"data-expanded=\"a\\,b\"">>, H)),
    %% the same selection as text
    ?assertEqual(H, r(?M:ah_datatable([name], Rows, [], [{id, dt}, {selection_mode, multiple},
                                                         {name, sel}, {expanded, [<<"a,b">>]},
                                                         {value, <<"a\\,b,c">>}]))).

comma_remote_test() ->
    Event = (query_event(#{<<"page">> => <<"1">>, <<"pageSize">> => <<"5">>,
                           <<"expanded">> => <<"1\\,5">>}))#{value => <<"1\\,5,2">>},
    Rows = [#{name => <<"x">>, age => 1, city => <<"c">>, id => <<"1,5">>},
            #{name => <<"y">>, age => 2, city => <<"c">>, id => 2}],
    [#{op := html, html := H}] =
        aihtml_action:render_ops(fun(Ctx) -> ?M:datatable_rows(Ctx, Event, remote(Rows, 2)) end),
    ?assert(vhas(<<"data-ah-value=\"1\\,5,2\"">>, H)),
    ?assert(vhas(<<"data-expanded=\"1\\,5\"">>, H)).
