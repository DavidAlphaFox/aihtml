%% Tests for aihtml_treegrid. The module is also the fake action module
%% of the lazy tree grid round trip.
-module(aihtml_treegrid_tests).
-behaviour(aihtml_action).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_treegrid.hrl").

-export([action/4]).

-define(M, aihtml_treegrid).

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
%%% treegrid
%%%===================================================================

cols() -> [#{field => name, title => <<"Name">>, width => 200}, {size, <<"Size">>}].

nested() ->
    [#{id => 1, name => <<"Docs">>, size => 30,
       children => [#{id => 2, name => <<"b.txt">>, size => 20},
                    #{id => 3, name => <<"a.txt">>, size => 10,
                      children => [#{id => 4, name => <<"x">>, size => 5}]}]},
     #{id => 5, name => <<"Lazy">>, children => lazy},
     #{id => 6, name => <<"z.md">>, size => 1}].

treegrid_structure_test() ->
    H = r(?M:ah_treegrid(cols(), nested(), [<<"mt-2">>], [{id, tg}, {expanded, [1]}])),
    ?assert(has(<<"<div class=\"ah-tg mt-2\" id=\"tg\" role=\"treegrid\" data-ah=\"treegrid\" "
                  "data-ah-value=\"\" data-selection=\"single\" data-sortable=\"true\" "
                  "data-alt-rows=\"true\">">>, H)),
    ?assert(has(<<"<colgroup><col style=\"width:200px;min-width:200px;\"><col></colgroup>">>, H)),
    ?assert(has(<<"<th class=\"ah-tg-th ah-tg-th-sortable\" role=\"columnheader\" data-field=\"name\" "
                  "data-type=\"text\" style=\"text-align:left;\" tabindex=\"0\"><div class=\"ah-tg-th-content\">"
                  "<span class=\"ah-tg-th-text\">Name</span><span class=\"ah-tg-sort-icon\" aria-hidden=\"true\">"
                  "</span></div></th>">>, H)),
    ?assert(has(<<"<tbody role=\"rowgroup\" id=\"tg-rows\">">>, H)),
    %% depth first, ids from positions, children of closed nodes hidden
    ?assertEqual([<<"1">>, <<"2">>, <<"3">>, <<"4">>, <<"5">>, <<"6">>], keys(H)),
    ?assertEqual([<<"1">>, <<"2">>, <<"3">>, <<"5">>, <<"6">>], shown(H)),
    ?assert(has(<<"<tr class=\"ah-tg-row ah-tg-row-hover\" id=\"tg-0\" role=\"row\" data-key=\"1\" "
                  "data-parent=\"\" data-level=\"0\" data-i=\"0\" aria-level=\"1\" aria-expanded=\"true\" "
                  "aria-selected=\"false\" tabindex=\"0\">">>, H)),
    ?assert(has(<<"id=\"tg-0-1\" role=\"row\" data-key=\"3\" data-parent=\"1\" data-level=\"1\" "
                  "data-i=\"1\" aria-level=\"2\" aria-expanded=\"false\"">>, H)),
    ?assert(has(<<"id=\"tg-0-1-0\" role=\"row\" data-key=\"4\" data-parent=\"3\" data-level=\"2\" "
                  "data-i=\"0\" aria-level=\"3\" aria-selected=\"false\" tabindex=\"-1\" hidden>">>, H)),
    %% the tree cell: indent, arrow, text; other cells keep the raw number
    ?assert(has(<<"<td class=\"ah-tg-cell ah-tg-tree-cell\" role=\"gridcell\" data-field=\"name\" "
                  "style=\"text-align:left;\"><div class=\"ah-tg-tree-indent\" style=\"padding-left:24px;\">"
                  "<span class=\"ah-tg-toggle ah-tg-toggle-closed\" aria-hidden=\"true\">▶</span>"
                  "<span class=\"ah-tg-cell-text\"><span>a.txt</span></span></div></td>"/utf8>>, H)),
    ?assert(has(<<"<span class=\"ah-tg-toggle ah-tg-toggle-leaf\" aria-hidden=\"true\"></span>">>, H)),
    ?assert(has(<<"<td class=\"ah-tg-cell\" role=\"gridcell\" data-field=\"size\" style=\"text-align:left;\" "
                  "data-value=\"20\"><span>20</span></td>">>, H)),
    %% zebra stripes by visible position: 2 and 5 are the 2nd and 4th shown
    ?assert(has(<<"<tr class=\"ah-tg-row ah-tg-row-leaf ah-tg-row-alt ah-tg-row-hover\" id=\"tg-0-0\"">>, H)),
    ?assert(has(<<"<tr class=\"ah-tg-row ah-tg-row-alt ah-tg-row-hover\" id=\"tg-1\"">>, H)),
    %% a lazy row is expandable and names the grid; no load token without load
    ?assert(has(<<"data-key=\"5\" data-parent=\"\" data-level=\"0\" data-i=\"1\" aria-level=\"1\" "
                  "aria-expanded=\"false\" aria-selected=\"false\" tabindex=\"-1\" data-lazy=\"true\" "
                  "data-treegrid=\"tg\">">>, H)),
    ?assertNot(has_quiet(<<"data-load">>, H)),
    ?assert(has(<<"<tr class=\"ah-tg-row-empty\" hidden><td class=\"ah-tg-cell-empty\" colspan=\"2\">"
                  "No data to display</td></tr>">>, H)).

treegrid_flat_test() ->
    Flat = [#{id => a, name => <<"A">>}, #{id => b, name => <<"B">>, parent_id => a},
            #{id => c, name => <<"C">>, parent_id => b}, #{id => d, name => <<"D">>, parent_id => nope},
            #{id => e, name => <<"E">>, parent_id => a}],
    H = r(?M:ah_treegrid([name], Flat, [], [{id, f}, {expanded, all}])),
    ?assertEqual([<<"a">>, <<"b">>, <<"c">>, <<"e">>, <<"d">>], keys(H)),
    ?assertEqual(keys(H), shown(H)),
    ?assert(has(<<"data-key=\"c\" data-parent=\"b\" data-level=\"2\"">>, H)),
    %% an unknown parent makes a root
    ?assert(has(<<"data-key=\"d\" data-parent=\"\" data-level=\"0\"">>, H)),
    %% other field names
    H2 = r(?M:ah_treegrid([t], [#{k => 1, t => <<"x">>}, #{k => 2, t => <<"y">>, up => 1}], [],
                          [{key_field, k}, {parent_field, up}, {id, g}])),
    ?assertEqual([<<"1">>, <<"2">>], keys(H2)),
    ?assertEqual([<<"1">>], shown(H2)).

treegrid_selection_test() ->
    H = r(?M:ah_treegrid(cols(), nested(), [],
                         [{id, s}, {selection_mode, checkbox}, {value, [2, 4, 99]}, {name, pick},
                          {expanded, [1]}])),
    %% unknown keys are dropped from the value
    ?assert(has(<<"aria-multiselectable=\"true\" data-ah=\"treegrid\" data-ah-value=\"2,4\" "
                  "data-selection=\"checkbox\"">>, H)),
    ?assert(has(<<"<input type=\"hidden\" name=\"pick\" value=\"2,4\" data-ah-input>">>, H)),
    ?assert(has(<<"<colgroup><col style=\"width:40px;min-width:40px;\">">>, H)),
    ?assert(has(<<"<th class=\"ah-tg-th ah-tg-th-checkbox\" role=\"columnheader\"><input "
                  "class=\"ah-tg-header-checkbox\" type=\"checkbox\" tabindex=\"-1\" "
                  "aria-label=\"Select all rows\"></th>">>, H)),
    ?assert(has(<<"<td class=\"ah-tg-cell ah-tg-checkbox-cell\" role=\"gridcell\"><input "
                  "class=\"ah-tg-row-checkbox\" type=\"checkbox\" tabindex=\"-1\" checked "
                  "aria-label=\"Select row\"></td>">>, H)),
    ?assertEqual(2, count(<<"ah-tg-row-selected">>, H)),
    ?assertEqual(2, count(<<"aria-selected=\"true\"">>, H)),
    %% the tab stop goes to the first visible selected row (4 is hidden)
    ?assert(has(<<"data-key=\"2\" data-parent=\"1\" data-level=\"1\" data-i=\"0\" aria-level=\"2\" "
                  "aria-selected=\"true\" tabindex=\"0\"">>, H)),
    %% none: no aria-selected at all; a comma separated value works too
    H2 = r(?M:ah_treegrid(cols(), nested(), [], [{selection_mode, none}, {value, <<"1,6">>}])),
    ?assertNot(has_quiet(<<"aria-selected">>, H2)),
    ?assertNot(has_quiet(<<"ah-tg-row-selected">>, H2)),
    H3 = r(?M:ah_treegrid(cols(), nested(), [], [{value, <<"1,6">>}])),
    ?assert(has(<<"data-ah-value=\"1,6\"">>, H3)).

treegrid_sort_and_options_test() ->
    H = r(?M:ah_treegrid(cols(), nested(), [disabled],
                         [{id, o}, {sort, {size, asc}}, {expanded, all}, {tree_column, size},
                          {indent, 10}, {alt_rows, false}, {hover, false}, {resizable, true},
                          {height, 300}, {empty_text, <<"Nothing">>}, {load, {?MODULE, kids, #{}}}])),
    %% siblings sorted by size; numbers before texts, so the row without a
    %% size (lazy 5) comes last
    ?assertEqual([<<"6">>, <<"1">>, <<"3">>, <<"4">>, <<"2">>, <<"5">>], keys(H)),
    ?assert(has(<<"class=\"ah-tg ah-tg-disabled\" id=\"o\" role=\"treegrid\" aria-disabled=\"true\"">>, H)),
    ?assert(has(<<"data-sort-field=\"size\" data-sort-dir=\"asc\" data-load=\"">>, H)),
    ?assert(has(<<"style=\"height:300px;\"">>, H)),
    ?assert(has(<<"ah-tg-th-sortable ah-tg-sort-asc\" role=\"columnheader\" data-field=\"size\" "
                  "data-type=\"text\" style=\"text-align:left;\" aria-sort=\"ascending\"">>, H)),
    ?assert(has(<<"<div class=\"ah-tg-resize-handle\" aria-hidden=\"true\"></div>">>, H)),
    ?assertNot(has_quiet(<<"ah-tg-row-alt">>, H)),
    ?assertNot(has_quiet(<<"ah-tg-row-hover">>, H)),
    ?assertNot(has_quiet(<<"data-alt-rows">>, H)),
    %% the tree column is size: indents there, name is a plain cell
    ?assert(has(<<"<td class=\"ah-tg-cell ah-tg-tree-cell\" role=\"gridcell\" data-field=\"size\"">>, H)),
    ?assert(has(<<"style=\"padding-left:20px;\"">>, H)),
    {match, [Token]} = re:run(H, <<"data-load=\"([^\"]+)\"">>, [{capture, all_but_first, binary}]),
    ?assertEqual({ok, {?MODULE, kids, #{}}}, aihtml_action:verify(Token)),
    %% no rows: the empty row shows; no header
    E = r(?M:ah_treegrid(cols(), [], [], [{show_header, false}, {empty_text, <<"Nothing">>}])),
    ?assert(has(<<"<tr class=\"ah-tg-row-empty\"><td class=\"ah-tg-cell-empty\" colspan=\"2\">Nothing</td></tr>">>, E)),
    ?assert(has(<<"<div class=\"ah-tg-header\" hidden>">>, E)),
    %% a renderer and an unsortable column
    R = r(?M:ah_treegrid([#{field => name, render => fun(V, #{id := I}) ->
                                                             [V, <<"#">>, integer_to_binary(I)]
                                                     end, sortable => false, align => right,
                            class => <<"font-bold">>}],
                         [#{id => 7, name => <<"n">>}], [], [])),
    ?assert(has(<<"<span class=\"ah-tg-cell-text\">n#7</span>">>, R)),
    ?assert(has(<<"class=\"ah-tg-cell ah-tg-tree-cell font-bold\" role=\"gridcell\" data-field=\"name\" "
                  "style=\"text-align:right;\" data-value=\"n\"">>, R)),
    ?assert(has(<<"<th class=\"ah-tg-th\" role=\"columnheader\" data-field=\"name\"">>, R)),
    ?assertNot(has_quiet(<<"ah-tg-sort-icon">>, R)).

treegrid_lazy_round_trip_test() ->
    Ref = {?MODULE, kids, #{children => [#{id => 50, name => <<"c1">>},
                                         #{id => 51, name => <<"c2">>, children => lazy}]}},
    H = r(?M:ah_treegrid(cols(), nested(), [], [{id, <<"lz">>}, {load, Ref}])),
    {match, [Token]} = re:run(H, <<"data-load=\"([^\"]+)\"">>, [{capture, all_but_first, binary}]),
    %% the browser sends the row's id and data-* attributes
    Event = #{<<"type">> => <<"ah:load">>, <<"id">> => <<"lz-1">>, <<"value">> => null,
              <<"data">> => #{<<"key">> => <<"5">>, <<"parent">> => <<>>, <<"level">> => <<"0">>,
                              <<"i">> => <<"1">>, <<"treegrid">> => <<"lz">>,
                              <<"value">> => <<"51">>}},
    {ok, Ops} = aihtml_action:execute(element(2, aihtml_action:verify(Token)), Event,
                                      #{send => fun(_) -> error(unexpected_flush) end}),
    [#{op := html, id := <<"lz-rows">>, swap := append, html := Rows},
     #{op := call, id := <<"lz">>, method := <<"childrenLoaded">>, args := [<<"lz-1">>]}] = Ops,
    ?assertEqual([<<"50">>, <<"51">>], keys(Rows)),
    ?assert(has(<<"id=\"lz-1-0\" role=\"row\" data-key=\"50\" data-parent=\"5\" data-level=\"1\" "
                  "data-i=\"0\" aria-level=\"2\"">>, Rows)),
    ?assert(has(<<"padding-left:24px;">>, Rows)),
    %% the current selection travels in data-value
    ?assert(has(<<"data-key=\"51\" data-parent=\"5\" data-level=\"1\" data-i=\"1\" aria-level=\"2\" "
                  "aria-expanded=\"false\" aria-selected=\"true\" tabindex=\"-1\" data-lazy=\"true\" "
                  "data-treegrid=\"lz\"">>, Rows)),
    %% an empty answer: nothing appended, the row becomes a leaf in the browser
    Ops2 = aihtml_action:render_ops(
             fun(Ctx) ->
                     ?M:treegrid_children(Ctx, #{id => <<"lz-1">>,
                                                 data => #{<<"key">> => <<"5">>, <<"level">> => <<"0">>,
                                                           <<"treegrid">> => <<"lz">>}},
                                          ?M:ah_treegrid(cols(), [], [], []))
             end),
    ?assertMatch([#{op := html, html := <<>>}, #{op := call, method := <<"childrenLoaded">>}], Ops2).


%% The action of the round trip: the lazy tree grid's load.
action(kids, #{children := Kids}, Event, Ctx) ->
    ?M:treegrid_children(Ctx, Event, ?M:ah_treegrid(cols(), Kids, [], [])).

%%%===================================================================
%%% Catalog
%%%===================================================================

catalog_test() ->
    [#{name := treegrid, category := data}] = ?M:catalog(),
    ?assertEqual([{treegrid_children, 3}], ?M:facade_extras()),
    [?assert(erlang:function_exported(?M, F, A)) || {F, A} <- ?M:facade_extras()],
    [begin
         #{flags := Fl, options := Op, option_docs := Docs, methods := Ms} = E =
             aihtml_catalog:entry(?M, N),
         ?assertEqual(lists:sort(Fl ++ Op), lists:sort(maps:keys(Docs))),
         [_ | _] = aihtml_catalog:classes(E, Fl),
         [?assert(is_binary(D)) || #{doc := D} <- Ms]
     end || #{name := N} <- ?M:catalog()],
    ?assertEqual([<<"ah-tg">>, <<"ah-tg-disabled">>],
                 aihtml_catalog:classes(aihtml_catalog:entry(?M, treegrid), [disabled])).

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
    Load = {?MODULE, kids, #{}},
    ?assertEqual(r(?M:ah_treegrid(cols(), nested(), [disabled, <<"w-64">>],
                                  [{id, t}, {value, [2]}, {name, n}, {selection_mode, multiple},
                                   {expanded, all}, {sort, {size, desc}}, {load, Load},
                                   {title, <<"t">>}])),
                 r(#ah_treegrid{columns = cols(), items = nested(), disabled = true,
                                css = [<<"w-64">>], id = t, value = [2], name = n,
                                selection_mode = multiple, expanded = all, sort = {size, desc},
                                load = Load, attrs = [{title, <<"t">>}]})).

builder_fills_fields_test() ->
    T = ?M:ah_treegrid([a], [], [disabled, <<"x">>], [{name, n}, {indent, 8}, {role, x}]),
    ?assertMatch(#ah_treegrid{columns = [a], items = [], disabled = true, name = n, indent = 8,
                              selection_mode = single, css = [<<"x">>], attrs = [{role, x}]}, T).

postback_test() ->
    Token = fun(Html) ->
                    {match, [Ev, Tok]} = re:run(r(Html), <<"data-ah-on=\"([a-z:]+):([^\" ]+)\"">>,
                                                [{capture, all_but_first, binary}]),
                    {ok, Ref} = aihtml_action:unsign(Tok),
                    {Ev, Ref}
            end,
    ?assertEqual({<<"change">>, {?MODULE, picked, #{id => 7}}},
                 Token(#ah_treegrid{columns = [a], postback = {picked, #{id => 7}}})).

field_validation_test() ->
    ?assertError({aihtml, {bad_option, selection_mode, many}},
                 r(#ah_treegrid{selection_mode = many})),
    ?assertError({aihtml, {bad_option, expanded, some}}, r(#ah_treegrid{expanded = some})),
    ?assertError({aihtml, {bad_option, indent, -1}}, r(#ah_treegrid{indent = -1})),
    ?assertError({aihtml, {bad_option, load, nope}}, r(#ah_treegrid{load = nope})),
    ?assertError({aihtml, {bad_option, sort, {a, up}}}, r(#ah_treegrid{sort = {a, up}})),
    ?assertError({aihtml, {bad_option, tree_column, zz}},
                 r(#ah_treegrid{columns = [a], tree_column = zz})),
    ?assertError({aihtml, {row_without_key, _}}, r(#ah_treegrid{columns = [a], items = [#{a => 1}]})),
    ?assertError({aihtml, {duplicate_row_keys, [<<"1">>]}},
                 r(#ah_treegrid{columns = [a], items = [#{id => 1}, #{id => 1}]})),
    ?assertError({aihtml, {bad_children, 3}},
                 r(#ah_treegrid{columns = [a], items = [#{id => 1, children => [#{id => 2}]},
                                                        #{id => 3, children => 3}]})),
    ?assertError({aihtml, {unknown_modifier, treegrid, big, _}}, ?M:ah_treegrid([], [], [big], [])).

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

default(ah_treegrid) -> #ah_treegrid{}.

%% A value containing a comma is escaped in data-ah-value (aihtml_value).
vhas(Sub, Bin) -> binary:match(Bin, Sub) =/= nomatch.

comma_values_test() ->
    Items = [#{id => <<"a,b">>, name => <<"A">>, size => 1}, #{id => c, name => <<"C">>, size => 2}],
    H = r(?M:ah_treegrid(cols(), Items, [], [{id, tg}, {selection_mode, multiple}, {value, [<<"a,b">>, c]}])),
    ?assert(vhas(<<"data-ah-value=\"a\\,b,c\"">>, H)),
    ?assertEqual(H, r(?M:ah_treegrid(cols(), Items, [], [{id, tg}, {selection_mode, multiple},
                                                        {value, <<"a\\,b,c">>}]))).
