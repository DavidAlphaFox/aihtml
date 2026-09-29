%% Tests for aihtml_form_lists. The module is also the fake action module
%% of the lazy cascader and listbox search round trips.
-module(aihtml_form_lists_tests).
-behaviour(aihtml_action).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_form_lists.hrl").

-export([action/4]).

-define(M, aihtml_form_lists).

r(Html) -> aihtml_html:render_binary(Html).

has(Needle, Hay) ->
    case binary:match(Hay, Needle) of
        nomatch -> ?debugFmt("~s not in~n~s", [Needle, Hay]), false;
        _ -> true
    end.

has_quiet(Needle, Hay) -> binary:match(Hay, Needle) =/= nomatch.

count(Needle, Hay) -> length(binary:matches(Hay, Needle)).

tree() ->
    [{<<"zj">>, <<"Zhejiang">>,
      [{<<"hz">>, <<"Hangzhou">>, [<<"xihu">>, {<<"bj">>, <<"Binjiang">>}]},
       #{value => <<"nb">>, label => <<"Ningbo">>, disabled => true}]},
     {<<"js">>, <<"Jiangsu">>, lazy},
     <<"hk">>].

%%%===================================================================
%%% cascader
%%%===================================================================

cascader_basic_test() ->
    H = r(?M:cascader(tree(), [<<"zj">>, <<"hz">>, <<"bj">>], [<<"w-64">>],
                      [{id, cs}, {name, region}, {title, <<"t">>}])),
    ?assert(has(<<"<div class=\"ah-cascader w-64\" id=\"cs\" data-ah=\"cascader\" "
                  "data-ah-value=\"zj,hz,bj\"">>, H)),
    ?assert(has(<<"<input type=\"hidden\" name=\"region\" value=\"zj,hz,bj\">">>, H)),
    ?assert(has(<<"value=\"Zhejiang / Hangzhou / Binjiang\"">>, H)),
    ?assert(has(<<"readonly">>, H)),
    ?assert(has(<<"placeholder=\"Please select\"">>, H)),
    ?assert(has(<<"role=\"combobox\"">>, H)),
    ?assert(has(<<"aria-controls=\"cs-menus\"">>, H)),
    ?assert(has(<<"title=\"t\"">>, H)),
    ?assertEqual(1, count(<<"name=">>, H)),
    %% one column for the top level and one per branch, the top one shown
    ?assertEqual(3, count(<<"class=\"ah-cascader-menu-column\"">>, H)),
    ?assert(has(<<"data-level=\"0\" data-parent=\"\"><ul">>, H)),
    ?assert(has(<<"data-level=\"1\" data-parent=\"zj\" hidden>">>, H)),
    ?assert(has(<<"data-level=\"2\" data-parent=\"zj,hz\" hidden>">>, H)),
    %% the path is active, branches have arrows, lazy nodes are marked
    ?assert(has(<<"<li class=\"has-children active\" role=\"option\" aria-selected=\"true\" "
                  "aria-haspopup=\"true\" data-value=\"zj\" data-level=\"0\">">>, H)),
    ?assert(has(<<"<li class=\"active\" role=\"option\" aria-selected=\"true\" "
                  "data-value=\"bj\" data-level=\"2\">">>, H)),
    ?assert(has(<<"data-value=\"js\" data-level=\"0\" data-lazy>">>, H)),
    ?assert(has(<<"aria-disabled=\"true\" data-value=\"nb\"">>, H)),
    ?assert(has(<<"<span class=\"ah-cascader-menu-item-label\">Hangzhou</span>">>, H)),
    ?assert(has(<<"<span class=\"ah-cascader-menu-item-arrow\" aria-hidden=\"true\">▶"/utf8>>, H)),
    ?assert(has(<<"style=\"max-height:240px\"">>, H)),
    ?assert(has(<<"<span class=\"ah-cascader-clear\" role=\"button\" aria-label=\"Clear\">">>, H)),
    ?assert(has(<<"ah-cascader-arrow-icon">>, H)),
    ?assertNot(has_quiet(<<"ah-cascader-search-panel">>, H)),
    ?assertNot(has_quiet(<<"ah-cascader-loader">>, H)).

cascader_empty_and_unknown_test() ->
    E = r(?M:cascader(tree(), undefined, [], [])),
    ?assert(has(<<"data-ah-value=\"\"">>, E)),
    ?assert(has(<<"id=\"ah-l">>, E)),
    ?assert(has(<<"<span class=\"ah-cascader-clear\" role=\"button\" aria-label=\"Clear\" hidden>">>, E)),
    ?assertNot(has_quiet(<<"class=\"active\"">>, E)),
    %% a path below a lazy node shows the values it cannot resolve
    U = r(?M:cascader(tree(), [<<"js">>, <<"nj">>], [], [{separator, <<" > ">>}])),
    ?assert(has(<<"value=\"Jiangsu &gt; nj\"">>, U)),
    ?assertError({aihtml, {bad_option, value, <<"zj">>}}, r(?M:cascader(tree(), <<"zj">>, [], []))),
    ?assertError({aihtml, {bad_cascader_node, _}}, r(?M:cascader([{1, 2, 3, 4}], undefined, [], []))),
    ?assertError({aihtml, {bad_cascader_node, _}},
                 r(?M:cascader([#{value => a, kids => []}], undefined, [], []))).

cascader_flags_options_test() ->
    H = r(?M:cascader(tree(), undefined,
                      [lg, disabled, filterable, change_on_select, no_arrow, no_clear],
                      [{id, c}, {placeholder, <<"Pick">>}, {popup_height, 300},
                       {empty_text, <<"None">>}, {separator, <<"/">>}])),
    ?assert(has(<<"class=\"ah-cascader ah-cascader-lg ah-cascader-change-on-select ah-cascader-disabled "
                  "ah-cascader-filterable ah-cascader-no-arrow ah-cascader-no-clear\"">>, H)),
    ?assert(has(<<"data-ah-separator=\"/\" data-ah-empty=\"None\" data-ah-change-on-select">>, H)),
    ?assert(has(<<"aria-disabled=\"true\"">>, H)),
    ?assert(has(<<"placeholder=\"Pick\"">>, H)),
    ?assert(has(<<"max-height:300px">>, H)),
    ?assertNot(has_quiet(<<"readonly">>, H)),
    ?assert(has(<<"aria-autocomplete=\"list\"">>, H)),
    ?assertNot(has_quiet(<<"ah-cascader-clear">>, H)),
    ?assertNot(has_quiet(<<"ah-cascader-arrow">>, H)),
    %% every path (change_on_select), a disabled node disables its path,
    %% lazy nodes are searched as themselves only
    ?assert(has(<<"<ul class=\"ah-cascader-search-panel\" id=\"c-search\" role=\"listbox\" hidden>">>, H)),
    ?assert(has(<<"data-path=\"zj,hz,bj\" data-label=\"Zhejiang/Hangzhou/Binjiang\">"
                  "<span class=\"ah-cascader-search-item-label\">Zhejiang/Hangzhou/Binjiang</span></li>">>, H)),
    ?assert(has(<<"data-path=\"zj\"">>, H)),
    ?assert(has(<<"aria-disabled=\"true\" data-path=\"zj,nb\"">>, H)),
    ?assert(has(<<"data-path=\"js\"">>, H)),
    S = r(?M:cascader(tree(), undefined, [sm, filterable], [])),
    ?assert(has(<<"ah-cascader-sm">>, S)),
    ?assertNot(has_quiet(<<"data-path=\"zj\"">>, S)),
    ?assertNot(has_quiet(<<"data-path=\"js\"">>, S)),
    ?assert(has(<<"data-path=\"hk\"">>, S)),
    ?assertError({aihtml, {bad_option, popup_height, 0}},
                 r(?M:cascader([], undefined, [], [{popup_height, 0}]))),
    ?assertError({aihtml, {conflicting_modifiers, cascader, size, _}},
                 ?M:cascader([], undefined, [sm, lg], [])).

%%%===================================================================
%%% Lazy levels: render, verify the token, run the action
%%%===================================================================

action(load, _Args, #{value := <<"js">>} = Ev, Ctx) ->
    ?M:cascader_children(Ctx, Ev, [{<<"nj">>, <<"Nanjing">>, [<<"gulou">>]},
                                   {<<"sz">>, <<"Suzhou">>, lazy}]);
action(load, _Args, Ev, Ctx) ->
    ?M:cascader_children(Ctx, Ev, []);
action(search, #{source := Source}, #{value := Q} = Ev, Ctx) ->
    ?M:listbox_items(Ctx, Ev, [I || I <- Source,
                                    binary:match(string:lowercase(I), Q) =/= nomatch]).

lazy_round_trip_test() ->
    Ref = {?MODULE, load, #{}},
    H = r(?M:cascader(tree(), undefined, [], [{id, <<"cz">>}, {load, Ref}])),
    %% the loader carries the queued action and names the cascader
    {match, [Token]} = re:run(H, <<"<span class=\"ah-cascader-loader\" hidden "
                                   "data-cascader=\"cz\" data-ah-on=\"ah:load:([^\"]+)\" "
                                   "data-ah-sync=\"queue\"></span>">>,
                              [{capture, all_but_first, binary}]),
    {ok, Ref} = aihtml_action:verify(Token),
    Self = self(),
    Event = #{<<"type">> => <<"ah:load">>, <<"id">> => <<"x">>, <<"value">> => <<"js">>,
              <<"data">> => #{<<"cascader">> => <<"cz">>}},
    ok = aihtml_action:execute(Ref, Event, #{emit => fun(E) -> Self ! {ev, E} end}),
    [#{<<"value">> := [Html, Call]}] = [E || #{<<"type">> := <<"CUSTOM">>} = E <- collect()],
    #{op := html, swap := append, id := <<"cz-menus">>, html := Cols} = Html,
    ?assertEqual(2, count(<<"class=\"ah-cascader-menu-column\"">>, Cols)),
    ?assert(has(<<"data-level=\"1\" data-parent=\"js\" hidden><ul">>, Cols)),
    ?assert(has(<<"data-level=\"2\" data-parent=\"js,nj\" hidden>">>, Cols)),
    ?assert(has(<<"data-value=\"sz\" data-level=\"1\" data-lazy>">>, Cols)),
    ?assertEqual(#{op => call, id => <<"cz">>, method => <<"childrenLoaded">>,
                   args => [<<"js">>]}, Call),
    %% no children: only the call, the node becomes a leaf in the browser
    Ops = aihtml_action:render_ops(
            fun(Ctx) -> ?M:cascader_children(Ctx, {id, cz}, [<<"js">>, sz], []) end),
    ?assertEqual([#{op => call, id => <<"cz">>, method => <<"childrenLoaded">>,
                    args => [<<"js,sz">>]}], Ops),
    _ = iolist_to_binary(json:encode([Html, Call])).

collect() ->
    receive {ev, E} -> [E | collect()]
    after 0 -> []
    end.

%%%===================================================================
%%% listbox
%%%===================================================================

listbox_single_test() ->
    Items = [<<"a">>, {b, <<"Bee">>}, #{value => 3, label => <<"Three">>, disabled => true,
                                         icon => <<"/i.png">>}],
    H = r(?M:listbox(Items, b, [<<"w-60">>], [{id, lb}, {name, pick}])),
    ?assert(has(<<"<div class=\"ah-listbox w-60\" id=\"lb\" data-ah=\"listbox\" "
                  "data-ah-value=\"b\" tabindex=\"0\" role=\"listbox\" "
                  "aria-multiselectable=\"false\">">>, H)),
    ?assert(has(<<"<ul class=\"ah-listbox-list\" id=\"lb-list\" role=\"none\">">>, H)),
    ?assert(has(<<"<li class=\"ah-listbox-item ah-listbox-item-selected\" id=\"lb-o-1\" "
                  "role=\"option\" aria-selected=\"true\" data-idx=\"1\" data-value=\"b\">"
                  "<span class=\"ah-listbox-label\">Bee</span></li>">>, H)),
    ?assert(has(<<"ah-listbox-item-disabled\" id=\"lb-o-2\"">>, H)),
    ?assert(has(<<"<img class=\"ah-listbox-icon\" src=\"/i.png\" alt=\"\">">>, H)),
    ?assert(has(<<"<div class=\"ah-listbox-empty\" hidden>No data</div>">>, H)),
    ?assert(has(<<"<input type=\"hidden\" name=\"pick\" value=\"b\">">>, H)),
    ?assertNot(has_quiet(<<"ah-listbox-filter">>, H)),
    ?assertNot(has_quiet(<<"ah-listbox-checkbox">>, H)),
    E = r(?M:listbox([], undefined, [], [{empty_text, <<"Nothing">>}])),
    ?assert(has(<<"<div class=\"ah-listbox-empty\">Nothing</div>">>, E)),
    ?assertError({aihtml, {bad_list_item, _}}, r(?M:listbox([{1, 2, 3}], undefined, [], []))),
    ?assertError({aihtml, {bad_list_item, _}},
                 r(?M:listbox([#{value => 1, colour => red}], undefined, [], []))).

listbox_multi_groups_test() ->
    Items = [#{value => a, group => <<"G1">>}, #{value => b, group => <<"G2">>},
             #{value => c, group => <<"G1">>}],
    H = r(?M:listbox(Items, [a, c], [checkboxes, check_all, filterable, disabled],
                     [{id, m}, {check_all_label, <<"All">>}, {filter_placeholder, <<"Find">>}])),
    ?assert(has(<<"class=\"ah-listbox ah-listbox-checkboxes ah-listbox-disabled "
                  "ah-listbox-filterable\"">>, H)),
    ?assert(has(<<"data-ah-value=\"a,c\" tabindex=\"-1\" role=\"listbox\" "
                  "aria-multiselectable=\"true\" aria-disabled=\"true\"">>, H)),
    ?assert(has(<<"<input class=\"ah-listbox-filter-input\" type=\"text\" id=\"m-filter\" "
                  "autocomplete=\"off\" placeholder=\"Find\" aria-label=\"Find\" "
                  "aria-controls=\"m\" disabled data-listbox=\"m\" data-checkboxes=\"true\">">>, H)),
    ?assert(has(<<"<div class=\"ah-listbox-check-all\" role=\"button\" aria-pressed=\"false\">"
                  "<span class=\"ah-listbox-checkbox\"></span>"
                  "<span class=\"ah-listbox-label\">All</span></div>">>, H)),
    %% grouped in order of appearance; data-idx keeps the item position
    {match, [[G1], [G2]]} = re:run(H, <<"ah-listbox-group\" role=\"presentation\">([^<]+)<">>,
                                   [global, {capture, all_but_first, binary}]),
    ?assertEqual({<<"G1">>, <<"G2">>}, {G1, G2}),
    ?assert(has(<<"G1</li><li class=\"ah-listbox-item ah-listbox-item-selected\" id=\"m-o-0\"">>, H)),
    ?assert(has(<<"id=\"m-o-2\"">>, H)),
    ?assertEqual(2, count(<<"ah-listbox-checkbox ah-listbox-checkbox-checked">>, H)),
    M = r(?M:listbox([a, b], [b], [multiple], [])),
    ?assert(has(<<"class=\"ah-listbox ah-listbox-multiple\"">>, M)),
    ?assert(has(<<"data-ah-value=\"b\"">>, M)),
    %% check_all needs check boxes
    ?assertNot(has_quiet(<<"class=\"ah-listbox-check-all\"">>, r(?M:listbox([a], a, [check_all], [])))).

listbox_search_round_trip_test() ->
    Ref = {?MODULE, search, #{source => [<<"Apple">>, <<"Banana">>, <<"Grape">>]}},
    H = r(?M:listbox([], undefined, [checkboxes], [{id, <<"ls">>}, {search, Ref}])),
    ?assert(has(<<"ah-listbox-remote">>, H)),
    ?assert(has(<<"ah-listbox-filter-input">>, H)),     % search implies the filter
    {match, [Token]} = re:run(H, <<"data-ah-on=\"input:([^:\"]+):250\"">>,
                              [{capture, all_but_first, binary}]),
    {ok, Ref} = aihtml_action:verify(Token),
    Self = self(),
    Event = #{<<"type">> => <<"input">>, <<"id">> => <<"ls-filter">>, <<"value">> => <<"ap">>,
              <<"data">> => #{<<"listbox">> => <<"ls">>, <<"checkboxes">> => <<"true">>}},
    ok = aihtml_action:execute(Ref, Event, #{emit => fun(E) -> Self ! {ev, E} end}),
    [#{<<"value">> := [Html, Call]}] = [E || #{<<"type">> := <<"CUSTOM">>} = E <- collect()],
    #{op := html, swap := morph_inner, id := <<"ls-list">>, html := Rows} = Html,
    ?assertEqual(extract_list(r(?M:listbox([<<"Apple">>, <<"Grape">>], undefined, [checkboxes],
                                           [{id, <<"ls">>}]))), Rows),
    ?assertEqual(#{op => call, id => <<"ls">>, method => <<"itemsLoaded">>, args => []}, Call),
    Ops = aihtml_action:render_ops(
            fun(Ctx) -> ?M:listbox_items(Ctx, {id, x}, [a, b], #{selected => [b]}) end),
    [#{op := html, id := <<"x-list">>, html := H2}, #{op := call, id := <<"x">>}] = Ops,
    ?assert(has(<<"ah-listbox-item ah-listbox-item-selected\" id=\"x-o-1\"">>, H2)).

extract_list(H) ->
    {match, [Inner]} = re:run(H, <<"<ul class=\"ah-listbox-list\"[^>]*>(.*)</ul>">>,
                              [{capture, all_but_first, binary}]),
    Inner.

%%%===================================================================
%%% transfer
%%%===================================================================

transfer_test() ->
    Items = [{a, <<"A">>}, {b, <<"B">>}, #{value => c, label => <<"C">>, icon => <<"★"/utf8>>},
             #{value => d, label => <<"D">>, disabled => true}],
    H = r(?M:transfer(Items, [c, a, zz], [<<"max-w-xl">>], [{id, t}, {name, keys}])),
    ?assert(has(<<"<div class=\"ah-transfer max-w-xl\" id=\"t\" data-ah=\"transfer\" "
                  "data-ah-value=\"c,a\">">>, H)),
    ?assert(has(<<"<input type=\"hidden\" name=\"keys\" value=\"c,a\">">>, H)),
    %% the source keeps the item order, the target the value order
    {match, [Src]} = re:run(H, <<"<ul class=\"ah-transfer-list\" id=\"t-source\"[^>]*>(.*?)</ul>">>,
                            [{capture, all_but_first, binary}]),
    {match, [Tgt]} = re:run(H, <<"<ul class=\"ah-transfer-list\" id=\"t-target\"[^>]*>(.*?)</ul>">>,
                            [{capture, all_but_first, binary}]),
    ?assertEqual([<<"b">>, <<"d">>], values(Src)),
    ?assertEqual([<<"c">>, <<"a">>], values(Tgt)),
    ?assert(has(<<"<li class=\"ah-transfer-item\" id=\"t-i-2\" role=\"option\" "
                  "aria-selected=\"false\" data-value=\"c\" data-idx=\"2\" "
                  "data-source=\"target\"><span class=\"ah-transfer-item-icon\" "
                  "aria-hidden=\"true\">★</span>"/utf8>>, Tgt)),
    ?assert(has(<<"ah-transfer-item ah-transfer-item-disabled\" id=\"t-i-3\"">>, Src)),
    ?assert(has(<<"<span class=\"ah-transfer-panel-title\" id=\"t-source-title\">Source</span>"
                  "<span class=\"ah-transfer-panel-count\">2</span>">>, H)),
    ?assert(has(<<"Target</span><span class=\"ah-transfer-panel-count\">2</span>">>, H)),
    ?assertEqual(2, count(<<"ah-transfer-filter-input">>, H)),
    ?assert(has(<<"data-empty-text=\"No data\"">>, H)),
    ?assert(has(<<"<button class=\"ah-transfer-btn ah-transfer-btn-to-target "
                  "ah-transfer-btn-disabled\" type=\"button\" data-direction=\"to-target\"">>, H)),
    ?assert(has(<<"tabindex=\"0\" aria-multiselectable=\"true\" aria-labelledby=\"t-target-title\"">>, H)),
    D = r(?M:transfer([a], [], [disabled, no_filter],
                      [{source_title, <<"L">>}, {target_title, <<"R">>},
                       {empty_text, <<"-">>}, {filter_placeholder, <<"F">>}])),
    ?assert(has(<<"class=\"ah-transfer ah-transfer-disabled ah-transfer-no-filter\"">>, D)),
    ?assertNot(has_quiet(<<"ah-transfer-filter">>, D)),
    ?assert(has(<<"tabindex=\"-1\"">>, D)),
    ?assert(has(<<">L</span>">>, D)),
    ?assert(has(<<"data-empty-text=\"-\"">>, D)),
    ?assertError({aihtml, {bad_option, value, a}}, r(?M:transfer([a], a, [], []))).

values(Html) ->
    {match, Vs} = re:run(Html, <<"data-value=\"([^\"]*)\"">>,
                         [global, {capture, all_but_first, binary}]),
    [V || [V] <- Vs].

%%%===================================================================
%%% Catalog
%%%===================================================================

catalog_test() ->
    [#{name := cascader}, #{name := listbox}, #{name := transfer}] = ?M:catalog(),
    [begin
         #{flags := Fl, options := Op, option_docs := Docs, methods := Ms} =
             aihtml_catalog:entry(?M, N),
         ?assertEqual(lists:sort(Fl ++ Op), lists:sort(maps:keys(Docs))),
         ?assert(lists:member(setValue, [Name || #{name := Name} <- Ms]))
     end || N <- [cascader, listbox, transfer]],
    [?assert(erlang:function_exported(?M, F, A)) || {F, A} <- ?M:facade_extras()].

%%%===================================================================
%%% element records (designs/05-records.md)
%%%===================================================================

record_equals_builder_test() ->
    Ref = {?MODULE, load, #{}},
    ?assertEqual(r(?M:cascader(tree(), [<<"zj">>], [sm, filterable, <<"w-64">>],
                               [{id, c}, {name, n}, {separator, <<">">>}, {load, Ref},
                                {title, <<"t">>}])),
                 r(#ah_cascader{items = tree(), value = [<<"zj">>], size = sm, filterable = true,
                                css = [<<"w-64">>], id = c, name = n, separator = <<">">>,
                                load = Ref, attrs = [{title, <<"t">>}]})),
    ?assertEqual(r(?M:listbox([a, b], [a], [checkboxes, check_all],
                              [{id, l}, {check_all_label, <<"All">>}])),
                 r(#ah_listbox{items = [a, b], value = [a], checkboxes = true, check_all = true,
                               id = l, check_all_label = <<"All">>})),
    ?assertEqual(r(?M:transfer([a, b], [b], [no_filter], [{id, t}, {source_title, <<"S">>}])),
                 r(#ah_transfer{items = [a, b], value = [b], no_filter = true, id = t,
                                source_title = <<"S">>})).

builder_fills_fields_test() ->
    C = ?M:cascader([a], [a], [lg, no_arrow, <<"x">>], [{popup_height, 100}, {title, <<"t">>}]),
    ?assertMatch(#ah_cascader{items = [a], value = [a], size = lg, no_arrow = true,
                              popup_height = 100, css = [<<"x">>], attrs = [{title, <<"t">>}]}, C),
    L = ?M:listbox([a], a, [multiple], [{empty_text, <<"-">>}, {name, n}]),
    ?assertMatch(#ah_listbox{multiple = true, empty_text = <<"-">>, name = n, attrs = []}, L),
    ?assertError({aihtml, {record_only_field, ah_transfer, postback}},
                 ?M:transfer([], [], [], [{postback, moved}])).

generated_id_test() ->
    H = r(#ah_listbox{items = [a]}),
    {match, [Id]} = re:run(H, <<"^<div class=\"ah-listbox\" id=\"(ah-l[0-9]+)\"">>,
                           [{capture, all_but_first, binary}]),
    ?assert(has(<<"id=\"", Id/binary, "-o-0\"">>, H)),
    ?assertNotEqual(r(#ah_transfer{}), r(#ah_transfer{})).

postback_test() ->
    Token = fun(Html) ->
                    {match, [T]} = re:run(r(Html), <<"data-ah-on=\"(change:[^\"]+)\"">>,
                                          [{capture, all_but_first, binary}]),
                    [Ev, Tok] = binary:split(T, <<":">>),
                    {ok, Ref} = aihtml_action:unsign(Tok),
                    {Ev, Ref}
            end,
    ?assertEqual({<<"change">>, {?MODULE, picked, #{id => 7}}},
                 Token(#ah_cascader{items = tree(), postback = {picked, #{id => 7}}})),
    ?assertEqual({<<"change">>, {other_mod, pick, #{}}},
                 Token(#ah_listbox{items = [a], postback = pick, delegate = other_mod})),
    ?assertEqual({<<"change">>, {?MODULE, moved, #{}}},
                 Token(#ah_transfer{items = [a], postback = moved})),
    %% the id stays first on the root, the postback follows its own attributes
    ?assertMatch({match, _}, re:run(r(#ah_transfer{id = t, postback = moved}),
                                    <<"^<div class=\"ah-transfer\" id=\"t\" data-ah=\"transfer\""
                                      "[^>]* data-ah-on=\"change:">>)).

field_validation_test() ->
    ?assertError({aihtml, {bad_flag, cascader, filterable, yes}},
                 r(#ah_cascader{filterable = yes})),
    ?assertError({aihtml, {bad_modifier, cascader, size, huge, _}}, r(#ah_cascader{size = huge})),
    ?assertError({aihtml, {bad_option, popup_height, tall}}, r(#ah_cascader{popup_height = tall})),
    ?assertError({aihtml, {bad_list_item, _}}, r(#ah_listbox{items = [{1, 2, 3}]})),
    ?assertError({aihtml, {modifier_in_css, transfer, no_filter}},
                 r(#ah_transfer{css = [no_filter]})),
    ?assertError({aihtml, {unknown_modifier, listbox, big, _}},
                 ?M:listbox([], undefined, [big], [])).

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

default(ah_cascader) -> #ah_cascader{};
default(ah_listbox) -> #ah_listbox{};
default(ah_transfer) -> #ah_transfer{}.
