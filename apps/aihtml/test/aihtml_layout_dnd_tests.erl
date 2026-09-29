-module(aihtml_layout_dnd_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_layout_dnd.hrl").

-define(M, aihtml_layout_dnd).
-define(ITEMS, [{a, <<"Alpha">>}, {b, <<"Beta">>}, {c, <<"Gamma">>, [{class, <<"x">>}]}]).

r(H) -> aihtml_html:render_binary(H).

has(Sub, Bin) -> binary:match(Bin, Sub) =/= nomatch.

-define(has(Sub, Bin), ?assert(has(Sub, Bin))).
-define(hasnt(Sub, Bin), ?assertNot(has(Sub, Bin))).

keys(H) ->
    {match, Ks} = re:run(H, <<"class=\"ah-sortable-item[^\"]*\"[^>]* data-value=\"([^\"]*)\"">>,
                         [global, {capture, all_but_first, binary}]),
    [K || [K] <- Ks].

%%% sortable

sortable_default_test() ->
    H = r(?M:sortable(?ITEMS, undefined, [], [])),
    ?has(<<"<div class=\"ah-sortable ah-sortable-vertical\" role=\"list\" data-ah=\"sortable\" "
           "data-ah-value=\"a,b,c\">">>, H),
    ?assertEqual([<<"a">>, <<"b">>, <<"c">>], keys(H)),
    ?has(<<"<div class=\"ah-sortable-item\" role=\"listitem\" data-value=\"a\" tabindex=\"0\" "
           "aria-roledescription=\"sortable item\">Alpha</div>">>, H),
    ?has(<<"data-value=\"b\" tabindex=\"-1\"">>, H),
    ?has(<<"class=\"ah-sortable-item x\"">>, H),
    ?has(<<"<span class=\"ah-sortable-live\" aria-live=\"assertive\" aria-atomic=\"true\">">>, H),
    ?hasnt(<<"type=\"hidden\"">>, H),
    ?hasnt(<<"ah-sortable-handle">>, H).

sortable_value_orders_items_test() ->
    ?assertEqual([<<"c">>, <<"a">>, <<"b">>], keys(r(?M:sortable(?ITEMS, [c, a], [], [])))),
    ?assertEqual([<<"b">>, <<"c">>, <<"a">>], keys(r(?M:sortable(?ITEMS, <<"b,c,a">>, [], [])))),
    ?assertEqual([<<"b">>, <<"a">>, <<"c">>], keys(r(?M:sortable(?ITEMS, "b,zz,b", [], [])))),
    H = r(?M:sortable(?ITEMS, [c], [], [{name, order}])),
    ?has(<<"data-ah-value=\"c,a,b\"">>, H),
    ?has(<<"<input type=\"hidden\" name=\"order\" value=\"c,a,b\" data-ah-input>">>, H),
    %% the first item in the order is the one in the tab order
    ?has(<<"data-value=\"c\" tabindex=\"0\"">>, H).

sortable_modifiers_test() ->
    H = r(?M:sortable(?ITEMS, undefined, [grid, handle, <<"gap-4">>],
                      [{group, board}, {id, l1}])),
    ?has(<<"class=\"ah-sortable ah-sortable-grid gap-4\"">>, H),
    ?has(<<"data-ah-group=\"board\" id=\"l1\"">>, H),
    ?has(<<"class=\"ah-sortable-item ah-sortable-handle-mode\"">>, H),
    ?has(<<"<span class=\"ah-sortable-handle\" aria-hidden=\"true\">"/utf8>>, H),
    ?has(<<"ah-sortable-horizontal">>, r(?M:sortable([], undefined, [horizontal], []))).

sortable_disabled_test() ->
    H = r(?M:sortable(?ITEMS, undefined, [], [{disabled, true}])),
    ?has(<<"class=\"ah-sortable ah-sortable-vertical ah-sortable-disabled\"">>, H),
    ?has(<<"aria-disabled=\"true\"">>, H),
    ?hasnt(<<"tabindex=\"0\"">>, H).

sortable_bad_items_test() ->
    ?assertError({aihtml, {bad_item_key, <<"a,b">>}},
                 r(?M:sortable([{<<"a,b">>, <<"x">>}], undefined, [], []))),
    ?assertError(function_clause, r(?M:sortable([<<"x">>], undefined, [], []))).

%%% dragdrop

dragdrop_test() ->
    H = r(?M:dragdrop([aihtml_html:el('div', <<"Task">>, [<<"p-2">>],
                                      ?M:draggable_attrs(t1, #{type => task})),
                       aihtml_html:el('div', <<"Done">>, [],
                                      ?M:drop_zone_attrs(done, #{accept => [task, bug]}))],
                      [<<"flex">>], [{move, true}, {id, dd}])),
    ?has(<<"<div class=\"ah-dragdrop flex\" data-ah=\"dragdrop\" data-ah-tolerance=\"intersect\" "
           "data-ah-move id=\"dd\">">>, H),
    ?has(<<"<div class=\"p-2 ah-draggable\" data-ah-drag=\"t1\" data-ah-drag-type=\"task\" "
           "tabindex=\"0\" aria-roledescription=\"draggable\">Task</div>">>, H),
    ?has(<<"<div class=\"ah-drop-zone\" data-ah-drop=\"done\" data-ah-drop-accept=\"task,bug\">">>, H),
    ?hasnt(<<"data-ah-revert">>, H).

dragdrop_options_test() ->
    H = r(?M:dragdrop([], [], [{tolerance, pointer}, {revert, true}, {disabled, true}])),
    ?has(<<"class=\"ah-dragdrop ah-dragdrop-disabled\"">>, H),
    ?has(<<"data-ah-tolerance=\"pointer\"">>, H),
    ?has(<<"data-ah-revert">>, H),
    ?has(<<"aria-disabled=\"true\"">>, H).

attr_helpers_test() ->
    D = r(aihtml_html:el(span, [], [], ?M:draggable_attrs(<<"k">>, #{disabled => true}))),
    ?has(<<"class=\"ah-draggable ah-draggable-disabled\"">>, D),
    ?has(<<"aria-disabled=\"true\"">>, D),
    ?hasnt(<<"data-ah-drag-type">>, D),
    Z = r(aihtml_html:el(span, [], [], ?M:drop_zone_attrs(3, #{accept => task, disabled => true}))),
    ?has(<<"data-ah-drop=\"3\" data-ah-drop-accept=\"task\" data-ah-drop-disabled=\"true\"">>, Z),
    ?assertEqual([{draggable_attrs, 2}, {drop_zone_attrs, 2}], ?M:facade_extras()).

%%% catalog

catalog_test() ->
    Cat = ?M:catalog(),
    ?assertEqual([sortable, dragdrop], [N || #{name := N} <- Cat]),
    [begin
         ?assert(erlang:function_exported(?M, N, length(binary:split(S, <<",">>, [global])))),
         ?assertEqual(layout, C)
     end || #{name := N, signature := S, category := C} <- Cat].

catalog_docs_test() ->
    [begin
         Docs = maps:get(option_docs, E, #{}),
         ?assertEqual(lists:sort(maps:get(options, E, []) ++ maps:get(flags, E, [])),
                      lists:sort(maps:keys(Docs))),
         [?assert(is_binary(D) andalso D =/= <<>>) || D <- maps:values(Docs)],
         Ms = maps:get(methods, E),
         [#{name := _, args := <<"(", _/binary>>, doc := _} = X || X <- Ms],
         ?assertEqual(maps:get(behavior, E, none) =/= none, Ms =/= [])
     end || E <- ?M:catalog()].

%%% element records (designs/05-records.md)

record_equals_builder_test() ->
    ?assertEqual(r(?M:sortable(?ITEMS, [b], [horizontal, handle, <<"p-1">>],
                               [{group, g}, {name, n}, {id, s}, {title, <<"t">>}])),
                 r(#ah_sortable{items = ?ITEMS, value = [b], orientation = horizontal,
                                handle = true, css = [<<"p-1">>], group = g, name = n, id = s,
                                attrs = [{title, <<"t">>}]})),
    ?assertEqual(r(?M:dragdrop(<<"x">>, [], [{tolerance, fit}, {move, true}, {revert, true}])),
                 r(#ah_dragdrop{body = <<"x">>, tolerance = fit, move = true, revert = true})).

builder_fills_fields_test() ->
    S = ?M:sortable(?ITEMS, undefined, [grid], [{disabled, true}, {title, <<"t">>}]),
    ?assertMatch(#ah_sortable{orientation = grid, disabled = true, handle = false,
                              attrs = [{title, <<"t">>}]}, S),
    ?assertError({aihtml, {record_only_field, ah_dragdrop, postback}},
                 ?M:dragdrop([], [], [{postback, drop}])).

postback_test() ->
    Token = fun(Html) ->
                    {match, [Ev, Tok]} = re:run(r(Html), <<"data-ah-on=\"([a-z:-]+):([^\":]+\\.[^\":]+)\"">>,
                                                [{capture, all_but_first, binary}]),
                    {ok, Ref} = aihtml_action:unsign(Tok),
                    {Ev, Ref}
            end,
    ?assertEqual({<<"change">>, {?MODULE, reorder, #{list => 1}}},
                 Token(#ah_sortable{items = ?ITEMS, postback = {reorder, #{list => 1}}})),
    ?assertEqual({<<"ah:drop">>, {other, dropped, #{}}},
                 Token(#ah_dragdrop{postback = dropped, delegate = other})).

field_validation_test() ->
    ?assertError({aihtml, {bad_option, tolerance, near}}, r(#ah_dragdrop{tolerance = near})),
    ?assertError({aihtml, {bad_option, move, yes}}, r(#ah_dragdrop{move = yes})),
    ?assertError({aihtml, {bad_option, disabled, 1}}, r(#ah_sortable{disabled = 1})),
    ?assertError({aihtml, {bad_modifier, sortable, orientation, diagonal, _}},
                 r(#ah_sortable{orientation = diagonal})),
    ?assertError({aihtml, {bad_flag, sortable, handle, yes}}, r(#ah_sortable{handle = yes})),
    ?assertError({aihtml, {bad_option, group, {x}}}, r(#ah_sortable{group = {x}})),
    ?assertError({aihtml, {unknown_modifier, sortable, big, _}},
                 ?M:sortable([], undefined, [big], [])).

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

default(ah_sortable) -> #ah_sortable{};
default(ah_dragdrop) -> #ah_dragdrop{}.
