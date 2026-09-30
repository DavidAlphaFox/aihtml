-module(aihtml_sortable_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_sortable.hrl").

-define(M, aihtml_sortable).
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
    H = r(?M:ah_sortable(?ITEMS, undefined, [], [])),
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
    ?assertEqual([<<"c">>, <<"a">>, <<"b">>], keys(r(?M:ah_sortable(?ITEMS, [c, a], [], [])))),
    ?assertEqual([<<"b">>, <<"c">>, <<"a">>], keys(r(?M:ah_sortable(?ITEMS, <<"b,c,a">>, [], [])))),
    ?assertEqual([<<"b">>, <<"a">>, <<"c">>], keys(r(?M:ah_sortable(?ITEMS, "b,zz,b", [], [])))),
    H = r(?M:ah_sortable(?ITEMS, [c], [], [{name, order}])),
    ?has(<<"data-ah-value=\"c,a,b\"">>, H),
    ?has(<<"<input type=\"hidden\" name=\"order\" value=\"c,a,b\" data-ah-input>">>, H),
    %% the first item in the order is the one in the tab order
    ?has(<<"data-value=\"c\" tabindex=\"0\"">>, H).

sortable_modifiers_test() ->
    H = r(?M:ah_sortable(?ITEMS, undefined, [grid, handle, <<"gap-4">>],
                         [{group, board}, {id, l1}])),
    ?has(<<"class=\"ah-sortable ah-sortable-grid gap-4\"">>, H),
    ?has(<<"data-ah-group=\"board\" id=\"l1\"">>, H),
    ?has(<<"class=\"ah-sortable-item ah-sortable-handle-mode\"">>, H),
    ?has(<<"<span class=\"ah-sortable-handle\" aria-hidden=\"true\">"/utf8>>, H),
    ?has(<<"ah-sortable-horizontal">>, r(?M:ah_sortable([], undefined, [horizontal], []))).

sortable_disabled_test() ->
    H = r(?M:ah_sortable(?ITEMS, undefined, [], [{disabled, true}])),
    ?has(<<"class=\"ah-sortable ah-sortable-vertical ah-sortable-disabled\"">>, H),
    ?has(<<"aria-disabled=\"true\"">>, H),
    ?hasnt(<<"tabindex=\"0\"">>, H).

sortable_comma_key_test() ->
    Items = [{<<"a,b">>, <<"x">>}, {<<"c\\d">>, <<"y">>}, {e, <<"z">>}],
    H = r(?M:ah_sortable(Items, undefined, [], [{name, o}])),
    ?has(<<"data-ah-value=\"a\\,b,c\\\\d,e\"">>, H),
    ?has(<<"name=\"o\" value=\"a\\,b,c\\\\d,e\"">>, H),
    ?has(<<"data-value=\"a,b\"">>, H),
    %% a value in the same text puts them in order
    H2 = r(?M:ah_sortable(Items, <<"e,c\\\\d,a\\,b">>, [], [])),
    ?has(<<"data-ah-value=\"e,c\\\\d,a\\,b\"">>, H2),
    ?assertEqual(H2, r(?M:ah_sortable(Items, [e, <<"c\\d">>, <<"a,b">>], [], []))).

sortable_bad_items_test() ->
    ?assertError(function_clause, r(?M:ah_sortable([<<"x">>], undefined, [], []))).

%%% catalog

catalog_test() ->
    Cat = ?M:catalog(),
    ?assertEqual([sortable], [N || #{name := N} <- Cat]),
    [begin
         ?assert(erlang:function_exported(?M, aihtml_catalog:builder(N), length(binary:split(S, <<",">>, [global])))),
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
    ?assertEqual(r(?M:ah_sortable(?ITEMS, [b], [horizontal, handle, <<"p-1">>],
                                  [{group, g}, {name, n}, {id, s}, {title, <<"t">>}])),
                 r(#ah_sortable{items = ?ITEMS, value = [b], orientation = horizontal,
                                handle = true, css = [<<"p-1">>], group = g, name = n, id = s,
                                attrs = [{title, <<"t">>}]})).

builder_fills_fields_test() ->
    S = ?M:ah_sortable(?ITEMS, undefined, [grid], [{disabled, true}, {title, <<"t">>}]),
    ?assertMatch(#ah_sortable{orientation = grid, disabled = true, handle = false,
                              attrs = [{title, <<"t">>}]}, S).

postback_test() ->
    Token = fun(Html) ->
                    {match, [Ev, Tok]} = re:run(r(Html), <<"data-ah-on=\"([a-z:-]+):([^\":]+\\.[^\":]+)\"">>,
                                                [{capture, all_but_first, binary}]),
                    {ok, Ref} = aihtml_action:unsign(Tok),
                    {Ev, Ref}
            end,
    ?assertEqual({<<"change">>, {?MODULE, reorder, #{list => 1}}},
                 Token(#ah_sortable{items = ?ITEMS, postback = {reorder, #{list => 1}}})).

field_validation_test() ->
    ?assertError({aihtml, {bad_option, disabled, 1}}, r(#ah_sortable{disabled = 1})),
    ?assertError({aihtml, {bad_modifier, sortable, orientation, diagonal, _}},
                 r(#ah_sortable{orientation = diagonal})),
    ?assertError({aihtml, {bad_flag, sortable, handle, yes}}, r(#ah_sortable{handle = yes})),
    ?assertError({aihtml, {bad_option, group, {x}}}, r(#ah_sortable{group = {x}})),
    ?assertError({aihtml, {unknown_modifier, sortable, big, _}},
                 ?M:ah_sortable([], undefined, [big], [])).

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

default(ah_sortable) -> #ah_sortable{}.
