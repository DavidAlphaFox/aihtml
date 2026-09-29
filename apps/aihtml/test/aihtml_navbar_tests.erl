-module(aihtml_navbar_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_navbar.hrl").

-define(M, aihtml_navbar).

r(Html) -> aihtml_html:render_binary(Html).

has(Bin, Sub) -> binary:match(Bin, Sub) =/= nomatch.

count(Bin, Sub) -> length(binary:matches(Bin, Sub)).

%%%===================================================================
%%% catalog
%%%===================================================================

catalog_names_test() ->
    ?assertEqual([navbar], [N || #{name := N} <- ?M:catalog()]).

catalog_entries_are_valid_test() ->
    [begin
         E = aihtml_catalog:entry(?M, N),
         ?assertMatch(#{category := layout, root := <<"ah-", _/binary>>}, E),
         ?assert(is_list(aihtml_catalog:classes(E, [])))
     end || #{name := N} <- ?M:catalog()].

catalog_docs_test() ->
    [begin
         ?assertMatch(#{option_docs := #{}, methods := _}, E),
         Documented = maps:keys(maps:get(option_docs, E)),
         Opts = maps:get(options, E, []) ++ maps:get(flags, E, []) ++
             lists:append([Ms || {Ms, _} <- maps:values(maps:get(groups, E, #{}))]),
         ?assertEqual([], Opts -- Documented)
     end || E <- ?M:catalog()].

%%%===================================================================
%%% navbar
%%%===================================================================

navbar_test() ->
    H = r(?M:navbar([{home, <<"Home">>}, #{key => docs, label => <<"Docs">>, href => <<"/d">>},
                     #{key => off, label => <<"Off">>, disabled => true}],
                    home, [], [{brand, <<"Acme">>}, {extra, <<"X">>},
                               {columns, [<<"30%">>]}, {name, <<"nav">>}])),
    ?assert(has(H, <<"class=\"ah-navbar\"">>)),
    ?assert(has(H, <<"role=\"tablist\"">>)),
    ?assert(has(H, <<"data-ah-value=\"home\"">>)),
    ?assert(has(H, <<"class=\"ah-navbar-item ah-navbar-item-selected\"">>)),
    ?assert(has(H, <<"aria-selected=\"true\"">>)),
    ?assert(has(H, <<"flex:none;width:30%;">>)),
    ?assert(has(H, <<"<a class=\"ah-navbar-item\"">>)),
    ?assert(has(H, <<"href=\"/d\"">>)),
    ?assert(has(H, <<"ah-navbar-item-disabled">>)),
    ?assert(has(H, <<"<div class=\"ah-navbar-brand\">Acme</div>">>)),
    ?assert(has(H, <<"<div class=\"ah-navbar-extra\">X</div>">>)),
    ?assert(has(H, <<"ah-navbar-header">>)),
    ?assertEqual(3, count(H, <<"ah-navbar-toggle-bar">>)),
    ?assert(has(H, <<"name=\"nav\" value=\"home\"">>)).

navbar_minimized_vertical_test() ->
    M = r(?M:navbar([{a, <<"A">>}], undefined, [minimized], [{title, <<"T">>},
                                                              {minimized_height, 40}])),
    ?assert(has(M, <<"ah-navbar ah-navbar-minimized">>)),
    ?assert(has(M, <<"data-ah-minimized=\"static\"">>)),
    ?assert(has(M, <<"height:40px;">>)),
    ?assert(has(M, <<"<span class=\"ah-navbar-title\">T</span>">>)),
    V = r(?M:navbar([{a, <<"A">>}], undefined, [vertical], [{selection, false}])),
    ?assert(has(V, <<"ah-navbar ah-navbar-vertical">>)),
    ?assert(has(V, <<"aria-orientation=\"vertical\"">>)),
    ?assert(has(V, <<"data-ah-selection=\"false\"">>)).


-define(ITEMS, [#{key => file, label => <<"File">>, children => [{new, <<"New">>}]},
                {help, <<"Help">>}]).

%%%===================================================================
%%% element record (designs/05-records.md)
%%%===================================================================

record_equals_builder_test() ->
    ?assertEqual(r(?M:navbar(?ITEMS, help, [minimized],
                             [{brand, <<"B">>}, {columns, [<<"30%">>]}, {minimized_height, 40}])),
                 r(#ah_navbar{items = ?ITEMS, value = help, minimized = true, brand = <<"B">>,
                              columns = [<<"30%">>], minimized_height = 40})).

token(Html) ->
    {match, [T]} = re:run(r(Html), <<"data-ah-on=\"([a-z]+:[^\"]+)\"">>,
                          [{capture, all_but_first, binary}]),
    [Ev, Tok] = binary:split(T, <<":">>),
    {ok, Ref} = aihtml_action:unsign(Tok),
    {Ev, Ref}.

postback_test() ->
    ?assertEqual({<<"change">>, {?MODULE, go, 1}},
                 token(#ah_navbar{items = ?ITEMS, postback = {go, 1}})).

field_validation_test() ->
    ?assertError({aihtml, {modifier_in_css, navbar, vertical}},
                 r(#ah_navbar{css = [vertical]})).

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

default(ah_navbar) -> #ah_navbar{}.
