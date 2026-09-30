-module(aihtml_listmenu_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_listmenu.hrl").

-define(M, aihtml_listmenu).

r(Html) -> aihtml_html:render_binary(Html).

has(Bin, Sub) -> binary:match(Bin, Sub) =/= nomatch.

count(Bin, Sub) -> length(binary:matches(Bin, Sub)).

%%%===================================================================
%%% catalog
%%%===================================================================

catalog_names_test() ->
    ?assertEqual([listmenu], [N || #{name := N} <- ?M:catalog()]).

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
%%% listmenu
%%%===================================================================

listmenu_test() ->
    Items = [#{key => fruit, label => <<"Fruit">>,
               children => [{apple, <<"Apple">>},
                            #{key => citrus, label => <<"Citrus">>,
                              children => [{lemon, <<"Lemon">>}]}]},
             {bread, <<"Bread">>},
             #{key => cake, label => <<"Cake">>, disabled => true}],
    Root = r(?M:ah_listmenu(Items, bread, [], [{filter, true}, {name, <<"food">>}])),
    ?assert(has(Root, <<"class=\"ah-listmenu\" data-ah=\"listmenu\" tabindex=\"0\"">>)),
    ?assert(has(Root, <<"data-ah-stack=\"\"">>)),
    ?assertEqual(3, count(Root, <<"class=\"ah-listmenu-page\"">>)),
    ?assertEqual(2, count(Root, <<"style=\"display:none\"">>) - 1),  % + back button
    ?assert(has(Root, <<"ah-listmenu-item ah-listmenu-item-selected\" data-item-id=\"1\"">>)),
    ?assert(has(Root, <<"ah-listmenu-item-disabled">>)),
    ?assert(has(Root, <<"ah-listmenu-filter-input">>)),
    ?assert(has(Root, <<"name=\"food\" value=\"bread\"">>)),
    ?assertEqual(2, count(Root, <<"ah-listmenu-arrow">>)),
    %% a nested value opens its page and fills the stack and title
    Nested = r(?M:ah_listmenu(Items, lemon, [], [{header, true}, {animation, fade}])),
    ?assert(has(Nested, <<"data-ah-stack=\"0,4\"">>)),
    ?assert(has(Nested, <<"<span class=\"ah-listmenu-title\">Citrus</span>">>)),
    ?assert(has(Nested, <<"data-page-id=\"4\" role=\"menu\">">>)),
    ?assert(has(Nested, <<"data-ah-animation=\"fade\"">>)),
    NoHeader = r(?M:ah_listmenu(Items, undefined, [disabled], [{header, false}, {arrows, false}])),
    ?assertNot(has(NoHeader, <<"ah-listmenu-header">>)),
    ?assertNot(has(NoHeader, <<"ah-listmenu-arrow">>)),
    ?assert(has(NoHeader, <<"ah-listmenu ah-listmenu-disabled">>)).


-define(ITEMS, [#{key => file, label => <<"File">>, children => [{new, <<"New">>}]},
                {help, <<"Help">>}]).

%%%===================================================================
%%% element record (designs/05-records.md)
%%%===================================================================

record_equals_builder_test() ->
    ?assertEqual(r(?M:ah_listmenu(?ITEMS, new, [], [{filter, true}, {back_label, <<"Up">>}])),
                 r(#ah_listmenu{items = ?ITEMS, value = new, filter = true,
                                back_label = <<"Up">>})).

builder_fills_fields_test() ->
    ?assertMatch(#ah_listmenu{header = true, back_button = true, filter = false,
                              arrows = true, back_label = <<"Back">>, name = food},
                 ?M:ah_listmenu([], undefined, [], [{name, food}])).

token(Html) ->
    {match, [T]} = re:run(r(Html), <<"data-ah-on=\"([a-z]+:[^\"]+)\"">>,
                          [{capture, all_but_first, binary}]),
    [Ev, Tok] = binary:split(T, <<":">>),
    {ok, Ref} = aihtml_action:unsign(Tok),
    {Ev, Ref}.

postback_test() ->
    ?assertMatch({<<"change">>, _}, token(#ah_listmenu{items = ?ITEMS, postback = go})).

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

default(ah_listmenu) -> #ah_listmenu{}.
