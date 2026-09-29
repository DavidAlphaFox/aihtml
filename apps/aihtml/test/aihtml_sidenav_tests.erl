-module(aihtml_sidenav_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_sidenav.hrl").

-define(M, aihtml_sidenav).

r(Html) -> aihtml_html:render_binary(Html).

has(Bin, Sub) -> binary:match(Bin, Sub) =/= nomatch.

count(Bin, Sub) -> length(binary:matches(Bin, Sub)).

%%%===================================================================
%%% catalog
%%%===================================================================

catalog_names_test() ->
    ?assertEqual([sidenav], [N || #{name := N} <- ?M:catalog()]).

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
%%% sidenav
%%%===================================================================

sidenav_test() ->
    Groups = [#{label => <<"Main">>,
                items => [{home, <<"Home">>},
                          #{key => users, label => <<"Users">>,
                            children => [{list, <<"List">>}, {roles, <<"Roles">>}]},
                          #{key => ext, label => <<"Ext">>, href => <<"https://x">>}]},
              #{items => [#{key => s, label => <<"S">>, children => [{t, <<"T">>}]}]}],
    H = r(?M:sidenav(Groups, roles, [],
                     [{brand, #{name => <<"Sigil">>, href => <<"/">>}},
                      {footer, <<"F">>}, {collapsible, true}])),
    ?assert(has(H, <<"<aside class=\"ah-sidenav\" data-ah=\"sidenav\" data-ah-value=\"roles\">">>)),
    ?assert(has(H, <<"<a class=\"ah-sidenav__brand\" href=\"/\">">>)),
    ?assert(has(H, <<"<div class=\"ah-nav-tree__group-label\">Main</div>">>)),
    ?assert(has(H, <<"ah-nav-tree__item ah-is-active\" href=\"#\" data-route=\"roles\"">>)),
    ?assert(has(H, <<"aria-current=\"page\"">>)),
    %% the active item's ancestors are open, the other node is not
    ?assert(has(H, <<"<details class=\"ah-nav-tree__node\" open>">>)),
    ?assertEqual(1, count(H, <<" open>">>)),
    ?assert(has(H, <<"ah-nav-tree__item--parent ah-is-open">>)),
    ?assert(has(H, <<"href=\"https://x\"">>)),
    ?assert(has(H, <<"<div class=\"ah-sidenav__footer\">F</div>">>)),
    ?assert(has(H, <<"ah-sidenav__toggle">>)),
    ?assert(has(H, <<"aria-expanded=\"true\"">>)).

sidenav_plain_items_prefix_collapsed_test() ->
    H = r(?M:sidenav([{a, <<"A">>}], undefined, [collapsed],
                     [{route_prefix, <<"#/">>}, {collapsible, true}])),
    ?assert(has(H, <<"class=\"ah-sidenav ah-sidenav-collapsed\"">>)),
    ?assert(has(H, <<"href=\"#/a\"">>)),
    ?assert(has(H, <<"aria-expanded=\"false\"">>)),
    ?assertNot(has(H, <<"ah-nav-tree__group-label">>)).


-define(ITEMS, [#{key => file, label => <<"File">>, children => [{new, <<"New">>}]},
                {help, <<"Help">>}]).

%%%===================================================================
%%% element record (designs/05-records.md)
%%%===================================================================

record_equals_builder_test() ->
    ?assertEqual(r(?M:sidenav(?ITEMS, new, [collapsed],
                              [{route_prefix, <<"#/">>}, {collapsible, true},
                               {style, <<"--w:1px">>}])),
                 r(#ah_sidenav{groups = ?ITEMS, value = new, collapsed = true,
                               route_prefix = <<"#/">>, collapsible = true,
                               attrs = [{style, <<"--w:1px">>}]})).

token(Html) ->
    {match, [T]} = re:run(r(Html), <<"data-ah-on=\"([a-z]+:[^\"]+)\"">>,
                          [{capture, all_but_first, binary}]),
    [Ev, Tok] = binary:split(T, <<":">>),
    {ok, Ref} = aihtml_action:unsign(Tok),
    {Ev, Ref}.

postback_test() ->
    ?assertEqual({<<"change">>, {other_mod, go, #{}}},
                 token(#ah_sidenav{groups = ?ITEMS, postback = go, delegate = other_mod})).

field_validation_test() ->
    ?assertError({aihtml, {bad_flag, sidenav, collapsed, yes}},
                 r(#ah_sidenav{collapsed = yes})).

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

default(ah_sidenav) -> #ah_sidenav{}.
