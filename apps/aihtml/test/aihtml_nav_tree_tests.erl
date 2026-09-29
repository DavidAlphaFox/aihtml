%% Tests for aihtml_nav_tree.
-module(aihtml_nav_tree_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_nav_tree.hrl").

-define(M, aihtml_nav_tree).

r(Html) -> aihtml_html:render_binary(Html).

has(Needle, Hay) ->
    case binary:match(Hay, Needle) of
        nomatch -> ?debugFmt("~s not in~n~s", [Needle, Hay]), false;
        _ -> true
    end.

count(Needle, Hay) -> length(binary:matches(Hay, Needle)).

has_quiet(Needle, Hay) -> binary:match(Hay, Needle) =/= nomatch.

%%%===================================================================
%%% nav_tree
%%%===================================================================

nav() ->
    [#{group => <<"OVERVIEW">>,
       items => [#{label => <<"Dashboard">>, icon => {safe, <<"<svg></svg>">>}, route => <<"dashboard">>},
                 {<<"Analytics">>, <<"analytics">>}]},
     #{group => <<"MANAGEMENT">>,
       items => [#{label => <<"User">>,
                   items => [{<<"Profile">>, <<"user/profile">>},
                             #{label => <<"Deep">>, items => [{<<"Cards">>, <<"user/cards">>}]}]},
                 #{label => <<"Docs">>, href => <<"https://example.com">>}]},
     {<<"Loose">>, <<"loose">>}].

nav_tree_test() ->
    H = r(?M:nav_tree(nav(), <<"user/cards">>, [<<"w-64">>], [{id, nt}])),
    ?assert(has(<<"<nav class=\"ah-nav-tree w-64\" data-ah=\"nav-tree\" data-ah-value=\"user/cards\" id=\"nt\">">>, H)),
    ?assert(has(<<"<div class=\"ah-nav-tree__group\"><div class=\"ah-nav-tree__group-label\">OVERVIEW</div>">>, H)),
    ?assert(has(<<"<a class=\"ah-nav-tree__item\" href=\"#/dashboard\" data-route=\"dashboard\">"
                  "<span class=\"ah-nav-tree__icon\" aria-hidden=\"true\"><svg></svg></span>"
                  "<span class=\"ah-nav-tree__label\">Dashboard</span></a>">>, H)),
    %% the nodes around the active link are open
    ?assertEqual(2, count(<<"<details class=\"ah-nav-tree__node\" open>">>, H)),
    ?assertEqual(2, count(<<"ah-nav-tree__item--parent ah-is-open">>, H)),
    ?assert(has(<<"<a class=\"ah-nav-tree__item ah-is-active\" href=\"#/user/cards\" "
                  "data-route=\"user/cards\" aria-current=\"page\">">>, H)),
    ?assertEqual(1, count(<<"ah-is-active">>, H)),
    ?assert(has(<<"<div class=\"ah-nav-tree__children\"><div class=\"ah-nav-tree__children-inner\">">>, H)),
    ?assert(has(<<"<a class=\"ah-nav-tree__item\" href=\"https://example.com\">">>, H)),
    %% a loose item gets a group without a heading
    ?assert(has(<<"<div class=\"ah-nav-tree__group\"><a class=\"ah-nav-tree__item\" href=\"#/loose\"">>, H)),
    %% another route: nothing active, nodes closed; route prefix
    H2 = r(?M:nav_tree(nav(), undefined, [], [{route_prefix, <<"/app/">>}])),
    ?assertNot(has_quiet(<<" open>">>, H2)),
    ?assertNot(has_quiet(<<"ah-is-">>, H2)),
    ?assert(has(<<"data-ah-value=\"\"">>, H2)),
    ?assert(has(<<"href=\"/app/analytics\"">>, H2)),
    ?assertError({aihtml, {bad_nav_tree_item, 42}}, r(?M:nav_tree([42], undefined, [], []))).

%%%===================================================================
%%% Catalog
%%%===================================================================

catalog_test() ->
    [#{name := nav_tree, category := layout}] = ?M:catalog(),
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
    ?assertEqual(r(?M:nav_tree(nav(), <<"loose">>, [], [{route_prefix, <<"/">>}, {id, n}])),
                 r(#ah_nav_tree{items = nav(), value = <<"loose">>, route_prefix = <<"/">>, id = n})).

postback_test() ->
    Token = fun(Html) ->
                    {match, [Ev, Tok]} = re:run(r(Html), <<"data-ah-on=\"([a-z:]+):([^\"]+)\"">>,
                                                [{capture, all_but_first, binary}]),
                    {ok, Ref} = aihtml_action:unsign(Tok),
                    {Ev, Ref}
            end,
    ?assertEqual({<<"change">>, {other, go, #{}}},
                 Token(#ah_nav_tree{postback = go, delegate = other})).

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

default(ah_nav_tree) -> #ah_nav_tree{}.
