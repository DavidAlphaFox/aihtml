-module(aihtml_splitter_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_splitter.hrl").

-define(M, aihtml_splitter).

r(Html) -> aihtml_html:render_binary(Html).

has(Bin, Sub) -> binary:match(Bin, Sub) =/= nomatch.

%%%===================================================================
%%% catalog
%%%===================================================================

catalog_names_test() ->
    ?assertEqual([splitter], [N || #{name := N} <- ?M:catalog()]).

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
%%% splitter
%%%===================================================================

splitter_test() ->
    H = r(?M:splitter([#{content => <<"L">>, size => <<"30%">>, min => 80},
                       #{content => <<"R">>, min => 60}],
                      [], [{name, <<"s">>}])),
    ?assert(has(H, <<"class=\"ah-splitter ah-splitter-vertical\"">>)),
    ?assert(has(H, <<"data-ah-value=\"30,70\"">>)),
    ?assert(has(H, <<"data-ah-min=\"80,60\"">>)),
    ?assert(has(H, <<"flex:0 0 calc((100% - 5px) * 0.3);min-width:80px;">>)),
    ?assert(has(H, <<"flex:1 1 0;min-width:60px;">>)),
    ?assert(has(H, <<"role=\"separator\" tabindex=\"0\" aria-orientation=\"vertical\"">>)),
    ?assert(has(H, <<"aria-valuenow=\"30\"">>)),
    ?assert(has(H, <<"style=\"width:5px\"">>)),
    ?assert(has(H, <<"ah-splitter-collapse-btn">>)),
    ?assert(has(H, <<"name=\"s\" value=\"30,70\"">>)).

splitter_horizontal_pixels_test() ->
    H = r(?M:splitter([#{content => <<"T">>, size => 120}, <<"B">>], [horizontal, disabled],
                      [{splitbar_size, 8}, {resizable, false}])),
    ?assert(has(H, <<"ah-splitter ah-splitter-horizontal ah-splitter-disabled">>)),
    ?assert(has(H, <<"flex:0 0 120px;min-height:0px;">>)),
    ?assert(has(H, <<"style=\"height:8px\"">>)),
    ?assert(has(H, <<"data-ah-resizable=\"false\"">>)),
    ?assertNot(has(H, <<"data-ah-value">>)),
    ?assertError({aihtml, {splitter_needs_two_panes, 3}}, r(?M:splitter([a, b, c], [], []))).


%%%===================================================================
%%% element record (designs/05-records.md)
%%%===================================================================

record_equals_builder_test() ->
    ?assertEqual(r(?M:splitter([<<"L">>, <<"R">>], [horizontal], [{splitbar_size, 8}])),
                 r(#ah_splitter{panes = [<<"L">>, <<"R">>], orientation = horizontal,
                                splitbar_size = 8})).

token(Html) ->
    {match, [T]} = re:run(r(Html), <<"data-ah-on=\"([a-z]+:[^\"]+)\"">>,
                          [{capture, all_but_first, binary}]),
    [Ev, Tok] = binary:split(T, <<":">>),
    {ok, Ref} = aihtml_action:unsign(Tok),
    {Ev, Ref}.

postback_test() ->
    ?assertMatch({<<"change">>, _}, token(#ah_splitter{panes = [<<"a">>], postback = go})).

field_validation_test() ->
    ?assertError({aihtml, {bad_modifier, splitter, orientation, diagonal, _}},
                 r(#ah_splitter{panes = [a], orientation = diagonal})),
    ?assertError({aihtml, {splitter_needs_two_panes, 0}}, r(#ah_splitter{})).

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

default(ah_splitter) -> #ah_splitter{}.
