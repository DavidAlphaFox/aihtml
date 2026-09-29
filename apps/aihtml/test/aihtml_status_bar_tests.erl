-module(aihtml_status_bar_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_status_bar.hrl").

-define(M, aihtml_status_bar).

r(Html) -> aihtml_html:render_binary(Html).

has(Bin, Sub) -> binary:match(Bin, Sub) =/= nomatch.

%%%===================================================================
%%% catalog
%%%===================================================================

catalog_names_test() ->
    ?assertEqual([status_bar], [N || #{name := N} <- ?M:catalog()]).

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
%%% status_bar
%%%===================================================================

status_bar_test() ->
    H = r(?M:status_bar([<<"Ln 1">>, #{content => <<"UTF-8">>, align => right}],
                        [], [{content, <<"Hello world 世界\n\nbye"/utf8>>}, {dirty, true},
                             {labels, #{unsaved => <<"Modified">>}}])),
    ?assert(has(H, <<"class=\"ah-status-bar\"">>)),
    ?assert(has(H, <<"data-ah=\"status-bar\"">>)),
    ?assert(has(H, <<"data-dirty=\"true\"">>)),
    %% 3 English words + 2 CJK characters
    ?assert(has(H, <<"<span class=\"ah-status-bar__count-num\">5</span>">>)),
    ?assert(has(H, <<"<span class=\"ah-status-bar__row-label\">Paragraphs</span>"
                     "<span class=\"ah-status-bar__row-val\">2</span>">>)),
    ?assert(has(H, <<"<span class=\"ah-status-bar__row-label\">Lines</span>"
                     "<span class=\"ah-status-bar__row-val\">3</span>">>)),
    ?assert(has(H, <<"<div class=\"ah-status-bar__extra\">Ln 1</div>">>)),
    ?assert(has(H, <<"Modified">>)),
    [_, Right] = binary:split(H, <<"ah-status-bar__side--right">>),
    ?assert(has(Right, <<"UTF-8">>)),
    ?assert(has(Right, <<"ah-status-bar__save">>)),
    Plain = r(?M:status_bar([#{count => 2, label => <<"errors">>}], [], [])),
    ?assertNot(has(Plain, <<"data-dirty">>)),
    ?assertNot(has(Plain, <<"ah-status-bar__popover">>)).


%%%===================================================================
%%% element record (designs/05-records.md)
%%%===================================================================

record_equals_builder_test() ->
    ?assertEqual(r(?M:status_bar([<<"Ln 1">>], [], [{dirty, true}, {content, <<"a b">>}])),
                 r(#ah_status_bar{segments = [<<"Ln 1">>], dirty = true,
                                  content = <<"a b">>})).

no_postback_event_test() ->
    ?assertError({aihtml, {no_postback_event, ah_status_bar}},
                 r(#ah_status_bar{postback = go})).

field_validation_test() ->
    ?assertError({aihtml, {bad_option, dirty, sometimes}}, r(#ah_status_bar{dirty = sometimes})).

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

default(ah_status_bar) -> #ah_status_bar{}.
