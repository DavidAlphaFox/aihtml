-module(aihtml_toolbar_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_toolbar.hrl").

-define(M, aihtml_toolbar).

r(Html) -> aihtml_html:render_binary(Html).

has(Bin, Sub) -> binary:match(Bin, Sub) =/= nomatch.

count(Bin, Sub) -> length(binary:matches(Bin, Sub)).

%%%===================================================================
%%% catalog
%%%===================================================================

catalog_names_test() ->
    ?assertEqual([toolbar], [N || #{name := N} <- ?M:catalog()]).

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
%%% toolbar
%%%===================================================================

toolbar_test() ->
    H = r(?M:toolbar([#{key => b, label => <<"B">>, toggle => true, pressed => true},
                      #{key => i, label => <<"I">>},
                      #{key => u, label => <<"U">>},
                      separator,
                      #{key => x, title => <<"Cut">>, disabled => true, minimizable => false},
                      separator,
                      {custom, <<"text">>}],
                     [], [{popup_width, 240}])),
    ?assert(has(H, <<"class=\"ah-toolbar\" data-ah=\"toolbar\" role=\"toolbar\"">>)),
    ?assert(has(H, <<"data-ah-popup-width=\"240\"">>)),
    ?assert(has(H, <<"ah-toolbar-tool ah-toolbar-tool-first">>)),
    ?assert(has(H, <<"ah-toolbar-tool ah-toolbar-tool-inner">>)),
    ?assert(has(H, <<"ah-toolbar-tool ah-toolbar-tool-last ah-toolbar-tool-separator-after">>)),
    ?assertEqual(2, count(H, <<"class=\"ah-toolbar-separator\"">>)),
    ?assert(has(H, <<"ah-btn ah-btn-sm ah-toolbar-tool-el ah-btn-toggled">>)),
    ?assert(has(H, <<"aria-pressed=\"true\"">>)),
    ?assert(has(H, <<"data-ah-toggle">>)),
    ?assert(has(H, <<"aria-label=\"Cut\"">>)),
    ?assert(has(H, <<" disabled>">>)),
    ?assert(has(H, <<"data-ah-minimizable=\"false\"">>)),
    ?assert(has(H, <<"<div class=\"ah-toolbar-tool-el\">text</div>">>)),
    ?assert(has(H, <<"ah-toolbar-minimize-btn">>)).


%%%===================================================================
%%% element record (designs/05-records.md)
%%%===================================================================

builder_fills_fields_test() ->
    T = ?M:toolbar([#{key => b, label => <<"B">>}], [disabled, <<"x">>],
                   [{popup_width, 160}, {id, tb}, {aria_label, <<"Tools">>}]),
    ?assertMatch(#ah_toolbar{disabled = true, popup_width = 160, id = tb, css = [<<"x">>],
                             attrs = [{aria_label, <<"Tools">>}]}, T).

token(Html) ->
    {match, [T]} = re:run(r(Html), <<"data-ah-on=\"([a-z]+:[^\"]+)\"">>,
                          [{capture, all_but_first, binary}]),
    [Ev, Tok] = binary:split(T, <<":">>),
    {ok, Ref} = aihtml_action:unsign(Tok),
    {Ev, Ref}.

postback_test() ->
    ?assertMatch({<<"change">>, _}, token(#ah_toolbar{tools = [#{key => b}], postback = go})).

field_validation_test() ->
    %% all defaults render
    ?assert(has(r(#ah_toolbar{}), <<"class=\"ah-toolbar\"">>)).

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

default(ah_toolbar) -> #ah_toolbar{}.
