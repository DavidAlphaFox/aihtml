-module(aihtml_tab_bar_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_tab_bar.hrl").

-export([action/4]).

-define(M, aihtml_tab_bar).

r(H) -> aihtml_html:render_binary(H).

has(Bin, Part) -> binary:match(Bin, Part) =/= nomatch.

count(Bin, Part) -> length(binary:matches(Bin, Part)).

%%% catalog

catalog_matches_exports_test() ->
    Exports = ?M:module_info(exports),
    Names = [N || #{name := N} <- ?M:catalog()],
    ?assertEqual([tab_bar], Names),
    [?assert(lists:keymember(aihtml_catalog:builder(N), 1, Exports)) || N <- Names],
    [?assertMatch(#{category := layout, root := <<"ah-", _/binary>>, signature := _}, E)
     || E <- ?M:catalog()].

catalog_documents_every_option_test() ->
    [begin
         Docs = maps:get(option_docs, E),
         ?assertEqual({N, []}, {N, [K || K <- maps:get(options, E, []) ++ maps:get(flags, E, []),
                                         not maps:is_key(K, Docs)]}),
         ?assert(is_list(maps:get(methods, E)))
     end || #{name := N} = E <- ?M:catalog()].

%%% tab_bar

tab_bar_test() ->
    H = r(?M:ah_tab_bar([{a, <<"a.erl">>}, {b, <<"b.erl">>, #{dirty => true}}], b, [], [])),
    ?assert(has(H, <<"class=\"ah-tab-bar\" role=\"tablist\" data-ah=\"tab-bar\" data-ah-value=\"b\"">>)),
    ?assert(has(H, <<"data-id=\"b\" data-active=\"true\" data-dirty=\"true\"">>)),
    ?assertEqual(1, count(H, <<"ah-tab-bar__dot">>)),
    ?assertEqual(2, count(H, <<"aria-label=\"close ">>)),
    ?assertNot(has(r(?M:ah_tab_bar([{a, <<"A">>}], a, [], [{closable, false}])), <<"__close">>)).

%%% CSS: every sigil class the module writes exists in the stylesheets

classes_are_styled_test() ->
    %% the sources when run from the project root, else the installed copy
    Root = "apps/aihtml/priv/css",
    Dir = case filelib:is_dir(Root) of
              true -> Root;
              false -> filename:join(code:priv_dir(aihtml), "css")
          end,
    Css = iolist_to_binary([element(2, file:read_file(F))
                            || F <- filelib:wildcard(filename:join([Dir, "**", "*.css"]))]),
    Html = iolist_to_binary([r(H) || H <- samples()]),
    {ok, Re} = re:compile(<<"class=\"([^\"]*)\"">>),
    {match, Ms} = re:run(Html, Re, [global, {capture, [1], binary}]),
    Classes = lists:usort([C || [Cs] <- Ms, C <- binary:split(Cs, <<" ">>, [global, trim_all]),
                                binary:match(C, <<"ah-">>) =:= {0, 3}]),
    Markers = [],
    Missing = [C || C <- Classes -- Markers, not has(Css, <<".", C/binary>>)],
    ?assertEqual([], Missing).

%% One render of the component in its main variants and states.
samples() ->
    [?M:ah_tab_bar([{a, <<"A">>, #{dirty => true, icon => <<"i">>}}, {b, <<"B">>}], a, [], [])].

%%% element records (designs/05-records.md)

-spec action(atom(), term(), map(), term()) -> ok.
action(_, _, _, _) -> ok.

token(Html) ->
    {match, [T]} = re:run(r(Html), <<"data-ah-on=\"([a-z]+:[^\"]+)\"">>,
                          [{capture, all_but_first, binary}]),
    [Ev, Tok] = binary:split(T, <<":">>),
    {ok, Ref} = aihtml_action:unsign(Tok),
    {Ev, Ref}.

builder_fills_fields_test() ->
    ?assertMatch(#ah_tab_bar{items = [], value = x, closable = false},
                 ?M:ah_tab_bar([], x, [], [{closable, false}])).

postback_test() ->
    ?assertEqual({<<"change">>, {?MODULE, focus, #{}}},
                 token(#ah_tab_bar{items = [{a, <<"a">>}], postback = focus})).

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

default(ah_tab_bar) -> #ah_tab_bar{}.
