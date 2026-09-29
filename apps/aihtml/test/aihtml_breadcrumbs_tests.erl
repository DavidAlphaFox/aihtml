-module(aihtml_breadcrumbs_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_breadcrumbs.hrl").

-define(M, aihtml_breadcrumbs).

r(H) -> aihtml_html:render_binary(H).

has(Bin, Part) -> binary:match(Bin, Part) =/= nomatch.

count(Bin, Part) -> length(binary:matches(Bin, Part)).

%%% catalog

catalog_matches_exports_test() ->
    Exports = ?M:module_info(exports),
    Names = [N || #{name := N} <- ?M:catalog()],
    ?assertEqual([breadcrumbs], Names),
    [?assert(lists:keymember(N, 1, Exports)) || N <- Names],
    [?assertMatch(#{category := layout, root := <<"ah-", _/binary>>, signature := _}, E)
     || E <- ?M:catalog()].

catalog_documents_every_option_test() ->
    [begin
         Docs = maps:get(option_docs, E),
         ?assertEqual({N, []}, {N, [K || K <- maps:get(options, E, []) ++ maps:get(flags, E, []),
                                         not maps:is_key(K, Docs)]}),
         ?assert(is_list(maps:get(methods, E)))
     end || #{name := N} = E <- ?M:catalog()].

%%% breadcrumbs

breadcrumbs_test() ->
    H = r(?M:breadcrumbs([{<<"Home">>, <<"/">>}, <<"Here">>], [], [])),
    ?assertEqual(<<"<nav class=\"ah-breadcrumbs\" aria-label=\"breadcrumb\" data-has-separator=\"true\">"
                   "<ol class=\"ah-breadcrumbs__list\">"
                   "<li class=\"ah-breadcrumbs__item\" data-index=\"0\">"
                   "<a class=\"ah-breadcrumbs__link\" href=\"/\">Home</a></li>"
                   "<li class=\"ah-breadcrumbs__separator\" role=\"presentation\" aria-hidden=\"true\">/</li>"
                   "<li class=\"ah-breadcrumbs__item\" data-index=\"1\" aria-current=\"page\">"
                   "<span class=\"ah-breadcrumbs__text\">Here</span></li></ol></nav>">>, H).

breadcrumbs_collapse_dots_test() ->
    H = r(?M:breadcrumbs([{<<"1">>, <<"#">>}, {<<"2">>, <<"#">>}, {<<"3">>, <<"#">>},
                          {<<"4">>, <<"#">>}, {<<"5">>, <<"#">>}], [],
                         [{max_items, 3}, {separator, none}, {active_last, true}])),
    ?assert(has(H, <<"data-has-separator=\"false\"">>)),
    ?assert(has(H, <<"ah-breadcrumbs__ellipsis">>)),
    ?assertEqual(3, count(H, <<"ah-breadcrumbs__link">>)),
    ?assertNot(has(H, <<"aria-current">>)).

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
    [?M:breadcrumbs([#{label => <<"H">>, href => <<"/">>, icon => <<"i">>}, {<<"A">>, <<"#">>},
                     {<<"B">>, <<"#">>}, {<<"C">>, <<"#">>}, <<"D">>], [], [{max_items, 3}])].

%%% element records (designs/05-records.md)

builder_fills_fields_test() ->
    ?assertMatch(#ah_breadcrumbs{items = [<<"A">>], separator = none, max_items = 3,
                                 label = <<"breadcrumb">>},
                 ?M:breadcrumbs([<<"A">>], [], [{separator, none}, {max_items, 3}])).

postback_test() ->
    ?assertError({aihtml, {no_postback_event, ah_breadcrumbs}},
                 r(setelement(6, #ah_breadcrumbs{}, x))).

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

default(ah_breadcrumbs) -> #ah_breadcrumbs{}.
