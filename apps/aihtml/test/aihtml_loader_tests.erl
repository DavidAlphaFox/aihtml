-module(aihtml_loader_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_loader.hrl").

-define(M, aihtml_loader).

r(H) -> aihtml_html:render_binary(H).

has(Bin, Part) -> binary:match(Bin, Part) =/= nomatch.

%%% catalog

catalog_matches_exports_test() ->
    Exports = ?M:module_info(exports),
    Names = [N || #{name := N} <- ?M:catalog()],
    ?assertEqual([loader], Names),
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

%%% loader

loader_test() ->
    H = r(?M:loader([hidden, top], [{text, <<"Wait">>}, {modal, true}])),
    ?assertEqual(<<"<div class=\"ah-loader ah-loader-text-top ah-loader-hidden\" role=\"status\" "
                   "aria-live=\"polite\" aria-busy=\"false\" aria-label=\"Wait\" data-ah=\"loader\" "
                   "data-modal=\"true\"><div class=\"ah-loader-icon\" aria-hidden=\"true\"></div>"
                   "<div class=\"ah-loader-text\">Wait</div></div>">>, H),
    ?assert(has(r(?M:loader([], [])), <<"ah-loader ah-loader-text-bottom\"">>)).

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
    [?M:loader([P, hidden, inline, center, disabled], []) || P <- [top, bottom, left, right]].

%%% element records (designs/05-records.md)

record_equals_builder_test() ->
    ?assertEqual(r(?M:loader([hidden, top], [{text, <<"Wait">>}, {modal, true}])),
                 r(#ah_loader{hidden = true, text_position = top, text = <<"Wait">>,
                              modal = true})).

postback_test() ->
    ?assertError({aihtml, {no_postback_event, ah_loader}}, r(setelement(6, #ah_loader{}, x))).

field_validation_test() ->
    ?assertError({aihtml, {bad_modifier, loader, text_position, center, _}},
                 r(#ah_loader{text_position = center})).

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

default(ah_loader) -> #ah_loader{}.
