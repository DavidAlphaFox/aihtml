-module(aihtml_card_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_card.hrl").

-define(M, aihtml_card).

r(H) -> aihtml_html:render_binary(H).

has(Bin, Part) -> binary:match(Bin, Part) =/= nomatch.

%%% catalog

catalog_matches_exports_test() ->
    Exports = ?M:module_info(exports),
    Names = [N || #{name := N} <- ?M:catalog()],
    ?assertEqual([card], Names),
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

%%% card

card_test() ->
    H = r(?M:card(<<"Body & more">>, [hover, <<"w-64">>],
                  [{title, <<"T">>}, {footer, <<"F">>}, {id, c1}])),
    ?assertEqual(<<"<div class=\"ah-card ah-card-hover w-64\" id=\"c1\">"
                   "<div class=\"ah-card-header\"><h3 class=\"ah-card-title\">T</h3></div>"
                   "<div class=\"ah-card-body\">Body &amp; more</div>"
                   "<div class=\"ah-card-footer\">F</div></div>">>, H).

card_minimal_test() ->
    ?assertEqual(<<"<div class=\"ah-card\"><div class=\"ah-card-body\">x</div></div>">>,
                 r(?M:card(<<"x">>, [], []))).

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
    [?M:card(<<"b">>, [hover, flush], [{title, <<"t">>}, {subtitle, <<"s">>}, {extra, <<"x">>},
                                   {media, <<"m">>}, {footer, <<"f">>}])].

%%% element records (designs/05-records.md)

unknown_modifier_test() ->
    ?assertError({aihtml, {unknown_modifier, card, bogus, _}}, ?M:card(<<"x">>, [bogus], [])).

record_equals_builder_test() ->
    ?assertEqual(r(?M:card(<<"b">>, [hover, <<"w-64">>],
                           [{title, <<"T">>}, {footer, <<"F">>}, {id, c1}, {data_x, 1}])),
                 r(#ah_card{body = <<"b">>, hover = true, css = [<<"w-64">>], title = <<"T">>,
                            footer = <<"F">>, id = c1, attrs = [{data_x, 1}]})).

builder_fills_fields_test() ->
    ?assertError({aihtml, {record_only_field, ah_card, postback}},
                 ?M:card(<<"x">>, [], [{postback, save}])).

postback_test() ->
    ?assertError({aihtml, {no_postback_event, ah_card}}, r(setelement(6, #ah_card{}, x))).

field_validation_test() ->
    ?assertError({aihtml, {bad_flag, card, hover, yes}}, r(#ah_card{hover = yes})).

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

default(ah_card) -> #ah_card{}.
