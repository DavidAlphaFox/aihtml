-module(aihtml_link_button_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_link_button.hrl").

-define(M, aihtml_link_button).

r(H) -> aihtml_html:render_binary(H).

has(Sub, Bin) -> binary:match(Bin, Sub) =/= nomatch.

-define(has(Sub, Bin), ?assert(has(Sub, Bin))).
-define(hasnt(Sub, Bin), ?assertNot(has(Sub, Bin))).

%%% link_button

link_button_test() ->
    H = r(?M:ah_link_button(<<"Docs & more">>, <<"/docs?a=1&b=2">>, [outlined], [{target, <<"_blank">>}])),
    ?assertEqual(<<"<a class=\"ah-btn ah-btn-outlined ah-link-btn\" role=\"link\" "
                   "href=\"/docs?a=1&amp;b=2\" target=\"_blank\">Docs &amp; more</a>">>, H).

link_button_disabled_test() ->
    H = r(?M:ah_link_button(<<"x">>, <<"/x">>, [], [{disabled, true}])),
    ?hasnt(<<"href">>, H),
    ?hasnt(<<" disabled">>, H),
    ?has(<<"aria-disabled=\"true\"">>, H),
    ?has(<<"tabindex=\"-1\"">>, H),
    ?has(<<"ah-btn-disabled">>, H).

%%% catalog (demos live in aihtml_example)

catalog_test() ->
    Cat = ?M:catalog(),
    Names = [N || #{name := N} <- Cat],
    ?assertEqual([link_button], Names),
    [begin
         ?assert(erlang:function_exported(?M, aihtml_catalog:builder(N), 4)),
         #{category := form, signature := S, root := <<"ah-", _/binary>>} = E,
         ?assert(is_binary(S))
     end || #{name := N} = E <- Cat],
    %% every behaviour named in the catalog is rendered by its component
    Behaviors = [B || #{behavior := B} <- Cat],
    ?assertEqual(0, length(Behaviors)).

catalog_docs_test() ->
    [begin
         Docs = maps:get(option_docs, E, #{}),
         ?assertEqual(lists:sort(maps:get(options, E, []) ++ maps:get(flags, E, [])),
                      lists:sort(maps:keys(Docs))),
         [?assert(is_binary(D) andalso D =/= <<>>) || D <- maps:values(Docs)],
         Ms = maps:get(methods, E),
         [#{name := _, args := <<"(", _/binary>>, doc := _} = X || X <- Ms],
         ?assertEqual(maps:get(behavior, E, none) =/= none, Ms =/= [])
     end || E <- ?M:catalog()].

%%% element records (designs/05-records.md)

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

default(ah_link_button) -> #ah_link_button{}.
