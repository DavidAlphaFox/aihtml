-module(aihtml_select_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_select.hrl").

-define(M, aihtml_select).

r(Html) -> aihtml_html:render_binary(Html).

has(Bin, Sub) -> binary:match(Bin, Sub) =/= nomatch.

token(Html) ->
    {match, [T]} = re:run(aihtml_html:render_binary(Html), <<"data-ah-on=\"([a-z]+:[^\"]+)\"">>,
                          [{capture, all_but_first, binary}]),
    [Ev, Tok] = binary:split(T, <<":">>),
    {ok, Ref} = aihtml_action:unsign(Tok),
    {Ev, Ref}.

-define(FRUITS, [{apple, <<"Apple">>}, {group, <<"G">>, [b, {c, <<"C">>, #{disabled => true}}]}]).

select_test() ->
    H = r(?M:select([{a, <<"A">>}, {group, <<"G">>, [b, c]}], b, [sm],
                    [{name, s}, {placeholder, <<"Choose">>}])),
    ?assert(has(H, <<"<span class=\"ah-select ah-select-sm\">">>)),
    ?assert(has(H, <<"<select class=\"ah-select-control\" name=\"s\">">>)),
    ?assert(has(H, <<"<option value=\"\">Choose</option>">>)),
    ?assert(has(H, <<"<optgroup label=\"G\">">>)),
    ?assert(has(H, <<"<option value=\"b\" selected>b</option>">>)),
    ?assertNot(has(H, <<"placeholder">>)).

select_multiple_test() ->
    H = r(?M:select([a, b, c], [a, c], [], [{multiple, true}])),
    ?assert(has(H, <<"<option value=\"a\" selected>">>)),
    ?assert(has(H, <<"<option value=\"b\">">>)),
    ?assert(has(H, <<"<option value=\"c\" selected>">>)),
    ?assert(has(H, <<" multiple>">>)).

%%% element records (designs/05-records.md)

record_equals_builder_test() ->
    ?assertEqual(r(?M:select(?FRUITS, [apple, b], [lg, block],
                             [{multiple, true}, {size, 4}, {name, s}, {id, sel}])),
                 r(#ah_select{items = ?FRUITS, value = [apple, b], size = lg, block = true,
                              id = sel, attrs = [{multiple, true}, {size, 4}, {name, s}]})).

builder_fills_fields_test() ->
    %% {size, N} stays an HTML attribute of the select
    S = ?M:select([a], a, [sm], [{size, 3}, {name, s}]),
    ?assertMatch(#ah_select{size = sm, attrs = [{<<"size">>, 3}, {name, s}]}, S),
    ?assert(has(r(S), <<"<select class=\"ah-select-control\" size=\"3\" name=\"s\">">>)).

postback_test() ->
    ?assertEqual({<<"change">>, {?MODULE, pick, 1}},
                 token(#ah_select{items = [a], postback = {pick, 1}})),
    %% select binds its postback on the <select>, not the wrapper
    ?assert(has(r(#ah_select{items = [a], postback = pick}),
                <<"<select class=\"ah-select-control\" data-ah-on=">>)).

field_validation_test() ->
    ?assertError({aihtml, {bad_modifier, select, size, md, _}}, r(#ah_select{size = md})),
    ?assertError({aihtml, {modifier_in_css, select, lg}}, r(#ah_select{css = [lg]})),
    %% groups without a default may stay undefined
    ?assert(has(r(#ah_select{}), <<"<span class=\"ah-select\">">>)).

%%% catalog

catalog_test() ->
    Names = [N || #{name := N} <- ?M:catalog()],
    ?assertEqual([select], Names),
    [begin
         ?assert(is_binary(maps:get(signature, E))),
         ?assert(erlang:function_exported(?M, N, 4))
     end || #{name := N} = E <- ?M:catalog()].

catalog_docs_test() ->
    [begin
         Docs = maps:get(option_docs, E),
         [?assert(maps:is_key(O, Docs)) || O <- maps:get(options, E, [])],
         ?assert(is_list(maps:get(methods, E)))
     end || E <- ?M:catalog()].

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

default(ah_select) -> #ah_select{}.
