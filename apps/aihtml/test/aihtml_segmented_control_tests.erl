-module(aihtml_segmented_control_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_segmented_control.hrl").

-export([action/4]).

-define(M, aihtml_segmented_control).

r(H) -> aihtml_html:render_binary(H).

has(Sub, Bin) -> binary:match(Bin, Sub) =/= nomatch.

-define(has(Sub, Bin), ?assert(has(Sub, Bin))).
-define(hasnt(Sub, Bin), ?assertNot(has(Sub, Bin))).

%%% segmented_control

-define(ITEMS, [{list, <<"List">>}, {grid, <<"Grid">>}, {board, <<"Board">>}]).

segmented_control_test() ->
    H = r(?M:ah_segmented_control(?ITEMS, grid, [lg, full_width], [{name, layout}, {id, s}])),
    ?has(<<"<div class=\"ah-segmented-control\" role=\"tablist\" data-size=\"lg\" "
           "data-full-width=\"true\" data-disabled=\"false\" data-ah=\"segmented-control\" "
           "data-ah-value=\"grid\" id=\"s\">">>, H),
    ?has(<<"<button class=\"ah-segmented-control__item\" type=\"button\" role=\"tab\" "
           "aria-selected=\"true\" data-value=\"grid\" data-state=\"active\" "
           "data-disabled=\"false\" tabindex=\"0\">Grid</button>">>, H),
    ?has(<<"data-state=\"inactive\"">>, H),
    ?has(<<"<input type=\"hidden\" name=\"layout\" value=\"grid\" data-ah-input>">>, H),
    ?has(<<"data-size=\"md\"">>, r(?M:ah_segmented_control(?ITEMS, grid, [], []))).

segmented_control_disabled_test() ->
    H = r(?M:ah_segmented_control([{a, <<"A">>}, {b, <<"B">>, [{disabled, true}]}], a, [], [])),
    ?has(<<"data-value=\"b\" data-state=\"inactive\" data-disabled=\"true\" tabindex=\"-1\" disabled">>, H),
    D = r(?M:ah_segmented_control(?ITEMS, list, [], [{disabled, true}])),
    ?has(<<"data-disabled=\"true\" aria-disabled=\"true\"">>, D),
    ?assertError({aihtml, {unknown_modifier, segmented_control, xl, _}},
                 ?M:ah_segmented_control(?ITEMS, list, [xl], [])).

%%% catalog (demos live in aihtml_example)

catalog_test() ->
    Cat = ?M:catalog(),
    Names = [N || #{name := N} <- Cat],
    ?assertEqual([segmented_control], Names),
    [begin
         ?assert(erlang:function_exported(?M, aihtml_catalog:builder(N), 4)),
         #{category := form, signature := S, root := <<"ah-", _/binary>>} = E,
         ?assert(is_binary(S))
     end || #{name := N} = E <- Cat],
    %% every behaviour named in the catalog is rendered by its component
    Behaviors = [B || #{behavior := B} <- Cat],
    ?assertEqual(1, length(Behaviors)).

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

-spec action(atom(), term(), map(), term()) -> ok.
action(_, _, _, _) -> ok.

postback_test() ->
    Token = fun(Html) ->
                    {match, [T]} = re:run(r(Html), <<"data-ah-on=\"([a-z]+:[^\"]+)\"">>,
                                          [{capture, all_but_first, binary}]),
                    [Ev, Tok] = binary:split(T, <<":">>),
                    {ok, Ref} = aihtml_action:unsign(Tok),
                    {Ev, Ref}
            end,
    ?assertEqual({<<"change">>, {?MODULE, pick, #{}}},
                 Token(#ah_segmented_control{items = ?ITEMS, postback = pick})).

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

default(ah_segmented_control) -> #ah_segmented_control{}.
