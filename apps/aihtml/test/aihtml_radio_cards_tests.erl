-module(aihtml_radio_cards_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_radio_cards.hrl").

-export([action/4]).

-define(M, aihtml_radio_cards).

r(H) -> aihtml_html:render_binary(H).

has(Sub, Bin) -> binary:match(Bin, Sub) =/= nomatch.

count(Sub, Bin) -> length(binary:matches(Bin, Sub)).

%% --- radio_cards ---------------------------------------------------

items() -> [{a, <<"A">>}, {b, <<"B & b">>}, {c, <<"C">>, #{disabled => true, class => <<"x">>}}].

radio_cards_test() ->
    Items = [{free, <<"Free">>, #{description => <<"<i>trial</i>">>}},
             {pro, <<"Pro">>, #{icon => <<"P">>}},
             {team, <<"Team">>, [{disabled, true}]}],
    H = r(?M:radio_cards(Items, pro, [], [{name, plan}, {columns, 2}, {align, start}])),
    ?assertMatch(<<"<div class=\"ah-radio-cards\" data-ah=\"radio-cards\" role=\"radiogroup\" data-ah-value=\"pro\" data-columns=\"2\" data-align=\"start\" data-disabled=\"false\">", _/binary>>, H),
    ?assert(has(<<"&lt;i&gt;trial&lt;/i&gt;">>, H)),
    ?assert(has(<<"<span class=\"ah-radio-cards__icon\" aria-hidden=\"true\">P</span>">>, H)),
    ?assert(has(<<"data-value=\"pro\" data-index=\"1\" data-selected=\"true\"">>, H)),
    ?assert(has(<<"data-value=\"team\" data-index=\"2\" data-selected=\"false\" data-disabled=\"true\"">>, H)),
    ?assertEqual(3, count(<<"name=\"plan\"">>, H)),
    ?assertError({aihtml, {bad_option, radio_cards, columns, <<"4">>}},
                 r(?M:radio_cards(Items, pro, [], [{columns, 4}]))),
    ?assertError({aihtml, {unknown_modifier, radio_cards, big, _}},
                 ?M:radio_cards(Items, pro, [big], [])).

render_all_test() ->
    Items = [{a, <<"A">>}, {b, <<"B">>}],
    ?assert(is_binary(r(?M:radio_cards(Items, a, [], [])))).

record_equals_builder_test() ->
    ?assertEqual(r(?M:radio_cards(items(), b, [], [{name, plan}, {columns, 2},
                                                   {align, start}, {disabled, true}])),
                 r(#ah_radio_cards{items = items(), value = b, name = plan, columns = 2,
                                   align = start, disabled = true})).

postback_test() ->
    postback_change(#ah_radio_cards{items = items(), postback = {save, #{id => 1}}}).

field_validation_test() ->
    ?assertError({aihtml, {bad_option, radio_cards, columns, <<"5">>}},
                 r(#ah_radio_cards{columns = 5})),
    ?assertError({aihtml, {bad_option, radio_cards, align, <<"end">>}},
                 r(#ah_radio_cards{align = 'end'})).

%% --- catalog --------------------------------------------------------

catalog_test() ->
    Names = [N || #{name := N} <- ?M:catalog()],
    ?assertEqual([radio_cards], Names),
    [begin
         ?assert(erlang:function_exported(?M, N, 4)),
         ?assertMatch(#{category := form, behavior := B} when is_binary(B), E)
     end || #{name := N} = E <- ?M:catalog()].

api_docs_test() ->
    [begin
         Documented = maps:keys(maps:get(option_docs, E)),
         Mods = lists:append([Ms || {Ms, _} <- maps:values(maps:get(groups, E, #{}))]),
         Keys = maps:get(options, E, []) ++ maps:get(flags, E, []) ++ Mods,
         ?assertEqual({N, []}, {N, Keys -- Documented}),
         ?assertMatch([_ | _], maps:get(methods, E))
     end || #{name := N} = E <- ?M:catalog()].

%%% element records (designs/05-records.md)

-spec action(atom(), term(), map(), term()) -> ok.
action(_, _, _, _) -> ok.

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

default(ah_radio_cards) -> #ah_radio_cards{}.

token(Html) ->
    {match, [T]} = re:run(r(Html), <<"data-ah-on=\"([a-z]+:[^\"]+)\"">>,
                          [{capture, all_but_first, binary}]),
    [Ev, Tok] = binary:split(T, <<":">>),
    {ok, Ref} = aihtml_action:unsign(Tok),
    {Ev, Ref}.

%% the postback fires on change
postback_change(E) ->
    ?assertEqual({element(1, E), {<<"change">>, {?MODULE, save, #{id => 1}}}},
                 {element(1, E), token(E)}).
