-module(aihtml_radiobutton_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_radiobutton.hrl").

-export([action/4]).

-define(M, aihtml_radiobutton).

r(H) -> aihtml_html:render_binary(H).

has(Sub, Bin) -> binary:match(Bin, Sub) =/= nomatch.

%% --- radiobutton ---------------------------------------------------

radiobutton_test() ->
    H = r(?M:radiobutton(<<"Email">>, email, [sm], [{name, c}, {checked, true}])),
    ?assertMatch(<<"<label class=\"ah-radiobutton ah-radiobutton-sm ah-radiobutton-checked\" data-ah=\"radiobutton\">", _/binary>>, H),
    ?assert(has(<<"type=\"radio\" value=\"email\" name=\"c\" checked">>, H)),
    ?assert(has(<<"ah-radiobutton-check ah-radiobutton-check-checked">>, H)),
    ?assertError({aihtml, {unknown_modifier, radiobutton, primary, _}},
                 ?M:radiobutton(<<"x">>, x, [primary], [])).

render_all_test() ->
    ?assert(is_binary(r(?M:radiobutton(<<"x">>, a, [], [])))).

postback_test() ->
    postback_change(#ah_radiobutton{postback = {save, #{id => 1}}}).

field_validation_test() ->
    ?assertError({aihtml, {bad_option, box_size, -1}}, r(#ah_radiobutton{box_size = -1})).

%% --- catalog --------------------------------------------------------

catalog_test() ->
    Names = [N || #{name := N} <- ?M:catalog()],
    ?assertEqual([radiobutton], Names),
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

default(ah_radiobutton) -> #ah_radiobutton{}.

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
