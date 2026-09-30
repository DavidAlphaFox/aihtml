-module(aihtml_rating_group_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_rating_group.hrl").

-export([action/4]).

-define(M, aihtml_rating_group).

r(H) -> aihtml_html:render_binary(H).

has(Sub, Bin) -> binary:match(Bin, Sub) =/= nomatch.

count(Sub, Bin) -> length(binary:matches(Bin, Sub)).

%% --- rating --------------------------------------------------------

rating_test() ->
    H = r(?M:ah_rating_group(5, 2.5, [lg, error], [{name, stars}, {precision, 0.5}, {id, <<"r">>}])),
    ?assertMatch(<<"<div class=\"ah-rating\" data-ah=\"rating\" role=\"radiogroup\" data-ah-value=\"2.5\" data-ah-max=\"5\" data-size=\"lg\" data-color=\"error\" data-precision=\"0.5\" data-readonly=\"false\" data-disabled=\"false\" data-allow-clear=\"true\" id=\"r\">", _/binary>>, H),
    ?assertEqual(5, count(<<"class=\"ah-rating__star\"">>, H)),
    ?assertEqual(2, count(<<"aria-checked=\"true\"">>, H)),
    ?assert(has(<<"style=\"width:50%;\"">>, H)),
    ?assertEqual(2, count(<<"style=\"width:100%;\"">>, H)),
    ?assert(has(<<"<input type=\"hidden\" name=\"stars\" value=\"2.5\">">>, H)),
    ?assert(has(<<"aria-label=\"3 / 5\"">>, H)).

rating_defaults_test() ->
    H = r(?M:ah_rating_group(3, undefined, [], [{readonly, true}, {allow_clear, false}])),
    ?assert(has(<<"data-ah-value=\"0\"">>, H)),
    ?assert(has(<<"data-size=\"md\" data-color=\"warning\" data-precision=\"1\" data-readonly=\"true\"">>, H)),
    ?assert(has(<<"data-allow-clear=\"false\"">>, H)),
    ?assert(has(<<"aria-readonly=\"true\"">>, H)),
    ?assertEqual(3, count(<<"tabindex=\"-1\"">>, H)),
    ?assertNot(has(<<"type=\"hidden\"">>, H)),
    ?assertNot(has(<<" readonly">>, H)),
    D = r(?M:ah_rating_group(2, 1.0, [], [{disabled, true}])),
    ?assert(has(<<"data-ah-value=\"1\"">>, D)),
    ?assertEqual(2, count(<<" disabled>">>, D)),
    ?assertError({aihtml, {conflicting_modifiers, rating_group, size, _}},
                 ?M:ah_rating_group(5, 1, [sm, lg], [])),
    ?assertError({aihtml, {bad_option, rating_group, precision, 0.25}},
                 r(?M:ah_rating_group(5, 1, [], [{precision, 0.25}]))),
    ?assertError({aihtml, {bad_max, rating_group, 0}}, ?M:ah_rating_group(0, 1, [], [])).

render_all_test() ->
    ?assert(is_binary(r(?M:ah_rating_group(5, 3, [], [])))).

record_equals_builder_test() ->
    ?assertEqual(r(?M:ah_rating_group(5, 2.5, [lg, error], [{name, stars}, {precision, 0.5}])),
                 r(#ah_rating_group{max = 5, value = 2.5, size = lg, color = error,
                                    name = stars, precision = 0.5})).

postback_test() ->
    postback_change(#ah_rating_group{postback = {save, #{id => 1}}}),
    ?assertEqual({<<"change">>, {other_mod, rate, 3}},
                 token(#ah_rating_group{postback = {rate, 3}, delegate = other_mod})).

field_validation_test() ->
    ?assertError({aihtml, {bad_modifier, rating_group, color, blue, _}},
                 r(#ah_rating_group{color = blue})),
    ?assertError({aihtml, {bad_option, rating_group, precision, 2}},
                 r(#ah_rating_group{precision = 2})),
    ?assertError({aihtml, {bad_max, rating_group, 0}}, r(#ah_rating_group{max = 0})),
    ?assertError({aihtml, {bad_value, rating_group, high}},
                 r(#ah_rating_group{value = high})).

%% --- catalog --------------------------------------------------------

catalog_test() ->
    Names = [N || #{name := N} <- ?M:catalog()],
    ?assertEqual([rating_group], Names),
    [begin
         ?assert(erlang:function_exported(?M, aihtml_catalog:builder(N), 4)),
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

default(ah_rating_group) -> #ah_rating_group{}.

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
