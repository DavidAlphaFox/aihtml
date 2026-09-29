-module(aihtml_radiobutton_group_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_radiobutton_group.hrl").

-export([action/4]).

-define(M, aihtml_radiobutton_group).

r(H) -> aihtml_html:render_binary(H).

has(Sub, Bin) -> binary:match(Bin, Sub) =/= nomatch.

count(Sub, Bin) -> length(binary:matches(Bin, Sub)).

%% --- radiobutton_group ---------------------------------------------

items() -> [{a, <<"A">>}, {b, <<"B & b">>}, {c, <<"C">>, #{disabled => true, class => <<"x">>}}].

radiobutton_group_test() ->
    H = r(?M:radiobutton_group(items(), b, [], [{name, p}])),
    ?assert(has(<<"role=\"radiogroup\" data-ah-value=\"b\"">>, H)),
    ?assert(has(<<"data-ah=\"radiobutton-group\"">>, H)),
    ?assertEqual(3, count(<<"type=\"radio\"">>, H)),
    ?assert(has(<<"value=\"b\" name=\"p\" checked">>, H)),
    ?assert(has(<<"ah-radiobutton ah-radiobutton-checked">>, H)),
    %% an unknown value selects nothing
    ?assert(has(<<"data-ah-value=\"\"">>, r(?M:radiobutton_group(items(), zzz, [], [])))).

render_all_test() ->
    Items = [{a, <<"A">>}, {b, <<"B">>}],
    ?assert(is_binary(r(?M:radiobutton_group(Items, a, [], [])))).

builder_fills_fields_test() ->
    G = ?M:radiobutton_group(items(), a, [], [{name, p}, {required, true}, {form, f}]),
    ?assertMatch(#ah_radiobutton_group{name = p, required = true, form = f,
                                       layout = vertical, attrs = []}, G).

postback_test() ->
    postback_change(#ah_radiobutton_group{items = items(), postback = {save, #{id => 1}}}).

field_validation_test() ->
    ?assertError({aihtml, {bad_flag, radiobutton_group, label_before, yes}},
                 r(#ah_radiobutton_group{label_before = yes})).

%% --- catalog --------------------------------------------------------

catalog_test() ->
    Names = [N || #{name := N} <- ?M:catalog()],
    ?assertEqual([radiobutton_group], Names),
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

default(ah_radiobutton_group) -> #ah_radiobutton_group{}.

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
