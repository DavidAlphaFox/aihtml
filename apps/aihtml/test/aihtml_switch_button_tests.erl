-module(aihtml_switch_button_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_switch_button.hrl").

-export([action/4]).

-define(M, aihtml_switch_button).

r(H) -> aihtml_html:render_binary(H).

has(Sub, Bin) -> binary:match(Bin, Sub) =/= nomatch.

%% --- switch --------------------------------------------------------

switch_test() ->
    H = r(?M:ah_switch_button(<<"Wi-Fi">>, undefined, [lg], [{name, wifi}, {checked, true},
                                                             {on_label, <<"<on>">>}])),
    ?assertMatch(<<"<label class=\"ah-switch ah-switch-lg ah-switch-on\" data-ah=\"switch-button\">", _/binary>>, H),
    ?assert(has(<<"type=\"checkbox\" name=\"wifi\" checked role=\"switch\">">>, H)),
    ?assert(has(<<"ah-switch-label ah-switch-label-on\">&lt;on&gt;</span>">>, H)),
    ?assertNot(has(<<"ah-switch-label-off">>, H)),
    ?assert(has(<<"<span class=\"ah-switch-text\">Wi-Fi</span>">>, H)),
    C = r(?M:ah_switch_button([], undefined, [], [{width, 80}, {height, 32}])),
    ?assert(has(<<"width:80px;height:32px;--sw-travel:-48px;">>, C)),
    ?assert(has(<<"width:28px;height:28px;">>, C)).

render_all_test() ->
    ?assert(is_binary(r(?M:ah_switch_button(<<"x">>, undefined, [], [])))).

record_equals_builder_test() ->
    ?assertEqual(r(?M:ah_switch_button(<<"Wi-Fi">>, undefined, [sm],
                                       [{name, wifi}, {disabled, true}, {on_label, <<"On">>},
                                        {width, 60}])),
                 r(#ah_switch_button{body = <<"Wi-Fi">>, size = sm, attrs = [{name, wifi}],
                                     disabled = true, on_label = <<"On">>, width = 60})).

postback_test() ->
    postback_change(#ah_switch_button{postback = {save, #{id => 1}}}).

field_validation_test() ->
    ?assertError({aihtml, {modifier_in_css, switch_button, lg}},
                 r(#ah_switch_button{css = [lg]})).

%% --- catalog --------------------------------------------------------

catalog_test() ->
    Names = [N || #{name := N} <- ?M:catalog()],
    ?assertEqual([switch_button], Names),
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

default(ah_switch_button) -> #ah_switch_button{}.

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
