-module(aihtml_checkbox_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_checkbox.hrl").

-export([action/4]).

-define(M, aihtml_checkbox).

r(H) -> aihtml_html:render_binary(H).

has(Sub, Bin) -> binary:match(Bin, Sub) =/= nomatch.

%% --- checkbox ------------------------------------------------------

checkbox_markup_test() ->
    H = r(?M:ah_checkbox(<<"Accept">>, yes, [], [{name, terms}, {checked, true}, {id, <<"t">>}])),
    ?assertMatch(<<"<label class=\"ah-checkbox ah-checkbox-checked\" data-ah=\"checkbox\">", _/binary>>, H),
    %% Attrs land on the native input
    ?assert(has(<<"<input class=\"ah-choice-input\" type=\"checkbox\" value=\"yes\" name=\"terms\" checked id=\"t\">">>, H)),
    ?assert(has(<<"ah-checkbox-check ah-checkbox-check-checked">>, H)),
    ?assert(has(<<"<span class=\"ah-checkbox-label\">Accept</span>">>, H)).

checkbox_states_test() ->
    I = r(?M:ah_checkbox(<<"x">>, undefined, [], [{indeterminate, true}])),
    ?assert(has(<<"ah-checkbox-indeterminate">>, I)),
    ?assert(has(<<"ah-checkbox-check-indeterminate">>, I)),
    ?assertNot(has(<<"indeterminate=">>, I)),         % an option, not an attribute
    D = r(?M:ah_checkbox(<<"x">>, undefined, [], [{disabled, true}, {three_states, true}, {locked, true}])),
    ?assert(has(<<"ah-checkbox-disabled">>, D)),
    ?assert(has(<<" disabled">>, D)),
    ?assert(has(<<"data-ah-three-states">>, D)),
    ?assert(has(<<"data-ah-locked">>, D)),
    ?assertNot(has(<<"value=">>, D)),
    B = r(?M:ah_checkbox([], undefined, [], [{box_size, 24}])),
    ?assert(has(<<"style=\"width:24px;height:24px;\"">>, B)),
    ?assertNot(has(<<"ah-checkbox-label">>, B)).

checkbox_escaping_test() ->
    H = r(?M:ah_checkbox(<<"<b>&">>, <<"a\"b">>, [<<"mt-2">>], [{title, <<"<x>">>}])),
    ?assert(has(<<"&lt;b&gt;&amp;">>, H)),
    ?assert(has(<<"value=\"a&quot;b\"">>, H)),
    ?assert(has(<<"title=\"&lt;x&gt;\"">>, H)),
    ?assert(has(<<"class=\"ah-checkbox mt-2\"">>, H)).

checkbox_modifiers_test() ->
    ?assert(has(<<"ah-checkbox ah-checkbox-lg">>, r(?M:ah_checkbox(<<"x">>, undefined, [lg], [])))),
    ?assertError({aihtml, {unknown_modifier, checkbox, huge, _}},
                 ?M:ah_checkbox(<<"x">>, undefined, [huge], [])),
    ?assertError({aihtml, {conflicting_modifiers, checkbox, size, _}},
                 ?M:ah_checkbox(<<"x">>, undefined, [sm, lg], [])).

checkbox_action_on_input_test() ->
    A = [{<<"data-ah-on">>, {actions, [{<<"change">>, <<"TOKEN">>, #{}}]}}],
    H = r(?M:ah_checkbox(<<"x">>, undefined, [], [A])),
    ?assert(has(<<"type=\"checkbox\" data-ah-on=\"change:TOKEN\">">>, H)).

render_all_test() ->
    ?assert(is_binary(r(?M:ah_checkbox(<<"x">>, undefined, [], [])))).

record_equals_builder_test() ->
    ?assertEqual(r(?M:ah_checkbox(<<"Accept">>, yes, [lg, <<"mt-2">>],
                                  [{name, terms}, {checked, true}, {id, t}, {box_size, 20}])),
                 r(#ah_checkbox{body = <<"Accept">>, value = yes, size = lg,
                                css = [<<"mt-2">>], attrs = [{name, terms}],
                                checked = true, id = t, box_size = 20})).

builder_fills_fields_test() ->
    C = ?M:ah_checkbox(<<"x">>, v, [sm, <<"x">>],
                       [{name, n}, {checked, true}, {three_states, true}, {title, <<"t">>},
                        on(change)]),
    ?assertMatch(#ah_checkbox{value = v, size = sm, checked = true, disabled = false,
                              three_states = true, css = [<<"x">>]}, C),
    [{name, n}, {title, <<"t">>}, {<<"data-ah-on">>, _} | _] = C#ah_checkbox.attrs,
    ?assertError({aihtml, {record_only_field, ah_checkbox, postback}},
                 ?M:ah_checkbox(<<"x">>, undefined, [], [{postback, save}])).

postback_test() ->
    P = {save, #{id => 1}},
    postback_change(#ah_checkbox{postback = P}),
    %% single controls bind it on the native input
    ?assertMatch({match, _}, re:run(r(#ah_checkbox{postback = P}),
                                    <<"<input [^>]*data-ah-on=">>)).

field_validation_test() ->
    ?assertError({aihtml, {bad_modifier, checkbox, size, huge, _}},
                 r(#ah_checkbox{size = huge})),
    %% groups without a default may stay undefined
    ?assert(has(<<"class=\"ah-checkbox\"">>, r(#ah_checkbox{}))).

on(Event) -> aihtml:on(Event, {?MODULE, x, #{}}).

%% --- catalog --------------------------------------------------------

catalog_test() ->
    Names = [N || #{name := N} <- ?M:catalog()],
    ?assertEqual([checkbox], Names),
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

default(ah_checkbox) -> #ah_checkbox{}.

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
