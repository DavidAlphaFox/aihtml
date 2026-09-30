-module(aihtml_toggle_button_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_toggle_button.hrl").

-export([action/4]).

-define(M, aihtml_toggle_button).

r(H) -> aihtml_html:render_binary(H).

has(Sub, Bin) -> binary:match(Bin, Sub) =/= nomatch.

-define(has(Sub, Bin), ?assert(has(Sub, Bin))).
-define(hasnt(Sub, Bin), ?assertNot(has(Sub, Bin))).

%%% toggle_button

toggle_button_test() ->
    Off = r(?M:ah_toggle_button(<<"B">>, false, [default], [])),
    ?has(<<"data-ah=\"toggle-button\"">>, Off),
    ?has(<<"data-ah-value=\"false\"">>, Off),
    ?has(<<"aria-pressed=\"false\"">>, Off),
    ?has(<<"value=\"false\"">>, Off),
    ?hasnt(<<"ah-btn-toggled">>, Off),
    On = r(?M:ah_toggle_button(<<"B">>, true, [], [])),
    ?has(<<"ah-btn-toggled">>, On),
    ?has(<<"data-ah-value=\"true\"">>, On).

toggle_button_hidden_input_test() ->
    H = r(?M:ah_toggle_button(<<"B">>, true, [], [{name, bold}, {id, t}])),
    ?has(<<"<input type=\"hidden\" name=\"bold\" value=\"true\" data-ah-input>">>, H),
    %% the name is on the hidden input only
    ?assertEqual(1, length(binary:matches(H, <<"name=">>))),
    ?has(<<"id=\"t\"">>, H),
    ?assertError(function_clause, ?M:ah_toggle_button(<<"B">>, yes, [], [])).

toggle_button_on_change_test() ->
    Attrs = [{<<"data-ah-on">>, {actions, [{<<"change">>, <<"TOKEN">>, #{}}]}}],
    H = r(?M:ah_toggle_button(<<"B">>, false, [], Attrs)),
    ?has(<<"data-ah-on=\"change:TOKEN\"">>, H).

%%% catalog (demos live in aihtml_example)

catalog_test() ->
    Cat = ?M:catalog(),
    Names = [N || #{name := N} <- Cat],
    ?assertEqual([toggle_button], Names),
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

builder_fills_fields_test() ->
    B = ?M:ah_toggle_button(<<"B">>, true, [secondary, <<"x">>],
                            [{name, bold}, {disabled, true}, {icon, <<"*">>}, {title, <<"t">>},
                             on(change)]),
    ?assertMatch(#ah_toggle_button{value = true, variant = secondary, size = md,
                                   name = bold, disabled = true, icon = <<"*">>,
                                   css = [<<"x">>]}, B),
    [{title, <<"t">>}, {<<"data-ah-on">>, _} | _] = B#ah_toggle_button.attrs.

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

default(ah_toggle_button) -> #ah_toggle_button{}.

on(Event) -> aihtml:on(Event, {?MODULE, x, #{}}).
