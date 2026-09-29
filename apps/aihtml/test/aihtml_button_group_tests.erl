-module(aihtml_button_group_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_button_group.hrl").

-export([action/4]).

-define(M, aihtml_button_group).

r(H) -> aihtml_html:render_binary(H).

has(Sub, Bin) -> binary:match(Bin, Sub) =/= nomatch.

-define(has(Sub, Bin), ?assert(has(Sub, Bin))).
-define(hasnt(Sub, Bin), ?assertNot(has(Sub, Bin))).

%%% button_group

-define(ITEMS, [{list, <<"List">>}, {grid, <<"Grid">>}, {board, <<"Board">>}]).

button_group_default_test() ->
    H = r(?M:button_group([<<"A">>, {b, <<"B">>, [{title, <<"t">>}]}], undefined, [], [])),
    ?has(<<"class=\"ah-btn-group ah-btn-group-horizontal ah-btn-group-rounded\"">>, H),
    ?has(<<"role=\"group\"">>, H),
    ?has(<<"data-ah=\"button-group\"">>, H),
    ?hasnt(<<"data-ah-value">>, H),
    ?has(<<"ah-btn-group-btn ah-btn-group-btn-first\" type=\"button\" value=\"A\"">>, H),
    ?has(<<"ah-btn-group-btn ah-btn-group-btn-last\" type=\"button\" value=\"b\" data-value=\"b\" title=\"t\"">>, H).

button_group_radio_test() ->
    H = r(?M:button_group(?ITEMS, grid, [radio], [{name, view}])),
    ?has(<<"role=\"radiogroup\"">>, H),
    ?has(<<"ah-btn-group-radio">>, H),
    ?has(<<"data-ah-value=\"grid\"">>, H),
    ?has(<<"<input type=\"hidden\" name=\"view\" value=\"grid\" data-ah-input>">>, H),
    ?has(<<"ah-btn-group-btn-selected\" type=\"button\" value=\"grid\" data-value=\"grid\" "
           "tabindex=\"0\" role=\"radio\" aria-checked=\"true\"">>, H),
    ?has(<<"value=\"list\" data-value=\"list\" tabindex=\"-1\" role=\"radio\" aria-checked=\"false\"">>, H),
    %% no selection: the first enabled button takes focus
    N = r(?M:button_group([{a, <<"A">>, [{disabled, true}]}, {b, <<"B">>}], undefined, [radio], [])),
    ?has(<<"data-ah-value=\"\"">>, N),
    ?has(<<"value=\"b\" data-value=\"b\" tabindex=\"0\"">>, N),
    ?has(<<"ah-btn-group-btn-disabled\" type=\"button\" value=\"a\" data-value=\"a\" disabled">>, N).

button_group_checkbox_test() ->
    H = r(?M:button_group(?ITEMS, [list, board], [checkbox], [])),
    ?has(<<"data-ah-value=\"list,board\"">>, H),
    ?has(<<"value=\"list\" data-value=\"list\" aria-pressed=\"true\"">>, H),
    ?has(<<"value=\"grid\" data-value=\"grid\" aria-pressed=\"false\"">>, H),
    ?assertEqual(r(?M:button_group(?ITEMS, [list, board], [checkbox], [])),
                 r(?M:button_group(?ITEMS, <<"list,board">>, [checkbox], []))).

button_group_modifiers_test() ->
    H = r(?M:button_group(?ITEMS, list, [radio, vertical, square, outlined], [])),
    ?has(<<"class=\"ah-btn-group ah-btn-group-outlined ah-btn-group-radio ah-btn-group-vertical\"">>, H),
    ?assertError({aihtml, {conflicting_modifiers, button_group, mode, _}},
                 ?M:button_group(?ITEMS, list, [radio, checkbox], [])),
    ?assertError({aihtml, {unknown_modifier, button_group, primary, _}},
                 ?M:button_group(?ITEMS, list, [primary], [])).

button_group_disabled_test() ->
    H = r(?M:button_group(?ITEMS, list, [radio], [{disabled, true}])),
    ?has(<<"ah-btn-group-disabled">>, H),
    ?has(<<"aria-disabled=\"true\"">>, H),
    ?assertEqual(3, length(binary:matches(H, <<" disabled">>))).

button_group_escaping_test() ->
    H = r(?M:button_group([{<<"a\"b">>, <<"<i>">>}], <<"a\"b">>, [radio], [])),
    ?has(<<"data-value=\"a&quot;b\"">>, H),
    ?has(<<"data-ah-value=\"a&quot;b\"">>, H),
    ?has(<<"&lt;i&gt;">>, H).

%%% catalog (demos live in aihtml_example)

catalog_test() ->
    Cat = ?M:catalog(),
    Names = [N || #{name := N} <- Cat],
    ?assertEqual([button_group], Names),
    [begin
         ?assert(erlang:function_exported(?M, N, 4)),
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

record_equals_builder_test() ->
    ?assertEqual(r(?M:button_group(?ITEMS, [list], [checkbox, vertical], [{name, v}])),
                 r(#ah_button_group{items = ?ITEMS, value = [list], mode = checkbox,
                                    orientation = vertical, name = v})).

postback_test() ->
    Token = fun(Html) ->
                    {match, [T]} = re:run(r(Html), <<"data-ah-on=\"([a-z]+:[^\"]+)\"">>,
                                          [{capture, all_but_first, binary}]),
                    [Ev, Tok] = binary:split(T, <<":">>),
                    {ok, Ref} = aihtml_action:unsign(Tok),
                    {Ev, Ref}
            end,
    %% button_group: click in default mode, change in radio mode
    ?assertMatch({<<"click">>, _}, Token(#ah_button_group{items = ?ITEMS, postback = a})),
    ?assertMatch({<<"change">>, _}, Token(#ah_button_group{items = ?ITEMS, mode = radio,
                                                           postback = a})).

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

default(ah_button_group) -> #ah_button_group{}.

%% A value containing a comma is escaped in data-ah-value (aihtml_value).
vhas(Sub, Bin) -> binary:match(Bin, Sub) =/= nomatch.

comma_values_test() ->
    Items = [{<<"a,b">>, <<"AB">>}, {c, <<"C">>}],
    C = r(?M:button_group(Items, [<<"a,b">>, c], [checkbox], [{name, k}])),
    ?assert(vhas(<<"data-ah-value=\"a\\,b,c\"">>, C)),
    ?assert(vhas(<<"name=\"k\" value=\"a\\,b,c\"">>, C)),
    %% the text form is read the same way
    ?assertEqual(C, r(?M:button_group(Items, <<"a\\,b,c">>, [checkbox], [{name, k}]))),
    %% radio mode: the value itself
    R = r(?M:button_group(Items, <<"a,b">>, [radio], [])),
    ?assert(vhas(<<"data-ah-value=\"a,b\"">>, R)).
