-module(aihtml_dropdown_button_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_dropdown_button.hrl").

-define(M, aihtml_dropdown_button).

r(H) -> aihtml_html:render_binary(H).

has(Sub, Bin) -> binary:match(Bin, Sub) =/= nomatch.

-define(has(Sub, Bin), ?assert(has(Sub, Bin))).
-define(hasnt(Sub, Bin), ?assertNot(has(Sub, Bin))).

%%% dropdown_button

-define(MENU, [{draft, <<"Draft">>}, divider, {copy, <<"Copy & paste">>, [{disabled, true}]}]).

dropdown_button_test() ->
    H = r(?M:dropdown_button(<<"Actions">>, ?MENU, [primary, sm, rounded], [{name, act}, {id, d}])),
    ?has(<<"<div class=\"ah-dropdown-btn ah-dropdown-btn-sm ah-dropdown-btn-primary "
           "ah-dropdown-btn-rounded\" data-ah=\"dropdown-button\" data-ah-value=\"\" id=\"d\">">>, H),
    ?has(<<"<button class=\"ah-dropdown-btn-wrapper\" type=\"button\" aria-haspopup=\"menu\" "
           "aria-expanded=\"false\"><div class=\"ah-dropdown-btn-content\">Actions</div>">>, H),
    ?has(<<"<div class=\"ah-dropdown-btn-popup\" role=\"menu\" hidden>">>, H),
    ?has(<<"<button class=\"ah-dropdown-btn-item\" type=\"button\" role=\"menuitem\" "
           "tabindex=\"-1\" data-value=\"draft\"><span>Draft</span></button>">>, H),
    ?has(<<"<div class=\"ah-dropdown-btn-divider\" role=\"separator\"></div>">>, H),
    ?has(<<"data-value=\"copy\" disabled><span>Copy &amp; paste</span>">>, H),
    ?has(<<"<input type=\"hidden\" name=\"act\" value=\"\" data-ah-input>">>, H).

dropdown_button_value_test() ->
    H = r(?M:dropdown_button(<<"A">>, ?MENU, [], [{value, draft}, {auto_open, true}])),
    ?has(<<"data-ah-value=\"draft\"">>, H),
    ?has(<<"ah-dropdown-btn-item selected\"">>, H),
    ?has(<<"ah-dropdown-btn-auto-open">>, H),
    ?hasnt(<<"auto-open=">>, H),
    ?hasnt(<<"auto_open">>, H).

dropdown_button_disabled_test() ->
    H = r(?M:dropdown_button(<<"A">>, ?MENU, [], [{disabled, true}])),
    ?has(<<"ah-dropdown-btn-disabled">>, H),
    ?has(<<"aria-disabled=\"true\"">>, H),
    ?has(<<"aria-expanded=\"false\" disabled>">>, H),
    ?assertError({aihtml, {unknown_modifier, dropdown_button, info, _}},
                 ?M:dropdown_button(<<"A">>, ?MENU, [info], [])).

dropdown_button_escaping_test() ->
    H = r(?M:dropdown_button(<<"<x>">>, [{<<"\"v">>, <<"<y>">>}], [], [])),
    ?has(<<"&lt;x&gt;">>, H),
    ?has(<<"&lt;y&gt;">>, H),
    ?has(<<"data-value=\"&quot;v\"">>, H).

%%% catalog (demos live in aihtml_example)

catalog_test() ->
    Cat = ?M:catalog(),
    Names = [N || #{name := N} <- Cat],
    ?assertEqual([dropdown_button], Names),
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

record_equals_builder_test() ->
    ?assertEqual(r(?M:dropdown_button(<<"A">>, ?MENU, [sm], [{value, draft}, {auto_open, true}])),
                 r(#ah_dropdown_button{body = <<"A">>, items = ?MENU, size = sm,
                                       value = draft, auto_open = true})).

field_validation_test() ->
    %% a group without a default may stay undefined
    ?has(<<"class=\"ah-dropdown-btn\"">>,
         r(#ah_dropdown_button{})).

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

default(ah_dropdown_button) -> #ah_dropdown_button{}.
