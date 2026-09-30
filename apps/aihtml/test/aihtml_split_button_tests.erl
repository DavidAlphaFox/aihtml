-module(aihtml_split_button_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_split_button.hrl").

-define(M, aihtml_split_button).

r(H) -> aihtml_html:render_binary(H).

has(Sub, Bin) -> binary:match(Bin, Sub) =/= nomatch.

-define(has(Sub, Bin), ?assert(has(Sub, Bin))).
-define(hasnt(Sub, Bin), ?assertNot(has(Sub, Bin))).

%%% split_button

split_button_test() ->
    H = r(?M:ah_split_button(<<"Save">>, [{a, <<"A">>, [{icon, <<"+">>}]}, divider,
                                           {b, <<"B">>, [{disabled, true}]}],
                             [success, lg], [{menu_align, start}, {id, s}])),
    ?has(<<"<div class=\"ah-split-button\" data-variant=\"success\" data-size=\"lg\" "
           "data-disabled=\"false\" data-menu-align=\"start\" data-open=\"false\" "
           "data-ah=\"split-button\" data-ah-value=\"\" id=\"s\">">>, H),
    ?has(<<"<button class=\"ah-split-button__main ah-btn ah-btn-success\" type=\"button\">Save</button>">>, H),
    ?has(<<"class=\"ah-split-button__arrow ah-btn ah-btn-success\" type=\"button\" "
           "aria-haspopup=\"menu\" aria-expanded=\"false\" aria-label=\"Open menu\">">>, H),
    ?has(<<"<div class=\"ah-split-button__menu\" role=\"menu\">">>, H),
    ?has(<<"data-value=\"a\" data-disabled=\"false\"><span class=\"ah-split-button__item-icon\" "
           "aria-hidden=\"true\">+</span><span>A</span>">>, H),
    ?hasnt(<<"icon=">>, H),
    ?has(<<"data-value=\"b\" data-disabled=\"true\" disabled>">>, H),
    ?has(<<"<div class=\"ah-split-button__divider\" role=\"separator\"></div>">>, H).

split_button_defaults_test() ->
    H = r(?M:ah_split_button(<<"Go">>, [], [], [{name, n}, {value, x}])),
    ?has(<<"data-variant=\"primary\" data-size=\"md\"">>, H),
    ?has(<<"data-menu-align=\"end\"">>, H),
    ?has(<<"data-ah-value=\"x\"">>, H),
    ?has(<<"<input type=\"hidden\" name=\"n\" value=\"x\" data-ah-input>">>, H).

split_button_disabled_test() ->
    H = r(?M:ah_split_button(<<"Go">>, [], [], [{disabled, true}])),
    ?has(<<"data-disabled=\"true\"">>, H),
    ?assertEqual(2, length(binary:matches(H, <<" disabled">>))),
    ?assertError({aihtml, {bad_option, menu_align, middle}},
                 r(?M:ah_split_button(<<"Go">>, [], [], [{menu_align, middle}]))),
    ?assertError({aihtml, {conflicting_modifiers, split_button, variant, _}},
                 ?M:ah_split_button(<<"Go">>, [], [primary, error], [])).

%%% catalog (demos live in aihtml_example)

catalog_test() ->
    Cat = ?M:catalog(),
    Names = [N || #{name := N} <- Cat],
    ?assertEqual([split_button], Names),
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

postback_test() ->
    Token = fun(Html) ->
                    {match, [T]} = re:run(r(Html), <<"data-ah-on=\"([a-z]+:[^\"]+)\"">>,
                                          [{capture, all_but_first, binary}]),
                    [Ev, Tok] = binary:split(T, <<":">>),
                    {ok, Ref} = aihtml_action:unsign(Tok),
                    {Ev, Ref}
            end,
    ?assertEqual({<<"click">>, {other_mod, go, 1}},
                 Token(#ah_split_button{body = <<"Go">>, postback = {go, 1},
                                        delegate = other_mod})).

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

default(ah_split_button) -> #ah_split_button{}.
