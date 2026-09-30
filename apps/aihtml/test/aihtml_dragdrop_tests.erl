-module(aihtml_dragdrop_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_dragdrop.hrl").

-define(M, aihtml_dragdrop).

r(H) -> aihtml_html:render_binary(H).

has(Sub, Bin) -> binary:match(Bin, Sub) =/= nomatch.

-define(has(Sub, Bin), ?assert(has(Sub, Bin))).
-define(hasnt(Sub, Bin), ?assertNot(has(Sub, Bin))).

%%% dragdrop

dragdrop_test() ->
    H = r(?M:ah_dragdrop([aihtml_html:el('div', <<"Task">>, [<<"p-2">>],
                                         ?M:draggable_attrs(t1, #{type => task})),
                          aihtml_html:el('div', <<"Done">>, [],
                                         ?M:drop_zone_attrs(done, #{accept => [task, bug]}))],
                         [<<"flex">>], [{move, true}, {id, dd}])),
    ?has(<<"<div class=\"ah-dragdrop flex\" data-ah=\"dragdrop\" data-ah-tolerance=\"intersect\" "
           "data-ah-move id=\"dd\">">>, H),
    ?has(<<"<div class=\"p-2 ah-draggable\" data-ah-drag=\"t1\" data-ah-drag-type=\"task\" "
           "tabindex=\"0\" aria-roledescription=\"draggable\">Task</div>">>, H),
    ?has(<<"<div class=\"ah-drop-zone\" data-ah-drop=\"done\" data-ah-drop-accept=\"task,bug\">">>, H),
    ?hasnt(<<"data-ah-revert">>, H).

dragdrop_options_test() ->
    H = r(?M:ah_dragdrop([], [], [{tolerance, pointer}, {revert, true}, {disabled, true}])),
    ?has(<<"class=\"ah-dragdrop ah-dragdrop-disabled\"">>, H),
    ?has(<<"data-ah-tolerance=\"pointer\"">>, H),
    ?has(<<"data-ah-revert">>, H),
    ?has(<<"aria-disabled=\"true\"">>, H).

attr_helpers_test() ->
    D = r(aihtml_html:el(span, [], [], ?M:draggable_attrs(<<"k">>, #{disabled => true}))),
    ?has(<<"class=\"ah-draggable ah-draggable-disabled\"">>, D),
    ?has(<<"aria-disabled=\"true\"">>, D),
    ?hasnt(<<"data-ah-drag-type">>, D),
    Z = r(aihtml_html:el(span, [], [], ?M:drop_zone_attrs(3, #{accept => task, disabled => true}))),
    ?has(<<"data-ah-drop=\"3\" data-ah-drop-accept=\"task\" data-ah-drop-disabled=\"true\"">>, Z),
    ?assertEqual([{draggable_attrs, 2}, {drop_zone_attrs, 2}], ?M:facade_extras()).

%%% catalog

catalog_test() ->
    Cat = ?M:catalog(),
    ?assertEqual([dragdrop], [N || #{name := N} <- Cat]),
    [begin
         ?assert(erlang:function_exported(?M, aihtml_catalog:builder(N), length(binary:split(S, <<",">>, [global])))),
         ?assertEqual(layout, C)
     end || #{name := N, signature := S, category := C} <- Cat].

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
    ?assertEqual(r(?M:ah_dragdrop(<<"x">>, [], [{tolerance, fit}, {move, true}, {revert, true}])),
                 r(#ah_dragdrop{body = <<"x">>, tolerance = fit, move = true, revert = true})).

builder_fills_fields_test() ->
    ?assertError({aihtml, {record_only_field, ah_dragdrop, postback}},
                 ?M:ah_dragdrop([], [], [{postback, drop}])).

postback_test() ->
    Token = fun(Html) ->
                    {match, [Ev, Tok]} = re:run(r(Html), <<"data-ah-on=\"([a-z:-]+):([^\":]+\\.[^\":]+)\"">>,
                                                [{capture, all_but_first, binary}]),
                    {ok, Ref} = aihtml_action:unsign(Tok),
                    {Ev, Ref}
            end,
    ?assertEqual({<<"ah:drop">>, {other, dropped, #{}}},
                 Token(#ah_dragdrop{postback = dropped, delegate = other})).

field_validation_test() ->
    ?assertError({aihtml, {bad_option, tolerance, near}}, r(#ah_dragdrop{tolerance = near})),
    ?assertError({aihtml, {bad_option, move, yes}}, r(#ah_dragdrop{move = yes})).

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

default(ah_dragdrop) -> #ah_dragdrop{}.
