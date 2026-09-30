% Tests for aihtml_repeat_button.
-module(aihtml_repeat_button_tests).
-behaviour(aihtml_action).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_repeat_button.hrl").

-export([action/4]).

-define(M, aihtml_repeat_button).

-spec action(atom(), term(), map(), term()) -> ok.
action(_, _, _, _) -> ok.

r(Html) -> aihtml_html:render_binary(Html).

has(Needle, Hay) ->
    case binary:match(Hay, Needle) of
        nomatch -> ?debugFmt("~s not in~n~s", [Needle, Hay]), false;
        _ -> true
    end.
%%%===================================================================
%%% repeat_button
%%%===================================================================

repeat_button_test() ->
    H = r(?M:ah_repeat_button(<<"+">>, 1, [secondary, sm, <<"px-2">>],
                              [{id, plus}, {interval, 100}, {title, <<"More">>}])),
    ?assertEqual(<<"<button class=\"ah-btn ah-btn-sm ah-btn-secondary px-2\" type=\"button\" "
                   "value=\"1\" id=\"plus\" data-ah=\"repeat-button\" data-ah-delay=\"300\" "
                   "data-ah-interval=\"100\" title=\"More\">+</button>">>, H),
    %% the same markup as ah_button/4 apart from the behaviour's attributes
    B = r(aihtml_button:ah_button(<<"+">>, 1, [secondary, sm, <<"px-2">>],
                                  [{id, plus}, {title, <<"More">>}])),
    ?assertEqual(B, re:replace(H, <<" data-ah=\"repeat-button\" data-ah-delay=\"300\" "
                                    "data-ah-interval=\"100\"">>, <<>>, [{return, binary}])),
    D = r(?M:ah_repeat_button(<<"x">>, undefined, [round], [{disabled, true}, {icon, <<"*">>}])),
    ?assert(has(<<"ah-btn-round">>, D)),
    ?assert(has(<<"ah-btn-disabled">>, D)),
    ?assert(has(<<" disabled">>, D)),
    ?assert(has(<<"ah-btn-img">>, D)),
    ?assertError({aihtml, {bad_option, interval, 0}},
                 r(?M:ah_repeat_button(<<"x">>, undefined, [], [{interval, 0}]))),
    ?assertError({aihtml, {unknown_modifier, repeat_button, huge, _}},
                 ?M:ah_repeat_button(<<"x">>, undefined, [huge], [])).

%%%===================================================================
%%% catalog
%%%===================================================================

catalog_test() ->
    Names = [N || #{name := N} <- ?M:catalog()],
    ?assertEqual([repeat_button], Names),
    [begin
         Arity = length(binary:matches(maps:get(signature, E), <<",">>)) + 1,
         ?assert(erlang:function_exported(?M, aihtml_catalog:builder(N), Arity)),
         ?assertEqual(form, maps:get(category, E))
     end || #{name := N} = E <- ?M:catalog()].

catalog_docs_test() ->
    [begin
         #{flags := Fl, options := Op, option_docs := Docs, methods := Ms} =
             aihtml_catalog:entry(?M, N),
         ?assertEqual(lists:sort(Fl ++ Op), lists:sort(maps:keys(Docs))),
         [?assert(is_binary(A) andalso is_binary(Doc)) || #{args := A, doc := Doc} <- Ms]
     end || #{name := N} <- ?M:catalog()].

%%%===================================================================
%%% element records (designs/05-records.md)
%%%===================================================================

record_equals_builder_test() ->
    ?assertEqual(r(?M:ah_repeat_button(<<"+">>, up, [warning, lg], [{delay, 100}])),
                 r(#ah_repeat_button{body = <<"+">>, value = up, variant = warning, size = lg,
                                     delay = 100})).

builder_fills_fields_test() ->
    ?assertMatch(#ah_repeat_button{variant = success, round = true, interval = 20, css = [<<"x">>]},
                 ?M:ah_repeat_button(<<"a">>, undefined, [success, round, <<"x">>], [{interval, 20}])).

postback_test() ->
    Token = fun(Html) ->
                    {match, [T]} = re:run(r(Html), <<"data-ah-on=\"([a-z]+:[^\"]+)\"">>,
                                          [{capture, all_but_first, binary}]),
                    [Ev, Tok] = binary:split(T, <<":">>),
                    {ok, Ref} = aihtml_action:unsign(Tok),
                    {Ev, Ref}
            end,
    ?assertEqual({<<"click">>, {?MODULE, step, 1}},
                 Token(#ah_repeat_button{body = <<"+">>, postback = {step, 1}})).

field_validation_test() ->
    ?assertError({aihtml, {bad_modifier, repeat_button, variant, loud, _}},
                 r(#ah_repeat_button{variant = loud})),
    ?assertError({aihtml, {bad_option, delay, -1}}, r(#ah_repeat_button{delay = -1})).

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

default(ah_repeat_button) -> #ah_repeat_button{}.
