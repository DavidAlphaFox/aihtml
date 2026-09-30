-module(aihtml_number_input_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_number_input.hrl").

-export([action/4]).

-define(M, aihtml_number_input).

r(E) -> aihtml_html:render_binary(E).

has(Html, Part) ->
    case binary:match(Html, Part) of
        nomatch -> erlang:error({missing, Part, Html});
        _ -> true
    end.

hasnt(Html, Part) ->
    case binary:match(Html, Part) of
        nomatch -> true;
        _ -> erlang:error({unexpected, Part, Html})
    end.

%%% number_input

number_basic_test() ->
    H = r(?M:ah_number_input(5, [], [{min, 0}, {max, 10}, {name, qty}])),
    has(H, <<"data-ah=\"number-input\" data-min=\"0\" data-max=\"10\" data-step=\"1\" "
             "data-decimals=\"0\"">>),
    has(H, <<"role=\"spinbutton\"">>),
    has(H, <<"value=\"5\"">>),
    has(H, <<"aria-valuenow=\"5\"">>),
    has(H, <<"name=\"qty\"">>),
    has(H, <<"<div class=\"ah-numinput-spin\"><span class=\"ah-numinput-spin-up\"">>).

number_clamp_and_decimals_test() ->
    has(r(?M:ah_number_input(50, [], [{max, 10}])), <<"value=\"10\"">>),
    has(r(?M:ah_number_input(-3, [], [{min, 0}])), <<"value=\"0\"">>),
    has(r(?M:ah_number_input(<<"2.5">>, [], [{step, 0.25}])), <<"value=\"2.50\"">>),
    has(r(?M:ah_number_input(3, [], [{decimals, 1}])), <<"value=\"3.0\"">>),
    has(r(?M:ah_number_input(undefined, [], [])), <<"type=\"text\"">>),
    has(r(?M:ah_number_input(undefined, [], [])), <<"value=\"\"">>),
    has(r(?M:ah_number_input(undefined, [], [{allow_null, false}, {min, 2}])), <<"value=\"2\"">>),
    ?assertError({aihtml, {bad_number, <<"abc">>}}, r(?M:ah_number_input(<<"abc">>, [], []))),
    ?assertError({aihtml, {bad_option, number_input, step, 0}},
                 r(?M:ah_number_input(1, [], [{step, 0}]))).

number_symbol_spin_test() ->
    H = r(?M:ah_number_input(1, [readonly, sm], [{symbol, <<"<$>">>}, {spin, false}])),
    has(H, <<"<span class=\"ah-numinput-prefix\">&lt;$&gt;</span><input">>),
    hasnt(H, <<"ah-numinput-spin">>),
    has(H, <<"ah-numinput-group ah-numinput-sm ah-numinput-readonly">>),
    has(H, <<" readonly">>),
    H2 = r(?M:ah_number_input(1, [], [{symbol, <<"%">>}, {symbol_position, right}])),
    has(H2, <<"</div><span class=\"ah-numinput-suffix\">%</span></div>">>).

record_equals_builder_test() ->
    ?assertEqual(r(?M:ah_number_input(<<"2.5">>, [readonly], [{step, 0.5}, {min, 0}, {max, 9},
                                                              {symbol, <<"%">>},
                                                              {symbol_position, right},
                                                              {name, n}])),
                 r(#ah_number_input{value = <<"2.5">>, readonly = true, step = 0.5, min = 0,
                                    max = 9, symbol = <<"%">>, symbol_position = right,
                                    attrs = [{name, n}]})).

builder_fills_fields_test() ->
    ?assertMatch(#ah_number_input{min = 0, step = 1, spin = false, allow_null = false},
                 ?M:ah_number_input(1, [], [{min, 0}, {spin, false}, {allow_null, false}])).

postback_test() ->
    ?assertEqual({<<"change">>, {?MODULE, save, #{k => 1}}},
                 token(#ah_number_input{postback = {save, #{k => 1}}})).

field_validation_test() ->
    ?assertError({aihtml, {bad_option, number_input, min, x}}, r(#ah_number_input{min = x})),
    ?assertError({aihtml, {bad_option, number_input, decimals, -1}},
                 r(#ah_number_input{decimals = -1})),
    ?assertError({aihtml, {bad_number, <<"z">>}}, r(#ah_number_input{value = <<"z">>})).

%%% catalog

catalog_test() ->
    Names = [N || #{name := N} <- ?M:catalog()],
    ?assertEqual([number_input], Names),
    [begin
         ?assert(erlang:function_exported(?M, aihtml_catalog:builder(N), arity(S))),
         ?assertEqual(form, C)
     end || #{name := N, signature := S, category := C} <- ?M:catalog()].

catalog_docs_test() ->
    [begin
         Documented = maps:keys(maps:get(option_docs, E)),
         Keys = maps:get(options, E, []) ++ maps:get(flags, E, [])
             ++ lists:append([Ms || {Ms, _} <- maps:values(maps:get(groups, E, #{}))]),
         ?assertEqual([], Keys -- Documented),
         ?assert(is_list(maps:get(methods, E)))
     end || E <- ?M:catalog()].

%%% element records (designs/05-records.md)

-spec action(atom(), term(), map(), term()) -> ok.
action(_, _, _, _) -> ok.

token(Html) ->
    {match, [T]} = re:run(r(Html), <<"data-ah-on=\"([a-z]+:[^\"]+)\"">>,
                          [{capture, all_but_first, binary}]),
    [Ev, Tok] = binary:split(T, <<":">>),
    {ok, Ref} = aihtml_action:unsign(Tok),
    {Ev, Ref}.

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

default(ah_number_input) -> #ah_number_input{}.

arity(Sig) ->
    [_, Args] = binary:split(Sig, <<"(">>),
    length(binary:split(Args, <<",">>, [global])).
