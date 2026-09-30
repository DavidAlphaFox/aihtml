-module(aihtml_input_otp_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_input_otp.hrl").

-export([action/4]).

-define(M, aihtml_input_otp).

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

%%% input_otp

otp_test() ->
    H = r(?M:ah_input_otp(4, <<"12a3">>, [], [{name, code}, {id, otp}])),
    has(H, <<"<div class=\"ah-input-otp\" data-ah=\"input-otp\" role=\"group\" "
             "data-ah-value=\"123\" data-length=\"4\" data-pattern=\"digit\" "
             "data-disabled=\"false\" data-complete=\"false\" id=\"otp\">">>),
    ?assertEqual(4, count(H, <<"class=\"ah-input-otp__slot\"">>)),
    has(H, <<"data-index=\"2\" data-filled=\"true\" value=\"3\"">>),
    has(H, <<"data-index=\"3\" data-filled=\"false\" value=\"\"">>),
    has(H, <<"autocomplete=\"one-time-code\"">>),
    has(H, <<"<input type=\"hidden\" name=\"code\" value=\"123\"></div>">>),
    %% name goes only to the hidden input
    ?assertEqual(1, count(H, <<"name=">>)).

otp_options_test() ->
    H = r(?M:ah_input_otp(6, <<"123456789">>, [disabled],
                          [{separator_at, 3}, {pattern, alphanumeric}])),
    has(H, <<"data-ah-value=\"123456\"">>),
    has(H, <<"data-complete=\"true\"">>),
    has(H, <<"data-disabled=\"true\"">>),
    ?assertEqual(1, count(H, <<"ah-input-otp__separator">>)),
    has(r(?M:ah_input_otp(4, <<"aB1">>, [], [{pattern, alphanumeric}])), <<"data-ah-value=\"aB1\"">>),
    hasnt(H, <<"separator-at=">>),
    ?assertError({aihtml, {bad_length, input_otp, 0}}, ?M:ah_input_otp(0, undefined, [], [])),
    ?assertError({aihtml, {unknown_modifier, input_otp, sm, _}},
                 ?M:ah_input_otp(4, undefined, [sm], [])).

record_equals_builder_test() ->
    ?assertEqual(r(?M:ah_input_otp(4, <<"a1">>, [disabled], [{pattern, alphanumeric},
                                                            {separator_at, 2}, {name, c}])),
                 r(#ah_input_otp{length = 4, value = <<"a1">>, disabled = true,
                                 pattern = alphanumeric, separator_at = 2, name = c})).

postback_test() ->
    ?assertEqual({<<"change">>, {?MODULE, save, #{k => 1}}},
                 token(#ah_input_otp{postback = {save, #{k => 1}}})).

field_validation_test() ->
    ?assertError({aihtml, {bad_length, input_otp, 0}}, r(#ah_input_otp{length = 0})),
    ?assertError({aihtml, {bad_option, input_otp, pattern, hex}},
                 r(#ah_input_otp{pattern = hex})).

%%% catalog

catalog_test() ->
    Names = [N || #{name := N} <- ?M:catalog()],
    ?assertEqual([input_otp], Names),
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

default(ah_input_otp) -> #ah_input_otp{}.

arity(Sig) ->
    [_, Args] = binary:split(Sig, <<"(">>),
    length(binary:split(Args, <<",">>, [global])).

count(H, P) -> length(binary:matches(H, P)).
