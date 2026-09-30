-module(aihtml_password_input_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_password_input.hrl").

-export([action/4]).

-define(M, aihtml_password_input).

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

%%% password_input

password_test() ->
    H = r(?M:ah_password_input(<<"p&w">>, [lg], [{name, pw}])),
    has(H, <<"class=\"ah-pwd-group ah-pwd-lg\" data-ah=\"password-input\"">>),
    has(H, <<"<input class=\"ah-pwd\" type=\"password\" value=\"p&amp;w\"">>),
    has(H, <<"name=\"pw\"">>),
    has(H, <<"<button class=\"ah-pwd-toggle\" type=\"button\" tabindex=\"-1\" "
             "aria-label=\"Show password\" aria-pressed=\"false\"">>),
    has(H, <<"ah-pwd-icon-show">>),
    has(H, <<"ah-pwd-icon-hide">>),
    hasnt(H, <<"ah-pwd-strength">>).

password_options_test() ->
    H = r(?M:ah_password_input(undefined, [disabled], [{toggle, false}, {strength, true}])),
    hasnt(H, <<"ah-pwd-toggle">>),
    has(H, <<"<div class=\"ah-pwd-strength\"><div class=\"ah-pwd-strength-bar\">"
             "<div class=\"ah-pwd-strength-fill\"></div></div>">>),
    has(H, <<"ah-pwd-disabled">>),
    hasnt(H, <<"toggle=">>),
    ?assertError({aihtml, {unknown_modifier, password_input, readonly, _}},
                 ?M:ah_password_input(undefined, [readonly], [])).

record_equals_builder_test() ->
    ?assertEqual(r(?M:ah_password_input(undefined, [valid], [{toggle, false}, {strength, true}])),
                 r(#ah_password_input{state = valid, toggle = false, strength = true})).

postback_test() ->
    ?assertEqual({<<"change">>, {?MODULE, save, #{k => 1}}},
                 token(#ah_password_input{postback = {save, #{k => 1}}})).

field_validation_test() ->
    ?assertError({aihtml, {modifier_in_css, password_input, sm}},
                 r(#ah_password_input{css = [sm]})).

%%% catalog

catalog_test() ->
    Names = [N || #{name := N} <- ?M:catalog()],
    ?assertEqual([password_input], Names),
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

default(ah_password_input) -> #ah_password_input{}.

arity(Sig) ->
    [_, Args] = binary:split(Sig, <<"(">>),
    length(binary:split(Args, <<",">>, [global])).
