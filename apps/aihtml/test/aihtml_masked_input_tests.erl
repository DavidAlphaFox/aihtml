% Tests for aihtml_masked_input.
-module(aihtml_masked_input_tests).
-behaviour(aihtml_action).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_masked_input.hrl").

-export([action/4]).

-define(M, aihtml_masked_input).

-spec action(atom(), term(), map(), term()) -> ok.
action(_, _, _, _) -> ok.

r(Html) -> aihtml_html:render_binary(Html).

has(Needle, Hay) ->
    case binary:match(Hay, Needle) of
        nomatch -> ?debugFmt("~s not in~n~s", [Needle, Hay]), false;
        _ -> true
    end.
%%%===================================================================
%%% masked_input
%%%===================================================================

masked_input_test() ->
    H = r(?M:masked_input(<<"5551234">>, [<<"w-48">>],
                          [{mask, <<"(999) 999-9999">>}, {name, phone}, {id, m},
                           {title, <<"t">>}])),
    ?assert(has(<<"<div class=\"ah-masked-input-group w-48\" data-ah=\"masked-input\" "
                  "data-ah-value=\"5551234\" data-ah-mask=\"(999) 999-9999\" "
                  "data-ah-prompt=\"_\" id=\"m\" title=\"t\">">>, H)),
    ?assert(has(<<"value=\"(555) 123-4___\"">>, H)),
    ?assert(has(<<"inputmode=\"numeric\"">>, H)),
    ?assert(has(<<"<input type=\"hidden\" name=\"phone\" value=\"5551234\">">>, H)).

masked_fill_test() ->
    Show = fun(V, Mask) ->
                   H = r(?M:masked_input(V, [], [{mask, Mask}])),
                   {match, [D]} = re:run(H, <<"<input class=\"ah-masked-input\"[^>]* value=\"([^\"]*)\"">>, [{capture, all_but_first, binary}]),
                   {match, [Val]} = re:run(H, <<"data-ah-value=\"([^\"]*)\"">>,
                                           [{capture, all_but_first, binary}]),
                   {D, Val}
           end,
    %% literals in the value are taken where they stand, misfits skipped
    ?assertEqual({<<"(555) 123-4567">>, <<"5551234567">>}, Show(<<"(555) 123-4567">>, <<"(999) 999-9999">>)),
    ?assertEqual({<<"12/31/2026">>, <<"12312026">>}, Show(<<"12x31y2026">>, <<"99/99/9999">>)),
    ?assertEqual({<<"AB_-____">>, <<"AB">>}, Show(<<"A1B">>, <<"LLL-9999">>)),
    ?assertEqual({<<"1f:__">>, <<"1f">>}, Show(<<"1fG">>, <<"[0-9A-F][0-9A-F]:99">>)),
    ?assertEqual({<<"_____">>, <<>>}, Show(undefined, <<"99999">>)),
    %% no inputmode for a mask with letters
    ?assertNot(has_quiet(<<"inputmode">>, r(?M:masked_input(<<>>, [], [{mask, <<"LL">>}])))).

has_quiet(Needle, Hay) -> binary:match(Hay, Needle) =/= nomatch.

masked_options_test() ->
    H = r(?M:masked_input(<<"41111">>, [floating_label, square, readonly],
                          [{mask, <<"9999 9999">>}, {prompt_char, <<"*">>},
                           {include_literals, true}, {placeholder, <<"Card">>}, {name, c}])),
    ?assert(has(<<"class=\"ah-masked-input-group ah-masked-input-readonly "
                  "ah-masked-input-no-rounded\"">>, H)),
    ?assert(has(<<"data-ah-value=\"4111 1***\"">>, H)),
    ?assert(has(<<"data-ah-literals">>, H)),
    ?assert(has(<<"<label class=\"ah-masked-input-label ah-masked-input-label-float\">Card</label>">>, H)),
    ?assert(has(<<"placeholder=\"\"">>, H)),
    ?assert(has(<<" readonly">>, H)),
    %% include_literals with nothing typed: empty value
    ?assert(has(<<"data-ah-value=\"\"">>,
                r(?M:masked_input(undefined, [], [{include_literals, true}])))),
    D = r(?M:masked_input(<<>>, [disabled], [{placeholder, <<"Zip">>}])),
    ?assert(has(<<"ah-masked-input-disabled">>, D)),
    ?assert(has(<<"aria-label=\"Zip\"">>, D)),
    ?assertError({aihtml, {bad_option, prompt_char, <<"__">>}},
                 r(?M:masked_input(<<>>, [], [{prompt_char, <<"__">>}]))),
    ?assertError({aihtml, {bad_option, mask, _}}, r(?M:masked_input(<<>>, [], [{mask, <<"[0-9">>}]))).

%%%===================================================================
%%% catalog
%%%===================================================================

catalog_test() ->
    Names = [N || #{name := N} <- ?M:catalog()],
    ?assertEqual([masked_input], Names),
    [begin
         Arity = length(binary:matches(maps:get(signature, E), <<",">>)) + 1,
         ?assert(erlang:function_exported(?M, N, Arity)),
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
    ?assertEqual(r(?M:masked_input(<<"12">>, [square, <<"w-40">>],
                                   [{id, m}, {name, n}, {mask, <<"99-99">>},
                                    {prompt_char, <<"#">>}, {title, <<"t">>}])),
                 r(#ah_masked_input{value = <<"12">>, square = true, css = [<<"w-40">>],
                                    id = m, name = n, mask = <<"99-99">>,
                                    prompt_char = <<"#">>, attrs = [{title, <<"t">>}]})).

postback_test() ->
    Token = fun(Html) ->
                    {match, [T]} = re:run(r(Html), <<"data-ah-on=\"([a-z]+:[^\"]+)\"">>,
                                          [{capture, all_but_first, binary}]),
                    [Ev, Tok] = binary:split(T, <<":">>),
                    {ok, Ref} = aihtml_action:unsign(Tok),
                    {Ev, Ref}
            end,
    ?assertEqual({<<"change">>, {?MODULE, typed, #{}}},
                 Token(#ah_masked_input{postback = typed})).

field_validation_test() ->
    ?assertError({aihtml, {bad_flag, masked_input, square, yes}},
                 r(#ah_masked_input{square = yes})).

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

default(ah_masked_input) -> #ah_masked_input{}.
