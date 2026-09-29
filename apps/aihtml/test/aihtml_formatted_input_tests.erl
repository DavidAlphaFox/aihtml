% Tests for aihtml_formatted_input.
-module(aihtml_formatted_input_tests).
-behaviour(aihtml_action).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_formatted_input.hrl").

-export([action/4]).

-define(M, aihtml_formatted_input).

-spec action(atom(), term(), map(), term()) -> ok.
action(_, _, _, _) -> ok.

r(Html) -> aihtml_html:render_binary(Html).

has(Needle, Hay) ->
    case binary:match(Hay, Needle) of
        nomatch -> ?debugFmt("~s not in~n~s", [Needle, Hay]), false;
        _ -> true
    end.
%%%===================================================================
%%% formatted_input
%%%===================================================================

formatted_input_test() ->
    H = r(?M:formatted_input(255, [], [{id, f}, {radix, 16}, {min, 0}, {max, <<"1000">>},
                                       {name, n}, {upper_case, true}])),
    ?assert(has(<<"<div class=\"ah-fmt-input-group\" id=\"f\" data-ah=\"formatted-input\" "
                  "data-ah-value=\"255\" data-ah-radix=\"16\" data-ah-min=\"0\" "
                  "data-ah-max=\"1000\" data-ah-step=\"1\" data-ah-upper>">>, H)),
    ?assert(has(<<"value=\"FF\" role=\"spinbutton\" aria-valuenow=\"255\"">>, H)),
    ?assert(has(<<"<span class=\"ah-fmt-spin-up\">">>, H)),
    ?assert(has(<<"aria-controls=\"f-radix\"">>, H)),
    ?assert(has(<<"class=\"ah-fmt-popup-item ah-fmt-popup-item-active\" role=\"option\" "
                  "id=\"f-radix-16\" aria-selected=\"true\" data-radix=\"16\"">>, H)),
    ?assert(has(<<"<input type=\"hidden\" name=\"n\" value=\"255\">">>, H)).

formatted_values_test() ->
    Shown = fun(V, Attrs) ->
                    H = r(?M:formatted_input(V, [], Attrs)),
                    {match, [D, Dec]} = re:run(H, <<"class=\"ah-fmt-input\"[^>]* value=\"([^\"]*)\" "
                                                     "role=\"spinbutton\" aria-valuenow=\"([^\"]*)\"">>,
                                               [{capture, all_but_first, binary}]),
                    {D, Dec}
            end,
    ?assertEqual({<<"-ff">>, <<"-255">>}, Shown(-255, [{radix, 16}])),
    ?assertEqual({<<"101">>, <<"5">>}, Shown(<<"5">>, [{radix, 2}])),
    ?assertEqual({<<"17">>, <<"15">>}, Shown("15", [{radix, 8}])),
    Big = 1 bsl 70,
    ?assertEqual({integer_to_binary(Big), integer_to_binary(Big)}, Shown(Big, [])),
    ?assertEqual({<<"1.23456e+5">>, <<"123456">>}, Shown(123456, [{notation, exponential}])),
    ?assertEqual({<<"-7">>, <<"-7">>}, Shown(-7, [{notation, exponential}])),
    %% clamped to min / max
    ?assertEqual({<<"10">>, <<"10">>}, Shown(3, [{min, 10}])),
    ?assertEqual({<<"20">>, <<"20">>}, Shown(99, [{max, 20}])),
    %% no spin buttons, no menu
    P = r(?M:formatted_input(0, [disabled], [{spin_buttons, false}, {drop_down, false},
                                             {placeholder, <<"n">>}, {drop_down_width, 90}])),
    ?assertNot(has_quiet(<<"ah-fmt-spin">>, P)),
    ?assertNot(has_quiet(<<"ah-fmt-popup">>, P)),
    ?assert(has(<<"ah-fmt-input-group ah-fmt-input-disabled">>, P)),
    ?assert(has(<<"style=\"width:120px\"">>,
                r(?M:formatted_input(0, [], [{drop_down_width, 120}])))),
    ?assertError({aihtml, {bad_option, radix, 3}}, r(?M:formatted_input(1, [], [{radix, 3}]))),
    ?assertError({aihtml, {bad_option, value, <<"x">>}}, r(?M:formatted_input(<<"x">>, [], []))),
    ?assertError({aihtml, {bad_option, notation, sci}},
                 r(?M:formatted_input(1, [], [{notation, sci}]))).

has_quiet(Needle, Hay) -> binary:match(Hay, Needle) =/= nomatch.

%%%===================================================================
%%% catalog
%%%===================================================================

catalog_test() ->
    Names = [N || #{name := N} <- ?M:catalog()],
    ?assertEqual([formatted_input], Names),
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
    ?assertEqual(r(?M:formatted_input(7, [disabled], [{id, f}, {radix, 2}, {max, 9}])),
                 r(#ah_formatted_input{value = 7, disabled = true, id = f, radix = 2, max = 9})).

builder_fills_fields_test() ->
    ?assertMatch(#ah_formatted_input{value = 1, radix = 16, spin_buttons = false,
                                     attrs = [{title, <<"t">>}]},
                 ?M:formatted_input(1, [], [{radix, 16}, {spin_buttons, false},
                                            {title, <<"t">>}])).

postback_test() ->
    Token = fun(Html) ->
                    {match, [T]} = re:run(r(Html), <<"data-ah-on=\"([a-z]+:[^\"]+)\"">>,
                                          [{capture, all_but_first, binary}]),
                    [Ev, Tok] = binary:split(T, <<":">>),
                    {ok, Ref} = aihtml_action:unsign(Tok),
                    {Ev, Ref}
            end,
    ?assertEqual({<<"change">>, {?MODULE, set, #{id => 1}}},
                 Token(#ah_formatted_input{postback = {set, #{id => 1}}})).

field_validation_test() ->
    ?assertError({aihtml, {bad_option, min, x}}, r(#ah_formatted_input{min = x})).

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

default(ah_formatted_input) -> #ah_formatted_input{}.
