-module(aihtml_textarea_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_textarea.hrl").

-export([action/4]).

-define(M, aihtml_textarea).

r(E) -> aihtml_html:render_binary(E).

has(Html, Part) ->
    case binary:match(Html, Part) of
        nomatch -> erlang:error({missing, Part, Html});
        _ -> true
    end.

%%% textarea

textarea_test() ->
    H = r(?M:ah_textarea(<<"a</textarea><b>">>, [sm, invalid], [{name, notes}, {rows, 5}])),
    has(H, <<"class=\"ah-input-group ah-textarea-group ah-input-sm ah-input-invalid\"">>),
    has(H, <<"<textarea class=\"ah-input ah-textarea\" rows=\"5\" aria-invalid=\"true\" "
             "name=\"notes\">a&lt;/textarea&gt;&lt;b&gt;</textarea>">>),
    ?assertError({aihtml, {unknown_modifier, textarea, clearable, _}},
                 ?M:ah_textarea(undefined, [clearable], [])).

record_equals_builder_test() ->
    ?assertEqual(r(?M:ah_textarea(<<"t">>, [sm, no_rounded], [{rows, 5}])),
                 r(#ah_textarea{value = <<"t">>, size = sm, no_rounded = true,
                                attrs = [{rows, 5}]})).

postback_test() ->
    ?assertEqual({<<"change">>, {?MODULE, save, #{k => 1}}},
                 token(#ah_textarea{postback = {save, #{k => 1}}})).

field_validation_test() ->
    ?assertError({aihtml, {bad_modifier, textarea, state, sm, _}}, r(#ah_textarea{state = sm})).

%%% catalog

catalog_test() ->
    Names = [N || #{name := N} <- ?M:catalog()],
    ?assertEqual([textarea], Names),
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

default(ah_textarea) -> #ah_textarea{}.

arity(Sig) ->
    [_, Args] = binary:split(Sig, <<"(">>),
    length(binary:split(Args, <<",">>, [global])).
