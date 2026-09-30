-module(aihtml_tag_input_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_tag_input.hrl").

-export([action/4]).

-define(M, aihtml_tag_input).

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

%%% tag_input

tag_input_test() ->
    H = r(?M:ah_tag_input([<<"a">>, <<"<b>">>], [<<"w-96">>], [{name, tags}, {max_tags, 5}])),
    has(H, <<"<div class=\"ah-tag-input w-96\" data-ah=\"tag-input\" role=\"group\" "
             "data-ah-value=\"a,&lt;b&gt;\" data-disabled=\"false\" data-chip-color=\"primary\" "
             "data-chip-variant=\"soft\" data-max-tags=\"5\">">>),
    has(H, <<"<span class=\"ah-chip ah-tag-input__chip\" data-variant=\"soft\" "
             "data-color=\"primary\" data-size=\"small\" data-index=\"1\">"
             "<span class=\"ah-chip__label\">&lt;b&gt;</span>">>),
    has(H, <<"aria-label=\"Remove &lt;b&gt;\"">>),
    has(H, <<"<input class=\"ah-tag-input__field\" type=\"text\" placeholder=\"Add tag", 226, 128, 166, "\"">>),
    has(H, <<"<input type=\"hidden\" name=\"tags\" value=\"a,&lt;b&gt;\"></div>">>).

tag_input_empty_disabled_test() ->
    H = r(?M:ah_tag_input([], [disabled], [{placeholder, <<"Tags">>}, {chip_color, error},
                                            {allow_duplicates, true}])),
    has(H, <<"data-ah-value=\"\"">>),
    has(H, <<"data-disabled=\"true\"">>),
    has(H, <<"data-chip-color=\"error\"">>),
    has(H, <<"data-allow-duplicates">>),
    has(H, <<"placeholder=\"Tags\" aria-label=\"Tags\" disabled">>),
    hasnt(H, <<"ah-tag-input__chip">>),
    hasnt(H, <<"type=\"hidden\"">>).

tag_input_action_on_root_test() ->
    H = r(?M:ah_tag_input([<<"x">>], [], [{<<"data-ah-on">>, {actions, [{<<"change">>, <<"T">>, #{}}]}}])),
    has(H, <<"data-chip-variant=\"soft\" data-ah-on=\"change:T\">">>).

record_equals_builder_test() ->
    ?assertEqual(r(?M:ah_tag_input([<<"a">>], [<<"w-96">>], [{name, t}, {max_tags, 3},
                                                            {chip_color, error},
                                                            {title, <<"x">>}])),
                 r(#ah_tag_input{value = [<<"a">>], css = [<<"w-96">>], name = t,
                                 max_tags = 3, chip_color = error,
                                 attrs = [{title, <<"x">>}]})).

builder_fills_fields_test() ->
    ?assertMatch(#ah_tag_input{value = [<<"a">>], name = tags, placeholder = <<"P">>,
                               disabled = true, attrs = []},
                 ?M:ah_tag_input([<<"a">>], [disabled], [{name, tags}, {placeholder, <<"P">>}])).

postback_test() ->
    ?assertEqual({<<"change">>, {?MODULE, save, #{k => 1}}},
                 token(#ah_tag_input{postback = {save, #{k => 1}}})),
    %% input_otp and tag_input bind on the root
    ?assert(has(r(#ah_tag_input{postback = save}),
                <<"data-chip-variant=\"soft\" data-ah-on=">>)).

%%% catalog

catalog_test() ->
    Names = [N || #{name := N} <- ?M:catalog()],
    ?assertEqual([tag_input], Names),
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

default(ah_tag_input) -> #ah_tag_input{}.

arity(Sig) ->
    [_, Args] = binary:split(Sig, <<"(">>),
    length(binary:split(Args, <<",">>, [global])).

%% A value containing a comma is escaped in data-ah-value (aihtml_value).
vhas(Sub, Bin) -> binary:match(Bin, Sub) =/= nomatch.

comma_values_test() ->
    H = r(?M:ah_tag_input([<<"1,000">>, <<"x\\y">>, <<"z">>], [], [{name, t}])),
    ?assert(vhas(<<"data-ah-value=\"1\\,000,x\\\\y,z\"">>, H)),
    ?assert(vhas(<<"value=\"1\\,000,x\\\\y,z\"">>, H)).
