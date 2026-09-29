-module(aihtml_form_layout_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_form_layout.hrl").

-export([action/4]).

-define(M, aihtml_form_layout).

r(Html) -> aihtml_html:render_binary(Html).

has(Bin, Sub) -> binary:match(Bin, Sub) =/= nomatch.

token(Html) ->
    {match, [T]} = re:run(aihtml_html:render_binary(Html), <<"data-ah-on=\"([a-z]+:[^\"]+)\"">>,
                          [{capture, all_but_first, binary}]),
    [Ev, Tok] = binary:split(T, <<":">>),
    {ok, Ref} = aihtml_action:unsign(Tok),
    {Ev, Ref}.

-spec action(atom(), term(), map(), term()) -> ok.
action(_, _, _, _) -> ok.

form_layout_test() ->
    Fields = [{text, <<"Intro">>},
              {<<"A">>, <<"ctl-a">>},
              #{label => <<"B">>, key => b, control => fun(V) -> [<<"val=">>, V] end},
              {columns, [{<<"C">>, <<"c">>}, {<<"D">>, <<"d">>}]},
              blank,
              #{label => <<"H">>, control => <<"h">>, hidden => true, label_position => top}],
    H = r(?M:form_layout(Fields, #{b => <<"bee">>}, [bordered, bg],
                         [{label_width, 100}, {id, f}])),
    ?assert(has(H, <<"<form class=\"ah-form ah-form-bg ah-form-bordered\" style=\"padding:10px\" id=\"f\">">>)),
    ?assert(has(H, <<"ah-form-label-text\">Intro">>)),
    ?assert(has(H, <<"val=bee">>)),
    ?assert(has(H, <<"data-ah-key=\"b\"">>)),
    ?assert(has(H, <<"ah-form-row ah-form-columns">>)),
    ?assert(has(H, <<"<div class=\"ah-form-col\">">>)),
    ?assert(has(H, <<"ah-form-row-blank\" style=\"height:16px\"">>)),
    ?assert(has(H, <<"ah-form-row ah-form-row-top\" hidden>">>)),
    ?assert(has(H, <<"width:100px">>)).

form_layout_div_padding_test() ->
    H = r(?M:form_layout([], #{}, [], [{tag, 'div'}, {padding, {1, 2, 3, 4}},
                                       {label_position, top}])),
    ?assert(has(H, <<"<div class=\"ah-form\" style=\"padding:1px 2px 3px 4px\">">>)).

form_layout_bad_field_test() ->
    ?assertError({aihtml, {bad_form_field, 42}}, r(?M:form_layout([42], #{}, [], []))).

%%% element records (designs/05-records.md)

record_equals_builder_test() ->
    Fields = [{<<"A">>, <<"a">>}, #{label => <<"B">>, key => b, control => fun(V) -> V end}],
    ?assertEqual(r(?M:form_layout(Fields, #{b => <<"bee">>}, [bordered],
                                  [{label_width, 80}, {padding, 4}, {action, <<"#x">>}])),
                 r(#ah_form_layout{fields = Fields, values = #{b => <<"bee">>}, bordered = true,
                                   label_width = 80, padding = 4,
                                   attrs = [{action, <<"#x">>}]})).

builder_fills_fields_test() ->
    ?assertMatch(#ah_form_layout{fields = [], tag = 'div', label_position = top, bg = true},
                 ?M:form_layout([], #{}, [bg], [{tag, 'div'}, {label_position, top}])).

postback_test() ->
    ?assertEqual({<<"submit">>, {?MODULE, save, #{id => 7}}},
                 token(#ah_form_layout{postback = {save, #{id => 7}}})),
    ?assertError({aihtml, {no_postback_event, ah_form_layout}},
                 r(#ah_form_layout{tag = 'div', postback = x})).

field_validation_test() ->
    ?assertError({aihtml, {bad_flag, form_layout, bordered, 1}},
                 r(#ah_form_layout{bordered = 1})).

%%% catalog

catalog_test() ->
    Names = [N || #{name := N} <- ?M:catalog()],
    ?assertEqual([form_layout], Names),
    [begin
         ?assert(is_binary(maps:get(signature, E))),
         ?assert(erlang:function_exported(?M, N, 4))
     end || #{name := N} = E <- ?M:catalog()].

catalog_docs_test() ->
    [begin
         Docs = maps:get(option_docs, E),
         [?assert(maps:is_key(O, Docs)) || O <- maps:get(options, E, [])],
         ?assert(is_list(maps:get(methods, E)))
     end || E <- ?M:catalog()].

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

default(ah_form_layout) -> #ah_form_layout{}.
