-module(aihtml_field_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_field.hrl").
%% field_can_wrap_records_test/0 puts other components' records in rows
-include_lib("aihtml/include/aihtml_dropdownlist.hrl").
-include_lib("aihtml/include/aihtml_slider.hrl").

-define(M, aihtml_field).

r(Html) -> aihtml_html:render_binary(Html).

has(Bin, Sub) -> binary:match(Bin, Sub) =/= nomatch.

field_test() ->
    H = r(?M:field(<<"Name">>, aihtml_html:void(input, [<<"ah-input">>], [{id, n}]), [top],
                   [{for, n}, {required, true}, {help, <<"Help">>}, {id, row}])),
    ?assert(has(H, <<"<div class=\"ah-form-row ah-form-row-top\" id=\"row\">">>)),
    ?assert(has(H, <<"<label class=\"ah-form-label\" for=\"n\">">>)),
    ?assert(has(H, <<"ah-form-required">>)),
    ?assert(has(H, <<"<div class=\"ah-form-help\">Help</div>">>)),
    ?assert(has(H, <<"ah-form-field">>)).

field_error_test() ->
    H = r(?M:field(<<"E">>, <<"x">>, [], [{error, <<"Bad">>}, {label_width, 90}])),
    ?assert(has(H, <<"ah-form-row ah-form-row-invalid">>)),
    ?assert(has(H, <<"ah-form-error ah-validator-error-label\" role=\"alert\">Bad">>)),
    ?assert(has(H, <<"width:90px;min-width:90px">>)).

%%% validate

validate_test() ->
    A = aihtml_html:attrs(?M:validate([required, {min_length, 3}, {email, <<"Mail!">>},
                                       {{range, 1, 9}, <<"1-9">>}, {pattern, <<"[a-z]+">>},
                                       {hint, label}, {on, [blur, input]}])),
    {_, Json} = lists:keyfind(<<"data-ah-validate">>, 1, A),
    Rules = json:decode(Json),
    ?assertEqual([#{<<"rule">> => <<"required">>},
                  #{<<"rule">> => <<"min_length">>, <<"args">> => [3]},
                  #{<<"rule">> => <<"email">>, <<"msg">> => <<"Mail!">>},
                  #{<<"rule">> => <<"range">>, <<"args">> => [1, 9], <<"msg">> => <<"1-9">>},
                  #{<<"rule">> => <<"pattern">>, <<"args">> => [<<"[a-z]+">>]}], Rules),
    ?assertEqual({<<"data-ah-validate-on">>, <<"blur input">>},
                 lists:keyfind(<<"data-ah-validate-on">>, 1, A)),
    ?assertEqual({<<"data-ah-validate-hint">>, <<"label">>},
                 lists:keyfind(<<"data-ah-validate-hint">>, 1, A)),
    ?assertEqual({<<"aria-required">>, <<"true">>}, lists:keyfind(<<"aria-required">>, 1, A)).

validate_no_required_test() ->
    A = aihtml_html:attrs(?M:validate([email])),
    ?assertEqual(false, lists:keyfind(<<"aria-required">>, 1, A)),
    ?assertEqual(false, lists:keyfind(<<"data-ah-validate-hint">>, 1, A)).

validate_errors_test() ->
    ?assertError({aihtml, {unknown_rule, bogus}}, ?M:validate([bogus])),
    ?assertError({aihtml, {bad_rule_argument, min_length, x}}, ?M:validate([{min_length, x}])),
    ?assertError({aihtml, {bad_validate_option, hint, big}}, ?M:validate([{hint, big}])).

validate_on_native_input_test() ->
    H = r(aihtml_html:void(input, [], [{name, e}, ?M:validate([required])])),
    ?assert(has(H, <<"data-ah-validate=\"[{&quot;rule&quot;:&quot;required&quot;}]\"">>)).

facade_extras_test() ->
    ?assertEqual([{validate, 1}], ?M:facade_extras()),
    [?assert(erlang:function_exported(?M, F, A)) || {F, A} <- ?M:facade_extras()].

%%% element records (designs/05-records.md)

record_equals_builder_test() ->
    ?assertEqual(r(?M:field(<<"L">>, <<"ctl">>, [top],
                            [{for, x}, {error, <<"E">>}, {required, true}, {id, row}])),
                 r(#ah_field{label = <<"L">>, body = <<"ctl">>, label_position = top, for = x,
                             error = <<"E">>, required = true, id = row})).

builder_fills_fields_test() ->
    ?assertMatch(#ah_field{label = <<"L">>, body = <<"c">>, help = <<"h">>, label_width = 60},
                 ?M:field(<<"L">>, <<"c">>, [], [{help, <<"h">>}, {label_width, 60}])).

field_can_wrap_records_test() ->
    %% a control given as a record is rendered in place
    Dd = #ah_dropdownlist{items = [a, b], value = a, id = pick},
    H = r(#ah_field{label = <<"Pick">>, body = Dd, for = pick}),
    ?assert(has(H, <<"<label class=\"ah-form-label\" for=\"pick\">">>)),
    ?assert(has(H, <<"<div><div class=\"ah-dropdownlist\" role=\"combobox\"">>)),
    H2 = r(aihtml_form_layout:form_layout([#{label => <<"S">>, key => s,
                                             control => fun(V) -> #ah_slider{value = V} end}],
                                          #{s => 30}, [], [])),
    ?assert(has(H2, <<"data-ah-value=\"30\"">>)).

postback_test() ->
    ?assertError({aihtml, {no_postback_event, ah_field}},
                 r(#ah_field{postback = x})).

field_validation_test() ->
    ?assertError({aihtml, {bad_modifier, field, label_position, middle, _}},
                 r(#ah_field{label_position = middle})).

%%% catalog

catalog_test() ->
    Names = [N || #{name := N} <- ?M:catalog()],
    ?assertEqual([field], Names),
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

default(ah_field) -> #ah_field{}.
