-module(aihtml_input_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_input.hrl").

-export([action/4]).

-define(M, aihtml_input).

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

%%% input

input_basic_test() ->
    H = r(?M:input(<<"hi">>, [], [{name, q}, {placeholder, <<"Search">>}])),
    has(H, <<"<div class=\"ah-input-group ah-input-has-value\" data-ah=\"input\">">>),
    has(H, <<"<input class=\"ah-input\" type=\"text\" value=\"hi\" name=\"q\" placeholder=\"Search\">">>),
    hasnt(H, <<"ah-input-row">>).

input_escapes_value_test() ->
    H = r(?M:input(<<"\"><script>x</script>">>, [], [])),
    has(H, <<"value=\"&quot;&gt;&lt;script&gt;x&lt;/script&gt;\"">>),
    hasnt(H, <<"<script>">>).

input_modifiers_test() ->
    H = r(?M:input(undefined, [sm, invalid, disabled, no_rounded, <<"w-64">>], [])),
    has(H, <<"class=\"ah-input-group ah-input-sm ah-input-invalid ah-input-disabled "
             "ah-input-no-rounded w-64\"">>),
    has(H, <<"aria-invalid=\"true\"">>),
    has(H, <<" disabled">>),
    has(r(?M:input(undefined, [lg, valid], [])), <<"ah-input-lg ah-input-valid">>).

input_bad_modifiers_test() ->
    ?assertError({aihtml, {unknown_modifier, input, huge, _}}, ?M:input(undefined, [huge], [])),
    ?assertError({aihtml, {conflicting_modifiers, input, size, [sm, lg]}},
                 ?M:input(undefined, [sm, lg], [])),
    ?assertError({aihtml, {conflicting_modifiers, input, state, _}},
                 ?M:input(undefined, [valid, invalid], [])).

input_addons_test() ->
    H = r(?M:input(undefined, [clearable], [{prefix, <<"<$>">>}, {suffix, <<"kg">>}])),
    has(H, <<"<div class=\"ah-input-row\"><span class=\"ah-input-addon ah-input-addon-prefix\">"
             "&lt;$&gt;</span><input class=\"ah-input\" type=\"text\">"
             "<button class=\"ah-input-clear\"">>),
    has(H, <<"<span class=\"ah-input-addon ah-input-addon-suffix\">kg</span></div>">>),
    has(H, <<"ah-input-clearable">>),
    %% options are not written as attributes
    hasnt(H, <<"prefix=">>).

input_label_test() ->
    H = r(?M:input(undefined, [], [{label, <<"A & B">>}, {id, x1}, {placeholder, <<"gone">>}])),
    has(H, <<"<label class=\"ah-input-label\" for=\"x1\">A &amp; B</label>">>),
    has(H, <<"id=\"x1\"">>),
    hasnt(H, <<"placeholder">>),
    H2 = r(?M:input(<<"v">>, [], [{label, <<"L">>}])),
    has(H2, <<"ah-input-label ah-input-label-float">>),
    {match, [[Id]]} = re:run(H2, <<"id=\"(ah-in-[0-9]+)\"">>, [global, {capture, all_but_first, binary}]),
    has(H2, <<"for=\"", Id/binary, "\"">>).

input_action_on_native_test() ->
    H = r(?M:input(undefined, [], [{<<"data-ah-on">>, {actions, [{<<"change">>, <<"TOK">>, #{}}]}}])),
    has(H, <<"<input class=\"ah-input\" type=\"text\" data-ah-on=\"change:TOK\">">>).

record_equals_builder_test() ->
    ?assertEqual(r(?M:input(<<"hi">>, [lg, invalid, clearable, disabled, <<"w-64">>],
                            [{name, q}, {placeholder, <<"P">>}, {prefix, <<"$">>},
                             {label, <<"L">>}, {id, q1}])),
                 r(#ah_input{value = <<"hi">>, size = lg, state = invalid, clearable = true,
                             disabled = true, css = [<<"w-64">>], prefix = <<"$">>,
                             label = <<"L">>, id = q1,
                             attrs = [{name, q}, {placeholder, <<"P">>}]})).

builder_fills_fields_test() ->
    I = ?M:input(<<"v">>, [sm, clearable, <<"x">>],
                 [{id, a}, {label, <<"L">>}, {suffix, <<"kg">>}, {name, q}]),
    ?assertMatch(#ah_input{value = <<"v">>, size = sm, state = undefined, clearable = true,
                           id = a, label = <<"L">>, suffix = <<"kg">>, css = [<<"x">>],
                           attrs = [{name, q}]}, I),
    ?assertError({aihtml, {record_only_field, ah_input, postback}},
                 ?M:input(undefined, [], [{postback, save}])).

postback_test() ->
    ?assertEqual({<<"change">>, {?MODULE, save, #{k => 1}}},
                 token(#ah_input{postback = {save, #{k => 1}}})),
    %% the native control carries the binding of the input family
    ?assert(has(r(#ah_input{postback = save}), <<"<input class=\"ah-input\" type=\"text\" data-ah-on=">>)).

field_validation_test() ->
    ?assertError({aihtml, {bad_modifier, input, size, huge, _}}, r(#ah_input{size = huge})),
    ?assertError({aihtml, {bad_flag, input, clearable, yes}}, r(#ah_input{clearable = yes})),
    %% groups without a default may stay undefined
    ?assert(has(r(#ah_input{}), <<"<div class=\"ah-input-group\" data-ah=\"input\">">>)).

%%% catalog

catalog_test() ->
    Names = [N || #{name := N} <- ?M:catalog()],
    ?assertEqual([input], Names),
    [begin
         ?assert(erlang:function_exported(?M, N, arity(S))),
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

default(ah_input) -> #ah_input{}.

arity(Sig) ->
    [_, Args] = binary:split(Sig, <<"(">>),
    length(binary:split(Args, <<",">>, [global])).
