-module(aihtml_form_text_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_form_text.hrl").

-export([action/4]).

-define(M, aihtml_form_text).

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

%%% textarea

textarea_test() ->
    H = r(?M:textarea(<<"a</textarea><b>">>, [sm, invalid], [{name, notes}, {rows, 5}])),
    has(H, <<"class=\"ah-input-group ah-textarea-group ah-input-sm ah-input-invalid\"">>),
    has(H, <<"<textarea class=\"ah-input ah-textarea\" rows=\"5\" aria-invalid=\"true\" "
             "name=\"notes\">a&lt;/textarea&gt;&lt;b&gt;</textarea>">>),
    ?assertError({aihtml, {unknown_modifier, textarea, clearable, _}},
                 ?M:textarea(undefined, [clearable], [])).

%%% password_input

password_test() ->
    H = r(?M:password_input(<<"p&w">>, [lg], [{name, pw}])),
    has(H, <<"class=\"ah-pwd-group ah-pwd-lg\" data-ah=\"password-input\"">>),
    has(H, <<"<input class=\"ah-pwd\" type=\"password\" value=\"p&amp;w\"">>),
    has(H, <<"name=\"pw\"">>),
    has(H, <<"<button class=\"ah-pwd-toggle\" type=\"button\" tabindex=\"-1\" "
             "aria-label=\"Show password\" aria-pressed=\"false\"">>),
    has(H, <<"ah-pwd-icon-show">>),
    has(H, <<"ah-pwd-icon-hide">>),
    hasnt(H, <<"ah-pwd-strength">>).

password_options_test() ->
    H = r(?M:password_input(undefined, [disabled], [{toggle, false}, {strength, true}])),
    hasnt(H, <<"ah-pwd-toggle">>),
    has(H, <<"<div class=\"ah-pwd-strength\"><div class=\"ah-pwd-strength-bar\">"
             "<div class=\"ah-pwd-strength-fill\"></div></div>">>),
    has(H, <<"ah-pwd-disabled">>),
    hasnt(H, <<"toggle=">>),
    ?assertError({aihtml, {unknown_modifier, password_input, readonly, _}},
                 ?M:password_input(undefined, [readonly], [])).

%%% number_input

number_basic_test() ->
    H = r(?M:number_input(5, [], [{min, 0}, {max, 10}, {name, qty}])),
    has(H, <<"data-ah=\"number-input\" data-min=\"0\" data-max=\"10\" data-step=\"1\" "
             "data-decimals=\"0\"">>),
    has(H, <<"role=\"spinbutton\"">>),
    has(H, <<"value=\"5\"">>),
    has(H, <<"aria-valuenow=\"5\"">>),
    has(H, <<"name=\"qty\"">>),
    has(H, <<"<div class=\"ah-numinput-spin\"><span class=\"ah-numinput-spin-up\"">>).

number_clamp_and_decimals_test() ->
    has(r(?M:number_input(50, [], [{max, 10}])), <<"value=\"10\"">>),
    has(r(?M:number_input(-3, [], [{min, 0}])), <<"value=\"0\"">>),
    has(r(?M:number_input(<<"2.5">>, [], [{step, 0.25}])), <<"value=\"2.50\"">>),
    has(r(?M:number_input(3, [], [{decimals, 1}])), <<"value=\"3.0\"">>),
    has(r(?M:number_input(undefined, [], [])), <<"type=\"text\"">>),
    has(r(?M:number_input(undefined, [], [])), <<"value=\"\"">>),
    has(r(?M:number_input(undefined, [], [{allow_null, false}, {min, 2}])), <<"value=\"2\"">>),
    ?assertError({aihtml, {bad_number, <<"abc">>}}, r(?M:number_input(<<"abc">>, [], []))),
    ?assertError({aihtml, {bad_option, number_input, step, 0}},
                 r(?M:number_input(1, [], [{step, 0}]))).

number_symbol_spin_test() ->
    H = r(?M:number_input(1, [readonly, sm], [{symbol, <<"<$>">>}, {spin, false}])),
    has(H, <<"<span class=\"ah-numinput-prefix\">&lt;$&gt;</span><input">>),
    hasnt(H, <<"ah-numinput-spin">>),
    has(H, <<"ah-numinput-group ah-numinput-sm ah-numinput-readonly">>),
    has(H, <<" readonly">>),
    H2 = r(?M:number_input(1, [], [{symbol, <<"%">>}, {symbol_position, right}])),
    has(H2, <<"</div><span class=\"ah-numinput-suffix\">%</span></div>">>).

%%% input_otp

otp_test() ->
    H = r(?M:input_otp(4, <<"12a3">>, [], [{name, code}, {id, otp}])),
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
    H = r(?M:input_otp(6, <<"123456789">>, [disabled],
                       [{separator_at, 3}, {pattern, alphanumeric}])),
    has(H, <<"data-ah-value=\"123456\"">>),
    has(H, <<"data-complete=\"true\"">>),
    has(H, <<"data-disabled=\"true\"">>),
    ?assertEqual(1, count(H, <<"ah-input-otp__separator">>)),
    has(r(?M:input_otp(4, <<"aB1">>, [], [{pattern, alphanumeric}])), <<"data-ah-value=\"aB1\"">>),
    hasnt(H, <<"separator-at=">>),
    ?assertError({aihtml, {bad_length, input_otp, 0}}, ?M:input_otp(0, undefined, [], [])),
    ?assertError({aihtml, {unknown_modifier, input_otp, sm, _}},
                 ?M:input_otp(4, undefined, [sm], [])).

%%% tag_input

tag_input_test() ->
    H = r(?M:tag_input([<<"a">>, <<"<b>">>], [<<"w-96">>], [{name, tags}, {max_tags, 5}])),
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
    H = r(?M:tag_input([], [disabled], [{placeholder, <<"Tags">>}, {chip_color, error},
                                         {allow_duplicates, true}])),
    has(H, <<"data-ah-value=\"\"">>),
    has(H, <<"data-disabled=\"true\"">>),
    has(H, <<"data-chip-color=\"error\"">>),
    has(H, <<"data-allow-duplicates">>),
    has(H, <<"placeholder=\"Tags\" aria-label=\"Tags\" disabled">>),
    hasnt(H, <<"ah-tag-input__chip">>),
    hasnt(H, <<"type=\"hidden\"">>).

tag_input_action_on_root_test() ->
    H = r(?M:tag_input([<<"x">>], [], [{<<"data-ah-on">>, {actions, [{<<"change">>, <<"T">>, #{}}]}}])),
    has(H, <<"data-chip-variant=\"soft\" data-ah-on=\"change:T\">">>).

%%% catalog

catalog_test() ->
    Names = [N || #{name := N} <- ?M:catalog()],
    ?assertEqual([input, textarea, password_input, number_input, input_otp, tag_input], Names),
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

record_equals_builder_test() ->
    ?assertEqual(r(?M:input(<<"hi">>, [lg, invalid, clearable, disabled, <<"w-64">>],
                            [{name, q}, {placeholder, <<"P">>}, {prefix, <<"$">>},
                             {label, <<"L">>}, {id, q1}])),
                 r(#ah_input{value = <<"hi">>, size = lg, state = invalid, clearable = true,
                             disabled = true, css = [<<"w-64">>], prefix = <<"$">>,
                             label = <<"L">>, id = q1,
                             attrs = [{name, q}, {placeholder, <<"P">>}]})),
    ?assertEqual(r(?M:textarea(<<"t">>, [sm, no_rounded], [{rows, 5}])),
                 r(#ah_textarea{value = <<"t">>, size = sm, no_rounded = true,
                                attrs = [{rows, 5}]})),
    ?assertEqual(r(?M:password_input(undefined, [valid], [{toggle, false}, {strength, true}])),
                 r(#ah_password_input{state = valid, toggle = false, strength = true})),
    ?assertEqual(r(?M:number_input(<<"2.5">>, [readonly], [{step, 0.5}, {min, 0}, {max, 9},
                                                           {symbol, <<"%">>},
                                                           {symbol_position, right},
                                                           {name, n}])),
                 r(#ah_number_input{value = <<"2.5">>, readonly = true, step = 0.5, min = 0,
                                    max = 9, symbol = <<"%">>, symbol_position = right,
                                    attrs = [{name, n}]})),
    ?assertEqual(r(?M:input_otp(4, <<"a1">>, [disabled], [{pattern, alphanumeric},
                                                         {separator_at, 2}, {name, c}])),
                 r(#ah_input_otp{length = 4, value = <<"a1">>, disabled = true,
                                 pattern = alphanumeric, separator_at = 2, name = c})),
    ?assertEqual(r(?M:tag_input([<<"a">>], [<<"w-96">>], [{name, t}, {max_tags, 3},
                                                         {chip_color, error},
                                                         {title, <<"x">>}])),
                 r(#ah_tag_input{value = [<<"a">>], css = [<<"w-96">>], name = t,
                                 max_tags = 3, chip_color = error,
                                 attrs = [{title, <<"x">>}]})).

builder_fills_fields_test() ->
    I = ?M:input(<<"v">>, [sm, clearable, <<"x">>],
                 [{id, a}, {label, <<"L">>}, {suffix, <<"kg">>}, {name, q}]),
    ?assertMatch(#ah_input{value = <<"v">>, size = sm, state = undefined, clearable = true,
                           id = a, label = <<"L">>, suffix = <<"kg">>, css = [<<"x">>],
                           attrs = [{name, q}]}, I),
    ?assertMatch(#ah_number_input{min = 0, step = 1, spin = false, allow_null = false},
                 ?M:number_input(1, [], [{min, 0}, {spin, false}, {allow_null, false}])),
    ?assertMatch(#ah_tag_input{value = [<<"a">>], name = tags, placeholder = <<"P">>,
                               disabled = true, attrs = []},
                 ?M:tag_input([<<"a">>], [disabled], [{name, tags}, {placeholder, <<"P">>}])),
    ?assertError({aihtml, {record_only_field, ah_input, postback}},
                 ?M:input(undefined, [], [{postback, save}])).

postback_test() ->
    Token = fun(Html) ->
                    {match, [T]} = re:run(r(Html), <<"data-ah-on=\"([a-z]+:[^\"]+)\"">>,
                                          [{capture, all_but_first, binary}]),
                    [Ev, Tok] = binary:split(T, <<":">>),
                    {ok, Ref} = aihtml_action:unsign(Tok),
                    {Ev, Ref}
            end,
    Change = fun(R) -> ?assertEqual({<<"change">>, {?MODULE, save, #{k => 1}}}, Token(R)) end,
    Change(#ah_input{postback = {save, #{k => 1}}}),
    Change(#ah_textarea{postback = {save, #{k => 1}}}),
    Change(#ah_password_input{postback = {save, #{k => 1}}}),
    Change(#ah_number_input{postback = {save, #{k => 1}}}),
    Change(#ah_input_otp{postback = {save, #{k => 1}}}),
    Change(#ah_tag_input{postback = {save, #{k => 1}}}),
    %% the native control carries the binding of the input family
    ?assert(has(r(#ah_input{postback = save}), <<"<input class=\"ah-input\" type=\"text\" data-ah-on=">>)),
    %% input_otp and tag_input bind on the root
    ?assert(has(r(#ah_tag_input{postback = save}),
                <<"data-chip-variant=\"soft\" data-ah-on=">>)).

field_validation_test() ->
    ?assertError({aihtml, {bad_modifier, input, size, huge, _}}, r(#ah_input{size = huge})),
    ?assertError({aihtml, {bad_modifier, textarea, state, sm, _}}, r(#ah_textarea{state = sm})),
    ?assertError({aihtml, {bad_flag, input, clearable, yes}}, r(#ah_input{clearable = yes})),
    ?assertError({aihtml, {modifier_in_css, password_input, sm}},
                 r(#ah_password_input{css = [sm]})),
    ?assertError({aihtml, {bad_option, number_input, min, x}}, r(#ah_number_input{min = x})),
    ?assertError({aihtml, {bad_option, number_input, decimals, -1}},
                 r(#ah_number_input{decimals = -1})),
    ?assertError({aihtml, {bad_number, <<"z">>}}, r(#ah_number_input{value = <<"z">>})),
    ?assertError({aihtml, {bad_length, input_otp, 0}}, r(#ah_input_otp{length = 0})),
    ?assertError({aihtml, {bad_option, input_otp, pattern, hex}},
                 r(#ah_input_otp{pattern = hex})),
    %% groups without a default may stay undefined
    ?assert(has(r(#ah_input{}), <<"<div class=\"ah-input-group\" data-ah=\"input\">">>)).

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

default(ah_input) -> #ah_input{};
default(ah_textarea) -> #ah_textarea{};
default(ah_password_input) -> #ah_password_input{};
default(ah_number_input) -> #ah_number_input{};
default(ah_input_otp) -> #ah_input_otp{};
default(ah_tag_input) -> #ah_tag_input{}.

arity(Sig) ->
    [_, Args] = binary:split(Sig, <<"(">>),
    length(binary:split(Args, <<",">>, [global])).

count(H, P) -> length(binary:matches(H, P)).
