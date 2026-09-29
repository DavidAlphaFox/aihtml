-module(aihtml_button_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_button.hrl").

-behaviour(aihtml_element).
-export([render/1, action/4]).

-define(M, aihtml_button).

r(H) -> aihtml_html:render_binary(H).

has(Sub, Bin) -> binary:match(Bin, Sub) =/= nomatch.

-define(has(Sub, Bin), ?assert(has(Sub, Bin))).
-define(hasnt(Sub, Bin), ?assertNot(has(Sub, Bin))).

%%% button

button_default_test() ->
    ?assertEqual(<<"<button class=\"ah-btn ah-btn-primary\" type=\"button\" value=\"save\">Save</button>">>,
                 r(?M:button(<<"Save">>, save, [], []))).

button_no_value_test() ->
    ?hasnt(<<"value=">>, r(?M:button(<<"Go">>, undefined, [], []))).

button_variants_and_sizes_test() ->
    H = r(?M:button(<<"x">>, undefined, [outlined, lg, round, <<"mt-2">>], [])),
    ?has(<<"class=\"ah-btn ah-btn-lg ah-btn-outlined ah-btn-round mt-2\"">>, H),
    ?has(<<"class=\"ah-btn ah-btn-primary\"">>, r(?M:button(<<"x">>, undefined, [md], []))),
    ?has(<<"ah-btn-sm">>, r(?M:button(<<"x">>, undefined, [sm], []))).

button_type_override_test() ->
    H = r(?M:button(<<"x">>, undefined, [], [{type, submit}])),
    ?has(<<"type=\"submit\"">>, H),
    ?hasnt(<<"type=\"button\"">>, H).

button_disabled_test() ->
    H = r(?M:button(<<"x">>, undefined, [], [{disabled, true}])),
    ?has(<<"ah-btn-disabled">>, H),
    ?has(<<" disabled>">>, H),
    ?hasnt(<<"ah-btn-disabled">>, r(?M:button(<<"x">>, undefined, [], [{disabled, false}]))).

button_escaping_test() ->
    H = r(?M:button(<<"<b>&\"">>, <<"a\"b">>, [], [{title, <<"<t>">>}])),
    ?has(<<"&lt;b&gt;&amp;&quot;</button>">>, H),
    ?has(<<"value=\"a&quot;b\"">>, H),
    ?has(<<"title=\"&lt;t&gt;\"">>, H),
    ?hasnt(<<"<b>">>, H).

button_icon_test() ->
    H = r(?M:button(<<"Go">>, undefined, [], [{icon, <<"*">>}, {icon_position, right}])),
    ?has(<<"ah-btn-img-right">>, H),
    ?has(<<"<span class=\"ah-btn-text\">Go</span><span class=\"ah-btn-img\" aria-hidden=\"true\">*</span>">>, H),
    ?hasnt(<<"icon">>, binary:replace(H, <<"ah-btn-img">>, <<>>, [global])),
    I = r(?M:button(<<"Go">>, undefined, [], [{img, <<"/a.png">>}])),
    ?has(<<"<img class=\"ah-btn-img\" src=\"/a.png\" width=\"16\" height=\"16\" alt=\"\">">>, I),
    %% field values are checked when rendering
    ?assertError({aihtml, {bad_option, icon_position, middle}},
                 r(?M:button(<<"Go">>, undefined, [], [{icon, <<"*">>}, {icon_position, middle}]))).

button_modifier_validation_test() ->
    ?assertError({aihtml, {unknown_modifier, button, huge, _}},
                 ?M:button(<<"x">>, undefined, [huge], [])),
    ?assertError({aihtml, {conflicting_modifiers, button, variant, _}},
                 ?M:button(<<"x">>, undefined, [primary, error], [])),
    ?assertError({aihtml, {conflicting_modifiers, button, size, _}},
                 ?M:button(<<"x">>, undefined, [sm, lg], [])).

%%% catalog (demos live in aihtml_example)

catalog_test() ->
    Cat = ?M:catalog(),
    Names = [N || #{name := N} <- Cat],
    ?assertEqual([button], Names),
    [begin
         ?assert(erlang:function_exported(?M, N, 4)),
         #{category := form, signature := S, root := <<"ah-", _/binary>>} = E,
         ?assert(is_binary(S))
     end || #{name := N} = E <- Cat],
    %% every behaviour named in the catalog is rendered by its component
    Behaviors = [B || #{behavior := B} <- Cat],
    ?assertEqual(0, length(Behaviors)).

catalog_docs_test() ->
    [begin
         Docs = maps:get(option_docs, E, #{}),
         ?assertEqual(lists:sort(maps:get(options, E, []) ++ maps:get(flags, E, [])),
                      lists:sort(maps:keys(Docs))),
         [?assert(is_binary(D) andalso D =/= <<>>) || D <- maps:values(Docs)],
         Ms = maps:get(methods, E),
         [#{name := _, args := <<"(", _/binary>>, doc := _} = X || X <- Ms],
         ?assertEqual(maps:get(behavior, E, none) =/= none, Ms =/= [])
     end || E <- ?M:catalog()].

%%% element records (designs/05-records.md)

%% A test element that renders through this module.
-record(test_badge, {?AH_BASE(?MODULE), text = <<>>}).

-spec render(tuple()) -> aihtml_html:html().
render(#test_badge{text = T} = R) ->
    aihtml_html:el(span, T, [<<"badge">> | R#test_badge.css],
                   aihtml_element:root_attrs(R, none));
%% wraps the default rendering of a button
render(#ah_button{} = B) ->
    aihtml_html:el(span, ?M:render(B#ah_button{module = ?M}), [<<"wrap">>], []).

-spec action(atom(), term(), map(), term()) -> ok.
action(_, _, _, _) -> ok.

record_equals_builder_test() ->
    ?assertEqual(r(?M:button(<<"Save">>, save, [outlined, lg, round, <<"mt-2">>],
                             [{disabled, true}, {id, s}, {title, <<"t">>}])),
                 r(#ah_button{body = <<"Save">>, value = save, variant = outlined, size = lg,
                              round = true, css = [<<"mt-2">>], disabled = true, id = s,
                              attrs = [{title, <<"t">>}]})).

builder_fills_fields_test() ->
    ?assertError({aihtml, {record_only_field, ah_button, postback}},
                 ?M:button(<<"x">>, undefined, [], [{postback, save}])).

postback_test() ->
    Token = fun(Html) ->
                    {match, [T]} = re:run(r(Html), <<"data-ah-on=\"([a-z]+:[^\"]+)\"">>,
                                          [{capture, all_but_first, binary}]),
                    [Ev, Tok] = binary:split(T, <<":">>),
                    {ok, Ref} = aihtml_action:unsign(Tok),
                    {Ev, Ref}
            end,
    ?assertEqual({<<"click">>, {?MODULE, save, #{id => 7}}},
                 Token(#ah_button{body = <<"Save">>, postback = {save, #{id => 7}}})),
    H = r(#ah_button{postback = {save, #{}, #{debounce => 300, confirm => <<"Sure?">>}}}),
    ?has(<<"data-ah-confirm=\"Sure?\"">>, H),
    ?assertMatch({match, _}, re:run(H, <<"data-ah-on=\"click:[^\":]+:300\"">>)),
    ?assertError({aihtml, {bad_postback, ah_button, "save"}},
                 r(#ah_button{postback = "save"})).

field_validation_test() ->
    ?assertError({aihtml, {bad_modifier, button, variant, huge, _}},
                 r(#ah_button{variant = huge})),
    ?assertError({aihtml, {bad_modifier, button, size, primary, _}},
                 r(#ah_button{size = primary})),
    ?assertError({aihtml, {bad_flag, button, round, yes}}, r(#ah_button{round = yes})),
    ?assertError({aihtml, {modifier_in_css, button, primary}},
                 r(#ah_button{css = [primary]})).

custom_module_test() ->
    %% an element of another module, nested in a component
    H = r(#ah_button{body = #test_badge{text = <<"<3">>, css = [<<"ml-1">>], id = b}}),
    ?has(<<"<span class=\"badge ml-1\" id=\"b\">&lt;3</span></button>">>, H),
    ?assertError({aihtml, {no_postback_event, test_badge}},
                 r(#test_badge{postback = x})),
    %% one button rendered by another module
    ?assertEqual(<<"<span class=\"wrap\">", (r(#ah_button{body = <<"x">>}))/binary, "</span>">>,
                 r(#ah_button{module = ?MODULE, body = <<"x">>})),
    ?assertError({aihtml, {no_render, no_such_module, ah_button}},
                 r(#ah_button{module = no_such_module})).

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

default(ah_button) -> #ah_button{}.
