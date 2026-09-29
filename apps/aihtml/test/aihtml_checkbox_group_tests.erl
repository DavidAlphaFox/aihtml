-module(aihtml_checkbox_group_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_checkbox_group.hrl").

-export([action/4]).

-define(M, aihtml_checkbox_group).

r(H) -> aihtml_html:render_binary(H).

has(Sub, Bin) -> binary:match(Bin, Sub) =/= nomatch.

count(Sub, Bin) -> length(binary:matches(Bin, Sub)).

%% --- checkbox_group ------------------------------------------------

items() -> [{a, <<"A">>}, {b, <<"B & b">>}, {c, <<"C">>, #{disabled => true, class => <<"x">>}}].

checkbox_group_test() ->
    A = [{<<"data-ah-on">>, {actions, [{<<"change">>, <<"TOK">>, #{}}]}}],
    H = r(?M:checkbox_group(items(), [a, <<"c">>], [horizontal],
                            [{name, f}, {id, <<"g">>}, A])),
    ?assertMatch(<<"<div class=\"ah-checkbox-group ah-checkbox-group-horizontal\" data-ah=\"checkbox-group\" role=\"group\" data-ah-value=\"a,c\" data-label-position=\"after\" id=\"g\" data-ah-on=\"change:TOK\">", _/binary>>, H),
    %% name goes to every input, the action only to the root
    ?assertEqual(3, count(<<"name=\"f\"">>, H)),
    ?assertEqual(1, count(<<"data-ah-on">>, H)),
    ?assertEqual(2, count(<<" checked">>, H)),
    ?assert(has(<<"B &amp; b">>, H)),
    ?assert(has(<<"ah-checkbox-group-item x ah-checkbox-group-item-disabled\" data-value=\"c\" data-index=\"2\" data-ah-item-disabled">>, H)),
    ?assertEqual(1, count(<<" disabled">>, H)).

checkbox_group_disabled_before_test() ->
    H = r(?M:checkbox_group(items(), [], [label_before, sm], [{disabled, true}])),
    ?assert(has(<<"ah-checkbox-group ah-checkbox-group-vertical ah-checkbox-group-disabled\"">>, H)),
    ?assert(has(<<"data-label-position=\"before\"">>, H)),
    ?assert(has(<<"aria-disabled=\"true\"">>, H)),
    ?assertEqual(3, count(<<" disabled">>, H)),
    ?assert(has(<<"<span class=\"ah-checkbox-group-label\">A</span><span class=\"ah-checkbox ah-checkbox-sm ah-checkbox-disabled\">">>, H)),
    ?assertNot(has(<<"ah-checkbox-group-sm">>, H)),
    ?assertError({aihtml, {conflicting_modifiers, checkbox_group, layout, _}},
                 ?M:checkbox_group(items(), [], [vertical, horizontal], [])).

render_all_test() ->
    Items = [{a, <<"A">>}, {b, <<"B">>}],
    ?assert(is_binary(r(?M:checkbox_group(Items, [a], [], [])))).

record_equals_builder_test() ->
    ?assertEqual(r(?M:checkbox_group(items(), [a], [horizontal, label_before, sm],
                                     [{name, f}, {id, g}, {title, <<"t">>}])),
                 r(#ah_checkbox_group{items = items(), value = [a], layout = horizontal,
                                      label_before = true, size = sm, name = f, id = g,
                                      attrs = [{title, <<"t">>}]})).

postback_test() ->
    P = {save, #{id => 1}},
    postback_change(#ah_checkbox_group{items = items(), postback = P}),
    %% groups bind it on the root
    ?assertMatch({match, _}, re:run(r(#ah_checkbox_group{items = items(), postback = P}),
                                    <<"^<div [^>]*data-ah-on=">>)).

field_validation_test() ->
    ?assertError({aihtml, {bad_modifier, checkbox_group, layout, grid, _}},
                 r(#ah_checkbox_group{layout = grid})).

%% --- catalog --------------------------------------------------------

catalog_test() ->
    Names = [N || #{name := N} <- ?M:catalog()],
    ?assertEqual([checkbox_group], Names),
    [begin
         ?assert(erlang:function_exported(?M, N, 4)),
         ?assertMatch(#{category := form, behavior := B} when is_binary(B), E)
     end || #{name := N} = E <- ?M:catalog()].

api_docs_test() ->
    [begin
         Documented = maps:keys(maps:get(option_docs, E)),
         Mods = lists:append([Ms || {Ms, _} <- maps:values(maps:get(groups, E, #{}))]),
         Keys = maps:get(options, E, []) ++ maps:get(flags, E, []) ++ Mods,
         ?assertEqual({N, []}, {N, Keys -- Documented}),
         ?assertMatch([_ | _], maps:get(methods, E))
     end || #{name := N} = E <- ?M:catalog()].

%%% element records (designs/05-records.md)

-spec action(atom(), term(), map(), term()) -> ok.
action(_, _, _, _) -> ok.

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

default(ah_checkbox_group) -> #ah_checkbox_group{}.

token(Html) ->
    {match, [T]} = re:run(r(Html), <<"data-ah-on=\"([a-z]+:[^\"]+)\"">>,
                          [{capture, all_but_first, binary}]),
    [Ev, Tok] = binary:split(T, <<":">>),
    {ok, Ref} = aihtml_action:unsign(Tok),
    {Ev, Ref}.

%% the postback fires on change
postback_change(E) ->
    ?assertEqual({element(1, E), {<<"change">>, {?MODULE, save, #{id => 1}}}},
                 {element(1, E), token(E)}).

%% A value containing a comma is escaped in data-ah-value (aihtml_value).
vhas(Sub, Bin) -> binary:match(Bin, Sub) =/= nomatch.

comma_values_test() ->
    H = r(?M:checkbox_group([{<<"a,b">>, <<"AB">>}, {c, <<"C">>}], [<<"a,b">>, c], [], [])),
    ?assert(vhas(<<"data-ah-value=\"a\\,b,c\"">>, H)),
    ?assert(vhas(<<"value=\"a,b\" checked">>, H)).
