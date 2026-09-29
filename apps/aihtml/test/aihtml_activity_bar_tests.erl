%% Tests for aihtml_activity_bar. The module is also the fake action
%% module of the postback test.
-module(aihtml_activity_bar_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_activity_bar.hrl").

-define(M, aihtml_activity_bar).

r(Html) -> aihtml_html:render_binary(Html).

has(Needle, Hay) ->
    case binary:match(Hay, Needle) of
        nomatch -> ?debugFmt("~s not in~n~s", [Needle, Hay]), false;
        _ -> true
    end.

count(Needle, Hay) -> length(binary:matches(Hay, Needle)).

activity_items() ->
    [{files, <<"F">>, <<"Explorer">>},
     {search, <<"S">>, <<"Search">>},
     divider,
     {git, <<"G">>, <<"Source control">>, [{disabled, true}]}].

activity_bar_test() ->
    H = r(?M:activity_bar(activity_items(), search, [<<"h-64">>], [{id, ab}, {name, view}])),
    ?assert(has(<<"<div class=\"ah-activity-bar h-64\" role=\"tablist\" "
                  "aria-orientation=\"vertical\" data-placement=\"left\" "
                  "data-ah=\"activity-bar\" data-ah-value=\"search\" id=\"ab\">">>, H)),
    ?assert(has(<<"<input type=\"hidden\" name=\"view\" value=\"search\">">>, H)),
    ?assert(has(<<"<button class=\"ah-activity-bar__item\" type=\"button\" role=\"tab\" "
                  "data-id=\"search\" data-active=\"true\" data-disabled=\"false\" "
                  "aria-selected=\"true\" aria-label=\"Search\" title=\"Search\" "
                  "tabindex=\"0\"><span class=\"ah-activity-bar__icon\">S</span></button>">>, H)),
    ?assert(has(<<"data-id=\"files\" data-active=\"false\"">>, H)),
    ?assert(has(<<"<div class=\"ah-activity-bar__divider\" role=\"presentation\" "
                  "data-index=\"2\"></div>">>, H)),
    ?assert(has(<<"data-id=\"git\" data-active=\"false\" data-disabled=\"true\"">>, H)),
    ?assert(has(<<" disabled>">>, H)),
    ?assertEqual(1, count(<<"tabindex=\"0\"">>, H)).

activity_bar_right_and_no_value_test() ->
    H = r(?M:activity_bar(activity_items(), undefined, [right], [])),
    ?assert(has(<<"class=\"ah-activity-bar\"">>, H)),
    ?assert(has(<<"data-placement=\"right\"">>, H)),
    ?assert(has(<<"data-ah-value=\"\"">>, H)),
    %% nothing active: the first enabled item takes the tab stop
    ?assert(has(<<"data-id=\"files\" data-active=\"false\" data-disabled=\"false\" "
                  "aria-selected=\"false\" aria-label=\"Explorer\" title=\"Explorer\" "
                  "tabindex=\"0\"">>, H)),
    ?assertEqual(1, count(<<"tabindex=\"0\"">>, H)),
    ?assertError({aihtml, {bad_activity_bar_item, _}}, r(?M:activity_bar([x], x, [], []))).

%%%===================================================================
%%% Catalog
%%%===================================================================

catalog_test() ->
    [#{name := activity_bar}] = ?M:catalog(),
    #{flags := Fl, options := Op, option_docs := Docs, methods := Ms} =
        aihtml_catalog:entry(?M, activity_bar),
    ?assertEqual(lists:sort(Fl ++ Op), lists:sort(maps:keys(Docs))),
    ?assertMatch([_ | _], Ms).

%%%===================================================================
%%% element records (designs/05-records.md)
%%%===================================================================

record_equals_builder_test() ->
    ?assertEqual(r(?M:activity_bar(activity_items(), files, [right, <<"x">>],
                                   [{id, a}, {name, n}, {title, <<"t">>}])),
                 r(#ah_activity_bar{items = activity_items(), value = files, placement = right,
                                    css = [<<"x">>], id = a, name = n,
                                    attrs = [{title, <<"t">>}]})).

postback_test() ->
    Token = fun(Html) ->
                    {match, [T]} = re:run(r(Html), <<"data-ah-on=\"([a-z:]+:[^\":]+)\"">>,
                                          [{capture, all_but_first, binary}]),
                    [Tok | Rev] = lists:reverse(binary:split(T, <<":">>, [global])),
                    {ok, Ref} = aihtml_action:unsign(Tok),
                    {iolist_to_binary(lists:join(<<":">>, lists:reverse(Rev))), Ref}
            end,
    ?assertEqual({<<"change">>, {?MODULE, pick, #{id => 1}}},
                 Token(#ah_activity_bar{postback = {pick, #{id => 1}}})).

field_validation_test() ->
    ?assertError({aihtml, {bad_modifier, activity_bar, placement, top, _}},
                 r(#ah_activity_bar{placement = top})).

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

default(ah_activity_bar) -> #ah_activity_bar{}.
