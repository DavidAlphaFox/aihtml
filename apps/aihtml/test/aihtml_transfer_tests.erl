%% Tests for aihtml_transfer.
-module(aihtml_transfer_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_transfer.hrl").

-define(M, aihtml_transfer).

r(Html) -> aihtml_html:render_binary(Html).

has(Needle, Hay) ->
    case binary:match(Hay, Needle) of
        nomatch -> ?debugFmt("~s not in~n~s", [Needle, Hay]), false;
        _ -> true
    end.

has_quiet(Needle, Hay) -> binary:match(Hay, Needle) =/= nomatch.

count(Needle, Hay) -> length(binary:matches(Hay, Needle)).

%%%===================================================================
%%% transfer
%%%===================================================================

transfer_test() ->
    Items = [{a, <<"A">>}, {b, <<"B">>}, #{value => c, label => <<"C">>, icon => <<"★"/utf8>>},
             #{value => d, label => <<"D">>, disabled => true}],
    H = r(?M:transfer(Items, [c, a, zz], [<<"max-w-xl">>], [{id, t}, {name, keys}])),
    ?assert(has(<<"<div class=\"ah-transfer max-w-xl\" id=\"t\" data-ah=\"transfer\" "
                  "data-ah-value=\"c,a\">">>, H)),
    ?assert(has(<<"<input type=\"hidden\" name=\"keys\" value=\"c,a\">">>, H)),
    %% the source keeps the item order, the target the value order
    {match, [Src]} = re:run(H, <<"<ul class=\"ah-transfer-list\" id=\"t-source\"[^>]*>(.*?)</ul>">>,
                            [{capture, all_but_first, binary}]),
    {match, [Tgt]} = re:run(H, <<"<ul class=\"ah-transfer-list\" id=\"t-target\"[^>]*>(.*?)</ul>">>,
                            [{capture, all_but_first, binary}]),
    ?assertEqual([<<"b">>, <<"d">>], values(Src)),
    ?assertEqual([<<"c">>, <<"a">>], values(Tgt)),
    ?assert(has(<<"<li class=\"ah-transfer-item\" id=\"t-i-2\" role=\"option\" "
                  "aria-selected=\"false\" data-value=\"c\" data-idx=\"2\" "
                  "data-source=\"target\"><span class=\"ah-transfer-item-icon\" "
                  "aria-hidden=\"true\">★</span>"/utf8>>, Tgt)),
    ?assert(has(<<"ah-transfer-item ah-transfer-item-disabled\" id=\"t-i-3\"">>, Src)),
    ?assert(has(<<"<span class=\"ah-transfer-panel-title\" id=\"t-source-title\">Source</span>"
                  "<span class=\"ah-transfer-panel-count\">2</span>">>, H)),
    ?assert(has(<<"Target</span><span class=\"ah-transfer-panel-count\">2</span>">>, H)),
    ?assertEqual(2, count(<<"ah-transfer-filter-input">>, H)),
    ?assert(has(<<"data-empty-text=\"No data\"">>, H)),
    ?assert(has(<<"<button class=\"ah-transfer-btn ah-transfer-btn-to-target "
                  "ah-transfer-btn-disabled\" type=\"button\" data-direction=\"to-target\"">>, H)),
    ?assert(has(<<"tabindex=\"0\" aria-multiselectable=\"true\" aria-labelledby=\"t-target-title\"">>, H)),
    D = r(?M:transfer([a], [], [disabled, no_filter],
                      [{source_title, <<"L">>}, {target_title, <<"R">>},
                       {empty_text, <<"-">>}, {filter_placeholder, <<"F">>}])),
    ?assert(has(<<"class=\"ah-transfer ah-transfer-disabled ah-transfer-no-filter\"">>, D)),
    ?assertNot(has_quiet(<<"ah-transfer-filter">>, D)),
    ?assert(has(<<"tabindex=\"-1\"">>, D)),
    ?assert(has(<<">L</span>">>, D)),
    ?assert(has(<<"data-empty-text=\"-\"">>, D)),
    ?assertError({aihtml, {bad_option, value, a}}, r(?M:transfer([a], a, [], []))).

values(Html) ->
    {match, Vs} = re:run(Html, <<"data-value=\"([^\"]*)\"">>,
                         [global, {capture, all_but_first, binary}]),
    [V || [V] <- Vs].

%%%===================================================================
%%% Catalog
%%%===================================================================

catalog_test() ->
    [#{name := transfer}] = ?M:catalog(),
    #{flags := Fl, options := Op, option_docs := Docs, methods := Ms} =
        aihtml_catalog:entry(?M, transfer),
    ?assertEqual(lists:sort(Fl ++ Op), lists:sort(maps:keys(Docs))),
    ?assert(lists:member(setValue, [Name || #{name := Name} <- Ms])),
    ?assert(erlang:function_exported(?M, transfer, 4)).

%%%===================================================================
%%% element records (designs/05-records.md)
%%%===================================================================

record_equals_builder_test() ->
    ?assertEqual(r(?M:transfer([a, b], [b], [no_filter], [{id, t}, {source_title, <<"S">>}])),
                 r(#ah_transfer{items = [a, b], value = [b], no_filter = true, id = t,
                                source_title = <<"S">>})).

builder_fills_fields_test() ->
    ?assertError({aihtml, {record_only_field, ah_transfer, postback}},
                 ?M:transfer([], [], [], [{postback, moved}])).

generated_id_test() ->
    ?assertNotEqual(r(#ah_transfer{}), r(#ah_transfer{})).

token(Html) ->
    {match, [T]} = re:run(r(Html), <<"data-ah-on=\"(change:[^\"]+)\"">>,
                          [{capture, all_but_first, binary}]),
    [Ev, Tok] = binary:split(T, <<":">>),
    {ok, Ref} = aihtml_action:unsign(Tok),
    {Ev, Ref}.

postback_test() ->
    ?assertEqual({<<"change">>, {?MODULE, moved, #{}}},
                 token(#ah_transfer{items = [a], postback = moved})),
    %% the id stays first on the root, the postback follows its own attributes
    ?assertMatch({match, _}, re:run(r(#ah_transfer{id = t, postback = moved}),
                                    <<"^<div class=\"ah-transfer\" id=\"t\" data-ah=\"transfer\""
                                      "[^>]* data-ah-on=\"change:">>)).

field_validation_test() ->
    ?assertError({aihtml, {modifier_in_css, transfer, no_filter}},
                 r(#ah_transfer{css = [no_filter]})).

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

default(ah_transfer) -> #ah_transfer{}.

%% A value containing a comma is escaped in data-ah-value (aihtml_value).
vhas(Sub, Bin) -> binary:match(Bin, Sub) =/= nomatch.

comma_values_test() ->
    H = r(?M:transfer([<<"1,000">>, <<"2,000">>, <<"x">>], [<<"2,000">>, <<"1,000">>], [],
                      [{name, k}])),
    ?assert(vhas(<<"data-ah-value=\"2\\,000,1\\,000\"">>, H)),
    ?assert(vhas(<<"name=\"k\" value=\"2\\,000,1\\,000\"">>, H)).
