%% Tests for aihtml_listbox. The module is also the fake action module
%% of the listbox search round trip.
-module(aihtml_listbox_tests).
-behaviour(aihtml_action).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_listbox.hrl").

-export([action/4]).

-define(M, aihtml_listbox).

r(Html) -> aihtml_html:render_binary(Html).

has(Needle, Hay) ->
    case binary:match(Hay, Needle) of
        nomatch -> ?debugFmt("~s not in~n~s", [Needle, Hay]), false;
        _ -> true
    end.

has_quiet(Needle, Hay) -> binary:match(Hay, Needle) =/= nomatch.

count(Needle, Hay) -> length(binary:matches(Hay, Needle)).

collect() ->
    receive {ev, E} -> [E | collect()]
    after 0 -> []
    end.

%%%===================================================================
%%% listbox
%%%===================================================================

listbox_single_test() ->
    Items = [<<"a">>, {b, <<"Bee">>}, #{value => 3, label => <<"Three">>, disabled => true,
                                         icon => <<"/i.png">>}],
    H = r(?M:listbox(Items, b, [<<"w-60">>], [{id, lb}, {name, pick}])),
    ?assert(has(<<"<div class=\"ah-listbox w-60\" id=\"lb\" data-ah=\"listbox\" "
                  "data-ah-value=\"b\" tabindex=\"0\" role=\"listbox\" "
                  "aria-multiselectable=\"false\">">>, H)),
    ?assert(has(<<"<ul class=\"ah-listbox-list\" id=\"lb-list\" role=\"none\">">>, H)),
    ?assert(has(<<"<li class=\"ah-listbox-item ah-listbox-item-selected\" id=\"lb-o-1\" "
                  "role=\"option\" aria-selected=\"true\" data-idx=\"1\" data-value=\"b\">"
                  "<span class=\"ah-listbox-label\">Bee</span></li>">>, H)),
    ?assert(has(<<"ah-listbox-item-disabled\" id=\"lb-o-2\"">>, H)),
    ?assert(has(<<"<img class=\"ah-listbox-icon\" src=\"/i.png\" alt=\"\">">>, H)),
    ?assert(has(<<"<div class=\"ah-listbox-empty\" hidden>No data</div>">>, H)),
    ?assert(has(<<"<input type=\"hidden\" name=\"pick\" value=\"b\">">>, H)),
    ?assertNot(has_quiet(<<"ah-listbox-filter">>, H)),
    ?assertNot(has_quiet(<<"ah-listbox-checkbox">>, H)),
    E = r(?M:listbox([], undefined, [], [{empty_text, <<"Nothing">>}])),
    ?assert(has(<<"<div class=\"ah-listbox-empty\">Nothing</div>">>, E)),
    ?assertError({aihtml, {bad_list_item, _}}, r(?M:listbox([{1, 2, 3}], undefined, [], []))),
    ?assertError({aihtml, {bad_list_item, _}},
                 r(?M:listbox([#{value => 1, colour => red}], undefined, [], []))).

listbox_multi_groups_test() ->
    Items = [#{value => a, group => <<"G1">>}, #{value => b, group => <<"G2">>},
             #{value => c, group => <<"G1">>}],
    H = r(?M:listbox(Items, [a, c], [checkboxes, check_all, filterable, disabled],
                     [{id, m}, {check_all_label, <<"All">>}, {filter_placeholder, <<"Find">>}])),
    ?assert(has(<<"class=\"ah-listbox ah-listbox-checkboxes ah-listbox-disabled "
                  "ah-listbox-filterable\"">>, H)),
    ?assert(has(<<"data-ah-value=\"a,c\" tabindex=\"-1\" role=\"listbox\" "
                  "aria-multiselectable=\"true\" aria-disabled=\"true\"">>, H)),
    ?assert(has(<<"<input class=\"ah-listbox-filter-input\" type=\"text\" id=\"m-filter\" "
                  "autocomplete=\"off\" placeholder=\"Find\" aria-label=\"Find\" "
                  "aria-controls=\"m\" disabled data-listbox=\"m\" data-checkboxes=\"true\">">>, H)),
    ?assert(has(<<"<div class=\"ah-listbox-check-all\" role=\"button\" aria-pressed=\"false\">"
                  "<span class=\"ah-listbox-checkbox\"></span>"
                  "<span class=\"ah-listbox-label\">All</span></div>">>, H)),
    %% grouped in order of appearance; data-idx keeps the item position
    {match, [[G1], [G2]]} = re:run(H, <<"ah-listbox-group\" role=\"presentation\">([^<]+)<">>,
                                   [global, {capture, all_but_first, binary}]),
    ?assertEqual({<<"G1">>, <<"G2">>}, {G1, G2}),
    ?assert(has(<<"G1</li><li class=\"ah-listbox-item ah-listbox-item-selected\" id=\"m-o-0\"">>, H)),
    ?assert(has(<<"id=\"m-o-2\"">>, H)),
    ?assertEqual(2, count(<<"ah-listbox-checkbox ah-listbox-checkbox-checked">>, H)),
    M = r(?M:listbox([a, b], [b], [multiple], [])),
    ?assert(has(<<"class=\"ah-listbox ah-listbox-multiple\"">>, M)),
    ?assert(has(<<"data-ah-value=\"b\"">>, M)),
    %% check_all needs check boxes
    ?assertNot(has_quiet(<<"class=\"ah-listbox-check-all\"">>, r(?M:listbox([a], a, [check_all], [])))).

-spec action(atom(), term(), aihtml_action:event(), aihtml_action:ctx()) -> ok.
action(search, #{source := Source}, #{value := Q} = Ev, Ctx) ->
    ?M:listbox_items(Ctx, Ev, [I || I <- Source,
                                    binary:match(string:lowercase(I), Q) =/= nomatch]).

listbox_search_round_trip_test() ->
    Ref = {?MODULE, search, #{source => [<<"Apple">>, <<"Banana">>, <<"Grape">>]}},
    H = r(?M:listbox([], undefined, [checkboxes], [{id, <<"ls">>}, {search, Ref}])),
    ?assert(has(<<"ah-listbox-remote">>, H)),
    ?assert(has(<<"ah-listbox-filter-input">>, H)),     % search implies the filter
    {match, [Token]} = re:run(H, <<"data-ah-on=\"input:([^:\"]+):250\"">>,
                              [{capture, all_but_first, binary}]),
    {ok, Ref} = aihtml_action:verify(Token),
    Self = self(),
    Event = #{<<"type">> => <<"input">>, <<"id">> => <<"ls-filter">>, <<"value">> => <<"ap">>,
              <<"data">> => #{<<"listbox">> => <<"ls">>, <<"checkboxes">> => <<"true">>}},
    ok = aihtml_action:execute(Ref, Event, #{emit => fun(E) -> Self ! {ev, E} end}),
    [#{<<"value">> := [Html, Call]}] = [E || #{<<"type">> := <<"CUSTOM">>} = E <- collect()],
    #{op := html, swap := morph_inner, id := <<"ls-list">>, html := Rows} = Html,
    ?assertEqual(extract_list(r(?M:listbox([<<"Apple">>, <<"Grape">>], undefined, [checkboxes],
                                           [{id, <<"ls">>}]))), Rows),
    ?assertEqual(#{op => call, id => <<"ls">>, method => <<"itemsLoaded">>, args => []}, Call),
    Ops = aihtml_action:render_ops(
            fun(Ctx) -> ?M:listbox_items(Ctx, {id, x}, [a, b], #{selected => [b]}) end),
    [#{op := html, id := <<"x-list">>, html := H2}, #{op := call, id := <<"x">>}] = Ops,
    ?assert(has(<<"ah-listbox-item ah-listbox-item-selected\" id=\"x-o-1\"">>, H2)).

extract_list(H) ->
    {match, [Inner]} = re:run(H, <<"<ul class=\"ah-listbox-list\"[^>]*>(.*)</ul>">>,
                              [{capture, all_but_first, binary}]),
    Inner.

%%%===================================================================
%%% Catalog
%%%===================================================================

catalog_test() ->
    [#{name := listbox}] = ?M:catalog(),
    #{flags := Fl, options := Op, option_docs := Docs, methods := Ms} =
        aihtml_catalog:entry(?M, listbox),
    ?assertEqual(lists:sort(Fl ++ Op), lists:sort(maps:keys(Docs))),
    ?assert(lists:member(setValue, [Name || #{name := Name} <- Ms])),
    ?assert(erlang:function_exported(?M, listbox, 4)),
    [?assert(erlang:function_exported(?M, F, A)) || {F, A} <- ?M:facade_extras()].

%%%===================================================================
%%% element records (designs/05-records.md)
%%%===================================================================

record_equals_builder_test() ->
    ?assertEqual(r(?M:listbox([a, b], [a], [checkboxes, check_all],
                              [{id, l}, {check_all_label, <<"All">>}])),
                 r(#ah_listbox{items = [a, b], value = [a], checkboxes = true, check_all = true,
                               id = l, check_all_label = <<"All">>})).

builder_fills_fields_test() ->
    L = ?M:listbox([a], a, [multiple], [{empty_text, <<"-">>}, {name, n}]),
    ?assertMatch(#ah_listbox{multiple = true, empty_text = <<"-">>, name = n, attrs = []}, L).

generated_id_test() ->
    H = r(#ah_listbox{items = [a]}),
    {match, [Id]} = re:run(H, <<"^<div class=\"ah-listbox\" id=\"(ah-l[0-9]+)\"">>,
                           [{capture, all_but_first, binary}]),
    ?assert(has(<<"id=\"", Id/binary, "-o-0\"">>, H)).

token(Html) ->
    {match, [T]} = re:run(r(Html), <<"data-ah-on=\"(change:[^\"]+)\"">>,
                          [{capture, all_but_first, binary}]),
    [Ev, Tok] = binary:split(T, <<":">>),
    {ok, Ref} = aihtml_action:unsign(Tok),
    {Ev, Ref}.

postback_test() ->
    ?assertEqual({<<"change">>, {other_mod, pick, #{}}},
                 token(#ah_listbox{items = [a], postback = pick, delegate = other_mod})).

field_validation_test() ->
    ?assertError({aihtml, {bad_list_item, _}}, r(#ah_listbox{items = [{1, 2, 3}]})),
    ?assertError({aihtml, {unknown_modifier, listbox, big, _}},
                 ?M:listbox([], undefined, [big], [])).

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

default(ah_listbox) -> #ah_listbox{}.
