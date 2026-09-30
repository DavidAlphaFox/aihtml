-module(aihtml_dropdownlist_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_dropdownlist.hrl").

-export([action/4]).

-define(M, aihtml_dropdownlist).

r(Html) -> aihtml_html:render_binary(Html).

has(Bin, Sub) -> binary:match(Bin, Sub) =/= nomatch.

dropdownlist_markup_test() ->
    H = r(?M:ah_dropdownlist([{a, <<"Alpha">>}, b], b, [primary, <<"w-40">>],
                             [{name, pick}, {id, <<"dd">>}])),
    ?assert(has(H, <<"class=\"ah-dropdownlist ah-dropdownlist-primary w-40\"">>)),
    ?assert(has(H, <<"role=\"combobox\"">>)),
    ?assert(has(H, <<"aria-expanded=\"false\"">>)),
    ?assert(has(H, <<"data-ah=\"dropdownlist\"">>)),
    ?assert(has(H, <<"data-ah-value=\"b\"">>)),
    ?assert(has(H, <<"<span class=\"ah-dropdownlist-content\">b</span>">>)),
    ?assert(has(H, <<"<input type=\"hidden\" name=\"pick\" value=\"b\">">>)),
    ?assert(has(H, <<"ah-listbox-item ah-listbox-item-selected\" role=\"option\" data-idx=\"1\" data-value=\"b\" aria-selected=\"true\"">>)),
    ?assert(has(H, <<"ah-dropdownlist-arrow">>)),
    ?assertNot(has(H, <<" name=\"pick\" role">>)).

dropdownlist_placeholder_test() ->
    H = r(?M:ah_dropdownlist([a], undefined, [], [{placeholder, <<"Pick">>}])),
    ?assert(has(H, <<"ah-dropdownlist-content-placeholder\">Pick</span>">>)),
    ?assert(has(H, <<"data-ah-value=\"\"">>)),
    ?assertNot(has(H, <<"type=\"hidden\"">>)).

dropdownlist_groups_filter_disabled_test() ->
    H = r(?M:ah_dropdownlist([{group, <<"G">>, [{x, <<"X">>, #{disabled => true}}, y]}], y,
                             [simple, disabled], [{filterable, true}, {dropdown_height, 120}])),
    ?assert(has(H, <<"ah-dropdownlist-simple">>)),
    ?assert(has(H, <<"ah-dropdownlist-disabled">>)),
    ?assert(has(H, <<"tabindex=\"-1\"">>)),
    ?assert(has(H, <<"aria-disabled=\"true\"">>)),
    ?assertNot(has(H, <<"ah-dropdownlist-arrow">>)),
    ?assert(has(H, <<"<li class=\"ah-listbox-group\" role=\"presentation\">G</li>">>)),
    ?assert(has(H, <<"ah-listbox-item-disabled">>)),
    ?assert(has(H, <<"ah-listbox-filter-input">>)),
    ?assert(has(H, <<"max-height:120px">>)).

dropdownlist_escapes_test() ->
    H = r(?M:ah_dropdownlist([{<<"<v>">>, <<"<b>">>}], <<"<v>">>, [], [])),
    ?assertNot(has(H, <<"<b>">>)),
    ?assert(has(H, <<"&lt;b&gt;">>)).

dropdownlist_bad_modifier_test() ->
    ?assertError({aihtml, {unknown_modifier, dropdownlist, huge, _}},
                 ?M:ah_dropdownlist([a], a, [huge], [])).

%%% catalog

catalog_test() ->
    Names = [N || #{name := N} <- ?M:catalog()],
    ?assertEqual([dropdownlist], Names),
    [begin
         ?assert(is_binary(maps:get(signature, E))),
         ?assert(erlang:function_exported(?M, aihtml_catalog:builder(N), 4))
     end || #{name := N} = E <- ?M:catalog()].

catalog_docs_test() ->
    [begin
         Docs = maps:get(option_docs, E),
         [?assert(maps:is_key(O, Docs)) || O <- maps:get(options, E, [])],
         ?assert(is_list(maps:get(methods, E)))
     end || E <- ?M:catalog()].

%%% element records (designs/05-records.md)

-define(FRUITS, [{apple, <<"Apple">>}, {group, <<"G">>, [b, {c, <<"C">>, #{disabled => true}}]}]).

-spec action(atom(), term(), map(), term()) -> ok.
action(_, _, _, _) -> ok.

record_equals_builder_test() ->
    ?assertEqual(r(?M:ah_dropdownlist(?FRUITS, b, [success, simple, <<"w-40">>],
                                      [{name, pick}, {id, dd}, {filterable, true},
                                       {title, <<"t">>}])),
                 r(#ah_dropdownlist{items = ?FRUITS, value = b, template = success,
                                    simple = true, css = [<<"w-40">>], name = pick, id = dd,
                                    filterable = true, attrs = [{title, <<"t">>}]})).

builder_fills_fields_test() ->
    D = ?M:ah_dropdownlist([a], a, [danger, disabled, <<"x">>],
                           [{name, n}, {placeholder, <<"P">>}, {dropdown_height, 90},
                            {id, i}, {title, <<"t">>}]),
    ?assertMatch(#ah_dropdownlist{template = danger, disabled = true, simple = false,
                                  name = n, placeholder = <<"P">>, dropdown_height = 90,
                                  id = i, css = [<<"x">>], attrs = [{title, <<"t">>}]}, D).

postback_test() ->
    Token = fun(Html) ->
                    {match, [T]} = re:run(r(Html), <<"data-ah-on=\"([a-z]+:[^\"]+)\"">>,
                                          [{capture, all_but_first, binary}]),
                    [Ev, Tok] = binary:split(T, <<":">>),
                    {ok, Ref} = aihtml_action:unsign(Tok),
                    {Ev, Ref}
            end,
    ?assertEqual({<<"change">>, {?MODULE, pick, #{}}},
                 Token(#ah_dropdownlist{items = [a], postback = pick})).

field_validation_test() ->
    ?assertError({aihtml, {bad_modifier, dropdownlist, template, info, _}},
                 r(#ah_dropdownlist{template = info})),
    ?assertError({aihtml, {bad_flag, dropdownlist, simple, yes}},
                 r(#ah_dropdownlist{simple = yes})),
    ?assertError({aihtml, {bad_item, {1, 2, 3, 4}}},
                 r(#ah_dropdownlist{items = [{1, 2, 3, 4}]})),
    %% groups without a default may stay undefined
    ?assert(has(r(#ah_dropdownlist{}), <<"class=\"ah-dropdownlist\"">>)).

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

default(ah_dropdownlist) -> #ah_dropdownlist{}.
