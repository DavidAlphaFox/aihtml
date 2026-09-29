%% Tests for aihtml_combobox. The module is also the fake action module
%% of the server-side search round trip.
-module(aihtml_combobox_tests).
-behaviour(aihtml_action).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_combobox.hrl").

-export([action/4]).

-define(M, aihtml_combobox).

r(Html) -> aihtml_html:render_binary(Html).

has(Needle, Hay) ->
    case binary:match(Hay, Needle) of
        nomatch -> ?debugFmt("~s not in~n~s", [Needle, Hay]), false;
        _ -> true
    end.

has_quiet(Needle, Hay) -> binary:match(Hay, Needle) =/= nomatch.

combobox_tag_template_test() ->
    H = r(?M:combobox([{<<"a&b">>, <<"A <&>">>}], [<<"a&b">>], [multiple], [])),
    ?assert(has(<<"<span class=\"ah-combobox-tag\"><span class=\"ah-combobox-tag-text\">A &lt;&amp;&gt;</span><span class=\"ah-combobox-tag-close\" data-value=\"a&amp;b\" role=\"button\" aria-label=\"Remove A &lt;&amp;&gt;\">&times;</span></span>">>, H)).

combobox_single_test() ->
    Items = [<<"Apple">>, {b, <<"Banana">>}, #{value => 3, label => <<"Cherry <3>">>,
                                               description => <<"red">>}],
    H = r(?M:combobox(Items, b, [<<"w-56">>], [{id, cb}, {name, fruit},
                                               {placeholder, <<"Pick">>}])),
    ?assert(has(<<"class=\"ah-combobox w-56\"">>, H)),
    ?assert(has(<<"data-ah=\"combobox\"">>, H)),
    ?assert(has(<<"data-ah-value=\"b\"">>, H)),
    ?assert(has(<<"<input type=\"hidden\" name=\"fruit\" value=\"b\">">>, H)),
    ?assert(has(<<"value=\"Banana\"">>, H)),               % the label is shown
    ?assert(has(<<"placeholder=\"Pick\"">>, H)),
    ?assert(has(<<"aria-controls=\"cb-list\"">>, H)),
    ?assert(has(<<"data-combobox=\"cb\"">>, H)),
    ?assert(has(<<"<ul class=\"ah-combobox-list\" id=\"cb-list\" role=\"listbox\">">>, H)),
    ?assert(has(<<"class=\"ah-combobox-item ah-combobox-item-selected\" id=\"cb-opt-1\" role=\"option\" aria-selected=\"true\"">>, H)),
    ?assert(has(<<"Cherry &lt;3&gt;">>, H)),               % escaped
    ?assert(has(<<"<div class=\"ah-combobox-item-desc\">red</div>">>, H)),
    ?assert(has(<<"data-value=\"3\"">>, H)),
    ?assert(has(<<"ah-combobox-arrow-icon">>, H)),
    ?assertNot(has_quiet(<<"data-ah-on">>, H)).

combobox_value_not_in_items_test() ->
    H = r(?M:combobox([], <<"zed">>, [], [])),
    ?assert(has(<<"value=\"zed\"">>, H)),
    ?assert(has(<<"data-ah-value=\"zed\"">>, H)).

combobox_multi_test() ->
    H = r(?M:combobox([<<"a">>, <<"b">>, <<"c">>], [<<"a">>, <<"c">>], [checkboxes],
                      [{name, n}, {placeholder, <<"P">>}])),
    ?assert(has(<<"ah-combobox-checkboxes">>, H)),
    ?assert(has(<<"data-ah-value=\"a,c\"">>, H)),
    ?assert(has(<<"name=\"n\" value=\"a,c\"">>, H)),
    ?assert(has(<<"aria-multiselectable=\"true\"">>, H)),
    ?assertEqual(2, length(binary:matches(H, <<"class=\"ah-combobox-tag\"">>))),
    ?assertEqual(2, length(binary:matches(H, <<"ah-combobox-checkbox ah-combobox-checkbox-checked">>))),
    ?assertEqual(1, length(binary:matches(H, <<"class=\"ah-combobox-checkbox\"">>))),
    %% the placeholder moves to the root while tags are shown
    ?assert(has(<<"data-ah-placeholder=\"P\"">>, H)),
    ?assertNot(has_quiet(<<" placeholder=">>, H)).

combobox_groups_test() ->
    Items = [#{value => 1, label => <<"A">>, group => <<"G1">>},
             #{value => 2, label => <<"B">>, group => <<"G2">>},
             #{value => 3, label => <<"C">>, group => <<"G1">>, disabled => true}],
    H = r(?M:combobox(Items, undefined, [], [{id, g}])),
    [{P1, _}, {P2, _}] = binary:matches(H, <<"ah-combobox-group-header">>),
    {PA, _} = binary:match(H, <<"data-value=\"1\"">>),
    {PC, _} = binary:match(H, <<"data-value=\"3\"">>),
    {PB, _} = binary:match(H, <<"data-value=\"2\"">>),
    %% G1 (A, C) then G2 (B): grouped in order of first appearance
    ?assert(P1 < PA andalso PA < PC andalso PC < P2 andalso P2 < PB),
    ?assert(has(<<"ah-combobox-item ah-combobox-item-disabled\" id=\"g-opt-1\"">>, H)),
    ?assert(has(<<"data-group=\"G1\"">>, H)).

combobox_flags_options_test() ->
    H = r(?M:combobox([<<"x">>], undefined, [disabled, no_arrow, free_text],
                      [{search_mode, starts_with}, {min_length, 2},
                       {empty_text, <<"Nothing">>}, {dropdown_height, 120}])),
    ?assert(has(<<"class=\"ah-combobox ah-combobox-disabled ah-combobox-free-text ah-combobox-no-arrow\"">>, H)),
    ?assert(has(<<"data-ah-search-mode=\"starts_with\"">>, H)),
    ?assert(has(<<"data-ah-min-length=\"2\"">>, H)),
    ?assert(has(<<"data-ah-empty=\"Nothing\"">>, H)),
    ?assert(has(<<"style=\"max-height:120px\"">>, H)),
    ?assertError({aihtml, {bad_search_mode, fuzzy}},
                 r(?M:combobox([], undefined, [], [{search_mode, fuzzy}]))),
    ?assertError({aihtml, {bad_combobox_item, _}},
                 r(?M:combobox([{1, 2, 3}], undefined, [], []))).

%%%===================================================================
%%% Server-side search: render, verify the token, run the action
%%%===================================================================

action(search, #{source := Source}, #{value := Q} = Ev, Ctx) ->
    Lower = string:lowercase(Q),
    ?M:set_items(Ctx, Ev, [I || I <- Source,
                                binary:match(string:lowercase(I), Lower) =/= nomatch]).

search_round_trip_test() ->
    Ref = {?MODULE, search, #{source => [<<"Apple">>, <<"Apricot">>, <<"Banana">>]}},
    H = r(?M:combobox([], undefined, [], [{id, <<"cb-s">>}, {search, Ref}])),
    ?assert(has(<<"data-ah-remote">>, H)),
    %% the input carries the debounced action: input:TOKEN:250
    {match, [Token]} = re:run(H, <<"data-ah-on=\"input:([^:\"]+):250\"">>,
                              [{capture, all_but_first, binary}]),
    {ok, Ref} = aihtml_action:verify(Token),
    Self = self(),
    Event = #{<<"type">> => <<"input">>, <<"id">> => <<"cb-s-input">>,
              <<"value">> => <<"AP">>, <<"data">> => #{<<"combobox">> => <<"cb-s">>}},
    ok = aihtml_action:execute(Ref, Event, #{emit => fun(E) -> Self ! {ev, E} end}),
    Evs = collect(),
    ?assertEqual([<<"RUN_STARTED">>, <<"CUSTOM">>, <<"RUN_FINISHED">>],
                 [maps:get(<<"type">>, E) || E <- Evs]),
    [#{<<"value">> := [Html, Call]}] = [E || #{<<"type">> := <<"CUSTOM">>} = E <- Evs],
    %% the items, rendered like the first render, morphed into the list
    #{op := html, swap := morph_inner, id := <<"cb-s-list">>, html := Items} = Html,
    ?assertEqual(extract_list(r(?M:combobox([<<"Apple">>, <<"Apricot">>], undefined, [],
                                            [{id, <<"cb-s">>}]))),
                 Items),
    ?assert(has(<<"id=\"cb-s-opt-1\" role=\"option\" aria-selected=\"false\" data-index=\"1\" data-value=\"Apricot\"">>, Items)),
    ?assertNot(has_quiet(<<"Banana">>, Items)),
    ?assertEqual(#{op => call, id => <<"cb-s">>, method => <<"itemsLoaded">>, args => []}, Call),
    %% what the transport sends is valid JSON
    _ = iolist_to_binary(json:encode([Html, Call])).

%% The <ul> content of a rendered combobox.
extract_list(H) ->
    {match, [Inner]} = re:run(H, <<"<ul class=\"ah-combobox-list\"[^>]*>(.*)</ul>">>,
                              [{capture, all_but_first, binary}]),
    Inner.

search_checkboxes_test() ->
    Ops = aihtml_action:render_ops(
            fun(Ctx) ->
                    ?M:set_items(Ctx, #{data => #{<<"combobox">> => <<"c">>,
                                                  <<"checkboxes">> => <<"true">>}}, [<<"a">>])
            end),
    [#{html := H}, _] = Ops,
    ?assert(has(<<"class=\"ah-combobox-checkbox\"">>, H)),
    %% the input tells the search action about check boxes
    ?assert(has(<<"data-checkboxes=\"true\"">>,
                r(?M:combobox([], undefined, [checkboxes], [{search, {?MODULE, search, #{}}}])))).

collect() ->
    receive {ev, E} -> [E | collect()]
    after 0 -> []
    end.

set_items_targets_test() ->
    Ops = aihtml_action:render_ops(
            fun(Ctx) ->
                    ?M:set_items(Ctx, {id, cb}, [#{value => 1, label => <<"One">>,
                                                    description => <<"d">>, group => g,
                                                    disabled => true}]),
                    ?M:set_items(Ctx, {id, <<"x">>}, [<<"a">>, <<"b">>],
                                 #{selected => [b], checkboxes => true})
            end),
    [#{op := html, swap := morph_inner, id := <<"cb-list">>, html := H1},
     #{op := call, id := <<"cb">>, method := <<"itemsLoaded">>},
     #{op := html, id := <<"x-list">>, html := H2},
     #{op := call, id := <<"x">>}] = Ops,
    ?assert(has(<<"<li class=\"ah-combobox-group-header\" role=\"presentation\">g</li>">>, H1)),
    ?assert(has(<<"ah-combobox-item-disabled">>, H1)),
    ?assert(has(<<"<div class=\"ah-combobox-item-desc\">d</div>">>, H1)),
    ?assert(has(<<"ah-combobox-item ah-combobox-item-selected\" id=\"x-opt-1\"">>, H2)),
    ?assert(has(<<"ah-combobox-checkbox-checked">>, H2)),
    ?assertError(function_clause, aihtml_action:render_ops(
                                    fun(Ctx) -> ?M:set_items(Ctx, <<"#cb">>, []) end)).

%%%===================================================================
%%% Catalog
%%%===================================================================

catalog_test() ->
    [#{name := combobox}] = ?M:catalog(),
    E = aihtml_catalog:entry(?M, combobox),
    #{flags := Flags} = E,
    [_ | _] = aihtml_catalog:classes(E, Flags),
    ?assertEqual([{set_items, 3}, {set_items, 4}], ?M:facade_extras()),
    %% every option and flag is documented, every behaviour method listed
    #{flags := Fl, options := Op, option_docs := Docs, methods := Ms} = E,
    ?assertEqual(lists:sort(Fl ++ Op), lists:sort(maps:keys(Docs))),
    ?assert(lists:member(setValue, [Name || #{name := Name} <- Ms])),
    [?assert(erlang:function_exported(?M, F, A)) || {F, A} <- ?M:facade_extras()].

%%%===================================================================
%%% element records (designs/05-records.md)
%%%===================================================================

record_equals_builder_test() ->
    Items = [<<"a">>, {b, <<"B">>}, #{value => c, group => <<"G">>}],
    ?assertEqual(r(?M:combobox(Items, [a, c], [checkboxes, no_arrow],
                               [{id, <<"cb">>}, {name, n}, {placeholder, <<"P">>},
                                {search_mode, starts_with}, {dropdown_height, 100},
                                {search, {?MODULE, search, #{}}}])),
                 r(#ah_combobox{items = Items, value = [a, c], checkboxes = true,
                                no_arrow = true, id = <<"cb">>, name = n, placeholder = <<"P">>,
                                search_mode = starts_with, dropdown_height = 100,
                                search = {?MODULE, search, #{}}})).

builder_fills_fields_test() ->
    C = ?M:combobox([<<"a">>], <<"a">>, [free_text], [{min_length, 2}, {empty_text, <<"-">>}]),
    ?assertMatch(#ah_combobox{items = [<<"a">>], value = <<"a">>, free_text = true,
                              min_length = 2, empty_text = <<"-">>, id = undefined,
                              attrs = []}, C),
    ?assertError({aihtml, {record_only_field, ah_combobox, postback}},
                 ?M:combobox([], undefined, [], [{postback, pick}])).

generated_id_test() ->
    %% no id: one is generated at render, and the parts refer to it
    H = r(#ah_combobox{items = [<<"a">>], postback = pick}),
    {match, [Id]} = re:run(H, <<"^<div class=\"ah-combobox\" id=\"(ah-p[0-9]+)\"">>,
                           [{capture, all_but_first, binary}]),
    ?assert(has(<<"aria-controls=\"", Id/binary, "-list\"">>, H)),
    ?assertEqual(1, length(binary:matches(H, <<" id=\"", Id/binary, "\"">>))).

postback_test() ->
    Token = fun(Html) ->
                    {match, [T]} = re:run(r(Html), <<"data-ah-on=\"([a-z]+:[^\"]+)\"">>,
                                          [{capture, all_but_first, binary}]),
                    [Ev, Tok] = binary:split(T, <<":">>),
                    {ok, Ref} = aihtml_action:unsign(Tok),
                    {Ev, Ref}
            end,
    ?assertEqual({<<"change">>, {other_mod, pick, #{}}},
                 Token(#ah_combobox{items = [<<"a">>], postback = pick,
                                    delegate = other_mod})),
    %% the id stays first on the root, the postback follows its own attributes
    H = r(#ah_combobox{id = c, postback = pick}),
    ?assertMatch({match, _}, re:run(H, <<"^<div class=\"ah-combobox\" id=\"c\" "
                                         "data-ah=\"combobox\"[^>]* data-ah-on=\"change:">>)).

field_validation_test() ->
    ?assertError({aihtml, {bad_search_mode, fuzzy}}, r(#ah_combobox{search_mode = fuzzy})),
    ?assertError({aihtml, {bad_combobox_item, _}}, r(#ah_combobox{items = [{1, 2, 3}]})),
    ?assertError({aihtml, {modifier_in_css, combobox, multiple}},
                 r(#ah_combobox{css = [multiple]})),
    %% modifier names still fail in the builder
    ?assertError({aihtml, {unknown_modifier, combobox, big, _}},
                 ?M:combobox([], undefined, [big], [])).

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

default(ah_combobox) -> #ah_combobox{}.

%% A value containing a comma is escaped in data-ah-value (aihtml_value).
vhas(Sub, Bin) -> binary:match(Bin, Sub) =/= nomatch.

comma_values_test() ->
    Items = [<<"a,b">>, <<"c">>, <<"d\\e">>],
    M = r(?M:combobox(Items, [<<"a,b">>, <<"d\\e">>], [multiple], [{name, k}])),
    ?assert(vhas(<<"data-ah-value=\"a\\,b,d\\\\e\"">>, M)),
    ?assert(vhas(<<"name=\"k\" value=\"a\\,b,d\\\\e\"">>, M)),
    %% a single value is written as it is
    S = r(?M:combobox(Items, <<"a,b">>, [], [])),
    ?assert(vhas(<<"data-ah-value=\"a,b\"">>, S)).
