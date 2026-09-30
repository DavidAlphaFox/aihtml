-module(aihtml_tabs_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_tabs.hrl").

-export([action/4]).

-define(M, aihtml_tabs).

-define(TABS, [{a, <<"A">>, <<"pa">>}, {b, <<"B">>, <<"pb">>, #{disabled => true}}]).

r(H) -> aihtml_html:render_binary(H).

has(Bin, Part) -> binary:match(Bin, Part) =/= nomatch.

count(Bin, Part) -> length(binary:matches(Bin, Part)).

%%% catalog

catalog_matches_exports_test() ->
    Exports = ?M:module_info(exports),
    Names = [N || #{name := N} <- ?M:catalog()],
    ?assertEqual([tabs], Names),
    [?assert(lists:keymember(aihtml_catalog:builder(N), 1, Exports)) || N <- Names],
    [?assertMatch(#{category := layout, root := <<"ah-", _/binary>>, signature := _}, E)
     || E <- ?M:catalog()].

catalog_documents_every_option_test() ->
    [begin
         Docs = maps:get(option_docs, E),
         ?assertEqual({N, []}, {N, [K || K <- maps:get(options, E, []) ++ maps:get(flags, E, []),
                                         not maps:is_key(K, Docs)]}),
         ?assert(is_list(maps:get(methods, E)))
     end || #{name := N} = E <- ?M:catalog()].

%%% tabs

tabs_test() ->
    H = r(?M:ah_tabs([{a, <<"A">>, <<"pa">>}, {<<"b">>, <<"B">>, <<"pb">>},
                      {c, <<"C">>, <<"pc">>, #{disabled => true}}],
                     b, [left], [{id, t}, {name, tab}])),
    ?assert(has(H, <<"class=\"ah-tabs ah-tabs-left\" id=\"t\" data-ah=\"tabs\" data-ah-value=\"b\"">>)),
    ?assert(has(H, <<"role=\"tablist\" aria-orientation=\"vertical\"">>)),
    ?assert(has(H, <<"class=\"ah-tabs-item ah-tabs-item-selected\" id=\"t-tab-1\" role=\"tab\" "
                     "data-key=\"b\" tabindex=\"0\" aria-selected=\"true\" aria-controls=\"t-panel-1\"">>)),
    ?assert(has(H, <<"ah-tabs-item ah-tabs-item-disabled">>)),
    ?assertEqual(2, count(H, <<"style=\"display:none\"">>)),
    ?assert(has(H, <<"class=\"ah-tabs-panel ah-tabs-panel-active\" id=\"t-panel-1\"">>)),
    ?assert(has(H, <<"<input type=\"hidden\" name=\"tab\" value=\"b\">">>)),
    ?assertNot(has(H, <<" name=\"tab\" data">>)).

tabs_default_active_skips_disabled_test() ->
    H = r(?M:ah_tabs([{a, <<"A">>, <<>>, #{disabled => true}}, {b, <<"B">>, <<>>}], undefined, [],
                     [{scrollable, true}])),
    ?assert(has(H, <<"data-ah-value=\"b\"">>)),
    ?assert(has(H, <<"ah-tabs ah-tabs-top ah-tabs-scrollable">>)),
    ?assertEqual(2, count(H, <<"ah-tabs-scroll-btn">>)).

%%% CSS: every sigil class the module writes exists in the stylesheets

classes_are_styled_test() ->
    %% the sources when run from the project root, else the installed copy
    Root = "apps/aihtml/priv/css",
    Dir = case filelib:is_dir(Root) of
              true -> Root;
              false -> filename:join(code:priv_dir(aihtml), "css")
          end,
    Css = iolist_to_binary([element(2, file:read_file(F))
                            || F <- filelib:wildcard(filename:join([Dir, "**", "*.css"]))]),
    Html = iolist_to_binary([r(H) || H <- samples()]),
    {ok, Re} = re:compile(<<"class=\"([^\"]*)\"">>),
    {match, Ms} = re:run(Html, Re, [global, {capture, [1], binary}]),
    Classes = lists:usort([C || [Cs] <- Ms, C <- binary:split(Cs, <<" ">>, [global, trim_all]),
                                binary:match(C, <<"ah-">>) =:= {0, 3}]),
    Markers = [],
    Missing = [C || C <- Classes -- Markers, not has(Css, <<".", C/binary>>)],
    ?assertEqual([], Missing).

%% One render of the component in its main variants and states.
samples() ->
    Tabs = [{a, <<"A">>, <<"a">>}, {b, <<"B">>, <<"b">>, #{disabled => true}}],
    [?M:ah_tabs(Tabs, a, [P, disabled], [{scrollable, true}]) || P <- [top, bottom, left, right]].

%%% element records (designs/05-records.md)

-spec action(atom(), term(), map(), term()) -> ok.
action(_, _, _, _) -> ok.

token(Html) ->
    {match, [T]} = re:run(r(Html), <<"data-ah-on=\"([a-z]+:[^\"]+)\"">>,
                          [{capture, all_but_first, binary}]),
    [Ev, Tok] = binary:split(T, <<":">>),
    {ok, Ref} = aihtml_action:unsign(Tok),
    {Ev, Ref}.

unknown_modifier_test() ->
    ?assertError({aihtml, {conflicting_modifiers, tabs, position, _}},
                 ?M:ah_tabs([], undefined, [left, right], [])).

record_equals_builder_test() ->
    ?assertEqual(r(?M:ah_tabs(?TABS, b, [left], [{id, t}, {scrollable, true}])),
                 r(#ah_tabs{items = ?TABS, value = b, position = left, id = t,
                            scrollable = true})).

generated_ids_test() ->
    %% an id among the attrs is used too
    T = r(#ah_tabs{items = ?TABS, attrs = [{<<"id">>, <<"t">>}]}),
    ?assert(has(T, <<"class=\"ah-tabs ah-tabs-top\" id=\"t\" data-ah=\"tabs\"">>)),
    ?assert(has(T, <<"id=\"t-panel-0\"">>)).

postback_test() ->
    ?assertEqual({<<"change">>, {?MODULE, tab, #{id => 1}}},
                 token(#ah_tabs{items = ?TABS, id = t, postback = {tab, #{id => 1}}})).

field_validation_test() ->
    ?assertError({aihtml, {bad_modifier, tabs, position, middle, _}},
                 r(#ah_tabs{id = t, position = middle})),
    ?assertError({aihtml, {bad_option, selection_mode, drag}},
                 r(#ah_tabs{id = t, selection_mode = drag})).

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

default(ah_tabs) -> #ah_tabs{}.
