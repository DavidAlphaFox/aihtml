-module(aihtml_menu_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_menu.hrl").

-define(M, aihtml_menu).

r(Html) -> aihtml_html:render_binary(Html).

has(Bin, Sub) -> binary:match(Bin, Sub) =/= nomatch.

count(Bin, Sub) -> length(binary:matches(Bin, Sub)).

%%%===================================================================
%%% catalog
%%%===================================================================

catalog_names_test() ->
    ?assertEqual([menu], [N || #{name := N} <- ?M:catalog()]).

catalog_entries_are_valid_test() ->
    [begin
         E = aihtml_catalog:entry(?M, N),
         ?assertMatch(#{category := layout, root := <<"ah-", _/binary>>}, E),
         ?assert(is_list(aihtml_catalog:classes(E, [])))
     end || #{name := N} <- ?M:catalog()].

catalog_docs_test() ->
    [begin
         ?assertMatch(#{option_docs := #{}, methods := _}, E),
         Documented = maps:keys(maps:get(option_docs, E)),
         Opts = maps:get(options, E, []) ++ maps:get(flags, E, []) ++
             lists:append([Ms || {Ms, _} <- maps:values(maps:get(groups, E, #{}))]),
         ?assertEqual([], Opts -- Documented)
     end || E <- ?M:catalog()].

unknown_modifier_fails_test() ->
    ?assertError({aihtml, {unknown_modifier, menu, bogus, _}},
                 ?M:menu([], undefined, [bogus], [])),
    ?assertError({aihtml, {conflicting_modifiers, menu, mode, _}},
                 ?M:menu([], undefined, [vertical, popup], [])).

%%%===================================================================
%%% menu
%%%===================================================================

menu_structure_test() ->
    H = r(?M:menu([#{key => file, label => <<"File">>,
                     children => [{new, <<"New">>}, divider,
                                  #{key => x, label => <<"X">>, disabled => true}]},
                   #{key => help, label => <<"Help">>, href => <<"/help">>}],
                  new, [], [{name, <<"cmd">>}, {id, <<"m">>}])),
    ?assert(has(H, <<"class=\"ah-menu ah-menu-horizontal\"">>)),
    ?assert(has(H, <<"data-ah=\"menu\"">>)),
    ?assert(has(H, <<"role=\"menubar\"">>)),
    ?assert(has(H, <<"data-ah-value=\"new\"">>)),
    ?assert(has(H, <<"<input type=\"hidden\" name=\"cmd\" value=\"new\">">>)),
    ?assert(has(H, <<"ah-menu-item ah-menu-has-submenu">>)),
    ?assert(has(H, <<"aria-haspopup=\"true\" aria-expanded=\"false\"">>)),
    ?assert(has(H, <<"<ul class=\"ah-menu-submenu\" role=\"menu\">">>)),
    ?assert(has(H, <<"<li class=\"ah-menu-separator\" role=\"separator\"></li>">>)),
    ?assert(has(H, <<"ah-menu-item ah-menu-item-disabled">>)),
    ?assert(has(H, <<"class=\"ah-menu-link ah-menu-link-active\"">>)),
    ?assert(has(H, <<"href=\"/help\"">>)),
    ?assert(has(H, <<"ah-menu-minimized-btn">>)),
    ?assert(has(H, <<"id=\"m\"">>)).

menu_modes_and_options_test() ->
    V = r(?M:menu([{a, <<"A">>}], undefined, [vertical, show_arrows, disabled],
                  [{click_to_open, true}, {keyboard, false}, {minimize_width, 600},
                   {title, <<"Main">>}])),
    ?assert(has(V, <<"ah-menu ah-menu-vertical ah-menu-disabled ah-menu-show-arrows">>)),
    ?assert(has(V, <<"role=\"menu\"">>)),
    ?assert(has(V, <<"data-ah-click-to-open">>)),
    ?assert(has(V, <<"data-ah-keyboard=\"false\"">>)),
    ?assert(has(V, <<"data-ah-minimize-width=\"600\"">>)),
    ?assert(has(V, <<"data-title=\"Main\"">>)),
    ?assert(has(V, <<"aria-label=\"Main\"">>)),
    ?assertNot(has(V, <<"data-ah-value">>)),
    ?assertNot(has(V, <<"click_to_open">>)),
    P = r(?M:menu([], undefined, [popup], [{popup_target, <<"#area">>}])),
    ?assert(has(P, <<"ah-menu-popup">>)),
    ?assert(has(P, <<"data-ah-popup-target=\"#area\"">>)).

menu_columns_and_direction_test() ->
    H = r(?M:menu([#{key => v, label => <<"V">>, open => [left, up],
                     columns => [#{header => <<"H1">>, children => [{a, <<"A">>}]},
                                 #{children => [{b, <<"B">>}]}]}],
                  undefined, [], [])),
    ?assert(has(H, <<"ah-menu-submenu ah-menu-columns ah-menu-open-left ah-menu-open-up">>)),
    ?assertEqual(2, count(H, <<"class=\"ah-menu-column\"">>)),
    ?assert(has(H, <<"<div class=\"ah-menu-column-header\">H1</div>">>)).

menu_escapes_and_icons_test() ->
    H = r(?M:menu([#{key => <<"a\"b">>, label => <<"<b>">>, icon => <<"/i.png">>},
                   #{key => 1, label => <<"One">>, icon => {safe, <<"<svg></svg>">>}}],
                  1, [], [])),
    ?assert(has(H, <<"&lt;b&gt;">>)),
    ?assert(has(H, <<"data-id=\"a&quot;b\"">>)),
    ?assert(has(H, <<"<img class=\"ah-menu-icon\" src=\"/i.png\" alt=\"\">">>)),
    ?assert(has(H, <<"<span class=\"ah-menu-icon\" aria-hidden=\"true\"><svg></svg></span>">>)),
    ?assert(has(H, <<"data-ah-value=\"1\"">>)).


-define(ITEMS, [#{key => file, label => <<"File">>, children => [{new, <<"New">>}]},
                {help, <<"Help">>}]).

%%%===================================================================
%%% element record (designs/05-records.md)
%%%===================================================================

record_equals_builder_test() ->
    ?assertEqual(r(?M:menu(?ITEMS, new, [vertical, show_arrows, <<"mt-2">>],
                           [{name, cmd}, {keyboard, false}, {id, m}, {title, <<"Main">>},
                            {aria_label, <<"x">>}])),
                 r(#ah_menu{items = ?ITEMS, value = new, mode = vertical, show_arrows = true,
                            css = [<<"mt-2">>], name = cmd, keyboard = false, id = m,
                            title = <<"Main">>, attrs = [{aria_label, <<"x">>}]})).

builder_fills_fields_test() ->
    ?assertError({aihtml, {record_only_field, ah_menu, postback}},
                 ?M:menu([], undefined, [], [{postback, go}])).

token(Html) ->
    {match, [T]} = re:run(r(Html), <<"data-ah-on=\"([a-z]+:[^\"]+)\"">>,
                          [{capture, all_but_first, binary}]),
    [Ev, Tok] = binary:split(T, <<":">>),
    {ok, Ref} = aihtml_action:unsign(Tok),
    {Ev, Ref}.

postback_test() ->
    ?assertEqual({<<"change">>, {?MODULE, go, #{}}},
                 token(#ah_menu{items = ?ITEMS, postback = go})).

field_validation_test() ->
    ?assertError({aihtml, {bad_modifier, menu, mode, sideways, _}},
                 r(#ah_menu{mode = sideways})).

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

default(ah_menu) -> #ah_menu{}.
