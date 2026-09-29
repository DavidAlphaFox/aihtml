-module(aihtml_layout_nav_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_layout_nav.hrl").

-define(M, aihtml_layout_nav).

r(Html) -> aihtml_html:render_binary(Html).

has(Bin, Sub) -> binary:match(Bin, Sub) =/= nomatch.

count(Bin, Sub) -> length(binary:matches(Bin, Sub)).

%%%===================================================================
%%% catalog / examples
%%%===================================================================

catalog_names_test() ->
    ?assertEqual([menu, navbar, sidenav, toolbar, splitter, listmenu, status_bar],
                 [N || #{name := N} <- ?M:catalog()]).

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

%%%===================================================================
%%% navbar
%%%===================================================================

navbar_test() ->
    H = r(?M:navbar([{home, <<"Home">>}, #{key => docs, label => <<"Docs">>, href => <<"/d">>},
                     #{key => off, label => <<"Off">>, disabled => true}],
                    home, [], [{brand, <<"Acme">>}, {extra, <<"X">>},
                               {columns, [<<"30%">>]}, {name, <<"nav">>}])),
    ?assert(has(H, <<"class=\"ah-navbar\"">>)),
    ?assert(has(H, <<"role=\"tablist\"">>)),
    ?assert(has(H, <<"data-ah-value=\"home\"">>)),
    ?assert(has(H, <<"class=\"ah-navbar-item ah-navbar-item-selected\"">>)),
    ?assert(has(H, <<"aria-selected=\"true\"">>)),
    ?assert(has(H, <<"flex:none;width:30%;">>)),
    ?assert(has(H, <<"<a class=\"ah-navbar-item\"">>)),
    ?assert(has(H, <<"href=\"/d\"">>)),
    ?assert(has(H, <<"ah-navbar-item-disabled">>)),
    ?assert(has(H, <<"<div class=\"ah-navbar-brand\">Acme</div>">>)),
    ?assert(has(H, <<"<div class=\"ah-navbar-extra\">X</div>">>)),
    ?assert(has(H, <<"ah-navbar-header">>)),
    ?assertEqual(3, count(H, <<"ah-navbar-toggle-bar">>)),
    ?assert(has(H, <<"name=\"nav\" value=\"home\"">>)).

navbar_minimized_vertical_test() ->
    M = r(?M:navbar([{a, <<"A">>}], undefined, [minimized], [{title, <<"T">>},
                                                              {minimized_height, 40}])),
    ?assert(has(M, <<"ah-navbar ah-navbar-minimized">>)),
    ?assert(has(M, <<"data-ah-minimized=\"static\"">>)),
    ?assert(has(M, <<"height:40px;">>)),
    ?assert(has(M, <<"<span class=\"ah-navbar-title\">T</span>">>)),
    V = r(?M:navbar([{a, <<"A">>}], undefined, [vertical], [{selection, false}])),
    ?assert(has(V, <<"ah-navbar ah-navbar-vertical">>)),
    ?assert(has(V, <<"aria-orientation=\"vertical\"">>)),
    ?assert(has(V, <<"data-ah-selection=\"false\"">>)).

%%%===================================================================
%%% sidenav
%%%===================================================================

sidenav_test() ->
    Groups = [#{label => <<"Main">>,
                items => [{home, <<"Home">>},
                          #{key => users, label => <<"Users">>,
                            children => [{list, <<"List">>}, {roles, <<"Roles">>}]},
                          #{key => ext, label => <<"Ext">>, href => <<"https://x">>}]},
              #{items => [#{key => s, label => <<"S">>, children => [{t, <<"T">>}]}]}],
    H = r(?M:sidenav(Groups, roles, [],
                     [{brand, #{name => <<"Sigil">>, href => <<"/">>}},
                      {footer, <<"F">>}, {collapsible, true}])),
    ?assert(has(H, <<"<aside class=\"ah-sidenav\" data-ah=\"sidenav\" data-ah-value=\"roles\">">>)),
    ?assert(has(H, <<"<a class=\"ah-sidenav__brand\" href=\"/\">">>)),
    ?assert(has(H, <<"<div class=\"ah-nav-tree__group-label\">Main</div>">>)),
    ?assert(has(H, <<"ah-nav-tree__item ah-is-active\" href=\"#\" data-route=\"roles\"">>)),
    ?assert(has(H, <<"aria-current=\"page\"">>)),
    %% the active item's ancestors are open, the other node is not
    ?assert(has(H, <<"<details class=\"ah-nav-tree__node\" open>">>)),
    ?assertEqual(1, count(H, <<" open>">>)),
    ?assert(has(H, <<"ah-nav-tree__item--parent ah-is-open">>)),
    ?assert(has(H, <<"href=\"https://x\"">>)),
    ?assert(has(H, <<"<div class=\"ah-sidenav__footer\">F</div>">>)),
    ?assert(has(H, <<"ah-sidenav__toggle">>)),
    ?assert(has(H, <<"aria-expanded=\"true\"">>)).

sidenav_plain_items_prefix_collapsed_test() ->
    H = r(?M:sidenav([{a, <<"A">>}], undefined, [collapsed],
                     [{route_prefix, <<"#/">>}, {collapsible, true}])),
    ?assert(has(H, <<"class=\"ah-sidenav ah-sidenav-collapsed\"">>)),
    ?assert(has(H, <<"href=\"#/a\"">>)),
    ?assert(has(H, <<"aria-expanded=\"false\"">>)),
    ?assertNot(has(H, <<"ah-nav-tree__group-label">>)).

%%%===================================================================
%%% toolbar
%%%===================================================================

toolbar_test() ->
    H = r(?M:toolbar([#{key => b, label => <<"B">>, toggle => true, pressed => true},
                      #{key => i, label => <<"I">>},
                      #{key => u, label => <<"U">>},
                      separator,
                      #{key => x, title => <<"Cut">>, disabled => true, minimizable => false},
                      separator,
                      {custom, <<"text">>}],
                     [], [{popup_width, 240}])),
    ?assert(has(H, <<"class=\"ah-toolbar\" data-ah=\"toolbar\" role=\"toolbar\"">>)),
    ?assert(has(H, <<"data-ah-popup-width=\"240\"">>)),
    ?assert(has(H, <<"ah-toolbar-tool ah-toolbar-tool-first">>)),
    ?assert(has(H, <<"ah-toolbar-tool ah-toolbar-tool-inner">>)),
    ?assert(has(H, <<"ah-toolbar-tool ah-toolbar-tool-last ah-toolbar-tool-separator-after">>)),
    ?assertEqual(2, count(H, <<"class=\"ah-toolbar-separator\"">>)),
    ?assert(has(H, <<"ah-btn ah-btn-sm ah-toolbar-tool-el ah-btn-toggled">>)),
    ?assert(has(H, <<"aria-pressed=\"true\"">>)),
    ?assert(has(H, <<"data-ah-toggle">>)),
    ?assert(has(H, <<"aria-label=\"Cut\"">>)),
    ?assert(has(H, <<" disabled>">>)),
    ?assert(has(H, <<"data-ah-minimizable=\"false\"">>)),
    ?assert(has(H, <<"<div class=\"ah-toolbar-tool-el\">text</div>">>)),
    ?assert(has(H, <<"ah-toolbar-minimize-btn">>)).

%%%===================================================================
%%% splitter
%%%===================================================================

splitter_test() ->
    H = r(?M:splitter([#{content => <<"L">>, size => <<"30%">>, min => 80},
                       #{content => <<"R">>, min => 60}],
                      [], [{name, <<"s">>}])),
    ?assert(has(H, <<"class=\"ah-splitter ah-splitter-vertical\"">>)),
    ?assert(has(H, <<"data-ah-value=\"30,70\"">>)),
    ?assert(has(H, <<"data-ah-min=\"80,60\"">>)),
    ?assert(has(H, <<"flex:0 0 calc((100% - 5px) * 0.3);min-width:80px;">>)),
    ?assert(has(H, <<"flex:1 1 0;min-width:60px;">>)),
    ?assert(has(H, <<"role=\"separator\" tabindex=\"0\" aria-orientation=\"vertical\"">>)),
    ?assert(has(H, <<"aria-valuenow=\"30\"">>)),
    ?assert(has(H, <<"style=\"width:5px\"">>)),
    ?assert(has(H, <<"ah-splitter-collapse-btn">>)),
    ?assert(has(H, <<"name=\"s\" value=\"30,70\"">>)).

splitter_horizontal_pixels_test() ->
    H = r(?M:splitter([#{content => <<"T">>, size => 120}, <<"B">>], [horizontal, disabled],
                      [{splitbar_size, 8}, {resizable, false}])),
    ?assert(has(H, <<"ah-splitter ah-splitter-horizontal ah-splitter-disabled">>)),
    ?assert(has(H, <<"flex:0 0 120px;min-height:0px;">>)),
    ?assert(has(H, <<"style=\"height:8px\"">>)),
    ?assert(has(H, <<"data-ah-resizable=\"false\"">>)),
    ?assertNot(has(H, <<"data-ah-value">>)),
    ?assertError({aihtml, {splitter_needs_two_panes, 3}}, r(?M:splitter([a, b, c], [], []))).

%%%===================================================================
%%% listmenu
%%%===================================================================

listmenu_test() ->
    Items = [#{key => fruit, label => <<"Fruit">>,
               children => [{apple, <<"Apple">>},
                            #{key => citrus, label => <<"Citrus">>,
                              children => [{lemon, <<"Lemon">>}]}]},
             {bread, <<"Bread">>},
             #{key => cake, label => <<"Cake">>, disabled => true}],
    Root = r(?M:listmenu(Items, bread, [], [{filter, true}, {name, <<"food">>}])),
    ?assert(has(Root, <<"class=\"ah-listmenu\" data-ah=\"listmenu\" tabindex=\"0\"">>)),
    ?assert(has(Root, <<"data-ah-stack=\"\"">>)),
    ?assertEqual(3, count(Root, <<"class=\"ah-listmenu-page\"">>)),
    ?assertEqual(2, count(Root, <<"style=\"display:none\"">>) - 1),  % + back button
    ?assert(has(Root, <<"ah-listmenu-item ah-listmenu-item-selected\" data-item-id=\"1\"">>)),
    ?assert(has(Root, <<"ah-listmenu-item-disabled">>)),
    ?assert(has(Root, <<"ah-listmenu-filter-input">>)),
    ?assert(has(Root, <<"name=\"food\" value=\"bread\"">>)),
    ?assertEqual(2, count(Root, <<"ah-listmenu-arrow">>)),
    %% a nested value opens its page and fills the stack and title
    Nested = r(?M:listmenu(Items, lemon, [], [{header, true}, {animation, fade}])),
    ?assert(has(Nested, <<"data-ah-stack=\"0,4\"">>)),
    ?assert(has(Nested, <<"<span class=\"ah-listmenu-title\">Citrus</span>">>)),
    ?assert(has(Nested, <<"data-page-id=\"4\" role=\"menu\">">>)),
    ?assert(has(Nested, <<"data-ah-animation=\"fade\"">>)),
    NoHeader = r(?M:listmenu(Items, undefined, [disabled], [{header, false}, {arrows, false}])),
    ?assertNot(has(NoHeader, <<"ah-listmenu-header">>)),
    ?assertNot(has(NoHeader, <<"ah-listmenu-arrow">>)),
    ?assert(has(NoHeader, <<"ah-listmenu ah-listmenu-disabled">>)).

%%%===================================================================
%%% status_bar
%%%===================================================================

status_bar_test() ->
    H = r(?M:status_bar([<<"Ln 1">>, #{content => <<"UTF-8">>, align => right}],
                        [], [{content, <<"Hello world 世界\n\nbye"/utf8>>}, {dirty, true},
                             {labels, #{unsaved => <<"Modified">>}}])),
    ?assert(has(H, <<"class=\"ah-status-bar\"">>)),
    ?assert(has(H, <<"data-ah=\"status-bar\"">>)),
    ?assert(has(H, <<"data-dirty=\"true\"">>)),
    %% 3 English words + 2 CJK characters
    ?assert(has(H, <<"<span class=\"ah-status-bar__count-num\">5</span>">>)),
    ?assert(has(H, <<"<span class=\"ah-status-bar__row-label\">Paragraphs</span>"
                     "<span class=\"ah-status-bar__row-val\">2</span>">>)),
    ?assert(has(H, <<"<span class=\"ah-status-bar__row-label\">Lines</span>"
                     "<span class=\"ah-status-bar__row-val\">3</span>">>)),
    ?assert(has(H, <<"<div class=\"ah-status-bar__extra\">Ln 1</div>">>)),
    ?assert(has(H, <<"Modified">>)),
    [_, Right] = binary:split(H, <<"ah-status-bar__side--right">>),
    ?assert(has(Right, <<"UTF-8">>)),
    ?assert(has(Right, <<"ah-status-bar__save">>)),
    Plain = r(?M:status_bar([#{count => 2, label => <<"errors">>}], [], [])),
    ?assertNot(has(Plain, <<"data-dirty">>)),
    ?assertNot(has(Plain, <<"ah-status-bar__popover">>)).

%%%===================================================================
%%% element records (designs/05-records.md)
%%%===================================================================

-define(ITEMS, [#{key => file, label => <<"File">>, children => [{new, <<"New">>}]},
                {help, <<"Help">>}]).

record_equals_builder_test() ->
    ?assertEqual(r(?M:menu(?ITEMS, new, [vertical, show_arrows, <<"mt-2">>],
                           [{name, cmd}, {keyboard, false}, {id, m}, {title, <<"Main">>},
                            {aria_label, <<"x">>}])),
                 r(#ah_menu{items = ?ITEMS, value = new, mode = vertical, show_arrows = true,
                            css = [<<"mt-2">>], name = cmd, keyboard = false, id = m,
                            title = <<"Main">>, attrs = [{aria_label, <<"x">>}]})),
    ?assertEqual(r(?M:navbar(?ITEMS, help, [minimized],
                             [{brand, <<"B">>}, {columns, [<<"30%">>]}, {minimized_height, 40}])),
                 r(#ah_navbar{items = ?ITEMS, value = help, minimized = true, brand = <<"B">>,
                              columns = [<<"30%">>], minimized_height = 40})),
    ?assertEqual(r(?M:sidenav(?ITEMS, new, [collapsed],
                              [{route_prefix, <<"#/">>}, {collapsible, true},
                               {style, <<"--w:1px">>}])),
                 r(#ah_sidenav{groups = ?ITEMS, value = new, collapsed = true,
                               route_prefix = <<"#/">>, collapsible = true,
                               attrs = [{style, <<"--w:1px">>}]})),
    ?assertEqual(r(?M:splitter([<<"L">>, <<"R">>], [horizontal], [{splitbar_size, 8}])),
                 r(#ah_splitter{panes = [<<"L">>, <<"R">>], orientation = horizontal,
                                splitbar_size = 8})),
    ?assertEqual(r(?M:listmenu(?ITEMS, new, [], [{filter, true}, {back_label, <<"Up">>}])),
                 r(#ah_listmenu{items = ?ITEMS, value = new, filter = true,
                                back_label = <<"Up">>})),
    ?assertEqual(r(?M:status_bar([<<"Ln 1">>], [], [{dirty, true}, {content, <<"a b">>}])),
                 r(#ah_status_bar{segments = [<<"Ln 1">>], dirty = true,
                                  content = <<"a b">>})).

builder_fills_fields_test() ->
    T = ?M:toolbar([#{key => b, label => <<"B">>}], [disabled, <<"x">>],
                   [{popup_width, 160}, {id, tb}, {aria_label, <<"Tools">>}]),
    ?assertMatch(#ah_toolbar{disabled = true, popup_width = 160, id = tb, css = [<<"x">>],
                             attrs = [{aria_label, <<"Tools">>}]}, T),
    ?assertMatch(#ah_listmenu{header = true, back_button = true, filter = false,
                              arrows = true, back_label = <<"Back">>, name = food},
                 ?M:listmenu([], undefined, [], [{name, food}])),
    ?assertError({aihtml, {record_only_field, ah_menu, postback}},
                 ?M:menu([], undefined, [], [{postback, go}])).

postback_test() ->
    Token = fun(Html) ->
                    {match, [T]} = re:run(r(Html), <<"data-ah-on=\"([a-z]+:[^\"]+)\"">>,
                                          [{capture, all_but_first, binary}]),
                    [Ev, Tok] = binary:split(T, <<":">>),
                    {ok, Ref} = aihtml_action:unsign(Tok),
                    {Ev, Ref}
            end,
    ?assertEqual({<<"change">>, {?MODULE, go, #{}}},
                 Token(#ah_menu{items = ?ITEMS, postback = go})),
    ?assertEqual({<<"change">>, {?MODULE, go, 1}},
                 Token(#ah_navbar{items = ?ITEMS, postback = {go, 1}})),
    ?assertEqual({<<"change">>, {other_mod, go, #{}}},
                 Token(#ah_sidenav{groups = ?ITEMS, postback = go, delegate = other_mod})),
    ?assertMatch({<<"change">>, _}, Token(#ah_toolbar{tools = [#{key => b}], postback = go})),
    ?assertMatch({<<"change">>, _}, Token(#ah_splitter{panes = [<<"a">>], postback = go})),
    ?assertMatch({<<"change">>, _}, Token(#ah_listmenu{items = ?ITEMS, postback = go})).

no_postback_event_test() ->
    ?assertError({aihtml, {no_postback_event, ah_status_bar}},
                 r(#ah_status_bar{postback = go})).

field_validation_test() ->
    ?assertError({aihtml, {bad_modifier, menu, mode, sideways, _}},
                 r(#ah_menu{mode = sideways})),
    ?assertError({aihtml, {bad_modifier, splitter, orientation, diagonal, _}},
                 r(#ah_splitter{panes = [a], orientation = diagonal})),
    ?assertError({aihtml, {bad_flag, sidenav, collapsed, yes}},
                 r(#ah_sidenav{collapsed = yes})),
    ?assertError({aihtml, {modifier_in_css, navbar, vertical}},
                 r(#ah_navbar{css = [vertical]})),
    ?assertError({aihtml, {splitter_needs_two_panes, 0}}, r(#ah_splitter{})),
    ?assertError({aihtml, {bad_option, dirty, sometimes}}, r(#ah_status_bar{dirty = sometimes})),
    %% all defaults render
    ?assert(has(r(#ah_toolbar{}), <<"class=\"ah-toolbar\"">>)).

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

default(ah_menu) -> #ah_menu{};
default(ah_navbar) -> #ah_navbar{};
default(ah_sidenav) -> #ah_sidenav{};
default(ah_toolbar) -> #ah_toolbar{};
default(ah_splitter) -> #ah_splitter{};
default(ah_listmenu) -> #ah_listmenu{};
default(ah_status_bar) -> #ah_status_bar{}.
