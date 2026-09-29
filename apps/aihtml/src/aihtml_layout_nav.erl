%%%-------------------------------------------------------------------
%%% @doc Navigation and layout chrome ported from sigil: menu, navbar,
%%% sidenav, toolbar, splitter, listmenu and status_bar.
%%%
%%% The DOM and class names follow sigil so the ported stylesheets in
%%% priv/css/sigil/components apply unchanged; behaviour is in
%%% assets/js/components/layout_nav.js (see designs/04-components.md).
%%%
%%% Menus, the navbar, the sidenav and the listmenu take the same item
%%% shape (`item()'):
%%%
%%%   #{key => Key,            value written to data-ah-value on selection
%%%     label => Html,
%%%     icon => Icon,          binary: an image URL; other html: inline (SVG)
%%%     href => Url,           follow the link instead of selecting
%%%     target => Target,
%%%     disabled => boolean(),
%%%     children => [item()],  submenu / drill-down page / tree node
%%%     columns => [#{header => Html, children => [item()]}],   menu only
%%%     open => left | up | [left | up],                        menu only
%%%     expanded => boolean()} sidenav only: node open initially
%%%   | divider                a separator
%%%   | {Key, Label}           shorthand for #{key => Key, label => Label}
%%%
%%% Selecting an item without `href' sets `data-ah-value' on the root to
%%% the item's key and fires `change' there, so `on(change, Action)' in the
%%% root's Attrs receives it (Event.value is the key).
%%%
%%% Each function builds an element record (#ah_menu{} ..., defined in
%%% include/aihtml_layout_nav.hrl) and render/1 turns it into HTML, so
%%% pages may also write the records directly (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_layout_nav).
-behaviour(aihtml_element).

-include("aihtml_layout_nav.hrl").

-export([menu/4, navbar/4, sidenav/4, toolbar/3, splitter/3, listmenu/4,
         status_bar/3]).
-export([render/1, fields/1, catalog/0]).

-export_type([item/0, key/0, icon/0, tool/0, pane/0, segment/0, element/0]).

-type html() :: aihtml_html:html().
-type key() :: ah_nav_key().
-type icon() :: ah_nav_icon().
-type item() :: ah_nav_item().
%% A toolbar tool: a button (map), a separator, or any other html (custom).
-type tool() :: ah_nav_tool().
%% A splitter pane: its content, or a map with its initial size
%% (<<"30%">> or pixels) and minimum size in pixels.
-type pane() :: ah_nav_pane().
-type segment() :: ah_nav_segment().
-type element() :: #ah_menu{} | #ah_navbar{} | #ah_sidenav{} | #ah_toolbar{}
                 | #ah_splitter{} | #ah_listmenu{} | #ah_status_bar{}.

-define(EL, aihtml_element).

-define(TOGGLE_SVG, <<"<svg width=\"18\" height=\"18\" viewBox=\"0 0 24 24\" fill=\"none\" "
                      "stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" "
                      "stroke-linejoin=\"round\" aria-hidden=\"true\"><path d=\"m15 18-6-6 6-6\"/>"
                      "</svg>">>).
-define(CARET_SVG, <<"<svg class=\"ah-nav-tree__caret-svg\" width=\"16\" height=\"16\" "
                     "viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" "
                     "stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\" "
                     "aria-hidden=\"true\"><path d=\"m6 9 6 6 6-6\"/></svg>">>).

%%%===================================================================
%%% Records
%%%===================================================================

build(R, Css, Attrs) ->
    Tag = element(1, R),
    ?EL:build(R, fields(Tag), entry(?EL:component_name(Tag)), Css, Attrs).

%% @doc The field names of one of this group's records.
-spec fields(atom()) -> [atom()].
fields(ah_menu) -> record_info(fields, ah_menu);
fields(ah_navbar) -> record_info(fields, ah_navbar);
fields(ah_sidenav) -> record_info(fields, ah_sidenav);
fields(ah_toolbar) -> record_info(fields, ah_toolbar);
fields(ah_splitter) -> record_info(fields, ah_splitter);
fields(ah_listmenu) -> record_info(fields, ah_listmenu);
fields(ah_status_bar) -> record_info(fields, ah_status_bar).

-spec render(element()) -> html().
render(#ah_menu{} = R) -> render_menu(R);
render(#ah_navbar{} = R) -> render_navbar(R);
render(#ah_sidenav{} = R) -> render_sidenav(R);
render(#ah_toolbar{} = R) -> render_toolbar(R);
render(#ah_splitter{} = R) -> render_splitter(R);
render(#ah_listmenu{} = R) -> render_listmenu(R);
render(#ah_status_bar{} = R) -> render_status_bar(R).

classes(R) ->
    Tag = element(1, R),
    ?EL:classes(R, fields(Tag), entry(?EL:component_name(Tag))).

entry(Name) -> aihtml_catalog:entry(?MODULE, Name).

%%%===================================================================
%%% menu
%%%===================================================================

%% @doc Menu bar with nested submenus (sigil menu). `Value' marks the
%% active item (`ah-menu-link-active'). Css: `horizontal' (default),
%% `vertical', `popup'; flags `show_arrows', `disabled'. Options: `title'
%% (collapsed title), `name' (hidden input), `click_to_open',
%% `keyboard' (default true), `minimize_width' (collapse to a hamburger
%% and drawer below this window width), `popup_target' (selector whose
%% right click opens a popup menu; default the document).
-spec menu([item()], key() | undefined, aihtml_html:css(), aihtml_html:attrs()) ->
          #ah_menu{}.
menu(Items, Value, Css, Attrs) ->
    build(#ah_menu{items = Items, value = Value}, Css, Attrs).

render_menu(#ah_menu{items = Items, value = Value, mode = Mode, disabled = Disabled,
                     title = Title} = R) ->
    Classes = classes(R),
    Horizontal = Mode =:= horizontal,
    Btn = aihtml_html:el(button,
              aihtml_html:el(span, [aihtml_html:el(span, [], [], []) || _ <- [1, 2, 3]],
                             [<<"ah-menu-hamburger">>], [{aria_hidden, <<"true">>}]),
              [<<"ah-menu-minimized-btn">>],
              [{type, button}, {aria_label, text_or(Title, <<"Menu">>)},
               {aria_haspopup, menu}, {aria_expanded, <<"false">>}]),
    List = aihtml_html:el(ul, [menu_item(norm(I), Value) || I <- Items],
                          [<<"ah-menu-list">>], [{role, none}]),
    aihtml_html:el('div', [Btn, List, hidden_input(R#ah_menu.name, Value)], Classes,
                   [[{data_ah, <<"menu">>},
                     {role, case Horizontal of true -> menubar; false -> menu end},
                     {tabindex, 0},
                     {aria_orientation, case Horizontal of
                                            true -> horizontal;
                                            false -> vertical
                                        end},
                     {aria_disabled, Disabled andalso <<"true">>},
                     {data_title, text_or(Title, undefined)},
                     {data_ah_value, value_attr(Value)},
                     {data_ah_click_to_open, R#ah_menu.click_to_open},
                     {data_ah_keyboard, R#ah_menu.keyboard =:= false andalso <<"false">>},
                     {data_ah_minimize_width, R#ah_menu.minimize_width},
                     {data_ah_popup_target, R#ah_menu.popup_target}],
                    ?EL:root_attrs(R, change)]).

menu_item(divider, _V) ->
    aihtml_html:el(li, [], [<<"ah-menu-separator">>], [{role, separator}]);
menu_item(Item, V) ->
    Kids = [norm(K) || K <- maps:get(children, Item, [])],
    Cols = maps:get(columns, Item, []),
    HasSub = Kids =/= [] orelse Cols =/= [],
    Disabled = maps:get(disabled, Item, false),
    Active = is_value(Item, V),
    Link = aihtml_html:el(a,
               [icon(<<"ah-menu-icon">>, maps:get(icon, Item, undefined)),
                aihtml_html:el(span, label(Item), [<<"ah-menu-label">>], []),
                [aihtml_html:el(span, [], [<<"ah-menu-arrow">>], []) || HasSub]],
               [<<"ah-menu-link">>, [<<"ah-menu-link-active">> || Active]],
               [{role, menuitem}, {tabindex, -1},
                {data_id, key_attr(Item)},
                {href, maps:get(href, Item, undefined)},
                {target, maps:get(target, Item, undefined)},
                {aria_haspopup, HasSub andalso <<"true">>},
                {aria_expanded, HasSub andalso <<"false">>},
                {aria_disabled, Disabled andalso <<"true">>},
                {aria_current, Active andalso <<"true">>}]),
    Sub = case HasSub of
              false -> [];
              true ->
                  Dirs = lists:flatten([maps:get(open, Item, [])]),
                  SubCls = [<<"ah-menu-submenu">>,
                            [<<"ah-menu-columns">> || Cols =/= []],
                            [<<"ah-menu-open-left">> || lists:member(left, Dirs)],
                            [<<"ah-menu-open-up">> || lists:member(up, Dirs)]],
                  Body = case Cols of
                             [] -> [menu_item(K, V) || K <- Kids];
                             _ -> [menu_column(C, V) || C <- Cols]
                         end,
                  aihtml_html:el(ul, Body, SubCls, [{role, menu}])
          end,
    aihtml_html:el(li, [Link, Sub],
                   [<<"ah-menu-item">>,
                    [<<"ah-menu-has-submenu">> || HasSub],
                    [<<"ah-menu-item-disabled">> || Disabled]],
                   [{role, none}]).

menu_column(Col, V) ->
    Header = case maps:get(header, Col, undefined) of
                 undefined -> [];
                 H -> aihtml_html:el('div', H, [<<"ah-menu-column-header">>], [])
             end,
    aihtml_html:el(li,
        [Header,
         aihtml_html:el(ul, [menu_item(norm(K), V) || K <- maps:get(children, Col, [])],
                        [], [{role, none},
                             {style, <<"list-style:none;padding:0;margin:0;">>}])],
        [<<"ah-menu-column">>], [{role, none}]).

%%%===================================================================
%%% navbar
%%%===================================================================

%% @doc Navigation bar of selectable items (sigil navbar), with optional
%% brand and trailing content. `Value' is the selected item's key. Css:
%% `horizontal' (default) or `vertical'; flags `minimized' (hamburger
%% header, items in a popup), `disabled'. Options: `brand', `extra'
%% (right-side html), `title' (minimized title), `minimized_height'
%% (default 36), `minimize_width' (minimize below this window width),
%% `columns' (item widths, e.g. [<<"30%">>, <<"70%">>]), `selection'
%% (default true), `name'.
-spec navbar([item()], key() | undefined, aihtml_html:css(), aihtml_html:attrs()) ->
          #ah_navbar{}.
navbar(Items, Value, Css, Attrs) ->
    build(#ah_navbar{items = Items, value = Value}, Css, Attrs).

render_navbar(#ah_navbar{items = Items, value = Value, minimized = Minimized,
                         columns = Columns, minimized_height = Height,
                         title = Title} = R) ->
    Classes = classes(R),
    Vertical = R#ah_navbar.orientation =:= vertical,
    Norm = [norm(I) || I <- Items, norm(I) =/= divider],
    Header = aihtml_html:el('div',
                 [aihtml_html:el('div',
                                 [aihtml_html:el(span, [], [<<"ah-navbar-toggle-bar">>], [])
                                  || _ <- [1, 2, 3]],
                                 [<<"ah-navbar-toggle">>], [{aria_hidden, <<"true">>}]),
                  aihtml_html:el(span, Title, [<<"ah-navbar-title">>], [])],
                 [<<"ah-navbar-header">>],
                 [{style, [<<"height:">>, px(Height), <<";">>]},
                  {role, button}, {tabindex, 0}, {aria_haspopup, <<"true">>},
                  {aria_expanded, <<"false">>},
                  {aria_label, text_or(Title, <<"Navigation">>)}]),
    ItemEls = [navbar_item(I, Value, col(Columns, N))
               || {N, I} <- lists:enumerate(Norm)],
    Brand = slot(R#ah_navbar.brand, <<"ah-navbar-brand">>),
    Extra = slot(R#ah_navbar.extra, <<"ah-navbar-extra">>),
    aihtml_html:el('div', [Header, Brand, ItemEls, Extra,
                           hidden_input(R#ah_navbar.name, Value)],
                   Classes,
                   [[{data_ah, <<"navbar">>}, {role, tablist},
                     {aria_orientation, case Vertical of
                                            true -> vertical;
                                            false -> horizontal
                                        end},
                     {data_ah_value, value_attr(Value)},
                     {data_ah_minimized, Minimized andalso <<"static">>},
                     {data_ah_selection, R#ah_navbar.selection =:= false
                                             andalso <<"false">>},
                     {data_ah_minimize_width, R#ah_navbar.minimize_width}],
                    ?EL:root_attrs(R, change)]).

navbar_item(Item, V, ColW) ->
    Selected = is_value(Item, V),
    Disabled = maps:get(disabled, Item, false),
    Href = maps:get(href, Item, undefined),
    Tag = case Href of undefined -> 'div'; _ -> a end,
    aihtml_html:el(Tag,
        [icon(<<"ah-navbar-icon">>, maps:get(icon, Item, undefined)), label(Item)],
        [<<"ah-navbar-item">>,
         [<<"ah-navbar-item-selected">> || Selected],
         [<<"ah-navbar-item-disabled">> || Disabled]],
        [{role, tab}, {data_key, key_attr(Item)},
         {href, Href}, {target, maps:get(target, Item, undefined)},
         {tabindex, case Selected of true -> 0; false -> -1 end},
         {aria_selected, atom_to_binary(Selected)},
         {aria_disabled, Disabled andalso <<"true">>},
         {style, ColW =/= undefined andalso [<<"flex:none;width:">>, px(ColW), <<";">>]}]).

col(Columns, N) when N =< length(Columns) -> lists:nth(N, Columns);
col(_, _) -> undefined.

%%%===================================================================
%%% sidenav
%%%===================================================================

%% @doc Application side navigation: brand, grouped tree of links (sigil
%% sidenav + nav-tree) and footer. `Groups' is `[#{label, items}]' or a
%% plain item list; `Value' is the active item's key (its ancestors open).
%% Flag `collapsed' renders the narrow, icon-only form. Options: `brand'
%% (`#{name, logo, href}' or html), `footer', `collapsible' (a toggle
%% button), `route_prefix' (href = prefix ++ key for items without href),
%% `name'.
-spec sidenav([#{label => html(), items := [item()]}] | [item()],
              key() | undefined, aihtml_html:css(), aihtml_html:attrs()) ->
          #ah_sidenav{}.
sidenav(Groups, Value, Css, Attrs) ->
    build(#ah_sidenav{groups = Groups, value = Value}, Css, Attrs).

render_sidenav(#ah_sidenav{groups = Groups0, value = Value, collapsed = Collapsed,
                           route_prefix = Prefix} = R) ->
    Classes = classes(R),
    Groups = case Groups0 of
                 [#{items := _} | _] -> Groups0;
                 _ -> [#{items => Groups0}]
             end,
    Tree = aihtml_html:el(nav,
               [aihtml_html:el('div',
                    [case maps:get(label, G, undefined) of
                         undefined -> [];
                         L -> aihtml_html:el('div', L, [<<"ah-nav-tree__group-label">>], [])
                     end,
                     [nav_item(norm(I), Value, Prefix)
                      || I <- maps:get(items, G), norm(I) =/= divider]],
                    [<<"ah-nav-tree__group">>], [])
                || G <- Groups],
               [<<"ah-nav-tree">>], []),
    Toggle = case R#ah_sidenav.collapsible of
                 false -> [];
                 true ->
                     aihtml_html:el(button, {safe, ?TOGGLE_SVG},
                                    [<<"ah-sidenav__toggle">>],
                                    [{type, button},
                                     {aria_label, <<"Toggle navigation">>},
                                     {aria_expanded, atom_to_binary(not Collapsed)}])
             end,
    Footer = case R#ah_sidenav.footer of
                 undefined -> [];
                 F -> aihtml_html:el('div', F, [<<"ah-sidenav__footer">>], [])
             end,
    aihtml_html:el(aside,
        [aihtml_html:el('div', [brand(R#ah_sidenav.brand), Toggle],
                        [<<"ah-sidenav__head">>], []),
         aihtml_html:el('div', Tree, [<<"ah-sidenav__nav">>], []),
         Footer, hidden_input(R#ah_sidenav.name, Value)],
        Classes,
        [[{data_ah, <<"sidenav">>}, {data_ah_value, value_attr(Value)}],
         ?EL:root_attrs(R, change)]).


brand(undefined) -> [];
brand(#{} = B) ->
    Body = [case maps:get(logo, B, undefined) of
                undefined -> [];
                Logo -> aihtml_html:el(span, icon_body(Logo), [<<"ah-sidenav__brand-mark">>], [])
            end,
            case maps:get(name, B, undefined) of
                undefined -> [];
                Name -> aihtml_html:el(span, Name, [<<"ah-sidenav__brand-name">>], [])
            end],
    case maps:get(href, B, undefined) of
        undefined -> aihtml_html:el('div', Body, [<<"ah-sidenav__brand">>], []);
        Href -> aihtml_html:el(a, Body, [<<"ah-sidenav__brand">>], [{href, Href}])
    end;
brand(Html) -> aihtml_html:el('div', Html, [<<"ah-sidenav__brand">>], []).

nav_item(Item, V, Prefix) ->
    Icon = case maps:get(icon, Item, undefined) of
               undefined -> [];
               I -> aihtml_html:el(span, icon_body(I), [<<"ah-nav-tree__icon">>], [])
           end,
    Label = aihtml_html:el(span, label(Item), [<<"ah-nav-tree__label">>], []),
    Title = case label(Item) of B when is_binary(B) -> B; _ -> undefined end,
    Disabled = maps:get(disabled, Item, false),
    case [norm(K) || K <- maps:get(children, Item, [])] -- [divider] of
        [] ->
            Active = is_value(Item, V),
            Href = case {maps:get(href, Item, undefined), Prefix} of
                       {undefined, undefined} -> <<"#">>;
                       {undefined, P} -> iolist_to_binary([P, key_attr(Item)]);
                       {H, _} -> H
                   end,
            aihtml_html:el(a, [Icon, Label],
                           [<<"ah-nav-tree__item">>, [<<"ah-is-active">> || Active],
                            [<<"ah-nav-tree__item--disabled">> || Disabled]],
                           [{href, Href}, {data_route, key_attr(Item)},
                            {target, maps:get(target, Item, undefined)},
                            {title, Title},
                            {aria_current, Active andalso <<"page">>},
                            {aria_disabled, Disabled andalso <<"true">>}]);
        Kids ->
            Open = maps:get(expanded, Item, false) orelse contains_value(Kids, V),
            aihtml_html:el(details,
                [aihtml_html:el(summary,
                     [Icon, Label,
                      aihtml_html:el(span, {safe, ?CARET_SVG}, [<<"ah-nav-tree__caret">>], [])],
                     [<<"ah-nav-tree__item">>, <<"ah-nav-tree__item--parent">>,
                      [<<"ah-is-open">> || Open]],
                     [{title, Title}, {data_route, key_attr(Item)}]),
                 aihtml_html:el('div',
                     aihtml_html:el('div', [nav_item(K, V, Prefix) || K <- Kids],
                                    [<<"ah-nav-tree__children-inner">>], []),
                     [<<"ah-nav-tree__children">>], [])],
                [<<"ah-nav-tree__node">>], [{open, Open}])
    end.

contains_value(Items, V) ->
    lists:any(fun(divider) -> false;
                 (I) -> is_value(I, V) orelse
                            contains_value([norm(K) || K <- maps:get(children, I, [])], V)
              end, Items).

%%%===================================================================
%%% toolbar
%%%===================================================================

%% @doc Horizontal toolbar (sigil toolbar). Tools that do not fit move into
%% an overflow popup behind a "☰" button. Clicking a button tool with a key
%% sets `data-ah-value' to that key and fires `change'; `toggle' tools also
%% flip `aria-pressed'. Flag `disabled'. Options: `popup_width' (default
%% 200).
-spec toolbar([tool()], aihtml_html:css(), aihtml_html:attrs()) -> #ah_toolbar{}.
toolbar(Tools, Css, Attrs) ->
    build(#ah_toolbar{tools = Tools}, Css, Attrs).

render_toolbar(#ah_toolbar{tools = Tools} = R) ->
    Classes = classes(R),
    Runs = tool_runs(Tools),
    Els = lists:join(aihtml_html:el('div', [], [<<"ah-toolbar-separator">>],
                                    [{role, separator}, {aria_orientation, vertical}]),
                     [tool_run(Run, Last) || {Run, Last} <- mark_last(Runs)]),
    MinBtn = aihtml_html:el('div', <<"\x{2630}"/utf8>>, [<<"ah-toolbar-minimize-btn">>],
                            [{role, button}, {tabindex, 0}, {aria_label, <<"More tools">>},
                             {aria_haspopup, menu}, {aria_expanded, <<"false">>}]),
    aihtml_html:el('div', [Els, MinBtn], Classes,
                   [[{data_ah, <<"toolbar">>}, {role, toolbar},
                     {aria_orientation, horizontal},
                     {data_ah_popup_width, R#ah_toolbar.popup_width}],
                    ?EL:root_attrs(R, change)]).

%% Tools split into runs at separators.
tool_runs(Tools) ->
    {Runs, Cur} = lists:foldl(fun(separator, {Acc, []}) -> {Acc, []};
                                 (separator, {Acc, Cur}) -> {[lists:reverse(Cur) | Acc], []};
                                 (T, {Acc, Cur}) -> {Acc, [T | Cur]}
                              end, {[], []}, Tools),
    lists:reverse(case Cur of [] -> Runs; _ -> [lists:reverse(Cur) | Runs] end).

mark_last([]) -> [];
mark_last([R]) -> [{R, true}];
mark_last([R | Rs]) -> [{R, false} | mark_last(Rs)].

%% One run; adjacent buttons share corners (first / inner / last).
tool_run(Run, LastRun) ->
    N = length(Run),
    Btn = [is_map(T) || T <- Run],
    [begin
         IsBtn = lists:nth(I, Btn),
         Prev = I > 1 andalso lists:nth(I - 1, Btn),
         Next = I < N andalso lists:nth(I + 1, Btn),
         Pos = case {IsBtn, Prev, Next} of
                   {true, true, true} -> <<"ah-toolbar-tool-inner">>;
                   {true, false, true} -> <<"ah-toolbar-tool-first">>;
                   {true, true, false} -> <<"ah-toolbar-tool-last">>;
                   _ -> []
               end,
         SepAfter = I =:= N andalso not LastRun,
         tool(T, [Pos, [<<"ah-toolbar-tool-separator-after">> || SepAfter]])
     end || {I, T} <- lists:enumerate(Run)].

tool(#{} = T, Cls) ->
    Toggle = maps:get(toggle, T, false),
    Pressed = Toggle andalso maps:get(pressed, T, false),
    Label = maps:get(label, T, undefined),
    Title = maps:get(title, T, undefined),
    Btn = aihtml_html:el(button,
              [icon(<<"ah-toolbar-icon">>, maps:get(icon, T, undefined)),
               case Label of undefined -> []; _ -> aihtml_html:el(span, Label, [], []) end],
              [<<"ah-btn">>, <<"ah-btn-sm">>, <<"ah-toolbar-tool-el">>,
               [<<"ah-btn-toggled">> || Pressed]],
              [{type, button}, {data_key, key_attr(T)},
               {title, Title},
               {aria_label, Label =:= undefined andalso Title},
               {aria_pressed, Toggle andalso atom_to_binary(Pressed)},
               {data_ah_toggle, Toggle},
               {disabled, maps:get(disabled, T, false)}]),
    aihtml_html:el('div', Btn, [<<"ah-toolbar-tool">>, Cls],
                   [{data_ah_minimizable, maps:get(minimizable, T, true) =:= false
                                              andalso <<"false">>}]);
tool({custom, Html}, Cls) ->
    tool_custom(Html, Cls);
tool(Html, Cls) ->
    tool_custom(Html, Cls).

tool_custom(Html, Cls) ->
    aihtml_html:el('div', aihtml_html:el('div', Html, [<<"ah-toolbar-tool-el">>], []),
                   [<<"ah-toolbar-tool">>, Cls], []).

%%%===================================================================
%%% splitter
%%%===================================================================

%% @doc Two panes separated by a draggable bar (sigil splitter). Css:
%% `vertical' (default; side by side, the bar is vertical) or `horizontal'
%% (stacked); flag `disabled'. Drag, arrow keys (Shift: larger steps),
%% Home/End and Enter (collapse the first pane) resize it; `input' fires
%% while dragging and `change' at the end with `data-ah-value' set to the
%% two sizes in percent ("30,70"). Options: `splitbar_size' (px, default
%% 5), `resizable' (default true), `step' (px, default 10), `name'.
-spec splitter([pane()], aihtml_html:css(), aihtml_html:attrs()) -> #ah_splitter{}.
splitter(Panes, Css, Attrs) ->
    build(#ah_splitter{panes = Panes}, Css, Attrs).

render_splitter(#ah_splitter{panes = Panes, splitbar_size = Bar} = R) ->
    Classes = classes(R),
    Horiz = R#ah_splitter.orientation =:= horizontal,
    [P0, P1] = case [pane(P) || P <- Panes] of
                   [A, B] -> [A, B];
                   [A] -> [A, pane([])];
                   _ -> error({aihtml, {splitter_needs_two_panes, length(Panes)}})
               end,
    {Basis, Pct} = case maps:get(size, P0, <<"50%">>) of
                       S when is_number(S) -> {[px(S)], undefined};
                       S -> F = percent(S),
                            {[<<"calc((100% - ">>, px(Bar), <<") * ">>, num(F / 100), $)],
                             F}
                   end,
    MinProp = case Horiz of true -> <<"min-height:">>; false -> <<"min-width:">> end,
    Min = fun(P) -> [MinProp, px(maps:get(min, P, 0)), $;] end,
    Value = case Pct of undefined -> undefined;
                        _ -> iolist_to_binary([num(Pct), $,, num(100 - Pct)])
            end,
    Panel = fun(N, P, Style) ->
                    aihtml_html:el('div', maps:get(content, P, []),
                                   [<<"ah-splitter-panel">>],
                                   [{data_panel, N}, {style, Style}])
            end,
    Splitbar = aihtml_html:el('div',
                   aihtml_html:el('div', [], [<<"ah-splitter-collapse-btn">>],
                                  [{aria_hidden, <<"true">>}]),
                   [<<"ah-splitter-splitbar">>],
                   [{role, separator}, {tabindex, 0},
                    {aria_orientation, case Horiz of true -> horizontal; false -> vertical end},
                    {aria_valuemin, 0}, {aria_valuemax, 100},
                    {aria_valuenow, case Pct of undefined -> 50; _ -> round(Pct) end},
                    {style, [case Horiz of true -> <<"height:">>; false -> <<"width:">> end,
                             px(Bar)]}]),
    aihtml_html:el('div',
        [Panel(0, P0, [<<"flex:0 0 ">>, Basis, $;, Min(P0)]),
         Splitbar,
         Panel(1, P1, [<<"flex:1 1 0;">>, Min(P1)]),
         hidden_input(R#ah_splitter.name, Value)],
        Classes,
        [[{data_ah, <<"splitter">>}, {data_ah_value, Value},
          {data_ah_min, iolist_to_binary([integer_to_binary(maps:get(min, P0, 0)), $,,
                                          integer_to_binary(maps:get(min, P1, 0))])},
          {data_ah_resizable, R#ah_splitter.resizable =:= false andalso <<"false">>},
          {data_ah_step, R#ah_splitter.step}],
         ?EL:root_attrs(R, change)]).

pane(#{} = P) -> P;
pane(Html) -> #{content => Html}.

percent(B) when is_binary(B) ->
    N = binary:replace(B, <<"%">>, <<>>),
    try binary_to_float(N) catch error:badarg -> float(binary_to_integer(N)) end;
percent(L) when is_list(L) -> percent(list_to_binary(L)).

%%%===================================================================
%%% listmenu
%%%===================================================================

%% @doc Drill-down list menu (sigil listmenu): one level at a time, a
%% header with title and back button, optional filter. Items with children
%% open their page; a leaf becomes the value (`data-ah-value', `change').
%% When `Value' is nested, its page is shown initially. Flag `disabled'.
%% Options: `header' (default true), `back_button' (true), `filter'
%% (false), `arrows' (true), `back_label' ("Back"), `filter_placeholder'
%% ("Filter..."), `animation' (slide | fade | none), `name'.
-spec listmenu([item()], key() | undefined, aihtml_html:css(), aihtml_html:attrs()) ->
          #ah_listmenu{}.
listmenu(Items, Value, Css, Attrs) ->
    build(#ah_listmenu{items = Items, value = Value}, Css, Attrs).

render_listmenu(#ah_listmenu{items = Items, value = Value, arrows = Arrows,
                             filter_placeholder = Placeholder} = R) ->
    Classes = classes(R),
    {Pages, _} = lm_pages(Items, <<"root">>, 0),
    Path = lm_path(Pages, Value),
    Current = case Path of [] -> <<"root">>; _ -> lists:last(Path) end,
    Titles = maps:from_list([{integer_to_binary(Id), label(I)}
                             || {_, Is} <- Pages, {Id, I} <- Is]),
    Header = case R#ah_listmenu.header of
                 false -> [];
                 true ->
                     aihtml_html:el('div',
                         [case R#ah_listmenu.back_button of
                              false -> [];
                              true ->
                                  aihtml_html:el(button,
                                      [aihtml_html:el(span, <<"\x{25C0}"/utf8>>,
                                                      [<<"ah-listmenu-back-arrow">>],
                                                      [{aria_hidden, <<"true">>}]),
                                       aihtml_html:el(span, R#ah_listmenu.back_label,
                                                      [<<"ah-listmenu-back-label">>], [])],
                                      [<<"ah-listmenu-back">>],
                                      [{type, button}, {tabindex, -1},
                                       {style, Path =:= [] andalso <<"display:none">>}])
                          end,
                          aihtml_html:el(span, maps:get(Current, Titles, []),
                                         [<<"ah-listmenu-title">>], [])],
                         [<<"ah-listmenu-header">>], [])
             end,
    Filter = case R#ah_listmenu.filter of
                 false -> [];
                 true ->
                     aihtml_html:el('div',
                         aihtml_html:void(input, [<<"ah-listmenu-filter-input">>],
                                          [{type, text}, {tabindex, -1},
                                           {placeholder, default(Placeholder,
                                                                 <<"Filter...">>)},
                                           {aria_label, default(Placeholder, <<"Filter">>)}]),
                         [<<"ah-listmenu-filter">>], [])
             end,
    Viewport = aihtml_html:el('div',
                   [lm_page(P, Is, P =:= Current, Value, Arrows) || {P, Is} <- Pages],
                   [<<"ah-listmenu-viewport">>], []),
    aihtml_html:el('div', [Header, Filter, Viewport, hidden_input(R#ah_listmenu.name, Value)],
                   Classes,
                   [[{data_ah, <<"listmenu">>}, {tabindex, 0},
                     {data_ah_value, value_attr(Value)},
                     {data_ah_stack, iolist_to_binary(lists:join($,, Path))},
                     {data_ah_animation, R#ah_listmenu.animation}],
                    ?EL:root_attrs(R, change)]).

%% [{PageId, [{ItemId, Item}]}] in document order, root first.
lm_pages(Items, PageId, N0) ->
    {Numbered, N1} = lists:mapfoldl(fun(I, N) -> {{N, norm(I)}, N + 1} end, N0,
                                    [I || I <- Items, norm(I) =/= divider]),
    {Subs, N2} = lists:mapfoldl(
                   fun({Id, I}, N) ->
                           case maps:get(children, I, []) of
                               [] -> {[], N};
                               Kids -> lm_pages(Kids, integer_to_binary(Id), N)
                           end
                   end, N1, Numbered),
    {[{PageId, Numbered} | lists:append(Subs)], N2}.

%% Page ids from the root's child down to the page holding Value.
lm_path(_Pages, undefined) -> [];
lm_path(Pages, V) ->
    Parent = maps:from_list([{integer_to_binary(Id), P}
                             || {P, Is} <- Pages, {Id, _} <- Is]),
    case [P || {P, Is} <- Pages, {_, I} <- Is, is_value(I, V)] of
        [] -> [];
        [Page | _] -> up(Page, Parent, [])
    end.

up(<<"root">>, _, Acc) -> Acc;
up(Page, Parent, Acc) -> up(maps:get(Page, Parent), Parent, [Page | Acc]).

lm_page(PageId, Items, Visible, V, Arrows) ->
    aihtml_html:el(ul, [lm_item(Id, I, V, Arrows) || {Id, I} <- Items],
                   [<<"ah-listmenu-page">>],
                   [{data_page_id, PageId}, {role, menu},
                    {style, not Visible andalso <<"display:none">>}]).

lm_item(Id, Item, V, Arrows) ->
    HasKids = maps:get(children, Item, []) =/= [],
    Selected = not HasKids andalso is_value(Item, V),
    Disabled = maps:get(disabled, Item, false),
    aihtml_html:el(li,
        [icon(<<"ah-listmenu-icon">>, maps:get(icon, Item, undefined)),
         aihtml_html:el(span, label(Item), [<<"ah-listmenu-item-label">>], []),
         [aihtml_html:el(span, <<"\x{203A}"/utf8>>, [<<"ah-listmenu-arrow">>],
                         [{aria_hidden, <<"true">>}]) || HasKids, Arrows]],
        [<<"ah-listmenu-item">>, [<<"ah-listmenu-item-selected">> || Selected],
         [<<"ah-listmenu-item-disabled">> || Disabled]],
        [{data_item_id, Id}, {data_key, key_attr(Item)},
         {data_href, maps:get(href, Item, undefined)},
         {role, case HasKids of true -> menuitem; false -> menuitemradio end},
         {aria_haspopup, HasKids andalso <<"true">>},
         {aria_checked, not HasKids andalso atom_to_binary(Selected)},
         {aria_disabled, Disabled andalso <<"true">>}]).

%%%===================================================================
%%% status_bar
%%%===================================================================

%% @doc Status bar with left and right segments (sigil status-bar).
%% A segment is html (left), `#{content, align}', or a count segment
%% `#{count, label, details => [{Label, Value}]}' whose details show on
%% hover. Options: `content' (text: adds sigil's CJK-aware word count
%% segment), `dirty' (true | false: adds the saved/unsaved dot on the
%% right), `labels' (map overriding the default English labels).
-spec status_bar([segment()], aihtml_html:css(), aihtml_html:attrs()) -> #ah_status_bar{}.
status_bar(Segments, Css, Attrs) ->
    build(#ah_status_bar{segments = Segments}, Css, Attrs).

render_status_bar(#ah_status_bar{segments = Segments, dirty = Dirty} = R) ->
    Classes = classes(R),
    lists:member(Dirty, [undefined, true, false])
        orelse error({aihtml, {bad_option, dirty, Dirty}}),
    Labels = maps:merge(default_labels(), R#ah_status_bar.labels),
    Words = case R#ah_status_bar.content of
                undefined -> [];
                Text -> [word_count_segment(Text, Labels)]
            end,
    Save = case Dirty of
               undefined -> [];
               _ -> [#{align => right,
                       content => {save, Dirty}}]
           end,
    All = [seg(S) || S <- Words ++ Segments ++ Save],
    Left = [segment(S, Labels) || S <- All, maps:get(align, S, left) =:= left],
    Right = [segment(S, Labels) || S <- All, maps:get(align, S, left) =:= right],
    aihtml_html:el('div',
        [aihtml_html:el('div', Left, [<<"ah-status-bar__side">>], []),
         aihtml_html:el('div', Right,
                        [<<"ah-status-bar__side">>, <<"ah-status-bar__side--right">>], [])],
        Classes,
        [[{role, status},
          {data_ah, <<"status-bar">>},
          {data_dirty, Dirty =/= undefined andalso atom_to_binary(Dirty =:= true)}],
         ?EL:root_attrs(R, none)]).

seg(#{content := _} = S) -> S;
seg(#{count := _} = S) -> S;
seg(Html) -> #{content => Html}.

segment(#{content := {save, Dirty}}, Labels) ->
    aihtml_html:el('div',
        [aihtml_html:el(span, [], [<<"ah-status-bar__dot">>], [{aria_hidden, <<"true">>}]),
         aihtml_html:el(span, case Dirty of
                                  true -> maps:get(unsaved, Labels);
                                  false -> maps:get(saved, Labels)
                              end, [], [])],
        [<<"ah-status-bar__save">>], []);
segment(#{count := N} = S, _Labels) ->
    Details = maps:get(details, S, []),
    aihtml_html:el('div',
        [aihtml_html:el(span, N, [<<"ah-status-bar__count-num">>], []),
         case maps:get(label, S, undefined) of
             undefined -> [];
             L -> aihtml_html:el(span, [<<" ">>, L], [], [])
         end,
         case Details of
             [] -> [];
             _ ->
                 aihtml_html:el('div',
                     [aihtml_html:el('div',
                          [aihtml_html:el(span, K, [<<"ah-status-bar__row-label">>], []),
                           aihtml_html:el(span, V, [<<"ah-status-bar__row-val">>], [])],
                          [<<"ah-status-bar__row">>], [])
                      || {K, V} <- Details],
                     [<<"ah-status-bar__popover">>], [{role, tooltip}])
         end],
        [<<"ah-status-bar__count">>], [{tabindex, 0}]);
segment(#{content := Html}, _Labels) ->
    aihtml_html:el('div', Html, [<<"ah-status-bar__extra">>], []).

default_labels() ->
    #{count => <<"words">>, cjk => <<"CJK">>, words => <<"EN words">>,
      chars => <<"Chars">>, chars_no_space => <<"No spaces">>, lines => <<"Lines">>,
      paragraphs => <<"Paragraphs">>, saved => <<"Saved">>, unsaved => <<"Unsaved">>}.

%% sigil's compute-stats: count = CJK characters + English words.
word_count_segment(Text0, Labels) ->
    Text = unicode:characters_to_binary(Text0),
    Cs = unicode:characters_to_list(Text),
    Cjk = length([C || C <- Cs, (C >= 16#4E00 andalso C =< 16#9FFF)
                                orelse (C >= 16#3400 andalso C =< 16#4DBF)
                                orelse (C >= 16#F900 andalso C =< 16#FAFF)]),
    Words = length(re:split(Text, <<"[^A-Za-z]+">>, [trim]) -- [<<>>]),
    Blank = string:trim(Text) =:= <<>>,
    Lines = case Blank of true -> 0; false -> length(binary:matches(Text, <<"\n">>)) + 1 end,
    Paras = length([P || P <- re:split(Text, <<"\n{2,}">>), string:trim(P) =/= <<>>]),
    NoSpace = length([C || C <- Cs, not lists:member(C, " \t\n\r\f\v")]),
    #{count => Cjk + Words, label => maps:get(count, Labels),
      details => [{maps:get(K, Labels), V}
                  || {K, V} <- [{cjk, Cjk}, {words, Words}, {chars, length(Cs)},
                                {chars_no_space, NoSpace}, {lines, Lines},
                                {paragraphs, Paras}]]}.

%%%===================================================================
%%% Shared helpers
%%%===================================================================

norm(divider) -> divider;
norm(separator) -> divider;
norm(#{divider := true}) -> divider;
norm(#{} = M) -> M;
norm({K, L}) -> #{key => K, label => L}.

label(Item) -> maps:get(label, Item, key_label(maps:get(key, Item, <<>>))).

key_label(K) when is_atom(K) -> atom_to_binary(K);
key_label(K) -> K.

key_attr(#{key := K}) -> key_bin(K);
key_attr(_) -> undefined.

key_bin(K) when is_atom(K) -> atom_to_binary(K);
key_bin(K) when is_integer(K) -> integer_to_binary(K);
key_bin(K) when is_binary(K) -> K;
key_bin(K) when is_list(K) -> unicode:characters_to_binary(K).

value_attr(undefined) -> undefined;
value_attr(V) -> key_bin(V).

is_value(_, undefined) -> false;
is_value(#{key := K}, V) -> key_bin(K) =:= key_bin(V);
is_value(_, _) -> false.

hidden_input(undefined, _Value) -> [];
hidden_input(Name, Value) ->
    aihtml_html:void(input, [], [{type, hidden}, {name, Name},
                                 {value, case Value of
                                             undefined -> <<>>;
                                             _ -> key_bin(Value)
                                         end}]).

icon(_Class, undefined) -> [];
icon(Class, Src) when is_binary(Src) ->
    aihtml_html:void(img, [Class], [{src, Src}, {alt, <<>>}]);
icon(Class, Html) ->
    aihtml_html:el(span, Html, [Class], [{aria_hidden, <<"true">>}]).

%% Inside an existing wrapper: an image or the html itself.
icon_body(Src) when is_binary(Src) -> aihtml_html:void(img, [], [{src, Src}, {alt, <<>>}]);
icon_body(Html) -> Html.

slot(undefined, _Class) -> [];
slot(Html, Class) -> aihtml_html:el('div', Html, [Class], []).

default(undefined, Default) -> Default;
default(V, _) -> V.

text_or(B, _) when is_binary(B), B =/= <<>> -> B;
text_or(_, Default) -> Default.

px(N) when is_integer(N) -> [integer_to_binary(N), <<"px">>];
px(N) when is_float(N) -> [num(N), <<"px">>];
px(B) when is_binary(B) -> B.

%% A number with at most three decimals and no trailing zeros.
num(F) ->
    R = round(F * 1000) / 1000,
    case R == trunc(R) of
        true -> integer_to_binary(trunc(R));
        false -> float_to_binary(R, [{decimals, 3}, compact])
    end.

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => menu, category => layout,
       signature => <<"menu(Items, Value, Css, Attrs)">>,
       root => <<"ah-menu">>,
       groups => #{mode => {[horizontal, vertical, popup], horizontal}},
       flags => [show_arrows, disabled],
       classes => #{show_arrows => [<<"ah-menu-show-arrows">>]},
       options => [title, name, click_to_open, keyboard, minimize_width, popup_target],
       behavior => <<"menu">>, events => [<<"change">>],
       option_docs => #{horizontal => <<"Menu bar, submenus open below (default).">>,
                       vertical => <<"Vertical list, submenus open to the right.">>,
                       popup => <<"Context menu: hidden until right click on popup_target.">>,
                       show_arrows => <<"Show the arrow on top-level items of a horizontal menu.">>,
                       disabled => <<"Disable the whole menu.">>,
                       title => <<"Title of the collapsed hamburger drawer (default Menu).">>,
                       name => <<"Name of a hidden input holding the value.">>,
                       click_to_open => <<"Open submenus by click instead of hover.">>,
                       keyboard => <<"Arrow key navigation, default true.">>,
                       minimize_width => <<"Collapse to a hamburger and drawer when the window is at most this wide (px).">>,
                       popup_target => <<"Selector whose right click opens a popup menu; default the document.">>},
       methods => [#{name => open, args => <<"(X, Y)">>, doc => <<"Show a popup menu at viewport coordinates.">>},
                   #{name => close, args => <<"()">>, doc => <<"Close a popup menu and its submenus.">>},
                   #{name => closeAll, args => <<"()">>, doc => <<"Close every open submenu.">>},
                   #{name => openItem, args => <<"(Key)">>, doc => <<"Open the submenu of an item.">>},
                   #{name => closeItem, args => <<"(Key)">>, doc => <<"Close the submenu of an item.">>},
                   #{name => disableItem, args => <<"(Key)">>, doc => <<"Disable an item.">>},
                   #{name => enableItem, args => <<"(Key)">>, doc => <<"Enable an item.">>},
                   #{name => setValue, args => <<"(Key)">>, doc => <<"Mark the active item without firing change.">>},
                   #{name => minimize, args => <<"()">>, doc => <<"Collapse to the hamburger button.">>},
                   #{name => restore, args => <<"()">>, doc => <<"Undo minimize.">>}],
       doc => <<"Menu bar or context menu with nested submenus, keyboard navigation "
                "and a responsive hamburger drawer.">>},
     #{name => navbar, category => layout,
       signature => <<"navbar(Items, Value, Css, Attrs)">>,
       root => <<"ah-navbar">>,
       groups => #{orientation => {[horizontal, vertical], horizontal}},
       flags => [minimized, disabled],
       classes => #{horizontal => []},
       options => [brand, extra, title, minimized_height, minimize_width, columns,
                   selection, name],
       behavior => <<"navbar">>, events => [<<"change">>],
       option_docs => #{horizontal => <<"Items in a row (default).">>,
                       vertical => <<"Items in a column.">>,
                       minimized => <<"Hamburger header; the items open in a popup list.">>,
                       disabled => <<"Disable the whole bar.">>,
                       brand => <<"Html before the items.">>,
                       extra => <<"Html at the right end.">>,
                       title => <<"Header text when minimized.">>,
                       minimized_height => <<"Header height when minimized, default 36 (px).">>,
                       minimize_width => <<"Minimize when the window is at most this wide (px).">>,
                       columns => <<"Item widths in order, e.g. [<<\"30%\">>, <<\"70%\">>].">>,
                       selection => <<"false: clicks do not select, default true.">>,
                       name => <<"Name of a hidden input holding the value.">>},
       methods => [#{name => setValue, args => <<"(Key)">>, doc => <<"Select an item without firing change.">>},
                   #{name => select, args => <<"(Key)">>, doc => <<"Select an item and fire change.">>},
                   #{name => minimize, args => <<"()">>, doc => <<"Switch to the hamburger header.">>},
                   #{name => restore, args => <<"()">>, doc => <<"Show the items again.">>}],
       doc => <<"Bar of selectable navigation items with brand and trailing slots; "
                "collapses to a hamburger header with a popup list.">>},
     #{name => sidenav, category => layout,
       signature => <<"sidenav(Groups, Value, Css, Attrs)">>,
       root => <<"ah-sidenav">>,
       flags => [collapsed],
       options => [brand, footer, collapsible, route_prefix, name],
       behavior => <<"sidenav">>, events => [<<"change">>, <<"ah:collapse">>],
       option_docs => #{collapsed => <<"Narrow, icon-only form.">>,
                       brand => <<"#{name, logo, href} or html at the top.">>,
                       footer => <<"Html at the bottom.">>,
                       collapsible => <<"Show a button that collapses and expands the sidebar.">>,
                       route_prefix => <<"href of items without one: prefix followed by the key.">>,
                       name => <<"Name of a hidden input holding the value.">>},
       methods => [#{name => setValue, args => <<"(Key)">>, doc => <<"Make an item active and open its ancestors, without firing change.">>},
                   #{name => collapse, args => <<"()">>, doc => <<"Collapse to icons (fires ah:collapse).">>},
                   #{name => expand, args => <<"()">>, doc => <<"Expand (fires ah:collapse).">>},
                   #{name => toggle, args => <<"()">>, doc => <<"Collapse or expand.">>}],
       doc => <<"Application sidebar: brand, grouped link tree with collapsible "
                "nodes, footer; can collapse to icons.">>},
     #{name => toolbar, category => layout,
       signature => <<"toolbar(Tools, Css, Attrs)">>,
       root => <<"ah-toolbar">>,
       flags => [disabled],
       options => [popup_width],
       behavior => <<"toolbar">>, events => [<<"change">>, <<"click">>],
       option_docs => #{disabled => <<"Disable the whole toolbar.">>,
                       popup_width => <<"Width of the overflow popup, default 200 (px).">>},
       methods => [#{name => layout, args => <<"()">>, doc => <<"Recompute which tools overflow.">>},
                   #{name => open, args => <<"()">>, doc => <<"Open the overflow popup.">>},
                   #{name => close, args => <<"()">>, doc => <<"Close the overflow popup.">>},
                   #{name => disableTool, args => <<"(Key, Disabled)">>, doc => <<"Disable or enable a tool.">>},
                   #{name => setPressed, args => <<"(Key, Pressed)">>, doc => <<"Set a toggle tool without firing change.">>}],
       doc => <<"Row of tool buttons and custom controls; tools that do not fit "
                "move into an overflow popup.">>},
     #{name => splitter, category => layout,
       signature => <<"splitter(Panes, Css, Attrs)">>,
       root => <<"ah-splitter">>,
       groups => #{orientation => {[vertical, horizontal], vertical}},
       flags => [disabled],
       options => [splitbar_size, resizable, step, name],
       behavior => <<"splitter">>, events => [<<"input">>, <<"change">>],
       option_docs => #{vertical => <<"Panes side by side, vertical bar (default).">>,
                       horizontal => <<"Panes stacked, horizontal bar.">>,
                       disabled => <<"No resizing.">>,
                       splitbar_size => <<"Bar thickness, default 5 (px).">>,
                       resizable => <<"false: no dragging or keys, default true.">>,
                       step => <<"Arrow key step, default 10 (px); Shift moves 5 steps.">>,
                       name => <<"Name of a hidden input holding the sizes.">>},
       methods => [#{name => setSizes, args => <<"(Percent)">>, doc => <<"Set the first pane to a percentage, without firing change.">>},
                   #{name => getSizes, args => <<"()">>, doc => <<"The two pane sizes in px.">>},
                   #{name => collapse, args => <<"()">>, doc => <<"Collapse the first pane.">>},
                   #{name => expand, args => <<"()">>, doc => <<"Restore the collapsed pane.">>}],
       doc => <<"Two panes with a draggable, keyboard-operable split bar; value is "
                "the pane sizes in percent.">>},
     #{name => listmenu, category => layout,
       signature => <<"listmenu(Items, Value, Css, Attrs)">>,
       root => <<"ah-listmenu">>,
       flags => [disabled],
       options => [header, back_button, filter, arrows, back_label,
                   filter_placeholder, animation, name],
       behavior => <<"listmenu">>, events => [<<"change">>, <<"ah:navigate">>],
       option_docs => #{disabled => <<"Disable the whole menu.">>,
                       header => <<"Show the header with title and back button, default true.">>,
                       back_button => <<"Show the back button, default true.">>,
                       filter => <<"Show a filter box for the current page, default false.">>,
                       arrows => <<"Show arrows on items with children, default true.">>,
                       back_label => <<"Back button text, default Back.">>,
                       filter_placeholder => <<"Filter box placeholder.">>,
                       animation => <<"slide (default), fade or none.">>,
                       name => <<"Name of a hidden input holding the value.">>},
       methods => [#{name => setValue, args => <<"(Key)">>, doc => <<"Select a leaf without firing change.">>},
                   #{name => back, args => <<"()">>, doc => <<"Go up one page.">>},
                   #{name => navigate, args => <<"(Key)">>, doc => <<"Open the page of an item on the current page.">>},
                   #{name => filter, args => <<"(Text)">>, doc => <<"Filter the current page.">>},
                   #{name => currentPage, args => <<"()">>, doc => <<"Id of the page shown.">>}],
       doc => <<"Drill-down list menu showing one level at a time; selecting a "
                "leaf sets the value.">>},
     #{name => status_bar, category => layout,
       signature => <<"status_bar(Segments, Css, Attrs)">>,
       root => <<"ah-status-bar">>,
       options => [content, dirty, labels],
       behavior => <<"status-bar">>,
       option_docs => #{content => <<"Text to count: adds a CJK-aware word count with details on hover.">>,
                       dirty => <<"true or false: adds the unsaved (amber) or saved (green) dot.">>,
                       labels => <<"Map overriding the English labels (count, cjk, words, chars, chars_no_space, lines, paragraphs, saved, unsaved).">>},
       methods => [],
       doc => <<"Bottom status bar with left and right segments, word count "
                "details and a saved/unsaved dot.">>}].
