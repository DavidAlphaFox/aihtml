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
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_layout_nav).

-export([menu/4, navbar/4, sidenav/4, toolbar/3, splitter/3, listmenu/4,
         status_bar/3]).
-export([catalog/0, examples/0]).

-export_type([item/0, key/0, icon/0, tool/0, pane/0, segment/0]).

-type html() :: aihtml_html:html().
-type key() :: atom() | binary() | integer().
-type icon() :: binary() | html().
-type item() :: #{key => key(), label => html(), icon => icon(),
                  href => binary(), target => binary(), disabled => boolean(),
                  children => [item()],
                  columns => [#{header => html(), children => [item()]}],
                  open => left | up | [left | up],
                  expanded => boolean(),
                  divider => true}
              | divider | {key(), html()}.
%% A toolbar tool: a button (map), a separator, or any other html (custom).
-type tool() :: #{key => key(), label => html(), icon => icon(),
                  title => binary(), disabled => boolean(),
                  toggle => boolean(), pressed => boolean(),
                  minimizable => boolean()}
              | separator | {custom, html()} | html().
%% A splitter pane: its content, or a map with its initial size
%% (<<"30%">> or pixels) and minimum size in pixels.
-type pane() :: #{content => html(), size => binary() | number(),
                  min => non_neg_integer()}
              | html().
-type segment() :: #{content := html(), align => left | right}
                 | #{count := integer(), label => html(),
                     details => [{html(), html()}], align => left | right}
                 | html().

-define(E(Name), aihtml_catalog:entry(?MODULE, Name)).

-define(TOGGLE_SVG, <<"<svg width=\"18\" height=\"18\" viewBox=\"0 0 24 24\" fill=\"none\" "
                      "stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" "
                      "stroke-linejoin=\"round\" aria-hidden=\"true\"><path d=\"m15 18-6-6 6-6\"/>"
                      "</svg>">>).
-define(CARET_SVG, <<"<svg class=\"ah-nav-tree__caret-svg\" width=\"16\" height=\"16\" "
                     "viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" "
                     "stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\" "
                     "aria-hidden=\"true\"><path d=\"m6 9 6 6 6-6\"/></svg>">>).
-define(ICON_GRID, <<"<svg width=\"20\" height=\"20\" viewBox=\"0 0 24 24\" fill=\"none\" "
                     "stroke=\"currentColor\" stroke-width=\"2\"><rect x=\"3\" y=\"3\" "
                     "width=\"7\" height=\"7\"/><rect x=\"14\" y=\"3\" width=\"7\" height=\"7\"/>"
                     "<rect x=\"3\" y=\"14\" width=\"7\" height=\"7\"/><rect x=\"14\" y=\"14\" "
                     "width=\"7\" height=\"7\"/></svg>">>).
-define(ICON_CHART, <<"<svg width=\"20\" height=\"20\" viewBox=\"0 0 24 24\" fill=\"none\" "
                     "stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" "
                     "stroke-linejoin=\"round\"><path d=\"M3 3v18h18\"/><path d=\"m7 14 4-4 4 4 5-5\"/></svg>">>).
-define(ICON_USER, <<"<svg width=\"20\" height=\"20\" viewBox=\"0 0 24 24\" fill=\"none\" "
                     "stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" "
                     "stroke-linejoin=\"round\"><circle cx=\"12\" cy=\"8\" r=\"4\"/><path d=\"M4 21a8 8 0 0 1 16 0\"/></svg>">>).
-define(ICON_GEAR, <<"<svg width=\"20\" height=\"20\" viewBox=\"0 0 24 24\" fill=\"none\" "
                     "stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" "
                     "stroke-linejoin=\"round\"><circle cx=\"12\" cy=\"12\" r=\"3\"/><path d=\"M12 2v3M12 19v3M2 12h3M19 12h3M4.9 4.9l2.1 2.1M17 17l2.1 2.1M4.9 19.1 7 17M17 7l2.1-2.1\"/></svg>">>).

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
          aihtml_html:element().
menu(Items, Value, Css, Attrs) ->
    E = ?E(menu),
    {Opts, Rest} = aihtml_catalog:split_options(E, Attrs),
    Classes = aihtml_catalog:classes(E, Css),
    Horizontal = lists:member(<<"ah-menu-horizontal">>, Classes),
    Disabled = lists:member(disabled, aihtml_catalog:flags(E, Css)),
    Title = maps:get(title, Opts, undefined),
    Btn = aihtml_html:el(button,
              aihtml_html:el(span, [aihtml_html:el(span, [], [], []) || _ <- [1, 2, 3]],
                             [<<"ah-menu-hamburger">>], [{aria_hidden, <<"true">>}]),
              [<<"ah-menu-minimized-btn">>],
              [{type, button}, {aria_label, text_or(Title, <<"Menu">>)},
               {aria_haspopup, menu}, {aria_expanded, <<"false">>}]),
    List = aihtml_html:el(ul, [menu_item(norm(I), Value) || I <- Items],
                          [<<"ah-menu-list">>], [{role, none}]),
    aihtml_html:el('div', [Btn, List, hidden_input(Opts, Value)], Classes,
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
                     {data_ah_click_to_open, maps:get(click_to_open, Opts, false)},
                     {data_ah_keyboard, maps:get(keyboard, Opts, true) =:= false
                                            andalso <<"false">>},
                     {data_ah_minimize_width, maps:get(minimize_width, Opts, undefined)},
                     {data_ah_popup_target, maps:get(popup_target, Opts, undefined)}],
                    Rest]).

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
          aihtml_html:element().
navbar(Items, Value, Css, Attrs) ->
    E = ?E(navbar),
    {Opts, Rest} = aihtml_catalog:split_options(E, Attrs),
    Classes = aihtml_catalog:classes(E, Css),
    Vertical = lists:member(<<"ah-navbar-vertical">>, Classes),
    Minimized = lists:member(minimized, aihtml_catalog:flags(E, Css)),
    Columns = maps:get(columns, Opts, []),
    Norm = [norm(I) || I <- Items, norm(I) =/= divider],
    Height = maps:get(minimized_height, Opts, 36),
    Title = maps:get(title, Opts, <<>>),
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
    Brand = slot(maps:get(brand, Opts, undefined), <<"ah-navbar-brand">>),
    Extra = slot(maps:get(extra, Opts, undefined), <<"ah-navbar-extra">>),
    aihtml_html:el('div', [Header, Brand, ItemEls, Extra, hidden_input(Opts, Value)],
                   Classes,
                   [[{data_ah, <<"navbar">>}, {role, tablist},
                     {aria_orientation, case Vertical of
                                            true -> vertical;
                                            false -> horizontal
                                        end},
                     {data_ah_value, value_attr(Value)},
                     {data_ah_minimized, Minimized andalso <<"static">>},
                     {data_ah_selection, maps:get(selection, Opts, true) =:= false
                                             andalso <<"false">>},
                     {data_ah_minimize_width, maps:get(minimize_width, Opts, undefined)}],
                    Rest]).

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
          aihtml_html:element().
sidenav(Groups0, Value, Css, Attrs) ->
    E = ?E(sidenav),
    {Opts, Rest} = aihtml_catalog:split_options(E, Attrs),
    Classes = aihtml_catalog:classes(E, Css),
    Collapsed = lists:member(collapsed, aihtml_catalog:flags(E, Css)),
    Groups = case Groups0 of
                 [#{items := _} | _] -> Groups0;
                 _ -> [#{items => Groups0}]
             end,
    Prefix = maps:get(route_prefix, Opts, undefined),
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
    Toggle = case maps:get(collapsible, Opts, false) of
                 false -> [];
                 true ->
                     aihtml_html:el(button, {safe, ?TOGGLE_SVG},
                                    [<<"ah-sidenav__toggle">>],
                                    [{type, button},
                                     {aria_label, <<"Toggle navigation">>},
                                     {aria_expanded, atom_to_binary(not Collapsed)}])
             end,
    Footer = case maps:get(footer, Opts, undefined) of
                 undefined -> [];
                 F -> aihtml_html:el('div', F, [<<"ah-sidenav__footer">>], [])
             end,
    aihtml_html:el(aside,
        [aihtml_html:el('div', [brand(maps:get(brand, Opts, undefined)), Toggle],
                        [<<"ah-sidenav__head">>], []),
         aihtml_html:el('div', Tree, [<<"ah-sidenav__nav">>], []),
         Footer, hidden_input(Opts, Value)],
        Classes,
        [[{data_ah, <<"sidenav">>}, {data_ah_value, value_attr(Value)}], Rest]).


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
-spec toolbar([tool()], aihtml_html:css(), aihtml_html:attrs()) -> aihtml_html:element().
toolbar(Tools, Css, Attrs) ->
    E = ?E(toolbar),
    {Opts, Rest} = aihtml_catalog:split_options(E, Attrs),
    Runs = tool_runs(Tools),
    Els = lists:join(aihtml_html:el('div', [], [<<"ah-toolbar-separator">>],
                                    [{role, separator}, {aria_orientation, vertical}]),
                     [tool_run(Run, Last) || {Run, Last} <- mark_last(Runs)]),
    MinBtn = aihtml_html:el('div', <<"\x{2630}"/utf8>>, [<<"ah-toolbar-minimize-btn">>],
                            [{role, button}, {tabindex, 0}, {aria_label, <<"More tools">>},
                             {aria_haspopup, menu}, {aria_expanded, <<"false">>}]),
    aihtml_html:el('div', [Els, MinBtn], aihtml_catalog:classes(E, Css),
                   [[{data_ah, <<"toolbar">>}, {role, toolbar},
                     {aria_orientation, horizontal},
                     {data_ah_popup_width, maps:get(popup_width, Opts, undefined)}],
                    Rest]).

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
-spec splitter([pane()], aihtml_html:css(), aihtml_html:attrs()) -> aihtml_html:element().
splitter(Panes, Css, Attrs) ->
    E = ?E(splitter),
    {Opts, Rest} = aihtml_catalog:split_options(E, Attrs),
    Classes = aihtml_catalog:classes(E, Css),
    Horiz = lists:member(<<"ah-splitter-horizontal">>, Classes),
    [P0, P1] = case [pane(P) || P <- Panes] of
                   [A, B] -> [A, B];
                   [A] -> [A, pane([])];
                   _ -> error({aihtml, {splitter_needs_two_panes, length(Panes)}})
               end,
    Bar = maps:get(splitbar_size, Opts, 5),
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
         hidden_input(Opts, Value)],
        Classes,
        [[{data_ah, <<"splitter">>}, {data_ah_value, Value},
          {data_ah_min, iolist_to_binary([integer_to_binary(maps:get(min, P0, 0)), $,,
                                          integer_to_binary(maps:get(min, P1, 0))])},
          {data_ah_resizable, maps:get(resizable, Opts, true) =:= false andalso <<"false">>},
          {data_ah_step, maps:get(step, Opts, undefined)}],
         Rest]).

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
          aihtml_html:element().
listmenu(Items, Value, Css, Attrs) ->
    E = ?E(listmenu),
    {Opts, Rest} = aihtml_catalog:split_options(E, Attrs),
    {Pages, _} = lm_pages(Items, <<"root">>, 0),
    Path = lm_path(Pages, Value),
    Current = case Path of [] -> <<"root">>; _ -> lists:last(Path) end,
    Arrows = maps:get(arrows, Opts, true),
    Titles = maps:from_list([{integer_to_binary(Id), label(I)}
                             || {_, Is} <- Pages, {Id, I} <- Is]),
    Header = case maps:get(header, Opts, true) of
                 false -> [];
                 true ->
                     aihtml_html:el('div',
                         [case maps:get(back_button, Opts, true) of
                              false -> [];
                              true ->
                                  aihtml_html:el(button,
                                      [aihtml_html:el(span, <<"\x{25C0}"/utf8>>,
                                                      [<<"ah-listmenu-back-arrow">>],
                                                      [{aria_hidden, <<"true">>}]),
                                       aihtml_html:el(span, maps:get(back_label, Opts, <<"Back">>),
                                                      [<<"ah-listmenu-back-label">>], [])],
                                      [<<"ah-listmenu-back">>],
                                      [{type, button}, {tabindex, -1},
                                       {style, Path =:= [] andalso <<"display:none">>}])
                          end,
                          aihtml_html:el(span, maps:get(Current, Titles, []),
                                         [<<"ah-listmenu-title">>], [])],
                         [<<"ah-listmenu-header">>], [])
             end,
    Filter = case maps:get(filter, Opts, false) of
                 false -> [];
                 true ->
                     aihtml_html:el('div',
                         aihtml_html:void(input, [<<"ah-listmenu-filter-input">>],
                                          [{type, text}, {tabindex, -1},
                                           {placeholder, maps:get(filter_placeholder, Opts,
                                                                  <<"Filter...">>)},
                                           {aria_label, maps:get(filter_placeholder, Opts,
                                                                 <<"Filter">>)}]),
                         [<<"ah-listmenu-filter">>], [])
             end,
    Viewport = aihtml_html:el('div',
                   [lm_page(P, Is, P =:= Current, Value, Arrows) || {P, Is} <- Pages],
                   [<<"ah-listmenu-viewport">>], []),
    aihtml_html:el('div', [Header, Filter, Viewport, hidden_input(Opts, Value)],
                   aihtml_catalog:classes(E, Css),
                   [[{data_ah, <<"listmenu">>}, {tabindex, 0},
                     {data_ah_value, value_attr(Value)},
                     {data_ah_stack, iolist_to_binary(lists:join($,, Path))},
                     {data_ah_animation, maps:get(animation, Opts, undefined)}],
                    Rest]).

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
-spec status_bar([segment()], aihtml_html:css(), aihtml_html:attrs()) ->
          aihtml_html:element().
status_bar(Segments, Css, Attrs) ->
    E = ?E(status_bar),
    {Opts, Rest} = aihtml_catalog:split_options(E, Attrs),
    Labels = maps:merge(default_labels(), maps:get(labels, Opts, #{})),
    Words = case maps:get(content, Opts, undefined) of
                undefined -> [];
                Text -> [word_count_segment(Text, Labels)]
            end,
    Dirty = maps:get(dirty, Opts, undefined),
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
        aihtml_catalog:classes(E, Css),
        [[{role, status},
          {data_ah, <<"status-bar">>},
          {data_dirty, Dirty =/= undefined andalso atom_to_binary(Dirty =:= true)}],
         Rest]).

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

hidden_input(Opts, Value) ->
    case maps:get(name, Opts, undefined) of
        undefined -> [];
        Name -> aihtml_html:void(input, [], [{type, hidden}, {name, Name},
                                            {value, case Value of
                                                        undefined -> <<>>;
                                                        _ -> key_bin(Value)
                                                    end}])
    end.

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
       doc => <<"Bar of selectable navigation items with brand and trailing slots; "
                "collapses to a hamburger header with a popup list.">>},
     #{name => sidenav, category => layout,
       signature => <<"sidenav(Groups, Value, Css, Attrs)">>,
       root => <<"ah-sidenav">>,
       flags => [collapsed],
       options => [brand, footer, collapsible, route_prefix, name],
       behavior => <<"sidenav">>, events => [<<"change">>, <<"ah:collapse">>],
       doc => <<"Application sidebar: brand, grouped link tree with collapsible "
                "nodes, footer; can collapse to icons.">>},
     #{name => toolbar, category => layout,
       signature => <<"toolbar(Tools, Css, Attrs)">>,
       root => <<"ah-toolbar">>,
       flags => [disabled],
       options => [popup_width],
       behavior => <<"toolbar">>, events => [<<"change">>, <<"click">>],
       doc => <<"Row of tool buttons and custom controls; tools that do not fit "
                "move into an overflow popup.">>},
     #{name => splitter, category => layout,
       signature => <<"splitter(Panes, Css, Attrs)">>,
       root => <<"ah-splitter">>,
       groups => #{orientation => {[vertical, horizontal], vertical}},
       flags => [disabled],
       options => [splitbar_size, resizable, step, name],
       behavior => <<"splitter">>, events => [<<"input">>, <<"change">>],
       doc => <<"Two panes with a draggable, keyboard-operable split bar; value is "
                "the pane sizes in percent.">>},
     #{name => listmenu, category => layout,
       signature => <<"listmenu(Items, Value, Css, Attrs)">>,
       root => <<"ah-listmenu">>,
       flags => [disabled],
       options => [header, back_button, filter, arrows, back_label,
                   filter_placeholder, animation, name],
       behavior => <<"listmenu">>, events => [<<"change">>, <<"ah:navigate">>],
       doc => <<"Drill-down list menu showing one level at a time; selecting a "
                "leaf sets the value.">>},
     #{name => status_bar, category => layout,
       signature => <<"status_bar(Segments, Css, Attrs)">>,
       root => <<"ah-status-bar">>,
       options => [content, dirty, labels],
       behavior => <<"status-bar">>,
       doc => <<"Bottom status bar with left and right segments, word count "
                "details and a saved/unsaved dot.">>}].

%%%===================================================================
%%% Examples
%%%===================================================================

-spec examples() -> [{atom(), binary(), html()}].
examples() ->
    File = [{new, <<"New">>}, {open, <<"Open…"/utf8>>},
            #{key => recent, label => <<"Recent">>,
              children => [{a, <<"report.txt">>}, {b, <<"notes.md">>}]},
            divider, {save, <<"Save">>},
            #{key => export, label => <<"Export">>, disabled => true}],
    Bar = [#{key => file, label => <<"File">>, children => File},
           #{key => edit, label => <<"Edit">>,
             children => [{undo, <<"Undo">>}, {redo, <<"Redo">>}, divider,
                          {cut, <<"Cut">>}, {copy, <<"Copy">>}, {paste, <<"Paste">>}]},
           #{key => view, label => <<"View">>,
             columns => [#{header => <<"Panels">>,
                           children => [{sidebar, <<"Sidebar">>}, {console, <<"Console">>}]},
                         #{header => <<"Zoom">>,
                           children => [{zoom_in, <<"Zoom in">>}, {zoom_out, <<"Zoom out">>}]}]},
           #{key => help, label => <<"Help">>, href => <<"#help">>}],
    Nav = [{home, <<"Home">>}, {products, <<"Products">>}, {pricing, <<"Pricing">>},
           #{key => docs, label => <<"Docs">>, disabled => true}],
    Side = [#{label => <<"Overview">>,
              items => [#{key => dashboard, label => <<"Dashboard">>, icon => {safe, ?ICON_GRID}},
                        #{key => analytics, label => <<"Analytics">>, icon => {safe, ?ICON_CHART}}]},
            #{label => <<"Management">>,
              items => [#{key => users, label => <<"Users">>, icon => {safe, ?ICON_USER},
                          children => [{user_list, <<"List">>}, {user_roles, <<"Roles">>}]},
                        #{key => settings, label => <<"Settings">>, icon => {safe, ?ICON_GEAR},
                          children => [{general, <<"General">>}, {billing, <<"Billing">>}]}]}],
    Tools = [#{key => bold, label => <<"B">>, title => <<"Bold">>, toggle => true, pressed => true},
             #{key => italic, label => <<"I">>, title => <<"Italic">>, toggle => true},
             #{key => underline, label => <<"U">>, title => <<"Underline">>, toggle => true},
             separator,
             #{key => left, label => <<"Left">>}, #{key => center, label => <<"Center">>},
             #{key => right, label => <<"Right">>},
             separator,
             #{key => undo, label => <<"Undo">>}, #{key => redo, label => <<"Redo">>,
                                                   disabled => true},
             separator,
             {custom, aihtml_html:el(select, [aihtml_html:el(option, <<"Paragraph">>, [], []),
                                              aihtml_html:el(option, <<"Heading">>, [], [])],
                                     [<<"text-sm">>], [{aria_label, <<"Block type">>}])}],
    Pane = fun(T) -> aihtml_html:el('div', T, [<<"p-3 text-sm">>], []) end,
    Drill = [#{key => fruit, label => <<"Fruit">>,
               children => [{apple, <<"Apple">>}, {banana, <<"Banana">>},
                            #{key => citrus, label => <<"Citrus">>,
                              children => [{lemon, <<"Lemon">>}, {orange, <<"Orange">>}]}]},
             #{key => veg, label => <<"Vegetables">>,
               children => [{carrot, <<"Carrot">>}, {pea, <<"Pea">>}]},
             {bread, <<"Bread">>},
             #{key => cake, label => <<"Cake">>, disabled => true}],
    Box = fun(Cls, Content) -> aihtml_html:el('div', Content, [Cls], []) end,
    [{menu, <<"horizontal with submenus, columns, divider, disabled, link">>,
      menu(Bar, copy, [show_arrows], [{id, <<"m-bar">>}, {name, <<"cmd">>}])},
     {menu, <<"vertical">>,
      menu(File, undefined, [vertical], [{id, <<"m-vert">>}])},
     {menu, <<"popup (right click the box)">>,
      [aihtml_html:el('div', <<"Right click here">>,
                      [<<"border border-dashed border-line rounded p-6 text-sm text-muted">>],
                      [{id, <<"menu-popup-area">>}]),
       menu([{cut, <<"Cut">>}, {copy, <<"Copy">>}, {paste, <<"Paste">>}, divider,
             #{key => more, label => <<"More">>,
               children => [{rename, <<"Rename">>}, {delete, <<"Delete">>}]}],
            undefined, [popup],
            [{id, <<"m-pop">>}, {popup_target, <<"#menu-popup-area">>}])]},
     {navbar, <<"brand, items, trailing content">>,
      navbar(Nav, products, [],
             [{id, <<"nb">>},
              {brand, aihtml_html:el(strong, <<"Acme">>, [], [])},
              {extra, aihtml_html:el(button, <<"Sign in">>, [<<"ah-btn ah-btn-sm">>],
                                     [{type, button}])}])},
     {navbar, <<"vertical">>,
      Box(<<"w-56">>, navbar(Nav, home, [vertical], []))},
     {navbar, <<"minimized (hamburger + popup)">>,
      Box(<<"w-72">>, navbar(Nav, pricing, [minimized],
                             [{id, <<"nb-min">>}, {title, <<"Pricing">>}]))},
     {sidenav, <<"brand, groups, active item, collapsible">>,
      Box(<<"flex h-[520px] border border-line rounded overflow-hidden">>,
          [sidenav(Side, user_roles, [],
                   [{id, <<"sn">>}, {collapsible, true},
                    {brand, #{name => <<"Sigil">>, logo => aihtml_html:el(b, <<"S">>, [], [])}},
                    {footer, aihtml_html:el(span, <<"v1.0">>, [<<"text-xs text-muted">>], [])},
                    {style, <<"--ah-ssn-height:100%">>}]),
           Box(<<"p-4 text-sm text-muted">>, <<"Content">>)])},
     {sidenav, <<"collapsed">>,
      Box(<<"flex h-[300px] border border-line rounded overflow-hidden">>,
          sidenav(Side, dashboard, [collapsed],
                  [{collapsible, true}, {brand, #{logo => aihtml_html:el(b, <<"S">>, [], []), name => <<"Sigil">>}},
                   {style, <<"--ah-ssn-height:100%">>}]))},
     {toolbar, <<"toggles, groups, separators, custom control">>,
      toolbar(Tools, [], [{id, <<"tb">>}, {aria_label, <<"Formatting">>}])},
     {toolbar, <<"overflow (narrow)">>,
      Box(<<"w-64">>, toolbar(Tools, [], [{id, <<"tb-narrow">>}]))},
     {splitter, <<"vertical bar (side by side)">>,
      Box(<<"h-40 border border-line rounded">>,
          splitter([#{content => Pane(<<"Left">>), size => <<"30%">>, min => 80},
                    #{content => Pane(<<"Right">>), min => 80}],
                   [], [{id, <<"sp">>}, {name, <<"split">>}]))},
     {splitter, <<"horizontal bar (stacked)">>,
      Box(<<"h-56 border border-line rounded">>,
          splitter([#{content => Pane(<<"Top">>), size => <<"40%">>, min => 40},
                    Pane(<<"Bottom">>)], [horizontal], [{id, <<"sp-h">>}]))},
     {listmenu, <<"drill-down with filter">>,
      Box(<<"w-72 border border-line rounded">>,
          listmenu(Drill, undefined, [], [{id, <<"lm">>}, {filter, true}]))},
     {listmenu, <<"nested value shown initially">>,
      Box(<<"w-72 border border-line rounded">>,
          listmenu(Drill, lemon, [], [{id, <<"lm-v">>}]))},
     {status_bar, <<"word count, segments, saved state">>,
      status_bar([<<"Ln 12, Col 4">>,
                  #{content => <<"UTF-8">>, align => right},
                  #{content => <<"Markdown">>, align => right}],
                 [], [{content, <<"Hello 世界，这是一段中英混排文本。\n\n第二段。"/utf8>>},
                      {dirty, false}])},
     {status_bar, <<"unsaved">>,
      status_bar([#{count => 3, label => <<"errors">>,
                    details => [{<<"Errors">>, 3}, {<<"Warnings">>, 7}]}],
                 [], [{dirty, true}])}].

