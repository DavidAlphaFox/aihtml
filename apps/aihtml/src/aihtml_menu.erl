%%%-------------------------------------------------------------------
%%% @doc Menu bar or context menu with nested submenus, ported from sigil's
%%% menu (DOM and class names are sigil's, so the ported styles under
%%% priv/css/sigil/components apply unchanged). The behaviour is in
%%% assets/js/components/menu.ts.
%%%
%%% Items take the shape of `aihtml_lib_nav:item()'. Selecting an item
%%% without `href' sets `data-ah-value' on the root to its key and fires
%%% `change' there.
%%%
%%% menu/4 builds an #ah_menu{} record (include/aihtml_menu.hrl) and
%%% render/1 turns it into HTML (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_menu).
-behaviour(aihtml_element).

-include("aihtml_menu.hrl").

-export([menu/4]).
-export([render/1, fields/1, catalog/0]).

-import(aihtml_lib_nav, [norm/1, label/1, key_attr/1, value_attr/1, is_value/2,
                         hidden_input/2, icon/2, text_or/2]).

-define(EL, aihtml_element).

%% @doc Menu bar with nested submenus (sigil menu). `Value' marks the
%% active item (`ah-menu-link-active'). Css: `horizontal' (default),
%% `vertical', `popup'; flags `show_arrows', `disabled'. Options: `title'
%% (collapsed title), `name' (hidden input), `click_to_open',
%% `keyboard' (default true), `minimize_width' (collapse to a hamburger
%% and drawer below this window width), `popup_target' (selector whose
%% right click opens a popup menu; default the document).
-spec menu([aihtml_lib_nav:item()], aihtml_lib_nav:key() | undefined, aihtml_html:css(),
           aihtml_html:attrs()) -> #ah_menu{}.
menu(Items, Value, Css, Attrs) ->
    ?EL:build(?MODULE, #ah_menu{items = Items, value = Value}, Css, Attrs).

%% @doc The field names of the record.
-spec fields(ah_menu) -> [atom()].
fields(ah_menu) -> record_info(fields, ah_menu).

-spec render(#ah_menu{}) -> aihtml_html:html().
render(#ah_menu{items = Items, value = Value, mode = Mode, disabled = Disabled,
                title = Title} = R) ->
    Classes = ?EL:classes(?MODULE, R),
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
                "and a responsive hamburger drawer.">>}].
