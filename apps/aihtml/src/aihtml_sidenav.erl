%%%-------------------------------------------------------------------
%%% @doc Application side navigation, ported from sigil's sidenav and
%%% nav-tree (DOM and class names are sigil's, so the ported styles under
%%% priv/css/sigil/components apply unchanged). The behaviour is in
%%% assets/js/components/sidenav.ts, aihtml's additions in
%%% priv/css/extra/sidenav.css.
%%%
%%% Items take the shape of `aihtml_lib_nav:item()'. Selecting an item
%%% sets `data-ah-value' on the root to its key and fires `change' there.
%%%
%%% ah_sidenav/4 builds an #ah_sidenav{} record (include/aihtml_sidenav.hrl)
%%% and render/1 turns it into HTML (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_sidenav).
-behaviour(aihtml_element).

-include("aihtml_sidenav.hrl").

-export([ah_sidenav/4]).
-export([render/1, fields/1, catalog/0]).

-export_type([group/0]).

-import(aihtml_lib_nav, [norm/1, label/1, key_attr/1, value_attr/1, is_value/2,
                         hidden_input/2]).

%% A sidenav group.
-type group() :: #{label => aihtml_html:html(), items := [aihtml_lib_nav:item()]}.

-define(EL, aihtml_element).

-define(TOGGLE_SVG, <<"<svg width=\"18\" height=\"18\" viewBox=\"0 0 24 24\" fill=\"none\" "
                      "stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" "
                      "stroke-linejoin=\"round\" aria-hidden=\"true\"><path d=\"m15 18-6-6 6-6\"/>"
                      "</svg>">>).
-define(CARET_SVG, <<"<svg class=\"ah-nav-tree__caret-svg\" width=\"16\" height=\"16\" "
                     "viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" "
                     "stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\" "
                     "aria-hidden=\"true\"><path d=\"m6 9 6 6 6-6\"/></svg>">>).

%% @doc Application side navigation: brand, grouped tree of links (sigil
%% sidenav + nav-tree) and footer. `Groups' is `[#{label, items}]' or a
%% plain item list; `Value' is the active item's key (its ancestors open).
%% Flag `collapsed' renders the narrow, icon-only form. Options: `brand'
%% (`#{name, logo, href}' or html), `footer', `collapsible' (a toggle
%% button), `route_prefix' (href = prefix ++ key for items without href),
%% `name'.
-spec ah_sidenav([group()] | [aihtml_lib_nav:item()],
                 aihtml_lib_nav:key() | undefined, aihtml_html:css(), aihtml_html:attrs()) ->
          #ah_sidenav{}.
ah_sidenav(Groups, Value, Css, Attrs) ->
    ?EL:build(?MODULE, #ah_sidenav{groups = Groups, value = Value}, Css, Attrs).

%% @doc The field names of the record.
-spec fields(ah_sidenav) -> [atom()].
fields(ah_sidenav) -> record_info(fields, ah_sidenav).

-spec render(#ah_sidenav{}) -> aihtml_html:html().
render(#ah_sidenav{groups = Groups0, value = Value, collapsed = Collapsed,
                   route_prefix = Prefix} = R) ->
    Classes = ?EL:classes(?MODULE, R),
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

%% Inside an existing wrapper: an image or the html itself.
icon_body(Src) when is_binary(Src) -> aihtml_html:void(img, [], [{src, Src}, {alt, <<>>}]);
icon_body(Html) -> Html.

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => sidenav, category => layout,
       signature => <<"ah_sidenav(Groups, Value, Css, Attrs)">>,
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
                "nodes, footer; can collapse to icons.">>}].
