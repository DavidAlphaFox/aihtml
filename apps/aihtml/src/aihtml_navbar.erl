%%%-------------------------------------------------------------------
%%% @doc Navigation bar of selectable items, ported from sigil's navbar
%%% (DOM and class names are sigil's, so the ported styles under
%%% priv/css/sigil/components apply unchanged). The behaviour is in
%%% assets/js/components/navbar.ts, aihtml's additions in
%%% priv/css/extra/navbar.css.
%%%
%%% Items take the shape of `aihtml_lib_nav:item()'. Selecting an item
%%% sets `data-ah-value' on the root to its key and fires `change' there.
%%%
%%% ah_navbar/4 builds an #ah_navbar{} record (include/aihtml_navbar.hrl) and
%%% render/1 turns it into HTML (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_navbar).
-behaviour(aihtml_element).

-include("aihtml_navbar.hrl").

-export([ah_navbar/4]).
-export([render/1, fields/1, catalog/0]).

-import(aihtml_lib_nav, [norm/1, label/1, key_attr/1, value_attr/1, is_value/2,
                         hidden_input/2, icon/2, text_or/2, px/1]).

-define(EL, aihtml_element).

%% @doc Navigation bar of selectable items (sigil navbar), with optional
%% brand and trailing content. `Value' is the selected item's key. Css:
%% `horizontal' (default) or `vertical'; flags `minimized' (hamburger
%% header, items in a popup), `disabled'. Options: `brand', `extra'
%% (right-side html), `title' (minimized title), `minimized_height'
%% (default 36), `minimize_width' (minimize below this window width),
%% `columns' (item widths, e.g. [<<"30%">>, <<"70%">>]), `selection'
%% (default true), `name'.
-spec ah_navbar([aihtml_lib_nav:item()], aihtml_lib_nav:key() | undefined, aihtml_html:css(),
                aihtml_html:attrs()) -> #ah_navbar{}.
ah_navbar(Items, Value, Css, Attrs) ->
    ?EL:build(?MODULE, #ah_navbar{items = Items, value = Value}, Css, Attrs).

%% @doc The field names of the record.
-spec fields(ah_navbar) -> [atom()].
fields(ah_navbar) -> record_info(fields, ah_navbar).

-spec render(#ah_navbar{}) -> aihtml_html:html().
render(#ah_navbar{items = Items, value = Value, minimized = Minimized,
                  columns = Columns, minimized_height = Height,
                  title = Title} = R) ->
    Classes = ?EL:classes(?MODULE, R),
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

slot(undefined, _Class) -> [];
slot(Html, Class) -> aihtml_html:el('div', Html, [Class], []).

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => navbar, category => layout,
       signature => <<"ah_navbar(Items, Value, Css, Attrs)">>,
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
                "collapses to a hamburger header with a popup list.">>}].
