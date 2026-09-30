%%%-------------------------------------------------------------------
%%% @doc Drill-down list menu, ported from sigil's listmenu (DOM and class
%%% names are sigil's, so the ported styles under
%%% priv/css/sigil/components apply unchanged). The behaviour is in
%%% assets/js/components/listmenu.ts, aihtml's additions in
%%% priv/css/extra/listmenu.css.
%%%
%%% Items take the shape of `aihtml_lib_nav:item()'. Selecting a leaf sets
%%% `data-ah-value' on the root to its key and fires `change' there.
%%%
%%% ah_listmenu/4 builds an #ah_listmenu{} record (include/aihtml_listmenu.hrl)
%%% and render/1 turns it into HTML (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_listmenu).
-behaviour(aihtml_element).

-include("aihtml_listmenu.hrl").

-export([ah_listmenu/4]).
-export([render/1, fields/1, catalog/0]).

-import(aihtml_lib_nav, [norm/1, label/1, key_attr/1, value_attr/1, is_value/2,
                         hidden_input/2, icon/2]).

-define(EL, aihtml_element).

%% @doc Drill-down list menu (sigil listmenu): one level at a time, a
%% header with title and back button, optional filter. Items with children
%% open their page; a leaf becomes the value (`data-ah-value', `change').
%% When `Value' is nested, its page is shown initially. Flag `disabled'.
%% Options: `header' (default true), `back_button' (true), `filter'
%% (false), `arrows' (true), `back_label' ("Back"), `filter_placeholder'
%% ("Filter..."), `animation' (slide | fade | none), `name'.
-spec ah_listmenu([aihtml_lib_nav:item()], aihtml_lib_nav:key() | undefined, aihtml_html:css(),
                  aihtml_html:attrs()) -> #ah_listmenu{}.
ah_listmenu(Items, Value, Css, Attrs) ->
    ?EL:build(?MODULE, #ah_listmenu{items = Items, value = Value}, Css, Attrs).

%% @doc The field names of the record.
-spec fields(ah_listmenu) -> [atom()].
fields(ah_listmenu) -> record_info(fields, ah_listmenu).

-spec render(#ah_listmenu{}) -> aihtml_html:html().
render(#ah_listmenu{items = Items, value = Value, arrows = Arrows,
                    filter_placeholder = Placeholder} = R) ->
    Classes = ?EL:classes(?MODULE, R),
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

default(undefined, Default) -> Default;
default(V, _) -> V.

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => listmenu, category => layout,
       signature => <<"ah_listmenu(Items, Value, Css, Attrs)">>,
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
                "leaf sets the value.">>}].
