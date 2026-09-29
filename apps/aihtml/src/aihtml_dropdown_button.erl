%%%-------------------------------------------------------------------
%%% @doc A button that opens a menu of items, ported from sigil's form
%%% components. Choosing an item sets `data-ah-value' on the root, updates
%%% the hidden input (when `Attrs' has a `name') and fires `change'
%%% (designs/04-components.md).
%%%
%%% Items are `Label | {Value, Label} | {Value, Label, ItemAttrs}' or
%%% `divider' (aihtml_lib_button); `ItemAttrs' may carry `icon'.
%%% dropdown_button/4 builds an #ah_dropdown_button{}
%%% (include/aihtml_dropdown_button.hrl) and render/1 turns it into HTML, so
%%% pages may also write the record directly (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_dropdown_button).
-behaviour(aihtml_element).

-include("aihtml_dropdown_button.hrl").

-export([dropdown_button/4, render/1, fields/1, catalog/0]).

-define(H, aihtml_html).
-define(E, aihtml_element).
-define(L, aihtml_lib_button).

%% @doc A button that opens a menu. Choosing an item sets `data-ah-value'
%% on the root and fires `change'. Options: `value' (the initially
%% selected item), `auto_open' (open on hover).
-spec dropdown_button(aihtml_html:html(), [aihtml_lib_button:item()], aihtml_html:css(),
                      aihtml_html:attrs()) -> #ah_dropdown_button{}.
dropdown_button(Content, Items, Css, Attrs) ->
    ?E:build(?MODULE, #ah_dropdown_button{body = Content, items = Items}, Css, Attrs).

%% @doc The field names of #ah_dropdown_button{}.
-spec fields(atom()) -> [atom()].
fields(ah_dropdown_button) -> record_info(fields, ah_dropdown_button).

-spec render(#ah_dropdown_button{}) -> aihtml_html:html().
render(#ah_dropdown_button{body = Content, items = Items0, value = Value, name = Name,
                           auto_open = AutoOpen, disabled = Disabled} = B) ->
    Cur = case Value of
              undefined -> <<>>;
              V0 -> ?L:bin(V0)
          end,
    Menu = [case I of
                divider ->
                    ?H:el('div', [], [<<"ah-dropdown-btn-divider">>], [{role, separator}]);
                _ ->
                    {V, Label, IA0} = ?L:item(I),
                    {Icon, IA} = ?L:take(<<"icon">>, IA0),
                    Sel = V =:= Cur andalso Cur =/= <<>>,
                    ?H:el(button, [?L:menu_icon(<<"ah-dropdown-btn-item-icon">>, Icon),
                                   ?H:el(span, Label, [], [])],
                          [<<"ah-dropdown-btn-item">>, [<<"selected">> || Sel]],
                          [[{type, button}, {role, menuitem}, {tabindex, <<"-1">>},
                            {data_value, V}], IA])
            end || I <- Items0],
    Trigger = ?H:el(button,
                    [?H:el('div', Content, [<<"ah-dropdown-btn-content">>], []),
                     ?H:el('div', ?H:el(span, [], [<<"ah-dropdown-btn-arrow-icon">>], []),
                           [<<"ah-dropdown-btn-arrow">>], [{aria_hidden, <<"true">>}])],
                    [<<"ah-dropdown-btn-wrapper">>],
                    [{type, button}, {aria_haspopup, menu}, {aria_expanded, <<"false">>},
                     {disabled, Disabled}]),
    ?H:el('div',
          [Trigger,
           ?H:el('div', Menu, [<<"ah-dropdown-btn-popup">>], [{role, menu}, {hidden, true}]),
           ?L:hidden_input(Name, Cur)],
          [?E:classes(?MODULE, B), [<<"ah-dropdown-btn-disabled">> || Disabled],
           [<<"ah-dropdown-btn-auto-open">> || AutoOpen]],
          [[{aria_disabled, Disabled andalso <<"true">>},
            {data_ah, <<"dropdown-button">>}, {data_ah_value, Cur}],
           ?E:root_attrs(B, change)]).

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => dropdown_button, category => form,
       signature => <<"dropdown_button(Content, Items, Css, Attrs)">>,
       root => <<"ah-dropdown-btn">>,
       groups => #{variant => {[primary, success, warning, error, outlined], none},
                   size => {[sm, md, lg], md}},
       flags => [rounded], classes => #{md => []},
       options => [value, auto_open],
       behavior => <<"dropdown-button">>,
       events => [<<"change">>, <<"ah:open">>, <<"ah:close">>],
       doc => <<"A button that opens a menu; choosing an item fires change.">>,
       option_docs => #{rounded => <<"Pill-shaped trigger.">>,
                        value => <<"Item value marked as selected initially.">>,
                        auto_open => <<"Open the menu on hover and close it when the pointer leaves.">>},
       methods => [#{name => open, args => <<"()">>, doc => <<"Open the menu.">>},
                   #{name => close, args => <<"()">>, doc => <<"Close the menu.">>},
                   #{name => toggle, args => <<"()">>, doc => <<"Open or close the menu.">>},
                   #{name => setValue, args => <<"(Value)">>, doc => <<"Mark an item as selected without firing change.">>},
                   #{name => getValue, args => <<"()">>, doc => <<"Return the last chosen value.">>}]}].
