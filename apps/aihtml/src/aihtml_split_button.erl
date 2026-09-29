%%%-------------------------------------------------------------------
%%% @doc A main action plus an arrow that opens a menu, ported from sigil's
%%% form components. Choosing an item sets `data-ah-value' on the root,
%%% updates the hidden input (when `Attrs' has a `name') and fires `change'
%%% (designs/04-components.md).
%%%
%%% Items are `Label | {Value, Label} | {Value, Label, ItemAttrs}' or
%%% `divider' (aihtml_lib_button); `ItemAttrs' may carry `icon'.
%%% split_button/4 builds an #ah_split_button{}
%%% (include/aihtml_split_button.hrl) and render/1 turns it into HTML, so
%%% pages may also write the record directly (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_split_button).
-behaviour(aihtml_element).

-include("aihtml_split_button.hrl").

-export([split_button/4, render/1, fields/1, catalog/0]).

-define(H, aihtml_html).
-define(E, aihtml_element).
-define(L, aihtml_lib_button).

%% @doc A main action and an arrow that opens a menu. A click on the main
%% half reaches the root (so `on(click, ...)' there is the main action);
%% clicks on the arrow and the menu do not. Choosing an item sets
%% `data-ah-value' and fires `change'. Options: `value', `menu_align'
%% (start | end, default end).
-spec split_button(aihtml_html:html(), [aihtml_lib_button:item()], aihtml_html:css(),
                   aihtml_html:attrs()) -> #ah_split_button{}.
split_button(Content, Items, Css, Attrs) ->
    ?E:build(?MODULE, #ah_split_button{body = Content, items = Items}, Css, Attrs).

%% @doc The field names of #ah_split_button{}.
-spec fields(atom()) -> [atom()].
fields(ah_split_button) -> record_info(fields, ah_split_button).

-spec render(#ah_split_button{}) -> aihtml_html:html().
render(#ah_split_button{body = Content, items = Items0, value = Value, name = Name,
                        variant = Variant, size = Size, menu_align = Align,
                        disabled = Disabled} = B) ->
    lists:member(Align, [start, 'end'])
        orelse error({aihtml, {bad_option, menu_align, Align}}),
    Classes = ?E:classes(?MODULE, B),
    Cur = case Value of
              undefined -> <<>>;
              V0 -> ?L:bin(V0)
          end,
    BtnCls = [<<"ah-btn">>, <<"ah-btn-", (atom_to_binary(Variant, utf8))/binary>>],
    Menu = [case I of
                divider ->
                    ?H:el('div', [], [<<"ah-split-button__divider">>], [{role, separator}]);
                _ ->
                    {V, Label, IA0} = ?L:item(I),
                    {Icon, IA} = ?L:take(<<"icon">>, IA0),
                    Off = ?L:is_disabled(IA),
                    ?H:el(button, [?L:menu_icon(<<"ah-split-button__item-icon">>, Icon),
                                   ?H:el(span, Label, [], [])],
                          [<<"ah-split-button__item">>],
                          [[{type, button}, {role, menuitem}, {tabindex, <<"-1">>},
                            {data_value, V}, {data_disabled, atom_to_binary(Off, utf8)}], IA])
            end || I <- Items0],
    ?H:el('div',
          [?H:el(button, Content, [<<"ah-split-button__main">> | BtnCls],
                 [{type, button}, {disabled, Disabled}]),
           ?H:el(button,
                 ?H:el(span, <<"▾"/utf8>>, [<<"ah-split-button__caret">>],
                       [{aria_hidden, <<"true">>}]),
                 [<<"ah-split-button__arrow">> | BtnCls],
                 [{type, button}, {aria_haspopup, menu}, {aria_expanded, <<"false">>},
                  {aria_label, <<"Open menu">>}, {disabled, Disabled}]),
           ?H:el('div', Menu, [<<"ah-split-button__menu">>], [{role, menu}]),
           ?L:hidden_input(Name, Cur)],
          Classes,
          [[{data_variant, Variant}, {data_size, Size},
            {data_disabled, atom_to_binary(Disabled, utf8)},
            {data_menu_align, Align}, {data_open, <<"false">>},
            {data_ah, <<"split-button">>}, {data_ah_value, Cur}],
           ?E:root_attrs(B, click)]).

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => split_button, category => form,
       signature => <<"split_button(Content, Items, Css, Attrs)">>,
       root => <<"ah-split-button">>,
       groups => #{variant => {[primary, secondary, success, warning, error, info,
                                outlined], primary},
                   size => {[sm, md, lg], md}},
       classes => maps:from_list([{M, []} || M <- [primary, secondary, success, warning,
                                                   error, info, outlined, sm, md, lg]]),
       options => [value, menu_align],
       behavior => <<"split-button">>,
       events => [<<"click">>, <<"change">>, <<"ah:open">>, <<"ah:close">>],
       doc => <<"A main action plus an arrow that opens more actions.">>,
       option_docs => #{value => <<"Initial data-ah-value.">>,
                        menu_align => <<"Align the menu with the start or the end (default) of the button.">>},
       methods => [#{name => open, args => <<"()">>, doc => <<"Open the menu.">>},
                   #{name => close, args => <<"()">>, doc => <<"Close the menu.">>},
                   #{name => setValue, args => <<"(Value)">>, doc => <<"Set data-ah-value without firing change.">>},
                   #{name => getValue, args => <<"()">>, doc => <<"Return the last chosen value.">>}]}].
