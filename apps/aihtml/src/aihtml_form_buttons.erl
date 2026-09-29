%%%-------------------------------------------------------------------
%%% @doc Buttons, ported from sigil's form components (DOM and classes as
%%% sigil renders them, so the styles in priv/css/sigil apply unchanged).
%%%
%%%   button/4             a native button
%%%   link_button/4        an `<a>' styled as a button
%%%   toggle_button/4      a pressed/released button (value "true"/"false")
%%%   button_group/4       joined buttons, optionally radio or checkbox
%%%   segmented_control/4  mutually exclusive segments
%%%   dropdown_button/4    a button that opens a menu of items
%%%   split_button/4       a main action plus an arrow that opens a menu
%%%
%%% Value-bearing components keep their value in `data-ah-value' on the
%%% root, render a hidden input when `Attrs' has a `name', and fire
%%% `change' on the root (designs/04-components.md).
%%%
%%% Items (button_group, segmented_control, menus) are
%%%   Label | {Value, Label} | {Value, Label, ItemAttrs}
%%% and a menu also takes `divider'. `ItemAttrs' are HTML attributes of
%%% the item's button, e.g. `[{disabled, true}]'; menus also read `icon'.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_form_buttons).

-export([button/4, link_button/4, toggle_button/4, button_group/4,
         segmented_control/4, dropdown_button/4, split_button/4,
         catalog/0]).

-export_type([item/0]).

-define(H, aihtml_html).

-type html() :: aihtml_html:html().
-type css() :: aihtml_html:css().
-type attrs() :: aihtml_html:attrs().
-type element() :: aihtml_html:element().
-type item() :: html() | {term(), html()} | {term(), html(), attrs()} | divider.

-define(BTN_VARIANTS, [primary, secondary, outlined, success, warning, error,
                       info, default, borderless]).

%%%===================================================================
%%% button, link_button, toggle_button
%%%===================================================================

%% @doc A native `<button type="button">'. `Value' becomes its `value'
%% attribute (`undefined' leaves it out). Options: `icon' (HTML shown
%% beside the text), `img' (an image URL, 16px), `icon_position' (left |
%% right | top | bottom).
-spec button(html(), term(), css(), attrs()) -> element().
button(Content, Value, Css, Attrs0) ->
    E = entry(button),
    {Opts, Attrs} = aihtml_catalog:split_options(E, Attrs0),
    Disabled = is_disabled(Attrs),
    {Body, ImgCls} = with_icon(Content, Opts),
    ?H:el(button, Body,
          [aihtml_catalog:classes(E, Css), ImgCls, [<<"ah-btn-disabled">> || Disabled]],
          [[{type, button}, {value, value_attr(Value)}], Attrs]).

%% @doc An `<a href=Href>' with button styling. `{disabled, true}' in
%% `Attrs' removes the href and marks it `aria-disabled'.
-spec link_button(html(), iodata() | undefined, css(), attrs()) -> element().
link_button(Content, Href, Css, Attrs0) ->
    E = entry(link_button),
    {Disabled, Attrs} = take_flag(<<"disabled">>, Attrs0),
    State = case Disabled of
                true  -> [{aria_disabled, <<"true">>}, {tabindex, <<"-1">>}];
                false -> [{href, Href}]
            end,
    ?H:el(a, Content,
          [aihtml_catalog:classes(E, Css), <<"ah-link-btn">>,
           [<<"ah-btn-disabled">> || Disabled]],
          [[{role, link} | State], Attrs]).

%% @doc A button with two states. `Value' is `true' (pressed) or `false';
%% `data-ah-value' and the button's own `value' are "true" / "false" and
%% a click toggles them and fires `change'. A `name' in `Attrs' goes to a
%% hidden input.
-spec toggle_button(html(), boolean(), css(), attrs()) -> element().
toggle_button(Content, Value, Css, Attrs0) when is_boolean(Value) ->
    E = entry(toggle_button),
    {Opts, Attrs1} = aihtml_catalog:split_options(E, Attrs0),
    {Name, Attrs} = take(<<"name">>, Attrs1),
    Disabled = is_disabled(Attrs),
    V = atom_to_binary(Value, utf8),
    {Body, ImgCls} = with_icon(Content, Opts),
    ?H:el(button, [Body, hidden_input(Name, V)],
          [aihtml_catalog:classes(E, Css), ImgCls,
           [<<"ah-btn-toggled">> || Value], [<<"ah-btn-disabled">> || Disabled]],
          [[{type, button}, {value, V}, {aria_pressed, V},
            {data_ah, <<"toggle-button">>}, {data_ah_value, V}], Attrs]).

with_icon(Content, Opts) ->
    Pos = maps:get(icon_position, Opts, left),
    lists:member(Pos, [left, right, top, bottom])
        orelse error({aihtml, {bad_option, icon_position, Pos}}),
    Icon = case Opts of
               #{img := Src} ->
                   ?H:void(img, [<<"ah-btn-img">>],
                           [{src, Src}, {width, 16}, {height, 16}, {alt, <<>>}]);
               #{icon := I} ->
                   ?H:el(span, I, [<<"ah-btn-img">>], [{aria_hidden, <<"true">>}]);
               #{} -> none
           end,
    case Icon of
        none -> {Content, []};
        _ ->
            Text = ?H:el(span, Content, [<<"ah-btn-text">>], []),
            Body = case Pos of
                       P when P =:= left; P =:= top -> [Icon, Text];
                       _ -> [Text, Icon]
                   end,
            {Body, [<<"ah-btn-img-", (atom_to_binary(Pos, utf8))/binary>>]}
    end.

%%%===================================================================
%%% button_group
%%%===================================================================

%% @doc Joined buttons. In `radio' mode `Value' is the selected item's
%% value; in `checkbox' mode a list of values (or "a,b"). In the default
%% mode `Value' is ignored and each button is a plain button whose own
%% `value' is the item value, so `ItemAttrs' can carry `on(click, ...)'.
-spec button_group([item()], term(), css(), attrs()) -> element().
button_group(Items0, Value, Css, Attrs0) ->
    E = entry(button_group),
    Classes = aihtml_catalog:classes(E, Css),
    Mode = pick(Css, [radio, checkbox], default),
    {Disabled, Attrs1} = take_flag(<<"disabled">>, Attrs0),
    {Name, Attrs} = take(<<"name">>, Attrs1),
    Items = [item(I) || I <- Items0],
    Selected = case Mode of
                   radio -> [bin(Value) || Value =/= undefined];
                   checkbox -> values(Value);
                   default -> []
               end,
    N = length(Items),
    Focus = roving_focus(Mode =:= radio, Items, Selected, Disabled),
    Buttons =
        [begin
             Sel = lists:member(V, Selected),
             Off = Disabled orelse is_disabled(IA),
             ?H:el(button, Label,
                   [<<"ah-btn-group-btn">>,
                    [<<"ah-btn-group-btn-first">> || Idx =:= 1],
                    [<<"ah-btn-group-btn-last">> || Idx =:= N],
                    [<<"ah-btn-group-btn-disabled">> || Off],
                    [<<"ah-btn-group-btn-selected">> || Sel]],
                   [[{type, button}, {value, V}, {data_value, V},
                     {disabled, Off},
                     {tabindex, case Mode of
                                    radio when V =:= Focus -> <<"0">>;
                                    radio -> <<"-1">>;
                                    _ -> undefined
                                end}],
                    case Mode of
                        radio -> [{role, radio}, {aria_checked, atom_to_binary(Sel, utf8)}];
                        checkbox -> [{aria_pressed, atom_to_binary(Sel, utf8)}];
                        default -> []
                    end,
                    IA])
         end || {Idx, {V, Label, IA}} <- lists:zip(lists:seq(1, N), Items)],
    Joined = join(Selected),
    ValueAttrs = case Mode of
                     default -> [];
                     _ -> [{data_ah_value, Joined}]
                 end,
    ?H:el('div',
          [Buttons, [hidden_input(Name, Joined) || Mode =/= default]],
          [Classes, [<<"ah-btn-group-disabled">> || Disabled]],
          [[{role, case Mode of radio -> radiogroup; _ -> group end},
            {aria_disabled, Disabled andalso <<"true">>},
            {data_ah, <<"button-group">>} | ValueAttrs], Attrs]).

%%%===================================================================
%%% segmented_control
%%%===================================================================

%% @doc Mutually exclusive segments (role tablist, as in sigil). `Value'
%% is the selected item's value.
-spec segmented_control([item()], term(), css(), attrs()) -> element().
segmented_control(Items0, Value, Css, Attrs0) ->
    E = entry(segmented_control),
    Classes = aihtml_catalog:classes(E, Css),
    Size = pick(Css, [sm, md, lg], md),
    Full = lists:member(full_width, aihtml_catalog:flags(E, Css)),
    {Disabled, Attrs1} = take_flag(<<"disabled">>, Attrs0),
    {Name, Attrs} = take(<<"name">>, Attrs1),
    Items = [item(I) || I <- Items0],
    Current = case Value of undefined -> undefined; _ -> bin(Value) end,
    Focus = roving_focus(true, Items, [Current], Disabled),
    Buttons =
        [begin
             Active = V =:= Current,
             Off = is_disabled(IA),
             ?H:el(button, Label, [<<"ah-segmented-control__item">>],
                   [[{type, button}, {role, tab},
                     {aria_selected, atom_to_binary(Active, utf8)},
                     {data_value, V},
                     {data_state, case Active of true -> active; false -> inactive end},
                     {data_disabled, atom_to_binary(Off, utf8)},
                     {tabindex, case V =:= Focus of true -> <<"0">>; false -> <<"-1">> end},
                     {disabled, Off orelse Disabled}],
                    IA])
         end || {V, Label, IA} <- Items],
    Cur = case Current of undefined -> <<>>; _ -> Current end,
    ?H:el('div', [Buttons, hidden_input(Name, Cur)], Classes,
          [[{role, tablist},
            {data_size, Size},
            {data_full_width, atom_to_binary(Full, utf8)},
            {data_disabled, atom_to_binary(Disabled, utf8)},
            {aria_disabled, Disabled andalso <<"true">>},
            {data_ah, <<"segmented-control">>},
            {data_ah_value, Cur}], Attrs]).

%%%===================================================================
%%% dropdown_button
%%%===================================================================

%% @doc A button that opens a menu. Choosing an item sets `data-ah-value'
%% on the root and fires `change'. Options: `value' (the initially
%% selected item), `auto_open' (open on hover).
-spec dropdown_button(html(), [item()], css(), attrs()) -> element().
dropdown_button(Content, Items0, Css, Attrs0) ->
    E = entry(dropdown_button),
    Classes = aihtml_catalog:classes(E, Css),
    {Opts, Attrs1} = aihtml_catalog:split_options(E, Attrs0),
    {Disabled, Attrs2} = take_flag(<<"disabled">>, Attrs1),
    {Name, Attrs} = take(<<"name">>, Attrs2),
    Cur = case maps:get(value, Opts, undefined) of
              undefined -> <<>>;
              V0 -> bin(V0)
          end,
    AutoOpen = maps:get(auto_open, Opts, false) =:= true,
    Menu = [case I of
                divider ->
                    ?H:el('div', [], [<<"ah-dropdown-btn-divider">>], [{role, separator}]);
                _ ->
                    {V, Label, IA0} = item(I),
                    {Icon, IA} = take(<<"icon">>, IA0),
                    Sel = V =:= Cur andalso Cur =/= <<>>,
                    ?H:el(button, [menu_icon(<<"ah-dropdown-btn-item-icon">>, Icon),
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
           hidden_input(Name, Cur)],
          [Classes, [<<"ah-dropdown-btn-disabled">> || Disabled],
           [<<"ah-dropdown-btn-auto-open">> || AutoOpen]],
          [[{aria_disabled, Disabled andalso <<"true">>},
            {data_ah, <<"dropdown-button">>}, {data_ah_value, Cur}], Attrs]).

%%%===================================================================
%%% split_button
%%%===================================================================

%% @doc A main action and an arrow that opens a menu. A click on the main
%% half reaches the root (so `on(click, ...)' there is the main action);
%% clicks on the arrow and the menu do not. Choosing an item sets
%% `data-ah-value' and fires `change'. Options: `value', `menu_align'
%% (start | end, default end).
-spec split_button(html(), [item()], css(), attrs()) -> element().
split_button(Content, Items0, Css, Attrs0) ->
    E = entry(split_button),
    Classes = aihtml_catalog:classes(E, Css),
    Variant = pick(Css, [primary, secondary, success, warning, error, info, outlined], primary),
    Size = pick(Css, [sm, md, lg], md),
    {Opts, Attrs1} = aihtml_catalog:split_options(E, Attrs0),
    {Disabled, Attrs2} = take_flag(<<"disabled">>, Attrs1),
    {Name, Attrs} = take(<<"name">>, Attrs2),
    Align = maps:get(menu_align, Opts, 'end'),
    lists:member(Align, [start, 'end'])
        orelse error({aihtml, {bad_option, menu_align, Align}}),
    Cur = case maps:get(value, Opts, undefined) of
              undefined -> <<>>;
              V0 -> bin(V0)
          end,
    BtnCls = [<<"ah-btn">>, <<"ah-btn-", (atom_to_binary(Variant, utf8))/binary>>],
    Menu = [case I of
                divider ->
                    ?H:el('div', [], [<<"ah-split-button__divider">>], [{role, separator}]);
                _ ->
                    {V, Label, IA0} = item(I),
                    {Icon, IA} = take(<<"icon">>, IA0),
                    Off = is_disabled(IA),
                    ?H:el(button, [menu_icon(<<"ah-split-button__item-icon">>, Icon),
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
           hidden_input(Name, Cur)],
          Classes,
          [[{data_variant, Variant}, {data_size, Size},
            {data_disabled, atom_to_binary(Disabled, utf8)},
            {data_menu_align, Align}, {data_open, <<"false">>},
            {data_ah, <<"split-button">>}, {data_ah_value, Cur}], Attrs]).

menu_icon(_Cls, undefined) -> [];
menu_icon(Cls, Icon) -> ?H:el(span, Icon, [Cls], [{aria_hidden, <<"true">>}]).

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    BtnGroups = #{variant => {?BTN_VARIANTS, primary}, size => {[sm, md, lg], md}},
    [#{name => button, category => form,
       signature => <<"button(Content, Value, Css, Attrs)">>,
       root => <<"ah-btn">>, groups => BtnGroups, flags => [round],
       classes => #{md => []}, options => [icon, img, icon_position],
       doc => <<"A native button; variant, size and round are modifiers.">>,
       methods => [],
       option_docs => #{round => <<"Pill-shaped corners.">>,
                        icon => <<"HTML shown beside the text, e.g. a glyph or an SVG.">>,
                        img => <<"URL of a 16px image shown beside the text.">>,
                        icon_position => <<"Where the icon goes: left (default), right, top or bottom.">>}},
     #{name => link_button, category => form,
       signature => <<"link_button(Content, Href, Css, Attrs)">>,
       root => <<"ah-btn">>, groups => BtnGroups, flags => [round],
       classes => #{md => []},
       doc => <<"A link that looks like a button.">>,
       option_docs => #{round => <<"Pill-shaped corners.">>},
       methods => []},
     #{name => toggle_button, category => form,
       signature => <<"toggle_button(Content, Pressed, Css, Attrs)">>,
       root => <<"ah-btn">>, groups => BtnGroups, flags => [round],
       classes => #{md => []}, options => [icon, img, icon_position],
       behavior => <<"toggle-button">>, events => [<<"change">>],
       doc => <<"A button that stays pressed; value \"true\" or \"false\".">>,
       option_docs => #{round => <<"Pill-shaped corners.">>,
                        icon => <<"HTML shown beside the text, e.g. a glyph or an SVG.">>,
                        img => <<"URL of a 16px image shown beside the text.">>,
                        icon_position => <<"Where the icon goes: left (default), right, top or bottom.">>},
       methods => [#{name => toggle, args => <<"()">>, doc => <<"Flip the state without firing change.">>},
                   #{name => setValue, args => <<"(Pressed)">>, doc => <<"Set the state (true / false) without firing change.">>},
                   #{name => getValue, args => <<"()">>, doc => <<"Return true when pressed.">>}]},
     #{name => button_group, category => form,
       signature => <<"button_group(Items, Value, Css, Attrs)">>,
       root => <<"ah-btn-group">>,
       groups => #{mode => {[default, radio, checkbox], default},
                   orientation => {[horizontal, vertical], horizontal},
                   shape => {[rounded, square], rounded},
                   fill => {[filled, outlined], none}},
       classes => #{default => [], square => []},
       behavior => <<"button-group">>, events => [<<"change">>],
       doc => <<"Joined buttons; radio and checkbox modes keep a selection.">>,
       methods => [#{name => setValue, args => <<"(Value)">>, doc => <<"Select a value, or in checkbox mode a list or \"a,b\", without firing change.">>},
                   #{name => getValue, args => <<"()">>, doc => <<"Return data-ah-value (comma separated in checkbox mode).">>},
                   #{name => clear, args => <<"()">>, doc => <<"Clear the selection without firing change.">>}]},
     #{name => segmented_control, category => form,
       signature => <<"segmented_control(Items, Value, Css, Attrs)">>,
       root => <<"ah-segmented-control">>,
       groups => #{size => {[sm, md, lg], md}}, flags => [full_width],
       classes => #{sm => [], md => [], lg => [], full_width => []},
       behavior => <<"segmented-control">>, events => [<<"change">>],
       doc => <<"A row of mutually exclusive segments.">>,
       option_docs => #{full_width => <<"Stretch to the container width with equal segments.">>},
       methods => [#{name => setValue, args => <<"(Value)">>, doc => <<"Select a segment without firing change.">>},
                   #{name => getValue, args => <<"()">>, doc => <<"Return the selected value.">>}]},
     #{name => dropdown_button, category => form,
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
                   #{name => getValue, args => <<"()">>, doc => <<"Return the last chosen value.">>}]},
     #{name => split_button, category => form,
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

%%%===================================================================
%%% Internal
%%%===================================================================

entry(Name) -> aihtml_catalog:entry(?MODULE, Name).

%% The first of Choices present in Css (already validated by classes/2).
pick(Css, Choices, Default) ->
    case [M || M <- flatten(Css), lists:member(M, Choices)] of
        [M | _] -> M;
        [] -> Default
    end.

flatten(L) when is_list(L) ->
    case L =/= [] andalso io_lib:printable_unicode_list(L) of
        true -> [L];
        false -> lists:flatmap(fun flatten/1, L)
    end;
flatten(X) -> [X].

item({V, Label, IA}) -> {bin(V), Label, IA};
item({V, Label}) -> {bin(V), Label, []};
item(divider) -> error({aihtml, {divider_not_allowed_here}});
item(Label) -> {bin(Label), Label, []}.

%% The value that gets keyboard focus in a roving-tabindex group.
roving_focus(false, _, _, _) -> undefined;
roving_focus(true, Items, Selected, Disabled) ->
    Enabled = [V || {V, _, IA} <- Items, not Disabled, not is_disabled(IA)],
    case [V || V <- Enabled, lists:member(V, Selected)] of
        [V | _] -> V;
        [] -> case Enabled of [V | _] -> V; [] -> undefined end
    end.

%% Normalised attributes, with Key taken out.
take(Key, Attrs) ->
    N = ?H:attrs(Attrs),
    case lists:keytake(Key, 1, N) of
        {value, {_, V}, Rest} -> {V, Rest};
        false -> {undefined, N}
    end.

take_flag(Key, Attrs) ->
    {V, Rest} = take(Key, Attrs),
    {V =/= undefined, Rest}.

is_disabled(Attrs) ->
    lists:keymember(<<"disabled">>, 1, ?H:attrs(Attrs)).

hidden_input(undefined, _) -> [];
hidden_input(Name, Value) ->
    ?H:void(input, [], [{type, hidden}, {name, Name}, {value, Value},
                        {data_ah_input, true}]).

value_attr(undefined) -> undefined;
value_attr(V) -> bin(V).

values(undefined) -> [];
values(B) when is_binary(B) -> binary:split(B, <<",">>, [global, trim_all]);
values(L) when is_list(L) ->
    case io_lib:printable_unicode_list(L) of
        true -> values(bin(L));
        false -> [bin(X) || X <- L]
    end;
values(X) -> [bin(X)].

join(Vs) -> iolist_to_binary(lists:join(<<",">>, Vs)).

bin(B) when is_binary(B) -> B;
bin(A) when is_atom(A) -> atom_to_binary(A, utf8);
bin(I) when is_integer(I) -> integer_to_binary(I);
bin(F) when is_float(F) -> float_to_binary(F, [short]);
bin(L) when is_list(L) ->
    case unicode:characters_to_binary(L) of
        B when is_binary(B) -> B;
        _ -> error({aihtml, {bad_value, L}})
    end;
bin(Other) -> error({aihtml, {bad_value, Other}}).
