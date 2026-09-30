%%%-------------------------------------------------------------------
%%% @doc A pressed / released button (value "true" / "false"), ported from
%%% sigil's form components. It keeps its value in `data-ah-value' on the
%%% root, renders a hidden input when `Attrs' has a `name', and fires
%%% `change' on the root (designs/04-components.md).
%%%
%%% ah_toggle_button/4 builds an #ah_toggle_button{}
%%% (include/aihtml_toggle_button.hrl) and render/1 turns it into HTML, so
%%% pages may also write the record directly (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_toggle_button).
-behaviour(aihtml_element).

-include("aihtml_toggle_button.hrl").

-export([ah_toggle_button/4, render/1, fields/1, catalog/0]).

-define(H, aihtml_html).
-define(E, aihtml_element).
-define(L, aihtml_lib_button).

%% @doc A button with two states. `Value' is `true' (pressed) or `false';
%% `data-ah-value' and the button's own `value' are "true" / "false" and
%% a click toggles them and fires `change'. A `name' in `Attrs' goes to a
%% hidden input.
-spec ah_toggle_button(aihtml_html:html(), boolean(), aihtml_html:css(), aihtml_html:attrs()) ->
          #ah_toggle_button{}.
ah_toggle_button(Content, Value, Css, Attrs) when is_boolean(Value) ->
    ?E:build(?MODULE, #ah_toggle_button{body = Content, value = Value}, Css, Attrs).

%% @doc The field names of #ah_toggle_button{}.
-spec fields(atom()) -> [atom()].
fields(ah_toggle_button) -> record_info(fields, ah_toggle_button).

-spec render(#ah_toggle_button{}) -> aihtml_html:html().
render(#ah_toggle_button{body = Content, value = Value, name = Name,
                         disabled = Disabled} = B) when is_boolean(Value) ->
    V = atom_to_binary(Value, utf8),
    {Body, ImgCls} = ?L:with_icon(Content, B#ah_toggle_button.icon, B#ah_toggle_button.img,
                                  B#ah_toggle_button.icon_position),
    ?H:el(button, [Body, ?L:hidden_input(Name, V)],
          [?E:classes(?MODULE, B), ImgCls,
           [<<"ah-btn-toggled">> || Value], [<<"ah-btn-disabled">> || Disabled]],
          [[{type, button}, {value, V}, {aria_pressed, V},
            {data_ah, <<"toggle-button">>}, {data_ah_value, V}, {disabled, Disabled}],
           ?E:root_attrs(B, change)]).

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => toggle_button, category => form,
       signature => <<"ah_toggle_button(Content, Pressed, Css, Attrs)">>,
       root => <<"ah-btn">>, groups => ?L:btn_groups(), flags => [round],
       classes => #{md => []}, options => [icon, img, icon_position],
       behavior => <<"toggle-button">>, events => [<<"change">>],
       doc => <<"A button that stays pressed; value \"true\" or \"false\".">>,
       option_docs => ?L:icon_option_docs(),
       methods => [#{name => toggle, args => <<"()">>, doc => <<"Flip the state without firing change.">>},
                   #{name => setValue, args => <<"(Pressed)">>, doc => <<"Set the state (true / false) without firing change.">>},
                   #{name => getValue, args => <<"()">>, doc => <<"Return true when pressed.">>}]}].
