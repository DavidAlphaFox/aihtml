%%%-------------------------------------------------------------------
%%% @doc A native button, ported from sigil's form components (DOM and
%%% classes as sigil renders them, so the styles in priv/css/sigil apply
%%% unchanged).
%%%
%%% ah_button/4 builds an #ah_button{} (include/aihtml_button.hrl) and
%%% render/1 turns it into HTML, so pages may also write the record
%%% directly (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_button).
-behaviour(aihtml_element).

-include("aihtml_button.hrl").

-export([ah_button/4, render/1, fields/1, catalog/0]).

-export_type([variant/0, size/0, icon_position/0]).

-define(H, aihtml_html).
-define(E, aihtml_element).
-define(L, aihtml_lib_button).

-type variant() :: primary | secondary | outlined | success | warning | error
                 | info | default | borderless.
-type size() :: sm | md | lg.
-type icon_position() :: left | right | top | bottom.

%% @doc A native `<button type="button">'. `Value' becomes its `value'
%% attribute (`undefined' leaves it out). Options: `icon' (HTML shown
%% beside the text), `img' (an image URL, 16px), `icon_position' (left |
%% right | top | bottom).
-spec ah_button(aihtml_html:html(), term(), aihtml_html:css(), aihtml_html:attrs()) ->
          #ah_button{}.
ah_button(Content, Value, Css, Attrs) ->
    ?E:build(?MODULE, #ah_button{body = Content, value = Value}, Css, Attrs).

%% @doc The field names of #ah_button{}.
-spec fields(atom()) -> [atom()].
fields(ah_button) -> record_info(fields, ah_button).

-spec render(#ah_button{}) -> aihtml_html:html().
render(#ah_button{body = Content, value = Value, disabled = Disabled} = B) ->
    {Body, ImgCls} = ?L:with_icon(Content, B#ah_button.icon, B#ah_button.img,
                                  B#ah_button.icon_position),
    ?H:el(button, Body,
          [?E:classes(?MODULE, B), ImgCls, [<<"ah-btn-disabled">> || Disabled]],
          [[{type, button}, {value, ?L:value_attr(Value)}, {disabled, Disabled}],
           ?E:root_attrs(B, click)]).

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => button, category => form,
       signature => <<"ah_button(Content, Value, Css, Attrs)">>,
       root => <<"ah-btn">>, groups => ?L:btn_groups(), flags => [round],
       classes => #{md => []}, options => [icon, img, icon_position],
       doc => <<"A native button; variant, size and round are modifiers.">>,
       methods => [],
       option_docs => ?L:icon_option_docs()}].
