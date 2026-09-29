%%%-------------------------------------------------------------------
%%% @doc The chip component (designs/04-components.md): `chip/3'
%%% builds an #ah_chip{} element record (include/aihtml_chip.hrl)
%%% and render/1 turns it into HTML, so pages may also write the record
%%% directly (designs/05-records.md).
%%%
%%% The DOM and class names follow sigil so the ported stylesheets in
%%% priv/css/sigil/components apply unchanged.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_chip).
-behaviour(aihtml_element).

-include("aihtml_chip.hrl").

-export([chip/3, render/1, fields/1, catalog/0]).

-import(aihtml_lib_display, [blank/1, tf/1, none_for/1, method/3]).

-define(H, aihtml_html).
-define(E, aihtml_element).
-define(COLORS, aihtml_lib_color:theme_colors()).

-type html() :: aihtml_html:html().
-type css() :: aihtml_html:css().
-type attrs() :: aihtml_html:attrs().

%%%===================================================================
%%% Builder
%%%===================================================================

%% @doc Chip: a compact label with optional avatar, icon and remove
%% button. `removable' chips fire `ah:remove' and then `change' before
%% they remove themselves; `clickable' chips are keyboard focusable.
%% Options: `avatar' (initials), `icon' (html), `value' (data-ah-value,
%% defaults to a binary `Content').
-spec chip(html(), css(), attrs()) -> #ah_chip{}.
chip(Content, Css, Attrs) ->
    ?E:build(?MODULE, #ah_chip{body = Content}, Css, Attrs).

%% @doc The field names of #ah_chip{}.
-spec fields(atom()) -> [atom()].
fields(ah_chip) -> record_info(fields, ah_chip).

%%%===================================================================
%%% Rendering
%%%===================================================================

-spec render(#ah_chip{}) -> html().
render(#ah_chip{body = Content, removable = Removable, clickable = Clickable,
                disabled = Disabled, avatar = Avatar, icon = Icon} = R) ->
    Cls = ?E:classes(?MODULE, R),
    Value = case R#ah_chip.value of
                undefined when is_binary(Content) -> Content;
                V -> V
            end,
    Children = [[?H:el(span, Avatar, [<<"ah-chip__avatar">>], []) || not blank(Avatar)],
                [?H:el(span, Icon, [<<"ah-chip__icon">>], []) || not blank(Icon)],
                ?H:el(span, Content, [<<"ah-chip__label">>], []),
                [?H:el(button, <<"×"/utf8>>, [<<"ah-chip__delete">>],
                       [{type, button}, {aria_label, <<"Remove">>}, {tabindex, -1}])
                 || Removable]],
    Focusable = (Clickable orelse Removable) andalso not Disabled,
    Event = case Removable andalso not Clickable of
                true -> change;
                false -> click
            end,
    ?H:el(span, Children, Cls,
          [[{data_variant, R#ah_chip.variant}, {data_color, R#ah_chip.color},
            {data_size, R#ah_chip.size}, {data_disabled, tf(Disabled)},
            {data_clickable, tf(Clickable)}, {data_ah, <<"chip">>},
            {data_ah_value, Value},
            {role, Clickable andalso <<"button">>},
            {tabindex, Focusable andalso 0},
            {aria_disabled, Disabled andalso <<"true">>}],
           ?E:root_attrs(R, Event)]).

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [maps:merge(entry(), api())].

entry() ->
    #{name => chip, category => media, root => <<"ah-chip">>,
      signature => <<"chip(Content, Css, Attrs)">>,
      groups => #{variant => {[filled, outlined, soft], filled},
                  color => {[default | ?COLORS], default},
                  size => {[small, medium], medium}},
      flags => [removable, clickable, disabled],
      classes => none_for([filled, outlined, soft, default, small, medium,
                           removable, clickable, disabled | ?COLORS]),
      options => [avatar, icon, value], behavior => <<"chip">>,
      events => [<<"ah:remove">>, <<"change">>, <<"click">>],
      doc => <<"Compact label; removable chips fire ah:remove and change, then "
               "remove themselves.">>}.

%% The API tab of the docs page: options, flags and client methods.
api() ->
    #{option_docs => #{avatar => <<"Initials shown in a small circle before the label.">>,
                       icon => <<"HTML (e.g. an SVG) shown before the label.">>,
                       value => <<"data-ah-value carried by change; defaults to a binary Content.">>,
                       removable => <<"Show a remove button (also Backspace / Delete): fires ah:remove, "
                                      "then change, then removes the chip unless ah:remove was cancelled.">>,
                       clickable => <<"Pointer cursor, role=button, focusable, Enter / Space click.">>,
                       disabled => <<"Dimmed and inert.">>},
      methods => [method(remove, <<"()">>, <<"Remove the chip as if its remove button were clicked.">>)]}.
