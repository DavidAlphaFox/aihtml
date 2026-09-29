%%%-------------------------------------------------------------------
%%% @doc A button that repeats its click while held, ported from sigil
%%% (form/repeat_button). See designs/04-components.md.
%%%
%%% repeat_button renders the markup of `aihtml_button:button/4' (it
%%% returns an #ah_button{}) and fires `click' on press and then
%%% repeatedly while held, so `on(click, ...)' or a postback runs once per
%%% repetition. The behaviour lives in assets/js/components/repeat_button.js.
%%%
%%% repeat_button/4 builds an #ah_repeat_button{}
%%% (include/aihtml_repeat_button.hrl) and render/1 turns it into HTML, so
%%% pages may also write the record directly (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_repeat_button).
-behaviour(aihtml_element).

-include("aihtml_repeat_button.hrl").
-include("aihtml_button.hrl").

-export([repeat_button/4, render/1, fields/1, catalog/0]).

-define(E, aihtml_element).

%% @doc A button that repeats its click while held, as sigil's
%% repeat-button: pressing it (mouse, touch, Enter or Space) fires `click'
%% at once, then every `interval' ms once `delay' ms have passed, until it
%% is released. The browser's own click on release is swallowed, so each
%% repetition is one `click' (one action with `on(click, ...)'). Renders
%% as `aihtml_button:button/4' with the same modifiers (variant,
%% size, round) and options (icon, img, icon_position).
%% Options: `delay' (ms before repeating, default 300), `interval' (ms
%% between clicks, default 50).
-spec repeat_button(aihtml_html:html(), term(), aihtml_html:css(), aihtml_html:attrs()) ->
          #ah_repeat_button{}.
repeat_button(Content, Value, Css, Attrs) ->
    ?E:build(?MODULE, #ah_repeat_button{body = Content, value = Value}, Css, Attrs).

%% @doc The field names of #ah_repeat_button{}.
-spec fields(atom()) -> [atom()].
fields(ah_repeat_button) -> record_info(fields, ah_repeat_button).

-spec render(#ah_repeat_button{}) -> #ah_button{}.
render(#ah_repeat_button{delay = Delay, interval = Interval} = R) ->
    _ = ?E:classes(?MODULE, R),                  % checks the modifier fields
    (is_integer(Delay) andalso Delay >= 0) orelse error({aihtml, {bad_option, delay, Delay}}),
    (is_integer(Interval) andalso Interval > 0)
        orelse error({aihtml, {bad_option, interval, Interval}}),
    #ah_button{id = R#ah_repeat_button.id, css = R#ah_repeat_button.css,
               attrs = [{data_ah, <<"repeat-button">>},
                        {data_ah_delay, integer_to_binary(Delay)},
                        {data_ah_interval, integer_to_binary(Interval)},
                        R#ah_repeat_button.attrs],
               postback = R#ah_repeat_button.postback,
               delegate = R#ah_repeat_button.delegate,
               body = R#ah_repeat_button.body, value = R#ah_repeat_button.value,
               variant = R#ah_repeat_button.variant, size = R#ah_repeat_button.size,
               round = R#ah_repeat_button.round, disabled = R#ah_repeat_button.disabled,
               icon = R#ah_repeat_button.icon, img = R#ah_repeat_button.img,
               icon_position = R#ah_repeat_button.icon_position}.

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => repeat_button, category => form,
       signature => <<"repeat_button(Content, Value, Css, Attrs)">>,
       root => <<"ah-btn">>,
       groups => #{variant => {[primary, secondary, outlined, success, warning, error,
                                info, default, borderless], primary},
                   size => {[sm, md, lg], md}},
       flags => [round],
       classes => #{md => []},
       options => [icon, img, icon_position, delay, interval],
       behavior => <<"repeat-button">>,
       events => [<<"click">>],
       doc => <<"A button that fires click again and again while it is held, "
                "e.g. to step a value.">>,
       option_docs =>
           #{round => <<"Pill-shaped corners.">>,
             icon => <<"HTML shown beside the text, e.g. a glyph or an SVG.">>,
             img => <<"URL of a 16px image shown beside the text.">>,
             icon_position => <<"Where the icon goes: left (default), right, top or bottom.">>,
             delay => <<"Milliseconds held before the clicks repeat (default 300).">>,
             interval => <<"Milliseconds between repeated clicks (default 50).">>},
       methods =>
           [#{name => stop, args => <<"()">>, doc => <<"Stop repeating (as if released).">>}]}].
