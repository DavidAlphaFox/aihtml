%%%-------------------------------------------------------------------
%%% @doc A spinner (sigil's loader), by default an overlay covering its
%%% positioned parent.
%%%
%%% loader/2 builds an element record (#ah_loader{}, defined in
%%% include/aihtml_loader.hrl) and render/1 turns it into HTML, so pages
%%% may also write the record directly (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_loader).
-behaviour(aihtml_element).

-include("aihtml_loader.hrl").

-export([loader/2, render/1, fields/1, catalog/0]).

-import(aihtml_html, [el/4]).
-import(aihtml_lib_layout, [bool/2, tf/1, text_of/1]).

-define(E, aihtml_element).

-type html() :: aihtml_html:html().

-type css() :: aihtml_html:css().
-type attrs() :: aihtml_html:attrs().

%% @doc A spinner. By default an overlay covering its positioned parent
%% (sigil's loader); `inline' puts it in the flow, `center' in a box fixed
%% at the middle of the viewport, `hidden' renders it hidden. Css also
%% picks the text position: bottom (default) | top | left | right.
%% Options: text (default "Loading..."; <<>> for none), modal (a page
%% scrim while shown; Esc hides it).
%% Methods: show([Left, Top]), hide, toggle, text(Text).
-spec loader(css(), attrs()) -> #ah_loader{}.
loader(Css, Attrs) ->
    aihtml_element:build(?MODULE, #ah_loader{}, Css, Attrs).

%% @doc The field names of #ah_loader{}.
-spec fields(ah_loader) -> [atom()].
fields(ah_loader) -> record_info(fields, ah_loader).

%%%===================================================================
%%% Rendering
%%%===================================================================

-spec render(#ah_loader{}) -> html().
render(#ah_loader{text = Text} = R) ->
    Classes = aihtml_element:classes(?MODULE, R),
    el('div', [el('div', [], [<<"ah-loader-icon">>], [{aria_hidden, <<"true">>}]),
               [el('div', Text, [<<"ah-loader-text">>], []) || Text =/= <<>>]],
       Classes,
       [[{role, status}, {aria_live, polite}, {aria_busy, tf(not R#ah_loader.hidden)},
         {aria_label, text_of(Text)}, {data_ah, <<"loader">>},
         {data_modal, bool(modal, R#ah_loader.modal) andalso <<"true">>}],
        ?E:root_attrs(R, none)]).

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => loader, category => layout,
       option_docs => #{text => <<"Text next to the spinner (default Loading...; <<>> for none).">>,
                        modal => <<"Dim the page while shown; Esc hides it.">>,
                        bottom => <<"Text under the spinner (default).">>,
                        top => <<"Text above the spinner.">>,
                        left => <<"Text left of the spinner.">>,
                        right => <<"Text right of the spinner.">>,
                        hidden => <<"Render hidden; show it with the show method.">>,
                        inline => <<"In the flow instead of covering the positioned parent.">>,
                        center => <<"A box fixed in the middle of the viewport.">>,
                        disabled => <<"Dimmed.">>},
       methods => [#{name => show, args => <<"([Left, Top])">>, doc => <<"Show, optionally at a position in px.">>},
                   #{name => hide, args => <<"()">>, doc => <<"Hide.">>},
                   #{name => toggle, args => <<"()">>, doc => <<"Show or hide.">>},
                   #{name => text, args => <<"(Text)">>, doc => <<"Change the text.">>}],
       signature => <<"loader(Css, Attrs)">>, root => <<"ah-loader">>,
       groups => #{text_position => {[bottom, top, left, right], bottom}},
       flags => [hidden, inline, center, disabled],
       classes => #{bottom => [<<"ah-loader-text-bottom">>], top => [<<"ah-loader-text-top">>],
                    left => [<<"ah-loader-text-left">>], right => [<<"ah-loader-text-right">>]},
       options => [text, modal],
       behavior => <<"loader">>,
       doc => <<"A spinner, by default an overlay over its positioned parent. "
                "Methods: show([left, top]), hide, toggle, text(t).">>}].
