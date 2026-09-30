%%%-------------------------------------------------------------------
%%% @doc A container with an optional media strip, header (title, subtitle,
%%% extra), body and footer, ported from sigil's card.
%%%
%%% ah_card/3 builds an element record (#ah_card{}, defined in
%%% include/aihtml_card.hrl) and render/1 turns it into HTML, so pages
%%% may also write the record directly (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_card).
-behaviour(aihtml_element).

-include("aihtml_card.hrl").

-export([ah_card/3, render/1, fields/1, catalog/0]).

-import(aihtml_html, [el/4]).
-import(aihtml_lib_layout, [maybe_el/3]).

-define(E, aihtml_element).

-type html() :: aihtml_html:html().
-type css() :: aihtml_html:css().
-type attrs() :: aihtml_html:attrs().

%% @doc A container with an optional media strip, header (title, subtitle,
%% extra), body (`Children') and footer.
%% Options: title, subtitle, extra, header, media, footer.
-spec ah_card(html(), css(), attrs()) -> #ah_card{}.
ah_card(Children, Css, Attrs) ->
    aihtml_element:build(?MODULE, #ah_card{body = Children}, Css, Attrs).

%% @doc The field names of #ah_card{}.
-spec fields(ah_card) -> [atom()].
fields(ah_card) -> record_info(fields, ah_card).

%%%===================================================================
%%% Rendering
%%%===================================================================

-spec render(#ah_card{}) -> html().
render(#ah_card{body = Children, header = H, title = T, subtitle = S, extra = X} = R) ->
    Header = case {H, T, S, X} of
                 {undefined, undefined, undefined, undefined} -> [];
                 {undefined, _, _, _} ->
                     el('div', [maybe_el(h3, T, <<"ah-card-title">>),
                                maybe_el(p, S, <<"ah-card-subtitle">>),
                                maybe_el('div', X, <<"ah-card-extra">>)],
                        [<<"ah-card-header">>], []);
                 _ -> el('div', H, [<<"ah-card-header">>], [])
             end,
    el('div', [maybe_el('div', R#ah_card.media, <<"ah-card-media">>),
               Header,
               el('div', Children, [<<"ah-card-body">>], []),
               maybe_el('div', R#ah_card.footer, <<"ah-card-footer">>)],
       aihtml_element:classes(?MODULE, R), ?E:root_attrs(R, none)).

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => card, category => layout,
       option_docs => #{title => <<"Header title.">>, subtitle => <<"Muted line under the title.">>,
                        extra => <<"Html on the right of the header (actions).">>,
                        header => <<"Html replacing the whole header.">>,
                        media => <<"Html shown full-bleed above the header (image, banner).">>,
                        footer => <<"Html of the footer strip.">>,
                        hover => <<"Lift and deepen the shadow on hover.">>,
                        flush => <<"No padding around the body.">>},
       methods => [],
       signature => <<"ah_card(Children, Css, Attrs)">>, root => <<"ah-card">>,
       flags => [hover, flush],
       options => [title, subtitle, extra, header, media, footer],
       doc => <<"A container with optional media, header (title, subtitle, extra) and footer.">>}].
