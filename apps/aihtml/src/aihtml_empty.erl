%%%-------------------------------------------------------------------
%%% @doc An empty-state placeholder: icon, title, description and actions.
%%%
%%% empty/3 builds an element record (#ah_empty{}, defined in
%%% include/aihtml_empty.hrl) and render/1 turns it into HTML, so pages
%%% may also write the record directly (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_empty).
-behaviour(aihtml_element).

-include("aihtml_empty.hrl").

-export([empty/3, render/1, fields/1, catalog/0]).

-import(aihtml_html, [el/4]).
-import(aihtml_lib_layout, [maybe_el/3, maybe_el/4]).

-define(E, aihtml_element).

-type html() :: aihtml_html:html().
-type css() :: aihtml_html:css().
-type attrs() :: aihtml_html:attrs().

%% @doc An empty-state placeholder: icon, title, description and
%% `Children' as the action area.
%% Options: icon (html, e.g. {safe, Svg}), title, description.
-spec empty(html(), css(), attrs()) -> #ah_empty{}.
empty(Children, Css, Attrs) ->
    aihtml_element:build(?MODULE, #ah_empty{body = Children}, Css, Attrs).

%% @doc The field names of #ah_empty{}.
-spec fields(ah_empty) -> [atom()].
fields(ah_empty) -> record_info(fields, ah_empty).

%%%===================================================================
%%% Rendering
%%%===================================================================

-spec render(#ah_empty{}) -> html().
render(#ah_empty{body = Children} = R) ->
    el('div', [maybe_el('div', R#ah_empty.icon, <<"ah-empty__icon">>, [{aria_hidden, <<"true">>}]),
               maybe_el('div', R#ah_empty.title, <<"ah-empty__title">>),
               maybe_el('div', R#ah_empty.description, <<"ah-empty__description">>),
               case Children of
                   [] -> [];
                   undefined -> [];
                   _ -> el('div', Children, [<<"ah-empty__content">>], [])
               end],
       aihtml_element:classes(?MODULE, R), ?E:root_attrs(R, none)).

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => empty, category => layout,
       option_docs => #{icon => <<"Icon html (an emoji or {safe, Svg}).">>,
                        title => <<"Title.">>,
                        description => <<"Explanation under the title.">>,
                        compact => <<"Less padding.">>},
       methods => [],
       signature => <<"empty(Children, Css, Attrs)">>, root => <<"ah-empty">>,
       flags => [compact],
       options => [icon, title, description],
       doc => <<"An empty-state placeholder: icon, title, description and actions.">>}].
