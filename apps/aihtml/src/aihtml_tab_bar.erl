%%%-------------------------------------------------------------------
%%% @doc An editor-style strip of closable tabs. The value, the active id, is
%%% in `data-ah-value' on the root and a user switch fires `change' there.
%%%
%%% ah_tab_bar/4 builds an element record (#ah_tab_bar{}, defined in
%%% include/aihtml_tab_bar.hrl) and render/1 turns it into HTML, so pages
%%% may also write the record directly (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_tab_bar).
-behaviour(aihtml_element).

-include("aihtml_tab_bar.hrl").

-export([ah_tab_bar/4, render/1, fields/1, catalog/0]).
-export_type([tab/0]).

%% {Id, Title} | {Id, Title, #{dirty => true, icon => Html}}
-type tab() :: {aihtml_lib_layout:key(), aihtml_html:html()}
             | {aihtml_lib_layout:key(), aihtml_html:html(), map()}.

-import(aihtml_html, [el/4]).
-import(aihtml_lib_layout, [maybe_el/3, bool/2, hidden/2, tf/1, bin/1, text_of/1, active_key/2]).

-define(E, aihtml_element).

-type html() :: aihtml_html:html().
-type css() :: aihtml_html:css().
-type attrs() :: aihtml_html:attrs().
-type key() :: aihtml_lib_layout:key().

%% @doc An editor-style strip of closable tabs (no panels). `Items' is
%% `[{Id, Title}]' or `[{Id, Title, #{dirty => true, icon => Html}}]'.
%% Options: closable (default true), close_label, name.
%% Value: the active id. Closing a tab removes it (and fires `change' when
%% the active tab moves); the root also gets `ah:close' with the id.
-spec ah_tab_bar([{key(), html()} | {key(), html(), map()}], key() | undefined,
                 css(), attrs()) -> #ah_tab_bar{}.
ah_tab_bar(Items, Active, Css, Attrs) ->
    aihtml_element:build(?MODULE, #ah_tab_bar{items = Items, value = Active}, Css, Attrs).

%% @doc The field names of #ah_tab_bar{}.
-spec fields(ah_tab_bar) -> [atom()].
fields(ah_tab_bar) -> record_info(fields, ah_tab_bar).

%%%===================================================================
%%% Rendering
%%%===================================================================

-spec render(#ah_tab_bar{}) -> html().
render(#ah_tab_bar{items = Items, value = Active} = R) ->
    Norm = [case I of
                {K, T} -> {bin(K), T, #{}};
                {K, T, M} when is_map(M) -> {bin(K), T, M}
            end || I <- Items],
    ActiveKey = active_key(Active, [{K, false} || {K, _, _} <- Norm]),
    Closable = bool(closable, R#ah_tab_bar.closable),
    CloseLabel = R#ah_tab_bar.close_label,
    Tabs = [begin
                Act = K =:= ActiveKey,
                Dirty = maps:get(dirty, M, false) =:= true,
                el('div',
                   [maybe_el(span, maps:get(icon, M, undefined), <<"ah-tab-bar__icon">>),
                    el(span, T, [<<"ah-tab-bar__label">>], []),
                    [el(span, [], [<<"ah-tab-bar__dot">>], [{aria_hidden, <<"true">>}]) || Dirty],
                    [el(button, {safe, close_icon()}, [<<"ah-tab-bar__close">>],
                        [{type, button}, {tabindex, -1},
                         {aria_label, [CloseLabel, <<" ">>, text_of(T)]}]) || Closable]],
                   [<<"ah-tab-bar__tab">>],
                   [{role, tab}, {data_id, K}, {data_active, tf(Act)},
                    {data_dirty, tf(Dirty)}, {aria_selected, tf(Act)},
                    {tabindex, case Act of true -> 0; false -> -1 end},
                    {title, text_of(T)}])
            end || {K, T, M} <- Norm],
    el('div', [Tabs, hidden(R#ah_tab_bar.name, ActiveKey)], aihtml_element:classes(?MODULE, R),
       [[{role, tablist}, {data_ah, <<"tab-bar">>}, {data_ah_value, ActiveKey}],
        ?E:root_attrs(R, change)]).

close_icon() ->
    <<"<svg viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" "
      "stroke-linecap=\"round\" aria-hidden=\"true\"><line x1=\"18\" y1=\"6\" x2=\"6\" y2=\"18\"/>"
      "<line x1=\"6\" y1=\"6\" x2=\"18\" y2=\"18\"/></svg>">>.

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => tab_bar, category => layout,
       option_docs => #{closable => <<"Close buttons on the tabs (default true); Delete closes the focused tab.">>,
                        close_label => <<"Accessible label prefix of the close buttons (default close).">>,
                        name => <<"Submit the active id as a hidden input.">>},
       methods => [#{name => select, args => <<"(Id)">>, doc => <<"Activate a tab without firing change.">>},
                   #{name => close, args => <<"(Id)">>, doc => <<"Remove a tab; the neighbour becomes active if it was.">>}],
       signature => <<"ah_tab_bar(Items, Active, Css, Attrs)">>, root => <<"ah-tab-bar">>,
       options => [closable, close_label, name],
       behavior => <<"tab-bar">>, events => [<<"change">>, <<"ah:close">>],
       doc => <<"An editor-style strip of closable tabs; value is the active id. "
                "Methods: select(id), close(id).">>}].
