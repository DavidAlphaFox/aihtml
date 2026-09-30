%%%-------------------------------------------------------------------
%%% @doc Tabbed panels (sigil's tabs). The value, the active key, is in
%%% `data-ah-value' on the root and a user switch fires `change' there.
%%%
%%% ah_tabs/4 builds an element record (#ah_tabs{}, defined in
%%% include/aihtml_tabs.hrl) and render/1 turns it into HTML, so pages
%%% may also write the record directly (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_tabs).
-behaviour(aihtml_element).

-include("aihtml_tabs.hrl").

-export([ah_tabs/4, render/1, fields/1, catalog/0]).
-export_type([tab/0]).

%% {Key, Label, Panel} | {Key, Label, Panel, #{disabled => true}}
-type tab() :: {key(), aihtml_html:html(), aihtml_html:html()}
             | {key(), aihtml_html:html(), aihtml_html:html(), map()}.

-import(aihtml_html, [el/4]).
-import(aihtml_lib_layout, [with_id/2, bool/2, one_of/3, hidden/2, tf/1, bin/1, active_key/2]).

-define(E, aihtml_element).

-type html() :: aihtml_html:html().
-type css() :: aihtml_html:css().
-type attrs() :: aihtml_html:attrs().
-type key() :: aihtml_lib_layout:key().

%% @doc Tabbed panels. `Tabs' is `[{Key, Label, Panel}]' or
%% `[{Key, Label, Panel, #{disabled => true}}]'; every panel is rendered,
%% the client switches between them. `Active' is a key (undefined: the
%% first enabled tab).
%% Options: animation (fade | none), selection_mode (click | hover),
%% scrollable, name. Value: the active key.
-spec ah_tabs([{key(), html(), html()} | {key(), html(), html(), map()}],
              key() | undefined, css(), attrs()) -> #ah_tabs{}.
ah_tabs(Tabs, Active, Css, Attrs) ->
    aihtml_element:build(?MODULE, #ah_tabs{items = Tabs, value = Active}, Css, Attrs).

%% @doc The field names of #ah_tabs{}.
-spec fields(ah_tabs) -> [atom()].
fields(ah_tabs) -> record_info(fields, ah_tabs).

%%%===================================================================
%%% Rendering
%%%===================================================================

-spec render(#ah_tabs{}) -> html().
render(#ah_tabs{items = Tabs, value = Active, position = Position} = R0) ->
    Classes = aihtml_element:classes(?MODULE, R0),
    {Id, R} = with_id(R0, <<"ah-tabs">>),
    one_of(animation, R#ah_tabs.animation, [undefined, fade, none]),
    one_of(selection_mode, R#ah_tabs.selection_mode, [undefined, click, hover]),
    Norm = [norm_tab(T) || T <- Tabs],
    ActiveKey = active_key(Active, [{K, D} || {K, _, _, D} <- Norm]),
    Vertical = lists:member(Position, [left, right]),
    Indexed = lists:zip(lists:seq(0, length(Norm) - 1), Norm),
    TabId = fun(I) -> <<Id/binary, "-tab-", (integer_to_binary(I))/binary>> end,
    PanelId = fun(I) -> <<Id/binary, "-panel-", (integer_to_binary(I))/binary>> end,
    Items = [el(li, Label,
                [<<"ah-tabs-item">>, [<<"ah-tabs-item-selected">> || K =:= ActiveKey],
                 [<<"ah-tabs-item-disabled">> || Dis]],
                [{id, TabId(I)}, {role, tab}, {data_key, K},
                 {tabindex, case K =:= ActiveKey of true -> 0; false -> -1 end},
                 {aria_selected, tf(K =:= ActiveKey)}, {aria_controls, PanelId(I)},
                 {aria_disabled, Dis andalso <<"true">>}])
             || {I, {K, Label, _, Dis}} <- Indexed],
    Scrollable = bool(scrollable, R#ah_tabs.scrollable),
    Scroll = fun(Side, Glyph) ->
                     [el(li, Glyph, [<<"ah-tabs-scroll-btn ah-tabs-scroll-", Side/binary>>],
                         [{role, presentation}, {aria_hidden, <<"true">>}]) || Scrollable]
             end,
    Header = el(ul, [Scroll(<<"left">>, <<"\x{25C0}"/utf8>>), Items,
                     Scroll(<<"right">>, <<"\x{25B6}"/utf8>>)],
                [<<"ah-tabs-header">>],
                [{role, tablist},
                 {aria_orientation, case Vertical of true -> vertical; false -> horizontal end}]),
    Panels = [el('div', Panel,
                 [<<"ah-tabs-panel">>, [<<"ah-tabs-panel-active">> || K =:= ActiveKey]],
                 [{id, PanelId(I)}, {role, tabpanel}, {tabindex, 0},
                  {aria_labelledby, TabId(I)}, {aria_hidden, tf(K =/= ActiveKey)},
                  {style, K =/= ActiveKey andalso <<"display:none">>}])
              || {I, {K, _, Panel, _}} <- Indexed],
    el('div', [Header, el('div', Panels, [<<"ah-tabs-content">>], []),
               hidden(R#ah_tabs.name, ActiveKey)],
       [Classes, [<<"ah-tabs-scrollable">> || Scrollable]],
       [[{id, Id}, {data_ah, <<"tabs">>}, {data_ah_value, ActiveKey},
         {data_animation, R#ah_tabs.animation},
         {data_selection_mode, R#ah_tabs.selection_mode}], ?E:root_attrs(R, change)]).

norm_tab({K, L, P}) -> {bin(K), L, P, false};
norm_tab({K, L, P, M}) when is_map(M) -> {bin(K), L, P, maps:get(disabled, M, false) =:= true}.

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => tabs, category => layout,
       option_docs => #{animation => <<"fade (default) or none when switching panels.">>,
                        selection_mode => <<"click (default) or hover.">>,
                        scrollable => <<"Scroll buttons for a header wider than the tabs.">>,
                        name => <<"Submit the active key as a hidden input.">>,
                        top => <<"Tabs above the panels (default).">>,
                        bottom => <<"Tabs below the panels.">>,
                        left => <<"Tabs on the left, arrow keys up and down.">>,
                        right => <<"Tabs on the right.">>,
                        disabled => <<"Ignore clicks.">>},
       methods => [#{name => select, args => <<"(Key)">>, doc => <<"Show a tab without firing change.">>},
                   #{name => enable, args => <<"(Key)">>, doc => <<"Enable a tab.">>},
                   #{name => disable, args => <<"(Key)">>, doc => <<"Disable a tab.">>}],
       signature => <<"ah_tabs(Tabs, Active, Css, Attrs)">>, root => <<"ah-tabs">>,
       groups => #{position => {[top, bottom, left, right], top}},
       flags => [disabled],
       options => [animation, selection_mode, scrollable, name],
       behavior => <<"tabs">>, events => [<<"change">>],
       doc => <<"Tabbed panels, all rendered by the server; value is the active key. "
                "Methods: select(key), enable(key), disable(key).">>}].
