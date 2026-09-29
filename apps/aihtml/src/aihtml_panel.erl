%%%-------------------------------------------------------------------
%%% @doc A scrollable content container (sigil's panel), optionally with a
%%% header bar holding a title, actions and a collapse toggle.
%%%
%%% panel/3 builds an element record (#ah_panel{}, defined in
%%% include/aihtml_panel.hrl) and render/1 turns it into HTML, so pages
%%% may also write the record directly (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_panel).
-behaviour(aihtml_element).

-include("aihtml_panel.hrl").

-export([panel/3, render/1, fields/1, catalog/0]).

-import(aihtml_html, [el/4]).
-import(aihtml_lib_layout, [maybe_el/3, maybe_el/4, with_id/2, bool/2, tf/1, len/1, style/1]).

-define(E, aihtml_element).

-type html() :: aihtml_html:html().
-type css() :: aihtml_html:css().
-type attrs() :: aihtml_html:attrs().

%% @doc A scrollable content container (sigil's panel), optionally with a
%% header bar holding a title, actions and a collapse toggle.
%% Options: title, actions, collapsible, collapsed, height, max_height.
-spec panel(html(), css(), attrs()) -> #ah_panel{}.
panel(Children, Css, Attrs) ->
    aihtml_element:build(?MODULE, #ah_panel{body = Children}, Css, Attrs).

%% @doc The field names of #ah_panel{}.
-spec fields(ah_panel) -> [atom()].
fields(ah_panel) -> record_info(fields, ah_panel).

%%%===================================================================
%%% Rendering
%%%===================================================================

-spec render(#ah_panel{}) -> html().
render(#ah_panel{body = Children, title = Title, actions = Actions} = R0) ->
    Classes = aihtml_element:classes(?MODULE, R0),
    {Id, R} = with_id(R0, <<"ah-panel">>),
    Collapsible = bool(collapsible, R#ah_panel.collapsible),
    Collapsed = bool(collapsed, R#ah_panel.collapsed) andalso Collapsible,
    BodyId = <<Id/binary, "-body">>,
    TitleId = <<Id/binary, "-title">>,
    Header = case Title =:= undefined andalso Actions =:= undefined
                 andalso not Collapsible of
                 true -> [];
                 false ->
                     el('div',
                        [maybe_el('div', Title, <<"ah-panel-title">>, [{id, TitleId}]),
                         maybe_el('div', Actions, <<"ah-panel-actions">>),
                         case Collapsible of
                             false -> [];
                             true ->
                                 el(button, {safe, chevron()}, [<<"ah-panel-toggle">>],
                                    [{type, button}, {aria_expanded, tf(not Collapsed)},
                                     {aria_controls, BodyId},
                                     {aria_label, R#ah_panel.toggle_label}])
                         end],
                        [<<"ah-panel-header">>], [])
             end,
    Style = style([{<<"height">>, len(R#ah_panel.height)},
                   {<<"max-height">>, len(R#ah_panel.max_height)},
                   {<<"display">>, Collapsed andalso <<"none">>}]),
    Wrapper = el('div', el('div', Children, [<<"ah-panel-content">>], []),
                 [<<"ah-panel-wrapper">>], [{id, BodyId}, {style, Style}]),
    el('div', [Header, Wrapper],
       [Classes, [<<"ah-panel-has-header">> || Header =/= []],
        [<<"ah-panel-collapsed">> || Collapsed]],
       [[{id, Id}, {data_ah, <<"panel">>},
         {aria_labelledby, Title =/= undefined andalso TitleId},
         {role, Title =/= undefined andalso region}], ?E:root_attrs(R, none)]).

chevron() ->
    <<"<svg viewBox=\"0 0 24 24\" width=\"16\" height=\"16\" fill=\"none\" stroke=\"currentColor\" "
      "stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\" aria-hidden=\"true\">"
      "<polyline points=\"6 9 12 15 18 9\"/></svg>">>.

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => panel, category => layout,
       option_docs => #{title => <<"Header title; the panel becomes a labelled region.">>,
                        actions => <<"Html on the right of the header.">>,
                        collapsible => <<"Show a toggle that collapses the scroll area.">>,
                        collapsed => <<"Start collapsed (with collapsible).">>,
                        height => <<"Height of the scroll area (integer px or CSS length).">>,
                        max_height => <<"Maximum height of the scroll area.">>,
                        toggle_label => <<"Accessible label of the toggle (default Toggle).">>,
                        bordered => <<"Frame the panel with a border and paper background.">>},
       methods => [#{name => scrollTo, args => <<"(X, Y)">>, doc => <<"Scroll the content to X, Y pixels.">>},
                   #{name => collapse, args => <<"()">>, doc => <<"Collapse the scroll area.">>},
                   #{name => expand, args => <<"()">>, doc => <<"Expand the scroll area.">>},
                   #{name => toggle, args => <<"()">>, doc => <<"Collapse or expand.">>}],
       signature => <<"panel(Children, Css, Attrs)">>, root => <<"ah-panel">>,
       flags => [bordered],
       options => [title, actions, collapsible, collapsed, height, max_height, toggle_label],
       behavior => <<"panel">>, events => [<<"ah:collapse">>, <<"ah:expand">>],
       doc => <<"A scrollable content container with an optional header, actions and "
                "collapse toggle. Methods: scrollTo(x, y), collapse, expand, toggle.">>}].
