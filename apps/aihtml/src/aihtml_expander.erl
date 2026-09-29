%%%-------------------------------------------------------------------
%%% @doc A collapsible section (sigil's expander). It is value-bearing: the
%%% value ("true" / "false") is in `data-ah-value' on the root and a user
%%% toggle fires `change' there.
%%%
%%% expander/3 builds an element record (#ah_expander{}, defined in
%%% include/aihtml_expander.hrl) and render/1 turns it into HTML, so pages
%%% may also write the record directly (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_expander).
-behaviour(aihtml_element).

-include("aihtml_expander.hrl").

-export([expander/3, render/1, fields/1, catalog/0]).

-import(aihtml_html, [el/4]).
-import(aihtml_lib_layout, [maybe_el/3, with_id/2, bool/2, one_of/3, hidden/2, tf/1]).

-define(E, aihtml_element).

-type html() :: aihtml_html:html().
-type css() :: aihtml_html:css().
-type attrs() :: aihtml_html:attrs().

%% @doc A collapsible section. `Children' is the content.
%% Options: header (html or #{title, subheader, extra}), actions,
%% expanded (default true), toggle_mode (click | dblclick | none),
%% animation (slide | fade | none), duration (ms), show_arrow,
%% arrow_position (right | left), expand_icon, collapse_icon,
%% accordion (a name: opening one closes the others with that name), name.
%% Value: "true" | "false".
-spec expander(html(), css(), attrs()) -> #ah_expander{}.
expander(Children, Css, Attrs) ->
    aihtml_element:build(?MODULE, #ah_expander{body = Children}, Css, Attrs).

%% @doc The field names of #ah_expander{}.
-spec fields(ah_expander) -> [atom()].
fields(ah_expander) -> record_info(fields, ah_expander).

%%%===================================================================
%%% Rendering
%%%===================================================================

-spec render(#ah_expander{}) -> html().
render(#ah_expander{body = Children, disabled = Disabled, toggle_mode = Mode,
                    expand_icon = ExpIcon, collapse_icon = ColIcon} = R0) ->
    Classes = aihtml_element:classes(?MODULE, R0),
    {Id, R} = with_id(R0, <<"ah-expander">>),
    HId = <<Id/binary, "-header">>,
    CId = <<Id/binary, "-content">>,
    Expanded = bool(expanded, R#ah_expander.expanded),
    one_of(toggle_mode, Mode, [click, dblclick, none]),
    one_of(animation, R#ah_expander.animation, [undefined, slide, fade, none]),
    ArrowPos = one_of(arrow_position, R#ah_expander.arrow_position, [right, left]),
    Dual = ExpIcon =/= undefined andalso ColIcon =/= undefined,
    ArrowCls = [<<"ah-expander-arrow">>,
                [<<"ah-expander-arrow-left">> || ArrowPos =:= left],
                [<<"ah-expander-arrow-expanded">> || Expanded],
                [<<"ah-expander-arrow-dual">> || Dual]],
    Primary = case ExpIcon of undefined -> <<"\x{25BE}"/utf8>>; I -> I end,
    Arrow = case {bool(show_arrow, R#ah_expander.show_arrow), Dual} of
                {false, _} -> [];
                {_, true} ->
                    el(span, [el(span, Primary, [<<"ah-expander-icon ah-expander-icon-expand">>], []),
                              el(span, ColIcon, [<<"ah-expander-icon ah-expander-icon-collapse">>], [])],
                       ArrowCls, [{aria_hidden, <<"true">>}]);
                {_, false} ->
                    el(span, Primary, ArrowCls, [{aria_hidden, <<"true">>}])
            end,
    Text = case R#ah_expander.header of
               #{} = M ->
                   el(span, [el(span, maps:get(title, M, <<>>), [<<"ah-expander-header-title">>], []),
                             maybe_el(span, maps:get(subheader, M, undefined),
                                      <<"ah-expander-header-subheader">>),
                             maybe_el(span, maps:get(extra, M, undefined),
                                      <<"ah-expander-header-extra">>)],
                      [<<"ah-expander-header-text ah-expander-header-text-structured">>], []);
               H -> el(span, H, [<<"ah-expander-header-text">>], [])
           end,
    Header = el('div', [Text, Arrow],
                [<<"ah-expander-header">>,
                 [<<"ah-expander-header-expanded">> || Expanded],
                 [<<"ah-expander-header-no-toggle">> || Mode =:= none]],
                [{id, HId}, {role, button},
                 {tabindex, case Disabled of true -> -1; false -> 0 end},
                 {aria_expanded, tf(Expanded)}, {aria_controls, CId},
                 {aria_disabled, Disabled andalso <<"true">>}]),
    Body = el('div', [el('div', Children, [<<"ah-expander-content">>], []),
                      maybe_el('div', R#ah_expander.actions, <<"ah-expander-actions">>)],
              [<<"ah-expander-body">>],
              [{id, CId}, {role, region}, {aria_labelledby, HId},
               {style, (not Expanded) andalso <<"display:none">>}]),
    Value = tf(Expanded),
    el('div', [Header, Body, hidden(R#ah_expander.name, Value)], Classes,
       [[{id, Id}, {data_ah, <<"expander">>}, {data_ah_value, Value},
         {data_toggle_mode, Mode =/= click andalso Mode},
         {data_animation, R#ah_expander.animation},
         {data_duration, R#ah_expander.duration},
         {data_accordion, R#ah_expander.accordion}], ?E:root_attrs(R, change)]).

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => expander, category => layout,
       option_docs => #{header => <<"Header html, or #{title, subheader, extra} for a structured header.">>,
                        actions => <<"Html of an action strip under the content, collapsing with it.">>,
                        expanded => <<"Initial state (default true).">>,
                        toggle_mode => <<"click (default), dblclick or none.">>,
                        animation => <<"slide (default), fade or none.">>,
                        duration => <<"Animation time in ms (default 250).">>,
                        show_arrow => <<"Show the arrow (default true).">>,
                        arrow_position => <<"right (default) or left.">>,
                        expand_icon => <<"Arrow html; with collapse_icon the two icons swap.">>,
                        collapse_icon => <<"Icon shown when expanded (with expand_icon).">>,
                        accordion => <<"A name: opening one expander closes the others with that name.">>,
                        name => <<"Submit the state as a hidden input.">>,
                        top => <<"Header above the content (default).">>,
                        bottom => <<"Header below the content.">>,
                        square => <<"No rounded corners.">>,
                        no_gutters => <<"No frame or side padding.">>,
                        disabled => <<"Ignore clicks and keys.">>},
       methods => [#{name => open, args => <<"()">>, doc => <<"Expand without firing change.">>},
                   #{name => close, args => <<"()">>, doc => <<"Collapse without firing change.">>},
                   #{name => toggle, args => <<"()">>, doc => <<"Flip the state without firing change.">>}],
       signature => <<"expander(Children, Css, Attrs)">>, root => <<"ah-expander">>,
       groups => #{position => {[top, bottom], top}},
       flags => [square, no_gutters, disabled],
       classes => #{no_gutters => [<<"ah-expander-no-gutters">>]},
       options => [header, actions, expanded, toggle_mode, animation, duration,
                   show_arrow, arrow_position, expand_icon, collapse_icon, accordion, name],
       behavior => <<"expander">>,
       events => [<<"change">>, <<"ah:expanded">>, <<"ah:collapsed">>],
       doc => <<"A collapsible section; value \"true\" or \"false\". "
                "Methods: open, close, toggle.">>}].
