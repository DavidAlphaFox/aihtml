%%%-------------------------------------------------------------------
%%% @doc The timeline component (designs/04-components.md): `timeline/3'
%%% builds an #ah_timeline{} element record (include/aihtml_timeline.hrl)
%%% and render/1 turns it into HTML, so pages may also write the record
%%% directly (designs/05-records.md).
%%%
%%% The DOM and class names follow sigil so the ported stylesheets in
%%% priv/css/sigil/components apply unchanged.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_timeline).
-behaviour(aihtml_element).

-include("aihtml_timeline.hrl").

-export([timeline/3, render/1, fields/1, catalog/0]).

-export_type([item/0]).

-import(aihtml_lib_display, [blank/1]).

-define(H, aihtml_html).
-define(E, aihtml_element).

-type html() :: aihtml_html:html().
-type css() :: aihtml_html:css().
-type attrs() :: aihtml_html:attrs().
%% #{date, title, subtitle, icon, description, dot, expanded}
-type item() :: #{atom() => term()}.

%%%===================================================================
%%% Builder
%%%===================================================================

%% @doc Timeline. `Items' are maps with `date', `title', `subtitle',
%% `icon' (html), `description', `dot' (primary | success | warning |
%% danger) and `expanded'. Items with a description expand on click
%% unless the `collapsible' option is false.
-spec timeline([map()], css(), attrs()) -> #ah_timeline{}.
timeline(Items, Css, Attrs) ->
    ?E:build(?MODULE, #ah_timeline{items = Items}, Css, Attrs).

%% @doc The field names of #ah_timeline{}.
-spec fields(atom()) -> [atom()].
fields(ah_timeline) -> record_info(fields, ah_timeline).

%%%===================================================================
%%% Rendering
%%%===================================================================

-spec render(#ah_timeline{}) -> html().
render(#ah_timeline{items = Items, position = Position} = R) ->
    Cls = ?E:classes(?MODULE, R),
    Collapsible = R#ah_timeline.collapsible =/= false,
    Rows = [timeline_row(I, Item, Position, Collapsible)
            || {I, Item} <- lists:enumerate(0, Items)],
    ?H:el('div', ?H:el('div', Rows, [<<"ah-timeline-container">>], []),
          [Cls, [<<"ah-collapsible">> || Collapsible]],
          [[{data_ah, <<"timeline">>}], ?E:root_attrs(R, 'ah:toggle')]).

timeline_row(Idx, Item, Position, Collapsible) ->
    Side = case Position of
               both when Idx rem 2 =:= 0 -> far;
               both -> near;
               S -> S
           end,
    Desc = maps:get(description, Item, undefined),
    CanToggle = Collapsible andalso not blank(Desc),
    Expanded = maps:get(expanded, Item, false) =:= true,
    Icon = maps:get(icon, Item, undefined),
    Sub = maps:get(subtitle, Item, undefined),
    Card = ?H:el('div',
                 [?H:el('div', [], [<<"ah-timeline-item-pointer">>], []),
                  ?H:el('div',
                        [?H:el('div',
                               [[?H:el('div', Icon, [<<"ah-timeline-item-icon">>], [])
                                 || not blank(Icon)],
                                ?H:el('div', [?H:el('div', maps:get(title, Item, <<>>),
                                                    [<<"ah-timeline-item-title">>], []),
                                              [?H:el('div', Sub, [<<"ah-timeline-item-subtitle">>], [])
                                               || not blank(Sub)]], [], [])],
                               [<<"ah-timeline-item-header">>], []),
                         [?H:el('div', Desc, [<<"ah-timeline-item-description">>], [])
                          || not blank(Desc)]],
                        [<<"ah-timeline-item-content">>], [])],
                 [<<"ah-timeline-item">>, [<<"ah-timeline-item-expanded">> || Expanded]],
                 [{<<"ah-collapsible">>, CanToggle}, {role, CanToggle andalso <<"button">>},
                  {tabindex, CanToggle andalso 0},
                  {aria_expanded, CanToggle andalso atom_to_binary(Expanded)}]),
    Date = ?H:el('div', maps:get(date, Item, <<>>), [<<"ah-timeline-date">>], []),
    Dot = case maps:get(dot, Item, undefined) of
              undefined -> [];
              D when D =:= primary; D =:= success; D =:= warning; D =:= danger ->
                  <<"ah-timeline-dot-", (atom_to_binary(D))/binary>>;
              D -> error({aihtml, {bad_dot, D}})
          end,
    [?H:el('div', if Side =:= near -> Card; true -> Date end, [<<"ah-timeline-near-cell">>], []),
     ?H:el('div', ?H:el('div', [], [<<"ah-timeline-dot">>, Dot], []),
           [<<"ah-timeline-track-cell">>], []),
     ?H:el('div', if Side =:= far -> Card; true -> Date end, [<<"ah-timeline-far-cell">>], [])].

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [maps:merge(entry(), api())].

entry() ->
    #{name => timeline, category => data, root => <<"ah-timeline">>,
      signature => <<"timeline(Items, Css, Attrs)">>,
      groups => #{position => {[both, near, far], both}},
      flags => [horizontal, disabled],
      classes => maps:from_list([{M, [<<"ah-timeline-position-", (atom_to_binary(M))/binary>>]}
                                 || M <- [both, near, far]]),
      options => [collapsible], behavior => <<"timeline">>,
      events => [<<"ah:toggle">>],
      doc => <<"Events along an axis, cards alternating (both) or on one side; "
               "cards with a description expand on click.">>}.

%% The API tab of the docs page: options, flags and client methods.
api() ->
    #{option_docs => #{collapsible => <<"Cards with a description expand on click / Enter "
                                        "(default true).">>,
                       horizontal => <<"Lay the axis out horizontally.">>,
                       disabled => <<"Dimmed and inert.">>},
      methods => []}.
