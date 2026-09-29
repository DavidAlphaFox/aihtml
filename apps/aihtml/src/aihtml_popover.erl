%%%-------------------------------------------------------------------
%%% @doc A popover, ported from sigil's overlay/popover: a bubble anchored
%%% to the element that opened it (`toggles(Id)' on a button) or to the
%%% `anchor' selector.
%%%
%%% Everything renders server-side, hidden; the behaviour opens and closes
%%% it (see aihtml_lib_overlay for the ways to drive an overlay: opens/1,
%%% toggles/1, closes/0,1,2 in Attrs, aihtml_lib_overlay:open/2 and
%%% close/2 in an action, AH.invoke in the browser). Opening and closing
%%% fire the DOM events `ah:open' and `ah:close' on the component root;
%%% `ah:close' carries `{result}' (the `closes/2' result, or null).
%%%
%%% Literal (binary) classes in `Css' go on the popover. A record's
%%% postback fires on `ah:close'. Behaviour:
%%% assets/js/components/popover.ts. popover/3 builds an #ah_popover{}
%%% (include/aihtml_popover.hrl) and render/1 turns it into HTML
%%% (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_popover).
-behaviour(aihtml_element).

-include("aihtml_popover.hrl").

-export([popover/3, render/1, fields/1, catalog/0]).

-define(H, aihtml_html).
-define(E, aihtml_element).
-define(L, aihtml_lib_overlay).

%%%===================================================================
%%% Builder
%%%===================================================================

%% @doc A bubble anchored to the element that opened it (`toggles(Id)' on
%% a button) or to the `anchor' selector. Css: position `top | bottom |
%% left | right' (default bottom, flips when there is no room), flag
%% `no_arrow'. Options: `title', `closable' (close button in the title
%% bar), `anchor', `modal' (scrim, outside clicks do not close),
%% `auto_close' (close on outside click, default true), `width'.
-spec popover(aihtml_html:html(), aihtml_html:css(), aihtml_html:attrs()) -> #ah_popover{}.
popover(Children, Css, Attrs) ->
    ?E:build(?MODULE, #ah_popover{body = Children}, Css, Attrs).

%% @doc The field names of #ah_popover{}.
-spec fields(atom()) -> [atom()].
fields(ah_popover) -> record_info(fields, ah_popover).

%%%===================================================================
%%% Rendering
%%%===================================================================

-spec render(#ah_popover{}) -> aihtml_html:html().
render(#ah_popover{body = Children, position = Pos, title = Title, width = Width} = P) ->
    Closable = ?L:bool(closable, P#ah_popover.closable),
    TitleBar = case Title of
                   undefined -> [];
                   _ ->
                       ?H:el('div',
                           [Title,
                            [?H:el('div', [], [<<"ah-popover-close-btn">>],
                                   [{role, button}, {tabindex, 0},
                                    {title, <<"Close">>}, {aria_label, <<"Close">>},
                                    {data_ah_close, <<>>}]) || Closable]],
                           [<<"ah-popover-title">>], [])
               end,
    Opts = #{anchor => P#ah_popover.anchor,
             modal => ?L:opt_bool(modal, P#ah_popover.modal),
             auto_close => ?L:opt_bool(auto_close, P#ah_popover.auto_close)},
    ?H:el('div',
        [?H:el('div', [], [<<"ah-popover-arrow">>], [{aria_hidden, <<"true">>}]),
         TitleBar,
         ?H:el('div', Children, [<<"ah-popover-content">>], [])],
        ?E:classes(?MODULE, P),
        [[{data_ah, popover}, {data_state, closed}, {role, dialog},
          {data_ah_position, Pos},
          {data_ah_anchor, ?L:opt_bin(anchor, Opts)},
          {data_ah_modal, ?L:opt_bin(modal, Opts)},
          {data_ah_auto_close, ?L:opt_bin(auto_close, Opts)},
          {aria_label, case Title of T when is_binary(T) -> T; _ -> undefined end},
          {style, ?L:width_style(Width)}],
         ?E:root_attrs(P, 'ah:close')]).

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    Sides = [top, bottom, left, right],
    Events = [<<"ah:open">>, <<"ah:close">>],
    [#{name => popover, category => overlay,
       signature => <<"popover(Children, Css, Attrs)">>,
       root => <<"ah-popover">>,
       groups => #{position => {Sides, bottom}},
       flags => [no_arrow],
       classes => #{no_arrow => [<<"ah-popover-no-arrow">>]},
       options => [title, closable, anchor, modal, auto_close, width],
       option_docs => #{top => <<"Above the anchor.">>, bottom => <<"Below the anchor (default).">>,
                       left => <<"Left of the anchor.">>, right => <<"Right of the anchor.">>,
                       no_arrow => <<"No arrow.">>,
                       title => <<"Title bar text.">>,
                       closable => <<"Close button in the title bar (needs title).">>,
                       anchor => <<"Selector of the anchor, which then toggles it on click; otherwise the element that opened it.">>,
                       modal => <<"Scrim behind it; outside clicks do not close it.">>,
                       auto_close => <<"Close on a click outside (true).">>,
                       width => <<"px or CSS length.">>},
       methods => [#{name => open, args => <<"()">>, doc => <<"Open it.">>},
                   #{name => close, args => <<"(Result)">>, doc => <<"Close it; Result (optional) is reported in ah:close.">>},
                   #{name => toggle, args => <<"()">>, doc => <<"Open or close it.">>},
                   #{name => isOpen, args => <<"()">>, doc => <<"True while open.">>}],
       behavior => <<"popover">>, events => Events,
       doc => <<"Bubble anchored to the element that opened it (toggles/1) or to "
                "the anchor selector; closes on outside click and Escape.">>}].
