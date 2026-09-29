%%%-------------------------------------------------------------------
%%% @doc Overlay components ported from sigil: tooltip, popover, drawer,
%%% sheet, toast, notification and window (designs/04-components.md).
%%%
%%% Everything renders server-side, hidden; assets/js/components/overlay.js
%%% opens and closes it. Three ways to drive an overlay:
%%%
%%%   declarative   splice `opens(Target)', `toggles(Target)', `closes()'
%%%                 or `closes(Target)' into any element's Attrs; a click on
%%%                 it runs the target's open/close/toggle behaviour method
%%%   server        inside an action, `open(Ctx, Target)', `close(Ctx,
%%%                 Target)', `toast(Ctx, Message, Opts)', `notify(Ctx, Opts)'
%%%   client        `AH.invoke(el, "open")', `AH.invoke(el, "close")'
%%%
%%% `Target' is a CSS selector (binary) or `{id, Id}'.
%%%
%%% Literal (binary) classes in `Css' go on the visible surface: the
%%% tooltip bubble, the drawer/sheet panel, the popover and the window.
%%%
%%% Opening and closing fire the jQuery events `ah:open' and `ah:close' on
%%% the component root; `ah:close' carries `{result}' (the `closes/2'
%%% result, or null). Overlays are not value controls, so no `change'.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_overlay).

%% Shared templates (see aihtml_tpl): also compiled to AH.tpl.* for the
%% browser, so a notification or toast card has one source of markup.
-compile({parse_transform, beamai_mustache_transform}).
-mustache_template({tpl_notification, "../templates/notification.mustache"}).
-mustache_template({tpl_toast, "../templates/toast.mustache"}).

%% Components
-export([tooltip/4, popover/3, drawer/3, sheet/3, notification/3, window/3]).
%% Attribute helpers (client-side triggers)
-export([tooltip_attrs/2, opens/1, closes/0, closes/1, closes/2, toggles/1,
         shows_toast/2]).
%% Server-driven helpers, for actions
-export([open/2, close/2, toggle/2, toast/3, notify/2]).
-export([catalog/0, facade_extras/0]).

-export_type([target/0]).

-type target() :: binary() | string() | {id, iodata() | atom()}.
-type html() :: aihtml_html:html().
-type css() :: aihtml_html:css().
-type attrs() :: aihtml_html:attrs().
-type element() :: aihtml_html:element().

-define(CLOSE_ICON,
        {safe, <<"<svg viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" "
                 "stroke-width=\"2\" stroke-linecap=\"round\" aria-hidden=\"true\">"
                 "<line x1=\"18\" y1=\"6\" x2=\"6\" y2=\"18\"></line>"
                 "<line x1=\"6\" y1=\"6\" x2=\"18\" y2=\"18\"></line></svg>">>}).

-define(RESIZE_DIRS, [<<"n">>, <<"s">>, <<"e">>, <<"w">>,
                      <<"ne">>, <<"nw">>, <<"se">>, <<"sw">>]).

%%%===================================================================
%%% Tooltip
%%%===================================================================

%% @doc Wrap `Trigger' so that hovering or focusing it shows `Content' in
%% a bubble (sigil's tooltip widget lives on its host element; the host
%% here is a `span.ah-tooltip-host' so the trigger keeps its own
%% behaviour). Css: position `top | bottom | left | right | mouse'
%% (default bottom), flag `no_arrow'. Options: `trigger' (hover | click |
%% none), `show_delay' (ms, 100), `auto_hide' (true), `auto_hide_delay'
%% (ms, 3000), `disabled', `width'.
-spec tooltip(html(), html(), css(), attrs()) -> element().
tooltip(Content, Trigger, Css, Attrs) ->
    Entry = aihtml_catalog:entry(?MODULE, tooltip),
    {Opts, Html} = aihtml_catalog:split_options(Entry, Attrs),
    {Mods, Lits} = split_css(Css),
    Root = aihtml_catalog:classes(Entry, Mods),
    Pos = group_value(Entry, position, Mods),
    Arrow = not lists:member(no_arrow, Mods),
    TipPos = case Pos of mouse -> bottom; _ -> Pos end,
    Tip = aihtml_html:el(span,
              [aihtml_html:el(span, [], [<<"ah-tooltip-arrow">>], [{aria_hidden, <<"true">>}]),
               aihtml_html:el(span, Content, [<<"ah-tooltip-content">>], [])],
              [<<"ah-tooltip">>, <<"ah-tooltip-", (atom_to_binary(TipPos))/binary>>,
               [<<"ah-tooltip-no-arrow">> || not Arrow], Lits],
              [{role, tooltip},
               {style, case maps:get(width, Opts, undefined) of
                           undefined -> undefined;
                           W -> [<<"width:">>, css_len(W)]
                       end}]),
    aihtml_html:el(span, [Trigger, Tip], Root,
                   [[{data_ah, tooltip} | tip_data(Opts#{position => Pos})], Html]).

%% @doc Attributes that give any element a plain-text tooltip, for when a
%% wrapper element is not wanted: `button(..., [tooltip_attrs(<<"Save">>,
%% #{position => top})])'. Opts as for tooltip/4 plus `position' and
%% `arrow' (boolean).
-spec tooltip_attrs(iodata(), map()) -> attrs().
tooltip_attrs(Text, Opts) when is_map(Opts) ->
    Arrow = maps:get(arrow, Opts, true),
    [{data_ah_tooltip, text(Text)},
     {data_ah_tip_arrow, if_(not Arrow, <<"false">>)}
     | tip_data(Opts)].

tip_data(Opts) ->
    [{data_ah_tip_position, opt_bin(position, Opts)},
     {data_ah_tip_trigger, opt_bin(trigger, Opts)},
     {data_ah_tip_delay, opt_bin(show_delay, Opts)},
     {data_ah_tip_auto_hide, opt_bin(auto_hide, Opts)},
     {data_ah_tip_hide_delay, opt_bin(auto_hide_delay, Opts)},
     {data_ah_tip_disabled, opt_bin(disabled, Opts)}].

%%%===================================================================
%%% Popover
%%%===================================================================

%% @doc A bubble anchored to the element that opened it (`toggles(Id)' on
%% a button) or to the `anchor' selector. Css: position `top | bottom |
%% left | right' (default bottom, flips when there is no room), flag
%% `no_arrow'. Options: `title', `closable' (close button in the title
%% bar), `anchor', `modal' (scrim, outside clicks do not close),
%% `auto_close' (close on outside click, default true), `width'.
-spec popover(html(), css(), attrs()) -> element().
popover(Children, Css, Attrs) ->
    Entry = aihtml_catalog:entry(?MODULE, popover),
    {Opts, Html} = aihtml_catalog:split_options(Entry, Attrs),
    {Mods, _} = split_css(Css),
    Pos = group_value(Entry, position, Mods),
    Title = maps:get(title, Opts, undefined),
    Closable = maps:get(closable, Opts, false),
    TitleBar = case Title of
                   undefined -> [];
                   _ ->
                       aihtml_html:el('div',
                           [Title,
                            [aihtml_html:el('div', [], [<<"ah-popover-close-btn">>],
                                            [{role, button}, {tabindex, 0},
                                             {title, <<"Close">>}, {aria_label, <<"Close">>},
                                             {data_ah_close, <<>>}]) || Closable]],
                           [<<"ah-popover-title">>], [])
               end,
    aihtml_html:el('div',
        [aihtml_html:el('div', [], [<<"ah-popover-arrow">>], [{aria_hidden, <<"true">>}]),
         TitleBar,
         aihtml_html:el('div', Children, [<<"ah-popover-content">>], [])],
        aihtml_catalog:classes(Entry, Css),
        [[{data_ah, popover}, {data_state, closed}, {role, dialog},
          {data_ah_position, Pos},
          {data_ah_anchor, opt_bin(anchor, Opts)},
          {data_ah_modal, opt_bin(modal, Opts)},
          {data_ah_auto_close, opt_bin(auto_close, Opts)},
          {aria_label, case Title of T when is_binary(T) -> T; _ -> undefined end},
          {style, case maps:get(width, Opts, undefined) of
                      undefined -> undefined;
                      W -> [<<"width:">>, css_len(W)]
                  end}],
         Html]).

%%%===================================================================
%%% Drawer and sheet
%%%===================================================================

%% @doc A modal panel that slides in from an edge and can be swiped away.
%% Css: side `bottom | top | left | right' (default bottom). Options:
%% `title', `description', `footer', `size' (width for left/right, height
%% otherwise; integer px or CSS length; default 50vh or 380px), `closable'
%% (true), `handle' (grab bar, true), `dismissible' (swipe to close, true),
%% `close_on_overlay' (true), `close_on_esc' (true), `open' (false).
-spec drawer(html(), css(), attrs()) -> element().
drawer(Children, Css, Attrs) -> slide(drawer, Children, Css, Attrs).

%% @doc A modal panel that slides in from an edge (no swipe gesture).
%% Css: side `right | left | top | bottom' (default right). Options as
%% drawer/3 without `handle' and `dismissible'; `size' defaults to 380px.
-spec sheet(html(), css(), attrs()) -> element().
sheet(Children, Css, Attrs) -> slide(sheet, Children, Css, Attrs).

slide(Kind, Children, Css, Attrs) ->
    Entry = aihtml_catalog:entry(?MODULE, Kind),
    {Opts, Html} = aihtml_catalog:split_options(Entry, Attrs),
    {Mods, Lits} = split_css(Css),
    Side = group_value(Entry, side, Mods),
    P = <<"ah-", (atom_to_binary(Kind))/binary>>,
    Title = maps:get(title, Opts, undefined),
    Desc = maps:get(description, Opts, undefined),
    Footer = maps:get(footer, Opts, undefined),
    Closable = maps:get(closable, Opts, true),
    Handle = Kind =:= drawer andalso maps:get(handle, Opts, true),
    Size = css_len(maps:get(size, Opts, default_size(Kind, Side))),
    Dim = case Side of left -> <<"width:">>; right -> <<"width:">>; _ -> <<"height:">> end,
    TitleId = case root_id(Html) of
                  undefined -> undefined;
                  Id -> <<Id/binary, "-title">>
              end,
    Header = case Title =/= undefined orelse Desc =/= undefined orelse Closable of
                 false -> [];
                 true ->
                     aihtml_html:el('div',
                         [aihtml_html:el('div',
                              [opt_el(h2, Title, [<<P/binary, "__title">>], [{id, TitleId}]),
                               opt_el(p, Desc, [<<P/binary, "__description">>], [])],
                              [], []),
                          [aihtml_html:el(button, ?CLOSE_ICON, [<<P/binary, "__close">>],
                                          [{type, button}, {aria_label, <<"close">>},
                                           {data_ah_close, <<>>}]) || Closable]],
                         [<<P/binary, "__header">>], [])
             end,
    Panel = aihtml_html:el('div',
                [[aihtml_html:el('div',
                      aihtml_html:el(span, [], [<<P/binary, "__handle-bar">>], []),
                      [<<P/binary, "__handle">>], [{aria_hidden, <<"true">>}]) || Handle],
                 Header,
                 aihtml_html:el('div', Children, [<<P/binary, "__body">>], []),
                 opt_el('div', Footer, [<<P/binary, "__footer">>], [])],
                [<<P/binary, "__panel">>, Lits],
                [{role, dialog}, {aria_modal, <<"true">>},
                 {aria_labelledby, case Title of undefined -> undefined; _ -> TitleId end},
                 {aria_label, case {TitleId, Title} of
                                  {undefined, T} when is_binary(T) -> T;
                                  _ -> undefined
                              end},
                 {data_side, Side}, {data_state, closed},
                 {style, [Dim, Size, $;]}, {tabindex, -1}]),
    aihtml_html:el('div', Panel, aihtml_catalog:classes(Entry, Mods),
                   [[{data_ah, Kind}, {data_state, closed},
                     {data_ah_esc, opt_bin(close_on_esc, Opts)},
                     {data_ah_scrim, opt_bin(close_on_overlay, Opts)},
                     {data_ah_dismissible, opt_bin(dismissible, Opts)},
                     {data_ah_initial, if_(maps:get(open, Opts, false) =:= true, <<"open">>)}],
                    Html]).

default_size(drawer, Side) when Side =:= left; Side =:= right -> <<"380px">>;
default_size(drawer, _) -> <<"50vh">>;
default_size(sheet, _) -> <<"380px">>.

%%%===================================================================
%%% Window
%%%===================================================================

%% @doc A floating dialog window: draggable by its title bar, resizable
%% from its edges, optionally modal. Rendered hidden, centred on first
%% open. Options: `title', `footer', `closable' (true), `collapsible'
%% (false), `collapsed' (false), `modal' (false; scrim, focus trap, scroll
%% lock), `draggable' (true), `resizable' (true), `width' (300), `height'
%% (auto), `close_on_overlay' (false), `close_on_esc' (true), `open'.
-spec window(html(), css(), attrs()) -> element().
window(Children, Css, Attrs) ->
    Entry = aihtml_catalog:entry(?MODULE, window),
    {Opts, Html} = aihtml_catalog:split_options(Entry, Attrs),
    Title = maps:get(title, Opts, <<>>),
    Footer = maps:get(footer, Opts, undefined),
    Modal = maps:get(modal, Opts, false) =:= true,
    Draggable = maps:get(draggable, Opts, true) =:= true,
    Resizable = maps:get(resizable, Opts, true) =:= true,
    Collapsed = maps:get(collapsed, Opts, false) =:= true,
    Collapsible = maps:get(collapsible, Opts, false) =:= true orelse Collapsed,
    Closable = maps:get(closable, Opts, true) =:= true,
    TitleId = case root_id(Html) of
                  undefined -> <<"ah-window-", (integer_to_binary(
                                                   erlang:unique_integer([positive])))/binary,
                                 "-title">>;
                  Id -> <<Id/binary, "-title">>
              end,
    Height = case maps:get(height, Opts, auto) of
                 auto -> [];
                 <<"auto">> -> [];
                 H -> [<<"height:">>, css_len(H), $;]
             end,
    Header = aihtml_html:el('div',
                 [aihtml_html:el('div', Title, [<<"ah-window-title">>], [{id, TitleId}]),
                  aihtml_html:el('div',
                      [[aihtml_html:el(button, [], [<<"ah-window-collapse-btn">>],
                                       [{type, button}, {aria_label, <<"Collapse">>},
                                        {aria_expanded, bool(not Collapsed)}])
                        || Collapsible],
                       [aihtml_html:el(button, [], [<<"ah-window-close-btn">>],
                                       [{type, button}, {aria_label, <<"Close">>},
                                        {data_ah_close, <<>>}])
                        || Closable]],
                      [<<"ah-window-header-buttons">>], [])],
                 [<<"ah-window-header">>, [<<"ah-window-header-draggable">> || Draggable]],
                 []),
    Handles = [aihtml_html:el('div', [],
                   [<<"ah-window-resize-handle">>, <<"ah-window-resize-", D/binary>>],
                   [{aria_hidden, <<"true">>}, {data_dir, D}])
               || D <- ?RESIZE_DIRS],
    aihtml_html:el('div',
        [Header,
         aihtml_html:el('div', Children, [<<"ah-window-content">>], []),
         opt_el('div', Footer, [<<"ah-window-footer">>], []),
         Handles],
        [aihtml_catalog:classes(Entry, Css),
         [<<"ah-window-resizable">> || Resizable],
         [<<"ah-window-collapsed">> || Collapsed]],
        [[{data_ah, window}, {data_state, closed}, {role, dialog}, {tabindex, -1},
          {aria_modal, bool(Modal)}, {aria_labelledby, TitleId},
          {style, [<<"display:none;width:">>, css_len(maps:get(width, Opts, 300)), $;, Height]},
          {data_ah_modal, if_(Modal, <<"true">>)},
          {data_ah_draggable, bool(Draggable)},
          {data_ah_esc, opt_bin(close_on_esc, Opts)},
          {data_ah_scrim, opt_bin(close_on_overlay, Opts)},
          {data_ah_initial, if_(maps:get(open, Opts, false) =:= true, <<"open">>)}],
         Html]).

%%%===================================================================
%%% Notification and toast
%%%===================================================================

%% @doc A hidden notification template (sigil's template + clone model):
%% the card is rendered here, from templates/notification.mustache, and
%% each `open' clones it into a stack in a screen corner. Open it with
%% `opens({id, Id})' or `open(Ctx, {id, Id})'. Css: variant `info |
%% success | warning | error' (default info), position `top_right |
%% top_left | bottom_right | bottom_left' (default top_right). Options:
%% `auto_close' (true), `delay' (ms, 3000), `closable' (true),
%% `close_on_click' (true), `width'.
-spec notification(html(), css(), attrs()) -> element().
notification(Content, Css, Attrs) ->
    Entry = aihtml_catalog:entry(?MODULE, notification),
    {Opts, Html} = aihtml_catalog:split_options(Entry, Attrs),
    {Mods, Lits} = split_css(Css),
    Pos = group_value(Entry, position, Mods),
    Duration = case maps:get(auto_close, Opts, true) of
                   false -> 0;
                   _ -> maps:get(delay, Opts, 3000)
               end,
    Card = card(Opts#{variant => group_value(Entry, variant, Mods)},
                aihtml_html:render_binary(Content)),
    aihtml_html:el('div', Card, [aihtml_catalog:classes(Entry, Mods), Lits],
                   [[{data_ah, notification}, {hidden, true},
                     {data_ah_position, dash(Pos)},
                     {data_ah_duration, Duration}],
                    Html]).

%% A card from templates/notification.mustache. The view is built the
%% same way in overlay.js (cardView).
card(Opts, ContentHtml) ->
    V = case maps:get(variant, Opts, info) of
            X when X =:= info; X =:= success; X =:= warning; X =:= error -> X;
            B when is_binary(B) -> variant(B);
            _ -> info
        end,
    aihtml_tpl:safe(tpl_notification(
        #{variant => atom_to_binary(V),
          info => V =:= info, success => V =:= success,
          warning => V =:= warning, error => V =:= error,
          clickable => maps:get(close_on_click, Opts, true) =/= false,
          closable => maps:get(closable, Opts, true) =/= false,
          width => case maps:get(width, Opts, undefined) of
                       undefined -> null;
                       W -> css_len(W)
                   end,
          content => ContentHtml})).

variant(<<"success">>) -> success;
variant(<<"warning">>) -> warning;
variant(<<"error">>) -> error;
variant(_) -> info.

%% @doc Attributes that pop a toast when the element is clicked, without a
%% server round trip. Opts: `description', `variant' (info | success |
%% warning | error), `duration' (ms, default 4000, 0 keeps it), `position'
%% (top_right ...), `closable'.
-spec shows_toast(iodata(), map()) -> attrs().
shows_toast(Message, Opts) when is_map(Opts) ->
    [{data_ah_toast, text(Message)},
     {data_ah_toast_description, case maps:get(description, Opts, undefined) of
                                     undefined -> undefined;
                                     D -> text(D)
                                 end},
     {data_ah_toast_variant, opt_bin(variant, Opts)},
     {data_ah_toast_duration, opt_bin(duration, Opts)},
     {data_ah_toast_position, case maps:get(position, Opts, undefined) of
                                  undefined -> undefined;
                                  P -> dash(P)
                              end},
     {data_ah_toast_closable, opt_bin(closable, Opts)}].

%%%===================================================================
%%% Declarative triggers
%%%===================================================================

%% @doc Attrs: a click opens `Target' (drawer, sheet, window, popover,
%% tooltip or notification template). A popover anchors to the element.
-spec opens(target()) -> attrs().
opens(Target) -> trigger_attrs(data_ah_open, Target).

%% @doc Attrs: a click toggles `Target'.
-spec toggles(target()) -> attrs().
toggles(Target) -> trigger_attrs(data_ah_toggle, Target).

%% @doc Attrs: a click closes the overlay the element is in.
-spec closes() -> attrs().
closes() -> [{data_ah_close, <<>>}].

%% @doc Attrs: a click closes `Target'.
-spec closes(target()) -> attrs().
closes(Target) -> [{data_ah_close, selector(Target)}].

%% @doc Attrs: a click closes `Target' (`closest' for the enclosing
%% overlay) and reports `Result' in the `ah:close' event, like sigil's
%% window ok/cancel buttons.
-spec closes(target() | closest, atom() | iodata()) -> attrs().
closes(closest, Result) -> [{data_ah_close, <<>>}, {data_ah_result, text(Result)}];
closes(Target, Result) -> [{data_ah_close, selector(Target)}, {data_ah_result, text(Result)}].

trigger_attrs(Key, Target) ->
    [{Key, selector(Target)}, {aria_haspopup, <<"dialog">>},
     {aria_controls, case Target of {id, Id} -> text(Id); _ -> undefined end}].

%%%===================================================================
%%% Server-driven helpers
%%%===================================================================

%% @doc In an action: open the overlay at `Target'.
-spec open(aihtml_action:ctx(), target()) -> ok.
open(Ctx, Target) -> aihtml_action:call(Ctx, Target, open, []).

%% @doc In an action: close the overlay at `Target'.
-spec close(aihtml_action:ctx(), target()) -> ok.
close(Ctx, Target) -> aihtml_action:call(Ctx, Target, close, []).

%% @doc In an action: toggle the overlay at `Target'.
-spec toggle(aihtml_action:ctx(), target()) -> ok.
toggle(Ctx, Target) -> aihtml_action:call(Ctx, Target, toggle, []).

%% Server-side cards: toast/3 and notify/2 render the card here, with
%% the same templates the browser uses, and send the finished HTML to
%% AH.fn("notify") as `card'; the browser only places it in its corner
%% (created on demand, so an html operation has no fixed target) and runs
%% its timer. No markup is built from options in the browser for them.

%% @doc In an action: pop a toast (sigil's toast/show!): title `Message'
%% plus Opts `description', `variant', `duration' (ms, default 4000, 0
%% keeps it), `position', `closable', `close_on_click', `width'.
-spec toast(aihtml_action:ctx(), iodata(), map()) -> ok.
toast(Ctx, Message, Opts) when is_map(Opts) ->
    Title = text(Message),
    Desc = case maps:get(description, Opts, undefined) of
               undefined -> <<>>;
               D -> text(D)
           end,
    {safe, Content} = aihtml_tpl:safe(tpl_toast(#{has_title => Title =/= <<>>, title => Title,
                                                  has_description => Desc =/= <<>>,
                                                  description => Desc})),
    send_card(Ctx, Opts, 4000, Content).

%% @doc In an action: show a notification card. Opts: `content' (html(),
%% rendered and escaped here), `variant', `position', `duration' (ms,
%% default 3000, 0 keeps it), `closable', `close_on_click', `width'.
-spec notify(aihtml_action:ctx(), map()) -> ok.
notify(Ctx, Opts) when is_map(Opts) ->
    send_card(Ctx, Opts, 3000,
              aihtml_html:render_binary(maps:get(content, Opts, <<>>))).

send_card(Ctx, Opts, DefaultDuration, Content) ->
    {safe, Card} = card(Opts, Content),
    aihtml_action:call(Ctx, global, notify,
                       [#{card => Card,
                          position => dash(maps:get(position, Opts, top_right)),
                          duration => maps:get(duration, Opts, DefaultDuration)}]).

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    Empty = fun(Ms) -> maps:from_list([{M, []} || M <- Ms]) end,
    Sides = [top, bottom, left, right],
    Variants = [info, success, warning, error],
    Corners = [top_right, top_left, bottom_right, bottom_left],
    Events = [<<"ah:open">>, <<"ah:close">>],
    [#{name => tooltip, category => overlay,
       signature => <<"tooltip(Content, Trigger, Css, Attrs)">>,
       root => <<"ah-tooltip-host">>,
       groups => #{position => {[top, bottom, left, right, mouse], bottom}},
       flags => [no_arrow],
       classes => Empty([top, bottom, left, right, mouse, no_arrow]),
       options => [trigger, show_delay, auto_hide, auto_hide_delay, disabled, width],
       option_docs => #{top => <<"Bubble above the trigger (flips when there is no room).">>,
                       bottom => <<"Bubble below the trigger (default).">>,
                       left => <<"Bubble to the left.">>, right => <<"Bubble to the right.">>,
                       mouse => <<"Bubble follows the mouse pointer.">>,
                       no_arrow => <<"No arrow.">>,
                       trigger => <<"hover (default, also keyboard focus) | click | none (methods only).">>,
                       show_delay => <<"Hover delay before showing, ms (100).">>,
                       auto_hide => <<"Hide by itself after auto_hide_delay (true).">>,
                       auto_hide_delay => <<"ms (3000).">>,
                       disabled => <<"Never show.">>,
                       width => <<"Bubble width, px or CSS length.">>},
       methods => [#{name => open, args => <<"()">>, doc => <<"Open it.">>},
                   #{name => close, args => <<"(Result)">>, doc => <<"Close it.">>},
                   #{name => toggle, args => <<"()">>, doc => <<"Open or close it.">>},
                   #{name => setContent, args => <<"(Text)">>, doc => <<"Replace the bubble text.">>}],
       behavior => <<"tooltip">>, events => Events,
       doc => <<"Wraps Trigger; hover, focus or click shows Content in a bubble. "
                "tooltip_attrs/2 does the same for any element with plain text.">>},
     #{name => popover, category => overlay,
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
                "the anchor selector; closes on outside click and Escape.">>},
     #{name => drawer, category => overlay,
       signature => <<"drawer(Children, Css, Attrs)">>,
       root => <<"ah-drawer__overlay">>,
       groups => #{side => {Sides, bottom}},
       classes => Empty(Sides),
       options => [title, description, footer, size, closable, handle, dismissible,
                   close_on_overlay, close_on_esc, open],
       option_docs => #{bottom => <<"Slides up from the bottom (default).">>, top => <<"From the top.">>,
                       left => <<"From the left.">>, right => <<"From the right.">>,
                       title => <<"Header title.">>, description => <<"Text under the title.">>,
                       footer => <<"Footer content, e.g. buttons with closes().">>,
                       size => <<"Width (left/right) or height; px or CSS length (50vh / 380px).">>,
                       closable => <<"Close button in the header (true).">>,
                       handle => <<"Grab bar (true).">>,
                       dismissible => <<"Swipe to close (true).">>,
                       close_on_overlay => <<"Close on a click on the scrim (true).">>,
                       close_on_esc => <<"Close on Escape (true).">>,
                       open => <<"Open when the page loads.">>},
       methods => [#{name => open, args => <<"()">>, doc => <<"Open it.">>},
                   #{name => close, args => <<"(Result)">>, doc => <<"Close it; Result (optional) is reported in ah:close.">>},
                   #{name => toggle, args => <<"()">>, doc => <<"Open or close it.">>},
                   #{name => isOpen, args => <<"()">>, doc => <<"True while open.">>}],
       behavior => <<"drawer">>, events => Events,
       doc => <<"Modal panel sliding in from an edge, swipe to dismiss. "
                "Methods open, close, toggle.">>},
     #{name => sheet, category => overlay,
       signature => <<"sheet(Children, Css, Attrs)">>,
       root => <<"ah-sheet__overlay">>,
       groups => #{side => {Sides, right}},
       classes => Empty(Sides),
       options => [title, description, footer, size, closable,
                   close_on_overlay, close_on_esc, open],
       option_docs => #{right => <<"Slides in from the right (default).">>, left => <<"From the left.">>,
                       top => <<"From the top.">>, bottom => <<"From the bottom.">>,
                       title => <<"Header title.">>, description => <<"Text under the title.">>,
                       footer => <<"Footer content.">>,
                       size => <<"Width (left/right) or height; px or CSS length (380px).">>,
                       closable => <<"Close button in the header (true).">>,
                       close_on_overlay => <<"Close on a click on the scrim (true).">>,
                       close_on_esc => <<"Close on Escape (true).">>,
                       open => <<"Open when the page loads.">>},
       methods => [#{name => open, args => <<"()">>, doc => <<"Open it.">>},
                   #{name => close, args => <<"(Result)">>, doc => <<"Close it; Result (optional) is reported in ah:close.">>},
                   #{name => toggle, args => <<"()">>, doc => <<"Open or close it.">>},
                   #{name => isOpen, args => <<"()">>, doc => <<"True while open.">>}],
       behavior => <<"sheet">>, events => Events,
       doc => <<"Modal side panel with scrim, scroll lock and focus trap. "
                "Methods open, close, toggle.">>},
     #{name => toast, category => overlay,
       signature => <<"toast(Ctx, Message, Opts)">>,
       root => <<"ah-notify">>,
       option_docs => #{description => <<"Second line under the message.">>,
                       variant => <<"info (default) | success | warning | error.">>,
                       duration => <<"ms before it closes (4000); 0 keeps it.">>,
                       position => <<"top_right (default) | top_left | bottom_right | bottom_left.">>,
                       closable => <<"Close button (true).">>,
                       close_on_click => <<"A click on the card closes it (true).">>,
                       width => <<"Card width.">>},
       methods => [],
       behavior => none, events => Events,
       doc => <<"Page-level API, not an element: toast/3 in an action or "
                "shows_toast/2 on a trigger pops a card built by AH.fn(\"toast\").">>},
     #{name => notification, category => overlay,
       signature => <<"notification(Content, Css, Attrs)">>,
       root => <<"ah-notify-tpl">>,
       groups => #{variant => {Variants, info}, position => {Corners, top_right}},
       classes => Empty(Variants ++ Corners),
       options => [auto_close, delay, closable, close_on_click, width],
       option_docs => #{info => <<"Info colours and icon (default).">>, success => <<"Success.">>,
                       warning => <<"Warning.">>, error => <<"Error.">>,
                       top_right => <<"Stack in the top right corner (default).">>,
                       top_left => <<"Top left.">>, bottom_right => <<"Bottom right.">>,
                       bottom_left => <<"Bottom left.">>,
                       auto_close => <<"Close by itself after delay (true).">>,
                       delay => <<"ms (3000).">>,
                       closable => <<"Close button (true).">>,
                       close_on_click => <<"A click on the card closes it (true).">>,
                       width => <<"Card width.">>},
       methods => [#{name => open, args => <<"()">>, doc => <<"Show one more card cloned from the template.">>},
                   #{name => close, args => <<"()">>, doc => <<"Close all its cards.">>},
                   #{name => closeAll, args => <<"()">>, doc => <<"Close all its cards.">>},
                   #{name => closeLast, args => <<"()">>, doc => <<"Close the newest card.">>}],
       behavior => <<"notification">>,
       events => Events ++ [<<"ah:click">>],
       doc => <<"Hidden template; each open clones it into a stacked corner card. "
                "Methods open, closeAll, closeLast. notify/2 needs no template.">>},
     #{name => window, category => overlay,
       signature => <<"window(Children, Css, Attrs)">>,
       root => <<"ah-window">>,
       options => [title, footer, closable, collapsible, collapsed, modal, draggable,
                   resizable, width, height, close_on_overlay, close_on_esc, open],
       option_docs => #{title => <<"Title bar text.">>, footer => <<"Footer content, e.g. closes(closest, ok).">>,
                       closable => <<"Close button (true).">>,
                       collapsible => <<"Collapse button (false).">>,
                       collapsed => <<"Start collapsed.">>,
                       modal => <<"Scrim, focus trap and scroll lock (false).">>,
                       draggable => <<"Move by the title bar (true).">>,
                       resizable => <<"Resize from the edges (true).">>,
                       width => <<"px or CSS length (300).">>, height => <<"px or CSS length (auto).">>,
                       close_on_overlay => <<"Close on a click on the scrim (false).">>,
                       close_on_esc => <<"Close on Escape (true).">>,
                       open => <<"Open when the page loads.">>},
       methods => [#{name => open, args => <<"()">>, doc => <<"Open it.">>},
                   #{name => close, args => <<"(Result)">>, doc => <<"Close it; Result (optional) is reported in ah:close.">>},
                   #{name => toggle, args => <<"()">>, doc => <<"Open or close it.">>},
                   #{name => isOpen, args => <<"()">>, doc => <<"True while open.">>},
                   #{name => collapse, args => <<"()">>, doc => <<"Collapse to the title bar.">>},
                   #{name => expand, args => <<"()">>, doc => <<"Expand again.">>},
                   #{name => move, args => <<"(X, Y)">>, doc => <<"Move to viewport coordinates.">>},
                   #{name => resize, args => <<"(W, H)">>, doc => <<"Resize, px.">>},
                   #{name => bringToFront, args => <<"()">>, doc => <<"Raise above other overlays.">>}],
       behavior => <<"window">>,
       events => Events ++ [<<"ah:collapse">>, <<"ah:expand">>, <<"ah:moved">>,
                            <<"ah:resize">>],
       doc => <<"Draggable, resizable dialog window, optionally modal. "
                "Methods open, close, toggle, collapse, expand, move, resize.">>}].

%%%===================================================================
%%% Internal
%%%===================================================================

split_css(Css) -> lists:partition(fun is_atom/1, flatten(Css)).

flatten(L) when is_list(L) ->
    case L =/= [] andalso io_lib:printable_unicode_list(L) of
        true -> [L];
        false -> lists:flatmap(fun flatten/1, L)
    end;
flatten(X) -> [X].

%% The modifier chosen in `Group' (validated by aihtml_catalog:classes/2).
group_value(Entry, Group, Mods) ->
    #{groups := #{Group := {Values, Default}}} = Entry,
    case [M || M <- Mods, lists:member(M, Values)] of
        [M | _] -> M;
        [] -> Default
    end.

root_id(Html) ->
    case lists:keyfind(<<"id">>, 1, aihtml_html:attrs(Html)) of
        {_, Id} when is_binary(Id) -> Id;
        _ -> undefined
    end.

opt_el(_Tag, undefined, _Css, _Attrs) -> [];
opt_el(Tag, Content, Css, Attrs) -> aihtml_html:el(Tag, Content, Css, Attrs).

opt_bin(Key, Opts) ->
    case maps:get(Key, Opts, undefined) of
        undefined -> undefined;
        true -> <<"true">>;
        false -> <<"false">>;
        V when is_atom(V) -> atom_to_binary(V);
        V when is_integer(V) -> integer_to_binary(V);
        V -> text(V)
    end.

if_(true, V) -> V;
if_(false, _) -> undefined.

bool(true) -> <<"true">>;
bool(false) -> <<"false">>.

css_len(N) when is_integer(N) -> <<(integer_to_binary(N))/binary, "px">>;
css_len(V) -> text(V).

dash(A) when is_atom(A) -> dash(atom_to_binary(A));
dash(B) -> binary:replace(text(B), <<"_">>, <<"-">>, [global]).

selector({id, Id}) -> <<"#", (text(Id))/binary>>;
selector(Sel) -> text(Sel).

text(A) when is_atom(A) -> atom_to_binary(A);
text(I) when is_integer(I) -> integer_to_binary(I);
text(B) when is_binary(B) -> B;
text(L) when is_list(L) ->
    case unicode:characters_to_binary(L) of
        B when is_binary(B) -> B;
        _ -> error({aihtml, {bad_text, L}})
    end.

%% @doc Attribute helpers re-exported by the aihtml facade (see
%% scripts/gen-facade.escript); open/close/toggle/notify stay module
%% qualified because their names are too generic to import.
-spec facade_extras() -> [{atom(), arity()}].
facade_extras() ->
    [{tooltip_attrs, 2}, {opens, 1}, {closes, 0}, {closes, 1}, {closes, 2},
     {toggles, 1}, {shows_toast, 2}].
