%%%-------------------------------------------------------------------
%%% @doc A window, ported from sigil's overlay/window: a floating dialog
%%% window, draggable by its title bar, resizable from its edges,
%%% optionally modal.
%%%
%%% Everything renders server-side, hidden; the behaviour opens and closes
%%% it (see aihtml_lib_overlay for the ways to drive an overlay: opens/1,
%%% toggles/1, closes/0,1,2 in Attrs, aihtml_lib_overlay:open/2 and
%%% close/2 in an action, AH.invoke in the browser). Opening and closing
%%% fire the DOM events `ah:open' and `ah:close' on the component root;
%%% `ah:close' carries `{result}' (the `closes/2' result, or null).
%%%
%%% Literal (binary) classes in `Css' go on the window. A record's
%%% postback fires on `ah:close'. Behaviour:
%%% assets/js/components/window.ts. window/3 builds an #ah_window{}
%%% (include/aihtml_window.hrl) and render/1 turns it into HTML
%%% (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_window).
-behaviour(aihtml_element).

-include("aihtml_window.hrl").

-export([window/3, render/1, fields/1, catalog/0]).

-define(H, aihtml_html).
-define(E, aihtml_element).
-define(L, aihtml_lib_overlay).

-define(RESIZE_DIRS, [<<"n">>, <<"s">>, <<"e">>, <<"w">>,
                      <<"ne">>, <<"nw">>, <<"se">>, <<"sw">>]).

%%%===================================================================
%%% Builder
%%%===================================================================

%% @doc A floating dialog window: draggable by its title bar, resizable
%% from its edges, optionally modal. Rendered hidden, centred on first
%% open. Options: `title', `footer', `closable' (true), `collapsible'
%% (false), `collapsed' (false), `modal' (false; scrim, focus trap, scroll
%% lock), `draggable' (true), `resizable' (true), `width' (300), `height'
%% (auto), `close_on_overlay' (false), `close_on_esc' (true), `open'.
-spec window(aihtml_html:html(), aihtml_html:css(), aihtml_html:attrs()) -> #ah_window{}.
window(Children, Css, Attrs) ->
    ?E:build(?MODULE, #ah_window{body = Children}, Css, Attrs).

%% @doc The field names of #ah_window{}.
-spec fields(atom()) -> [atom()].
fields(ah_window) -> record_info(fields, ah_window).

%%%===================================================================
%%% Rendering
%%%===================================================================

-spec render(#ah_window{}) -> aihtml_html:html().
render(#ah_window{body = Children, title = Title, footer = Footer, width = Width} = W) ->
    Modal = ?L:bool(modal, W#ah_window.modal),
    Draggable = ?L:bool(draggable, W#ah_window.draggable),
    Resizable = ?L:bool(resizable, W#ah_window.resizable),
    Collapsed = ?L:bool(collapsed, W#ah_window.collapsed),
    Collapsible = ?L:bool(collapsible, W#ah_window.collapsible) orelse Collapsed,
    Closable = ?L:bool(closable, W#ah_window.closable),
    Opts = #{close_on_esc => ?L:opt_bool(close_on_esc, W#ah_window.close_on_esc),
             close_on_overlay => ?L:opt_bool(close_on_overlay, W#ah_window.close_on_overlay)},
    TitleId = case ?L:root_id(W) of
                  undefined -> <<"ah-window-", (integer_to_binary(
                                                   erlang:unique_integer([positive])))/binary,
                                 "-title">>;
                  Id -> <<Id/binary, "-title">>
              end,
    Height = case W#ah_window.height of
                 auto -> [];
                 <<"auto">> -> [];
                 H -> [<<"height:">>, ?L:css_len(H), $;]
             end,
    Header = ?H:el('div',
                 [?H:el('div', Title, [<<"ah-window-title">>], [{id, TitleId}]),
                  ?H:el('div',
                      [[?H:el(button, [], [<<"ah-window-collapse-btn">>],
                              [{type, button}, {aria_label, <<"Collapse">>},
                               {aria_expanded, ?L:bool(not Collapsed)}])
                        || Collapsible],
                       [?H:el(button, [], [<<"ah-window-close-btn">>],
                              [{type, button}, {aria_label, <<"Close">>},
                               {data_ah_close, <<>>}])
                        || Closable]],
                      [<<"ah-window-header-buttons">>], [])],
                 [<<"ah-window-header">>, [<<"ah-window-header-draggable">> || Draggable]],
                 []),
    Handles = [?H:el('div', [],
                   [<<"ah-window-resize-handle">>, <<"ah-window-resize-", D/binary>>],
                   [{aria_hidden, <<"true">>}, {data_dir, D}])
               || D <- ?RESIZE_DIRS],
    ?H:el('div',
        [Header,
         ?H:el('div', Children, [<<"ah-window-content">>], []),
         ?L:opt_el('div', Footer, [<<"ah-window-footer">>], []),
         Handles],
        [?E:classes(?MODULE, W),
         [<<"ah-window-resizable">> || Resizable],
         [<<"ah-window-collapsed">> || Collapsed]],
        [[{data_ah, window}, {data_state, closed}, {role, dialog}, {tabindex, -1},
          {aria_modal, ?L:bool(Modal)}, {aria_labelledby, TitleId},
          {style, [<<"display:none;width:">>, ?L:css_len(Width), $;, Height]},
          {data_ah_modal, ?L:if_(Modal, <<"true">>)},
          {data_ah_draggable, ?L:bool(Draggable)},
          {data_ah_esc, ?L:opt_bin(close_on_esc, Opts)},
          {data_ah_scrim, ?L:opt_bin(close_on_overlay, Opts)},
          {data_ah_initial, ?L:if_(?L:bool(open, W#ah_window.open), <<"open">>)}],
         ?E:root_attrs(W, 'ah:close')]).

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    Events = [<<"ah:open">>, <<"ah:close">>],
    [#{name => window, category => overlay,
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
