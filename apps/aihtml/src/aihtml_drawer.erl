%%%-------------------------------------------------------------------
%%% @doc A drawer, ported from sigil's overlay/drawer: a modal panel that
%%% slides in from an edge and can be swiped away.
%%%
%%% Everything renders server-side, hidden; the behaviour opens and closes
%%% it (see aihtml_lib_overlay for the ways to drive an overlay: opens/1,
%%% toggles/1, closes/0,1,2 in Attrs, aihtml_lib_overlay:open/2 and
%%% close/2 in an action, AH.invoke in the browser). Opening and closing
%%% fire the jQuery events `ah:open' and `ah:close' on the component root;
%%% `ah:close' carries `{result}' (the `closes/2' result, or null).
%%%
%%% Literal (binary) classes in `Css' go on the panel. A record's
%%% postback fires on `ah:close'. Behaviour: assets/js/components/drawer.js
%%% (the markup and behaviour are shared with the sheet, see
%%% aihtml_lib_overlay:slide/5). drawer/3 builds an #ah_drawer{}
%%% (include/aihtml_drawer.hrl) and render/1 turns it into HTML
%%% (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_drawer).
-behaviour(aihtml_element).

-include("aihtml_drawer.hrl").

-export([drawer/3, render/1, fields/1, catalog/0]).

-define(H, aihtml_html).
-define(E, aihtml_element).
-define(L, aihtml_lib_overlay).

%%%===================================================================
%%% Builder
%%%===================================================================

%% @doc A modal panel that slides in from an edge and can be swiped away.
%% Css: side `bottom | top | left | right' (default bottom). Options:
%% `title', `description', `footer', `size' (width for left/right, height
%% otherwise; integer px or CSS length; default 50vh or 380px), `closable'
%% (true), `handle' (grab bar, true), `dismissible' (swipe to close, true),
%% `close_on_overlay' (true), `close_on_esc' (true), `open' (false).
-spec drawer(aihtml_html:html(), aihtml_html:css(), aihtml_html:attrs()) -> #ah_drawer{}.
drawer(Children, Css, Attrs) ->
    ?E:build(?MODULE, #ah_drawer{body = Children}, Css, Attrs).

%% @doc The field names of #ah_drawer{}.
-spec fields(atom()) -> [atom()].
fields(ah_drawer) -> record_info(fields, ah_drawer).

%%%===================================================================
%%% Rendering
%%%===================================================================

-spec render(#ah_drawer{}) -> aihtml_html:html().
render(#ah_drawer{} = D) ->
    ?L:slide(drawer, D, ?E:classes(?MODULE, D#ah_drawer{css = []}), D#ah_drawer.css,
            #{body => D#ah_drawer.body, side => D#ah_drawer.side,
              title => D#ah_drawer.title,
              description => D#ah_drawer.description,
              footer => D#ah_drawer.footer, size => D#ah_drawer.size,
              closable => ?L:bool(closable, D#ah_drawer.closable),
              handle => ?L:bool(handle, D#ah_drawer.handle),
              dismissible => ?L:opt_bool(dismissible, D#ah_drawer.dismissible),
              close_on_overlay => ?L:opt_bool(close_on_overlay,
                                           D#ah_drawer.close_on_overlay),
              close_on_esc => ?L:opt_bool(close_on_esc, D#ah_drawer.close_on_esc),
              open => ?L:bool(open, D#ah_drawer.open)}).

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    Empty = fun(Ms) -> maps:from_list([{M, []} || M <- Ms]) end,
    Sides = [top, bottom, left, right],
    Events = [<<"ah:open">>, <<"ah:close">>],
    [#{name => drawer, category => overlay,
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
                "Methods open, close, toggle.">>}].
