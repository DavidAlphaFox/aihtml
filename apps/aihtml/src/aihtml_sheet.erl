%%%-------------------------------------------------------------------
%%% @doc A sheet, ported from sigil's overlay/sheet: a modal panel that
%%% slides in from an edge (no swipe gesture).
%%%
%%% Everything renders server-side, hidden; the behaviour opens and closes
%%% it (see aihtml_lib_overlay for the ways to drive an overlay: opens/1,
%%% toggles/1, closes/0,1,2 in Attrs, aihtml_lib_overlay:open/2 and
%%% close/2 in an action, AH.invoke in the browser). Opening and closing
%%% fire the DOM events `ah:open' and `ah:close' on the component root;
%%% `ah:close' carries `{result}' (the `closes/2' result, or null).
%%%
%%% Literal (binary) classes in `Css' go on the panel. A record's
%%% postback fires on `ah:close'. Behaviour: assets/js/components/sheet.ts
%%% (the markup and behaviour are shared with the drawer, see
%%% aihtml_lib_overlay:slide/5). sheet/3 builds an #ah_sheet{}
%%% (include/aihtml_sheet.hrl) and render/1 turns it into HTML
%%% (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_sheet).
-behaviour(aihtml_element).

-include("aihtml_sheet.hrl").

-export([sheet/3, render/1, fields/1, catalog/0]).

-define(H, aihtml_html).
-define(E, aihtml_element).
-define(L, aihtml_lib_overlay).

%%%===================================================================
%%% Builder
%%%===================================================================

%% @doc A modal panel that slides in from an edge (no swipe gesture).
%% Css: side `right | left | top | bottom' (default right). Options as
%% drawer/3 without `handle' and `dismissible'; `size' defaults to 380px.
-spec sheet(aihtml_html:html(), aihtml_html:css(), aihtml_html:attrs()) -> #ah_sheet{}.
sheet(Children, Css, Attrs) ->
    ?E:build(?MODULE, #ah_sheet{body = Children}, Css, Attrs).

%% @doc The field names of #ah_sheet{}.
-spec fields(atom()) -> [atom()].
fields(ah_sheet) -> record_info(fields, ah_sheet).

%%%===================================================================
%%% Rendering
%%%===================================================================

-spec render(#ah_sheet{}) -> aihtml_html:html().
render(#ah_sheet{} = S) ->
    ?L:slide(sheet, S, ?E:classes(?MODULE, S#ah_sheet{css = []}), S#ah_sheet.css,
            #{body => S#ah_sheet.body, side => S#ah_sheet.side,
              title => S#ah_sheet.title,
              description => S#ah_sheet.description,
              footer => S#ah_sheet.footer, size => S#ah_sheet.size,
              closable => ?L:bool(closable, S#ah_sheet.closable),
              handle => false, dismissible => undefined,
              close_on_overlay => ?L:opt_bool(close_on_overlay,
                                           S#ah_sheet.close_on_overlay),
              close_on_esc => ?L:opt_bool(close_on_esc, S#ah_sheet.close_on_esc),
              open => ?L:bool(open, S#ah_sheet.open)}).

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    Empty = fun(Ms) -> maps:from_list([{M, []} || M <- Ms]) end,
    Sides = [top, bottom, left, right],
    Events = [<<"ah:open">>, <<"ah:close">>],
    [#{name => sheet, category => overlay,
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
                "Methods open, close, toggle.">>}].
