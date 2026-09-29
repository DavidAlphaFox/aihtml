%%%-------------------------------------------------------------------
%%% @doc A notification, ported from sigil's overlay/notification: a hidden
%%% card template (sigil's template + clone model); each `open' clones it
%%% into a stack in a screen corner. notify/2 shows a card from an action
%%% without a template.
%%%
%%% Everything renders server-side, hidden; the behaviour opens and closes
%%% it (see aihtml_lib_overlay for the ways to drive an overlay: opens/1,
%%% toggles/1, closes/0,1,2 in Attrs, aihtml_lib_overlay:open/2 and
%%% close/2 in an action, AH.invoke in the browser). Opening and closing
%%% fire the jQuery events `ah:open' and `ah:close' on the component root;
%%% `ah:close' carries `{result}' (the `closes/2' result, or null).
%%%
%%% The card is rendered from templates/notification.mustache (see
%%% aihtml_lib_overlay:card/2, shared with aihtml_toast). A record's
%%% postback fires on `ah:close' (when a card closes). Behaviour:
%%% assets/js/components/notification.js. notification/3 builds an
%%% #ah_notification{} (include/aihtml_notification.hrl) and render/1
%%% turns it into HTML (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_notification).
-behaviour(aihtml_element).

-include("aihtml_notification.hrl").

-export([notification/3, notify/2, render/1, fields/1, catalog/0]).

-export_type([variant/0]).

-define(H, aihtml_html).
-define(E, aihtml_element).
-define(L, aihtml_lib_overlay).

-type variant() :: info | success | warning | error.

%%%===================================================================
%%% Builder
%%%===================================================================

%% @doc A hidden notification template (sigil's template + clone model):
%% the card is rendered here, from templates/notification.mustache, and
%% each `open' clones it into a stack in a screen corner. Open it with
%% `opens({id, Id})' or `aihtml_lib_overlay:open(Ctx, {id, Id})'. Css:
%% variant `info | success | warning | error' (default info), position `top_right |
%% top_left | bottom_right | bottom_left' (default top_right). Options:
%% `auto_close' (true), `delay' (ms, 3000), `closable' (true),
%% `close_on_click' (true), `width'.
-spec notification(aihtml_html:html(), aihtml_html:css(), aihtml_html:attrs()) ->
          #ah_notification{}.
notification(Content, Css, Attrs) ->
    ?E:build(?MODULE, #ah_notification{body = Content}, Css, Attrs).

%% @doc The field names of #ah_notification{}.
-spec fields(atom()) -> [atom()].
fields(ah_notification) -> record_info(fields, ah_notification).

%%%===================================================================
%%% Server-driven
%%%===================================================================

%% @doc In an action: show a notification card. Opts: `content' (html(),
%% rendered and escaped here), `variant', `position', `duration' (ms,
%% default 3000, 0 keeps it), `closable', `close_on_click', `width'.
-spec notify(aihtml_action:ctx(), map()) -> ok.
notify(Ctx, Opts) when is_map(Opts) ->
    ?L:send_card(Ctx, Opts, 3000,
                 aihtml_html:render_binary(maps:get(content, Opts, <<>>))).

%%%===================================================================
%%% Rendering
%%%===================================================================

-spec render(#ah_notification{}) -> aihtml_html:html().
render(#ah_notification{body = Content, variant = Variant, position = Pos} = N) ->
    Classes = ?E:classes(?MODULE, N),
    Duration = case ?L:bool(auto_close, N#ah_notification.auto_close) of
                   false -> 0;
                   true -> ?L:int(delay, N#ah_notification.delay)
               end,
    Card = ?L:card(#{variant => Variant,
                     closable => ?L:bool(closable, N#ah_notification.closable),
                     close_on_click => ?L:bool(close_on_click,
                                               N#ah_notification.close_on_click),
                     width => N#ah_notification.width},
                   ?H:render_binary(Content)),
    ?H:el('div', Card, Classes,
          [[{data_ah, notification}, {hidden, true},
            {data_ah_position, ?L:dash(Pos)},
            {data_ah_duration, Duration}],
           ?E:root_attrs(N, 'ah:close')]).

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    Empty = fun(Ms) -> maps:from_list([{M, []} || M <- Ms]) end,
    Variants = [info, success, warning, error],
    Corners = [top_right, top_left, bottom_right, bottom_left],
    Events = [<<"ah:open">>, <<"ah:close">>],
    [#{name => notification, category => overlay,
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
                "Methods open, closeAll, closeLast. notify/2 needs no template.">>}].
