%%%-------------------------------------------------------------------
%%% @doc A tooltip, ported from sigil's overlay/tooltip.
%%%
%%% Everything renders server-side, hidden; the behaviour opens and closes
%%% it (see aihtml_lib_overlay for the ways to drive an overlay: opens/1,
%%% toggles/1, closes/0,1,2 in Attrs, aihtml_lib_overlay:open/2 and
%%% close/2 in an action, AH.invoke in the browser). Opening and closing
%%% fire the DOM events `ah:open' and `ah:close' on the component root;
%%% `ah:close' carries `{result}' (the `closes/2' result, or null).
%%%
%%% Literal (binary) classes in `Css' go on the tooltip bubble. The
%%% record has no postback. Behaviour: assets/js/components/tooltip.js.
%%% tooltip/4 builds an #ah_tooltip{} (include/aihtml_tooltip.hrl) and
%%% render/1 turns it into HTML (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_tooltip).
-behaviour(aihtml_element).

-include("aihtml_tooltip.hrl").

-export([tooltip/4, tooltip_attrs/2, render/1, fields/1, catalog/0, facade_extras/0]).

-export_type([position/0, trigger/0]).

-define(H, aihtml_html).
-define(E, aihtml_element).
-define(L, aihtml_lib_overlay).

-type position() :: aihtml_lib_overlay:side() | mouse.
-type trigger() :: hover | click | none.

%%%===================================================================
%%% Builders
%%%===================================================================

%% @doc Wrap `Trigger' so that hovering or focusing it shows `Content' in
%% a bubble (sigil's tooltip widget lives on its host element; the host
%% here is a `span.ah-tooltip-host' so the trigger keeps its own
%% behaviour). Css: position `top | bottom | left | right | mouse'
%% (default bottom), flag `no_arrow'. Options: `trigger' (hover | click |
%% none), `show_delay' (ms, 100), `auto_hide' (true), `auto_hide_delay'
%% (ms, 3000), `disabled', `width'. In the record, `Trigger' is the
%% `anchor' field, because `trigger' is the option.
-spec tooltip(aihtml_html:html(), aihtml_html:html(), aihtml_html:css(), aihtml_html:attrs()) ->
          #ah_tooltip{}.
tooltip(Content, Trigger, Css, Attrs) ->
    ?E:build(?MODULE, #ah_tooltip{body = Content, anchor = Trigger}, Css, Attrs).

%% @doc Attributes that give any element a plain-text tooltip, for when a
%% wrapper element is not wanted: `button(..., [tooltip_attrs(<<"Save">>,
%% #{position => top})])'. Opts as for tooltip/4 plus `position' and
%% `arrow' (boolean).
-spec tooltip_attrs(iodata(), map()) -> aihtml_html:attrs().
tooltip_attrs(Text, Opts) when is_map(Opts) ->
    Arrow = maps:get(arrow, Opts, true),
    [{data_ah_tooltip, ?L:text(Text)},
     {data_ah_tip_arrow, ?L:if_(not Arrow, <<"false">>)}
     | tip_data(Opts)].

tip_data(Opts) ->
    [{data_ah_tip_position, ?L:opt_bin(position, Opts)},
     {data_ah_tip_trigger, ?L:opt_bin(trigger, Opts)},
     {data_ah_tip_delay, ?L:opt_bin(show_delay, Opts)},
     {data_ah_tip_auto_hide, ?L:opt_bin(auto_hide, Opts)},
     {data_ah_tip_hide_delay, ?L:opt_bin(auto_hide_delay, Opts)},
     {data_ah_tip_disabled, ?L:opt_bin(disabled, Opts)}].

%% @doc The field names of #ah_tooltip{}.
-spec fields(atom()) -> [atom()].
fields(ah_tooltip) -> record_info(fields, ah_tooltip).

%% @doc Attribute helpers re-exported by the aihtml facade.
-spec facade_extras() -> [{atom(), arity()}].
facade_extras() -> [{tooltip_attrs, 2}].

%%%===================================================================
%%% Rendering
%%%===================================================================

-spec render(#ah_tooltip{}) -> aihtml_html:html().
render(#ah_tooltip{body = Content, anchor = Trigger, position = Pos,
                   no_arrow = NoArrow, width = Width} = T) ->
    %% literal classes go on the bubble, not on the host
    Root = ?E:classes(?MODULE, T#ah_tooltip{css = []}),
    lists:member(T#ah_tooltip.trigger, [undefined, hover, click, none])
        orelse error({aihtml, {bad_option, trigger, T#ah_tooltip.trigger}}),
    Opts = #{position => Pos,
             trigger => T#ah_tooltip.trigger,
             show_delay => ?L:opt_int(show_delay, T#ah_tooltip.show_delay),
             auto_hide => ?L:opt_bool(auto_hide, T#ah_tooltip.auto_hide),
             auto_hide_delay => ?L:opt_int(auto_hide_delay, T#ah_tooltip.auto_hide_delay),
             disabled => ?L:opt_bool(disabled, T#ah_tooltip.disabled)},
    TipPos = case Pos of mouse -> bottom; _ -> Pos end,
    Tip = ?H:el(span,
              [?H:el(span, [], [<<"ah-tooltip-arrow">>], [{aria_hidden, <<"true">>}]),
               ?H:el(span, Content, [<<"ah-tooltip-content">>], [])],
              [<<"ah-tooltip">>, <<"ah-tooltip-", (atom_to_binary(TipPos))/binary>>,
               [<<"ah-tooltip-no-arrow">> || NoArrow], T#ah_tooltip.css],
              [{role, tooltip}, {style, ?L:width_style(Width)}]),
    ?H:el(span, [Trigger, Tip], Root,
          [[{data_ah, tooltip} | tip_data(Opts)], ?E:root_attrs(T, none)]).

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    Empty = fun(Ms) -> maps:from_list([{M, []} || M <- Ms]) end,
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
                "tooltip_attrs/2 does the same for any element with plain text.">>}].
