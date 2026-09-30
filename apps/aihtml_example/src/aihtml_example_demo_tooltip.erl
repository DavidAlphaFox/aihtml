%% @doc Demos of the tooltip (aihtml_tooltip), shown on
%% /components/tooltip. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
-module(aihtml_example_demo_tooltip).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0]).
-export([tooltip_positions/0, tooltip_triggers/0, tooltip_on_any_element/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => tooltip, title => <<"Tooltip">>,
       summary => <<"悬停、聚焦或点击时浮出的一句话提示。"/utf8>>,
       demos => [{<<"四个方向"/utf8>>, tooltip_positions},
                 {<<"点击触发、跟随鼠标、无箭头"/utf8>>, tooltip_triggers},
                 {<<"不包裹元素：tooltip_attrs/2"/utf8>>, tooltip_on_any_element}]}].

%%%===================================================================
%%% Tooltip
%%%===================================================================

-spec tooltip_positions() -> aihtml:html().
tooltip_positions() ->
    row([ah_tooltip(<<"Shown above">>, ah_button(<<"Top">>, undefined, [outlined], []), [top], []),
         ah_tooltip(<<"Shown below">>, ah_button(<<"Bottom">>, undefined, [outlined], []), [], []),
         ah_tooltip(<<"On the left">>, ah_button(<<"Left">>, undefined, [outlined], []), [left], []),
         ah_tooltip(<<"On the right">>, ah_button(<<"Right">>, undefined, [outlined], []), [right], [])]).

-spec tooltip_triggers() -> aihtml:html().
tooltip_triggers() ->
    row([ah_tooltip(<<"Opened by a click">>, ah_button(<<"Click me">>, undefined, [], []),
                    [top], [{trigger, click}]),
         ah_tooltip(<<"Follows the mouse">>, ah_button(<<"Mouse">>, undefined, [default], []),
                    [mouse], []),
         ah_tooltip(<<"No arrow, stays until you leave">>,
                    ah_button(<<"No arrow">>, undefined, [default], []),
                    [no_arrow], [{auto_hide, false}])]).

-spec tooltip_on_any_element() -> aihtml:html().
tooltip_on_any_element() ->
    row([ah_button(<<"Save">>, save, [primary],
                   tooltip_attrs(<<"Save the document (Ctrl+S)">>, #{position => top})),
         ah_span(<<"Hover this text">>, [<<"underline decoration-dotted">>],
                 [{tabindex, 0}, tooltip_attrs(<<"Any element works">>, #{})])]).

%%%===================================================================
%%% Helpers
%%%===================================================================

row(Children) ->
    ah_div(Children, [<<"flex flex-wrap items-center gap-3">>], []).
