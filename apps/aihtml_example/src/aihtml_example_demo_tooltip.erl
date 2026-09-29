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
    row([tooltip(<<"Shown above">>, button(<<"Top">>, undefined, [outlined], []), [top], []),
         tooltip(<<"Shown below">>, button(<<"Bottom">>, undefined, [outlined], []), [], []),
         tooltip(<<"On the left">>, button(<<"Left">>, undefined, [outlined], []), [left], []),
         tooltip(<<"On the right">>, button(<<"Right">>, undefined, [outlined], []), [right], [])]).

-spec tooltip_triggers() -> aihtml:html().
tooltip_triggers() ->
    row([tooltip(<<"Opened by a click">>, button(<<"Click me">>, undefined, [], []),
                 [top], [{trigger, click}]),
         tooltip(<<"Follows the mouse">>, button(<<"Mouse">>, undefined, [default], []),
                 [mouse], []),
         tooltip(<<"No arrow, stays until you leave">>,
                 button(<<"No arrow">>, undefined, [default], []),
                 [no_arrow], [{auto_hide, false}])]).

-spec tooltip_on_any_element() -> aihtml:html().
tooltip_on_any_element() ->
    row([button(<<"Save">>, save, [primary],
                tooltip_attrs(<<"Save the document (Ctrl+S)">>, #{position => top})),
         span(<<"Hover this text">>, [<<"underline decoration-dotted">>],
              [{tabindex, 0}, tooltip_attrs(<<"Any element works">>, #{})])]).

%%%===================================================================
%%% Helpers
%%%===================================================================

row(Children) ->
    'div'(Children, [<<"flex flex-wrap items-center gap-3">>], []).
