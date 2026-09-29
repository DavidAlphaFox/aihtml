%% @doc Demos of the progress_circle component (aihtml_progress_circle), shown on
%% /components/progress_circle. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
-module(aihtml_example_demo_progress_circle).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0]).
-export([circle_sizes/0, circle_colors/0, circle_states/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => progress_circle, title => <<"ProgressCircle">>,
       summary => <<"环形进度，适合放在卡片角上。"/utf8>>,
       demos => [{<<"尺寸"/utf8>>, circle_sizes},
                 {<<"颜色"/utf8>>, circle_colors},
                 {<<"标签、不确定、禁用"/utf8>>, circle_states}]}].

%%% ProgressCircle

-spec circle_sizes() -> aihtml:html().
circle_sizes() ->
    row([progress_circle(25, [sm], []),
         progress_circle(50, [], []),
         progress_circle(75, [lg], [])]).

-spec circle_colors() -> aihtml:html().
circle_colors() ->
    row([progress_circle(60, [Color], []) || Color <- [primary, success, warning, info, error]]).

-spec circle_states() -> aihtml:html().
circle_states() ->
    row([progress_circle(75, [lg, success], [{label, <<"Uploaded">>}]),
         progress_circle(60, [info], [{show_value, false}, {label, <<"No value">>}]),
         progress_circle(undefined, [indeterminate], [{label, <<"Working">>}]),
         progress_circle(30, [disabled], [])]).

%% Layout helpers of the demos.
row(Children) ->
    'div'(Children, [<<"flex flex-wrap items-center gap-4">>], []).
