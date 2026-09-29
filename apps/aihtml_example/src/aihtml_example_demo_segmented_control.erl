%% @doc Demos of segmented_control (aihtml_segmented_control), shown on
%% /components/segmented_control. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
-module(aihtml_example_demo_segmented_control).

-include_lib("aihtml/include/aihtml.hrl").

-import(aihtml_example_fixture_button, [row/1]).

-export([demos/0]).
-export([segmented/0, segmented_full/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => segmented_control, title => <<"SegmentedControl">>,
       summary => <<"互斥的分段选择。"/utf8>>,
       demos => [{<<"尺寸"/utf8>>, segmented},
                 {<<"占满宽度、禁用项"/utf8>>, segmented_full}]}].

-spec segmented() -> aihtml:html().
segmented() ->
    Views = [{list, <<"List">>}, {grid, <<"Grid">>}, {board, <<"Board">>}],
    row([segmented_control(Views, list, [sm], []),
         segmented_control(Views, grid, [], [{name, layout}]),
         segmented_control(Views, board, [lg], [])]).

-spec segmented_full() -> aihtml:html().
segmented_full() ->
    'div'([segmented_control([{day, <<"Day">>}, {week, <<"Week">>, [{disabled, true}]},
                              {month, <<"Month">>}], day, [full_width], []),
           segmented_control([{a, <<"A">>}, {b, <<"B">>}], a, [], [{disabled, true}])],
          [<<"flex flex-col gap-3">>], []).
