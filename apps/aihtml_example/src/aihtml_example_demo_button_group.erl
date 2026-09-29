%% @doc Demos of button_group (aihtml_button_group), shown on
%% /components/button_group. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
-module(aihtml_example_demo_button_group).

-include_lib("aihtml/include/aihtml.hrl").

-import(aihtml_example_fixture_button, [row/1]).

-export([demos/0]).
-export([group_modes/0, group_layouts/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => button_group, title => <<"ButtonGroup">>,
       summary => <<"连在一起的一组按钮，可作单选或多选。"/utf8>>,
       demos => [{<<"默认、单选、多选"/utf8>>, group_modes},
                 {<<"竖排、填充、禁用"/utf8>>, group_layouts}]}].

-spec group_modes() -> aihtml:html().
group_modes() ->
    Views = [{list, <<"List">>}, {grid, <<"Grid">>}, {board, <<"Board">>}],
    row([button_group([<<"Left">>, <<"Middle">>, <<"Right">>], undefined, [], []),
         button_group(Views, grid, [radio], [{name, view}]),
         button_group([{b, <<"B">>}, {i, <<"I">>}, {u, <<"U">>}], [b, u], [checkbox, square], [])]).

-spec group_layouts() -> aihtml:html().
group_layouts() ->
    Views = [{list, <<"List">>}, {grid, <<"Grid">>}, {board, <<"Board">>}],
    row([button_group(Views, list, [radio, vertical], []),
         button_group(Views, board, [radio, filled], []),
         button_group(Views, list, [radio, outlined, square], []),
         button_group([{a, <<"Enabled">>}, {b, <<"Off">>, [{disabled, true}]}, {c, <<"On">>}],
                      c, [radio], []),
         button_group(Views, grid, [radio], [{disabled, true}])]).
