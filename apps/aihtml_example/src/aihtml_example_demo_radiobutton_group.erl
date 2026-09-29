%% @doc Demos of the radio button group (aihtml_radiobutton_group), shown on
%% /components/radiobutton_group. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
-module(aihtml_example_demo_radiobutton_group).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0]).
-export([radio_group_vertical/0, radio_group_layouts/0, radio_group_disabled/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => radiobutton_group, title => <<"RadioButtonGroup">>,
       summary => <<"一组单选按钮，方向键切换选中项。"/utf8>>,
       demos => [{<<"竖排"/utf8>>, radio_group_vertical},
                 {<<"横排、标签在前"/utf8>>, radio_group_layouts},
                 {<<"整组禁用"/utf8>>, radio_group_disabled}]}].

-spec radio_group_vertical() -> aihtml:html().
radio_group_vertical() ->
    radiobutton_group([{standard, <<"Standard shipping">>}, {express, <<"Express">>},
                       {pickup, <<"Pick up in store">>},
                       {drone, <<"Drone (coming soon)">>, [{disabled, true}]}],
                      express, [], [{name, shipping}]).

-spec radio_group_layouts() -> aihtml:html().
radio_group_layouts() ->
    Sizes = [{s, <<"S">>}, {m, <<"M">>}, {l, <<"L">>}, {xl, <<"XL">>}],
    'div'([radiobutton_group(Sizes, m, [horizontal], [{name, size}]),
           radiobutton_group(Sizes, l, [horizontal, label_before, lg], [{name, size2}])],
          [<<"flex flex-col gap-4">>], []).

-spec radio_group_disabled() -> aihtml:html().
radio_group_disabled() ->
    radiobutton_group([{yes, <<"Yes">>}, {no, <<"No">>}], yes, [horizontal],
                      [{name, agree}, {disabled, true}]).
