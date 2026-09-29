%% @doc Demos of the checkbox group (aihtml_checkbox_group), shown on
%% /components/checkbox_group. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
-module(aihtml_example_demo_checkbox_group).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0]).
-export([checkbox_group_vertical/0, checkbox_group_layouts/0, checkbox_group_disabled/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => checkbox_group, title => <<"CheckboxGroup">>,
       summary => <<"一组复选框，值是选中项的列表。"/utf8>>,
       demos => [{<<"竖排"/utf8>>, checkbox_group_vertical},
                 {<<"横排、标签在前"/utf8>>, checkbox_group_layouts},
                 {<<"整组禁用"/utf8>>, checkbox_group_disabled}]}].

-spec checkbox_group_vertical() -> aihtml:html().
checkbox_group_vertical() ->
    checkbox_group([{apple, <<"Apple">>}, {pear, <<"Pear">>}, {plum, <<"Plum">>},
                    {fig, <<"Fig (sold out)">>, [{disabled, true}]}],
                   [apple, plum], [], [{name, fruit}]).

-spec checkbox_group_layouts() -> aihtml:html().
checkbox_group_layouts() ->
    Days = [{mon, <<"Mon">>}, {tue, <<"Tue">>}, {wed, <<"Wed">>}, {thu, <<"Thu">>}, {fri, <<"Fri">>}],
    'div'([checkbox_group(Days, [mon, wed], [horizontal], [{name, days}]),
           checkbox_group(Days, [fri], [horizontal, label_before, sm], [{name, days2}])],
          [<<"flex flex-col gap-4">>], []).

-spec checkbox_group_disabled() -> aihtml:html().
checkbox_group_disabled() ->
    checkbox_group([{read, <<"Read">>}, {write, <<"Write">>}, {admin, <<"Admin">>}],
                   [read], [horizontal], [{name, perms}, {disabled, true}]).
