%% @doc Demos of panel (aihtml_panel), shown on /components/panel. Each
%% function is one example, written the way an application writes it;
%% the docs page prints its source under it.
-module(aihtml_example_demo_panel).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0]).
-export([panel_scroll/0, panel_header/0, panel_collapsed/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => panel, title => <<"Panel">>,
       summary => <<"可滚动的内容面板，可带标题栏、操作和折叠按钮。"/utf8>>,
       demos => [{<<"固定高度的滚动区"/utf8>>, panel_scroll},
                 {<<"标题栏、操作与折叠"/utf8>>, panel_header},
                 {<<"初始折叠"/utf8>>, panel_collapsed}]}].

-spec panel_scroll() -> aihtml:html().
panel_scroll() ->
    'div'(panel([p(<<"Log line ", (integer_to_binary(I))/binary>>) || I <- lists:seq(1, 20)],
                [bordered, <<"p-2">>], [{height, 160}]),
          [<<"w-80">>], []).

-spec panel_header() -> aihtml:html().
panel_header() ->
    'div'(panel([p(<<"Deployed ", (integer_to_binary(I))/binary, " minutes ago">>)
                 || I <- lists:seq(1, 12)],
                [bordered],
                [{title, <<"Activity">>}, {actions, button(<<"Refresh">>, refresh, [outlined, sm], [])},
                 {collapsible, true}, {max_height, 180}]),
          [<<"w-80">>], []).

-spec panel_collapsed() -> aihtml:html().
panel_collapsed() ->
    'div'(panel(p(<<"Advanced settings go here.">>), [bordered],
                [{title, <<"Advanced">>}, {collapsible, true}, {collapsed, true}]),
          [<<"w-80">>], []).
