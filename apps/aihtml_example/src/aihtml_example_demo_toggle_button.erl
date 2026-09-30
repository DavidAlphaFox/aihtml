%% @doc Demos of toggle_button (aihtml_toggle_button), shown on
%% /components/toggle_button. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
-module(aihtml_example_demo_toggle_button).

-include_lib("aihtml/include/aihtml.hrl").

-import(aihtml_example_fixture_button, [row/1]).

-export([demos/0]).
-export([toggle_buttons/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => toggle_button, title => <<"ToggleButton">>,
       summary => <<"可以保持按下状态的按钮，值为 true 或 false。"/utf8>>,
       demos => [{<<"按下与弹起"/utf8>>, toggle_buttons}]}].

-spec toggle_buttons() -> aihtml:html().
toggle_buttons() ->
    row([ah_toggle_button(<<"Bold">>, false, [default], []),
         ah_toggle_button(<<"Italic">>, true, [default], []),
         ah_toggle_button(<<"Primary">>, true, [], []),
         ah_toggle_button(<<"Outlined">>, false, [outlined, round], []),
         ah_toggle_button(<<"Disabled">>, true, [secondary], [{disabled, true}])]).
