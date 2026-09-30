%% @doc Demos of the switch button (aihtml_switch_button), shown on
%% /components/switch_button. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
-module(aihtml_example_demo_switch_button).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0]).
-export([switch_basic/0, switch_labels/0, switch_sizes/0, switch_records/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => switch_button, title => <<"SwitchButton">>,
       summary => <<"滑动开关，开或关两种状态。"/utf8>>,
       demos => [{<<"开与关"/utf8>>, switch_basic},
                 {<<"轨道文字"/utf8>>, switch_labels},
                 {<<"尺寸与自定义大小"/utf8>>, switch_sizes},
                 {<<"record 写法"/utf8>>, switch_records}]}].

-spec switch_basic() -> aihtml:html().
switch_basic() ->
    row([ah_switch_button(<<"Wi-Fi">>, undefined, [], [{name, wifi}, {checked, true}]),
         ah_switch_button(<<"Bluetooth">>, undefined, [], [{name, bluetooth}]),
         ah_switch_button(<<"Disabled">>, undefined, [], [{disabled, true}]),
         ah_switch_button(<<"Locked">>, undefined, [], [{locked, true}, {checked, true}])]).

-spec switch_labels() -> aihtml:html().
switch_labels() ->
    row([ah_switch_button(<<"Notifications">>, undefined, [],
                          [{on_label, <<"On">>}, {off_label, <<"Off">>}, {checked, true}]),
         ah_switch_button(<<"Auto save">>, undefined, [],
                          [{on_label, <<"是"/utf8>>}, {off_label, <<"否"/utf8>>}])]).

-spec switch_sizes() -> aihtml:html().
switch_sizes() ->
    row([ah_switch_button(<<"Small">>, undefined, [sm], [{checked, true}]),
         ah_switch_button(<<"Medium">>, undefined, [md], [{checked, true}]),
         ah_switch_button(<<"Large">>, undefined, [lg], [{checked, true}]),
         ah_switch_button(<<"80 x 32">>, undefined, [], [{width, 80}, {height, 32}])]).

-spec switch_records() -> aihtml:html().
switch_records() ->
    row([#ah_switch_button{body = <<"Dark mode">>, checked = true,
                           on_label = <<"On">>, off_label = <<"Off">>,
                           attrs = [{name, dark}]},
         #ah_switch_button{body = <<"Large">>, size = lg, width = 70, height = 34},
         #ah_switch_button{body = <<"Locked">>, checked = true, locked = true}]).

row(Children) ->
    ah_div(Children, [<<"flex flex-wrap items-center gap-6">>], []).
