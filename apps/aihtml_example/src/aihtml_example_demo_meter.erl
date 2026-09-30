%% @doc Demos of the meter component (aihtml_meter), shown on
%% /components/meter. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
-module(aihtml_example_demo_meter).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0]).
-export([meter_states/0, meter_sizes/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => meter, title => <<"Meter">>,
       summary => <<"带阈值分档的量表：偏低、正常、偏高。"/utf8>>,
       demos => [{<<"三种状态"/utf8>>, meter_states},
                 {<<"尺寸与说明文字"/utf8>>, meter_sizes}]}].

%%% Meter

-spec meter_states() -> aihtml:html().
meter_states() ->
    Zones = [{low, 25}, {high, 75}, {show_value, true}],
    stack([ah_meter(15, [], [{label, <<"Low">>} | Zones]),
           ah_meter(62, [], [{label, <<"Normal">>} | Zones]),
           ah_meter(88, [], [{label, <<"High">>} | Zones])]).

-spec meter_sizes() -> aihtml:html().
meter_sizes() ->
    stack([ah_meter(40, [sm], [{label, <<"Disk">>}]),
           ah_meter(15, [], [{low, 25}, {high, 75}, {optimum, 90}, {label, <<"Battery">>},
                             {show_value, true}, {helper_text, <<"Low: charge soon">>}]),
           ah_meter(700, [lg], [{max, 1000}, {label, <<"Memory (MB)">>}, {show_value, true}])]).

%% Layout helpers of the demos.
stack(Children) ->
    ah_div(Children, [<<"flex flex-col gap-3">>], []).
