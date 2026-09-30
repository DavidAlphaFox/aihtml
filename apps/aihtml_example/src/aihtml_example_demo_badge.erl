%% @doc Demos of the badge component (aihtml_badge), shown on
%% /components/badge. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
-module(aihtml_example_demo_badge).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0]).
-export([badge_counts/0, badge_status/0, badge_corners/0, badge_standalone/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => badge, title => <<"Badge">>,
       summary => <<"挂在元素角上的计数、圆点或在线状态。"/utf8>>,
       demos => [{<<"计数与上限"/utf8>>, badge_counts},
                 {<<"圆点与在线状态"/utf8>>, badge_status},
                 {<<"角落位置"/utf8>>, badge_corners},
                 {<<"独立使用"/utf8>>, badge_standalone}]}].

%%% Badge

-spec badge_counts() -> aihtml:html().
badge_counts() ->
    row([ah_badge(ah_avatar(<<"A">>, [square], []), [], [{count, 5}]),
         ah_badge(ah_avatar(<<"B">>, [square], []), [error], [{count, 120}]),
         ah_badge(ah_avatar(<<"C">>, [square], []), [info], [{count, 12}, {max, 9}]),
         ah_badge(ah_avatar(<<"D">>, [square], []), [success, show_zero], [{count, 0}])]).

-spec badge_status() -> aihtml:html().
badge_status() ->
    row([ah_badge(ah_avatar(<<"D">>, [square], []), [dot, warning], []),
         ah_badge(ah_avatar(<<"E">>, [], []), [online, circular, bottom], []),
         ah_badge(ah_avatar(<<"F">>, [], []), [busy, circular, bottom], []),
         ah_badge(ah_avatar(<<"G">>, [], []), [away, circular, bottom], []),
         ah_badge(ah_avatar(<<"H">>, [], []), [offline, circular, bottom], [])]).

-spec badge_corners() -> aihtml:html().
badge_corners() ->
    row([ah_badge(ah_avatar(<<"TR">>, [square], []), [], [{count, 1}]),
         ah_badge(ah_avatar(<<"TL">>, [square], []), [secondary, left], [{count, 2}]),
         ah_badge(ah_avatar(<<"BR">>, [square], []), [success, bottom], [{count, 3}]),
         ah_badge(ah_avatar(<<"BL">>, [square], []), [info, bottom, left], [{count, <<"new">>}])]).

-spec badge_standalone() -> aihtml:html().
badge_standalone() ->
    row([ah_span([<<"Inbox ">>, ah_badge(undefined, [], [{count, 42}])]),
         ah_badge(undefined, [success], [{count, <<"beta">>}]),
         ah_badge(undefined, [error], [{count, 1000}])]).

%% Layout helpers of the demos.
row(Children) ->
    ah_div(Children, [<<"flex flex-wrap items-center gap-4">>], []).
