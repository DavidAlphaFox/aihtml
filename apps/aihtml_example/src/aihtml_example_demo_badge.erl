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
    row([badge(avatar(<<"A">>, [square], []), [], [{count, 5}]),
         badge(avatar(<<"B">>, [square], []), [error], [{count, 120}]),
         badge(avatar(<<"C">>, [square], []), [info], [{count, 12}, {max, 9}]),
         badge(avatar(<<"D">>, [square], []), [success, show_zero], [{count, 0}])]).

-spec badge_status() -> aihtml:html().
badge_status() ->
    row([badge(avatar(<<"D">>, [square], []), [dot, warning], []),
         badge(avatar(<<"E">>, [], []), [online, circular, bottom], []),
         badge(avatar(<<"F">>, [], []), [busy, circular, bottom], []),
         badge(avatar(<<"G">>, [], []), [away, circular, bottom], []),
         badge(avatar(<<"H">>, [], []), [offline, circular, bottom], [])]).

-spec badge_corners() -> aihtml:html().
badge_corners() ->
    row([badge(avatar(<<"TR">>, [square], []), [], [{count, 1}]),
         badge(avatar(<<"TL">>, [square], []), [secondary, left], [{count, 2}]),
         badge(avatar(<<"BR">>, [square], []), [success, bottom], [{count, 3}]),
         badge(avatar(<<"BL">>, [square], []), [info, bottom, left], [{count, <<"new">>}])]).

-spec badge_standalone() -> aihtml:html().
badge_standalone() ->
    row([span([<<"Inbox ">>, badge(undefined, [], [{count, 42}])]),
         badge(undefined, [success], [{count, <<"beta">>}]),
         badge(undefined, [error], [{count, 1000}])]).

%% Layout helpers of the demos.
row(Children) ->
    'div'(Children, [<<"flex flex-wrap items-center gap-4">>], []).
