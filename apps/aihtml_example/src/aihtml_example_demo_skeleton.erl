%% @doc Demos of skeleton (aihtml_skeleton), shown on /components/skeleton. Each
%% function is one example, written the way an application writes it;
%% the docs page prints its source under it.
-module(aihtml_example_demo_skeleton).

-include_lib("aihtml/include/aihtml.hrl").

-import(aihtml_example_fixture_layout, [row/1, box/1]).

-export([demos/0]).
-export([skeleton_text/0, skeleton_shapes/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => skeleton, title => <<"Skeleton">>,
       summary => <<"内容加载中的占位骨架。"/utf8>>,
       demos => [{<<"文本行"/utf8>>, skeleton_text},
                 {<<"圆形与矩形"/utf8>>, skeleton_shapes}]}].

-spec skeleton_text() -> aihtml:html().
skeleton_text() ->
    row([box(ah_skeleton([], [])), box(ah_skeleton([text, static], [{lines, 2}]))]).

-spec skeleton_shapes() -> aihtml:html().
skeleton_shapes() ->
    row([ah_skeleton([circle], [{width, 48}]),
         box(ah_skeleton([rect], [{height, 80}])),
         box(ah_skeleton([rect], [{height, 80}, {radius, 0}]))]).
