%% @doc Demos of the star rating (aihtml_rating_group), shown on
%% /components/rating_group. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
-module(aihtml_example_demo_rating_group).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0]).
-export([rating_basic/0, rating_half/0, rating_sizes/0, rating_readonly/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => rating_group, title => <<"RatingGroup">>,
       summary => <<"星级评分，支持半星、悬停预览和只读。"/utf8>>,
       demos => [{<<"评分"/utf8>>, rating_basic},
                 {<<"半星"/utf8>>, rating_half},
                 {<<"尺寸与颜色"/utf8>>, rating_sizes},
                 {<<"只读与禁用"/utf8>>, rating_readonly}]}].

-spec rating_basic() -> aihtml:html().
rating_basic() ->
    rating_group(5, 3, [], [{name, stars}]).

-spec rating_half() -> aihtml:html().
rating_half() ->
    rating_group(5, 2.5, [], [{name, score}, {precision, 0.5}]).

-spec rating_sizes() -> aihtml:html().
rating_sizes() ->
    row([rating_group(5, 4, [sm], []),
         rating_group(5, 4, [md, primary], []),
         rating_group(5, 4, [lg, success], []),
         rating_group(10, 7, [sm, error], [])]).

-spec rating_readonly() -> aihtml:html().
rating_readonly() ->
    row([rating_group(5, 3.5, [], [{readonly, true}, {precision, 0.5}]),
         rating_group(5, 2, [], [{disabled, true}])]).

row(Children) ->
    'div'(Children, [<<"flex flex-wrap items-center gap-6">>], []).
