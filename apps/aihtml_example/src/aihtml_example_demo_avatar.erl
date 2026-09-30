%% @doc Demos of the avatar component (aihtml_avatar), shown on
%% /components/avatar. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
-module(aihtml_example_demo_avatar).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0]).
-export([avatar_sizes/0, avatar_shapes/0, avatar_images/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => avatar, title => <<"Avatar">>,
       summary => <<"头像，图片取不到时回落为首字母。"/utf8>>,
       demos => [{<<"尺寸"/utf8>>, avatar_sizes},
                 {<<"形状与颜色"/utf8>>, avatar_shapes},
                 {<<"图片与加载失败"/utf8>>, avatar_images}]}].

%%% Avatar

-spec avatar_sizes() -> aihtml:html().
avatar_sizes() ->
    row([ah_avatar(<<"SG">>, [sm], []),
         ah_avatar(<<"SG">>, [], []),
         ah_avatar(<<"SG">>, [lg], []),
         ah_avatar(<<"SG">>, [xl], [])]).

-spec avatar_shapes() -> aihtml:html().
avatar_shapes() ->
    row([ah_avatar(<<"AB">>, [square], []),
         ah_avatar(<<"CD">>, [rounded, success], []),
         ah_avatar(<<"EF">>, [warning], []),
         ah_avatar(<<"GH">>, [error], []),
         ah_avatar(<<"IJ">>, [info], []),
         ah_avatar(<<"KL">>, [secondary], []),
         ah_avatar(undefined, [], [])]).

-spec avatar_images() -> aihtml:html().
avatar_images() ->
    Photo = <<"data:image/svg+xml;utf8,<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 40 40'>"
              "<rect width='40' height='40' fill='%2360a5fa'/><circle cx='20' cy='16' r='7' fill='white'/>"
              "<rect x='8' y='26' width='24' height='14' rx='7' fill='white'/></svg>">>,
    row([ah_avatar(<<"IM">>, [lg], [{src, Photo}, {alt, <<"Jane">>}]),
         ah_avatar(<<"BR">>, [lg], [{src, <<"data:image/png;base64,AAAA">>}, {alt, <<"Broken">>}])]).

%% Layout helpers of the demos.
row(Children) ->
    ah_div(Children, [<<"flex flex-wrap items-center gap-4">>], []).
