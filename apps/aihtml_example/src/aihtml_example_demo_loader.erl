%% @doc Demos of loader (aihtml_loader), shown on /components/loader. Each
%% function is one example, written the way an application writes it;
%% the docs page prints its source under it.
-module(aihtml_example_demo_loader).

-include_lib("aihtml/include/aihtml.hrl").

-import(aihtml_example_fixture_layout, [row/1, frame/1]).

-export([demos/0]).
-export([loader_overlay/0, loader_positions/0, loader_inline/0, loader_hidden/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => loader, title => <<"Loader">>,
       summary => <<"转圈的加载指示，默认盖住所在的容器。"/utf8>>,
       demos => [{<<"遮罩容器"/utf8>>, loader_overlay},
                 {<<"文字位置"/utf8>>, loader_positions},
                 {<<"行内"/utf8>>, loader_inline},
                 {<<"先隐藏，用方法显示"/utf8>>, loader_hidden}]}].

-spec loader_overlay() -> aihtml:html().
loader_overlay() ->
    frame([p(<<"Refreshing the report…"/utf8>>), loader([], [])]).

-spec loader_positions() -> aihtml:html().
loader_positions() ->
    row([frame([loader([top], [{text, <<"Top">>}])]),
         frame([loader([left], [{text, <<"Left">>}])]),
         frame([loader([right], [{text, <<"Saving">>}])])]).

-spec loader_inline() -> aihtml:html().
loader_inline() ->
    row([loader([inline], []), loader([inline, right], [{text, <<"Syncing">>}])]).

-spec loader_hidden() -> aihtml:html().
loader_hidden() ->
    frame([p(<<"Shown by AH.invoke(el, 'show') or a server call.">>),
           loader([hidden], [{id, <<"report-loader">>}])]).
