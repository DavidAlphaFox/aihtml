%% @doc Demos of empty (aihtml_empty), shown on /components/empty. Each
%% function is one example, written the way an application writes it;
%% the docs page prints its source under it.
-module(aihtml_example_demo_empty).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0]).
-export([empty_basic/0, empty_compact/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => empty, title => <<"Empty">>,
       summary => <<"没有内容时的占位：图标、标题、说明和操作。"/utf8>>,
       demos => [{<<"图标、说明与操作"/utf8>>, empty_basic},
                 {<<"紧凑"/utf8>>, empty_compact}]}].

-spec empty_basic() -> aihtml:html().
empty_basic() ->
    'div'(empty(button(<<"Create project">>, create, [], []), [],
                [{icon, <<"📭"/utf8>>}, {title, <<"No projects yet">>},
                 {description, <<"Projects you create will show up here.">>}]),
          [<<"w-80 border border-line rounded">>], []).

-spec empty_compact() -> aihtml:html().
empty_compact() ->
    'div'(empty([], [compact], [{title, <<"No results">>},
                                {description, <<"Try another search.">>}]),
          [<<"w-80 border border-line rounded">>], []).
