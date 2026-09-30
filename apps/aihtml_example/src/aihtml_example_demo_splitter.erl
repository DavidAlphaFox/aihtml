%% @doc Demos of the Splitter component (aihtml_splitter), shown on
%% /components/splitter. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
-module(aihtml_example_demo_splitter).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0]).
-export([splitter_columns/0, splitter_rows/0, splitter_nested/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => splitter, title => <<"Splitter">>,
       summary => <<"两个面板之间可拖动的分割条，值是两边的百分比。"/utf8>>,
       demos => [{<<"左右分割"/utf8>>, splitter_columns},
                 {<<"上下分割"/utf8>>, splitter_rows},
                 {<<"嵌套"/utf8>>, splitter_nested}]}].

%%%-------------------------------------------------------------------
%%% splitter
%%%-------------------------------------------------------------------

-spec splitter_columns() -> aihtml:html().
splitter_columns() ->
    ah_div(ah_splitter([#{content => pane(<<"Left, at least 80px">>), size => <<"30%">>, min => 80},
                        #{content => pane(<<"Right">>), min => 80}],
                       [], [{name, split}]),
           [<<"h-40 border border-line rounded">>], []).

-spec splitter_rows() -> aihtml:html().
splitter_rows() ->
    ah_div(ah_splitter([#{content => pane(<<"Editor">>), size => <<"65%">>, min => 40},
                        pane(<<"Console">>)],
                       [horizontal], [{splitbar_size, 6}]),
           [<<"h-56 border border-line rounded">>], []).

-spec splitter_nested() -> aihtml:html().
splitter_nested() ->
    ah_div(ah_splitter([#{content => pane(<<"Tree">>), size => 160, min => 100},
                        ah_splitter([pane(<<"Code">>), pane(<<"Preview">>)], [horizontal], [])],
                       [], []),
           [<<"h-56 border border-line rounded">>], []).

%% Shared by the demos above.
pane(Text) ->
    ah_div(Text, [<<"p-3 text-sm">>], []).
