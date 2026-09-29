%% @doc Demos of the StatusBar component (aihtml_status_bar), shown on
%% /components/status_bar. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
-module(aihtml_example_demo_status_bar).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0]).
-export([status_editor/0, status_segments/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => status_bar, title => <<"StatusBar">>,
       summary => <<"窗口底部的状态条，左右两组分段。"/utf8>>,
       demos => [{<<"字数统计与保存状态"/utf8>>, status_editor},
                 {<<"自定义分段"/utf8>>, status_segments}]}].

%%%-------------------------------------------------------------------
%%% status_bar
%%%-------------------------------------------------------------------

-spec status_editor() -> aihtml:html().
status_editor() ->
    status_bar([<<"Ln 12, Col 4">>,
                #{content => <<"UTF-8">>, align => right},
                #{content => <<"Markdown">>, align => right}],
               [], [{content, <<"Hello 世界，这是一段中英混排文本。\n\n第二段。"/utf8>>},
                    {dirty, false}]).

-spec status_segments() -> aihtml:html().
status_segments() ->
    status_bar([#{count => 3, label => <<"errors">>,
                  details => [{<<"Errors">>, 3}, {<<"Warnings">>, 7}]},
                <<"main">>,
                #{content => <<"Spaces: 2">>, align => right}],
               [], [{dirty, true}, {labels, #{unsaved => <<"Modified">>}}]).
