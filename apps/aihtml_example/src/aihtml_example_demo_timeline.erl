%% @doc Demos of the timeline component (aihtml_timeline), shown on
%% /components/timeline. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
-module(aihtml_example_demo_timeline).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0]).
-export([timeline_both/0, timeline_near/0, timeline_horizontal/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => timeline, title => <<"Timeline">>,
       summary => <<"按时间排列的事件轴，卡片可展开。"/utf8>>,
       demos => [{<<"两侧交替"/utf8>>, timeline_both},
                 {<<"单侧"/utf8>>, timeline_near},
                 {<<"横向"/utf8>>, timeline_horizontal}]}].

%%% Timeline

-spec timeline_both() -> aihtml:html().
timeline_both() ->
    timeline([#{date => <<"2026-01">>, title => <<"Project start">>, subtitle => <<"Kick-off">>,
                description => <<"Scope agreed, team formed.">>},
              #{date => <<"2026-03">>, title => <<"Alpha">>, dot => success, expanded => true,
                description => <<"First internal release.">>},
              #{date => <<"2026-06">>, title => <<"Beta">>, dot => warning},
              #{date => <<"2026-09">>, title => <<"Launch">>, subtitle => <<"GA">>, dot => danger}],
             [], []).

-spec timeline_near() -> aihtml:html().
timeline_near() ->
    timeline([#{date => <<"09:12">>, title => <<"Order placed">>},
              #{date => <<"09:40">>, title => <<"Paid">>, dot => success},
              #{date => <<"14:05">>, title => <<"Shipped">>,
                description => <<"Parcel 4711 handed to the carrier.">>}],
             [near], []).

-spec timeline_horizontal() -> aihtml:html().
timeline_horizontal() ->
    timeline([#{date => <<"Q1">>, title => <<"Design">>},
              #{date => <<"Q2">>, title => <<"Build">>, dot => success},
              #{date => <<"Q3">>, title => <<"Test">>, dot => warning},
              #{date => <<"Q4">>, title => <<"Ship">>, dot => danger}],
             [horizontal], [{collapsible, false}]).
