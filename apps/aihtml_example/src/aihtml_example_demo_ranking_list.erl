%% @doc Demos of the ranking_list component (aihtml_ranking_list), shown on
%% /components/ranking_list. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
-module(aihtml_example_demo_ranking_list).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0]).
-export([ranking_basic/0, ranking_dense/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => ranking_list, title => <<"RankingList">>,
       summary => <<"带名次、国旗和标签的排行榜。"/utf8>>,
       demos => [{<<"国旗、标签、奖牌色"/utf8>>, ranking_basic},
                 {<<"紧凑、可点击、前 N 名"/utf8>>, ranking_dense}]}].

%%% RankingList

-spec ranking_basic() -> aihtml:html().
ranking_basic() ->
    ranking_list([#{name => <<"Germany">>, code => de, value => <<"12,300">>,
                    sub_value => <<"+4%">>, tag => <<"Free">>},
                  #{name => <<"United States">>, code => us, value => <<"9,870">>, tag => <<"Paid">>},
                  #{name => <<"Japan">>, code => jp, value => <<"7,450">>, secondary => <<"Asia">>,
                    tag => <<"Progress">>},
                  #{name => <<"Brazil">>, code => br, value => <<"3,120">>, tag => <<"Out of date">>}],
                 [<<"max-w-lg">>], [{title, <<"Top countries">>}]).

-spec ranking_dense() -> aihtml:html().
ranking_dense() ->
    ranking_list([#{name => <<"Erlang">>, value => 98, tag => <<"BEAM">>},
                  #{name => <<"Elixir">>, value => 95, tag => <<"BEAM">>},
                  #{name => <<"Gleam">>, value => 90, tag => <<"New">>},
                  #{name => <<"LFE">>, value => 70}],
                 [dense, clickable, <<"max-w-lg">>],
                 [{title, <<"Top 3">>}, {max_items, 3},
                  {tag_colors, #{<<"BEAM">> => success, <<"New">> => warning}}]).
