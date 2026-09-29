%% @doc Demos of tab_bar (aihtml_tab_bar), shown on /components/tab_bar. Each
%% function is one example, written the way an application writes it;
%% the docs page prints its source under it.
-module(aihtml_example_demo_tab_bar).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0]).
-export([tab_bar_editor/0, tab_bar_fixed/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => tab_bar, title => <<"TabBar">>,
       summary => <<"编辑器式可关闭的标签条，值为当前标签的 id。"/utf8>>,
       demos => [{<<"可关闭、未保存标记与图标"/utf8>>, tab_bar_editor},
                 {<<"不可关闭"/utf8>>, tab_bar_fixed}]}].

-spec tab_bar_editor() -> aihtml:html().
tab_bar_editor() ->
    tab_bar([{index, <<"index.erl">>}, {core, <<"core.erl">>, #{dirty => true}},
             {readme, <<"README.md">>, #{icon => <<"📄"/utf8>>}}, {config, <<"rebar.config">>}],
            core, [], []).

-spec tab_bar_fixed() -> aihtml:html().
tab_bar_fixed() ->
    tab_bar([{one, <<"One">>}, {two, <<"Two">>}, {three, <<"Three">>}], one, [],
            [{closable, false}]).
