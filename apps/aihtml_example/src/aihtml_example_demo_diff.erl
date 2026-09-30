%% @doc Demos of the diff view (aihtml_diff), shown on /components/diff.
%% Each function is one example, written the way an application writes it;
%% the docs page prints its source under it.
-module(aihtml_example_demo_diff).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0]).
-export([diff_unified/0, diff_split/0, diff_word/0, diff_plain/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => diff, title => <<"Diff">>,
       summary => <<"两段文本的差异视图，由服务端计算，支持单栏、左右分栏和词级对比。"/utf8>>,
       demos => [{<<"单栏、行号与统计"/utf8>>, diff_unified},
                 {<<"左右分栏"/utf8>>, diff_split},
                 {<<"词级差异"/utf8>>, diff_word},
                 {<<"最简用法"/utf8>>, diff_plain}]}].

%%%===================================================================
%%% Diff
%%%===================================================================

-spec diff_unified() -> aihtml:html().
diff_unified() ->
    ah_diff(old_code(), new_code(), [line_numbers, stats], []).

-spec diff_split() -> aihtml:html().
diff_split() ->
    ah_diff(old_code(), new_code(), [split, stats], []).

-spec diff_word() -> aihtml:html().
diff_word() ->
    ah_diff(<<"The quick brown fox jumps over the lazy dog. 今天天气晴朗，适合出门散步。"/utf8>>,
            <<"The quick red fox leaps over the sleepy dog. 今天天气多云，适合在家读书。"/utf8>>,
            [word], []).

-spec diff_plain() -> aihtml:html().
diff_plain() ->
    ah_diff(<<"apple\nbanana\ncherry\n">>, <<"apple\nblueberry\ncherry\ndate\n">>, [], []).

%%%===================================================================
%%% Data
%%%===================================================================

old_code() ->
    <<"function greet(name) {\n"
      "  console.log('hi')\n"
      "  return name\n"
      "}\n"
      "\n"
      "export default greet\n">>.

new_code() ->
    <<"function greet(name) {\n"
      "  console.log('hello', name)\n"
      "  return name.trim()\n"
      "}\n"
      "\n"
      "greet.version = 2\n"
      "export default greet\n">>.
