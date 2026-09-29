%% @doc Demos of the textarea (aihtml_textarea), shown on
%% /components/textarea. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
-module(aihtml_example_demo_textarea).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0]).
-export([textarea_basic/0, textarea_states/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => textarea, title => <<"Textarea">>,
       summary => <<"多行文本输入，样式与 Input 一致。"/utf8>>,
       demos => [{<<"基本用法"/utf8>>, textarea_basic},
                 {<<"尺寸、状态与浮动标签"/utf8>>, textarea_states}]}].

-spec textarea_basic() -> aihtml:html().
textarea_basic() ->
    row([textarea(undefined, [<<"w-80">>], [{name, notes},
                                            {placeholder, <<"Write something...">>}]),
         textarea(<<"Line one\nLine two">>, [<<"w-80">>], [{rows, 4}])]).

-spec textarea_states() -> aihtml:html().
textarea_states() ->
    row([textarea(undefined, [sm, <<"w-60">>], [{label, <<"Notes">>}]),
         textarea(<<"Too short">>, [invalid, <<"w-60">>], []),
         textarea(<<"Read only">>, [disabled, <<"w-60">>], [])]).

row(Children) ->
    'div'(Children, [<<"flex flex-wrap items-start gap-4">>], []).
