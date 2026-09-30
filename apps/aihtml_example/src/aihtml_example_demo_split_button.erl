%% @doc Demos of split_button (aihtml_split_button), shown on
%% /components/split_button. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
-module(aihtml_example_demo_split_button).

-include_lib("aihtml/include/aihtml.hrl").

-import(aihtml_example_fixture_button, [row/1, menu/0]).

-export([demos/0]).
-export([split/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => split_button, title => <<"SplitButton">>,
       summary => <<"主操作按钮加一个展开更多操作的箭头。"/utf8>>,
       demos => [{<<"主操作加菜单"/utf8>>, split}]}].

-spec split() -> aihtml:html().
split() ->
    row([ah_split_button(<<"Save">>, menu(), [], [{menu_align, start}]),
         ah_split_button(<<"Secondary">>, menu(), [secondary], []),
         ah_split_button(<<"Small">>, menu(), [success, sm], []),
         ah_split_button(<<"Large">>, menu(), [error, lg], []),
         ah_split_button(<<"Outlined">>, menu(), [outlined], []),
         ah_split_button(<<"Disabled">>, menu(), [info], [{disabled, true}])]).
