%% @doc Demos of link_button (aihtml_link_button), shown on
%% /components/link_button. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
-module(aihtml_example_demo_link_button).

-include_lib("aihtml/include/aihtml.hrl").

-import(aihtml_example_fixture_button, [row/1]).

-export([demos/0]).
-export([link_buttons/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => link_button, title => <<"LinkButton">>,
       summary => <<"外观是按钮的链接。"/utf8>>,
       demos => [{<<"看起来像按钮的链接"/utf8>>, link_buttons}]}].

-spec link_buttons() -> aihtml:html().
link_buttons() ->
    row([link_button(<<"Primary link">>, <<"#top">>, [], []),
         link_button(<<"Outlined">>, <<"#top">>, [outlined, round], []),
         link_button(<<"Small borderless">>, <<"#top">>, [borderless, sm], []),
         link_button(<<"Disabled">>, <<"#top">>, [secondary], [{disabled, true}])]).
