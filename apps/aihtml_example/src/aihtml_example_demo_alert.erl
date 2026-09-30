%% @doc Demos of the alert component (aihtml_alert), shown on
%% /components/alert. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
-module(aihtml_example_demo_alert).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0]).
-export([alert_variants/0, alert_dismissible/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => alert, title => <<"Alert">>,
       summary => <<"页面内的提示框，四种语气，可关闭。"/utf8>>,
       demos => [{<<"四种变体"/utf8>>, alert_variants},
                 {<<"标题与关闭按钮"/utf8>>, alert_dismissible}]}].

%%% Alert

-spec alert_variants() -> aihtml:html().
alert_variants() ->
    stack([ah_alert(<<"A new version is available.">>, [], []),
           ah_alert(<<"Your changes were saved.">>, [success], []),
           ah_alert(<<"Your trial ends in 3 days.">>, [warning], []),
           ah_alert(<<"Could not reach the server.">>, [error], []),
           ah_alert(<<"No icon, plain message.">>, [], [{icon, false}])]).

-spec alert_dismissible() -> aihtml:html().
alert_dismissible() ->
    stack([ah_alert(<<"Your changes were saved.">>, [success, dismissible], [{title, <<"Saved">>}]),
           ah_alert([<<"Could not reach the server. ">>, ah_a(<<"Retry">>, [<<"underline">>], [{href, <<"#">>}])],
                    [error, dismissible], [{title, <<"Connection failed">>}])]).

%% Layout helpers of the demos.
stack(Children) ->
    ah_div(Children, [<<"flex flex-col gap-3">>], []).
