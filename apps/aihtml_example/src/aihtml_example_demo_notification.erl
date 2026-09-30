%% @doc Demos of the notification (aihtml_notification), shown on
%% /components/notification. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it. The
%% server-driven demos call action/4 below.
-module(aihtml_example_demo_notification).
-behaviour(aihtml_action).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0, action/4]).
-export([notification_variants/0, notification_sticky/0, notify_from_server/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => notification, title => <<"Notification">>,
       summary => <<"服务端渲染的通知模板，每次打开克隆一张卡片堆叠在角落。"/utf8>>,
       demos => [{<<"四种语义"/utf8>>, notification_variants},
                 {<<"不自动关闭、右下角"/utf8>>, notification_sticky},
                 {<<"在 action 里调用 notify/2"/utf8>>, notify_from_server}]}].

%%%===================================================================
%%% Notification
%%%===================================================================

-spec notification_variants() -> aihtml:html().
notification_variants() ->
    row([ah_button(<<"Info">>, undefined, [info], opens({id, <<"note-info">>})),
         ah_button(<<"Success">>, undefined, [success], opens({id, <<"note-success">>})),
         ah_button(<<"Warning">>, undefined, [warning], opens({id, <<"note-warning">>})),
         ah_button(<<"Error">>, undefined, [error], opens({id, <<"note-error">>})),
         ah_notification(<<"A new version is available.">>, [info], [{id, <<"note-info">>}]),
         ah_notification([ah_strong(<<"Upload complete. ">>), <<"3 files were added.">>],
                         [success], [{id, <<"note-success">>}]),
         ah_notification(<<"Your session expires in 5 minutes.">>, [warning],
                         [{id, <<"note-warning">>}]),
         ah_notification(<<"The server could not be reached.">>, [error],
                         [{id, <<"note-error">>}])]).

-spec notification_sticky() -> aihtml:html().
notification_sticky() ->
    row([ah_button(<<"Notify">>, undefined, [outlined], opens({id, <<"note-sticky">>})),
         ah_button(<<"Close all">>, undefined, [default], closes({id, <<"note-sticky">>})),
         ah_notification(<<"Stays until you close it.">>, [warning, bottom_right],
                         [{id, <<"note-sticky">>}, {auto_close, false},
                          {close_on_click, false}, {width, 320}])]).

-spec notify_from_server() -> aihtml:html().
notify_from_server() ->
    row([ah_button(<<"Import on the server">>, undefined, [primary],
                   on(click, {?MODULE, import, #{}}))]).

%%%===================================================================
%%% Actions
%%%===================================================================

-spec action(atom(), map(), aihtml_action:event(), aihtml_action:ctx()) -> ok.
action(import, _, _Ev, Ctx) ->
    aihtml_notification:notify(Ctx, #{content => [ah_strong(<<"Import finished. ">>),
                                                  <<"128 rows added.">>],
                                      variant => success, position => bottom_right}).

%%%===================================================================
%%% Helpers
%%%===================================================================

row(Children) ->
    ah_div(Children, [<<"flex flex-wrap items-center gap-3">>], []).
