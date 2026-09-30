%% @doc Demos of the window (aihtml_window), shown on
%% /components/window. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it. The
%% server-driven demos call action/4 below.
-module(aihtml_example_demo_window).
-behaviour(aihtml_action).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0, action/4]).
-export([window_dialog/0, window_tool/0, window_from_server/0, window_record/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => window, title => <<"Window">>,
       summary => <<"可拖拽、可调整大小的窗口，也可作模态对话框。"/utf8>>,
       demos => [{<<"模态对话框"/utf8>>, window_dialog},
                 {<<"工具窗口：折叠、拖拽、缩放"/utf8>>, window_tool},
                 {<<"由服务端打开"/utf8>>, window_from_server},
                 {<<"record 写法"/utf8>>, window_record}]}].

%%%===================================================================
%%% Window
%%%===================================================================

-spec window_dialog() -> aihtml:html().
window_dialog() ->
    ah_div([ah_button(<<"Delete file">>, undefined, [error], opens({id, <<"win-confirm">>})),
            ah_window(ah_p(<<"Delete report.pdf? This cannot be undone.">>),
                      [], [{id, <<"win-confirm">>}, {title, <<"Confirm delete">>},
                           {modal, true}, {width, 380}, {resizable, false},
                           {footer, [ah_button(<<"Cancel">>, undefined, [default],
                                               closes(closest, cancel)),
                                     ah_button(<<"Delete">>, undefined, [error],
                                               closes(closest, ok))]}])]).

-spec window_tool() -> aihtml:html().
window_tool() ->
    ah_div([ah_button(<<"Open tool window">>, undefined, [outlined], toggles({id, <<"win-tool">>})),
            ah_window(ah_p(<<"Drag the title bar, resize from the edges, collapse with the arrow.">>),
                      [], [{id, <<"win-tool">>}, {title, <<"Properties">>}, {width, 320},
                           {collapsible, true}])]).

-spec window_from_server() -> aihtml:html().
window_from_server() ->
    ah_div([ah_button(<<"Load and open">>, undefined, [primary],
                      on(click, {?MODULE, open, #{target => <<"win-server">>}})),
            ah_window(ah_p(<<"Opened with aihtml_lib_overlay:open/2 from an action.">>),
                      [], [{id, <<"win-server">>}, {title, <<"From the server">>}, {width, 360}])]).

-spec window_record() -> aihtml:html().
window_record() ->
    ah_div([ah_button(<<"Rename file">>, undefined, [outlined], opens({id, <<"win-rename">>})),
            #ah_window{id = <<"win-rename">>, title = <<"Rename">>, modal = true, width = 360,
                       resizable = false,
                       body = ah_input(<<"report.pdf">>, [], [{name, file_name}]),
                       footer = [ah_button(<<"Cancel">>, undefined, [default], closes()),
                                 ah_button(<<"Rename">>, undefined, [primary],
                                           closes(closest, ok))],
                       %% fires when the window closes, handled by action/4 below
                       postback = window_closed}]).

%%%===================================================================
%%% Actions
%%%===================================================================

-spec action(atom(), map(), aihtml_action:event(), aihtml_action:ctx()) -> ok.
action(open, #{target := Id}, _Ev, Ctx) ->
    aihtml_lib_overlay:open(Ctx, {id, Id});
action(window_closed, _, _Ev, Ctx) ->
    ah_toast(Ctx, <<"The window was closed">>, #{description => <<"Sent by its postback.">>}).
