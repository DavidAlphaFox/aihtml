%% @doc Demos of the toasts (aihtml_toast), shown on
%% /components/toast. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it. The
%% server-driven demos call action/4 below.
-module(aihtml_example_demo_toast).
-behaviour(aihtml_action).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0, action/4]).
-export([toast_variants/0, toast_options/0, toast_from_server/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => toast, title => <<"Toast">>,
       summary => <<"角落里弹出的简短消息，一行代码调用，自动消失。"/utf8>>,
       demos => [{<<"四种语义"/utf8>>, toast_variants},
                 {<<"常驻与位置"/utf8>>, toast_options},
                 {<<"在 action 里调用 toast/3"/utf8>>, toast_from_server}]}].

%%%===================================================================
%%% Toast
%%%===================================================================

-spec toast_variants() -> aihtml:html().
toast_variants() ->
    row([button(<<"Info">>, undefined, [info],
                shows_toast(<<"Heads up">>, #{description => <<"Something happened.">>})),
         button(<<"Success">>, undefined, [success],
                shows_toast(<<"Saved">>, #{variant => success,
                                           description => <<"Your changes are live.">>})),
         button(<<"Warning">>, undefined, [warning],
                shows_toast(<<"Disk almost full">>, #{variant => warning})),
         button(<<"Error">>, undefined, [error],
                shows_toast(<<"Upload failed">>, #{variant => error}))]).

-spec toast_options() -> aihtml:html().
toast_options() ->
    row([button(<<"Sticky">>, undefined, [outlined],
                shows_toast(<<"Stays until closed">>, #{duration => 0})),
         button(<<"Bottom left">>, undefined, [outlined],
                shows_toast(<<"Bottom left">>, #{position => bottom_left})),
         button(<<"Bottom right, 1 s">>, undefined, [outlined],
                shows_toast(<<"Quick one">>, #{position => bottom_right, duration => 1000}))]).

-spec toast_from_server() -> aihtml:html().
toast_from_server() ->
    row([button(<<"Save on the server">>, undefined, [primary],
                on(click, {?MODULE, save, #{}}))]).

%%%===================================================================
%%% Actions
%%%===================================================================

-spec action(atom(), map(), aihtml_action:event(), aihtml_action:ctx()) -> ok.
action(save, _, _Ev, Ctx) ->
    toast(Ctx, <<"Saved on the server">>,
          #{variant => success, description => <<"Rendered by toast/3.">>}).

%%%===================================================================
%%% Helpers
%%%===================================================================

row(Children) ->
    'div'(Children, [<<"flex flex-wrap items-center gap-3">>], []).
