%% @doc Demos of the sheet (aihtml_sheet), shown on
%% /components/sheet. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it. The
%% server-driven demos call action/4 below.
-module(aihtml_example_demo_sheet).
-behaviour(aihtml_action).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0, action/4]).
-export([sheet_form/0, sheet_sides/0, sheet_from_server/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => sheet, title => <<"Sheet">>,
       summary => <<"从侧边滑入的模态面板，带遮罩、滚动锁和焦点陷阱。"/utf8>>,
       demos => [{<<"表单面板"/utf8>>, sheet_form},
                 {<<"四个方向"/utf8>>, sheet_sides},
                 {<<"由服务端打开"/utf8>>, sheet_from_server}]}].

%%%===================================================================
%%% Sheet
%%%===================================================================

-spec sheet_form() -> aihtml:html().
sheet_form() ->
    ah_div([ah_button(<<"Edit profile">>, undefined, [primary], opens({id, <<"sheet-profile">>})),
            ah_sheet([ah_input(undefined, [], [{name, name}, {placeholder, <<"Name">>}]),
                      ah_input(undefined, [<<"mt-3">>], [{name, email}, {placeholder, <<"Email">>}])],
                     [], [{id, <<"sheet-profile">>}, {title, <<"Edit profile">>},
                          {description, <<"Changes are saved when you click Save.">>},
                          {footer, [ah_button(<<"Cancel">>, undefined, [default], closes()),
                                    ah_button(<<"Save">>, undefined, [primary], closes(closest, save))]}])]).

-spec sheet_sides() -> aihtml:html().
sheet_sides() ->
    row([ah_button(atom_to_binary(Side), undefined, [outlined],
                   opens({id, <<"sheet-", (atom_to_binary(Side))/binary>>}))
         || Side <- [left, top, bottom]]
        ++ [ah_sheet(<<"Slides in from the ", (atom_to_binary(Side))/binary, ".">>, [Side],
                     [{id, <<"sheet-", (atom_to_binary(Side))/binary>>},
                      {title, atom_to_binary(Side)}, {size, 240}])
            || Side <- [left, top, bottom]]).

-spec sheet_from_server() -> aihtml:html().
sheet_from_server() ->
    ah_div([ah_button(<<"Ask the server">>, undefined, [primary],
                      on(click, {?MODULE, open, #{target => <<"sheet-server">>}})),
            ah_sheet(ah_p(<<"The server opened this sheet with aihtml_lib_overlay:open/2.">>),
                     [], [{id, <<"sheet-server">>}, {title, <<"Opened by an action">>}])]).

%%%===================================================================
%%% Actions
%%%===================================================================

-spec action(atom(), map(), aihtml_action:event(), aihtml_action:ctx()) -> ok.
action(open, #{target := Id}, _Ev, Ctx) ->
    aihtml_lib_overlay:open(Ctx, {id, Id}).

%%%===================================================================
%%% Helpers
%%%===================================================================

row(Children) ->
    ah_div(Children, [<<"flex flex-wrap items-center gap-3">>], []).
