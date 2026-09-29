%% @doc Demos of docking (aihtml_docking), shown on /components/docking.
%% Each function is one example, written the way an application writes
%% it; the docs page prints its source under it.
%%
%% The module is also the action module of the demos that talk to the
%% server: the saved layout and the window added from an action.
-module(aihtml_example_demo_docking).
-behaviour(aihtml_action).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0, action/4]).
-export([dk_basic/0, dk_vertical/0, dk_floating/0, dk_saved/0, dk_add/0, dk_record/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => docking, title => <<"Docking">>,
       summary => <<"可在面板之间拖动、折叠、关闭和浮动的窗口组，布局可保存到服务端。"/utf8>>,
       demos => [{<<"在两个面板之间拖动窗口"/utf8>>, dk_basic},
                 {<<"纵向排列、固定与折叠"/utf8>>, dk_vertical},
                 {<<"浮动窗口，禁止浮动"/utf8>>, dk_floating},
                 {<<"布局变化回传服务端"/utf8>>, dk_saved},
                 {<<"服务端添加窗口"/utf8>>, dk_add},
                 {<<"record 写法"/utf8>>, dk_record}]}].

%%%===================================================================
%%% Docking
%%%===================================================================

-spec dk_basic() -> aihtml:html().
dk_basic() ->
    docking([{inbox, [{mail, <<"收件箱"/utf8>>, p(<<"拖动标题栏，把我移到右边的面板。"/utf8>>)},
                      {tasks, <<"待办"/utf8>>, p(<<"3 项未完成"/utf8>>)}]},
             {side, [{calendar, <<"日历"/utf8>>, p(<<"今天没有会议。"/utf8>>)},
                     {notes, <<"便签"/utf8>>, p(<<"周五前提交报告。"/utf8>>)}]}],
            [<<"h-80">>], []).

-spec dk_vertical() -> aihtml:html().
dk_vertical() ->
    docking([{top, [{a, <<"固定窗口（不能拖动）"/utf8>>, p(<<"pinned"/utf8>>), #{pinned => true}},
                    {b, <<"已折叠"/utf8>>, p(<<"展开后可见"/utf8>>), #{collapsed => true}}]},
             {bottom, [{c, <<"窗口 C"/utf8>>, p(<<"Alt+方向键可用键盘移动"/utf8>>)}]}],
            [vertical, <<"h-80">>], [{offset, 8}]).

-spec dk_floating() -> aihtml:html().
dk_floating() ->
    'div'([docking([{left, [{log, <<"日志"/utf8>>, p(<<"拖到面板外会浮动"/utf8>>)}]},
                    {right, [{tip, <<"浮动窗口"/utf8>>, p(<<"floating"/utf8>>),
                              #{floating => {60, 120, 220}}}]}],
                   [<<"h-64">>], []),
           docking([{left, [{x, <<"不允许浮动"/utf8>>, p(<<"拖到面板外会回到原处"/utf8>>)}]},
                    {right, []}],
                   [<<"h-40">>], [{allow_float, false}, {collapse_buttons, false}])],
          [<<"flex flex-col gap-4">>], []).

-spec dk_saved() -> aihtml:html().
dk_saved() ->
    Saved = <<"{\"panels\":[{\"id\":\"l\",\"windows\":[\"w2\"]},{\"id\":\"r\",\"windows\":[\"w1\"]}],"
              "\"collapsed\":[\"w1\"],\"closed\":[\"w3\"]}">>,
    'div'([docking([{l, [{w1, <<"甲"/utf8>>, p(<<"1">>)}, {w2, <<"乙"/utf8>>, p(<<"2">>)}]},
                    {r, [{w3, <<"丙（已关闭）"/utf8>>, p(<<"3">>)}]}],
                   [<<"h-56">>],
                   [{layout, Saved}, {name, layout}, on(change, {?MODULE, docking_saved, #{}})]),
           pre(<<"拖动、折叠或关闭窗口后，服务端收到的布局显示在这里。"/utf8>>,
               [<<"text-xs text-muted whitespace-pre-wrap">>], [{id, <<"docking-saved">>}])],
          [<<"flex flex-col gap-3">>], []).

-spec dk_add() -> aihtml:html().
dk_add() ->
    'div'([button(<<"添加窗口"/utf8>>, add, [], [on(click, {?MODULE, docking_add, #{}})]),
           docking([{main, [{first, <<"第一个"/utf8>>, p(<<"服务端渲染的内容"/utf8>>)}]},
                    {more, []}],
                  [<<"h-64 w-full">>], [{id, <<"docking-add">>}])],
          [<<"flex flex-col gap-3 items-start">>], []).

%% The same component as a record: options are checked field names, and
%% the postback runs action(docking_saved, ...) below on change.
-spec dk_record() -> aihtml:html().
dk_record() ->
    #ah_docking{items = [{a, [{r1, <<"Record A">>, p(<<"a">>)}]},
                         {b, [{r2, <<"Record B">>, p(<<"b">>)}]}],
                orientation = horizontal, offset = 4, drag_opacity = 0.6,
                labels = #{collapse => <<"收起"/utf8>>, close => <<"关闭"/utf8>>},
                css = [<<"h-48">>], postback = docking_saved}.

%%%===================================================================
%%% Actions
%%%===================================================================

-spec action(atom(), term(), aihtml_action:event(), aihtml_action:ctx()) -> ok.
action(docking_saved, _Args, #{value := Json}, Ctx) ->
    aihtml_action:html(Ctx, {id, <<"docking-saved">>}, Json);
action(docking_add, _Args, _Event, Ctx) ->
    N = erlang:unique_integer([positive]),
    docking_add_window(
      Ctx, {id, <<"docking-add">>}, more,
      {<<"w", (integer_to_binary(N))/binary>>, <<"新窗口 "/utf8, (integer_to_binary(N))/binary>>,
       p(<<"由 action 在服务端渲染"/utf8>>)}).
