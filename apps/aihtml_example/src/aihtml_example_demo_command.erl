%% @doc Demos of the command palette (aihtml_command), shown on
%% /components/command. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
%%
%% The module is also the action module of the server-driven demos: the
%% command palette that reports its choice and the palette with
%% server-side search.
-module(aihtml_example_demo_command).
-behaviour(aihtml_action).

-include_lib("aihtml/include/aihtml.hrl").

-import(aihtml_example_fixture_layout_bars, [icon/1]).

-export([demos/0, action/4]).
-export([cmd_inline/0, cmd_palette/0, cmd_select/0, cmd_search/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => command, title => <<"Command">>,
       summary => <<"命令面板：输入过滤、分组、快捷键提示和键盘导航，可作为 ⌘K 浮层。"/utf8>>,
       demos => [{<<"内嵌面板"/utf8>>, cmd_inline},
                 {<<"⌘K 浮层"/utf8>>, cmd_palette},
                 {<<"选中后通知服务端"/utf8>>, cmd_select},
                 {<<"服务端搜索"/utf8>>, cmd_search}]}].

%%%===================================================================
%%% Command
%%%===================================================================

-spec cmd_inline() -> aihtml:html().
cmd_inline() ->
    ah_command(commands(), [<<"max-w-md">>], [{placeholder, <<"输入命令或搜索…"/utf8>>},
                                              {empty_text, <<"没有匹配的命令"/utf8>>}]).

%% Ctrl+K / ⌘K or the button opens the palette.
-spec cmd_palette() -> aihtml:html().
cmd_palette() ->
    ah_div([ah_button(<<"打开命令面板"/utf8>>, undefined, [outlined],
                      [{onclick, <<"AH.invoke('#cmd-palette', 'open')">>}]),
            ah_span([<<"或按 "/utf8>>, ah_kbd(<<"Ctrl K">>, [], [])], [<<"text-sm text-muted">>], []),
            ah_command(commands(), [palette], [{id, <<"cmd-palette">>}, {hotkey, <<"k">>}])],
           [<<"flex items-center gap-3">>], []).

%% Choosing a command runs action(ran, ...) below with its value.
-spec cmd_select() -> aihtml:html().
cmd_select() ->
    ah_div([ah_command([#{value => deploy, label => <<"部署到生产环境"/utf8>>, shortcut => <<"⌘D"/utf8>>},
                        #{value => rollback, label => <<"回滚上一版本"/utf8>>},
                        #{value => logs, label => <<"查看日志"/utf8>>, description => <<"最近 1 小时"/utf8>>}],
                       [<<"max-w-sm">>], [on('ah:select', {?MODULE, ran, #{}})]),
            ah_span(<<"还没有执行命令"/utf8>>, [<<"text-sm text-muted">>], [{id, <<"cmd-ran">>}])],
           [<<"flex flex-col gap-2">>], []).

%% Typing calls action(search, ...) below, which answers with
%% set_command_items.
-spec cmd_search() -> aihtml:html().
cmd_search() ->
    ah_command([], [<<"max-w-sm">>], [{placeholder, <<"搜索城市，如 an"/utf8>>},
                                      {empty_text, <<"输入后开始搜索"/utf8>>},
                                      {search, {?MODULE, search, #{}}}]).

%%%===================================================================
%%% Actions
%%%===================================================================

-spec action(atom(), term(), aihtml_action:event(), aihtml_action:ctx()) -> ok.
action(ran, _Args, #{value := Cmd}, Ctx) ->
    aihtml_action:html(Ctx, {id, <<"cmd-ran">>}, [<<"服务端执行了："/utf8>>, Cmd]);
action(search, _Args, #{value := Query} = Event, Ctx) ->
    Q = string:lowercase(Query),
    Found = [#{value => C, label => C, description => Country}
             || {C, Country} <- cities(), Q =/= <<>>,
                string:find(string:lowercase(C), Q) =/= nomatch],
    set_command_items(Ctx, Event, [#{heading => <<"城市"/utf8>>, items => lists:sublist(Found, 8)}]).

%%%===================================================================
%%% Data
%%%===================================================================

commands() ->
    [#{heading => <<"文件"/utf8>>,
       items => [#{value => new, label => <<"新建文件"/utf8>>, icon => icon(files),
                   shortcut => <<"⌘N"/utf8>>},
                 #{value => open, label => <<"打开…"/utf8>>, icon => icon(box),
                   shortcut => <<"⌘O"/utf8>>},
                 #{value => save_all, label => <<"全部保存"/utf8>>, disabled => true}]},
     #{heading => <<"视图"/utf8>>,
       items => [#{value => theme, label => <<"切换深色主题"/utf8>>, icon => icon(settings),
                   description => <<"在浅色和深色之间切换"/utf8>>},
                 #{value => sidebar, label => <<"显示侧栏"/utf8>>, icon => icon(list),
                   shortcut => <<"⌘B"/utf8>>},
                 #{value => search, label => <<"在文件中搜索"/utf8>>, icon => icon(search),
                   shortcut => <<"⇧⌘F"/utf8>>}]}].

cities() ->
    [{<<"Amsterdam">>, <<"Netherlands">>}, {<<"Athens">>, <<"Greece">>},
     {<<"Bangkok">>, <<"Thailand">>}, {<<"Beijing">>, <<"China">>},
     {<<"Dalian">>, <<"China">>}, {<<"Istanbul">>, <<"Türkiye"/utf8>>},
     {<<"Jakarta">>, <<"Indonesia">>}, {<<"London">>, <<"United Kingdom">>},
     {<<"Madrid">>, <<"Spain">>}, {<<"Nanjing">>, <<"China">>},
     {<<"Santiago">>, <<"Chile">>}, {<<"Shanghai">>, <<"China">>},
     {<<"Tokyo">>, <<"Japan">>}, {<<"Vienna">>, <<"Austria">>}].
