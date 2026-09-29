%% @doc Demos of the tile layout (aihtml_tile_layout), shown on
%% /components/tile_layout. Each function is one example, written the way
%% an application writes it; the docs page prints its source under it.
%%
%% The module is also the action module of the server-driven demo: a tile
%% layout whose arrangement the server receives and can reset.
-module(aihtml_example_demo_tile_layout).
-behaviour(aihtml_action).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0, action/4]).
-export([tl_ide/0, tl_dashboard/0, tl_saved/0, tl_server/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => tile_layout, title => <<"TileLayout">>,
       summary => <<"IDE 式平铺布局：拖动分割条调整大小，把标签拖到其他面板或面板边缘重新排列。"/utf8>>,
       demos => [{<<"IDE 风格布局"/utf8>>, tl_ide},
                 {<<"仪表盘：平铺面板与最小尺寸"/utf8>>, tl_dashboard},
                 {<<"按保存的排列渲染"/utf8>>, tl_saved},
                 {<<"排列交给服务端保存"/utf8>>, tl_server}]}].

%%%===================================================================
%%% TileLayout
%%%===================================================================

-spec tl_ide() -> aihtml:html().
tl_ide() ->
    tile_layout(ide_layout(), undefined, [<<"border rounded">>], [{height, 420}]).

-spec tl_dashboard() -> aihtml:html().
tl_dashboard() ->
    tile_layout({rows, [#{columns => [#{id => sales, label => <<"销售额"/utf8>>, min => 120,
                                        content => kpi(<<"销售额"/utf8>>, <<"¥ 1,284,300"/utf8>>)},
                                      #{id => orders, label => <<"订单"/utf8>>, min => 120,
                                        content => kpi(<<"订单"/utf8>>, <<"3,942">>)},
                                      #{id => users, label => <<"活跃用户"/utf8>>, min => 120,
                                        content => kpi(<<"活跃用户"/utf8>>, <<"18,204">>)}],
                          size => 110, resize => false},
                        #{columns => [{tabs, [{trend, <<"趋势"/utf8>>, pane(<<"最近 30 天的销售趋势图"/utf8>>)},
                                              {region, <<"地区"/utf8>>, pane(<<"按地区汇总"/utf8>>)}]},
                                      #{id => todo, label => <<"待办"/utf8>>, size => <<"35%">>, min => 160,
                                        content => pane(<<"3 个待审批的退款"/utf8>>)}]}]},
                undefined, [<<"border rounded">>], [{height, 380}]).

%% A stored arrangement: the terminal moved next to the editor, the
%% outline tab closed.
-spec tl_saved() -> aihtml:html().
tl_saved() ->
    Saved = <<"{\"closed\":[\"outline\"],\"root\":{\"id\":\"root\",\"items\":["
              "{\"active\":\"search\",\"id\":\"left\",\"size\":\"22fr\",\"tabs\":[\"explorer\",\"search\"],\"type\":\"tabs\"},"
              "{\"active\":\"core\",\"id\":\"editors\",\"size\":\"48fr\",\"tabs\":[\"core\",\"tiles\"],\"type\":\"tabs\"},"
              "{\"active\":\"terminal\",\"id\":\"bottom\",\"size\":\"30fr\",\"tabs\":[\"terminal\",\"output\"],\"type\":\"tabs\"}"
              "],\"type\":\"columns\"}}">>,
    tile_layout(ide_layout(), Saved, [<<"border rounded">>], [{height, 320}]).

%% Every change posts the arrangement to action(arranged, ...); the button
%% runs action(reset, ...), which renders the layout afresh.
-spec tl_server() -> aihtml:html().
tl_server() ->
    'div'([button(<<"重置布局"/utf8>>, undefined, [outlined, sm],
                  [on(click, {?MODULE, reset, #{}})]),
           server_layout(),
           pre(<<"拖动标签或分割条后，这里显示服务端收到的排列"/utf8>>,
               [<<"text-xs text-muted whitespace-pre-wrap break-all m-0">>], [{id, <<"tl-state">>}])],
          [<<"flex flex-col gap-2 items-start">>], []).

server_layout() ->
    tile_layout({columns, [{tabs, [{a, <<"甲"/utf8>>, pane(<<"面板甲"/utf8>>)},
                                   {b, <<"乙"/utf8>>, pane(<<"面板乙"/utf8>>)}]},
                           {tabs, [{c, <<"丙"/utf8>>, pane(<<"面板丙"/utf8>>)},
                                   {d, <<"丁"/utf8>>, pane(<<"面板丁"/utf8>>)}]}]},
                undefined, [<<"border rounded w-full">>],
                [{id, <<"tl-server">>}, {height, 220}, on(change, {?MODULE, arranged, #{}})]).

ide_layout() ->
    #{id => root,
      columns => [#{id => left, size => <<"22%">>, min => 120,
                    tabs => [{explorer, <<"资源管理器"/utf8>>,
                              pane(<<"src/  components/  tile_layout.erl  core.erl"/utf8>>)},
                             {search, <<"搜索"/utf8>>,
                              input(undefined, [], [{placeholder, <<"搜索文件…"/utf8>>}])}]},
                  #{id => center,
                    rows => [#{id => editors, min => 100,
                               tabs => [{core, <<"core.erl">>, src_block(<<"-module(core).\n-export([main/0]).\n\nmain() -> ok.">>)},
                                        {tiles, <<"tiles.erl">>, src_block(<<"%% tile layout\n-module(tiles).">>)}]},
                             #{id => bottom, size => <<"35%">>, min => 80,
                               tabs => [{terminal, <<"终端"/utf8>>, src_block(<<"$ rebar3 compile\n===> Compiling aihtml">>)},
                                        {output, <<"输出"/utf8>>, pane(<<"构建成功，0 个警告"/utf8>>)}]}]},
                  #{id => right, size => <<"18%">>, min => 100,
                    tabs => [#{id => outline, label => <<"大纲"/utf8>>, close => false,
                               content => pane(<<"main/0"/utf8>>)}]}]}.

%%%===================================================================
%%% Actions
%%%===================================================================

-spec action(atom(), term(), aihtml_action:event(), aihtml_action:ctx()) -> ok.
action(arranged, _Args, #{value := Value}, Ctx) ->
    aihtml_action:html(Ctx, {id, <<"tl-state">>}, Value);
action(reset, _Args, _Event, Ctx) ->
    aihtml_action:html(Ctx, {id, <<"tl-server">>}, server_layout(), outer),
    aihtml_action:html(Ctx, {id, <<"tl-state">>}, <<"布局已重置"/utf8>>).

%%%===================================================================
%%% Helpers
%%%===================================================================

pane(T) ->
    'div'(T, [<<"p-3 text-sm text-muted">>], []).

src_block(T) ->
    pre(T, [<<"m-0 p-3 text-xs font-mono whitespace-pre text-muted">>], []).

kpi(Label, Value) ->
    'div'([span(Label, [<<"text-xs text-muted">>], []),
           span(Value, [<<"text-2xl font-bold">>], [])],
          [<<"flex flex-col gap-1 p-4">>], []).
