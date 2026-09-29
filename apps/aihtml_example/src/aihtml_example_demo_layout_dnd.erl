%% @doc Demos of the drag and drop components (aihtml_layout_dnd), shown on
%% /components/<name>. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
%%
%% The module is also the action module of the demos that report to the
%% server: reordered/4 receives a sortable's new order, dropped/4 a drop.
-module(aihtml_example_demo_layout_dnd).

-include_lib("aihtml/include/aihtml.hrl").

-behaviour(aihtml_action).

-export([demos/0, action/4]).
-export([sortable_basic/0, sortable_handle/0, sortable_grid/0, sortable_connected/0,
         sortable_save/0, sortable_record/0, sortable_disabled/0,
         dragdrop_basic/0, dragdrop_accept/0, dragdrop_server/0, dragdrop_record/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => sortable, title => <<"Sortable">>,
       summary => <<"拖拽或用键盘重排的列表，值是条目键的顺序。"/utf8>>,
       demos => [{<<"拖拽排序"/utf8>>, sortable_basic},
                 {<<"拖拽手柄"/utf8>>, sortable_handle},
                 {<<"横排与网格"/utf8>>, sortable_grid},
                 {<<"关联列表"/utf8>>, sortable_connected},
                 {<<"放下后保存顺序"/utf8>>, sortable_save},
                 {<<"禁用"/utf8>>, sortable_disabled},
                 {<<"record 写法"/utf8>>, sortable_record}]},
     #{component => dragdrop, title => <<"DragDrop">>,
       summary => <<"把条目拖到放置区，放下时触发 ah:drop 事件。"/utf8>>,
       demos => [{<<"拖到放置区"/utf8>>, dragdrop_basic},
                 {<<"按类型接收"/utf8>>, dragdrop_accept},
                 {<<"服务端处理放置"/utf8>>, dragdrop_server},
                 {<<"record 写法"/utf8>>, dragdrop_record}]}].

%%%===================================================================
%%% Sortable
%%%===================================================================

-spec sortable_basic() -> aihtml:html().
sortable_basic() ->
    sortable([{design, <<"设计首页"/utf8>>}, {api, <<"接口联调"/utf8>>},
              {test, <<"编写测试"/utf8>>}, {docs, <<"更新文档"/utf8>>},
              {release, <<"发布上线"/utf8>>}],
             undefined, [<<"max-w-xs">>], [{name, tasks}]).

-spec sortable_handle() -> aihtml:html().
sortable_handle() ->
    sortable([{name, <<"姓名"/utf8>>}, {email, <<"邮箱"/utf8>>},
              {phone, <<"电话"/utf8>>}, {city, <<"城市"/utf8>>}],
             [email, name], [handle, <<"max-w-xs">>], [{name, columns}]).

-spec sortable_grid() -> aihtml:html().
sortable_grid() ->
    Tile = fun(N, Color) ->
                   {N, integer_to_binary(N),
                    [{class, <<"justify-center h-20 text-lg font-semibold text-white border-0">>},
                     {style, <<"background:var(--ah-color-", Color/binary, ")">>}]}
           end,
    Colors = [<<"primary">>, <<"success">>, <<"warning">>, <<"error">>, <<"info">>,
              <<"secondary">>],
    'div'([sortable([{T, T} || T <- [<<"Erlang">>, <<"Elixir">>, <<"Gleam">>, <<"LFE">>]],
                    undefined, [horizontal], []),
           sortable([Tile(N, lists:nth((N - 1) rem 6 + 1, Colors)) || N <- lists:seq(1, 9)],
                    undefined, [grid, <<"max-w-md">>], [])],
          [<<"flex flex-col gap-6">>], []).

-spec sortable_connected() -> aihtml:html().
sortable_connected() ->
    Column = fun(Title, Items) ->
                     'div'([h4(Title, [<<"text-sm font-semibold mb-2">>], []),
                            sortable(Items, undefined,
                                     [<<"p-2 rounded-md border border-dashed border-line min-h-32">>],
                                     [{group, board}])],
                           [<<"flex-1">>], [])
             end,
    'div'([Column(<<"待办"/utf8>>, [{login, <<"登录页"/utf8>>}, {search, <<"搜索"/utf8>>},
                                     {export, <<"导出报表"/utf8>>}]),
           Column(<<"进行中"/utf8>>, [{cart, <<"购物车"/utf8>>}]),
           Column(<<"已完成"/utf8>>, [])],
          [<<"flex gap-4 max-w-2xl">>], []).

%% A drop that changes the order runs action(reordered, ...) below.
-spec sortable_save() -> aihtml:html().
sortable_save() ->
    'div'([sortable([{p1, <<"高优先级"/utf8>>}, {p2, <<"中优先级"/utf8>>},
                     {p3, <<"低优先级"/utf8>>}],
                    undefined, [<<"max-w-xs">>],
                    [on(change, {?MODULE, reordered, #{}})]),
           p(<<"拖动或按空格拿起条目后再按方向键。"/utf8>>, [<<"text-sm opacity-70">>],
             [{id, <<"dnd-order">>}])],
          [<<"flex flex-col gap-3">>], []).

-spec sortable_disabled() -> aihtml:html().
sortable_disabled() ->
    sortable([{a, <<"不能拖动"/utf8>>}, {b, <<"顺序固定"/utf8>>}],
             undefined, [<<"max-w-xs">>], [{disabled, true}]).

%% The same component as a record; the postback runs action(reordered, ...).
-spec sortable_record() -> aihtml:html().
sortable_record() ->
    'div'([#ah_sortable{items = [{north, <<"北区"/utf8>>}, {south, <<"南区"/utf8>>},
                                 {east, <<"东区"/utf8>>}, {west, <<"西区"/utf8>>}],
                        value = [east, west], orientation = horizontal, handle = true,
                        name = regions, postback = {reordered, #{target => record}}},
           p(<<"顺序：east,west,north,south"/utf8>>, [<<"text-sm opacity-70">>],
             [{id, <<"dnd-order-record">>}])],
          [<<"flex flex-col gap-3">>], []).

%%%===================================================================
%%% DragDrop
%%%===================================================================

-spec dragdrop_basic() -> aihtml:html().
dragdrop_basic() ->
    Item = fun(Key, Label) ->
                   'div'(Label, [<<"px-4 py-2 rounded-md text-sm text-white">>],
                         [draggable_attrs(Key, #{}),
                          {style, <<"background:var(--ah-color-primary)">>}])
           end,
    dragdrop('div'([
                 'div'([Item(a, <<"项目 A"/utf8>>), Item(b, <<"项目 B"/utf8>>),
                        Item(c, <<"项目 C"/utf8>>)],
                       [<<"flex flex-col gap-2 w-32">>], [drop_zone_attrs(shelf, #{})]),
                 'div'(<<"拖放至此"/utf8>>,
                       [<<"flex flex-col gap-2 items-center justify-center w-52 min-h-36 p-2 "
                          "rounded-md border-2 border-dashed border-line text-sm opacity-80">>],
                       [drop_zone_attrs(box, #{})])],
                   [<<"flex gap-6 items-start">>], []),
             [], [{move, true}, {revert, true}]).

-spec dragdrop_accept() -> aihtml:html().
dragdrop_accept() ->
    Card = fun(Key, Type, Label) ->
                   'div'(Label, [<<"px-3 py-1.5 rounded border border-line text-sm">>],
                         [draggable_attrs(Key, #{type => Type})])
           end,
    Zone = fun(Zone, Accept, Title) ->
                   'div'([h4(Title, [<<"text-xs font-semibold opacity-70">>], [])],
                         [<<"flex flex-col gap-2 w-40 min-h-32 p-2 rounded-md border border-line">>],
                         [drop_zone_attrs(Zone, #{accept => Accept}),
                          {aria_label, Title}])
           end,
    dragdrop('div'([
                 'div'([Card(f1, feature, <<"特性：导出"/utf8>>),
                        Card(b1, bug, <<"缺陷：崩溃"/utf8>>),
                        Card(f2, feature, <<"特性：分享"/utf8>>),
                        Card(b2, bug, <<"缺陷：乱码"/utf8>>)],
                       [<<"flex flex-col gap-2 w-40">>], []),
                 Zone(features, [feature], <<"只收特性"/utf8>>),
                 Zone(bugs, [bug], <<"只收缺陷"/utf8>>)],
                   [<<"flex gap-4 items-start">>], []),
             [], [{move, true}, {tolerance, pointer}]).

%% Each drop runs action(dropped, ...) below with Event.data.
-spec dragdrop_server() -> aihtml:html().
dragdrop_server() ->
    Files = [{<<"report.pdf">>, <<"📄 report.pdf"/utf8>>},
             {<<"photo.jpg">>, <<"🖼 photo.jpg"/utf8>>}],
    dragdrop('div'([
                 'div'(['div'(Label, [<<"px-3 py-1.5 rounded border border-line text-sm">>],
                              [draggable_attrs(Key, #{})]) || {Key, Label} <- Files],
                       [<<"flex flex-col gap-2">>], []),
                 'div'(<<"🗑 回收站"/utf8>>,
                       [<<"flex items-center justify-center w-40 h-24 rounded-md border-2 "
                          "border-dashed border-line">>],
                       [drop_zone_attrs(trash, #{})]),
                 span(<<"还没有放置"/utf8>>, [<<"text-sm opacity-70">>],
                      [{id, <<"dnd-dropped">>}])],
                   [<<"flex gap-6 items-center">>], []),
             [], [{revert, true}, on('ah:drop', {?MODULE, dropped, #{}})]).

%% The same component as a record; the postback runs action(dropped, ...).
-spec dragdrop_record() -> aihtml:html().
dragdrop_record() ->
    Seat = fun(N) ->
                   'div'(<<"座位 "/utf8, (integer_to_binary(N))/binary>>,
                         [<<"flex items-center justify-center h-16 rounded-md border border-line text-xs">>],
                         [drop_zone_attrs(N, #{})])
           end,
    #ah_dragdrop{
       body = 'div'(['div'(<<"张三"/utf8>>,
                           [<<"px-3 py-1.5 rounded-full border border-line text-sm w-fit">>],
                           [draggable_attrs(zhang, #{})]),
                     'div'([Seat(N) || N <- lists:seq(1, 4)],
                           [<<"grid grid-cols-4 gap-2 max-w-md">>], []),
                     span(<<"把张三拖到座位上"/utf8>>, [<<"text-sm opacity-70">>],
                          [{id, <<"dnd-dropped-record">>}])],
                    [<<"flex flex-col gap-3">>], []),
       tolerance = pointer, move = true, postback = {dropped, #{target => record}}}.

%%%===================================================================
%%% Actions
%%%===================================================================

-spec action(atom(), term(), aihtml_action:event(), aihtml_action:ctx()) -> ok.
action(reordered, Args, #{value := Order}, Ctx) ->
    Target = case Args of
                 #{target := record} -> <<"dnd-order-record">>;
                 _ -> <<"dnd-order">>
             end,
    aihtml_action:html(Ctx, {id, Target}, [<<"服务端收到新顺序："/utf8>>, Order]);
action(dropped, Args, #{data := Data}, Ctx) ->
    Target = case Args of
                 #{target := record} -> <<"dnd-dropped-record">>;
                 _ -> <<"dnd-dropped">>
             end,
    aihtml_action:html(Ctx, {id, Target},
                       [<<"服务端收到："/utf8>>, maps:get(<<"drag">>, Data, <<>>),
                        <<" → "/utf8>>, maps:get(<<"drop">>, Data, <<>>),
                        <<"（来自 "/utf8>>, maps:get(<<"from">>, Data, <<>>), <<"）"/utf8>>]).
