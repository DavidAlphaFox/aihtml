%% @doc Demos of drag and drop (aihtml_dragdrop), shown on
%% /components/dragdrop. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
%%
%% The module is also the action module of the demos that report to the
%% server: dropped/4 receives a drop.
-module(aihtml_example_demo_dragdrop).

-include_lib("aihtml/include/aihtml.hrl").

-behaviour(aihtml_action).

-export([demos/0, action/4]).
-export([dragdrop_basic/0, dragdrop_accept/0, dragdrop_server/0, dragdrop_record/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => dragdrop, title => <<"DragDrop">>,
       summary => <<"把条目拖到放置区，放下时触发 ah:drop 事件。"/utf8>>,
       demos => [{<<"拖到放置区"/utf8>>, dragdrop_basic},
                 {<<"按类型接收"/utf8>>, dragdrop_accept},
                 {<<"服务端处理放置"/utf8>>, dragdrop_server},
                 {<<"record 写法"/utf8>>, dragdrop_record}]}].

%%%===================================================================
%%% DragDrop
%%%===================================================================

-spec dragdrop_basic() -> aihtml:html().
dragdrop_basic() ->
    Item = fun(Key, Label) ->
                   ah_div(Label, [<<"px-4 py-2 rounded-md text-sm text-white">>],
                          [draggable_attrs(Key, #{}),
                           {style, <<"background:var(--ah-color-primary)">>}])
           end,
    ah_dragdrop(ah_div([
                     ah_div([Item(a, <<"项目 A"/utf8>>), Item(b, <<"项目 B"/utf8>>),
                             Item(c, <<"项目 C"/utf8>>)],
                            [<<"flex flex-col gap-2 w-32">>], [drop_zone_attrs(shelf, #{})]),
                     ah_div(<<"拖放至此"/utf8>>,
                            [<<"flex flex-col gap-2 items-center justify-center w-52 min-h-36 p-2 "
                               "rounded-md border-2 border-dashed border-line text-sm opacity-80">>],
                            [drop_zone_attrs(box, #{})])],
                       [<<"flex gap-6 items-start">>], []),
                [], [{move, true}, {revert, true}]).

-spec dragdrop_accept() -> aihtml:html().
dragdrop_accept() ->
    Card = fun(Key, Type, Label) ->
                   ah_div(Label, [<<"px-3 py-1.5 rounded border border-line text-sm">>],
                          [draggable_attrs(Key, #{type => Type})])
           end,
    Zone = fun(Zone, Accept, Title) ->
                   ah_div([ah_h4(Title, [<<"text-xs font-semibold opacity-70">>], [])],
                          [<<"flex flex-col gap-2 w-40 min-h-32 p-2 rounded-md border border-line">>],
                          [drop_zone_attrs(Zone, #{accept => Accept}),
                           {aria_label, Title}])
           end,
    ah_dragdrop(ah_div([
                     ah_div([Card(f1, feature, <<"特性：导出"/utf8>>),
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
    ah_dragdrop(ah_div([
                     ah_div([ah_div(Label, [<<"px-3 py-1.5 rounded border border-line text-sm">>],
                                    [draggable_attrs(Key, #{})]) || {Key, Label} <- Files],
                            [<<"flex flex-col gap-2">>], []),
                     ah_div(<<"🗑 回收站"/utf8>>,
                            [<<"flex items-center justify-center w-40 h-24 rounded-md border-2 "
                               "border-dashed border-line">>],
                            [drop_zone_attrs(trash, #{})]),
                     ah_span(<<"还没有放置"/utf8>>, [<<"text-sm opacity-70">>],
                             [{id, <<"dnd-dropped">>}])],
                       [<<"flex gap-6 items-center">>], []),
                [], [{revert, true}, on('ah:drop', {?MODULE, dropped, #{}})]).

%% The same component as a record; the postback runs action(dropped, ...).
-spec dragdrop_record() -> aihtml:html().
dragdrop_record() ->
    Seat = fun(N) ->
                   ah_div(<<"座位 "/utf8, (integer_to_binary(N))/binary>>,
                          [<<"flex items-center justify-center h-16 rounded-md border border-line text-xs">>],
                          [drop_zone_attrs(N, #{})])
           end,
    #ah_dragdrop{
       body = ah_div([ah_div(<<"张三"/utf8>>,
                             [<<"px-3 py-1.5 rounded-full border border-line text-sm w-fit">>],
                             [draggable_attrs(zhang, #{})]),
                      ah_div([Seat(N) || N <- lists:seq(1, 4)],
                             [<<"grid grid-cols-4 gap-2 max-w-md">>], []),
                      ah_span(<<"把张三拖到座位上"/utf8>>, [<<"text-sm opacity-70">>],
                              [{id, <<"dnd-dropped-record">>}])],
                     [<<"flex flex-col gap-3">>], []),
       tolerance = pointer, move = true, postback = {dropped, #{target => record}}}.

%%%===================================================================
%%% Actions
%%%===================================================================

-spec action(atom(), term(), aihtml_action:event(), aihtml_action:ctx()) -> ok.
action(dropped, Args, #{data := Data}, Ctx) ->
    Target = case Args of
                 #{target := record} -> <<"dnd-dropped-record">>;
                 _ -> <<"dnd-dropped">>
             end,
    aihtml_action:html(Ctx, {id, Target},
                       [<<"服务端收到："/utf8>>, maps:get(<<"drag">>, Data, <<>>),
                        <<" → "/utf8>>, maps:get(<<"drop">>, Data, <<>>),
                        <<"（来自 "/utf8>>, maps:get(<<"from">>, Data, <<>>), <<"）"/utf8>>]).
