%% @doc Demos of the nav tree (aihtml_nav_tree), shown on
%% /components/nav_tree. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
%%
%% The module is also the action module of the demo that tells the server
%% the chosen route.
-module(aihtml_example_demo_nav_tree).
-behaviour(aihtml_action).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0, action/4]).
-export([nav_basic/0, nav_plain/0, nav_change/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => nav_tree, title => <<"NavTree">>,
       summary => <<"分组的树状侧边导航，可折叠子菜单带圆角连接线，当前路由高亮。"/utf8>>,
       demos => [{<<"分组、图标与当前路由"/utf8>>, nav_basic},
                 {<<"不分组、外部链接"/utf8>>, nav_plain},
                 {<<"切换路由时通知服务端"/utf8>>, nav_change}]}].

%%%===================================================================
%%% NavTree
%%%===================================================================

-spec nav_basic() -> aihtml:html().
nav_basic() ->
    side(nav_tree(nav_groups(), <<"user/cards">>, [], [])).

-spec nav_plain() -> aihtml:html().
nav_plain() ->
    side(nav_tree([{<<"概览"/utf8>>, <<"overview">>},
                   #{label => <<"设置"/utf8>>,
                     items => [{<<"账号"/utf8>>, <<"settings/account">>},
                               {<<"通知"/utf8>>, <<"settings/notify">>},
                               #{label => <<"安全"/utf8>>,
                                 items => [{<<"密码"/utf8>>, <<"settings/security/password">>},
                                           {<<"两步验证"/utf8>>, <<"settings/security/2fa">>}]}]},
                   #{label => <<"帮助文档"/utf8>>, href => <<"https://example.com/docs">>}],
                  <<"settings/security/2fa">>, [], [])).

-spec nav_change() -> aihtml:html().
nav_change() ->
    row([side(nav_tree(nav_groups(), <<"dashboard">>, [],
                       [on(change, {?MODULE, route_picked, #{}})])),
         span(<<"点击左侧链接"/utf8>>, [<<"text-sm text-muted">>], [{id, <<"route-picked">>}])]).

%%%===================================================================
%%% Actions
%%%===================================================================

-spec action(atom(), term(), aihtml_action:event(), aihtml_action:ctx()) -> ok.
action(route_picked, _Args, #{value := Route}, Ctx) ->
    aihtml_action:html(Ctx, {id, <<"route-picked">>}, [<<"当前路由："/utf8>>, Route]).

%%%===================================================================
%%% Data
%%%===================================================================

nav_groups() ->
    [#{group => <<"OVERVIEW">>,
       items => [#{label => <<"Dashboard">>, icon => icon(home), route => <<"dashboard">>},
                 #{label => <<"Analytics">>, icon => icon(chart), route => <<"analytics">>}]},
     #{group => <<"MANAGEMENT">>,
       items => [#{label => <<"User">>, icon => icon(user),
                   items => [{<<"Profile">>, <<"user/profile">>}, {<<"Cards">>, <<"user/cards">>},
                             {<<"List">>, <<"user/list">>}, {<<"Account">>, <<"user/account">>}]},
                 #{label => <<"Invoice">>, icon => icon(file),
                   items => [{<<"List">>, <<"invoice/list">>},
                             {<<"Details">>, <<"invoice/details">>},
                             {<<"Create">>, <<"invoice/create">>}]}]}].

icon(Name) ->
    Paths = #{home => <<"<path d=\"m3 9 9-7 9 7v11a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2z\"/>"
                        "<polyline points=\"9 22 9 12 15 12 15 22\"/>">>,
              chart => <<"<path d=\"M3 3v18h18\"/><path d=\"M18 17V9\"/><path d=\"M13 17V5\"/>"
                         "<path d=\"M8 17v-3\"/>">>,
              user => <<"<circle cx=\"12\" cy=\"8\" r=\"5\"/><path d=\"M20 21a8 8 0 0 0-16 0\"/>">>,
              file => <<"<path d=\"M14 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8z\"/>"
                        "<path d=\"M14 2v6h6\"/>">>},
    {safe, [<<"<svg width=\"20\" height=\"20\" viewBox=\"0 0 24 24\" fill=\"none\" "
              "stroke=\"currentColor\" stroke-width=\"1.8\" stroke-linecap=\"round\" "
              "stroke-linejoin=\"round\">">>, maps:get(Name, Paths), <<"</svg>">>]}.

side(Nav) ->
    'div'(Nav, [<<"w-64 rounded-md border border-border p-3 bg-surface">>], []).

row(Children) ->
    'div'(Children, [<<"flex flex-wrap items-start gap-4">>], []).
