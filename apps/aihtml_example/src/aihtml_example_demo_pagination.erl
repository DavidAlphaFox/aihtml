%% @doc Demos of pagination (aihtml_pagination), shown on /components/pagination. Each
%% function is one example, written the way an application writes it;
%% the docs page prints its source under it.
%%
%% The link demos point at /components/pagination/state (aihtml_example_state),
%% which renders this docs page with the page from the query, so every link
%% opens the page it names on the server. The module is also the action
%% module of pagination_server.
-module(aihtml_example_demo_pagination).
-behaviour(aihtml_action).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0, action/4]).
-export([pagination_basic/0, pagination_full/0, pagination_siblings/0, pagination_simple/0, pagination_links/0, pagination_server/0, pagination_disabled/0, pagination_record/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => pagination, title => <<"Pagination">>,
       summary => <<"页码导航，值为当前页，服务端据此渲染新的一页。"/utf8>>,
       demos => [{<<"基本用法"/utf8>>, pagination_basic},
                 {<<"首末页、总数、每页条数、跳转"/utf8>>, pagination_full},
                 {<<"当前页两侧各两页"/utf8>>, pagination_siblings},
                 {<<"简洁模式"/utf8>>, pagination_simple},
                 {<<"链接模式：不需要脚本"/utf8>>, pagination_links},
                 {<<"链接 + 服务端渲染：页内更新并写入地址栏"/utf8>>, pagination_server},
                 {<<"禁用"/utf8>>, pagination_disabled},
                 {<<"record 写法"/utf8>>, pagination_record}]}].

-spec pagination_basic() -> aihtml:html().
pagination_basic() ->
    pagination(200, 1, [], [{show_size_selector, false}, {name, page}]).

-spec pagination_full() -> aihtml:html().
pagination_full() ->
    pagination(500, 12, [], [{page_size, 20}, {show_first_last, true}, {show_total, true},
                             {show_jumper, true}]).

-spec pagination_siblings() -> aihtml:html().
pagination_siblings() ->
    pagination(300, 15, [], [{siblings, 2}, {show_size_selector, false}]).

-spec pagination_simple() -> aihtml:html().
pagination_simple() ->
    pagination(95, 3, [simple], [{show_size_selector, false}]).

-spec pagination_links() -> aihtml:html().
pagination_links() ->
    Page = aihtml_example_state:int(<<"page">>, 4),
    pagination(120, Page, [], [{href, <<"/components/pagination/state?page={page}&size={size}">>},
                               {show_size_selector, false}]).

%% Every page is a link the server can render (crawlers, new tabs); with
%% the change action a click renders the page in place and pushes its URL.
-spec pagination_server() -> aihtml:html().
pagination_server() ->
    Page = aihtml_example_state:int(<<"pg">>, 1),
    'div'([ul(articles(Page), [<<"mb-3 text-sm">>], [{id, <<"pg-articles">>}]),
           pagination(42, Page, [], [{page_size, 5}, {show_size_selector, false},
                                     {href, <<"/components/pagination/state?pg={page}">>},
                                     on(change, {?MODULE, turned, #{}})])],
          [], []).

-spec pagination_disabled() -> aihtml:html().
pagination_disabled() ->
    pagination(50, 2, [disabled], [{show_size_selector, false}]).

-spec pagination_record() -> aihtml:html().
pagination_record() ->
    #ah_pagination{total = 480, value = 6, page_size = 20, siblings = 1,
                   show_total = true, show_first_last = true, show_size_selector = false,
                   labels = #{total => <<"{0} orders">>}, name = page}.

%%%===================================================================
%%% Actions and data
%%%===================================================================

-spec action(atom(), term(), aihtml_action:event(), aihtml_action:ctx()) -> ok.
action(turned, _Args, #{value := Value}, Ctx) ->
    aihtml_action:html(Ctx, {id, <<"pg-articles">>}, articles(binary_to_integer(Value)),
                       morph_inner).

%% The five articles of a page (42 in all).
articles(Page) ->
    [li([<<"第 "/utf8>>, integer_to_binary(N), <<" 篇文章"/utf8>>], [], [])
     || N <- lists:seq((Page - 1) * 5 + 1, min(42, Page * 5))].
