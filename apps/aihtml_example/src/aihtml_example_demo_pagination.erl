%% @doc Demos of pagination (aihtml_pagination), shown on /components/pagination. Each
%% function is one example, written the way an application writes it;
%% the docs page prints its source under it.
-module(aihtml_example_demo_pagination).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0]).
-export([pagination_basic/0, pagination_full/0, pagination_siblings/0, pagination_simple/0, pagination_links/0, pagination_disabled/0, pagination_record/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => pagination, title => <<"Pagination">>,
       summary => <<"页码导航，值为当前页，服务端据此渲染新的一页。"/utf8>>,
       demos => [{<<"基本用法"/utf8>>, pagination_basic},
                 {<<"首末页、总数、每页条数、跳转"/utf8>>, pagination_full},
                 {<<"当前页两侧各两页"/utf8>>, pagination_siblings},
                 {<<"简洁模式"/utf8>>, pagination_simple},
                 {<<"链接模式：不需要脚本"/utf8>>, pagination_links},
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
    pagination(120, 4, [], [{href, <<"?page={page}&size={size}">>},
                            {show_size_selector, false}]).

-spec pagination_disabled() -> aihtml:html().
pagination_disabled() ->
    pagination(50, 2, [disabled], [{show_size_selector, false}]).

-spec pagination_record() -> aihtml:html().
pagination_record() ->
    #ah_pagination{total = 480, value = 6, page_size = 20, siblings = 1,
                   show_total = true, show_first_last = true, show_size_selector = false,
                   labels = #{total => <<"{0} orders">>}, name = page}.
