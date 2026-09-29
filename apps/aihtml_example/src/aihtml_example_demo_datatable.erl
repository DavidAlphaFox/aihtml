%% @doc Demos of the data table (aihtml_datatable), shown on
%% /components/datatable. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
%%
%% The module is also the action module of the demos that talk to the
%% server: the remote data table's pages, cell edits and selections.
-module(aihtml_example_demo_datatable).
-behaviour(aihtml_action).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0, action/4]).
-export([dt_basic/0, dt_paging/0, dt_filter_row/0, dt_search/0, dt_advanced/0,
         dt_checkbox/0, dt_details/0, dt_editing/0, dt_columns/0, dt_remote/0, dt_texts/0,
         dt_record/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => datatable, title => <<"DataTable">>,
       summary => <<"数据表格：排序、筛选、搜索、分页、行选择、行详情和单元格编辑，可在浏览器本地处理，也可每页由服务端渲染。"/utf8>>,
       demos => [{<<"基础表格"/utf8>>, dt_basic},
                 {<<"分页"/utf8>>, dt_paging},
                 {<<"筛选行"/utf8>>, dt_filter_row},
                 {<<"搜索栏"/utf8>>, dt_search},
                 {<<"高级筛选"/utf8>>, dt_advanced},
                 {<<"复选框选择并通知服务端"/utf8>>, dt_checkbox},
                 {<<"行详情"/utf8>>, dt_details},
                 {<<"单元格编辑"/utf8>>, dt_editing},
                 {<<"列宽调整与列选择器"/utf8>>, dt_columns},
                 {<<"服务端分页、排序与筛选"/utf8>>, dt_remote},
                 {<<"中文界面文字"/utf8>>, dt_texts},
                 {<<"record 写法"/utf8>>, dt_record}]}].

%%%===================================================================
%%% DataTable
%%%===================================================================

-spec dt_basic() -> aihtml:html().
dt_basic() ->
    datatable(employee_columns(), lists:sublist(employees(), 8), [], [{height, 330}]).

-spec dt_paging() -> aihtml:html().
dt_paging() ->
    datatable(employee_columns(), employees(), [], [{page_size, 5}, {sort, {salary, desc}}]).

-spec dt_filter_row() -> aihtml:html().
dt_filter_row() ->
    datatable(employee_columns(), employees(), [],
              [{filter, row}, {filters, #{dept => <<"研发"/utf8>>}}, {page_size, 10}]).

-spec dt_search() -> aihtml:html().
dt_search() ->
    datatable(employee_columns(), employees(), [],
              [{filter, search}, {page_size, 5}, {page_sizes, []}]).

-spec dt_advanced() -> aihtml:html().
dt_advanced() ->
    datatable(employee_columns(), employees(), [],
              [{filter, advanced}, {filters, #{age => {gte, 30}}}, {page_size, 5}]).

-spec dt_checkbox() -> aihtml:html().
dt_checkbox() ->
    'div'([datatable(employee_columns(), lists:sublist(employees(), 6), [],
                     [{selection_mode, checkbox}, {value, [2, 5]}, {name, employees},
                      on(change, {?MODULE, employees_picked, #{}})]),
           p(<<"已选：2,5"/utf8>>, [<<"text-sm text-muted mt-2">>], [{id, <<"employees-picked">>}])],
          [], []).

-spec dt_details() -> aihtml:html().
dt_details() ->
    Details = fun(#{name := Name, title := Title, hired := Hired}) ->
                      'div'([strong(Name, [], []), <<" · "/utf8>>, Title,
                             p([<<"入职日期："/utf8>>, Hired], [<<"text-muted mt-1">>], [])],
                            [<<"text-sm">>], [])
              end,
    datatable(lists:sublist(employee_columns(), 4), lists:sublist(employees(), 5), [],
              [{row_details, Details}, {expanded, [1]}, {selection_mode, none}]).

%% A committed edit runs action(employee_edited, ...) below, which answers
%% with the stored row (the salary renderer applies again).
-spec dt_editing() -> aihtml:html().
dt_editing() ->
    editable_table().

-spec dt_columns() -> aihtml:html().
dt_columns() ->
    Columns = [C#{hidden => maps:get(field, C) =:= hired} || C <- employee_columns()],
    datatable(Columns, lists:sublist(employees(), 6), [],
              [{resizable, true}, {column_chooser, true}]).

%% Each sort, filter or page change runs action(orders, ...) below, which
%% queries the rows and answers with datatable_rows. The pager's links
%% (href) lead to /components/datatable/state?dt_page=..., where this
%% function renders that page from the query: crawlable without script.
-spec dt_remote() -> aihtml:html().
dt_remote() ->
    Page = max(1, aihtml_example_state:int(<<"dt_page">>, 1)),
    Size = min(50, max(1, aihtml_example_state:int(<<"dt_size">>, 10))),
    Sort = case binary:split(aihtml_example_state:param(<<"dt_sort">>, <<>>), <<":">>) of
               [F, <<"asc">>] -> {F, asc};
               [F, <<"desc">>] -> {F, desc};
               _ -> undefined
           end,
    Query = #{sort => Sort, search => <<>>, filters => #{}, page => Page, page_size => Size,
              offset => (Page - 1) * Size, limit => Size},
    {Rows, Total} = aihtml_datatable:datatable_page(order_columns(), orders(), Query),
    orders_table(Rows, Total, Query).

-spec dt_texts() -> aihtml:html().
dt_texts() ->
    datatable(employee_columns(), employees(), [],
              [{filter, search}, {page_size, 5},
               {empty_text, <<"没有符合条件的员工"/utf8>>},
               {texts, #{search => <<"搜索姓名、部门……"/utf8>>, info => <<"第 {start}-{end} 条，共 {total} 条"/utf8>>,
                         prev => <<"上一页"/utf8>>, next => <<"下一页"/utf8>>,
                         page_size => <<"每页行数"/utf8>>}}]).

%% The postback runs action(employees_picked, ...) below on change.
-spec dt_record() -> aihtml:html().
dt_record() ->
    #ah_datatable{columns = lists:sublist(employee_columns(), 4), rows = employees(),
                  selection_mode = multiple, sort = {age, asc}, filter = row,
                  page_size = 5, page_sizes = [5, 10], hover = true,
                  postback = employees_picked}.

employee_columns() ->
    [#{field => id, title => <<"ID">>, width => 64, align => center},
     #{field => name, title => <<"姓名"/utf8>>, width => 120},
     #{field => dept, title => <<"部门"/utf8>>, width => 110},
     #{field => age, title => <<"年龄"/utf8>>, width => 80, align => center, type => number},
     #{field => salary, title => <<"月薪"/utf8>>, width => 120, align => right, type => number,
       render => fun(V, _) -> [<<"¥"/utf8>>, thousands(V)] end},
     #{field => hired, title => <<"入职日期"/utf8>>, width => 120, type => date}].

employees() ->
    Names = [<<"张伟"/utf8>>, <<"王芳"/utf8>>, <<"李娜"/utf8>>, <<"刘洋"/utf8>>, <<"陈静"/utf8>>,
             <<"杨磊"/utf8>>, <<"赵敏"/utf8>>, <<"黄强"/utf8>>, <<"周杰"/utf8>>, <<"吴霞"/utf8>>,
             <<"徐涛"/utf8>>, <<"孙丽"/utf8>>, <<"马超"/utf8>>, <<"朱婷"/utf8>>, <<"胡斌"/utf8>>,
             <<"郭琳"/utf8>>, <<"何勇"/utf8>>, <<"高洁"/utf8>>],
    Depts = [<<"研发部"/utf8>>, <<"市场部"/utf8>>, <<"财务部"/utf8>>, <<"研发部"/utf8>>, <<"人事部"/utf8>>],
    Titles = [<<"工程师"/utf8>>, <<"经理"/utf8>>, <<"分析师"/utf8>>, <<"主管"/utf8>>],
    [#{id => I, name => N, dept => lists:nth(I rem 5 + 1, Depts),
       title => lists:nth(I rem 4 + 1, Titles),
       age => 23 + (I * 7) rem 19, salary => 9000 + (I * 3700) rem 21000,
       hired => iolist_to_binary(io_lib:format("20~2..0B-~2..0B-~2..0B",
                                               [14 + I rem 11, 1 + I rem 12, 1 + (I * 5) rem 28]))}
     || {I, N} <- lists:zip(lists:seq(1, length(Names)), Names)].

editable_table() ->
    datatable(lists:sublist(employee_columns(), 5), lists:sublist(employees(), 6), [],
              [{editable, true}, {edit, {?MODULE, employee_edited, #{}}}]).

order_columns() ->
    [#{field => no, title => <<"订单号"/utf8>>, width => 110},
     #{field => customer, title => <<"客户"/utf8>>},
     #{field => city, title => <<"城市"/utf8>>, width => 90},
     #{field => amount, title => <<"金额"/utf8>>, width => 120, align => right, type => number,
       render => fun(V, _) -> [<<"¥"/utf8>>, thousands(V)] end},
     #{field => status, title => <<"状态"/utf8>>, width => 90}].

%% 137 orders, standing in for a database table.
orders() ->
    Customers = [<<"星河科技"/utf8>>, <<"蓝鲸物流"/utf8>>, <<"青禾餐饮"/utf8>>, <<"远山教育"/utf8>>,
                 <<"北辰制造"/utf8>>, <<"南风传媒"/utf8>>, <<"云帆软件"/utf8>>],
    Cities = [<<"北京"/utf8>>, <<"上海"/utf8>>, <<"广州"/utf8>>, <<"深圳"/utf8>>, <<"杭州"/utf8>>, <<"成都"/utf8>>],
    States = [<<"待付款"/utf8>>, <<"已发货"/utf8>>, <<"已完成"/utf8>>, <<"已取消"/utf8>>],
    [#{id => I, no => iolist_to_binary(io_lib:format("SO-~4..0B", [I])),
       customer => lists:nth(I rem 7 + 1, Customers), city => lists:nth(I rem 6 + 1, Cities),
       amount => 200 + (I * 7919) rem 50000, status => lists:nth(I rem 4 + 1, States)}
     || I <- lists:seq(1, 137)].

orders_table(Rows, Total, #{page := Page, page_size := Size, sort := Sort}) ->
    datatable(order_columns(), Rows, [],
              [{source, {?MODULE, orders, #{}}}, {total, Total}, {page, Page},
               {page_size, Size}, {sort, Sort}, {filter, row}, {height, 470},
               {href, <<"/components/datatable/state?dt_page={page}&dt_size={size}&dt_sort={sort}">>}]).

thousands(N) when is_integer(N) ->
    S = integer_to_list(N),
    L = length(S),
    list_to_binary(lists:append([[C | [$, || I > 1, (I - 1) rem 3 =:= 0]]
                                 || {C, I} <- lists:zip(S, lists:seq(L, 1, -1))]));
thousands(_) -> <<>>.

%%%===================================================================
%%% Actions
%%%===================================================================

-spec action(atom(), term(), aihtml_action:event(), aihtml_action:ctx()) -> ok.
action(employees_picked, _Args, #{value := Keys}, Ctx) ->
    aihtml_action:html(Ctx, {id, <<"employees-picked">>}, [<<"已选："/utf8>>, Keys]);
action(employee_edited, _Args,
       #{data := #{<<"key">> := Key, <<"field">> := Field, <<"value">> := Value}} = Event, Ctx) ->
    Id = binary_to_integer(Key),
    [Row] = [E || #{id := I} = E <- employees(), I =:= Id],
    F = binary_to_existing_atom(Field),
    New = case F of
              _ when F =:= age; F =:= salary ->
                  try binary_to_integer(Value) catch error:badarg -> maps:get(F, Row) end;
              _ -> Value
          end,
    datatable_row(Ctx, Event, editable_table(), Row#{F => New});
action(orders, _Args, Event, Ctx) ->
    Query = datatable_query(Event),
    {Rows, Total} = aihtml_datatable:datatable_page(order_columns(), orders(), Query),
    datatable_rows(Ctx, Event, orders_table(Rows, Total, Query)).
