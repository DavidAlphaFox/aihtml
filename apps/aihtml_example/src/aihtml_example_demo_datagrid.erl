%% @doc Demos of the data grid (aihtml_datagrid), shown on
%% /components/datagrid. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
%%
%% The module is also the action module of the demos that talk to the
%% server: the remote grid's rows, a saved edit, the selection and the
%% command buttons.
-module(aihtml_example_demo_datagrid).
-behaviour(aihtml_action).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0, action/4]).
-export([grid_basic/0, grid_multi/0, grid_checkbox/0, grid_paging_filter/0, grid_types/0,
         grid_pinned/0, grid_editing/0, grid_grouping/0, grid_toolbar/0, grid_remote/0,
         grid_change/0, grid_empty/0, grid_record/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => datagrid, title => <<"DataGrid">>,
       summary => <<"功能完整的数据表格：排序、过滤、分页、选择、键盘导航、列宽调整与固定、"
                    "单元格编辑、分组聚合和导出，数据可以一次给全，也可以按页从服务端取。"/utf8>>,
       demos => [{<<"排序与单选"/utf8>>, grid_basic},
                 {<<"多行选择（Ctrl / Shift）"/utf8>>, grid_multi},
                 {<<"复选框选择"/utf8>>, grid_checkbox},
                 {<<"过滤行与分页"/utf8>>, grid_paging_filter},
                 {<<"格式化与列类型"/utf8>>, grid_types},
                 {<<"固定列、隐藏列与固定高度"/utf8>>, grid_pinned},
                 {<<"单元格编辑并保存到服务端"/utf8>>, grid_editing},
                 {<<"分组与聚合状态栏"/utf8>>, grid_grouping},
                 {<<"工具栏：搜索与导出"/utf8>>, grid_toolbar},
                 {<<"服务端分页、排序与过滤"/utf8>>, grid_remote},
                 {<<"选择变化通知服务端"/utf8>>, grid_change},
                 {<<"空数据与中文文案"/utf8>>, grid_empty},
                 {<<"record 写法"/utf8>>, grid_record}]}].

%%%===================================================================
%%% Demos
%%%===================================================================

-spec grid_basic() -> aihtml:html().
grid_basic() ->
    datagrid(columns(), lists:sublist(employees(), 6), [], [{sort, [{salary, desc}]}]).

-spec grid_multi() -> aihtml:html().
grid_multi() ->
    datagrid(columns(), lists:sublist(employees(), 6), [multi], [{value, [2, 3]}]).

-spec grid_checkbox() -> aihtml:html().
grid_checkbox() ->
    datagrid(columns(), lists:sublist(employees(), 6), [checkbox],
             [{value, [1]}, {name, <<"ids">>}]).

-spec grid_paging_filter() -> aihtml:html().
grid_paging_filter() ->
    datagrid(columns(), employees(), [filter_row, pageable],
             [{page_size, 5}, {page_sizes, [5, 10, 20]}, {filters, [{dept, <<"研发"/utf8>>}]}]).

-spec grid_types() -> aihtml:html().
grid_types() ->
    Cols = [#{key => name, title => <<"项目"/utf8>>, width => 120},
            #{key => budget, title => <<"预算"/utf8>>, format => <<"c2">>, align => right, width => 130},
            #{key => rate, title => <<"完成率"/utf8>>, format => <<"p1">>, align => right, width => 90},
            #{key => progress, title => <<"进度"/utf8>>, type => progress, width => 120},
            #{key => score, title => <<"评分"/utf8>>, type => rating, width => 110},
            #{key => status, title => <<"状态"/utf8>>, type => badge, width => 90,
              badges => #{done => {<<"已完成"/utf8>>, success}, late => {<<"延期"/utf8>>, danger},
                          open => {<<"进行中"/utf8>>, info}}},
            #{key => due, title => <<"截止"/utf8>>, format => <<"yyyy年MM月dd日"/utf8>>, width => 130},
            #{key => public, title => <<"公开"/utf8>>, type => bool, align => center, width => 70},
            #{key => site, title => <<"链接"/utf8>>, type => link, link_text => <<"打开"/utf8>>, width => 70}],
    Rows = [#{id => 1, name => <<"官网改版"/utf8>>, budget => 128000, rate => 0.82, progress => 82,
              score => 4, status => open, due => {2026, 11, 30}, public => true,
              site => <<"https://example.com/a">>},
            #{id => 2, name => <<"移动端"/utf8>>, budget => 356000.5, rate => 1, progress => 100,
              score => 5, status => done, due => {2026, 6, 15}, public => false,
              site => <<"https://example.com/b">>},
            #{id => 3, name => <<"数据平台"/utf8>>, budget => 990000, rate => 0.35, progress => 35,
              score => 3, status => late, due => <<"2026-08-01">>, public => true,
              site => <<"https://example.com/c">>}],
    datagrid(Cols, Rows, [none], []).

-spec grid_pinned() -> aihtml:html().
grid_pinned() ->
    Cols = [#{key => id, title => <<"ID">>, width => 60, align => center, pinned => true},
            #{key => name, title => <<"姓名"/utf8>>, width => 100, pinned => true},
            #{key => dept, title => <<"部门"/utf8>>, width => 120},
            #{key => email, title => <<"邮箱"/utf8>>, width => 220},
            #{key => city, title => <<"城市"/utf8>>, width => 120},
            #{key => hired, title => <<"入职日期"/utf8>>, width => 140},
            #{key => salary, title => <<"月薪"/utf8>>, width => 130, align => right, format => <<"n0">>},
            #{key => age, title => <<"年龄"/utf8>>, width => 100, align => center, hidden => true}],
    'div'(datagrid(Cols, employees(), [], [{height, 300}]), [<<"max-w-2xl">>], []).

%% A double click (or Enter / F2) edits a cell; the change runs
%% action(saved, ...) below, which re-renders the row from the server.
-spec grid_editing() -> aihtml:html().
grid_editing() ->
    Cols = [#{key => id, title => <<"ID">>, width => 60, align => center},
            #{key => name, title => <<"姓名"/utf8>>, width => 110, editable => true},
            #{key => dept, title => <<"部门"/utf8>>, width => 110, type => select, editable => true,
              options => depts()},
            #{key => age, title => <<"年龄"/utf8>>, width => 80, type => number, align => right,
              editable => true},
            #{key => salary, title => <<"月薪"/utf8>>, width => 120, type => number, align => right,
              format => <<"n0">>, editable => true},
            #{key => hired, title => <<"入职日期"/utf8>>, width => 140, type => date, editable => true},
            #{key => active, title => <<"在职"/utf8>>, width => 70, type => bool, align => center,
              editable => true}],
    'div'([datagrid(Cols, lists:sublist(employees(), 5), [],
                    [on('ah:edit', {?MODULE, saved, #{}}, #{sync => queue})]),
           p(<<"双击单元格编辑，Enter 保存，Esc 取消，Tab 跳到下一个可编辑列。"/utf8>>,
             [<<"text-sm text-muted mt-2">>], [{id, <<"dg-edit-log">>}])],
          [], []).

-spec grid_grouping() -> aihtml:html().
grid_grouping() ->
    Cols = [#{key => name, title => <<"姓名"/utf8>>, width => 110},
            #{key => dept, title => <<"部门"/utf8>>, width => 110},
            #{key => city, title => <<"城市"/utf8>>, width => 100},
            #{key => age, title => <<"年龄"/utf8>>, width => 90, align => right,
              aggregates => [avg, max]},
            #{key => salary, title => <<"月薪"/utf8>>, width => 150, align => right,
              format => <<"n0">>, aggregates => [sum, avg]}],
    datagrid(Cols, employees(), [statusbar, filter_row],
             [{group_by, [dept]}, {height, 420},
              {labels, #{sum => <<"合计"/utf8>>, avg => <<"平均"/utf8>>, max => <<"最大"/utf8>>}}]).

-spec grid_toolbar() -> aihtml:html().
grid_toolbar() ->
    'div'([datagrid(columns(), employees(), [multi, pageable],
                    [{toolbar, [export_csv, export_xlsx, export_pdf, separator,
                                {archive, <<"归档所选"/utf8>>}, spacer, search]},
                     {export_name, <<"employees">>}, {page_size, 5},
                     on('ah:toolbar', {?MODULE, tool, #{}})]),
           p(<<"导出的是过滤、排序后的全部行；Excel 与 PDF 的库在第一次导出时才加载。"/utf8>>,
             [<<"text-sm text-muted mt-2">>], [{id, <<"dg-tool-log">>}])],
          [], []).

%% Only the first page is rendered; sorting, the filter row, the pager and
%% the export ask action(people, ...) below, which answers with
%% datagrid_rows/4.
-spec grid_remote() -> aihtml:html().
grid_remote() ->
    Query = #{sort => [], filters => [], search => <<>>, page => 1, page_size => 5,
              offset => 0, limit => 5, export => undefined},
    {Rows, Total} = datagrid_select(Query, employees()),
    datagrid(columns(), Rows, [filter_row, pageable, checkbox],
             [{source, {?MODULE, people, #{}}}, {total, Total}, {page_size, 5},
              {page_sizes, [5, 10, 20]}, {toolbar, [search, spacer, export_csv]}]).

-spec grid_change() -> aihtml:html().
grid_change() ->
    'div'([datagrid(columns(), lists:sublist(employees(), 5), [checkbox],
                    [on(change, {?MODULE, picked, #{}}),
                     on('ah:command', {?MODULE, command, #{}})]),
           p(<<"还没有选择"/utf8>>, [<<"text-sm text-muted mt-2">>], [{id, <<"dg-picked">>}])],
          [], []).

-spec grid_empty() -> aihtml:html().
grid_empty() ->
    datagrid(columns(), [], [filter_row, pageable],
             [{labels, #{empty => <<"暂无数据"/utf8>>, total => <<"共 {0} 条"/utf8>>,
                         per_page => <<"{0} 条/页"/utf8>>, filter => <<"过滤..."/utf8>>}}]).

%% The same component as a record: options are checked field names, and
%% the postback runs action(picked, ...) below on change.
-spec grid_record() -> aihtml:html().
grid_record() ->
    'div'([#ah_datagrid{columns = columns(), rows = lists:sublist(employees(), 8),
                        selection = multi, pageable = true, page_size = 4,
                        sort = [{age, asc}], postback = {picked, #{log => <<"dg-picked-r">>}}},
           p(<<"还没有选择"/utf8>>, [<<"text-sm text-muted mt-2">>], [{id, <<"dg-picked-r">>}])],
          [], []).

%%%===================================================================
%%% Actions
%%%===================================================================

-spec action(atom(), term(), aihtml_action:event(), aihtml_action:ctx()) -> ok.
action(people, _Args, Event, Ctx) ->
    {Rows, Total} = datagrid_select(datagrid_query(Event), employees()),
    datagrid_rows(Ctx, Event, Rows, Total);
action(saved, _Args, #{data := #{<<"key">> := Key, <<"field">> := Field,
                                 <<"value">> := Value}} = Event, Ctx) ->
    [Row] = [R || #{id := Id} = R <- employees(), integer_to_binary(Id) =:= Key],
    F = binary_to_existing_atom(Field),
    datagrid_row(Ctx, Event, Row#{F => parse(F, Value)}),
    aihtml_action:html(Ctx, {id, <<"dg-edit-log">>},
                       [<<"已保存："/utf8>>, Key, <<" · "/utf8>>, Field, <<" = ">>, Value]);
action(picked, Args, #{value := Keys}, Ctx) ->
    aihtml_action:html(Ctx, {id, maps:get(log, Args, <<"dg-picked">>)},
                       [<<"服务端收到选中行："/utf8>>, Keys]);
action(command, _Args, #{data := #{<<"key">> := Key, <<"name">> := Name}}, Ctx) ->
    aihtml_action:html(Ctx, {id, <<"dg-picked">>}, [<<"命令 "/utf8>>, Name, <<" · 行 "/utf8>>, Key]);
action(tool, _Args, #{value := Keys, data := #{<<"name">> := Name}}, Ctx) ->
    aihtml_action:html(Ctx, {id, <<"dg-tool-log">>},
                       [<<"工具栏 "/utf8>>, Name, <<"，选中行："/utf8>>, Keys]).

parse(active, V) -> V =:= <<"true">>;
parse(F, V) when F =:= age; F =:= salary ->
    try binary_to_integer(V) catch error:badarg -> V end;
parse(_, V) -> V.

%%%===================================================================
%%% Data
%%%===================================================================

columns() ->
    [#{key => id, title => <<"ID">>, width => 60, align => center},
     #{key => name, title => <<"姓名"/utf8>>, width => 110},
     #{key => dept, title => <<"部门"/utf8>>, width => 110},
     #{key => age, title => <<"年龄"/utf8>>, width => 80, align => right},
     #{key => salary, title => <<"月薪"/utf8>>, width => 120, align => right, format => <<"n0">>},
     #{key => hired, title => <<"入职日期"/utf8>>, width => 120},
     #{key => ops, title => <<"操作"/utf8>>, width => 120, type => command,
       commands => [{view, <<"查看"/utf8>>}, {mail, <<"邮件"/utf8>>}]}].

depts() -> [<<"研发"/utf8>>, <<"设计"/utf8>>, <<"市场"/utf8>>, <<"销售"/utf8>>].

%% Twenty-four made-up employees, the same on every render.
employees() ->
    Names = [<<"张伟"/utf8>>, <<"王芳"/utf8>>, <<"李娜"/utf8>>, <<"刘洋"/utf8>>, <<"陈静"/utf8>>,
             <<"杨磊"/utf8>>, <<"赵敏"/utf8>>, <<"黄强"/utf8>>, <<"周杰"/utf8>>, <<"吴霞"/utf8>>,
             <<"徐涛"/utf8>>, <<"孙丽"/utf8>>, <<"马超"/utf8>>, <<"朱婷"/utf8>>, <<"胡斌"/utf8>>,
             <<"郭琳"/utf8>>, <<"何勇"/utf8>>, <<"高洁"/utf8>>, <<"林峰"/utf8>>, <<"罗欣"/utf8>>,
             <<"梁宇"/utf8>>, <<"宋佳"/utf8>>, <<"郑凯"/utf8>>, <<"谢雨"/utf8>>],
    Cities = [<<"北京"/utf8>>, <<"上海"/utf8>>, <<"深圳"/utf8>>, <<"杭州"/utf8>>, <<"成都"/utf8>>],
    [#{id => I, name => N,
       dept => lists:nth(1 + erlang:phash2({d, I}, 4), depts()),
       city => lists:nth(1 + erlang:phash2({c, I}, 5), Cities),
       age => 23 + erlang:phash2({a, I}, 20),
       salary => 8000 + 500 * erlang:phash2({s, I}, 40),
       hired => iolist_to_binary(io_lib:format("20~2..0B-~2..0B-~2..0B",
                                               [15 + erlang:phash2({y, I}, 11),
                                                1 + erlang:phash2({m, I}, 12),
                                                1 + erlang:phash2({dd, I}, 28)])),
       email => <<"user", (integer_to_binary(I))/binary, "@example.com">>,
       active => erlang:phash2({x, I}, 5) > 0}
     || {I, N} <- lists:zip(lists:seq(1, length(Names)), Names)].
