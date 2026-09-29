%% @doc Demos of the transfer component (aihtml_transfer), shown on
%% /components/transfer. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
%%
%% The module is also the action module of the change demo.
-module(aihtml_example_demo_transfer).
-behaviour(aihtml_action).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0, action/4]).
-export([tr_basic/0, tr_no_filter/0, tr_change/0, tr_disabled/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => transfer, title => <<"Transfer">>,
       summary => <<"在两个列表之间移动条目，值是右侧列表的键。"/utf8>>,
       demos => [{<<"选择用户"/utf8>>, tr_basic},
                 {<<"无搜索框"/utf8>>, tr_no_filter},
                 {<<"改动后通知服务端"/utf8>>, tr_change},
                 {<<"禁用"/utf8>>, tr_disabled}]}].

-spec tr_basic() -> aihtml:html().
tr_basic() ->
    Users = [{zhangsan, <<"张三"/utf8>>}, {lisi, <<"李四"/utf8>>}, {wangwu, <<"王五"/utf8>>},
             {zhaoliu, <<"赵六"/utf8>>}, {sunqi, <<"孙七"/utf8>>},
             #{value => zhouba, label => <<"周八（离职）"/utf8>>, disabled => true},
             {wujiu, <<"吴九"/utf8>>}, {zhengshi, <<"郑十"/utf8>>}],
    transfer(Users, [lisi], [<<"max-w-2xl">>],
             [{name, members}, {source_title, <<"可选用户"/utf8>>},
              {target_title, <<"已选用户"/utf8>>}, {filter_placeholder, <<"搜索"/utf8>>}]).

-spec tr_no_filter() -> aihtml:html().
tr_no_filter() ->
    transfer(departments(), [], [no_filter, <<"max-w-2xl">>],
             [{source_title, <<"可选部门"/utf8>>}, {target_title, <<"已选部门"/utf8>>},
              {empty_text, <<"暂无"/utf8>>}]).

-spec tr_change() -> aihtml:html().
tr_change() ->
    Columns = [#{value => name, label => <<"姓名"/utf8>>, icon => <<"👤"/utf8>>},
               #{value => email, label => <<"邮箱"/utf8>>, icon => <<"✉"/utf8>>},
               #{value => phone, label => <<"电话"/utf8>>, icon => <<"☎"/utf8>>},
               #{value => city, label => <<"城市"/utf8>>, icon => <<"🏙"/utf8>>}],
    'div'([transfer(Columns, [name, email], [<<"max-w-2xl">>],
                    [{source_title, <<"隐藏的列"/utf8>>}, {target_title, <<"显示的列"/utf8>>},
                     on(change, {?MODULE, columns_changed, #{}})]),
           p(<<"显示：name,email"/utf8>>, [<<"text-sm text-muted mt-2">>],
             [{id, <<"columns-shown">>}])],
          [], []).

-spec tr_disabled() -> aihtml:html().
tr_disabled() ->
    transfer(lists:sublist(departments(), 4), [<<"design">>], [disabled, no_filter,
                                                               <<"max-w-2xl">>], []).

%%%===================================================================
%%% Actions
%%%===================================================================

-spec action(atom(), term(), aihtml_action:event(), aihtml_action:ctx()) -> ok.
action(columns_changed, _Args, #{value := Value}, Ctx) ->
    aihtml_action:html(Ctx, {id, <<"columns-shown">>}, [<<"显示："/utf8>>, Value]).

%%%===================================================================
%%% Data
%%%===================================================================

departments() ->
    [{<<"rd">>, <<"研发部"/utf8>>}, {<<"product">>, <<"产品部"/utf8>>},
     {<<"design">>, <<"设计部"/utf8>>}, {<<"marketing">>, <<"市场部"/utf8>>},
     {<<"sales">>, <<"销售部"/utf8>>}, {<<"hr">>, <<"人力资源部"/utf8>>},
     {<<"finance">>, <<"财务部"/utf8>>}].
