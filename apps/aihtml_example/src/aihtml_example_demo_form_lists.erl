%% @doc Demos of the list selection components (aihtml_form_lists), shown
%% on /components/<name>. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
%%
%% The module is also the action module of the demos that talk to the
%% server: lazily loaded cascader levels, the listbox search and the
%% change events.
-module(aihtml_example_demo_form_lists).
-behaviour(aihtml_action).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0, action/4]).
-export([cas_basic/0, cas_sizes/0, cas_any_level/0, cas_search/0, cas_lazy/0, cas_record/0,
         lb_single/0, lb_multiple/0, lb_checkboxes/0, lb_groups/0, lb_search/0,
         lb_disabled/0,
         tr_basic/0, tr_no_filter/0, tr_change/0, tr_disabled/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => cascader, title => <<"Cascader">>,
       summary => <<"逐级展开的多级选择，值是到叶子的路径，支持搜索和服务端懒加载。"/utf8>>,
       demos => [{<<"省市区选择"/utf8>>, cas_basic},
                 {<<"尺寸与禁用"/utf8>>, cas_sizes},
                 {<<"任意一级可选、自定义分隔符"/utf8>>, cas_any_level},
                 {<<"输入搜索路径"/utf8>>, cas_search},
                 {<<"服务端懒加载下一级"/utf8>>, cas_lazy},
                 {<<"record 写法"/utf8>>, cas_record}]},
     #{component => listbox, title => <<"ListBox">>,
       summary => <<"可键盘操作的选择列表，支持单选、多选、勾选框、分组和过滤。"/utf8>>,
       demos => [{<<"单选"/utf8>>, lb_single},
                 {<<"多选（Ctrl/Shift + 点击）"/utf8>>, lb_multiple},
                 {<<"勾选框与全选"/utf8>>, lb_checkboxes},
                 {<<"分组、禁用项与过滤"/utf8>>, lb_groups},
                 {<<"服务端搜索"/utf8>>, lb_search},
                 {<<"禁用"/utf8>>, lb_disabled}]},
     #{component => transfer, title => <<"Transfer">>,
       summary => <<"在两个列表之间移动条目，值是右侧列表的键。"/utf8>>,
       demos => [{<<"选择用户"/utf8>>, tr_basic},
                 {<<"无搜索框"/utf8>>, tr_no_filter},
                 {<<"改动后通知服务端"/utf8>>, tr_change},
                 {<<"禁用"/utf8>>, tr_disabled}]}].

%%%===================================================================
%%% Cascader
%%%===================================================================

-spec cas_basic() -> aihtml:html().
cas_basic() ->
    row([cascader(regions(), undefined, [<<"w-64">>], [{name, region}]),
         cascader(regions(), [<<"guangdong">>, <<"shenzhen">>, <<"nanshan">>], [<<"w-64">>],
                  [{name, office}])]).

-spec cas_sizes() -> aihtml:html().
cas_sizes() ->
    row([cascader(regions(), undefined, [sm, <<"w-56">>], [{placeholder, <<"小号"/utf8>>}]),
         cascader(regions(), undefined, [<<"w-56">>], [{placeholder, <<"默认"/utf8>>}]),
         cascader(regions(), undefined, [lg, <<"w-56">>], [{placeholder, <<"大号"/utf8>>}]),
         cascader(regions(), [<<"beijing">>, <<"haidian">>], [disabled, <<"w-56">>], [])]).

-spec cas_any_level() -> aihtml:html().
cas_any_level() ->
    row([cascader(categories(), [<<"electronics">>], [change_on_select, <<"w-64">>],
                  [{separator, <<" > ">>}, {placeholder, <<"商品分类"/utf8>>}]),
         cascader(categories(), undefined, [no_clear, no_arrow, <<"w-64">>],
                  [{placeholder, <<"无清除按钮、无箭头"/utf8>>}])]).

-spec cas_search() -> aihtml:html().
cas_search() ->
    cascader(regions(), undefined, [filterable, <<"w-72">>],
             [{placeholder, <<"输入搜索，如 南"/utf8>>}, {empty_text, <<"没有匹配的地区"/utf8>>}]).

%% Opening a province calls action(load_cities, ...) below.
-spec cas_lazy() -> aihtml:html().
cas_lazy() ->
    Provinces = [{<<"zhejiang">>, <<"浙江省"/utf8>>, lazy},
                 {<<"jiangsu">>, <<"江苏省"/utf8>>, lazy},
                 {<<"hainan">>, <<"海南省"/utf8>>, lazy}],
    cascader(Provinces, undefined, [<<"w-64">>],
             [{name, city}, {load, {?MODULE, load_cities, #{}}}]).

%% The same component as a record: options are checked field names, and
%% the postback runs action(region_picked, ...) below on change.
-spec cas_record() -> aihtml:html().
cas_record() ->
    row([#ah_cascader{items = regions(), value = [<<"shanghai">>, <<"xuhui">>],
                      name = region, filterable = true, separator = <<" · "/utf8>>,
                      css = [<<"w-64">>], postback = region_picked},
         span(<<"还没有选择"/utf8>>, [<<"text-sm text-muted">>], [{id, <<"region-picked">>}])]).

%%%===================================================================
%%% ListBox
%%%===================================================================

-spec lb_single() -> aihtml:html().
lb_single() ->
    listbox(coffees(), <<"breve">>, [<<"w-60 h-64">>], [{name, coffee}]).

-spec lb_multiple() -> aihtml:html().
lb_multiple() ->
    listbox(coffees(), [<<"latte">>, <<"lungo">>], [multiple, <<"w-60 h-64">>],
            [{name, coffees}]).

-spec lb_checkboxes() -> aihtml:html().
lb_checkboxes() ->
    listbox(coffees(), [<<"americano">>], [checkboxes, check_all, <<"w-60 h-72">>],
            [{check_all_label, <<"全选"/utf8>>}]).

-spec lb_groups() -> aihtml:html().
lb_groups() ->
    Drinks = [#{value => espresso, label => <<"Espresso">>, group => <<"热饮"/utf8>>},
              #{value => americano, label => <<"Americano">>, group => <<"热饮"/utf8>>},
              #{value => cappuccino, label => <<"Cappuccino">>, group => <<"热饮"/utf8>>},
              #{value => mocha, label => <<"Mocha">>, group => <<"热饮"/utf8>>},
              #{value => iced_latte, label => <<"Iced Latte">>, group => <<"冷饮"/utf8>>},
              #{value => cold_brew, label => <<"Cold Brew">>, group => <<"冷饮"/utf8>>},
              #{value => green_tea, label => <<"Green Tea">>, group => <<"茶饮"/utf8>>,
                disabled => true},
              #{value => earl_grey, label => <<"Earl Grey">>, group => <<"茶饮"/utf8>>}],
    listbox(Drinks, cold_brew, [filterable, <<"w-60 h-80">>],
            [{filter_placeholder, <<"过滤"/utf8>>}, {empty_text, <<"没有匹配项"/utf8>>}]).

%% Typing calls action(search_people, ...) below, which answers with
%% listbox_items.
-spec lb_search() -> aihtml:html().
lb_search() ->
    listbox(lists:sublist(people(), 5), undefined, [checkboxes, <<"w-64 h-72">>],
            [{name, people}, {filter_placeholder, <<"搜索姓名"/utf8>>},
             {search, {?MODULE, search_people, #{}}}]).

-spec lb_disabled() -> aihtml:html().
lb_disabled() ->
    listbox(lists:sublist(coffees(), 5), <<"bicerin">>, [disabled, <<"w-60">>], []).

%%%===================================================================
%%% Transfer
%%%===================================================================

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
action(load_cities, _Args, #{value := Path} = Event, Ctx) ->
    cascader_children(Ctx, Event, cities(Path));
action(search_people, _Args, #{value := Query} = Event, Ctx) ->
    Q = string:lowercase(Query),
    listbox_items(Ctx, Event, [P || P <- people(),
                                    string:find(string:lowercase(P), Q) =/= nomatch]);
action(region_picked, _Args, #{value := Value}, Ctx) ->
    aihtml_action:html(Ctx, {id, <<"region-picked">>}, [<<"服务端收到："/utf8>>, Value]);
action(columns_changed, _Args, #{value := Value}, Ctx) ->
    aihtml_action:html(Ctx, {id, <<"columns-shown">>}, [<<"显示："/utf8>>, Value]).

%% The children of a lazily loaded node, as a database would answer.
cities(<<"zhejiang">>) ->
    [{<<"hangzhou">>, <<"杭州市"/utf8>>, lazy}, {<<"ningbo">>, <<"宁波市"/utf8>>, lazy},
     {<<"wenzhou">>, <<"温州市"/utf8>>}];
cities(<<"jiangsu">>) ->
    [{<<"nanjing">>, <<"南京市"/utf8>>, lazy}, {<<"suzhou">>, <<"苏州市"/utf8>>, lazy}];
cities(<<"zhejiang,hangzhou">>) ->
    [{<<"xihu">>, <<"西湖区"/utf8>>}, {<<"binjiang">>, <<"滨江区"/utf8>>}];
cities(<<"zhejiang,ningbo">>) ->
    [{<<"yinzhou">>, <<"鄞州区"/utf8>>}, {<<"haishu">>, <<"海曙区"/utf8>>}];
cities(<<"jiangsu,nanjing">>) ->
    [{<<"xuanwu">>, <<"玄武区"/utf8>>}, {<<"gulou">>, <<"鼓楼区"/utf8>>}];
cities(<<"jiangsu,suzhou">>) ->
    [{<<"gusu">>, <<"姑苏区"/utf8>>}, {<<"wuzhong">>, <<"吴中区"/utf8>>}];
cities(_) ->
    [].

%%%===================================================================
%%% Data
%%%===================================================================

regions() ->
    [{<<"beijing">>, <<"北京市"/utf8>>,
      [{<<"dongcheng">>, <<"东城区"/utf8>>}, {<<"xicheng">>, <<"西城区"/utf8>>},
       {<<"chaoyang">>, <<"朝阳区"/utf8>>}, {<<"haidian">>, <<"海淀区"/utf8>>}]},
     {<<"shanghai">>, <<"上海市"/utf8>>,
      [{<<"huangpu">>, <<"黄浦区"/utf8>>}, {<<"xuhui">>, <<"徐汇区"/utf8>>},
       {<<"changning">>, <<"长宁区"/utf8>>}, {<<"pudong">>, <<"浦东新区"/utf8>>}]},
     {<<"guangdong">>, <<"广东省"/utf8>>,
      [{<<"guangzhou">>, <<"广州市"/utf8>>,
        [{<<"tianhe">>, <<"天河区"/utf8>>}, {<<"yuexiu">>, <<"越秀区"/utf8>>},
         {<<"haizhu">>, <<"海珠区"/utf8>>}]},
       {<<"shenzhen">>, <<"深圳市"/utf8>>,
        [{<<"futian">>, <<"福田区"/utf8>>}, {<<"nanshan">>, <<"南山区"/utf8>>},
         {<<"baoan">>, <<"宝安区"/utf8>>}]},
       {<<"dongguan">>, <<"东莞市"/utf8>>},
       #{value => <<"zhuhai">>, label => <<"珠海市"/utf8>>, disabled => true}]},
     {<<"hainan">>, <<"海南省"/utf8>>,
      [{<<"haikou">>, <<"海口市"/utf8>>}, {<<"sanya">>, <<"三亚市"/utf8>>}]}].

categories() ->
    [{<<"electronics">>, <<"电子产品"/utf8>>,
      [{<<"phone">>, <<"手机"/utf8>>, [{<<"iphone">>, <<"iPhone">>},
                                        {<<"android">>, <<"Android 手机"/utf8>>}]},
       {<<"laptop">>, <<"笔记本"/utf8>>, [{<<"macbook">>, <<"MacBook">>},
                                          {<<"windows">>, <<"Windows 笔记本"/utf8>>}]}]},
     {<<"books">>, <<"图书"/utf8>>,
      [{<<"fiction">>, <<"小说"/utf8>>, [{<<"scifi">>, <<"科幻"/utf8>>},
                                          {<<"mystery">>, <<"悬疑"/utf8>>}]},
       {<<"textbook">>, <<"教材"/utf8>>}]},
     {<<"food">>, <<"食品"/utf8>>,
      [{<<"coffee">>, <<"咖啡"/utf8>>}, {<<"tea">>, <<"茶"/utf8>>}]}].

coffees() ->
    [{<<"affogato">>, <<"Affogato">>}, {<<"americano">>, <<"Americano">>},
     {<<"bicerin">>, <<"Bicerin">>}, {<<"breve">>, <<"Breve">>},
     {<<"bombon">>, <<"Café Bombón"/utf8>>}, {<<"au-lait">>, <<"Café au lait"/utf8>>},
     {<<"corretto">>, <<"Caffé Corretto"/utf8>>}, {<<"crema">>, <<"Café Crema"/utf8>>},
     {<<"latte">>, <<"Caffé Latte"/utf8>>}, {<<"cortado">>, <<"Cortado">>},
     {<<"espresso">>, <<"Espresso">>}, {<<"flat-white">>, <<"Flat white">>},
     {<<"irish">>, <<"Irish coffee">>}, {<<"lungo">>, <<"Lungo">>}].

people() ->
    [<<"Ada Lovelace">>, <<"Alan Turing">>, <<"Barbara Liskov">>, <<"Donald Knuth">>,
     <<"Edsger Dijkstra">>, <<"Grace Hopper">>, <<"John McCarthy">>, <<"Joe Armstrong">>,
     <<"Ken Thompson">>, <<"Leslie Lamport">>, <<"Margaret Hamilton">>,
     <<"Robin Milner">>, <<"Tony Hoare">>].

departments() ->
    [{<<"rd">>, <<"研发部"/utf8>>}, {<<"product">>, <<"产品部"/utf8>>},
     {<<"design">>, <<"设计部"/utf8>>}, {<<"marketing">>, <<"市场部"/utf8>>},
     {<<"sales">>, <<"销售部"/utf8>>}, {<<"hr">>, <<"人力资源部"/utf8>>},
     {<<"finance">>, <<"财务部"/utf8>>}].

row(Children) ->
    'div'(Children, [<<"flex flex-wrap items-start gap-4">>], []).
