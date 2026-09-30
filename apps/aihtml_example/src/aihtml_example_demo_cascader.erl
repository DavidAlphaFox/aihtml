%% @doc Demos of the cascader component (aihtml_cascader), shown on
%% /components/cascader. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
%%
%% The module is also the action module of the demos that talk to the
%% server: lazily loaded levels and the change event.
-module(aihtml_example_demo_cascader).
-behaviour(aihtml_action).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0, action/4]).
-export([cas_basic/0, cas_sizes/0, cas_any_level/0, cas_search/0, cas_lazy/0, cas_record/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => cascader, title => <<"Cascader">>,
       summary => <<"逐级展开的多级选择，值是到叶子的路径，支持搜索和服务端懒加载。"/utf8>>,
       demos => [{<<"省市区选择"/utf8>>, cas_basic},
                 {<<"尺寸与禁用"/utf8>>, cas_sizes},
                 {<<"任意一级可选、自定义分隔符"/utf8>>, cas_any_level},
                 {<<"输入搜索路径"/utf8>>, cas_search},
                 {<<"服务端懒加载下一级"/utf8>>, cas_lazy},
                 {<<"record 写法"/utf8>>, cas_record}]}].

-spec cas_basic() -> aihtml:html().
cas_basic() ->
    row([ah_cascader(regions(), undefined, [<<"w-64">>], [{name, region}]),
         ah_cascader(regions(), [<<"guangdong">>, <<"shenzhen">>, <<"nanshan">>], [<<"w-64">>],
                     [{name, office}])]).

-spec cas_sizes() -> aihtml:html().
cas_sizes() ->
    row([ah_cascader(regions(), undefined, [sm, <<"w-56">>], [{placeholder, <<"小号"/utf8>>}]),
         ah_cascader(regions(), undefined, [<<"w-56">>], [{placeholder, <<"默认"/utf8>>}]),
         ah_cascader(regions(), undefined, [lg, <<"w-56">>], [{placeholder, <<"大号"/utf8>>}]),
         ah_cascader(regions(), [<<"beijing">>, <<"haidian">>], [disabled, <<"w-56">>], [])]).

-spec cas_any_level() -> aihtml:html().
cas_any_level() ->
    row([ah_cascader(categories(), [<<"electronics">>], [change_on_select, <<"w-64">>],
                     [{separator, <<" > ">>}, {placeholder, <<"商品分类"/utf8>>}]),
         ah_cascader(categories(), undefined, [no_clear, no_arrow, <<"w-64">>],
                     [{placeholder, <<"无清除按钮、无箭头"/utf8>>}])]).

-spec cas_search() -> aihtml:html().
cas_search() ->
    ah_cascader(regions(), undefined, [filterable, <<"w-72">>],
                [{placeholder, <<"输入搜索，如 南"/utf8>>}, {empty_text, <<"没有匹配的地区"/utf8>>}]).

%% Opening a province calls action(load_cities, ...) below.
-spec cas_lazy() -> aihtml:html().
cas_lazy() ->
    Provinces = [{<<"zhejiang">>, <<"浙江省"/utf8>>, lazy},
                 {<<"jiangsu">>, <<"江苏省"/utf8>>, lazy},
                 {<<"hainan">>, <<"海南省"/utf8>>, lazy}],
    ah_cascader(Provinces, undefined, [<<"w-64">>],
                [{name, city}, {load, {?MODULE, load_cities, #{}}}]).

%% The same component as a record: options are checked field names, and
%% the postback runs action(region_picked, ...) below on change.
-spec cas_record() -> aihtml:html().
cas_record() ->
    row([#ah_cascader{items = regions(), value = [<<"shanghai">>, <<"xuhui">>],
                      name = region, filterable = true, separator = <<" · "/utf8>>,
                      css = [<<"w-64">>], postback = region_picked},
         ah_span(<<"还没有选择"/utf8>>, [<<"text-sm text-muted">>], [{id, <<"region-picked">>}])]).

%%%===================================================================
%%% Actions
%%%===================================================================

-spec action(atom(), term(), aihtml_action:event(), aihtml_action:ctx()) -> ok.
action(load_cities, _Args, #{value := Path} = Event, Ctx) ->
    cascader_children(Ctx, Event, cities(Path));
action(region_picked, _Args, #{value := Value}, Ctx) ->
    aihtml_action:html(Ctx, {id, <<"region-picked">>}, [<<"服务端收到："/utf8>>, Value]).

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

row(Children) ->
    ah_div(Children, [<<"flex flex-wrap items-start gap-4">>], []).
