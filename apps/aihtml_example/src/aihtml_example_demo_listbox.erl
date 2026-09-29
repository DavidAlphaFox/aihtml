%% @doc Demos of the listbox component (aihtml_listbox), shown on
%% /components/listbox. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
%%
%% The module is also the action module of the server-side search demo.
-module(aihtml_example_demo_listbox).
-behaviour(aihtml_action).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0, action/4]).
-export([lb_single/0, lb_multiple/0, lb_checkboxes/0, lb_groups/0, lb_search/0,
         lb_disabled/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => listbox, title => <<"ListBox">>,
       summary => <<"可键盘操作的选择列表，支持单选、多选、勾选框、分组和过滤。"/utf8>>,
       demos => [{<<"单选"/utf8>>, lb_single},
                 {<<"多选（Ctrl/Shift + 点击）"/utf8>>, lb_multiple},
                 {<<"勾选框与全选"/utf8>>, lb_checkboxes},
                 {<<"分组、禁用项与过滤"/utf8>>, lb_groups},
                 {<<"服务端搜索"/utf8>>, lb_search},
                 {<<"禁用"/utf8>>, lb_disabled}]}].

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
%%% Actions
%%%===================================================================

-spec action(atom(), term(), aihtml_action:event(), aihtml_action:ctx()) -> ok.
action(search_people, _Args, #{value := Query} = Event, Ctx) ->
    Q = string:lowercase(Query),
    listbox_items(Ctx, Event, [P || P <- people(),
                                    string:find(string:lowercase(P), Q) =/= nomatch]).

%%%===================================================================
%%% Data
%%%===================================================================

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
