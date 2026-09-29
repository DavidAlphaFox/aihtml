%% @doc Demos of the ListMenu component (aihtml_listmenu), shown on
%% /components/listmenu. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
-module(aihtml_example_demo_listmenu).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0]).
-export([listmenu_drill/0, listmenu_filter/0, listmenu_nested_value/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => listmenu, title => <<"ListMenu">>,
       summary => <<"逐级钻取的列表菜单，一次显示一层。"/utf8>>,
       demos => [{<<"逐级钻取"/utf8>>, listmenu_drill},
                 {<<"过滤"/utf8>>, listmenu_filter},
                 {<<"初始值在深层时直接显示所在页"/utf8>>, listmenu_nested_value}]}].

%%%-------------------------------------------------------------------
%%% listmenu
%%%-------------------------------------------------------------------

-spec listmenu_drill() -> aihtml:html().
listmenu_drill() ->
    'div'(listmenu(food(), undefined, [], [{name, food}]),
          [<<"w-72 border border-line rounded">>], []).

-spec listmenu_filter() -> aihtml:html().
listmenu_filter() ->
    'div'(listmenu(food(), undefined, [],
                   [{filter, true}, {filter_placeholder, <<"Filter food">>},
                    {animation, fade}]),
          [<<"w-72 border border-line rounded">>], []).

-spec listmenu_nested_value() -> aihtml:html().
listmenu_nested_value() ->
    'div'(listmenu(food(), lemon, [], [{back_label, <<"Up">>}]),
          [<<"w-72 border border-line rounded">>], []).

%% Shared by the demos above.
food() ->
    [#{key => fruit, label => <<"Fruit">>,
       children => [{apple, <<"Apple">>}, {banana, <<"Banana">>},
                    #{key => citrus, label => <<"Citrus">>,
                      children => [{lemon, <<"Lemon">>}, {orange, <<"Orange">>}]}]},
     #{key => veg, label => <<"Vegetables">>,
       children => [{carrot, <<"Carrot">>}, {pea, <<"Pea">>}]},
     {bread, <<"Bread">>},
     #{key => cake, label => <<"Cake">>, disabled => true}].

