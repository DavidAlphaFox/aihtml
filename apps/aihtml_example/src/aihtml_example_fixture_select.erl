%% @doc Demo data shared by the dropdownlist and select demos.
-module(aihtml_example_fixture_select).

-export([fruits/0]).

%% @doc Fruit items as dropdownlist/4 and select/4 take them.
-spec fruits() -> [{atom(), binary()}].
fruits() ->
    [{apple, <<"Apple">>}, {banana, <<"Banana">>}, {cherry, <<"Cherry">>},
     {grape, <<"Grape">>}, {lemon, <<"Lemon">>}, {mango, <<"Mango">>},
     {orange, <<"Orange">>}, {peach, <<"Peach">>}].
