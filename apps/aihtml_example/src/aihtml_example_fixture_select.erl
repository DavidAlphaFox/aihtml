%% @doc Demo data shared by the dropdownlist and select demos.
-module(aihtml_example_fixture_select).

-export([fruits/0]).

%% @doc Fruit items as ah_dropdownlist/4 and ah_select/4 take them.
-spec fruits() -> [{atom(), binary()}].
fruits() ->
    [{apple, <<"Apple">>}, {banana, <<"Banana">>}, {cherry, <<"Cherry">>},
     {grape, <<"Grape">>}, {lemon, <<"Lemon">>}, {mango, <<"Mango">>},
     {orange, <<"Orange">>}, {peach, <<"Peach">>}].
