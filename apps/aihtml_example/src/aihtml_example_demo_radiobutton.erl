%% @doc Demos of the radio button (aihtml_radiobutton), shown on
%% /components/radiobutton. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
-module(aihtml_example_demo_radiobutton).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0]).
-export([radio_basic/0, radio_sizes/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => radiobutton, title => <<"RadioButton">>,
       summary => <<"单选按钮，同名的一组互斥。"/utf8>>,
       demos => [{<<"同名互斥"/utf8>>, radio_basic},
                 {<<"尺寸与禁用"/utf8>>, radio_sizes}]}].

-spec radio_basic() -> aihtml:html().
radio_basic() ->
    row([ah_radiobutton(<<"Email">>, email, [], [{name, contact}, {checked, true}]),
         ah_radiobutton(<<"Phone">>, phone, [], [{name, contact}]),
         ah_radiobutton(<<"Post">>, post, [], [{name, contact}])]).

-spec radio_sizes() -> aihtml:html().
radio_sizes() ->
    row([ah_radiobutton(<<"Small">>, s, [sm], [{name, size}, {checked, true}]),
         ah_radiobutton(<<"Medium">>, m, [md], [{name, size}]),
         ah_radiobutton(<<"Large">>, l, [lg], [{name, size}]),
         ah_radiobutton(<<"Disabled">>, x, [], [{disabled, true}, {checked, true}])]).

row(Children) ->
    ah_div(Children, [<<"flex flex-wrap items-center gap-6">>], []).
