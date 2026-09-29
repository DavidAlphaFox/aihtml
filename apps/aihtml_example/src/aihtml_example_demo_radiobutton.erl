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
    row([radiobutton(<<"Email">>, email, [], [{name, contact}, {checked, true}]),
         radiobutton(<<"Phone">>, phone, [], [{name, contact}]),
         radiobutton(<<"Post">>, post, [], [{name, contact}])]).

-spec radio_sizes() -> aihtml:html().
radio_sizes() ->
    row([radiobutton(<<"Small">>, s, [sm], [{name, size}, {checked, true}]),
         radiobutton(<<"Medium">>, m, [md], [{name, size}]),
         radiobutton(<<"Large">>, l, [lg], [{name, size}]),
         radiobutton(<<"Disabled">>, x, [], [{disabled, true}, {checked, true}])]).

row(Children) ->
    'div'(Children, [<<"flex flex-wrap items-center gap-6">>], []).
