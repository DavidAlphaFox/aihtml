%% @doc Demos of the kbd component (aihtml_kbd), shown on
%% /components/kbd. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
-module(aihtml_example_demo_kbd).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0]).
-export([kbd_keys/0, kbd_combos/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => kbd, title => <<"Kbd">>,
       summary => <<"键盘按键与组合键。"/utf8>>,
       demos => [{<<"单键与尺寸"/utf8>>, kbd_keys},
                 {<<"组合键"/utf8>>, kbd_combos}]}].

%%% Kbd

-spec kbd_keys() -> aihtml:html().
kbd_keys() ->
    row([ah_kbd(<<"Esc">>, [], []),
         ah_kbd(<<"⌘"/utf8>>, [], []),
         ah_kbd(<<"Tab">>, [], []),
         ah_kbd(<<"Enter">>, [lg], [])]).

-spec kbd_combos() -> aihtml:html().
kbd_combos() ->
    row([ah_kbd([<<"Ctrl">>, <<"Shift">>, <<"P">>], [], []),
         ah_kbd([<<"⌘"/utf8>>, <<"K">>], [lg], []),
         ah_span([<<"Press ">>, ah_kbd([<<"Ctrl">>, <<"C">>], [], []), <<" to copy">>])]).

%% Layout helpers of the demos.
row(Children) ->
    ah_div(Children, [<<"flex flex-wrap items-center gap-4">>], []).
