%% @doc Demos of the repeat button (aihtml_repeat_button), shown on
%% /components/repeat_button.
%% Each function is one example, written the way an application writes
%% it; the docs page prints its source under it.
%%
%% The module is also the action module of repeat_counter (repeat buttons
%% stepping a number).
-module(aihtml_example_demo_repeat_button).
-behaviour(aihtml_action).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0, action/4]).
-export([repeat_counter/0, repeat_variants/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => repeat_button, title => <<"RepeatButton">>,
       summary => <<"按住不放时连续触发点击的按钮，适合步进调整数值。"/utf8>>,
       demos => [{<<"按住连续加减"/utf8>>, repeat_counter},
                 {<<"样式、尺寸、间隔"/utf8>>, repeat_variants}]}].

%% Each click (repeated while held) runs action(step, ...) below with the
%% number's current value, and the server sets the new one.
-spec repeat_counter() -> aihtml:html().
repeat_counter() ->
    Step = fun(Label, By) ->
                   ah_repeat_button(Label, By, [outlined],
                                    [{interval, 80},
                                     on(click, {?MODULE, step, #{by => By}},
                                        #{include => [<<"input[name=qty]">>]})])
           end,
    row([Step(<<"−"/utf8>>, -1),
         ah_formatted_input(10, [<<"w-32">>],
                            [{id, qty}, {name, qty}, {min, 0}, {max, 999},
                             {spin_buttons, false}, {drop_down, false}]),
         Step(<<"+">>, 1)]).

-spec repeat_variants() -> aihtml:html().
repeat_variants() ->
    row([ah_repeat_button(<<"Primary">>, undefined, [], []),
         ah_repeat_button(<<"Slow">>, undefined, [secondary, sm], [{delay, 600}, {interval, 250}]),
         ah_repeat_button(<<"Round">>, undefined, [success, round], []),
         ah_repeat_button(<<"Next">>, undefined, [outlined, lg],
                          [{icon, <<"▶"/utf8>>}, {icon_position, right}]),
         ah_repeat_button(<<"Disabled">>, undefined, [], [{disabled, true}])]).

-spec action(atom(), term(), aihtml_action:event(), aihtml_action:ctx()) -> ok.
action(step, #{by := By}, #{values := Values}, Ctx) ->
    Old = binary_to_integer(maps:get(<<"qty">>, Values, <<"0">>)),
    aihtml_action:call(Ctx, {id, qty}, setValue, [max(0, min(999, Old + By))]).

row(Children) ->
    ah_div(Children, [<<"flex flex-wrap items-center gap-4">>], []).
