%% Actions used by aihtml_action_tests.
-module(aihtml_action_test_mod).
-behaviour(aihtml_action).

-include_lib("aihtml/include/aihtml.hrl").

-export([action/4]).

action(inc, #{n := N}, _Ev, Ctx) ->
    aihtml_action:html(Ctx, {id, n}, N + 1);
action(echo, _, #{value := V, values := Vs}, Ctx) ->
    aihtml_action:html(Ctx, <<"#out">>, [V, <<"|">>, maps:get(<<"a">>, Vs, <<>>)]);
action(steps, _, _Ev, Ctx) ->
    aihtml_action:html(Ctx, {id, s}, <<"loading">>),
    aihtml_action:flush(Ctx),
    aihtml_action:html(Ctx, {id, s}, <<"done">>),
    aihtml_action:add_class(Ctx, {id, s}, [ready]);
action(nested, _, _Ev, Ctx) ->
    %% HTML rendered inside an action may carry actions of its own
    aihtml_action:html(Ctx, {id, list},
                       li(span(<<"x">>, [], [on(click, {?MODULE, inc, #{n => 1}})])),
                       append);
action(publish_then_html, #{topic := T}, _Ev, Ctx) ->
    %% publishing renders its own operations without touching this reply
    aihtml_action:html(Ctx, {id, mine}, <<"before">>),
    aihtml_push:publish(T, fun(C) -> aihtml_action:html(C, {id, theirs}, <<"pushed">>) end,
                        #{except => Ctx}),
    aihtml_action:html(Ctx, {id, mine}, <<"after">>);
action(boom, _, _Ev, _Ctx) ->
    error(secret_detail).
