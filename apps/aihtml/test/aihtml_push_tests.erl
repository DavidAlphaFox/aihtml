-module(aihtml_push_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml.hrl").

-define(M, aihtml_action_test_mod).

push_test_() ->
    {setup,
     fun() -> {ok, Apps} = application:ensure_all_started(aihtml), Apps end,
     fun(Apps) -> [application:stop(A) || A <- lists:reverse(Apps)] end,
     [fun publish_reaches_subscribers/0,
      fun publish_carries_the_sender_stream/0,
      fun publish_inside_an_action_keeps_the_reply/0,
      fun nobody_listening_is_fine/0,
      fun leaving_is_automatic/0,
      fun call_and_trigger_publish_data/0,
      fun topics_change_without_reconnecting/0]}.

%% A stand-in for an SSE process: joins topics, forwards what it gets.
subscriber(Topics) ->
    Parent = self(),
    Pid = spawn(fun() ->
                    ok = aihtml_push:join(Topics),
                    Parent ! {joined, self()},
                    (fun Loop() -> receive M -> Parent ! {got, self(), M}, Loop() end end)()
                end),
    receive {joined, Pid} -> Pid end.

got(Pid) ->
    receive {got, Pid, {aihtml_push, Except, Json}} -> {Except, json:decode(Json)}
    after 1000 -> none
    end.

%%%===================================================================

publish_reaches_subscribers() ->
    A = subscriber([todos]),
    B = subscriber([todos, clock]),
    C = subscriber([clock]),
    ?assertEqual(2, aihtml_push:subscribers(todos)),
    ok = aihtml_push:publish(todos, fun(Ctx) ->
                                        aihtml_action:html(Ctx, {id, list}, ah_li(<<"<new>">>), append)
                                    end),
    Expected = {undefined, [#{<<"op">> => <<"html">>, <<"id">> => <<"list">>,
                              <<"swap">> => <<"append">>,
                              <<"html">> => <<"<li>&lt;new&gt;</li>">>}]},
    ?assertEqual(Expected, got(A)),
    ?assertEqual(Expected, got(B)),
    ?assertEqual(none, got(C)),
    [exit(P, kill) || P <- [A, B, C]].

publish_carries_the_sender_stream() ->
    A = subscriber([{room, 1}]),
    ok = aihtml_push:publish({room, 1}, fun(Ctx) -> aihtml_action:remove(Ctx, <<".x">>) end,
                             #{except => <<"stream-7">>}),
    ?assertMatch({<<"stream-7">>, _}, got(A)),
    exit(A, kill).

publish_inside_an_action_keeps_the_reply() ->
    A = subscriber([side]),
    {ok, Ref} = aihtml_action:verify(aihtml_action:token({?M, publish_then_html, #{topic => side}})),
    {ok, Reply} = aihtml_action:execute(Ref, #{}, #{send => fun(_) -> error(unexpected_flush) end,
                                                    stream_id => <<"me">>}),
    ?assertMatch([#{html := <<"before">>}, #{html := <<"after">>}], Reply),
    ?assertMatch({<<"me">>, [#{<<"id">> := <<"theirs">>, <<"html">> := <<"pushed">>}]}, got(A)),
    exit(A, kill).

call_and_trigger_publish_data() ->
    A = subscriber([prices]),
    ok = aihtml_push:call(prices, {id, chart}, setData, [[1, 2, 3]]),
    ?assertEqual({undefined, [#{<<"op">> => <<"call">>, <<"id">> => <<"chart">>,
                                <<"method">> => <<"setData">>, <<"args">> => [[1, 2, 3]]}]}, got(A)),
    ok = aihtml_push:trigger(prices, document, 'price:new', #{sym => <<"X">>}, #{except => <<"s1">>}),
    ?assertEqual({<<"s1">>, [#{<<"op">> => <<"trigger">>, <<"event">> => <<"price:new">>,
                               <<"detail">> => #{<<"sym">> => <<"X">>}}]}, got(A)),
    exit(A, kill).

%% What the SSE process does with a topic change (aihtml_cowboy_events).
topics_change_without_reconnecting() ->
    Parent = self(),
    S = spawn(fun() ->
                  ok = aihtml_push:join([a]),
                  ok = aihtml_push:register_stream(<<"sid">>),
                  Parent ! ready,
                  receive {aihtml_push_topics, New} ->
                      ok = aihtml_push:leave([a] -- New),
                      ok = aihtml_push:join(New -- [a]),
                      Parent ! switched
                  end,
                  receive stop -> ok end
              end),
    receive ready -> ok end,
    ?assertEqual({error, no_stream}, aihtml_push:set_topics(<<"other">>, [aihtml_push:token(b)])),
    ?assertEqual({error, invalid_topic}, aihtml_push:set_topics(<<"sid">>, [<<"forged">>])),
    ?assertEqual(ok, aihtml_push:set_topics(<<"sid">>, [aihtml_push:token(b)])),
    receive switched -> ok after 1000 -> error(timeout) end,
    ?assertEqual({0, 1}, {aihtml_push:subscribers(a), aihtml_push:subscribers(b)}),
    S ! stop.

nobody_listening_is_fine() ->
    ?assertEqual(0, aihtml_push:subscribers(nobody)),
    ?assertEqual(ok, aihtml_push:publish(nobody, fun(C) -> aihtml_action:title(C, <<"x">>) end)).

leaving_is_automatic() ->
    A = subscriber([gone]),
    ?assertEqual(1, aihtml_push:subscribers(gone)),
    Ref = erlang:monitor(process, A),
    exit(A, kill),
    receive {'DOWN', Ref, _, _, _} -> ok end,
    timer:sleep(20),
    ?assertEqual(0, aihtml_push:subscribers(gone)).

%%%===================================================================
%%% Tokens (no application needed)
%%%===================================================================

topic_token_round_trip_test() ->
    Toks = [aihtml_push:token(todos), aihtml_push:token({room, 42}), aihtml_push:token(todos)],
    ?assertEqual({ok, [todos, {room, 42}]}, aihtml_push:verify(Toks)).

tokens_do_not_cross_test() ->
    Action = aihtml_action:token({?M, inc, #{n => 1}}),
    Topic = aihtml_push:token(todos),
    ?assertEqual({error, invalid_topic}, aihtml_push:verify([Topic, Action])),
    ?assertEqual({error, invalid_action}, aihtml_action:verify(Topic)),
    ?assertEqual({error, invalid_topic}, aihtml_push:verify([<<"x", Topic/binary>>])).

too_many_topics_test() ->
    Toks = [aihtml_push:token(N) || N <- lists:seq(1, aihtml_push:max_topics() + 1)],
    ?assertEqual({error, invalid_topic}, aihtml_push:verify(Toks)).

topic_must_be_data_test() ->
    ?assertError({aihtml, {topic_not_data, _}}, aihtml_push:token({pid, self()})).

subscribe_renders_tokens_test() ->
    Html = aihtml:render_binary(ah_ul([], [], [{id, list},
                                               subscribe(todos, #{refresh => {?M, inc, #{n => 0}}})])),
    {match, [T, R]} = re:run(Html, <<"data-ah-subscribe=\"([^\"]+)\" data-ah-refresh=\"([^\"]+)\"">>,
                             [{capture, all_but_first, binary}]),
    ?assertEqual({ok, [todos]}, aihtml_push:verify([T])),
    ?assertEqual({ok, {?M, inc, #{n => 0}}}, aihtml_action:verify(R)).
