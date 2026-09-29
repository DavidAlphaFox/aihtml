-module(aihtml_action_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml.hrl").

-define(M, aihtml_action_test_mod).

%% Run an action the way a transport does and collect the AG-UI events.
run(Token, Ev) ->
    {ok, Ref} = aihtml_action:verify(Token),
    Self = self(),
    Result = aihtml_action:execute(Ref, Ev, #{emit => fun(E) -> Self ! {ev, E} end,
                                              thread_id => <<"t1">>, run_id => <<"r1">>}),
    {Result, collect()}.

collect() ->
    receive {ev, E} -> [json:decode(iolist_to_binary(json:encode(E))) | collect()]
    after 0 -> []
    end.

ops(Events) ->
    [Ops || #{<<"type">> := <<"CUSTOM">>, <<"name">> := <<"aihtml.ui">>, <<"value">> := Ops} <- Events].

%% The token of the first action in a rendered element.
token_of(Html) ->
    {match, [Tok]} = re:run(aihtml:render_binary(Html), <<"data-ah-on=\"[a-z]+:([^\":]+)">>,
                            [{capture, all_but_first, binary}]),
    Tok.

%%%===================================================================
%%% Tokens
%%%===================================================================

token_round_trip_test() ->
    Tok = aihtml_action:token({?M, inc, #{n => 41}}),
    ?assertEqual({ok, {?M, inc, #{n => 41}}}, aihtml_action:verify(Tok)).

tampered_token_is_refused_test() ->
    Tok = aihtml_action:token({?M, inc, #{n => 1}}),
    [_, Mac] = binary:split(Tok, <<".">>),
    Forged = base64:encode(term_to_binary({?M, inc, #{n => 1000}}),
                           #{mode => urlsafe, padding => false}),
    ?assertEqual({error, invalid_action}, aihtml_action:verify(<<Forged/binary, ".", Mac/binary>>)),
    ?assertEqual({error, invalid_action}, aihtml_action:verify(<<"garbage">>)),
    ?assertEqual({error, invalid_action}, aihtml_action:verify(<<"a.b">>)).

module_without_behaviour_is_refused_test() ->
    %% even correctly signed: lists does not declare -behaviour(aihtml_action)
    Tok = aihtml_action:token({lists, reverse, []}),
    ?assertEqual({error, invalid_action}, aihtml_action:verify(Tok)).

args_must_be_data_test() ->
    ?assertError({aihtml, {action_args_not_data, _}},
                 aihtml_action:token({?M, inc, #{pid => self()}})),
    ?assertError({aihtml, {action_must_be_mfa, _}}, on(click, fun() -> ok end)).

request_options_render_as_attributes_test() ->
    Html = aihtml:render_binary(
             span([], [], [on(click, {?M, inc, #{n => 1}},
                              #{sync => queue, sync_scope => <<"form">>,
                                indicator => <<"#spin">>, disable => this}),
                           preserve()])),
    [?assertMatch({match, _}, re:run(Html, P))
     || P <- [<<"data-ah-sync=\"queue\"">>, <<"data-ah-sync-scope=\"form\"">>,
              <<"data-ah-indicator=\"#spin\"">>, <<"data-ah-disable=\"this\"">>,
              <<" data-ah-preserve[ >]">>]],
    ?assertError({aihtml, {bad_sync, later}}, on(click, {?M, inc, #{}}, #{sync => later})),
    F = aihtml:render_binary(span([], [], [fetch(get, <<"/x">>, this, #{indicator => this})])),
    ?assertMatch({match, _}, re:run(F, <<"data-ah-indicator=\"this\"">>)).

trigger_and_history_ops_test() ->
    {ok, Events} = run_fun(fun(Ctx) ->
                               aihtml_action:trigger(Ctx, document, 'ah:saved', #{id => 7}),
                               aihtml_action:trigger(Ctx, {id, list}, refresh, null),
                               aihtml_action:push_url(Ctx, <<"/items?page=3">>),
                               aihtml_action:replace_url(Ctx, "/items")
                           end),
    ?assertEqual([[#{<<"op">> => <<"trigger">>, <<"event">> => <<"ah:saved">>,
                     <<"detail">> => #{<<"id">> => 7}},
                   #{<<"op">> => <<"trigger">>, <<"id">> => <<"list">>, <<"event">> => <<"refresh">>,
                     <<"detail">> => null},
                   #{<<"op">> => <<"url">>, <<"mode">> => <<"push">>, <<"value">> => <<"/items?page=3">>},
                   #{<<"op">> => <<"url">>, <<"mode">> => <<"replace">>, <<"value">> => <<"/items">>}]],
                 ops(Events)).

%% Ops produced by a fun, through the same path pushes use.
run_fun(Fun) ->
    Ops = aihtml_action:render_ops(Fun),
    {ok, [json:decode(iolist_to_binary(json:encode(aihtml_push:event(Ops))))]}.

component_event_names_test() ->
    Html = aihtml:render_binary(span([], [], [on('ah:close', {?M, inc, #{n => 1}})])),
    ?assertMatch({match, _}, re:run(Html, <<"data-ah-on=\"ah:close:[A-Za-z0-9_-]+\\.[A-Za-z0-9_-]+\"">>)),
    ?assertError({aihtml, {bad_event_name, _}}, on(<<"ah:Close">>, {?M, inc, #{}})),
    ?assertError({aihtml, {bad_event_name, _}}, on(<<"x y">>, {?M, inc, #{}})).

%%%===================================================================
%%% Rendering
%%%===================================================================

static_render_carries_signed_actions_test() ->
    Html = aihtml:render_binary(
             span(<<"+">>, [], [on(click, {?M, inc, #{n => 1}}),
                                     on(input, {?M, echo, #{}}, #{debounce => 150,
                                                                   include => [{id, a}, <<".b">>],
                                                                   confirm => <<"Sure?">>})])),
    ?assertMatch({match, _}, re:run(Html, <<"data-ah-on=\"click:[A-Za-z0-9_-]+\\.[A-Za-z0-9_-]+ "
                                            "input:[A-Za-z0-9_.-]+:150\"">>)),
    ?assertMatch({match, _}, re:run(Html, <<"data-ah-include=\"#a, .b\"">>)),
    ?assertMatch({match, _}, re:run(Html, <<"data-ah-confirm=\"Sure\\?\"">>)).

page_points_at_the_action_endpoint_test() ->
    H = iolist_to_binary(aihtml:page(<<"x">>, #{action => <<"/act">>})),
    ?assertMatch({match, _}, re:run(H, <<"<body class=\"ah-body\" data-ah-action=\"/act\"">>)).

%%%===================================================================
%%% Running
%%%===================================================================

run_streams_agui_events_test() ->
    Tok = token_of(span(<<"+">>, [], [on(click, {?M, inc, #{n => 41}})])),
    {ok, Events} = run(Tok, #{<<"type">> => <<"click">>}),
    ?assertEqual([#{<<"type">> => <<"RUN_STARTED">>, <<"threadId">> => <<"t1">>, <<"runId">> => <<"r1">>},
                  #{<<"type">> => <<"CUSTOM">>, <<"name">> => <<"aihtml.ui">>,
                    <<"value">> => [#{<<"op">> => <<"html">>, <<"id">> => <<"n">>,
                                      <<"swap">> => <<"inner">>, <<"html">> => <<"42">>}]},
                  #{<<"type">> => <<"RUN_FINISHED">>, <<"threadId">> => <<"t1">>, <<"runId">> => <<"r1">>}],
                 Events).

event_fields_reach_the_action_test() ->
    Tok = aihtml_action:token({?M, echo, #{}}),
    {ok, Events} = run(Tok, #{<<"value">> => <<"<i>">>, <<"values">> => #{<<"a">> => <<"7">>}}),
    ?assertMatch([[#{<<"sel">> := <<"#out">>, <<"html">> := <<"&lt;i&gt;|7">>}]], ops(Events)).

flush_streams_progressively_test() ->
    {ok, Events} = run(aihtml_action:token({?M, steps, #{}}), #{}),
    ?assertMatch([[#{<<"html">> := <<"loading">>}],
                  [#{<<"html">> := <<"done">>}, #{<<"op">> := <<"class">>, <<"add">> := <<"ready">>}]],
                 ops(Events)).

actions_inside_action_responses_test() ->
    {ok, Events} = run(aihtml_action:token({?M, nested, #{}}), #{}),
    [[#{<<"html">> := Html, <<"swap">> := <<"append">>}]] = ops(Events),
    {match, [Tok]} = re:run(Html, <<"click:([^\"]+)\"">>, [{capture, all_but_first, binary}]),
    ?assertEqual({ok, {?M, inc, #{n => 1}}}, aihtml_action:verify(Tok)).

crash_ends_with_run_error_without_details_test() ->
    logger:set_primary_config(level, none),
    try
        {error, Events} = run(aihtml_action:token({?M, boom, #{}}), #{}),
        ?assertMatch([#{<<"type">> := <<"RUN_STARTED">>},
                      #{<<"type">> := <<"RUN_ERROR">>, <<"message">> := <<"action failed">>}],
                     Events),
        ?assertEqual(nomatch, binary:match(iolist_to_binary(json:encode(Events)), <<"secret_detail">>))
    after
        logger:set_primary_config(level, notice)
    end.

ctx_is_bound_to_its_request_test() ->
    Self = self(),
    {ok, Ref} = aihtml_action:verify(aihtml_action:token({?M, inc, #{n => 0}})),
    %% smuggle the ctx out through the emit fun is not possible; build one
    %% via a run and check operations from another process fail
    _ = aihtml_action:execute(Ref, #{}, #{emit => fun(_) -> ok end}),
    Ctx = {aihtml_ctx, Self, fun(_) -> ok end, #{}, undefined},
    {_, R} = spawn_monitor(fun() -> aihtml_action:html(Ctx, {id, x}, <<"y">>) end),
    ?assertEqual(ok, receive {'DOWN', R, process, _, {{aihtml, action_ctx_used_outside_its_request}, _}} -> ok
                     after 1000 -> timeout end).
