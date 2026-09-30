-module(aihtml_action_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml.hrl").

-define(M, aihtml_action_test_mod).

%% Run an action the way a transport does: the batches of operations it
%% sends (each flush, then what is left when it returns), as JSON.
run(Token, Ev) ->
    {ok, Ref} = aihtml_action:verify(Token),
    Self = self(),
    Result = aihtml_action:execute(Ref, Ev, #{send => fun(Ops) -> Self ! {batch, Ops} end}),
    Flushed = collect(),
    case Result of
        {ok, []} -> {ok, Flushed};
        {ok, Rest} -> {ok, Flushed ++ [json(Rest)]};
        error -> {error, Flushed}
    end.

collect() ->
    receive {batch, Ops} -> [json(Ops) | collect()]
    after 0 -> []
    end.

json(Ops) -> json:decode(iolist_to_binary(aihtml_json:encode(Ops))).

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
             ah_span([], [], [on(click, {?M, inc, #{n => 1}},
                                 #{sync => queue, sync_scope => <<"form">>,
                                   indicator => <<"#spin">>, disable => this}),
                              preserve()])),
    [?assertMatch({match, _}, re:run(Html, P))
     || P <- [<<"data-ah-sync=\"queue\"">>, <<"data-ah-sync-scope=\"form\"">>,
              <<"data-ah-indicator=\"#spin\"">>, <<"data-ah-disable=\"this\"">>,
              <<" data-ah-preserve[ >]">>]],
    ?assertError({aihtml, {bad_sync, later}}, on(click, {?M, inc, #{}}, #{sync => later})),
    F = aihtml:render_binary(ah_span([], [], [fetch(get, <<"/x">>, this, #{indicator => this})])),
    ?assertMatch({match, _}, re:run(F, <<"data-ah-indicator=\"this\"">>)).

trigger_and_history_ops_test() ->
    {ok, Batches} = run_fun(fun(Ctx) ->
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
                 Batches).

%% Ops produced by a fun, through the same path pushes use.
run_fun(Fun) ->
    Ops = aihtml_action:render_ops(Fun),
    {ok, [json(Ops)]}.

component_event_names_test() ->
    Html = aihtml:render_binary(ah_span([], [], [on('ah:close', {?M, inc, #{n => 1}})])),
    ?assertMatch({match, _}, re:run(Html, <<"data-ah-on=\"ah:close:[A-Za-z0-9_-]+\\.[A-Za-z0-9_-]+\"">>)),
    ?assertError({aihtml, {bad_event_name, _}}, on(<<"ah:Close">>, {?M, inc, #{}})),
    ?assertError({aihtml, {bad_event_name, _}}, on(<<"x y">>, {?M, inc, #{}})).

%%%===================================================================
%%% Rendering
%%%===================================================================

static_render_carries_signed_actions_test() ->
    Html = aihtml:render_binary(
             ah_span(<<"+">>, [], [on(click, {?M, inc, #{n => 1}}),
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

run_returns_the_operations_test() ->
    Tok = token_of(ah_span(<<"+">>, [], [on(click, {?M, inc, #{n => 41}})])),
    ?assertEqual({ok, [[#{<<"op">> => <<"html">>, <<"id">> => <<"n">>,
                          <<"swap">> => <<"inner">>, <<"html">> => <<"42">>}]]},
                 run(Tok, #{<<"type">> => <<"click">>})).

event_fields_reach_the_action_test() ->
    Tok = aihtml_action:token({?M, echo, #{}}),
    {ok, Batches} = run(Tok, #{<<"value">> => <<"<i>">>, <<"values">> => #{<<"a">> => <<"7">>}}),
    ?assertMatch([[#{<<"sel">> := <<"#out">>, <<"html">> := <<"&lt;i&gt;|7">>}]], Batches).

flush_streams_progressively_test() ->
    {ok, Batches} = run(aihtml_action:token({?M, steps, #{}}), #{}),
    ?assertMatch([[#{<<"html">> := <<"loading">>}],
                  [#{<<"html">> := <<"done">>}, #{<<"op">> := <<"class">>, <<"add">> := <<"ready">>}]],
                 Batches).

actions_inside_action_responses_test() ->
    {ok, Batches} = run(aihtml_action:token({?M, nested, #{}}), #{}),
    [[#{<<"html">> := Html, <<"swap">> := <<"append">>}]] = Batches,
    {match, [Tok]} = re:run(Html, <<"click:([^\"]+)\"">>, [{capture, all_but_first, binary}]),
    ?assertEqual({ok, {?M, inc, #{n => 1}}}, aihtml_action:verify(Tok)).

crash_returns_error_without_details_test() ->
    logger:set_primary_config(level, none),
    try
        %% nothing of the crash reaches the transport
        ?assertEqual({error, []}, run(aihtml_action:token({?M, boom, #{}}), #{}))
    after
        logger:set_primary_config(level, notice)
    end.

ctx_is_bound_to_its_request_test() ->
    Self = self(),
    {ok, Ref} = aihtml_action:verify(aihtml_action:token({?M, inc, #{n => 0}})),
    %% a ctx used from another process than its request's fails
    _ = aihtml_action:execute(Ref, #{}, #{send => fun(_) -> ok end}),
    Ctx = {aihtml_ctx, Self, fun(_) -> ok end, #{}, undefined, <<"en">>},
    {_, R} = spawn_monitor(fun() -> aihtml_action:html(Ctx, {id, x}, <<"y">>) end),
    ?assertEqual(ok, receive {'DOWN', R, process, _, {{aihtml, action_ctx_used_outside_its_request}, _}} -> ok
                     after 1000 -> timeout end).
