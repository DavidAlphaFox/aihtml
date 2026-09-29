-module(aihtml_example_store_tests).

-include_lib("eunit/include/eunit.hrl").

-define(STORE, aihtml_example_store).

%% Each test gets a fresh Mnesia in its own directory.
with_store(Storage, Test) ->
    Dir = filename:join(["_build", "test", "mnesia_eunit",
                         integer_to_list(erlang:unique_integer([positive]))]),
    fun() ->
        application:set_env(mnesia, dir, Dir),
        application:set_env(aihtml_example, db_storage, Storage),
        try
            ok = ?STORE:init(),
            Test(Dir)
        after
            _ = application:stop(mnesia),
            _ = mnesia:delete_schema([node()]),
            application:unset_env(mnesia, dir),
            application:unset_env(aihtml_example, db_storage)
        end
    end.

store_test_() ->
    [{"seeded in order", with_store(ram_copies, fun seeded/1)},
     {"todos", with_store(ram_copies, fun todo_ops/1)},
     {"counter", with_store(ram_copies, fun counter_ops/1)},
     {"concurrent writers", with_store(ram_copies, fun concurrent/1)},
     {"disc copies survive a restart", with_store(disc_copies, fun restart/1)}].

seeded(_) ->
    ?assertMatch([#{id := 1, done := false}, #{id := 2}, #{id := 3}], ?STORE:todos()),
    ?assertEqual(0, ?STORE:counter()).

todo_ops(_) ->
    #{id := Id} = ?STORE:add_todo(<<"new">>),
    ?assertEqual(4, Id),
    ?assertMatch(#{id := 4, text := <<"new">>, done := true}, ?STORE:toggle_todo(Id)),
    ?assertMatch(#{done := true}, ?STORE:todo(Id)),
    ?assertMatch(#{done := false}, ?STORE:toggle_todo(Id)),
    ok = ?STORE:delete_todo(Id),
    ?assertEqual(undefined, ?STORE:todo(Id)),
    ?assertEqual(undefined, ?STORE:toggle_todo(Id)),
    %% ids are never reused
    ?assertMatch(#{id := 5}, ?STORE:add_todo(<<"next">>)).

counter_ops(_) ->
    ?assertEqual(1, ?STORE:bump()),
    ?assertEqual(2, ?STORE:bump()),
    ?assertEqual(2, ?STORE:counter()),
    ok = ?STORE:reset(),
    ?assertEqual(0, ?STORE:counter()).

%% Transactions keep read-modify-write correct under contention.
concurrent(_) ->
    Parent = self(),
    Pids = [spawn(fun() ->
                      _ = ?STORE:bump(),
                      #{id := Id} = ?STORE:add_todo(<<"c">>),
                      Parent ! {done, self(), Id}
                  end) || _ <- lists:seq(1, 50)],
    Ids = [receive {done, P, Id} -> Id after 5000 -> error(timeout) end || P <- Pids],
    ?assertEqual(50, ?STORE:counter()),
    ?assertEqual(50, length(lists:usort(Ids))),
    ?assertEqual(53, length(?STORE:todos())).

restart(_) ->
    #{id := Id} = ?STORE:add_todo(<<"kept">>),
    _ = ?STORE:bump(),
    stopped = application:stop(mnesia) =:= ok andalso stopped,
    ok = ?STORE:init(),
    ?assertMatch(#{text := <<"kept">>}, ?STORE:todo(Id)),
    ?assertEqual(1, ?STORE:counter()),
    %% no second seeding
    ?assertEqual(4, length(?STORE:todos())).
