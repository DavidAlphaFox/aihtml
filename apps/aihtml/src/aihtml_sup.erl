%% @doc Starts the `pg' scope that aihtml_push publishes through. It is
%% infrastructure only: group membership of open push streams, no
%% application state.
-module(aihtml_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    {ok, {#{strategy => one_for_one, intensity => 5, period => 10},
          [#{id => aihtml_push, start => {pg, start_link, [aihtml_push]}}]}}.
