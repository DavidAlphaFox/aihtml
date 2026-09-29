-module(aihtml_example_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% Runs the clock publisher. The data lives in Mnesia, which the
%% application starts before this supervisor (aihtml_example_store:init/0).
-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    {ok, {#{strategy => one_for_one, intensity => 5, period => 10},
          [#{id => aihtml_example_clock, start => {aihtml_example_clock, start_link, []}}]}}.
