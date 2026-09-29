-module(aihtml_example_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% The supervisor owns the demo's in-memory store, so it lives exactly as
%% long as the application, and runs the clock publisher.
-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    aihtml_example_store:init(),
    {ok, {#{strategy => one_for_one, intensity => 5, period => 10},
          [#{id => aihtml_example_clock, start => {aihtml_example_clock, start_link, []}}]}}.
