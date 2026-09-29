%% @doc Publishes the server time to the `clock' topic every second: push
%% with no user action. In a cluster only one node's ticker runs (it holds
%% a global name); the others wait and take over if that node goes away.
-module(aihtml_example_clock).

-export([start_link/0, init/0, now_text/0]).

-spec start_link() -> {ok, pid()}.
start_link() ->
    {ok, proc_lib:spawn_link(?MODULE, init, [])}.

-spec init() -> no_return().
init() ->
    %% When two clusters join, the loser of the name is told instead of
    %% killed, and goes back to waiting.
    case global:register_name(?MODULE, self(), fun global:random_notify_name/3) of
        yes ->
            {ok, TRef} = timer:send_interval(1000, tick),
            tick(TRef);
        no ->
            timer:sleep(5000),
            init()
    end.

tick(TRef) ->
    receive
        {global_name_conflict, ?MODULE} ->
            {ok, cancel} = timer:cancel(TRef),
            flush_ticks(),
            init();
        tick ->
            %% skip rendering when nobody is watching
            _ = aihtml_push:subscribers(clock) > 0 andalso
                aihtml_push:publish(clock, fun(Ctx) ->
                                               aihtml_action:html(Ctx, {id, clock}, now_text())
                                           end),
            tick(TRef)
    end.

flush_ticks() ->
    receive tick -> flush_ticks() after 0 -> ok end.

-spec now_text() -> binary().
now_text() ->
    {_, {H, M, S}} = calendar:local_time(),
    iolist_to_binary(io_lib:format("~2..0b:~2..0b:~2..0b", [H, M, S])).
