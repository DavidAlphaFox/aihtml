-module(aihtml_app).
-behaviour(application).

-export([start/2, stop/1]).

-spec start(application:start_type(), term()) -> {ok, pid()} | {error, term()}.
start(_Type, _Args) ->
    %% A configured secret that cannot sign safely stops the boot here,
    %% instead of failing on the first page render.
    case aihtml_action:check_secret() of
        ok -> aihtml_sup:start_link();
        {error, _} = E -> E
    end.

-spec stop(term()) -> ok.
stop(_State) ->
    ok.
