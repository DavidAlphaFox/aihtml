%%%-------------------------------------------------------------------
%%% @doc Development-time reloading across both engines.
%%%
%%% One function, because that is the only thing the two dev modules have in
%%% common. Everything else about them differs -- the attribute they read, the
%%% compiler they call -- and a shared module parameterised by an engine would
%%% only push that argument out to the caller, who is a hand-written line in
%%% somebody's request handler.
%%%
%%% Like the per-engine modules this has no switch and starts nothing. It does
%%% exactly what it is called to do, once.
%%% @end
%%%-------------------------------------------------------------------
-module(ai_html_dev).

-export([reload_all/0, check/0]).

%% @doc Recompile and reload every stale template of either engine.
-spec reload_all() -> ok | {error, term()}.
reload_all() ->
    case ai_mustache_dev:reload(all) of
        ok             -> ai_jinja_dev:reload(all);
        {error, _} = E -> E
    end.

%% @doc Both engines' one-shot self-test.
-spec check() -> ok | {error, term()}.
check() ->
    case ai_mustache_dev:check() of
        ok             -> ai_jinja_dev:check();
        {error, _} = E -> E
    end.
