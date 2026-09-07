%%%-------------------------------------------------------------------
%%% @doc rebar3 plugin entry point.
%%%
%%% Chains every provider's init/1. The fun only matches {ok, S}: a provider
%%% init/1 that returned anything else would be a bug in this plugin, and a
%%% badmatch here is a better outcome than a half registered provider set.
%%% @end
%%%-------------------------------------------------------------------
-module(rebar3_aihtml).

-export([init/1]).

-spec init(term()) -> {ok, term()}.
init(State) ->
    ok = ensure_core(),
    lists:foldl(fun(M, {ok, S}) -> M:init(S) end, {ok, State},
                [rebar3_aihtml_prv, rebar3_aihtml_jinja,
                 rebar3_aihtml_migrate]).

%% @doc Make the compile cores reachable from the rebar3 VM.
%%
%% The parent app's src/ is built through extra_src_dirs (see rebar.config for
%% why it cannot be src_dirs), which puts those beams next to the plugin's own
%% ebin rather than in it, and rebar3 only adds a plugin's ebin to the code
%% path. The layout is fixed -- <out>/rebar3_aihtml/ebin and <out>/src -- so
%% the sibling can be derived from this module's own beam, in both the
%% standalone build and the installed-plugin build.
%%
%% If the modules are already loadable, nothing is touched: an aihtml already
%% on the path (as a dependency of the project being built, say) is left to
%% win, so there is one copy in play rather than two.
-spec ensure_core() -> ok.
ensure_core() ->
    case code:ensure_loaded(ai_mustache_compiler) of
        {module, _} -> ok;
        _           -> add_core_path()
    end.

-spec add_core_path() -> ok.
add_core_path() ->
    case code:which(?MODULE) of
        Beam when is_list(Beam) ->
            Ebin = filename:dirname(Beam),                 % .../rebar3_aihtml/ebin
            AppDir = filename:dirname(Ebin),               % .../rebar3_aihtml
            Extras = filename:join(filename:dirname(AppDir), "src"),
            case filelib:is_dir(Extras) of
                true  -> _ = code:add_pathz(Extras), ok;
                false -> ok
            end;
        _ ->
            ok
    end.
