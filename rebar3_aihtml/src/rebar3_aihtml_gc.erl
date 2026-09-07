%%%-------------------------------------------------------------------
%%% @doc Orphan collection in out_dir.
%%%
%%% An orphan is a generated .erl whose template is gone. It is found by a
%%% forward set difference -- everything in out_dir minus everything this run
%%% maps to -- and never by deriving a template path back from a module name,
%%% which is impossible: `_' does not remember whether it was a `/', a `-' or
%%% a `.'.
%%% @end
%%%-------------------------------------------------------------------
-module(rebar3_aihtml_gc).

-include("rebar3_aihtml.hrl").

-export([sweep/2]).

%% @doc Delete generated files with no template, return how many went.
sweep(Tpls, #mopts{out_dir = OutDir, ebin_dir = EbinDir, app_dir = AppDir}) ->
    case filelib:is_dir(OutDir) of
        false -> 0;
        true  ->
            Keep = [unicode:characters_to_list(P) || #tpl{out_path = P} <- Tpls],
            Present = lists:sort(filelib:wildcard(filename:join(OutDir, "*.erl"))),
            Orphans = [F || F <- Present, not lists:member(F, Keep)],
            %% Leftovers from a write that died between write_file and rename.
            Tmps = filelib:wildcard(filename:join(OutDir, "*.erl.tmp")),
            lists:foreach(fun(F) -> _ = file:delete(F) end, Tmps),
            lists:foldl(fun(F, N) -> N + remove(F, EbinDir, AppDir) end, 0, Orphans)
    end.

remove(File, EbinDir, AppDir) ->
    case ours(File) of
        false ->
            rebar_api:warn("mustache: ~ts is not a generated file and was left "
                           "alone; out_dir should contain generated code only",
                           [rel(File, AppDir)]),
            0;
        true ->
            %% info, not debug: deleting files is destructive and the user is
            %% entitled to see it in a default run.
            rebar_api:info("mustache: removing orphan ~ts", [rel(File, AppDir)]),
            _ = file:delete(File),
            %% The stale beam matters more than the stale source. Left behind,
            %% it stays on the code path and the deleted template keeps
            %% rendering happily, which is a genuinely hard bug to find.
            Beam = filename:join(EbinDir,
                                 filename:basename(File, ".erl") ++ ".beam"),
            _ = file:delete(Beam),
            1
    end.

rel(Abs, Dir) ->
    D = unicode:characters_to_list(Dir),
    P = unicode:characters_to_list(Abs),
    case lists:prefix(D ++ "/", P) of
        true  -> lists:nthtail(length(D) + 1, P);
        false -> P
    end.

%% Ours if the banner is there, or failing that if it carries a
%% -mustache_source attribute. Either alone is enough; a user's own file in
%% out_dir has neither and survives.
ours(File) ->
    case file:read_file(File) of
        {error, _} -> false;
        {ok, Bin} ->
            case binary:match(Bin, <<?R3A_BANNER>>) of
                {0, _} -> true;
                _      -> rebar3_aihtml_scan:meta(File) =/= error
            end
    end.
