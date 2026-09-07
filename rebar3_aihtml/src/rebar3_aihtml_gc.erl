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
sweep(Tpls, #mopts{out_dir = OutDir, ebin_dir = EbinDir, app_dir = AppDir,
                   engine = Engine}) ->
    case filelib:is_dir(OutDir) of
        false -> 0;
        true  ->
            Keep = [unicode:characters_to_list(P) || #tpl{out_path = P} <- Tpls],
            Present = lists:sort(filelib:wildcard(filename:join(OutDir, "*.erl"))),
            Orphans = [F || F <- Present, not lists:member(F, Keep)],
            %% Leftovers from a write that died between write_file and rename.
            Tmps = filelib:wildcard(filename:join(OutDir, "*.erl.tmp")),
            lists:foreach(fun(F) -> _ = file:delete(F) end, Tmps),
            lists:foldl(fun(F, N) -> N + remove(F, Engine, EbinDir, AppDir) end,
                        0, Orphans)
    end.

remove(File, Engine, EbinDir, AppDir) ->
    case classify(File, Engine) of
        foreign ->
            rebar_api:warn("aihtml: ~ts is not a generated file and was left "
                           "alone; out_dir should contain generated code only",
                           [rel(File, AppDir)]),
            0;
        other_engine ->
            %% Another engine sharing this out_dir owns it. Silently, because
            %% it is not the user's problem and the other provider will
            %% collect it if it really is an orphan.
            0;
        mine ->
            %% info, not debug: deleting files is destructive and the user is
            %% entitled to see it in a default run.
            rebar_api:info("aihtml: removing orphan ~ts", [rel(File, AppDir)]),
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

%% Whose file is this?
%%
%%   mine          this engine's banner, or this engine's source attribute
%%   other_engine  the plugin's banner, but another engine's tag
%%   foreign       neither; a user's own file, left alone with a warning
%%
%% Either the banner or the attribute is enough on its own, so a file whose
%% banner was hand-removed is still recognised. A file written before the
%% banner carried an engine tag is treated as mustache, which is the only
%% engine that existed then.
classify(File, Engine) ->
    case file:read_file(File) of
        {error, _} ->
            foreign;
        {ok, Bin} ->
            Mine = Engine:banner_tag(),
            case banner_of(Bin) of
                {tag, Mine}   -> mine;
                {tag, _Other} -> other_engine;
                none ->
                    case rebar3_aihtml_scan:meta(File, Engine) of
                        error -> foreign;
                        _     -> mine
                    end
            end
    end.

banner_of(Bin) ->
    Prefix = <<?R3A_BANNER_PREFIX>>,
    Legacy = <<?R3A_BANNER_LEGACY>>,
    case binary:match(Bin, Legacy) of
        {0, _} ->
            {tag, "mustache"};
        _ ->
            case binary:match(Bin, Prefix) of
                {0, _} -> tag_after(Bin, byte_size(Prefix));
                _      -> none
            end
    end.

tag_after(Bin, Pos) ->
    case binary:part(Bin, Pos, min(32, byte_size(Bin) - Pos)) of
        <<" (", Rest/binary>> ->
            case binary:match(Rest, <<")">>) of
                {End, 1} -> {tag, binary_to_list(binary:part(Rest, 0, End))};
                nomatch  -> none
            end;
        _ ->
            none
    end.
