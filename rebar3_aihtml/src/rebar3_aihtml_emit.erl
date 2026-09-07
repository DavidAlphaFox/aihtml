%%%-------------------------------------------------------------------
%%% @doc Template -> generated .erl text, and atomic write-out.
%%%
%%% The pipeline is parser -> ai_mustache_compiler:forms/2 -> erl_prettypr.
%%% forms/2 runs ai_mustache_ast:postprocess/2 itself, so there is no separate
%%% post-processing step here, and it returns no {eof, _} terminator.
%%% @end
%%%-------------------------------------------------------------------
-module(rebar3_aihtml_emit).

-include("rebar3_aihtml.hrl").

-export([render/3, write/2]).

%% @doc Compile one template to the exact bytes of its .erl.
%%
%% Available is the set of views-relative template names in this app, used to
%% resolve partials; see rebar3_aihtml_check:partials_exist/3 for why the
%% check does not go through the compiler's `views' option.
render(#tpl{source = Body, rel_path = Rel, module = Mod, stamp = Stamp,
            mtime = Mtime},
       #mopts{compiler_opts = CO}, Available) ->
    Opts = CO#{module => Mod,
               source => unicode:characters_to_binary(Rel),
               %% stamp and mtime reach the -mustache_source attribute through
               %% here; the compiler never stats a file itself. Neither key is
               %% part of normalize_opts/1, so feeding the stamp back in cannot
               %% make the stamp depend on itself.
               stamp  => Stamp,
               mtime  => Mtime},
    case ai_mustache_parser:parse(Body, Opts) of
        {error, _} = E -> E;
        {ok, Nodes} ->
            case rebar3_aihtml_check:partials_exist(Nodes, Available, Rel) of
                []   -> compile(Nodes, Opts, Rel);
                Errs -> {errors, Errs}
            end
    end.

compile(Nodes, Opts, Rel) ->
    case ai_mustache_compiler:forms(Nodes, Opts) of
        {error, _} = E     -> E;
        {ok, Forms, _Deps} -> {ok, text(Forms, Rel)}
    end.

text(Forms, Rel) ->
    Body = erl_prettypr:format(erl_syntax:form_list(Forms)),
    %% erl_prettypr drops comments, so the banner is prepended as text rather
    %% than smuggled in as a form.
    %%
    %% The file is written as UTF-8 with no coding directive, which is erlc's
    %% default. ai_mustache_compiler emits non-ASCII static text as
    %% <<"..."/utf8>> -- characters with the utf8 type, not raw bytes -- so it
    %% round-trips through the source file exactly. (An earlier arrangement
    %% wrote byte-per-character literals and needed `%% coding: latin-1' to
    %% survive; that is no longer the case, and declaring latin-1 now would
    %% itself corrupt the output.)
    Header = [?R3A_BANNER, Rel, ?R3A_DONT_EDIT, "\n",
              "%%\n",
              "%% The `stamp' of -mustache_source is the combined digest of the\n"
              "%% template body, the normalised compile options and the generated\n"
              "%% code version -- not a digest of the template text. `mtime' is\n"
              "%% recorded for diagnostics only; nothing decides anything from it.\n\n"],
    unicode:characters_to_binary([Header, Body, "\n"]).

%% @doc Write Bin to Path, atomically, and only if it would change the file.
%%
%% Skipping an identical write keeps the mtime stable, which matters because
%% rebar3's own .erl -> .beam step is mtime driven: rewriting byte-identical
%% files would make every `rebar3 mustache --force' recompile the world.
%%
%% The temporary file is a sibling so that rename/2 stays within one file
%% system and is therefore atomic; a crash mid-write leaves the old file
%% intact rather than a truncated new one.
write(Path, Bin) ->
    case file:read_file(Path) of
        {ok, Bin} -> unchanged;
        _         -> do_write(Path, Bin)
    end.

do_write(Path0, Bin) ->
    Path = unicode:characters_to_list(Path0),
    Tmp = Path ++ ".tmp",
    %% Nothing here raises. A read-only out_dir is a plain error the provider
    %% reports, not a badmatch with a stack trace, and a failure between
    %% write_file and rename leaves the previous generation intact rather than
    %% a truncated file. Whatever .tmp survives a crash is swept by
    %% rebar3_aihtml_gc on the next run.
    case filelib:ensure_dir(Path) of
        {error, Posix} ->
            {error, {write_failed, Path, Posix}};
        ok ->
            case file:write_file(Tmp, Bin) of
                {error, Posix} ->
                    {error, {write_failed, Path, Posix}};
                ok ->
                    case file:rename(Tmp, Path) of
                        ok -> written;
                        {error, Posix} ->
                            _ = file:delete(Tmp),
                            {error, {write_failed, Path, Posix}}
                    end
            end
    end.
