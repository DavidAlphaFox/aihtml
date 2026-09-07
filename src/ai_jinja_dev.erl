%%%-------------------------------------------------------------------
%%% @doc Development-time hot reloading of compiled templates.
%%%
%%% There is deliberately no cache here: no ets, no persistent_term, no
%%% process, no cache file. The BEAM code server already is the cache. A
%%% loaded module is a compiled template, its -jinja_source attribute is
%%% the cache key, and the file on disk is the source. Anything else would be
%%% duplicated state -- exactly what the old loader's gen_server plus ets plus
%%% process dictionary arrangement was, and what this refactor removed.
%%%
%%% Because of that constraint there is nowhere to keep an "enabled" flag, so
%%% this module has no global switch. check/0 is a one-shot environment
%%% self-test; reloading is triggered explicitly by the caller -- typically a
%%% dev-only middleware, an editor save hook, or a call at the top of a
%%% request handler. Calling check/0 alone does nothing on its own.
%%%
%%% This module must not be referenced from any other module under src/: it is
%%% a development aid and has no place in a production build.
%%%
%%% It is a near-copy of ai_mustache_dev, and deliberately not a shared module
%%% parameterised by an engine: each is about 200 lines, they read different
%%% attributes and call different compilers, and the only caller is a line the
%%% user writes by hand in a request handler -- who would then have to pass an
%%% engine argument. ai_html_dev:reload_all/0 is the one place the two meet.
%%% @end
%%%-------------------------------------------------------------------
-module(ai_jinja_dev).

-include("ai_jinja.hrl").
-include_lib("kernel/include/file.hrl").

-export([check/0, stale/1, reload/1, reload/2, source/1, template_modules/0]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc One-shot environment self-test.
%%
%% Confirms the compiler is reachable and that every loaded template module
%% points at a readable file. It is NOT a switch: nothing starts watching
%% anything as a result of calling it.
-spec check() -> ok | {error, term()}.
check() ->
    case code:ensure_loaded(ai_jinja_compiler) of
        {error, R} ->
            {error, {compiler_unavailable, R}};
        {module, _} ->
            %% A module compiled from a string has no file to go stale
            %% against; it is skipped rather than reported missing.
            case [{M, P} || M <- template_modules(),
                            {ok, #{path := P} = S} <- [source(M)],
                            maps:get(origin, S, file) =:= file,
                            not filelib:is_regular(P)] of
                []      -> ok;
                Missing -> {error, {missing_templates, Missing}}
            end
    end.

%% @doc Every loaded module that carries a -jinja_source attribute.
-spec template_modules() -> [module()].
template_modules() ->
    [M || {M, _} <- code:all_loaded(), is_template(M)].

-spec is_template(module()) -> boolean().
is_template(M) ->
    case source(M) of
        {ok, _} -> true;
        _       -> false
    end.

%% @doc Read a generated module's self-description.
-spec source(module()) -> {ok, ai_jinja_source()} | {error, not_a_template_module}.
source(Mod) ->
    try Mod:module_info(attributes) of
        Attrs ->
            %% Attribute values arrive wrapped in a list by module_info/1.
            case lists:keyfind(jinja_source, 1, Attrs) of
                {jinja_source, [#{path := _, stamp := _} = S]} -> {ok, S};
                {jinja_source, #{path := _, stamp := _} = S}    -> {ok, S};
                _ -> {error, not_a_template_module}
            end
    catch
        _:_ -> {error, not_a_template_module}
    end.

%% @doc Whether the template on disk differs from what the module was built
%% from.
%%
%% The stamp -- content plus normalised options plus compiler version -- is the
%% only thing consulted, and it is computed by the very same
%% ai_jinja_compiler:source_hash/2 the plugin uses. Two independent
%% implementations would drift, and dev would then rebuild forever while the
%% plugin rebuilt nothing.
%%
%% mtime deliberately plays no part. It is tempting as a cheap "unchanged
%% mtime means not stale" filter, but POSIX mtime has one-second granularity
%% and the edit-then-reload loop this module exists to serve routinely happens
%% inside one second, so the filter would answer "fresh" for a file that was
%% just edited. Reading a template and hashing it costs microseconds; a wrong
%% answer costs a confusing debugging session. The mtime in the attribute is
%% kept for diagnostics only.
-spec stale(module()) -> boolean() | {error, term()}.
stale(Mod) ->
    case source(Mod) of
        {error, _} = E ->
            E;
        {ok, #{path := Path, stamp := Stamp, vsn := Vsn} = Src} ->
            case Vsn =/= ?AI_JINJA_VSN of
                %% The compiler's output shape changed; everything is stale.
                true  -> true;
                false -> stale_by_stamp(Path, Stamp, Src)
            end
    end.

-spec stale_by_stamp(binary() | string(), binary(), ai_jinja_source()) ->
          boolean() | {error, term()}.
stale_by_stamp(Path, Stamp, #{opts := Opts}) ->
    case file:read_file(Path) of
        {error, R} -> {error, {R, Path}};
        {ok, Body} -> ai_jinja_compiler:source_hash(Body, Opts) =/= Stamp
    end.

%% @doc Recompile and reload a template module, or every stale one.
-spec reload(module() | all) -> ok | {error, term()}.
reload(all) ->
    reload_list([M || M <- template_modules(), stale(M) =:= true]);
reload(Mod) ->
    case source(Mod) of
        {error, _} = E          -> E;
        {ok, #{opts := Opts}}   -> reload(Mod, Opts)
    end.

%% @doc As reload/1 but with explicit compile options.
%%
%% reload/1 recovers the options from the module's own -jinja_source, which
%% is why that attribute carries them: without them a reload could not
%% reproduce the compilation the plugin originally performed, and the
%% hot-loaded module would quietly differ from the built one.
-spec reload(module(), map() | [{atom(), term()}]) -> ok | {error, term()}.
reload(Mod, Opts0) ->
    case source(Mod) of
        {error, _} = E ->
            E;
        {ok, #{path := Path}} ->
            case file:read_file(Path) of
                {error, R} -> {error, {R, Path}};
                {ok, Body} -> rebuild(Mod, Path, Body, Opts0)
            end
    end.

-spec reload_list([module()]) -> ok | {error, term()}.
reload_list([]) ->
    ok;
reload_list([M | Rest]) ->
    case reload(M) of
        ok             -> reload_list(Rest);
        {error, _} = E -> E
    end.

%%%===================================================================
%%% Internal
%%%===================================================================

-spec rebuild(module(), binary() | string(), binary(),
              map() | [{atom(), term()}]) -> ok | {error, term()}.
rebuild(Mod, Path, Body, Recorded) ->
    %% The attribute records the options as a sorted list, so that the
    %% generated file does not depend on map iteration order; here they go
    %% back to being a map.
    Opts0 = case Recorded of
                L when is_list(L) -> maps:from_list(L);
                M when is_map(M)  -> M
            end,
    Opts = Opts0#{module => Mod,
                  source => iolist_to_binary(Path),
                  stamp  => ai_jinja_compiler:source_hash(Body, Opts0),
                  mtime  => mtime(Path)},
    case ai_jinja_parser:parse(Body, Opts) of
        {error, _} = E -> E;
        {ok, Ast} ->
            case ai_jinja_compiler:forms(Ast, Opts) of
                {error, _} = E     -> E;
                {ok, Forms, _Deps} -> load(Mod, Path, Forms)
            end
    end.

-spec load(module(), binary() | string(), [erl_parse:abstract_form()]) ->
          ok | {error, term()}.
load(Mod, Path, Forms) ->
    case compile:forms(Forms, [return_errors, binary, debug_info]) of
        {error, Errors, _Warnings} ->
            {error, {codegen_failed, Errors}};
        {ok, Mod, Bin} ->
            %% soft_purge first so a process still running old code is not
            %% killed mid-render; fall back to a hard purge only if needed.
            case code:soft_purge(Mod) of
                true  -> ok;
                false -> _ = code:purge(Mod), ok
            end,
            case code:load_binary(Mod, unicode:characters_to_list(Path), Bin) of
                {module, Mod}  -> ok;
                {error, R}     -> {error, {load_failed, R}}
            end
    end.

-spec mtime(binary() | string()) -> integer().
mtime(Path) ->
    case file:read_file_info(Path, [{time, posix}]) of
        {ok, #file_info{mtime = M}} -> M;
        _                           -> 0
    end.
