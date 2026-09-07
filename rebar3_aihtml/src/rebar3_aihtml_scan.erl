%%%-------------------------------------------------------------------
%%% @doc Template discovery and generated-module introspection.
%%%
%%% Two jobs that both feed the incremental decision:
%%%
%%%   * walk views_dir and describe every template, including the build stamp
%%%     ai_mustache_compiler:source_hash/2 computes for it;
%%%   * read back what a previously generated .erl says about itself, namely
%%%     its -mustache_source attribute and its partials/0 list.
%%%
%%% There is no cache file anywhere in the plugin: the generated module is
%%% the cache, and it describes itself. Nothing here ever consults an mtime
%%% to decide whether to recompile (see rebar3_aihtml_prv for why).
%%% @end
%%%-------------------------------------------------------------------
-module(rebar3_aihtml_scan).

-include("rebar3_aihtml.hrl").

-export([templates/1, meta/1, stamp_of/1, partials_of/1]).

%% @doc Every template under views_dir, sorted by path.
%%
%% Sorted so that compilation order, and therefore the order of any error
%% report, is reproducible.
templates(Opts = #mopts{views_dir = Dir, suffix = Suffix}) ->
    Re = "\\." ++ escape_re(strip_dot(Suffix)) ++ "$",
    Files = filelib:fold_files(Dir, Re, true, fun(F, A) -> [F | A] end, []),
    Regular = [F || F <- lists:sort(Files), filelib:is_regular(F)],
    lists:foldr(fun(F, Acc) -> [describe(F, Opts) | Acc] end, [], Regular).

strip_dot("." ++ Rest) -> Rest;
strip_dot(S)           -> S.

escape_re(S) ->
    lists:flatmap(fun(C) ->
                          case lists:member(C, ".^$*+?()[]{}|\\-") of
                              true  -> [$\\, C];
                              false -> [C]
                          end
                  end, S).

describe(Abs, Opts = #mopts{app_dir = AppDir, views_dir = ViewsDir,
                            suffix = Suffix, compiler_opts = CO}) ->
    Name = rebar3_aihtml_name:name_of(Abs, ViewsDir, Suffix),
    Mod  = rebar3_aihtml_name:module_of(Name, Opts),
    Rel  = relative(Abs, AppDir),
    case file:read_file(Abs) of
        {error, Posix} ->
            throw({rebar3_aihtml, {errors, [{Rel, 0, {read_failed, Posix}}]}});
        {ok, Body} ->
            #tpl{abs_path = Abs,
                 rel_path = Rel,
                 name     = Name,
                 module   = Mod,
                 out_path = rebar3_aihtml_name:out_file(Mod, Opts),
                 source   = Body,
                 %% The one and only place a stamp comes from. Computing it
                 %% here instead of in the plugin keeps the plugin and
                 %% ai_mustache_dev from ever disagreeing about freshness.
                 stamp    = ai_mustache_compiler:source_hash(Body, CO),
                 mtime    = mtime(Abs)}
    end.

mtime(File) ->
    case file:read_file_info(File, [{time, posix}]) of
        {ok, Info} -> element(6, Info);   % #file_info.mtime
        _          -> 0
    end.

relative(Abs, Dir) ->
    D = unicode:characters_to_list(Dir),
    P = unicode:characters_to_list(Abs),
    case lists:prefix(D ++ "/", P) of
        true  -> lists:nthtail(length(D) + 1, P);
        false -> P
    end.

%%%===================================================================
%%% Reading a generated module back
%%%===================================================================

%% @doc What a generated .erl says about itself.
%%
%% Returns #{stamp => binary(), source => map(), partials => [module()]} or
%% `error' if the file is absent, truncated, hand-mangled or simply not one of
%% ours. `error' always means "recompile it", never "abort the build": a
%% corrupted generated file must heal itself on the next run.
%%
%% Deliberately neither compile:file/2 nor a beam load. The beam on disk may
%% be from an older build and would answer for the wrong source. Scanning is
%% incremental and stops as soon as partials/0 has been seen, so the (possibly
%% very large) render body is never tokenised.
meta(File) ->
    case file:read_file(File) of
        {error, _} -> error;
        {ok, Bin}  ->
            %% Generated files are UTF-8, so decode before scanning. Reading
            %% them as bytes would split a multi-byte character inside a
            %% <<"..."/utf8>> literal and the tokeniser would give up.
            case unicode:characters_to_list(Bin, utf8) of
                Chars when is_list(Chars) -> forms(Chars, 1, #{});
                _                         -> error
            end
    end.

forms(Chars, Loc, Acc) ->
    case erl_scan:tokens([], Chars, Loc) of
        {done, {ok, Toks, Loc1}, Rest} ->
            case erl_parse:parse_form(Toks) of
                {ok, Form} -> absorb(Form, Rest, Loc1, Acc);
                {error, _} -> error
            end;
        _ ->
            %% eof, scan error, or an incomplete trailing form.
            finish(Acc)
    end.

absorb({attribute, _, mustache_source, Map}, Rest, Loc, Acc) when is_map(Map) ->
    forms(Rest, Loc, Acc#{source => Map});
absorb({function, _, partials, 0, [{clause, _, [], [], [Expr]}]}, _Rest, _Loc, Acc) ->
    %% partials/0 is the last header form we care about; stop here.
    try erl_parse:normalise(Expr) of
        L when is_list(L) -> finish(Acc#{partials => L});
        _                 -> error
    catch _:_ -> error
    end;
absorb(_Form, Rest, Loc, Acc) ->
    forms(Rest, Loc, Acc).

finish(Acc) ->
    case Acc of
        #{source := #{stamp := Stamp}} when is_binary(Stamp) ->
            {ok, Acc#{stamp => Stamp, partials => maps:get(partials, Acc, [])}};
        _ ->
            error
    end.

%% @doc The recorded build stamp, or `error'.
stamp_of(File) ->
    case meta(File) of
        {ok, #{stamp := S}} -> {ok, S};
        error               -> error
    end.

%% @doc The modules a generated module calls as partials, or `error'.
partials_of(File) ->
    case meta(File) of
        {ok, #{partials := P}} -> {ok, P};
        error                  -> error
    end.
