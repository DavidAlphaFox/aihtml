%%%-------------------------------------------------------------------
%%% @doc Template-attribute staleness fallback.
%%%
%%% The parse_transform's "compile entry point" form (designs/06 4) creates a
%%% dependency rebar3 cannot see: my_views.erl reads views/index.mustache at
%%% compile time, and a parse_transform has no way to register that with
%%% rebar3 -- only -include is tracked. So editing the template would not
%%% rebuild the module. The plugin closes the gap by touching the .erl.
%%%
%%% Both the scanning of the attribute and the resolution of its path go
%%% through ai_html_path, which each engine's parse_transform also uses. That
%%% is the whole point of that module: if the two search orders diverged, the
%%% plugin would touch one file while the transform read another and this
%%% fallback would silently do nothing.
%%%
%%% Which attribute to look for comes from the engine, so `-jinja_template'
%%% works exactly as `-mustache_template' does.
%%%
%%% NOTE ON MTIME. Everywhere else in this plugin freshness is decided by a
%%% content stamp and mtime is explicitly ignored. Here it is the only thing
%%% available, and that is not an inconsistency: the target is a hand written
%%% .erl with nowhere to record a stamp, and rebar3's own erl -> beam step is
%%% itself mtime driven, so mtime is exactly the currency that matters. Do not
%%% "unify" this with the stamp logic.
%%% @end
%%%-------------------------------------------------------------------
-module(rebar3_aihtml_stale).

-include("rebar3_aihtml.hrl").

-export([touch/2, sources/1]).

%% @doc Touch every .erl whose template is newer than it. {Touched, Errors}.
touch(Opts, IncDirs) ->
    lists:foldl(fun(F, Acc) -> one(F, Opts, IncDirs, Acc) end,
                {0, []}, sources(Opts)).

%% @doc Every .erl under the app's source directories.
sources(#mopts{src_dirs = SrcDirs}) ->
    lists:usort(lists:append([filelib:wildcard(filename:join([D, "**", "*.erl"]))
                              || D <- SrcDirs])).

one(Erl, Opts = #mopts{app_dir = AppDir, engine = Engine}, IncDirs, {N, Errs}) ->
    case attributes(Erl, Engine) of
        [] -> {N, Errs};
        Attrs ->
            Rel = rel(Erl, AppDir),
            lists:foldl(
              fun({_Name, Path}, {N1, E1}) ->
                      case ai_html_path:resolve(Path, dirs(Erl, Opts, IncDirs)) of
                          {error, {not_found, _}} ->
                              %% Line 0: ai_html_path:scan/1 works on the
                              %% form list and does not report where each
                              %% attribute sat. The path itself identifies it.
                              {N1, [{Rel, 0, {template_not_found, Path}} | E1]};
                          {ok, Tpl} ->
                              {N1 + maybe_touch(Tpl, Erl), E1}
                      end
              end, {N, Errs}, Attrs)
    end.

%% The transform's order is: directory of the .erl, the raw `views' option,
%% every -I, then the cwd. Mirrored here, with the app-absolute views
%% directory added after the raw one. The raw option is resolved against the
%% cwd, which under an umbrella is the project root rather than the app, so on
%% its own it would miss the app's own templates; the extra entry can only
%% find more files, never different ones.
dirs(Erl, #mopts{views_rel = Rel, views_dir = Abs}, IncDirs) ->
    [filename:dirname(Erl), Rel, Abs] ++ IncDirs ++ ["."].

maybe_touch(Tpl, Erl) ->
    case {mtime(Tpl), mtime(Erl)} of
        {T, E} when T > E, T > 0 ->
            rebar_api:debug("aihtml: touching ~ts (template ~ts is newer)",
                            [Erl, Tpl]),
            %% Only when it is actually stale. Touching unconditionally would
            %% rebuild these modules on every single `rebar3 compile'.
            _ = file:change_time(Erl, calendar:local_time()),
            1;
        _ ->
            0
    end.

%% Running epp_dodger over every source file in a project would be wasteful
%% when almost none of them mention the attribute, so cheap-check the text
%% first. A file that cannot be parsed contributes nothing rather than failing
%% the build: it is the compiler's job to complain about it, not ours.
attributes(File, Engine) ->
    Attr = template_attribute(Engine),
    case file:read_file(File) of
        {error, _} -> [];
        {ok, Bin} ->
            case binary:match(Bin, atom_to_binary(Attr, utf8)) of
                nomatch -> [];
                _ ->
                    case ai_html_path:scan(File, Attr) of
                        {ok, Templates} -> Templates;
                        {error, _}      -> []
                    end
            end
    end.

%% -mustache_template / -jinja_template. Derived from the engine's banner tag
%% rather than added as a tenth callback: the two names are the same word.
template_attribute(Engine) ->
    list_to_atom(Engine:banner_tag() ++ "_template").

mtime(File) ->
    case file:read_file_info(File, [{time, posix}]) of
        {ok, Info} -> element(6, Info);
        _          -> 0
    end.

rel(Abs, Dir) ->
    D = unicode:characters_to_list(Dir),
    P = unicode:characters_to_list(Abs),
    case lists:prefix(D ++ "/", P) of
        true  -> lists:nthtail(length(D) + 1, P);
        false -> P
    end.
