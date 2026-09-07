%%%-------------------------------------------------------------------
%%% @doc mustache_opts parsing and normalisation.
%%%
%%% One #mopts{} per app. App level configuration overrides project level key
%%% by key, so an umbrella can set a shared default at the top and let a
%%% single app deviate on, say, prefix.
%%% @end
%%%-------------------------------------------------------------------
-module(rebar3_aihtml_opts).

-include("rebar3_aihtml.hrl").

-export([for_app/3, raw/3, normalize/4, known_keys/1]).

%% @doc Build the normalised options for one app, or skip it.
%%
%% An app without a views directory is skipped silently: most apps in an
%% umbrella have no templates, and making them all configure their way out of
%% the plugin would be noise. out_dir is not created here either -- nothing to
%% generate means nothing to create.
for_app(AppInfo, State, Engine) ->
    AppDir = rebar_app_info:dir(AppInfo),
    Raw = raw(AppInfo, State, Engine),
    Opts = normalize(Raw, AppDir, AppInfo, Engine),
    case filelib:is_dir(Opts#mopts.views_dir) of
        true  -> {ok, Opts};
        false -> skip
    end.

%% @doc The engine's options block, with app level entries overriding project
%% level ones.
%%
%% Three sources, each overriding the previous key by key: the project's
%% rebar.config, whatever rebar3 resolved for the app, and the app's own
%% rebar.config read directly.
%%
%% That last one is not redundant. rebar3 does not fold an unrecognised
%% top-level key such as mustache_opts from an umbrella member's rebar.config
%% into that app's opts, so rebar_app_info:get/3 hands back the project's value
%% and `apps/b/rebar.config' would have no effect at all. It is still consulted
%% first, because a profile or an `overrides' directive does reach it and that
%% must not be thrown away.
raw(AppInfo, State, Engine) ->
    Key = Engine:config_key(),
    Project = to_proplist(rebar_state:get(State, Key, [])),
    Resolved = to_proplist(rebar_app_info:get(AppInfo, Key, [])),
    Own = own_config(rebar_app_info:dir(AppInfo), Key),
    lists:foldl(fun({K, V}, Acc) -> lists:keystore(K, 1, Acc, {K, V}) end,
                Project, Resolved ++ Own).

own_config(AppDir, Key) ->
    case file:consult(filename:join(AppDir, "rebar.config")) of
        {ok, Terms} -> to_proplist(proplists:get_value(Key, Terms, []));
        {error, _}  -> []
    end.

to_proplist(L) when is_list(L) -> [normalise_entry(E) || E <- L];
to_proplist(M) when is_map(M)  -> maps:to_list(M);
to_proplist(_)                 -> [].

%% {views, "v"} and a bare atom flag such as `line_map' are both accepted.
normalise_entry({K, V}) -> {K, V};
normalise_entry(K) when is_atom(K) -> {K, true};
normalise_entry(Other) -> {'$invalid', Other}.

known_keys(Engine) -> Engine:known_keys().

%% @doc Turn a raw proplist into #mopts{}.
normalize(Raw, AppDir, AppInfo, Engine) ->
    Wae = boolean_opt(warnings_as_errors, Raw, false),
    ok = check_unknown(Raw, Wae, Engine),
    Views  = string_opt(views,   Raw, ?R3A_DEFAULT_VIEWS),
    OutRel = string_opt(out_dir, Raw, ?R3A_DEFAULT_OUT_DIR),
    Suffix = string_opt(suffix,  Raw, Engine:default_suffix()),
    Prefix = prefix(Raw, Engine),
    Exts   = extensions(Raw),
    ExtOpts = ext_opts(Raw),
    LineMap = boolean_opt(line_map, Raw, true),
    #mopts{engine     = Engine,
           app_name   = rebar_app_info:name(AppInfo),
           app_dir    = AppDir,
           ebin_dir   = rebar_app_info:ebin_dir(AppInfo),
           src_dirs   = src_dirs(AppInfo, AppDir),
           views_dir  = filename:join(AppDir, Views),
           views_rel  = Views,
           out_dir    = filename:join(AppDir, OutRel),
           out_rel    = OutRel,
           suffix     = Suffix,
           prefix     = Prefix,
           extensions = Exts,
           ext_opts   = ExtOpts,
           line_map   = LineMap,
           wae        = Wae,
           %% Only code shaping options, and no absolute paths: this map is
           %% written verbatim into every generated module's -mustache_source
           %% and hashed into its stamp.
           %%
           %% `views' is deliberately absent. ai_mustache_ast:resolve_partials/2
           %% only uses it to probe the file system for a partial, and it would
           %% probe relative to rebar3's cwd (the project root), which is wrong
           %% for an umbrella app. rebar3_aihtml_check does that check instead,
           %% against the template set the plugin already scanned, which is both
           %% correct under umbrellas and independent of the cwd.
           engine_opts = maps:merge(#{prefix     => Prefix,
                                      extensions => lists:usort(Exts),
                                      ext_opts   => ExtOpts,
                                      line_map   => LineMap},
                                    engine_flags(Raw, Engine))}.

%% The booleans an engine adds of its own. Every one of them changes the
%% generated code, so every one is in the stamp; a key the engine does not
%% declare is simply not there, and check_unknown/3 has already rejected it.
engine_flags(Raw, Engine) ->
    Known = Engine:known_keys(),
    Flags = [{escape, true}, {trim_blocks, true}, {lstrip_blocks, true},
             {keep_trailing_newline, false}, {strict_undefined, false}],
    maps:from_list([{K, boolean_opt(K, Raw, D)}
                    || {K, D} <- Flags, lists:member(K, Known)]).

src_dirs(AppInfo, AppDir) ->
    Dirs = try rebar_dir:src_dirs(rebar_app_info:opts(AppInfo), ["src"])
           catch _:_ -> ["src"] end,
    [filename:join(AppDir, D) || D <- Dirs].

check_unknown(Raw, Wae, Engine) ->
    Known = known_keys(Engine),
    case [K || {K, _} <- Raw, not lists:member(K, Known)] of
        [] -> ok;
        Unknown ->
            %% Naming the block matters when two are configured: a mustache
            %% key written into jinja_opts is otherwise a mystery.
            Msg = io_lib:format(
                    "unknown ~p key(s) ~p; supported keys are ~p",
                    [Engine:config_key(), Unknown, Known]),
            case Wae of
                true  -> throw({rebar3_aihtml, {bad_opts, lists:flatten(Msg)}});
                false -> rebar_api:warn("mustache: ~s", [Msg]), ok
            end
    end.

string_opt(Key, Raw, Default) ->
    case proplists:get_value(Key, Raw, Default) of
        B when is_binary(B) -> binary_to_list(B);
        L when is_list(L)   -> L;
        Other -> throw({rebar3_aihtml, {bad_opt, Key, Other}})
    end.

boolean_opt(Key, Raw, Default) ->
    case proplists:get_value(Key, Raw, Default) of
        B when is_boolean(B) -> B;
        Other -> throw({rebar3_aihtml, {bad_opt, Key, Other}})
    end.

%% The prefix has to make the module name a bare Erlang atom. An empty prefix
%% is legal, but then the template name itself has to start with a lowercase
%% letter -- rebar3_aihtml_name enforces that half.
prefix(Raw, Engine) ->
    P = string_opt(prefix, Raw,
                   unicode:characters_to_list(Engine:default_prefix())),
    case valid_prefix(P) of
        true  -> unicode:characters_to_binary(P);
        false -> throw({rebar3_aihtml, {bad_prefix, P}})
    end.

valid_prefix("") -> true;
valid_prefix([C | Rest]) when C >= $a, C =< $z -> lists:all(fun tail_char/1, Rest);
valid_prefix(_) -> false.

tail_char(C) when C >= $a, C =< $z -> true;
tail_char(C) when C >= $A, C =< $Z -> true;
tail_char(C) when C >= $0, C =< $9 -> true;
tail_char($_) -> true;
tail_char(_)  -> false.

%% Extensions have to be loadable inside the rebar3 VM, not merely inside the
%% target app: the plugin runs them at build time. Marker conflict detection
%% belongs to T23; here we only make sure the modules exist.
extensions(Raw) ->
    Mods = proplists:get_value(extensions, Raw, []),
    is_list(Mods) orelse throw({rebar3_aihtml, {bad_opt, extensions, Mods}}),
    lists:foreach(
      fun(M) when is_atom(M) ->
              case code:ensure_loaded(M) of
                  {module, M} -> ok;
                  {error, R}  -> throw({rebar3_aihtml, {ext_not_loadable, M, R}})
              end;
         (Other) ->
              throw({rebar3_aihtml, {bad_opt, extensions, Other}})
      end, Mods),
    Mods.

ext_opts(Raw) ->
    case proplists:get_value(ext_opts, Raw, #{}) of
        M when is_map(M) -> M;
        L when is_list(L) -> maps:from_list(L);
        Other -> throw({rebar3_aihtml, {bad_opt, ext_opts, Other}})
    end.
