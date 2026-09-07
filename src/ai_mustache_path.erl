%%%-------------------------------------------------------------------
%%% @doc Template path resolution and static `-mustache_template' scanning.
%%%
%%% Two consumers share this module and they must agree byte for byte:
%%%
%%% <ul>
%%%   <li>`ai_mustache_transform' resolves the path of every
%%%       `-mustache_template' it is about to compile;</li>
%%%   <li>the rebar3 plugin resolves the same paths when it builds the reverse
%%%       dependency table that lets it touch a `.erl' whose template
%%%       changed. A parse_transform cannot register extra file dependencies
%%%       with rebar3 -- only `-include' is tracked -- so without that touch a
%%%       template edit does not rebuild the module that embeds it.</li>
%%% </ul>
%%%
%%% If the two used different search orders the plugin would touch one file
%%% while the transform read another and the fallback would quietly do nothing.
%%%
%%% See tasks/T25.md and designs/05-rebar3-plugin.md.
%%% @end
%%%-------------------------------------------------------------------
-module(ai_mustache_path).

-export([resolve/2, template_spec/1, scan/1]).

-type template() :: {Name :: atom(), Path :: string()}.
-export_type([template/0]).

%%%===================================================================
%%% Resolution
%%%===================================================================

%% @doc Find Path in the first of Dirs that has it.
%%
%% An absolute path is used as given. The caller supplies Dirs in priority
%% order; the transform uses "directory of the .erl" then the `views' option
%% then every `{i, Dir}' then the cwd. On failure the full list of directories
%% actually tried comes back with the error, because a bare "not found" leaves
%% the user with nowhere to start.
-spec resolve(file:filename_all(), [file:filename_all()]) ->
          {ok, file:filename_all()} | {error, {not_found, [file:filename_all()]}}.
resolve(Path, Dirs) ->
    case filename:pathtype(Path) of
        absolute ->
            case filelib:is_regular(Path) of
                true  -> {ok, Path};
                false -> {error, {not_found, [filename:dirname(Path)]}}
            end;
        _ ->
            Tried = [D || D <- Dirs, D =/= undefined, D =/= ""],
            try_dirs(Path, Tried, Tried)
    end.

-spec try_dirs(file:filename_all(), [file:filename_all()],
               [file:filename_all()]) ->
          {ok, file:filename_all()} | {error, {not_found, [file:filename_all()]}}.
try_dirs(_Path, [], Tried) ->
    {error, {not_found, Tried}};
try_dirs(Path, [D | Rest], Tried) ->
    Candidate = filename:join(D, Path),
    case filelib:is_regular(Candidate) of
        true  -> {ok, Candidate};
        false -> try_dirs(Path, Rest, Tried)
    end.

%%%===================================================================
%%% Attribute shapes
%%%===================================================================

%% @doc Read a `-mustache_template' attribute term.
%%
%% Two shapes are accepted and both consumers must recognise both:
%% ``{Name, "views/index.mustache"}'' names the generated function outright,
%% while a bare ``"views/index.mustache"'' derives `index' from the basename.
-spec template_spec(term()) -> {ok, template()} | error.
template_spec({Name, Path}) when is_atom(Name) ->
    case to_path(Path) of
        error      -> error;
        {ok, Str}  -> {ok, {Name, Str}}
    end;
template_spec(Path) ->
    case to_path(Path) of
        error     -> error;
        {ok, Str} -> {ok, {name_of(Str), Str}}
    end.

-spec to_path(term()) -> {ok, string()} | error.
to_path(P) when is_binary(P) -> to_path(unicode:characters_to_list(P));
to_path([])                  -> error;
to_path(P) when is_list(P)   ->
    case lists:all(fun(C) -> is_integer(C) andalso C >= 0 end, P) of
        true  -> {ok, P};
        false -> error
    end;
to_path(_) -> error.

%% The function name derived from a path: basename without extension, with
%% anything that cannot appear in an atom folded to `_'.
-spec name_of(string()) -> atom().
name_of(Path) ->
    Base = filename:rootname(filename:basename(Path)),
    list_to_atom([sanitise(C) || C <- Base]).

-spec sanitise(char()) -> char().
sanitise(C) when C >= $a, C =< $z -> C;
sanitise(C) when C >= $0, C =< $9 -> C;
sanitise(C) when C >= $A, C =< $Z -> C + 32;
sanitise(_)                       -> $_.

%%%===================================================================
%%% Static scanning
%%%===================================================================

%% @doc Read the `-mustache_template' attributes out of an `.erl' source file.
%%
%% The plugin needs this before anything is compiled, so it works on text
%% rather than on a beam. Macros are left unexpanded (epp_dodger), which is
%% fine: a template path spelled as a macro cannot be resolved statically and
%% is skipped rather than guessed at.
-spec scan(file:filename_all()) -> {ok, [template()]} | {error, term()}.
scan(File) ->
    case epp_dodger:quick_parse_file(File) of
        {ok, Forms} ->
            {ok, [T || {attribute, _, mustache_template, Term} <- Forms,
                       {ok, T} <- [template_spec(Term)]]};
        {error, _} = E ->
            E
    end.
