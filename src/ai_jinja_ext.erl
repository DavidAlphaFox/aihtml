%%%-------------------------------------------------------------------
%%% @doc The jinja extension behaviour: user-defined filters and tests.
%%%
%%% Deliberately much narrower than the mustache extension behaviour. A
%%% mustache extension takes part in code generation (`compile_tag/4' returns
%%% an abstract expression); a jinja filter is an ordinary function of two
%%% arguments. Custom `{% tag %}' statements are not offered at all, because
%%% designing that interface without a real requirement driving it would
%%% almost certainly have to be redone (designs/12-jinja-toolchain.md 5).
%%%
%%%     -module(my_filters).
%%%     -behaviour(ai_jinja_ext).
%%%     -export([filters/0, tests/0, params/0, money/2]).
%%%
%%%     filters() -> #{money => {?MODULE, money}}.
%%%     tests()   -> #{}.
%%%     params()  -> #{money => [currency]}.
%%%
%%%     money(V, Args) -> ...
%%%
%%% `params/0' names the positional parameters, so that `x|money("EUR")' and
%%% `x|money(currency="EUR")' reach the function the same way.
%%% @end
%%%-------------------------------------------------------------------
-module(ai_jinja_ext).

-include("ai_jinja.hrl").

-callback filters() -> #{atom() => {module(), atom()}}.
-callback tests()   -> #{atom() => {module(), atom()}}.
-callback params()  -> #{atom() => [atom()]}.

-optional_callbacks([params/0]).

-export([registry/1, lookup/2, builtin/1, format_error/1]).
-export([spec_module/1]).

-type kind() :: filter | test.
-type table() :: #{atom() => {module(), atom()}}.
-type registry() :: #{filters := table(), tests := table(),
                      params := #{atom() => [atom()]},
                      modules := [module()]}.

-export_type([kind/0, registry/0]).

%%%===================================================================
%%% Registry
%%%===================================================================

%% @doc Build the filter and test tables for a compilation.
%%
%% A user filter that shadows a builtin is an error, not an override: a
%% template that silently gets a different `join' because a dependency
%% registered one is the kind of bug nobody finds.
-spec registry([module()]) -> {ok, registry()} | {error, ai_jinja_reason()}.
registry(Mods) ->
    Base = #{filters => builtin(filter),
             tests   => builtin(test),
             params  => ai_jinja_filters:params(),
             modules => []},
    try {ok, lists:foldl(fun add/2, Base, Mods)}
    catch throw:{ai_jinja_ext, Reason} -> {error, Reason} end.

add(Mod, Acc) ->
    _ = code:ensure_loaded(Mod),
    Fs = call_table(Mod, filters),
    Ts = call_table(Mod, tests),
    Ps = case erlang:function_exported(Mod, params, 0) of
             true  -> Mod:params();
             false -> #{}
         end,
    #{filters := F0, tests := T0, params := P0, modules := M0} = Acc,
    Acc#{filters => merge(F0, Fs, Mod),
         tests   => merge(T0, Ts, Mod),
         params  => maps:merge(P0, Ps),
         modules => lists:usort([Mod | M0] ++ [M || {M, _} <- maps:values(Fs)]
                                ++ [M || {M, _} <- maps:values(Ts)])}.

call_table(Mod, Fun) ->
    case erlang:function_exported(Mod, Fun, 0) of
        true ->
            case Mod:Fun() of
                M when is_map(M) -> M;
                _ -> throw({ai_jinja_ext, {filter_name_conflict, Fun, Mod}})
            end;
        false ->
            #{}
    end.

merge(Existing, New, Mod) ->
    maps:fold(fun(Name, MF, Acc) ->
                      case maps:is_key(Name, Acc) of
                          true  -> throw({ai_jinja_ext,
                                          {filter_name_conflict, Name, Mod}});
                          false -> Acc#{Name => MF}
                      end
              end, Existing, New).

%%%===================================================================
%%% Lookup
%%%===================================================================

%% @doc The builtin table for one kind.
-spec builtin(kind()) -> table().
builtin(filter) -> ai_jinja_filters:filters();
builtin(test)   -> ai_jinja_tests:tests().

%% @doc Resolve a name at RUN time.
%%
%% Only the builtins are reachable here. `map', `select' and friends take a
%% filter or test name as a value, and there is nowhere to keep a per-render
%% registry without the ets or process state the architecture forbids -- so a
%% custom filter cannot be reached by name from inside one of those. Using it
%% directly, `x|money("EUR")', works: the compiler resolved it.
-spec lookup(kind(), atom()) -> {ok, {module(), atom()}} | error.
lookup(Kind, Name) ->
    case builtin(Kind) of
        #{Name := MF} -> {ok, MF};
        _             -> error
    end.

%%%===================================================================
%%% Static inspection, for the parse_transform
%%%===================================================================

%% @doc What a module declares, read from its source rather than its beam.
%%
%% `-jinja_ext(m)' has to be validated while compiling the module that
%% declares it, and `m' may not be compiled yet. Loading is tried first; only
%% if that fails is the source scanned, and only literal maps can be read --
%% a table built at run time is reported as unknown rather than guessed at.
-spec spec_module(module()) ->
          {ok, #{filters := [atom()], tests := [atom()]}} | {error, ai_jinja_reason()}.
spec_module(Mod) ->
    case code:ensure_loaded(Mod) of
        {module, Mod} ->
            {ok, #{filters => maps:keys(call_table(Mod, filters)),
                   tests   => maps:keys(call_table(Mod, tests))}};
        _ ->
            case source_of(Mod) of
                {ok, File} -> scan_source(File);
                error      -> {error, {filter_name_conflict, Mod, Mod}}
            end
    end.

source_of(Mod) ->
    Name = atom_to_list(Mod) ++ ".erl",
    case [F || D <- ["src", "."], F <- [filename:join(D, Name)],
               filelib:is_regular(F)] of
        [F | _] -> {ok, F};
        []      -> error
    end.

scan_source(File) ->
    case epp_dodger:quick_parse_file(File) of
        {ok, Forms} ->
            {ok, #{filters => literal_keys(Forms, filters),
                   tests   => literal_keys(Forms, tests)}};
        {error, _} ->
            {ok, #{filters => [], tests => []}}
    end.

literal_keys(Forms, Fun) ->
    case [Body || {function, _, F, 0, [{clause, _, [], [], Body}]} <- Forms, F =:= Fun] of
        [[Expr]] ->
            try erl_parse:normalise(Expr) of
                M when is_map(M) -> maps:keys(M);
                _                -> []
            catch _:_ -> [] end;
        _ -> []
    end.

%%%===================================================================
%%% Diagnostics
%%%===================================================================

-spec format_error(ai_jinja_reason()) -> string().
format_error({filter_name_conflict, Name, Mod}) ->
    lists:flatten(io_lib:format(
                    "~p already names a builtin filter or test, and ~p tries "
                    "to register it again. Rename the one in ~p; aihtml does "
                    "not let an extension shadow a builtin, because a template "
                    "would then behave differently depending on which "
                    "extensions happen to be configured.",
                    [Name, Mod, Mod]));
format_error(Other) ->
    lists:flatten(io_lib:format("~p", [Other])).
