%%%-------------------------------------------------------------------
%%% @doc Test support for the vendored mustache spec suite.
%%%
%%% Zero dependencies: the spec files are JSON and OTP 27+ ships a `json'
%%% module in stdlib, so nothing outside OTP is needed to read them.
%%%
%%% See designs/02-architecture.md section 6.1 and tasks/T03.md.
%%% @end
%%%-------------------------------------------------------------------
-module(ai_mustache_test_lib).

-export([required_specs/0, optional_specs/0]).
-export([load_spec/1, load_spec/2, spec_dir/0]).
-export([to_ctx/1, name_to_atom/1, name_to_atoms/1]).
-export([skip_unless_implemented/1, implemented/0]).

-type spec_case() :: #{name     := binary(),
                       desc     := binary(),
                       data     := term(),
                       template := binary(),
                       partials := #{binary() => binary()},
                       expected := binary()}.
-export_type([spec_case/0]).

%%%===================================================================
%%% Spec modules
%%%===================================================================

%% @doc Mandatory spec modules. All of these must pass (136 cases total).
-spec required_specs() -> [atom()].
required_specs() ->
    [comments, delimiters, interpolation, inverted, partials, sections].

%% @doc Optional spec modules, kept for reference only. These correspond to
%% the known deviations listed in designs/03-semantics.md section 8 and are
%% not part of any acceptance criterion.
-spec optional_specs() -> [atom()].
optional_specs() ->
    ['optional-dynamic-names', 'optional-inheritance', 'optional-lambdas'].

%% @doc Directory holding the vendored spec files.
-spec spec_dir() -> file:filename_all().
spec_dir() ->
    filename:join(code:lib_dir(aihtml), "test/spec").

%%%===================================================================
%%% Loading
%%%===================================================================

-spec load_spec(atom()) -> [spec_case()].
load_spec(Mod) -> load_spec(Mod, spec_dir()).

-spec load_spec(atom(), file:filename_all()) -> [spec_case()].
load_spec(Mod, Dir) ->
    File = filename:join(Dir, atom_to_list(Mod) ++ ".json"),
    {ok, Bin} = file:read_file(File),
    #{<<"tests">> := Tests} = json:decode(Bin),
    [to_case(T) || T <- Tests].

-spec to_case(map()) -> spec_case().
to_case(T) ->
    #{name     => maps:get(<<"name">>, T),
      desc     => maps:get(<<"desc">>, T, <<>>),
      data     => maps:get(<<"data">>, T),
      template => maps:get(<<"template">>, T),
      partials => maps:get(<<"partials">>, T, #{}),
      expected => maps:get(<<"expected">>, T)}.

%%%===================================================================
%%% Data conversion
%%%===================================================================

%% @doc Convert decoded spec data into an aihtml context.
%%
%% The spec uses string keys; aihtml uses atom keys (a deliberate deviation,
%% see designs/03-semantics.md section 8). Only map *keys* are converted --
%% values keep their JSON types:
%%
%% <ul>
%%   <li>binaries stay binaries</li>
%%   <li>integers and floats stay numbers (1.21 must render as "1.21",
%%       see designs/02-architecture.md section 6.2)</li>
%%   <li>`true' / `false' / `null' stay atoms (`null' is falsy)</li>
%% </ul>
%%
%% The top level need not be a map: `interpolation.json' has a case whose
%% data is the bare integer 85 (designs/02-architecture.md section 6.3).
-spec to_ctx(term()) -> term().
to_ctx(M) when is_map(M) ->
    maps:fold(fun(K, V, Acc) -> Acc#{key_to_atom(K) => to_ctx(V)} end, #{}, M);
to_ctx(L) when is_list(L) ->
    [to_ctx(E) || E <- L];
to_ctx(V) ->
    V.

-spec key_to_atom(binary() | atom()) -> atom().
key_to_atom(K) when is_atom(K)   -> K;
key_to_atom(K) when is_binary(K) -> binary_to_atom(K, utf8).

%%%===================================================================
%%% Test case naming
%%%===================================================================

%% @doc Normalise a spec case name into a legal CT testcase atom.
%%
%% "Implicit Iterators - Basic Integer Interpolation"
%%   -> implicit_iterators_basic_integer_interpolation
-spec name_to_atom(binary() | string()) -> atom().
name_to_atom(Name) when is_binary(Name) ->
    name_to_atom(unicode:characters_to_list(Name));
name_to_atom(Name) when is_list(Name) ->
    Squashed = squash(lists:map(fun normalise_char/1, string:lowercase(Name))),
    binary_to_atom(unicode:characters_to_binary(trim_us(Squashed)), utf8).

-spec normalise_char(char()) -> char().
normalise_char(C) when C >= $a, C =< $z -> C;
normalise_char(C) when C >= $0, C =< $9 -> C;
normalise_char(_)                       -> $_.

%% Collapse runs of underscores.
-spec squash(string()) -> string().
squash([$_, $_ | T]) -> squash([$_ | T]);
squash([H | T])      -> [H | squash(T)];
squash([])           -> [].

-spec trim_us(string()) -> string().
trim_us(S) -> string:trim(S, both, "_").

%% @doc Normalise a list of names, disambiguating collisions by appending a
%% counter. Collisions are possible because normalisation is lossy.
-spec name_to_atoms([binary() | string()]) -> [atom()].
name_to_atoms(Names) ->
    {Atoms, _} = lists:mapfoldl(fun dedup/2, #{}, [name_to_atom(N) || N <- Names]),
    Atoms.

-spec dedup(atom(), #{atom() => pos_integer()}) -> {atom(), #{atom() => pos_integer()}}.
dedup(A, Seen) ->
    case maps:get(A, Seen, 0) of
        0 -> {A, Seen#{A => 1}};
        N -> {binary_to_atom(<<(atom_to_binary(A, utf8))/binary, "_",
                               (integer_to_binary(N + 1))/binary>>, utf8),
              Seen#{A => N + 1}}
    end.

%%%===================================================================
%%% Phase gating
%%%===================================================================

%% @doc Whether the rendering entry point exists yet.
%%
%% During phase 1 there is no implementation at all, so the spec-driven
%% suites enumerate every case but skip them. As phases 2 and 3 land the
%% suites light up on their own with no edits. See tasks/T04.md.
-spec implemented() -> boolean().
implemented() ->
    _ = code:ensure_loaded(ai_mustache),
    erlang:function_exported(ai_mustache, render_string, 3).

-spec skip_unless_implemented(Config) -> Config | {skip, string()} when Config :: term().
skip_unless_implemented(Config) ->
    case implemented() of
        true  -> Config;
        false -> {skip, "ai_mustache:render_string/3 not implemented yet "
                        "(phase 1 baseline, see tasks/T04.md)"}
    end.
