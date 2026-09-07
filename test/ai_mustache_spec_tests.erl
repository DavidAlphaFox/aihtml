%%%-------------------------------------------------------------------
%%% @doc Mustache spec conformance tests.
%%%
%%% Every case from the vendored spec becomes its own named EUnit test so a
%%% single failure does not mask the rest and the pass rate stays visible as
%%% the refactor progresses. See tasks/T04.md.
%%%
%%% Until ai_mustache is implemented (phases 2 and 3) the conformance tests
%%% are not generated; the inventory tests below still run and genuinely
%%% verify the vendored data and the loader.
%%% @end
%%%-------------------------------------------------------------------
-module(ai_mustache_spec_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Inventory -- always runs
%%%===================================================================

%% Case counts of the six mandatory spec modules, 136 in total. These are
%% asserted so that a botched vendoring or a silently truncated file is caught
%% immediately rather than showing up as a suspiciously high pass rate.
counts() ->
    [{comments, 12}, {delimiters, 14}, {interpolation, 42},
     {inverted, 22}, {partials, 12}, {sections, 34}].

inventory_test_() ->
    [{atom_to_list(Mod) ++ ": " ++ integer_to_list(N) ++ " cases",
      ?_assertEqual(N, length(ai_mustache_test_lib:load_spec(Mod)))}
     || {Mod, N} <- counts()].

total_case_count_test() ->
    Total = lists:sum([length(ai_mustache_test_lib:load_spec(M))
                       || M <- ai_mustache_test_lib:required_specs()]),
    ?assertEqual(136, Total).

optional_specs_present_test_() ->
    [{atom_to_list(Mod),
      ?_assert(length(ai_mustache_test_lib:load_spec(Mod)) > 0)}
     || Mod <- ai_mustache_test_lib:optional_specs()].

%% Case names must stay distinct after normalisation, otherwise two spec cases
%% would report under one title.
no_name_collisions_test_() ->
    [{atom_to_list(Mod),
      fun() ->
              Names = case_names(Mod),
              ?assertEqual([], Names -- lists:usort(Names))
      end}
     || Mod <- ai_mustache_test_lib:required_specs()].

%%%===================================================================
%%% Conformance -- generated once ai_mustache exists
%%%===================================================================

conformance_test_() ->
    case ai_mustache_test_lib:implemented() of
        false ->
            {"mustache spec conformance: PENDING -- ai_mustache:render_string/3 "
             "not implemented yet (phase 1 baseline, see tasks/T04.md)",
             fun() -> ok end};
        true ->
            [{atom_to_list(Mod), module_tests(Mod)}
             || Mod <- ai_mustache_test_lib:required_specs()]
    end.

module_tests(Mod) ->
    Cases = ai_mustache_test_lib:load_spec(Mod),
    Names = ai_mustache_test_lib:name_to_atoms([maps:get(name, C) || C <- Cases]),
    [{atom_to_list(Name), fun() -> exec(Case) end}
     || {Name, Case} <- lists:zip(Names, Cases)].

exec(#{template := T, data := Data, partials := P, expected := Expected}) ->
    Ctx = ai_mustache_test_lib:to_ctx(Data),
    Got = ai_mustache:render_string(T, Ctx, #{partials => P}),
    ?assertEqual({Expected, T, Ctx, P}, {Got, T, Ctx, P}).

%%%===================================================================
%%% Internal
%%%===================================================================

case_names(Mod) ->
    ai_mustache_test_lib:name_to_atoms(
      [maps:get(name, C) || C <- ai_mustache_test_lib:load_spec(Mod)]).
