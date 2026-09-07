%%%-------------------------------------------------------------------
%%% @doc The jinja conformance suite.
%%%
%%% Every case in test/jinja_spec/*.json becomes one named EUnit test. Cases
%%% belonging to a stage the implementation has not reached yet are reported
%%% as skipped, so the suite is green throughout the build-up and turns red
%%% the moment a stage that IS claimed regresses.
%%%
%%% The inventory and meta assertions below always run for real: they are what
%%% stops a fixture file from quietly emptying itself.
%%% @end
%%%-------------------------------------------------------------------
-module(ai_jinja_spec_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Inventory -- always real
%%%===================================================================

inventory_test_() ->
    [{"group " ++ binary_to_list(G) ++ " has at least " ++ integer_to_list(Min)
      ++ " cases",
      fun() ->
              N = length(ai_jinja_test_lib:cases(G)),
              ?assert(N >= Min)
      end}
     || {G, Min} <- ai_jinja_test_lib:inventory()].

total_inventory_test() ->
    ?assert(length(ai_jinja_test_lib:all_cases()) >= 500).

case_names_are_unique_test() ->
    Names = [{maps:get(group, C), maps:get(name, C)}
             || C <- ai_jinja_test_lib:all_cases()],
    ?assertEqual(lists:usort(Names), lists:sort(Names)).

every_case_has_an_expectation_test() ->
    Bad = [ai_jinja_test_lib:title(C)
           || C <- ai_jinja_test_lib:all_cases(),
              not (maps:is_key(expected, C) orelse maps:is_key(error, C))],
    ?assertEqual([], Bad).

%%%===================================================================
%%% Meta: the declared deviations and reasons are all exercised
%%%===================================================================

%% designs/10-jinja-semantics.md section 9. A deviation with no fixture is a
%% claim nobody checks.
-define(DEVIATIONS,
        [<<"J1">>, <<"J2">>, <<"J3">>, <<"J4">>, <<"J5">>, <<"J6">>, <<"J7">>,
         <<"J8">>, <<"J9">>, <<"J10">>, <<"J11">>, <<"J12">>, <<"J13">>,
         <<"J14">>, <<"J15">>, <<"J16">>]).

%% Deviations that cannot be expressed as a rendering fixture, with the reason.
-define(STRUCTURAL_DEVIATIONS,
        #{<<"J1">>  => "map keys are atoms; every fixture context relies on it",
          <<"J5">>  => "delimiters are not configurable, so there is nothing to render",
          <<"J8">>  => "the trim defaults are the environment every fixture runs in",
          <<"J12">> => "no sandbox; absence of a feature has no output",
          <<"J13">> => "asserted by the .items()/.keys()/.values() cases in for"}).

every_deviation_has_a_fixture_test() ->
    Seen = lists:usort([D || C <- ai_jinja_test_lib:all_cases(),
                             D <- [maps:get(deviation, C, undefined)],
                             D =/= undefined]),
    Missing = [D || D <- ?DEVIATIONS,
                    not lists:member(D, Seen),
                    not maps:is_key(D, ?STRUCTURAL_DEVIATIONS)],
    ?assertEqual([], Missing).

deviation_numbers_are_declared_test() ->
    Seen = [D || C <- ai_jinja_test_lib:all_cases(),
                 D <- [maps:get(deviation, C, undefined)], D =/= undefined],
    ?assertEqual([], [D || D <- lists:usort(Seen),
                           not lists:member(D, ?DEVIATIONS)]).

%% designs/11-jinja-codegen.md section 9 plus the runtime reasons. Each must be
%% reachable from at least one fixture, or the diagnostic is untested.
-define(REASONS,
        [unclosed_block, mismatched_end, block_name_mismatch, duplicate_block,
         orphan_clause, unexpected_token, chained_comparison, unknown_filter,
         unknown_test, filter_name_conflict, dynamic_target_unsupported,
         template_not_found, namespace_assignment_unsupported,
         target_in_inline_template, invalid_utf8, unexpected_remote_calls,
         unknown_statement, not_iterable, super_outside_block, macro_not_found,
         extends_cycle, mutual_macro_in_inline, required_block_not_provided,
         undefined_operation, division_by_zero, unsupported_operands,
         not_callable, not_renderable]).

every_reason_has_a_fixture_test() ->
    Seen = lists:usort([R || C <- ai_jinja_test_lib:all_cases(),
                             R <- [maps:get(error, C, undefined)],
                             R =/= undefined]),
    ?assertEqual([], [R || R <- ?REASONS, not lists:member(R, Seen)]).

reasons_are_declared_test() ->
    Seen = lists:usort([R || C <- ai_jinja_test_lib:all_cases(),
                             R <- [maps:get(error, C, undefined)],
                             R =/= undefined]),
    ?assertEqual([], [R || R <- Seen, not lists:member(R, ?REASONS)]).

%%%===================================================================
%%% The suite itself
%%%===================================================================

conformance_test_() ->
    [{ai_jinja_test_lib:title(C), fun() -> assert_case(C) end}
     || C <- ai_jinja_test_lib:all_cases()].

%% EUnit has no notion of a pending test, so a case belonging to a stage that
%% has not landed yet simply does nothing. pending_count_test/0 below keeps
%% that honest by printing how many are in that state -- otherwise a green
%% suite would say nothing about how much of it is actually running.
assert_case(C) ->
    case ai_jinja_test_lib:due(C) of
        false -> ok;
        true  -> assert_due(C)
    end.

pending_count_test() ->
    All = ai_jinja_test_lib:all_cases(),
    Pending = [C || C <- All, not ai_jinja_test_lib:due(C)],
    case Pending of
        [] -> ok;
        _  -> ?debugFmt("jinja spec: ~p of ~p cases pending at stage ~p",
                        [length(Pending), length(All),
                         ai_jinja_test_lib:current_stage()])
    end.

assert_due(#{error := Reason} = C) ->
    ?assertEqual({error, Reason}, ai_jinja_test_lib:run(C));
assert_due(#{expected := Expected} = C) ->
    ?assertEqual({ok, Expected}, ai_jinja_test_lib:run(C)).
