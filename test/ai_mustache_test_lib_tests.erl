%%%-------------------------------------------------------------------
%%% @doc Self-tests for the spec test support library.
%%%
%%% If the loader or the data conversion is wrong, a green spec run means
%%% nothing. These tests run from phase 1 onwards and never skip.
%%% See tasks/T05.md.
%%% @end
%%%-------------------------------------------------------------------
-module(ai_mustache_test_lib_tests).

-include_lib("eunit/include/eunit.hrl").

-import(ai_mustache_test_lib, [to_ctx/1, name_to_atom/1, name_to_atoms/1]).

%%%===================================================================
%%% to_ctx/1
%%%===================================================================

%% Map keys become atoms; values keep their JSON types.
to_ctx_map_keys_test() ->
    ?assertEqual(#{subject => <<"world">>},
                 to_ctx(#{<<"subject">> => <<"world">>})).

to_ctx_nested_test() ->
    ?assertEqual(#{a => #{b => #{c => true}}},
                 to_ctx(#{<<"a">> => #{<<"b">> => #{<<"c">> => true}}})).

to_ctx_list_test() ->
    ?assertEqual(#{list => [#{n => 1}, #{n => 2}]},
                 to_ctx(#{<<"list">> => [#{<<"n">> => 1}, #{<<"n">> => 2}]})).

%% interpolation.json "Implicit Iterators - Basic Integer Interpolation" has a
%% bare integer as its whole data, so the top level is not always a map.
%% See designs/02-architecture.md section 6.3.
to_ctx_bare_scalar_root_test_() ->
    [?_assertEqual(85, to_ctx(85)),
     ?_assertEqual(<<"str">>, to_ctx(<<"str">>)),
     ?_assertEqual([1, 2, 3], to_ctx([1, 2, 3]))].

%% JSON null decodes to the atom null, which is part of the falsy set
%% (designs/03-semantics.md section 2.1). It must survive conversion.
to_ctx_null_test() ->
    ?assertEqual(#{a => null}, to_ctx(#{<<"a">> => null})).

to_ctx_booleans_test() ->
    ?assertEqual(#{t => true, f => false},
                 to_ctx(#{<<"t">> => true, <<"f">> => false})).

%% Floats must survive as floats so that to_binary/1 can render them per spec
%% (1.21, not 1.21000000000000000000e+00). See designs/02-architecture.md 6.2.
to_ctx_numbers_test_() ->
    [?_assertEqual(#{i => 85}, to_ctx(#{<<"i">> => 85})),
     ?_assertEqual(#{f => 1.21}, to_ctx(#{<<"f">> => 1.21})),
     ?_assert(is_float(maps:get(f, to_ctx(#{<<"f">> => 1.1}))))].

to_ctx_empty_test_() ->
    [?_assertEqual(#{}, to_ctx(#{})),
     ?_assertEqual([], to_ctx([]))].

%% This is the exact shape OTP's json module produces for the float cases in
%% sections.json "Implicit Iterator - Decimal".
to_ctx_spec_shape_test() ->
    Decoded = json:decode(<<"{\"list\":[1.1,2.2,3.3]}">>),
    ?assertEqual(#{list => [1.1, 2.2, 3.3]}, to_ctx(Decoded)).

%%%===================================================================
%%% name_to_atom/1
%%%===================================================================

name_to_atom_basic_test_() ->
    [?_assertEqual(basic_interpolation, name_to_atom(<<"Basic Interpolation">>)),
     ?_assertEqual(no_interpolation, name_to_atom(<<"No Interpolation">>))].

%% Runs of punctuation and spaces collapse to a single underscore.
name_to_atom_squash_test() ->
    ?assertEqual(implicit_iterators_basic_integer_interpolation,
                 name_to_atom(<<"Implicit Iterators - Basic Integer Interpolation">>)).

name_to_atom_trims_test_() ->
    [?_assertEqual(padded, name_to_atom(<<"  Padded  ">>)),
     ?_assertEqual(dashed, name_to_atom(<<"- Dashed -">>))].

name_to_atom_keeps_digits_test() ->
    ?assertEqual(standalone_line_endings_2, name_to_atom(<<"Standalone Line Endings 2">>)).

%%%===================================================================
%%% name_to_atoms/1 -- collision handling
%%%===================================================================

%% Normalisation is lossy, so distinct spec names can collapse. The second and
%% later occurrences get a counter suffix.
name_to_atoms_dedups_test() ->
    ?assertEqual([a_b, a_b_2, a_b_3],
                 name_to_atoms([<<"A B">>, <<"A-B">>, <<"A, B">>])).

name_to_atoms_preserves_order_test() ->
    ?assertEqual([first, second, third],
                 name_to_atoms([<<"First">>, <<"Second">>, <<"Third">>])).

%%%===================================================================
%%% Spec loading
%%%===================================================================

load_spec_shape_test() ->
    [First | _] = ai_mustache_test_lib:load_spec(interpolation),
    ?assertEqual(<<"No Interpolation">>, maps:get(name, First)),
    ?assertMatch(#{template := _, data := _, expected := _, partials := _,
                   desc := _}, First).

%% delimiters.json and partials.json carry a partials map; the others default
%% to an empty one rather than being absent.
load_spec_partials_default_test() ->
    Cases = ai_mustache_test_lib:load_spec(comments),
    ?assert(lists:all(fun(C) -> maps:get(partials, C) =:= #{} end, Cases)).

load_spec_partials_present_test() ->
    Cases = ai_mustache_test_lib:load_spec(partials),
    ?assert(lists:any(fun(C) -> maps:get(partials, C) =/= #{} end, Cases)).

required_and_optional_are_disjoint_test() ->
    R = ai_mustache_test_lib:required_specs(),
    O = ai_mustache_test_lib:optional_specs(),
    ?assertEqual([], [X || X <- R, lists:member(X, O)]).
