%%%-------------------------------------------------------------------
%%% Tests for the runtime.
%%%
%%% The happy paths are covered by the fixtures; what is asserted here is the
%%% shape of the scope stack, the failure modes of every operator, and the two
%%% places where a careless implementation would open an attack surface.
%%%-------------------------------------------------------------------
-module(ai_jinja_rt_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Scope
%%%===================================================================

resolve_walks_outward_test() ->
    S = [#{a => 1}, #{a => 2, b => 3}, #{c => 4}],
    ?assertEqual(1, ai_jinja_rt:resolve(a, S)),
    ?assertEqual(3, ai_jinja_rt:resolve(b, S)),
    ?assertEqual(4, ai_jinja_rt:resolve(c, S)),
    ?assertEqual(undefined, ai_jinja_rt:resolve(z, S)),
    ?assertEqual(undefined, ai_jinja_rt:resolve(a, [])).

%% The bottom frame is whatever the caller passed and need not be a map.
non_map_frames_are_skipped_test() ->
    ?assertEqual(1, ai_jinja_rt:resolve(a, [#{}, 85, #{a => 1}])),
    ?assertEqual(undefined, ai_jinja_rt:resolve(a, ai_jinja_rt:new_scope(85))).

%% {% set %} rebinds the top frame. Pushing instead would make a loop that
%% sets on every iteration grow the scope without bound.
bind_replaces_the_top_frame_test() ->
    S0 = ai_jinja_rt:new_scope(#{}),
    S1 = lists:foldl(fun(N, S) -> ai_jinja_rt:bind(S, a, N) end, S0,
                     lists:seq(1, 100)),
    ?assertEqual(length(S0), length(S1)),
    ?assertEqual(100, ai_jinja_rt:resolve(a, S1)).

push_adds_a_frame_test() ->
    S = ai_jinja_rt:push(ai_jinja_rt:new_scope(#{a => 1}), #{a => 2}),
    ?assertEqual(2, ai_jinja_rt:resolve(a, S)),
    ?assertEqual(3, length(S)).

globals_drops_local_frames_test() ->
    S = ai_jinja_rt:push(
          ai_jinja_rt:push(ai_jinja_rt:new_scope(#{ctx => 1}), #{local => 2}),
          #{deeper => 3}),
    G = ai_jinja_rt:globals(S),
    ?assertEqual(1, ai_jinja_rt:resolve(ctx, G)),
    ?assertEqual(undefined, ai_jinja_rt:resolve(local, G)),
    ?assertEqual(undefined, ai_jinja_rt:resolve(deeper, G)).

root_is_the_render_context_test() ->
    ?assertEqual(#{a => 1},
                 ai_jinja_rt:root(ai_jinja_rt:push(
                                    ai_jinja_rt:new_scope(#{a => 1}), #{b => 2}))).

%%%===================================================================
%%% Truth
%%%===================================================================

%% Python's rule, not mustache's. The two engines disagree here on purpose and
%% must not share an implementation.
truthy_test() ->
    [?assertNot(ai_jinja_rt:truthy(V))
     || V <- [undefined, false, null, 0, 0.0, [], <<>>, #{}, {}]],
    [?assert(ai_jinja_rt:truthy(V))
     || V <- [true, 1, -1, 0.1, [0], <<"a">>, #{a => 1}, {1}]].

truthy_differs_from_mustache_test() ->
    [?assertNotEqual(ai_mustache_rt:truthy(V), ai_jinja_rt:truthy(V))
     || V <- [0, 0.0, #{}]].

%%%===================================================================
%%% Access
%%%===================================================================

%% Deviation J14: undefined is chainable, so a typo deep in a template renders
%% as nothing rather than raising.
undefined_is_chainable_test() ->
    ?assertEqual(undefined,
                 ai_jinja_rt:attr(ai_jinja_rt:attr(
                                    ai_jinja_rt:attr(#{}, a), b), c)),
    ?assertEqual(undefined, ai_jinja_rt:subscript(undefined, 0)).

subscript_test() ->
    ?assertEqual(1, ai_jinja_rt:subscript([1, 2, 3], 0)),
    ?assertEqual(3, ai_jinja_rt:subscript([1, 2, 3], -1)),
    ?assertEqual(undefined, ai_jinja_rt:subscript([1], 5)),
    ?assertEqual(<<"b">>, ai_jinja_rt:subscript(<<"abc">>, 1)),
    ?assertEqual(1, ai_jinja_rt:subscript(#{a => 1}, a)),
    ?assertEqual(1, ai_jinja_rt:subscript(#{a => 1}, <<"a">>)).

%% A subscript key can come straight from template data. Creating atoms from
%% it would be an unbounded atom table.
string_keys_never_create_atoms_test() ->
    Fresh = <<"an_atom_that_does_not_exist_yet_in_this_vm">>,
    ?assertEqual(undefined, ai_jinja_rt:subscript(#{a => 1}, Fresh)),
    ?assertError(badarg, binary_to_existing_atom(Fresh, utf8)).

slice_test() ->
    ?assertEqual([2, 3], ai_jinja_rt:slice([1, 2, 3], 1, undefined, undefined)),
    ?assertEqual([1, 2], ai_jinja_rt:slice([1, 2, 3], undefined, 2, undefined)),
    ?assertEqual([1, 3, 5], ai_jinja_rt:slice([1, 2, 3, 4, 5], 0, 5, 2)),
    ?assertEqual([3, 2, 1], ai_jinja_rt:slice([1, 2, 3], undefined, undefined, -1)),
    ?assertEqual(<<"bc">>, ai_jinja_rt:slice(<<"abc">>, 1, undefined, undefined)).

mapping_methods_test() ->
    M = #{b => 2, a => 1},
    ?assertEqual([{a, 1}, {b, 2}], ai_jinja_rt:items(M)),
    ?assertEqual([a, b], ai_jinja_rt:keys(M)),
    ?assertEqual([1, 2], ai_jinja_rt:values(M)),
    ?assertEqual([], ai_jinja_rt:items(undefined)).

%%%===================================================================
%%% Operators
%%%===================================================================

arithmetic_test() ->
    ?assertEqual(3, ai_jinja_rt:add(1, 2)),
    ?assertEqual(<<"ab">>, ai_jinja_rt:add(<<"a">>, <<"b">>)),
    ?assertEqual([1, 2], ai_jinja_rt:add([1], [2])),
    ?assertEqual(0.5, ai_jinja_rt:divide(1, 2)),
    ?assertEqual(8, ai_jinja_rt:pow(2, 3)),
    ?assertEqual(<<"a1">>, ai_jinja_rt:concat(<<"a">>, 1)).

%% Python's modulo and floor division take the sign of the divisor; Erlang's
%% rem and div do not.
python_modulo_test() ->
    ?assertEqual(2, ai_jinja_rt:mod(-7, 3)),
    ?assertEqual(-2, ai_jinja_rt:mod(7, -3)),
    ?assertEqual(-4, ai_jinja_rt:floordiv(-7, 2)),
    ?assertEqual(3, ai_jinja_rt:floordiv(7, 2)).

comparison_test() ->
    ?assert(ai_jinja_rt:eq(1, 1.0)),
    ?assert(ai_jinja_rt:lt(<<"a">>, <<"b">>)),
    ?assert(ai_jinja_rt:contains([1, 2], 1)),
    ?assert(ai_jinja_rt:contains(<<"abc">>, <<"b">>)),
    ?assert(ai_jinja_rt:contains(#{a => 1}, a)),
    ?assertNot(ai_jinja_rt:contains(undefined, 1)).

%% Erlang would happily answer `1 < <<"a">>' from its own term order. That is
%% not a useful answer to give a template author.
mixed_type_ordering_is_an_error_test() ->
    ?assertError({ai_jinja, {unsupported_operands, '<', _, _}},
                 ai_jinja_rt:lt(1, <<"a">>)).

division_by_zero_test() ->
    [?assertError({ai_jinja, division_by_zero}, F())
     || F <- [fun() -> ai_jinja_rt:divide(1, 0) end,
              fun() -> ai_jinja_rt:floordiv(1, 0) end,
              fun() -> ai_jinja_rt:mod(1, 0) end,
              fun() -> ai_jinja_rt:divide(1, 0.0) end]].

%% `{{ total + 1 }}' with a missing total should say so, not "unsupported
%% operands".
undefined_operands_are_named_test() ->
    ?assertError({ai_jinja, {undefined_operation, '+'}},
                 ai_jinja_rt:add(undefined, 1)),
    ?assertError({ai_jinja, {undefined_operation, '-'}},
                 ai_jinja_rt:sub(1, undefined)).

every_operator_has_a_failure_path_test() ->
    Ops = ['+', '-', '*', '/', '//', '%', '**', '<', '<=', '>', '>='],
    [?assertError({ai_jinja, _}, ai_jinja_rt:binop(Op, self(), self()))
     || Op <- Ops].

%%%===================================================================
%%% Iteration
%%%===================================================================

to_list_test() ->
    ?assertEqual([1, 2], ai_jinja_rt:to_list([1, 2])),
    ?assertEqual([1, 2], ai_jinja_rt:to_list({1, 2})),
    ?assertEqual([a, b], ai_jinja_rt:to_list(#{b => 2, a => 1})),
    ?assertEqual([], ai_jinja_rt:to_list(undefined)).

%% Deviation J9: in a template, iterating a string is almost always a typo.
iterating_a_string_is_refused_test() ->
    ?assertError({ai_jinja, {not_iterable, <<"ab">>}}, ai_jinja_rt:to_list(<<"ab">>)),
    ?assertError({ai_jinja, {not_iterable, 1}}, ai_jinja_rt:to_list(1)).

loop_fields_test() ->
    All = [10, 20, 30],
    L1 = ai_jinja_rt:loop(1, 3, All, 0, 10),
    L3 = ai_jinja_rt:loop(3, 3, All, 0, 30),
    ?assertMatch(#{index := 1, index0 := 0, revindex := 3, revindex0 := 2,
                   first := true, last := false, length := 3,
                   depth := 1, depth0 := 0, previtem := undefined,
                   nextitem := 20}, L1),
    ?assertMatch(#{index := 3, first := false, last := true,
                   previtem := 20, nextitem := undefined}, L3),
    Cycle = maps:get(cycle, L1),
    ?assertEqual(<<"a">>, Cycle([<<"a">>, <<"b">>])),
    ?assertEqual(<<"a">>, (maps:get(cycle, L3))([<<"a">>, <<"b">>])).

changed_test() ->
    ?assert(ai_jinja_rt:changed(1, undefined, true)),
    ?assertNot(ai_jinja_rt:changed(1, 1, false)),
    ?assert(ai_jinja_rt:changed(2, 1, false)).

%%%===================================================================
%%% Rendering
%%%===================================================================

%% Output form follows the reference implementation, not Erlang's own.
rendering_follows_python_test() ->
    ?assertEqual(<<"True">>,  ai_jinja_rt:to_binary(true)),
    ?assertEqual(<<"False">>, ai_jinja_rt:to_binary(false)),
    ?assertEqual(<<"[1, 2]">>, ai_jinja_rt:to_binary([1, 2])),
    ?assertEqual(<<"(1,)">>,   ai_jinja_rt:to_binary({1})),
    ?assertEqual(<<"(1, 2)">>, ai_jinja_rt:to_binary({1, 2})),
    ?assertEqual(<<"{'a': 1}">>, ai_jinja_rt:to_binary(#{a => 1})),
    ?assertEqual(<<"['a', 'b']">>, ai_jinja_rt:to_binary([<<"a">>, <<"b">>])),
    ?assertEqual(<<>>, ai_jinja_rt:to_binary(undefined)).

%% A bare string prints itself; the same string inside a container is quoted,
%% exactly as Python's str and repr differ.
top_level_strings_are_unquoted_test() ->
    ?assertEqual(<<"ab">>, ai_jinja_rt:to_binary(<<"ab">>)),
    ?assertEqual(<<"'ab'">>, ai_jinja_rt:repr(<<"ab">>)),
    %% A map key rendered by {% for k in m %} is an atom, and must not gain
    %% quotes on the way out.
    ?assertEqual(<<"a">>, ai_jinja_rt:to_binary(a)).

float_form_follows_python_test() ->
    ?assertEqual(<<"1000.0">>, ai_jinja_rt:to_binary(1000.0)),
    ?assertEqual(<<"1.5">>,    ai_jinja_rt:to_binary(1.5)),
    ?assertEqual(<<"1e+16">>,  ai_jinja_rt:to_binary(1.0e16)),
    ?assertEqual(<<"1e-05">>,  ai_jinja_rt:to_binary(1.0e-5)),
    ?assertEqual(<<"1000000000000000.0">>, ai_jinja_rt:to_binary(1.0e15)).

quotes_pick_the_cheaper_delimiter_test() ->
    ?assertEqual(<<"'a'">>,     ai_jinja_rt:repr(<<"a">>)),
    ?assertEqual(<<"\"it's\"">>, ai_jinja_rt:repr(<<"it's">>)),
    ?assertEqual(<<"'a\\nb'">>, ai_jinja_rt:repr(<<"a\nb">>)).

not_renderable_test() ->
    ?assertError({ai_jinja, {not_renderable, _}}, ai_jinja_rt:to_binary(self())),
    ?assertError({ai_jinja, {not_renderable, _}},
                 ai_jinja_rt:to_binary(fun() -> ok end)).

%% Jinja's Markup.escape spells the double quote numerically; mustache's spec
%% spells it by name. Same table otherwise.
escape_entity_differs_from_mustache_test() ->
    ?assertEqual(<<"&#34;">>, iolist_to_binary(ai_jinja_rt:escape(<<"\"">>))),
    ?assertEqual(<<"&quot;">>, iolist_to_binary(ai_mustache_rt:escape(<<"\"">>))),
    ?assertEqual(<<"&amp;&lt;&gt;&#39;">>,
                 iolist_to_binary(ai_jinja_rt:escape(<<"&<>'">>))).

safe_round_trips_test() ->
    ?assertEqual(<<"<b>">>, iolist_to_binary(ai_jinja_rt:escape(
                                               ai_jinja_rt:safe(<<"<b>">>)))),
    ?assertEqual(<<"<b>">>, ai_jinja_rt:unsafe(ai_jinja_rt:safe(<<"<b>">>))).

%%%===================================================================
%%% Calls and builtins
%%%===================================================================

call_test() ->
    ?assertEqual(3, ai_jinja_rt:call(fun(A, B) -> A + B end, [1, 2], [])),
    ?assertError({ai_jinja, {not_callable, 1}}, ai_jinja_rt:call(1, [], [])).

builtins_test() ->
    ?assertEqual([0, 1, 2], ai_jinja_rt:range(3)),
    ?assertEqual([1, 2], ai_jinja_rt:range(1, 3)),
    ?assertEqual([0, 2, 4], ai_jinja_rt:range(0, 5, 2)),
    ?assertEqual([], ai_jinja_rt:range(3, 1)),
    ?assertEqual(#{a => 1}, ai_jinja_rt:dict([{a, 1}])),
    ?assertEqual({ns, #{a => 1}}, ai_jinja_rt:namespace([{a, 1}])).

%% `|map("upper")' takes a name as a value; the name is checked against the
%% registry rather than turned into an apply/3.
apply_named_is_a_whitelist_test() ->
    ?assertEqual(<<"AB">>, ai_jinja_rt:apply_named(filter, upper, <<"ab">>, [])),
    ?assertError({ai_jinja, {unknown_filter, no_such_filter}},
                 ai_jinja_rt:apply_named(filter, no_such_filter, <<"a">>, [])),
    ?assertError({ai_jinja, {unknown_test, no_such_test}},
                 ai_jinja_rt:apply_named(test, no_such_test, 1, [])).

%%%===================================================================
%%% Architecture invariants
%%%===================================================================

%% No processes, no ets, no persistent_term, no process dictionary, and only
%% OTP plus the shared escape module underneath.
runtime_is_pure_test() ->
    {ok, {_, [{imports, Imports}]}} =
        beam_lib:chunks(code:which(ai_jinja_rt), [imports]),
    Mods = lists:usort([M || {M, _, _} <- Imports]),
    Forbidden = [ets, persistent_term, gen_server, proc_lib],
    ?assertEqual([], [M || M <- Mods, lists:member(M, Forbidden)]),
    ?assertEqual([], [M || {M, F, _} <- Imports, M =:= erlang,
                           lists:member(F, [put, get, erase, spawn, spawn_link])]),
    ?assertEqual([], [M || M <- Mods,
                           lists:prefix("ai_mustache", atom_to_list(M))]).
