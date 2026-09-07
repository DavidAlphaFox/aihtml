%%%-------------------------------------------------------------------
%%% Tests for the AST passes.
%%%
%%% The pass that matters most here is constant folding, and the property that
%%% matters most about it is that it must never turn a run-time error into a
%%% build failure.
%%%-------------------------------------------------------------------
-module(ai_jinja_ast_tests).

-include_lib("eunit/include/eunit.hrl").

parse(Bin) -> {ok, N} = ai_jinja_parser:parse(Bin, #{source => <<"t">>}), N.
fold(Bin)  -> ai_jinja_ast:fold_constants(parse(Bin)).
merge(Bin) -> ai_jinja_ast:merge_text(parse(Bin)).

%%%===================================================================
%%% merge_text
%%%===================================================================

merge_text_test() ->
    ?assertMatch([{text, _, <<"ab">>}], merge(<<"a{# c #}b">>)).

%% Every body-bearing node has to be descended into, and there are eight of
%% them. Missing one is how the mustache engine ended up with unmerged text
%% inside {{+x}} blocks (bug P2).
merge_descends_into_every_body_test() ->
    Cases = [<<"{% if c %}a{# x #}b{% endif %}">>,
             <<"{% if c %}z{% else %}a{# x #}b{% endif %}">>,
             <<"{% for i in l %}a{# x #}b{% endfor %}">>,
             <<"{% for i in l %}z{% else %}a{# x #}b{% endfor %}">>,
             <<"{% with v = 1 %}a{# x #}b{% endwith %}">>,
             <<"{% filter upper %}a{# x #}b{% endfilter %}">>,
             <<"{% block n %}a{# x #}b{% endblock %}">>,
             <<"{% macro m() %}a{# x #}b{% endmacro %}">>,
             <<"{% set s %}a{# x #}b{% endset %}">>,
             <<"{% macro m() %}x{% endmacro %}"
               "{% call m() %}a{# x #}b{% endcall %}">>],
    [?assert(has_merged_text(ai_jinja_ast:merge_text(parse(C)))) || C <- Cases].

has_merged_text(Nodes) ->
    lists:any(fun({text, _, <<"ab">>}) -> true;
                 (N) -> lists:any(fun has_merged_text/1, ai_jinja_ast:bodies(N))
              end, Nodes).

%%%===================================================================
%%% Constant folding
%%%===================================================================

arithmetic_is_folded_test() ->
    ?assertMatch([{output, _, {lit, _, 3}}], fold(<<"{{ 1 + 2 }}">>)),
    ?assertMatch([{output, _, {lit, _, <<"ab">>}}], fold(<<"{{ \"a\" ~ \"b\" }}">>)),
    ?assertMatch([{output, _, {lit, _, true}}], fold(<<"{{ 1 < 2 }}">>)),
    ?assertMatch([{output, _, {lit, _, false}}], fold(<<"{{ not 1 }}">>)).

%% Folding evaluates through ai_jinja_rt, so `{{ 1 + 2 }}' and `{{ a + b }}'
%% cannot answer differently for the same values.
folding_uses_the_runtime_test() ->
    ?assertMatch([{output, _, {lit, _, 2}}], fold(<<"{{ -7 % 3 }}">>)),
    ?assertEqual(2, ai_jinja_rt:mod(-7, 3)).

%% A template may legitimately contain `{{ 1 / 0 }}' inside a branch that
%% never runs; folding must leave it alone rather than fail the build.
failing_expressions_are_not_folded_test() ->
    ?assertMatch([{output, _, {binop, _, '/', _, _}}], fold(<<"{{ 1 / 0 }}">>)),
    ?assertMatch([{output, _, {binop, _, '+', _, _}}], fold(<<"{{ \"a\" + 1 }}">>)).

pure_filters_are_folded_test() ->
    ?assertMatch([{output, _, {lit, _, <<"ABC">>}}], fold(<<"{{ \"abc\"|upper }}">>)),
    ?assertMatch([{output, _, {lit, _, 3}}], fold(<<"{{ -3|abs }}">>)).

%% `random' is the one builtin that is not a function of its arguments, so
%% folding it would freeze one draw into the module.
impure_filters_are_not_folded_test() ->
    ?assertMatch([{output, _, {filter, _, random, _, _}}],
                 fold(<<"{{ [1,2]|random }}">>)).

containers_of_literals_are_folded_test() ->
    ?assertMatch([{output, _, {lit, _, [1, 2]}}], fold(<<"{{ [1, 2] }}">>)),
    ?assertMatch([{output, _, {lit, _, #{a := 1}}}], fold(<<"{{ {\"a\": 1} }}">>)),
    ?assertMatch([{output, _, {list, _, _}}], fold(<<"{{ [1, x] }}">>)).

short_circuit_is_folded_test() ->
    ?assertMatch([{output, _, {name, _, b}}], fold(<<"{{ 1 and b }}">>)),
    ?assertMatch([{output, _, {lit, _, 0}}],  fold(<<"{{ 0 and b }}">>)),
    ?assertMatch([{output, _, {lit, _, 1}}],  fold(<<"{{ 1 or b }}">>)),
    ?assertMatch([{output, _, {name, _, b}}], fold(<<"{{ 0 or b }}">>)).

%%%===================================================================
%%% Dead branches
%%%===================================================================

dead_branches_disappear_test() ->
    ?assertEqual([], fold(<<"{% if false %}x{% endif %}">>)),
    ?assertMatch([{text, _, <<"x">>}], fold(<<"{% if true %}x{% endif %}">>)),
    ?assertMatch([{text, _, <<"e">>}], fold(<<"{% if false %}x{% else %}e{% endif %}">>)).

%% Eliminating a branch can leave two literals adjacent that were not before,
%% which is why merge_text runs again after folding.
postprocess_merges_after_folding_test() ->
    {ok, Nodes, []} = ai_jinja_ast:postprocess(parse(<<"a{% if false %}x{% endif %}b">>),
                                               #{source => <<"t">>}),
    ?assertMatch([{text, _, <<"ab">>}], Nodes).

%%%===================================================================
%%% Targets
%%%===================================================================

module_names_test() ->
    ?assertEqual(j2_index, ai_jinja_ast:module_name(<<"index.j2">>, #{})),
    ?assertEqual(j2_layout_base, ai_jinja_ast:module_name(<<"layout/base.j2">>, #{})),
    ?assertEqual(view_a_b, ai_jinja_ast:module_name(<<"a-b.j2">>,
                                                    #{prefix => <<"view_">>})),
    %% The suffix is stripped only when it is the configured one.
    ?assertEqual(j2_index_html, ai_jinja_ast:module_name(<<"index.html">>, #{})).

targets_resolve_to_modules_test() ->
    Opts = #{source => <<"t">>, templates => #{<<"b.j2">> => <<"x">>}},
    {ok, Nodes, Deps} =
        ai_jinja_ast:postprocess(parse(<<"{% include \"b.j2\" %}">>), Opts),
    ?assertMatch([{include, _, j2_b, false, true}], Nodes),
    ?assertEqual([j2_b], Deps).

missing_target_test() ->
    Opts = #{source => <<"t">>, templates => #{}},
    ?assertMatch({error, {_, _, {template_not_found, <<"b.j2">>}}},
                 ai_jinja_ast:postprocess(parse(<<"{% include \"b.j2\" %}">>), Opts)).

%% `ignore missing' is the one place a target is allowed not to exist.
ignore_missing_skips_the_check_test() ->
    Opts = #{source => <<"t">>, templates => #{}},
    ?assertMatch({ok, [{include, _, j2_b, true, true}], [j2_b]},
                 ai_jinja_ast:postprocess(
                   parse(<<"{% include \"b.j2\" ignore missing %}">>), Opts)).

%% Deviation J11.
dynamic_target_test() ->
    Opts = #{source => <<"t">>, templates => #{}},
    ?assertMatch({error, {_, _, {dynamic_target_unsupported, _}}},
                 ai_jinja_ast:postprocess(parse(<<"{% include name %}">>), Opts)).

%% A module only ever knows its direct parent, so a longer cycle is the
%% plugin's job; this one it can see for itself.
self_extends_is_a_cycle_test() ->
    Opts = #{source => <<"t">>, module => j2_b,
             templates => #{<<"b.j2">> => <<"x">>}},
    ?assertMatch({error, {_, _, {extends_cycle, [j2_b]}}},
                 ai_jinja_ast:postprocess(parse(<<"{% extends \"b.j2\" %}">>), Opts)).

deps_are_sorted_and_deduplicated_test() ->
    Opts = #{source => <<"t">>,
             templates => #{<<"a.j2">> => <<"x">>, <<"b.j2">> => <<"y">>}},
    {ok, _, Deps} =
        ai_jinja_ast:postprocess(
          parse(<<"{% include \"b.j2\" %}{% include \"a.j2\" %}{% include \"b.j2\" %}">>),
          Opts),
    ?assertEqual([j2_a, j2_b], Deps).

%%%===================================================================
%%% Performance shape
%%%===================================================================

%% Accumulating with `Acc ++ [X]' would make this quadratic; the mustache
%% engine had exactly that bug (P1).
merge_is_linear_test() ->
    N = 20000,
    Nodes = lists:append([[{text, {1, 1}, <<"a">>}] || _ <- lists:seq(1, N)]),
    {Micros, [{text, _, Merged}]} =
        timer:tc(fun() -> ai_jinja_ast:merge_text(Nodes) end),
    ?assertEqual(N, byte_size(Merged)),
    ?assert(Micros < 2000000).
