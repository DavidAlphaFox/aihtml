%%%-------------------------------------------------------------------
%%% @doc Unit tests for the three AST post-processing passes.
%%%
%%% Each pass is exported and tested on its own, which is the whole point of
%%% splitting them out of parse/1. Most cases build the AST by parsing a
%%% template, because that is how the passes are really fed; the `ext' cases
%%% construct nodes by hand, since producing one needs a registered extension
%%% module and the pass under test does not care where the node came from.
%%%
%%% See tasks/T09.md and tasks/T10.md.
%%% @end
%%%-------------------------------------------------------------------
-module(ai_mustache_ast_tests).

-include_lib("eunit/include/eunit.hrl").
-include("ai_mustache.hrl").

%% A stand-in loc() for hand-built nodes.
-define(L, {1, 1}).

%%%===================================================================
%%% Helpers
%%%===================================================================

%% Parse and unwrap, turning a parse failure into a readable test error.
ast(Bin) ->
    case ai_mustache_parser:parse(Bin) of
        {ok, Ns}       -> Ns;
        {error, _} = E -> erlang:error({parse_failed, Bin, E})
    end.

%% A hand-built inline extension marker; `$@' is not a builtin, so nothing
%% else in the tree can collide with it.
ext(Body) -> {ext, ?L, $@, [k], Body}.

txt(Loc, Bin) -> {text, Loc, Bin}.

%%%===================================================================
%%% merge_text/1
%%%===================================================================

merge_text_joins_adjacent_text_test() ->
    ?assertEqual([{text, {1, 1}, <<"ab">>}],
                 ai_mustache_ast:merge_text(ast(<<"a{{!c}}b">>))).

merge_text_joins_a_long_run_test() ->
    ?assertEqual([{text, {1, 1}, <<"abcd">>}],
                 ai_mustache_ast:merge_text(ast(<<"a{{!1}}b{{!2}}c{{!3}}d">>))).

%% The merged literal keeps the loc() of the FIRST fragment: diagnostics and
%% the line_map option must point at where the text started.
merge_text_keeps_the_first_fragments_loc_test() ->
    Nodes = [txt({7, 3}, <<"a">>), txt({7, 9}, <<"b">>), txt({8, 1}, <<"c">>)],
    ?assertEqual([{text, {7, 3}, <<"abc">>}], ai_mustache_ast:merge_text(Nodes)).

merge_text_stops_at_a_non_text_node_test() ->
    ?assertEqual([{text, {1, 1}, <<"ab">>},
                  {var, {1, 3}, [x], escape},
                  {text, {1, 8}, <<"cd">>}],
                 ai_mustache_ast:merge_text(
                   [txt({1, 1}, <<"a">>), txt({1, 2}, <<"b">>),
                    {var, {1, 3}, [x], escape},
                    txt({1, 8}, <<"c">>), txt({1, 9}, <<"d">>)])).

merge_text_leaves_a_single_text_alone_test() ->
    ?assertEqual([{text, {1, 1}, <<"abc">>}],
                 ai_mustache_ast:merge_text([txt({1, 1}, <<"abc">>)])).

merge_text_on_empty_input_test() ->
    ?assertEqual([], ai_mustache_ast:merge_text([])).

merge_text_is_idempotent_test() ->
    Nodes = ast(<<"a{{!c}}b{{#s}}x{{!c}}y{{/s}}z{{!c}}w">>),
    Once  = ai_mustache_ast:merge_text(Nodes),
    ?assertEqual(Once, ai_mustache_ast:merge_text(Once)).

%% P2 regression: the old merge_continuous_binary/1 recursed into sections
%% only, so text inside a {{+x}} block was never merged. Every body-bearing
%% node type now goes through the same traversal, so each gets its own case.
p2_regression_merges_inside_a_has_body_test() ->
    ?assertEqual([{has, {1, 1}, [x], [{text, {1, 7}, <<"ab">>}], true}],
                 ai_mustache_ast:merge_text(ast(<<"{{+x}}a{{! c }}b{{/x}}">>))).

p2_regression_merges_inside_a_negative_has_body_test() ->
    ?assertEqual([{has, {1, 1}, [x], [{text, {1, 7}, <<"ab">>}], false}],
                 ai_mustache_ast:merge_text(ast(<<"{{-x}}a{{! c }}b{{/x}}">>))).

p2_regression_merges_inside_a_section_body_test() ->
    ?assertEqual([{section, {1, 1}, [x], [{text, {1, 7}, <<"ab">>}]}],
                 ai_mustache_ast:merge_text(ast(<<"{{#x}}a{{! c }}b{{/x}}">>))).

p2_regression_merges_inside_an_inverted_body_test() ->
    ?assertEqual([{inverted, {1, 1}, [x], [{text, {1, 7}, <<"ab">>}]}],
                 ai_mustache_ast:merge_text(ast(<<"{{^x}}a{{! c }}b{{/x}}">>))).

p2_regression_merges_inside_an_ext_body_test() ->
    Node = ext([txt({2, 1}, <<"a">>), txt({2, 2}, <<"b">>)]),
    ?assertEqual([{ext, ?L, $@, [k], [{text, {2, 1}, <<"ab">>}]}],
                 ai_mustache_ast:merge_text([Node])).

p2_regression_merges_at_every_depth_test() ->
    ?assertEqual([{section, {1, 1}, [a],
                   [{has, {1, 7}, [b],
                     [{inverted, {1, 13}, [c],
                       [{text, {1, 19}, <<"xy">>}]}], true}]}],
                 ai_mustache_ast:merge_text(
                   ast(<<"{{#a}}{{+b}}{{^c}}x{{!m}}y{{/c}}{{/b}}{{/a}}">>))).

%% P1 regression: post-processing must not be O(n^2). 5000 alternating
%% fragments finish in well under a second with the accumulate-and-reverse
%% implementation and take minutes with `Acc ++ [X]'. The threshold is
%% deliberately loose -- the point is to catch an order-of-magnitude
%% regression, not to measure the machine.
p1_regression_merge_text_is_linear_test_() ->
    {timeout, 30,
     fun() ->
             Nodes = [txt({1, I}, integer_to_binary(I)) || I <- lists:seq(1, 5000)],
             {Micros, Merged} = timer:tc(fun() -> ai_mustache_ast:merge_text(Nodes) end),
             ?assertMatch([{text, {1, 1}, _}], Merged),
             ?assert(Micros < 1000000)
     end}.

%%%===================================================================
%%% drop_empty/1
%%%===================================================================

drop_empty_removes_an_empty_has_test() ->
    ?assertEqual([], ai_mustache_ast:drop_empty(ast(<<"{{+x}}{{/x}}">>))),
    ?assertEqual([], ai_mustache_ast:drop_empty(ast(<<"{{-x}}{{/x}}">>))).

drop_empty_keeps_a_non_empty_has_test() ->
    ?assertEqual([{has, {1, 1}, [x], [{text, {1, 7}, <<"y">>}], true}],
                 ai_mustache_ast:drop_empty(ast(<<"{{+x}}y{{/x}}">>))).

%% An empty section must NOT be dropped. `{{#w}}{{/w}}' where `w' holds a
%% fun/2 still has to call F(<<>>, Frame) and emit whatever it returns; the
%% compiler cannot prove at build time that the key is not a function, so
%% removing the node would silently lose output.
drop_empty_keeps_an_empty_section_because_of_fun2_lambdas_test() ->
    ?assertEqual([{section, {1, 1}, [w], []}],
                 ai_mustache_ast:drop_empty(ast(<<"{{#w}}{{/w}}">>))).

%% Same reasoning for an inverted section: an empty body is still a branch
%% that has to be evaluated, and its emptiness is not a licence to delete it.
drop_empty_keeps_an_empty_inverted_section_test() ->
    ?assertEqual([{inverted, {1, 1}, [w], []}],
                 ai_mustache_ast:drop_empty(ast(<<"{{^w}}{{/w}}">>))).

%% An inline extension legitimately has no body, and a block extension with
%% an empty one is indistinguishable from it here, so ext is always kept.
drop_empty_keeps_an_empty_ext_test() ->
    ?assertEqual([ext([])], ai_mustache_ast:drop_empty([ext([])])).

drop_empty_leaves_leaf_nodes_alone_test() ->
    Nodes = [txt(?L, <<"a">>),
             {var, ?L, [x], escape},
             {lambda, ?L, [f]},
             {partial, ?L, <<"p">>, <<>>}],
    ?assertEqual(Nodes, ai_mustache_ast:drop_empty(Nodes)).

%% The pass descends: an empty has nested inside a section is dropped too.
%% The old remove_empty_section/1 only walked the top level.
drop_empty_descends_into_a_section_test() ->
    ?assertEqual([{section, {1, 1}, [a], []}],
                 ai_mustache_ast:drop_empty(ast(<<"{{#a}}{{+b}}{{/b}}{{/a}}">>))).

drop_empty_descends_into_every_body_type_test() ->
    ?assertEqual([{inverted, {1, 1}, [a], []}],
                 ai_mustache_ast:drop_empty(ast(<<"{{^a}}{{+b}}{{/b}}{{/a}}">>))),
    ?assertEqual([{has, {1, 1}, [a], [{text, {1, 7}, <<"k">>}], true}],
                 ai_mustache_ast:drop_empty(
                   ast(<<"{{+a}}k{{+b}}{{/b}}{{/a}}">>))),
    ?assertEqual([ext([])],
                 ai_mustache_ast:drop_empty(
                   [ext([{has, ?L, [b], [], true}])])).

%% Dropping the only child of a has empties it, and the now-empty has is
%% dropped in turn -- the inner pass runs before the outer decision.
drop_empty_cascades_upwards_test() ->
    ?assertEqual([], ai_mustache_ast:drop_empty(
                       ast(<<"{{+a}}{{+b}}{{/b}}{{/a}}">>))).

drop_empty_is_idempotent_test() ->
    Nodes = ast(<<"{{#a}}{{+b}}{{/b}}x{{/a}}{{+c}}{{/c}}">>),
    Once  = ai_mustache_ast:drop_empty(Nodes),
    ?assertEqual(Once, ai_mustache_ast:drop_empty(Once)).

%% P3 regression: both halves in one case -- the empty section survives and
%% the pass really did descend.
p3_regression_drop_empty_descends_and_keeps_sections_test() ->
    ?assertEqual([{section, {1, 1}, [a],
                   [{section, {1, 7}, [b], []}]}],
                 ai_mustache_ast:drop_empty(
                   ast(<<"{{#a}}{{#b}}{{/b}}{{/a}}">>))).

%%%===================================================================
%%% resolve_partials/2
%%%===================================================================
%% {Title, RawName, ExpectedModule}

module_name_cases() ->
    [{"a bare name",            <<"index">>,          view_index},
     {"a slash becomes _",      <<"shared/item">>,    view_shared_item},
     {"a nested path",          <<"layout/default">>, view_layout_default},
     {"a dash becomes _",       <<"a-b">>,            view_a_b},
     {"a dot becomes _",        <<"a.b">>,            view_a_b},
     {"all three at once",      <<"a-b/c.d">>,        view_a_b_c_d},
     {"several path segments",  <<"a/b/c">>,          view_a_b_c}].

module_name_test_() ->
    [{Title, ?_assertEqual(Expected, ai_mustache_ast:module_name(Input, #{}))}
     || {Title, Input, Expected} <- module_name_cases()].

module_name_honours_the_prefix_option_test() ->
    ?assertEqual(tpl_shared_item,
                 ai_mustache_ast:module_name(<<"shared/item">>,
                                             #{prefix => <<"tpl_">>})),
    ?assertEqual(shared_item,
                 ai_mustache_ast:module_name(<<"shared/item">>,
                                             #{prefix => <<>>})).

default_prefix_test() ->
    ?assertEqual(<<"view_">>, ai_mustache_ast:default_prefix()).

resolve_partials_rewrites_the_target_test() ->
    ?assertEqual({ok, [{partial, {1, 1}, view_shared_item, <<>>},
                       {partial, {1, 17}, view_index, <<>>},
                       {partial, {1, 27}, view_layout_default, <<>>}],
                  [view_index, view_layout_default, view_shared_item]},
                 ai_mustache_ast:resolve_partials(
                   ast(<<"{{>shared/item}}{{>index}}{{>layout/default}}">>), #{})).

resolve_partials_honours_the_prefix_option_test() ->
    ?assertEqual({ok, [{partial, {1, 1}, tpl_index, <<>>}], [tpl_index]},
                 ai_mustache_ast:resolve_partials(ast(<<"{{>index}}">>),
                                                  #{prefix => <<"tpl_">>})).

resolve_partials_keeps_the_indent_test() ->
    ?assertEqual({ok, [{partial, {1, 3}, view_p, <<"  ">>}], [view_p]},
                 ai_mustache_ast:resolve_partials(ast(<<"  {{> p}}\n">>), #{})).

%% Deps are sorted and de-duplicated, so the dependency list of a template is
%% a function of its content alone -- which is what makes the plugin's
%% incremental rebuild stable.
resolve_partials_deps_are_sorted_and_unique_test() ->
    {ok, _, Deps} =
        ai_mustache_ast:resolve_partials(
          ast(<<"{{>z}}{{>a}}{{>z}}{{>m}}{{>a}}">>), #{}),
    ?assertEqual([view_a, view_m, view_z], Deps).

%% A partial buried in a body still contributes to Deps and is still
%% rewritten in place.
resolve_partials_descends_into_bodies_test() ->
    ?assertEqual({ok, [{partial, {1, 1}, view_b, <<>>},
                       {section, {1, 7}, [s],
                        [{partial, {1, 13}, view_a, <<>>},
                         {partial, {1, 19}, view_b, <<>>}]}],
                  [view_a, view_b]},
                 ai_mustache_ast:resolve_partials(
                   ast(<<"{{>b}}{{#s}}{{>a}}{{>b}}{{/s}}">>), #{})).

resolve_partials_descends_into_every_body_type_test() ->
    Check = fun(Bin) ->
                    {ok, _, Deps} =
                        ai_mustache_ast:resolve_partials(ast(Bin), #{}),
                    Deps
            end,
    ?assertEqual([view_p], Check(<<"{{#s}}{{>p}}{{/s}}">>)),
    ?assertEqual([view_p], Check(<<"{{^s}}{{>p}}{{/s}}">>)),
    ?assertEqual([view_p], Check(<<"{{+s}}{{>p}}{{/s}}">>)),
    ?assertEqual([view_p], Check(<<"{{-s}}{{>p}}{{/s}}">>)),
    ?assertEqual({ok, [ext([{partial, ?L, view_p, <<>>}])], [view_p]},
                 ai_mustache_ast:resolve_partials(
                   [ext([{partial, ?L, <<"p">>, <<>>}])], #{})).

resolve_partials_deeply_nested_test() ->
    {ok, _, Deps} =
        ai_mustache_ast:resolve_partials(
          ast(<<"{{#a}}{{^b}}{{+c}}{{>deep/one}}{{/c}}{{/b}}{{/a}}{{>top}}">>),
          #{}),
    ?assertEqual([view_deep_one, view_top], Deps).

resolve_partials_without_partials_test() ->
    Nodes = ai_mustache_ast:merge_text(ast(<<"a{{x}}b">>)),
    ?assertEqual({ok, Nodes, []}, ai_mustache_ast:resolve_partials(Nodes, #{})).

%% With a views root configured, a missing template is reported rather than
%% compiled into a call to a module that will never exist.
resolve_partials_reports_a_missing_template_test() ->
    Dir = filename:join(test_dir(), "no_such_views_dir"),
    ?assertEqual({error, {<<"v.mustache">>, 1, {partial_not_found, <<"gone">>}}},
                 ai_mustache_ast:resolve_partials(
                   ast(<<"{{>gone}}">>),
                   #{views => Dir, source => <<"v.mustache">>})).

test_dir() ->
    filename:dirname(code:which(?MODULE)).

%%%===================================================================
%%% postprocess/2
%%%===================================================================

%% postprocess/2 is exactly the three passes in order. Asserting the identity
%% pins the order down without duplicating the individual pass expectations.
postprocess_is_the_three_passes_in_order_test() ->
    Nodes = ast(<<"a{{!c}}b{{#s}}x{{!c}}y{{+e}}{{/e}}{{>p}}{{/s}}{{>p}}">>),
    Expected = ai_mustache_ast:resolve_partials(
                 ai_mustache_ast:drop_empty(
                   ai_mustache_ast:merge_text(Nodes)), #{}),
    ?assertEqual(Expected, ai_mustache_ast:postprocess(Nodes, #{})).

postprocess_result_shape_test() ->
    ?assertEqual({ok, [{text, {1, 1}, <<"ab">>},
                       {partial, {1, 9}, view_p, <<>>}],
                  [view_p]},
                 ai_mustache_ast:postprocess(ast(<<"a{{!c}}b{{>p}}">>), #{})).

postprocess_propagates_a_partial_error_test() ->
    Dir = filename:join(test_dir(), "no_such_views_dir"),
    ?assertMatch({error, {_, 1, {partial_not_found, <<"gone">>}}},
                 ai_mustache_ast:postprocess(ast(<<"a{{>gone}}">>),
                                             #{views => Dir})).

postprocess_is_idempotent_on_the_node_list_test() ->
    Nodes = ast(<<"a{{!c}}b{{#s}}x{{!c}}y{{+e}}{{/e}}{{/s}}">>),
    {ok, Once, Deps1} = ai_mustache_ast:postprocess(Nodes, #{}),
    {ok, Twice, Deps2} = ai_mustache_ast:postprocess(Once, #{}),
    ?assertEqual(Once, Twice),
    ?assertEqual(Deps1, Deps2).

%% Stability: post-processing must be a pure function of its input, byte for
%% byte. The plugin hashes the result to decide whether to recompile, so any
%% run-to-run variation would either rebuild everything or nothing.
postprocess_is_byte_stable_test() ->
    Nodes = ast(<<"a{{!c}}b{{#s}}{{>z}}{{>a}}{{.}}{{/s}}{{>a}}{{+e}}{{/e}}">>),
    {ok, N1, D1} = ai_mustache_ast:postprocess(Nodes, #{}),
    {ok, N2, D2} = ai_mustache_ast:postprocess(Nodes, #{}),
    ?assertEqual(term_to_binary({N1, D1}), term_to_binary({N2, D2})).

postprocess_is_byte_stable_across_reparses_test() ->
    Bin = <<"x\n{{#s}}\n  {{>shared/item}}\n{{/s}}\n{{! done }}\n">>,
    Run = fun() ->
                  {ok, N, D} = ai_mustache_ast:postprocess(ast(Bin), #{}),
                  term_to_binary({N, D})
          end,
    ?assertEqual(1, length(lists:usort([Run() || _ <- lists:seq(1, 20)]))).
