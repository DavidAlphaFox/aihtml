%%%-------------------------------------------------------------------
%%% Tests for the compiler: the generated module contract, the build stamp,
%%% and the architecture invariants that only the forms can show.
%%%-------------------------------------------------------------------
-module(ai_jinja_compiler_tests).

-include_lib("eunit/include/eunit.hrl").

compile(Src) -> compile(Src, #{}).
compile(Src, Extra) ->
    Mod = list_to_atom("j2_probe_" ++ integer_to_list(erlang:unique_integer([positive]))),
    Opts = maps:merge(#{module => Mod, source => <<"t.j2">>}, Extra),
    {ok, Nodes} = ai_jinja_parser:parse(Src, Opts),
    {ok, Forms, Deps} = ai_jinja_compiler:forms(Nodes, Opts),
    {Mod, Forms, Deps}.

load(Src) -> load(Src, #{}).

%% Loaded probes are marked as string-compiled, which is what they are: they
%% have no file on disk, and without the mark ai_jinja_dev:check/0 would report
%% every one of them as a template whose source went missing.
load(Src, Extra) ->
    {Mod, Forms, _} = compile(Src, Extra#{origin => string}),
    {ok, Mod, Bin} = compile:forms(Forms, [return_errors, binary]),
    {module, Mod} = code:load_binary(Mod, "t.j2", Bin),
    Mod.

%%%===================================================================
%%% Module contract
%%%===================================================================

contract_test() ->
    Mod = load(<<"x">>),
    Exports = Mod:module_info(exports),
    [?assert(lists:member(E, Exports))
     || E <- [{render, 1}, {render_iolist, 1}, {render_scope, 2},
              {render_with, 3}, {blocks, 0}, {all_blocks, 0}, {partials, 0}]].

%% Read from the forms rather than from a loaded module: loading it would
%% leave a template module behind whose `t.j2' does not exist, and
%% ai_jinja_dev:check/0 would rightly complain about it.
self_description_test() ->
    Src = attribute(compile(<<"x">>, #{stamp => <<"S">>, mtime => 42})),
    ?assertMatch(#{path := <<"t.j2">>, stamp := <<"S">>, mtime := 42, vsn := 1},
                 Src),
    %% `origin' is absent for a file template, so its generated .erl is exactly
    %% what it always was.
    ?assertNot(maps:is_key(origin, Src)),
    %% The options are a SORTED LIST, not a map: a map prints in
    %% maps:to_list/1 order, which follows the VM's atom table, and the
    %% generated file has to be identical wherever it is built.
    Opts = maps:get(opts, Src),
    ?assert(is_list(Opts)),
    ?assertEqual(lists:sort(Opts), Opts).

origin_is_recorded_for_string_templates_test() ->
    Src = attribute(compile(<<"x">>, #{origin => string})),
    ?assertEqual(string, maps:get(origin, Src)).

attribute({_Mod, Forms, _Deps}) ->
    [A] = [Map || {attribute, _, jinja_source, Map} <- Forms],
    A.

blocks_and_all_blocks_test() ->
    Mod = load(<<"{% block a %}x{% endblock %}{% block b %}y{% endblock %}">>),
    ?assertEqual([a, b], lists:sort(maps:keys(Mod:blocks()))),
    %% Without a parent the two are the same map.
    ?assertEqual(Mod:blocks(), Mod:all_blocks()),
    ?assert(is_function(maps:get(a, Mod:blocks()), 3)).

%% Architecture invariant 9: a module names its DIRECT parent and nobody else,
%% or a base template gaining a block would force a rebuild of every
%% descendant.
only_the_direct_parent_is_named_test() ->
    {_, Forms, Deps} =
        compile(<<"{% extends \"mid.j2\" %}{% block t %}x{% endblock %}">>,
                #{templates => #{<<"mid.j2">> => <<>>, <<"base.j2">> => <<>>}}),
    ?assertEqual([j2_mid], Deps),
    Called = lists:usort(ai_html_forms:remotes(Forms, [])),
    ?assert(lists:member(j2_mid, Called)),
    ?assertNot(lists:member(j2_base, Called)).

%%%===================================================================
%%% Invariant 8: what generated code may call
%%%===================================================================

generated_code_calls_only_the_allowed_set_test() ->
    Src = <<"{% extends \"b.j2\" %}"
            "{% block t %}{% for x in xs if x %}{{ x|upper }}{{ loop.index }}"
            "{% endfor %}{% if a is odd %}{{ a }}{% endif %}"
            "{% include \"c.j2\" %}{% endblock %}">>,
    Templates = #{<<"b.j2">> => <<>>, <<"c.j2">> => <<>>},
    {Mod, Forms, Deps} = compile(Src, #{templates => Templates}),
    Allowed = [erlang, lists, maps, code, ai_jinja_rt, ai_jinja_filters,
               ai_jinja_tests, ai_html_escape, Mod | Deps],
    ?assertEqual([], [M || M <- lists:usort(ai_html_forms:remotes(Forms, [])),
                           not lists:member(M, Allowed)]).

%% The check is real, not decorative: an extension whose module is not
%% collected would be caught.
check_remotes_rejects_an_unknown_module_test() ->
    %% ai_jinja_ext_tests_ext is only allowed because the compiler collects it
    %% while resolving the filter; without the extension the name is unknown.
    ?assertMatch({error, {_, _, {unknown_filter, money}}},
                 begin
                     Opts = #{module => j2_probe_x, source => <<"t">>},
                     {ok, N} = ai_jinja_parser:parse(<<"{{ 1|money('E') }}">>, Opts),
                     ai_jinja_compiler:forms(N, Opts)
                 end).

%%%===================================================================
%%% The build stamp
%%%===================================================================

stamp_covers_every_shaping_option_test() ->
    Base = #{},
    S0 = ai_jinja_compiler:source_hash(<<"x">>, Base),
    [?assertNotEqual(S0, ai_jinja_compiler:source_hash(<<"x">>, Base#{K => V}))
     || {K, V} <- [{prefix, <<"p_">>}, {views, <<"v">>}, {extensions, [m]},
                   {escape, false}, {trim_blocks, false},
                   {lstrip_blocks, false}, {keep_trailing_newline, true},
                   {strict_undefined, true}, {line_map, false}]].

%% An absolute path would stamp the developer's home directory into every
%% artefact and make the build unreproducible across machines.
stamp_ignores_views_abs_and_suffix_test() ->
    S = ai_jinja_compiler:source_hash(<<"x">>, #{}),
    ?assertEqual(S, ai_jinja_compiler:source_hash(<<"x">>,
                                                  #{views_abs => <<"/home/a">>})),
    ?assertEqual(S, ai_jinja_compiler:source_hash(<<"x">>,
                                                  #{views_abs => <<"/home/b">>})),
    ?assertEqual(S, ai_jinja_compiler:source_hash(<<"x">>, #{suffix => <<".x">>})).

%% Whether the stamp matched must not depend on how a caller spelled a path.
stamp_normalises_paths_test() ->
    ?assertEqual(ai_jinja_compiler:source_hash(<<"x">>, #{views => "views"}),
                 ai_jinja_compiler:source_hash(<<"x">>, #{views => <<"views">>})).

stamp_follows_the_content_test() ->
    ?assertNotEqual(ai_jinja_compiler:source_hash(<<"a">>, #{}),
                    ai_jinja_compiler:source_hash(<<"b">>, #{})).

%%%===================================================================
%%% Code shape
%%%===================================================================

%% A body with no {% set %} must not pay for the scope threading that only a
%% {% set %} needs.
no_tuple_threading_without_set_test() ->
    {_, WithSet, _} = compile(<<"{% if a %}{% set x = 1 %}{{ x }}{% endif %}">>),
    {_, Without, _} = compile(<<"{% if a %}y{% endif %}">>),
    ?assert(counts_matches(WithSet) > counts_matches(Without)).

counts_matches(Forms) -> count(Forms, 0).
count({match, _, _, _} = T, N) -> count(tuple_to_list(T), N + 1);
count(T, N) when is_tuple(T)   -> count(tuple_to_list(T), N);
count([H | T], N)              -> count(T, count(H, N));
count(_, N)                    -> N.

%% A plain loop is a list comprehension; needing `loop' makes it a fold.
loop_variable_changes_the_shape_test() ->
    {_, Plain, _} = compile(<<"{% for x in xs %}{{ x }}{% endfor %}">>),
    {_, Folded, _} = compile(<<"{% for x in xs %}{{ loop.index }}{% endfor %}">>),
    ?assert(has_lc(Plain)),
    ?assertNot(has_lc(Folded)).

has_lc({lc, _, _, _})       -> true;
has_lc(T) when is_tuple(T)  -> has_lc(tuple_to_list(T));
has_lc([H | T])             -> has_lc(H) orelse has_lc(T);
has_lc(_)                   -> false.

%% Filters resolve to a direct call at compile time -- no lookup, no apply.
filters_are_statically_bound_test() ->
    {_, Forms, _} = compile(<<"{{ x|upper }}">>),
    ?assert(lists:member(ai_jinja_filters, ai_html_forms:remotes(Forms, []))),
    ?assertEqual([], [1 || {call, _, {remote, _, {atom, _, erlang},
                                      {atom, _, apply}}, _} <- flatten(Forms)]).

flatten(T) when is_tuple(T) -> [T | flatten(tuple_to_list(T))];
flatten([H | T])            -> flatten(H) ++ flatten(T);
flatten(_)                  -> [].

%% Generated modules are compiled with warnings_as_errors in a real build.
generated_module_is_warning_free_test() ->
    Srcs = [<<"x">>,
            <<"{{ a }}{% if b %}{% set c = 1 %}{{ c }}{% endif %}">>,
            <<"{% for x in xs %}{{ loop.index }}{{ x }}{% endfor %}">>,
            <<"{% for x in xs if x %}{{ x }}{% else %}none{% endfor %}">>,
            <<"{% macro m(a, b=1) %}{{ a }}{{ b }}{{ varargs }}{% endmacro %}{{ m(1) }}">>,
            <<"{% block a %}x{% endblock %}">>,
            <<"{% with a = 1 %}{{ a }}{% endwith %}">>,
            <<"{% filter upper %}x{% endfilter %}">>,
            <<"{% for n in t recursive %}{{ loop(n) }}{% endfor %}">>,
            <<"{% for x in xs %}{{ loop.changed(x) }}{% endfor %}">>],
    [begin
         {Mod, Forms, _} = compile(S),
         ?assertMatch({ok, Mod, _},
                      compile:forms(Forms, [return_errors, warnings_as_errors,
                                            binary]))
     end || S <- Srcs].

%%%===================================================================
%%% Diagnostics
%%%===================================================================

super_outside_a_block_test() ->
    Opts = #{module => j2_probe_s, source => <<"t">>},
    {ok, N} = ai_jinja_parser:parse(<<"{{ super() }}">>, Opts),
    ?assertMatch({error, {_, _, {super_outside_block, _}}},
                 ai_jinja_compiler:forms(N, Opts)).

namespace_assignment_test() ->
    Opts = #{module => j2_probe_n, source => <<"t">>},
    {ok, N} = ai_jinja_parser:parse(
                <<"{% set ns = namespace(t=0) %}{% set ns.t = 1 %}">>, Opts),
    ?assertMatch({error, {_, _, {namespace_assignment_unsupported, _}}},
                 ai_jinja_compiler:forms(N, Opts)).

required_block_without_an_implementation_test() ->
    Mod = load(<<"{% block m required %}{% endblock %}">>),
    ?assertError({ai_jinja, {required_block_not_provided, m}}, Mod:render(#{})).

%% When the caller says what each target exports -- the plugin and
%% render_string/3 both do -- a typo is a build error, not an undef.
missing_imported_macro_test() ->
    Opts = #{module => j2_probe_i, source => <<"t">>,
             templates => #{<<"l.j2">> => <<>>},
             macros => #{j2_l => [known]}},
    {ok, N} = ai_jinja_parser:parse(<<"{% from \"l.j2\" import nope %}">>, Opts),
    ?assertMatch({error, {_, _, {macro_not_found, nope, j2_l}}},
                 ai_jinja_compiler:forms(N, Opts)).
