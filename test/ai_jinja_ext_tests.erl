%%%-------------------------------------------------------------------
%%% Tests for the filter/test registry and the extension behaviour.
%%%-------------------------------------------------------------------
-module(ai_jinja_ext_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Registry
%%%===================================================================

builtin_registry_is_complete_test() ->
    {ok, #{filters := F, tests := T, modules := M}} = ai_jinja_ext:registry([]),
    ?assertEqual(lists:sort(maps:keys(ai_jinja_filters:filters())),
                 lists:sort(maps:keys(F))),
    ?assertEqual(lists:sort(maps:keys(ai_jinja_tests:tests())),
                 lists:sort(maps:keys(T))),
    ?assertEqual([], M).

%% A user filter is registered, and the module it lives in is collected so the
%% compiler can allow calls to it (architecture invariant 8).
extension_is_registered_test() ->
    {ok, #{filters := F, params := P, modules := M}} =
        ai_jinja_ext:registry([ai_jinja_ext_tests_ext]),
    ?assertMatch(#{money := {ai_jinja_ext_tests_ext, money}}, F),
    ?assertEqual([currency], maps:get(money, P)),
    ?assert(lists:member(ai_jinja_ext_tests_ext, M)).

%% Shadowing a builtin is an error rather than an override: a template that
%% silently gets a different `join' because a dependency registered one is the
%% kind of bug nobody finds.
shadowing_a_builtin_is_refused_test() ->
    ?assertMatch({error, {filter_name_conflict, upper, _}},
                 ai_jinja_ext:registry([ai_jinja_ext_tests_clash])).

two_extensions_claiming_one_name_test() ->
    ?assertMatch({error, {filter_name_conflict, money, _}},
                 ai_jinja_ext:registry([ai_jinja_ext_tests_ext,
                                        ai_jinja_ext_tests_ext2])).

format_error_names_both_sides_test() ->
    Msg = ai_jinja_ext:format_error({filter_name_conflict, upper, my_mod}),
    ?assert(is_list(Msg)),
    ?assertNotEqual(nomatch, string:find(Msg, "upper")),
    ?assertNotEqual(nomatch, string:find(Msg, "my_mod")).

%%%===================================================================
%%% Use from a template
%%%===================================================================

custom_filter_is_usable_test() ->
    Opts = #{extensions => [ai_jinja_ext_tests_ext]},
    ?assertEqual(<<"9 EUR">>,
                 ai_jinja:render_string(<<"{{ 9|money('EUR') }}">>, #{}, Opts)),
    %% Positional and keyword forms have to reach the function identically.
    ?assertEqual(<<"9 EUR">>,
                 ai_jinja:render_string(<<"{{ 9|money(currency='EUR') }}">>,
                                        #{}, Opts)).

unknown_filter_is_a_compile_error_test() ->
    ?assertError({ai_jinja, {error, {_, _, {unknown_filter, nosuch}}}},
                 ai_jinja:render_string(<<"{{ 1|nosuch }}">>, #{}, #{})),
    ?assertError({ai_jinja, {error, {_, _, {unknown_test, nosuch}}}},
                 ai_jinja:render_string(<<"{{ 1 is nosuch }}">>, #{}, #{})).

%% The generated module is allowed to call the extension's module, and only
%% because the compiler collected it while resolving the filter.
extension_module_is_allowed_in_generated_code_test() ->
    Opts = #{extensions => [ai_jinja_ext_tests_ext],
             module => j2_ext_probe, source => <<"t">>},
    {ok, Nodes} = ai_jinja_parser:parse(<<"{{ x|money('EUR') }}">>, Opts),
    ?assertMatch({ok, _, []}, ai_jinja_compiler:forms(Nodes, Opts)).

%%%===================================================================
%%% Static inspection
%%%===================================================================

%% -jinja_ext(m) is validated while compiling the module that declares it, and
%% m may not be compiled yet -- so the tables have to be readable from source.
spec_module_reads_a_loaded_module_test() ->
    {ok, #{filters := F, tests := T}} =
        ai_jinja_ext:spec_module(ai_jinja_ext_tests_ext),
    ?assertEqual([money], F),
    ?assertEqual([], T).
