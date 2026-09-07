%%%-------------------------------------------------------------------
%%% Tests for the facade, ai_jinja.
%%%-------------------------------------------------------------------
-module(ai_jinja_tests_tests).

-include_lib("eunit/include/eunit.hrl").

%% render/2 is one direct call into a generated module: no lookup table, no
%% process, no ets.
render_is_a_direct_call_test() ->
    Mod = ai_jinja_facade_probe,
    Opts = #{module => Mod, source => <<"t.j2">>},
    {ok, Nodes} = ai_jinja_parser:parse(<<"hi {{ n }}">>, Opts),
    {ok, Forms, _} = ai_jinja_compiler:forms(Nodes, Opts),
    {ok, Mod, Bin} = compile:forms(Forms, [return_errors, binary]),
    {module, Mod} = code:load_binary(Mod, "t.j2", Bin),
    ?assertEqual(<<"hi a">>, ai_jinja:render(Mod, #{n => <<"a">>})),
    ?assertEqual(<<"hi a">>,
                 iolist_to_binary(ai_jinja:render_iolist(Mod, #{n => <<"a">>}))),
    _ = code:delete(Mod), _ = code:purge(Mod),
    ok.

%% A template set is compiled once and reused; without that the fixture suite
%% would recompile the same template 500 times.
compiled_sets_are_reused_test() ->
    T = <<"{{ n }} unique-", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
    {T1, _} = timer:tc(fun() -> ai_jinja:render_string(T, #{n => 1}, #{}) end),
    {T2, _} = timer:tc(fun() -> ai_jinja:render_string(T, #{n => 1}, #{}) end),
    ?assert(T2 * 4 < T1 orelse T2 < 200).

%% Two sets that both define `base.j2' must not collide.
template_sets_are_independent_test() ->
    A = ai_jinja:render_string(<<"{% include \"b.j2\" %}">>, #{},
                               #{templates => #{<<"b.j2">> => <<"A">>}}),
    B = ai_jinja:render_string(<<"{% include \"b.j2\" %}">>, #{},
                               #{templates => #{<<"b.j2">> => <<"B">>}}),
    ?assertEqual(<<"A">>, A),
    ?assertEqual(<<"B">>, B).

%% Options that shape the output make a different module, or the first set
%% compiled would answer for every later one.
options_are_part_of_the_set_identity_test() ->
    T = <<"{{ n }}">>,
    ?assertEqual(<<"&lt;b&gt;">>, ai_jinja:render_string(T, #{n => <<"<b>">>}, #{})),
    ?assertEqual(<<"<b>">>,
                 ai_jinja:render_string(T, #{n => <<"<b>">>}, #{escape => false})).

errors_carry_the_engine_tag_test() ->
    ?assertError({ai_jinja, {error, {_, _, {unclosed_block, 'if', _}}}},
                 ai_jinja:render_string(<<"{% if x %}">>, #{}, #{})),
    ?assertError({ai_jinja, {not_renderable, _}},
                 ai_jinja:render_string(<<"{{ n }}">>, #{n => self()}, #{})).

%% A module compiled from a string has no file to go stale against, so it is
%% marked and ai_jinja_dev skips it.
string_templates_are_marked_test() ->
    _ = ai_jinja:render_string(<<"marked {{ n }}">>, #{n => 1}, #{}),
    Marked = [M || M <- ai_jinja_dev:template_modules(),
                   {ok, S} <- [ai_jinja_dev:source(M)],
                   maps:get(origin, S, file) =:= string],
    ?assert(length(Marked) > 0),
    ?assertEqual(ok, ai_jinja_dev:check()).
