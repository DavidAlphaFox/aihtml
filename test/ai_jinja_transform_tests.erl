%%%-------------------------------------------------------------------
%%% @doc Tests for ai_jinja_transform: the three source forms.
%%%
%%% Most cases compile a real module and run it, because that is the only way
%%% to prove a parse_transform works: forms that look right can still fail to
%%% compile, and an expansion the walker never reached quietly falls back to
%%% the run-time path without failing anything.
%%%
%%% The centre of gravity is diff_cases/0, which drives the same table down
%%% both paths -- expanded at compile time and compiled at run time -- and
%%% demands byte-identical output. Two evaluators for one language is the risk
%%% this design takes on; this is what holds them together.
%%% @end
%%%-------------------------------------------------------------------
-module(ai_jinja_transform_tests).

-include_lib("eunit/include/eunit.hrl").

%% A second parse_transform, used to prove ai_jinja_transform does not assume
%% it runs first or last.
-export([parse_transform/2]).

%%%===================================================================
%%% Fixtures
%%%===================================================================

%% Fixture modules are checked in as `<name>.erl.src', not `.erl': anything
%% ending in .erl under test/ is picked up by rebar3's eunit provider, and
%% several of these exist to fail.
fixture_dir() ->
    filename:join(code:lib_dir(aihtml), "test/fixtures_transform").

fixture_path(Mod) ->
    filename:join(fixture_dir(), atom_to_list(Mod) ++ ".erl.src").

fixture_forms(Mod) ->
    {ok, Forms} = epp:parse_file(fixture_path(Mod), [fixture_dir()], []),
    Forms.

compile_fixture(Mod, Opts) ->
    compile:forms(fixture_forms(Mod), [binary, return | Opts]).

load_fixture(Mod) -> load_fixture(Mod, []).

load_fixture(Mod, Opts) ->
    {ok, Mod, Bin, _Ws} = compile_fixture(Mod, Opts),
    _ = code:purge(Mod),
    {module, Mod} = code:load_binary(Mod, atom_to_list(Mod), Bin),
    Mod.

%% The views option makes the fixture's templates findable and, for form (c),
%% silences the "needs the plugin" warning.
opts() -> [{jinja_opts, [{views, fixture_dir()}]}].

setup() ->
    _ = load_fixture(jf_filters),
    _ = load_fixture(jf_shadow),
    ok.

%%%===================================================================
%%% The three forms, compiled and run
%%%===================================================================

all_forms_test_() ->
    {setup, fun setup/0,
     fun(_) ->
             Mod = load_fixture(jf_all, opts()),
             [{"inline expands and renders",
               ?_assertEqual(<<"Hi a!">>, Mod:greet(<<"a">>))},
              {"interpolation is escaped",
               ?_assertEqual(<<"Hi a &amp; b!">>, Mod:greet(<<"a & b">>))},
              %% Two expansions in one function must not bind the same
              %% variables twice.
              {"two expansions in one function",
               ?_assertEqual(<<"a1b1">>, Mod:twice(1))},
              {"file template becomes Name/1",
               ?_assertEqual(<<"<li>1</li><li>2</li>">>,
                             Mod:simple(#{xs => [1, 2]}))},
              {"and Name_iolist/1",
               ?_assertEqual(<<"<li>1</li>">>,
                             iolist_to_binary(Mod:simple_iolist(#{xs => [1]})))},
              %% -jinja_template is kept in the beam: the plugin's staleness
              %% fallback reads it.
              {"the template attribute survives",
               ?_assertEqual([{simple, "views/j2_simple.j2"}],
                             proplists:get_value(jinja_template,
                                                 Mod:module_info(attributes)))},
              %% -jinja_ext is consumed; it means nothing at run time.
              {"the extension attribute is consumed",
               ?_assertEqual(undefined,
                             proplists:get_value(jinja_ext,
                                                 Mod:module_info(attributes)))}]
     end}.

%% Every expression position the walker has to reach. A missed one degrades
%% silently to the run-time path, which is the hardest kind of bug to notice.
every_expression_position_test_() ->
    {setup, fun setup/0,
     fun(_) ->
             Mod = load_fixture(jf_all, opts()),
             [{atom_to_list(F),
               ?_assertEqual(Expected, Mod:F(1))}
              || {F, Expected} <- [{in_case, <<"case 1">>},
                                   {in_fun, <<"fun 1">>},
                                   {in_try, <<"try 1">>},
                                   {in_binary, <<"bin 1!">>},
                                   {in_map, <<"map 1">>},
                                   {nested, <<"[1]">>}]]
                 ++ [{"in_lc", ?_assertEqual([<<"lc 1">>], Mod:in_lc(1))}]
     end}.

%%%===================================================================
%%% Expansion and fallback agree
%%%===================================================================

%% Every template here is compiled twice: once by the transform into the
%% module below, once at run time by ai_jinja:inline/2. The outputs have to
%% match byte for byte.
diff_cases() ->
    [{<<"plain">>, #{}},
     {<<"{{ n }}">>, #{n => <<"<b>">>}},
     {<<"{{ n|upper }}">>, #{n => <<"ab">>}},
     {<<"{% if n %}y{% else %}n{% endif %}">>, #{n => 1}},
     {<<"{% for x in xs %}{{ x }},{% endfor %}">>, #{xs => [1, 2]}},
     {<<"{% for x in xs %}{{ loop.index }}{% endfor %}">>, #{xs => [1, 2]}},
     {<<"{% set a = 1 %}{{ a }}">>, #{}},
     {<<"{% with v = 2 %}{{ v }}{% endwith %}">>, #{}},
     {<<"{% filter upper %}x{% endfilter %}">>, #{}},
     {<<"{% macro m(a) %}[{{ a }}]{% endmacro %}{{ m(1) }}">>, #{}}].

expansion_matches_the_fallback_test_() ->
    Mod = build_diff_module(),
    [{binary_to_list(T),
      ?_assertEqual(ai_jinja:inline(T, C), Mod:F(C))}
     || {{T, C}, F} <- lists:zip(diff_cases(), diff_names())].

diff_names() ->
    [list_to_atom("case_" ++ integer_to_list(I))
     || I <- lists:seq(1, length(diff_cases()))].

%% Build a module whose functions are exactly the inline calls of diff_cases/0,
%% so both paths see the same source text.
build_diff_module() ->
    Mod = ai_jinja_transform_diff,
    Src = ["-module(", atom_to_list(Mod), ").\n",
           "-compile({parse_transform, ai_jinja_transform}).\n",
           "-compile(export_all).\n",
           "-compile(nowarn_export_all).\n",
           [[atom_to_list(F), "(Ctx) -> ai_jinja:inline(<<\"",
             escape_erl(T), "\">>, Ctx).\n"]
            || {{T, _}, F} <- lists:zip(diff_cases(), diff_names())]],
    Forms = forms_of(lists:flatten(Src)),
    {ok, Mod, Bin, _} = compile:forms(Forms, [binary, return]),
    _ = code:purge(Mod),
    {module, Mod} = code:load_binary(Mod, atom_to_list(Mod), Bin),
    Mod.

escape_erl(Bin) ->
    lists:flatmap(fun($") -> "\\\"";
                     ($\\) -> "\\\\";
                     (C)  -> [C]
                  end, unicode:characters_to_list(Bin)).

forms_of(Src) ->
    {ok, Toks, _} = erl_scan:string(Src, 1, [text]),
    Split = split_dots(Toks, [], []),
    [begin {ok, F} = erl_parse:parse_form(Ts), F end || Ts <- Split].

split_dots([], [], Acc) -> lists:reverse(Acc);
split_dots([{dot, _} = D | T], Cur, Acc) ->
    split_dots(T, [], [lists:reverse([D | Cur]) | Acc]);
split_dots([H | T], Cur, Acc) ->
    split_dots(T, [H | Cur], Acc);
split_dots([], Cur, Acc) ->
    lists:reverse([lists:reverse(Cur) | Acc]).

%%%===================================================================
%%% Diagnostics
%%%===================================================================

%% Shadowing a builtin filter is refused while compiling the module that
%% declares the extension, which is the whole point of -jinja_ext.
shadowing_a_builtin_is_an_error_test() ->
    _ = load_fixture(jf_shadow),
    {error, Es, _Ws} = compile_fixture(jf_clash, []),
    ?assert(reason_present(Es, filter_name_conflict)).

%% Both paths refuse the same statements, via one shared predicate.
inline_include_is_an_error_test() ->
    {error, Es, _Ws} = compile_fixture(jf_inline_include, []),
    ?assert(reason_present(Es, target_in_inline_template)),
    ?assertError({ai_jinja, {error, {_, _, {target_in_inline_template, _}}}},
                 ai_jinja:inline(<<"{% include \"x.j2\" %}">>, #{})).

%% A non-literal template still works; it is only slower, so it warns.
non_literal_warns_and_still_works_test() ->
    {ok, jf_not_literal, Bin, Ws} = compile_fixture(jf_not_literal, []),
    ?assert(reason_present(Ws, inline_not_literal)),
    _ = code:purge(jf_not_literal),
    {module, _} = code:load_binary(jf_not_literal, "jf_not_literal", Bin),
    ?assertEqual(<<"1">>, jf_not_literal:f(<<"{{ n }}">>)).

silencing_the_warning_test() ->
    {ok, jf_not_literal, _, Ws} =
        compile_fixture(jf_not_literal, [nowarn_jinja_inline]),
    ?assertNot(reason_present(Ws, inline_not_literal)).

%% The error names the directories actually searched; a bare "not found"
%% leaves the user with nowhere to start.
missing_template_lists_the_directories_test() ->
    {error, Es, _} = compile_fixture(jf_missing_template, []),
    ?assert(reason_present(Es, template_not_found)),
    Msg = ai_jinja_transform:format_error(
            {template_not_found, "views/x.j2", ["a", "b"]}),
    ?assertNotEqual(nomatch, string:find(Msg, "a, b")).

%% A problem inside the template is reported against the .j2 at its own line,
%% not against the .erl: that is the file the user has to edit.
template_errors_point_at_the_template_test() ->
    {error, Es, _} = compile_fixture(jf_broken_template, opts()),
    Files = [F || {F, _} <- Es],
    ?assert(lists:any(fun(F) -> string:find(F, "j2_broken.j2") =/= nomatch end,
                      Files)),
    [{_, [{Line, _, _} | _]}] = [E || {F, _} = E <- Es,
                                      string:find(F, "j2_broken.j2") =/= nomatch],
    ?assertEqual(2, Line).

reason_present(Groups, Reason) ->
    lists:any(fun({_File, Ds}) ->
                      lists:any(fun({_, _, R}) -> tag(R) =:= Reason end, Ds)
              end, Groups).

tag(R) when is_tuple(R) -> element(1, R);
tag(R)                  -> R.

%%%===================================================================
%%% Coexistence
%%%===================================================================

%% A module with neither attribute nor inline call must come out exactly as it
%% went in.
no_jinja_forms_is_a_no_op_test() ->
    Forms = fixture_forms(jf_filters),
    ?assertEqual(Forms, ai_jinja_transform:parse_transform(Forms, [])).

%% Running twice must be a no-op the second time: the attributes are consumed
%% and the calls are gone.
idempotent_test() ->
    Forms = fixture_forms(jf_not_literal),
    Once = strip(ai_jinja_transform:parse_transform(Forms, [nowarn_jinja_inline])),
    Twice = strip(ai_jinja_transform:parse_transform(Once, [nowarn_jinja_inline])),
    ?assertEqual(Once, Twice).

strip({warning, Forms, _}) -> Forms;
strip(Forms)               -> Forms.

%% The transform must not assume it runs first or last -- another one may have
%% inserted forms ahead of -module, which is why the module name is found by
%% searching rather than by taking hd/1.
coexists_with_another_transform_test() ->
    [begin
         {ok, jf_all, Bin, _} = compile_fixture(jf_all, [{parse_transform, ?MODULE} | opts()]),
         _ = code:purge(jf_all),
         {module, _} = code:load_binary(jf_all, "jf_all", Bin),
         ?assertEqual(<<"Hi a!">>, jf_all:greet(<<"a">>))
     end || _ <- [1]],
    ok.

%% Both engines in one module, each consuming only its own attributes.
both_engines_in_one_module_test() ->
    Opts = [{mustache_opts, [{views, fixture_dir()}]},
            {jinja_opts, [{views, fixture_dir()}]}],
    {ok, jf_both_engines, Bin, _} = compile_fixture(jf_both_engines, Opts),
    _ = code:purge(jf_both_engines),
    {module, _} = code:load_binary(jf_both_engines, "jf_both_engines", Bin),
    ?assertEqual(<<"<li>1</li>">>, jf_both_engines:j(#{xs => [1]})),
    ?assert(is_binary(jf_both_engines:m(#{}))),
    Attrs = jf_both_engines:module_info(attributes),
    ?assertMatch([{legacy, _}], proplists:get_value(mustache_template, Attrs)),
    ?assertMatch([{page, _}], proplists:get_value(jinja_template, Attrs)).

%% The second transform: it only has to be harmless.
parse_transform(Forms, _Opts) -> Forms.
