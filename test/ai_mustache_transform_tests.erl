%%%-------------------------------------------------------------------
%%% @doc Tests for ai_mustache_transform: the three source forms.
%%%
%%% Most cases compile a real module and run it, because that is the only way
%%% to prove a parse_transform works: forms that look right can still fail to
%%% compile, and an expansion the walker never reached quietly falls back to
%%% the runtime path without failing anything.
%%%
%%% The centre of gravity is diff_cases/0, which drives the same table down
%%% both paths -- expanded at compile time and interpreted at runtime -- and
%%% demands byte-identical output. Two evaluators for one language is the risk
%%% this design takes on; this is what holds them together.
%%%
%%% See tasks/T22.md -- T25.md.
%%% @end
%%%-------------------------------------------------------------------
-module(ai_mustache_transform_tests).

-include_lib("eunit/include/eunit.hrl").

%% A second parse_transform, used to prove ai_mustache_transform does not
%% assume it runs first or last.
-export([parse_transform/2]).

-define(TPL, <<"macro {{v}}">>).

%%%===================================================================
%%% Fixtures and helpers
%%%===================================================================

%% Fixture modules are checked in as `<name>.erl.src', not `.erl'. Anything
%% ending in .erl anywhere under test/ is picked up by rebar3's eunit provider
%% as a module of this project, and these are deliberately not built by the
%% build -- several of them exist to fail. epp:parse_file/3 ignores the
%% extension and compile:forms/2 honours the -compile attributes inside, so the
%% fixtures still go through the real compiler with the real parse_transform.
fixture_dir() ->
    filename:join(code:lib_dir(aihtml), "test/fixtures_transform").

fixture_path(Mod) ->
    filename:join(fixture_dir(), atom_to_list(Mod) ++ ".erl.src").

fixture_forms(Mod) ->
    {ok, Forms} = epp:parse_file(fixture_path(Mod), [fixture_dir()], []),
    Forms.

tmp_dir() ->
    D = filename:join(code:lib_dir(aihtml), "tmp_transform_tests"),
    ok = filelib:ensure_path(D),
    D.

%% Extensions must be loadable before a -mustache_tag naming them is checked.
load_exts() ->
    [load_fixture(M) || M <- [my_i18n, my_wrap, bad_ext_no_behaviour,
                              bad_ext_missing_cb, bad_ext_conflict_a,
                              bad_ext_conflict_b]],
    %% The source-fallback path looks for `<Mod>.erl', so this fixture has to
    %% exist under its real name somewhere outside test/.
    {ok, Src} = file:read_file(fixture_path(src_only_ext)),
    ok = file:write_file(filename:join(tmp_dir(), "src_only_ext.erl"), Src),
    ok.

%% Compile one of the checked-in fixture modules.
compile_fixture(Mod, Opts) ->
    compile:forms(fixture_forms(Mod), [binary, return | Opts]).

load_fixture(Mod) -> load_fixture(Mod, []).

load_fixture(Mod, Opts) ->
    {ok, Mod, Bin, _Ws} = compile_fixture(Mod, Opts),
    _ = code:purge(Mod),
    {module, Mod} = code:load_binary(Mod, fixture_path(Mod), Bin),
    Mod.

%% Compile a module written out on the fly. Error cases need exact control over
%% the source, and generating it keeps the expected line numbers next to the
%% text they belong to.
write_and_compile(Name, Source) -> write_and_compile(Name, Source, []).

write_and_compile(Name, Source, Opts) ->
    File = filename:join(tmp_dir(), atom_to_list(Name) ++ ".erl"),
    ok = file:write_file(File, Source),
    compile:file(File, [binary, return_errors, return_warnings,
                        {i, tmp_dir()} | Opts]).

write_and_load(Name, Source) -> write_and_load(Name, Source, []).

write_and_load(Name, Source, Opts) ->
    {ok, Name, Bin, _Ws} = write_and_compile(Name, Source, Opts),
    _ = code:purge(Name),
    {module, Name} = code:load_binary(Name, atom_to_list(Name), Bin),
    Name.

%% [{File, [{Loc, Mod, Reason}]}] -> [Reason]
reasons(Groups) -> [R || {_F, Ds} <- Groups, {_L, _M, R} <- Ds].

locs(Groups) -> [L || {_F, Ds} <- Groups, {L, _M, _R} <- Ds].

files(Groups) -> [F || {F, _Ds} <- Groups].

line(Loc) when is_integer(Loc) -> Loc;
line({L, _C})                  -> L.

%% Does any ai_mustache:inline/2 call survive in these forms?
has_inline(Term) when is_tuple(Term) ->
    case Term of
        {call, _, {remote, _, {atom, _, ai_mustache}, {atom, _, inline}}, [_, _]} ->
            true;
        _ ->
            has_inline(tuple_to_list(Term))
    end;
has_inline([H | T]) -> has_inline(H) orelse has_inline(T);
has_inline(_)       -> false.

%%%===================================================================
%%% A second parse_transform, for the coexistence test
%%%===================================================================

parse_transform(Forms, _Opts) ->
    A = erl_anno:new(1),
    Fn = {function, A, other_transform_ran, 0, [{clause, A, [], [], [{atom, A, yes}]}]},
    Ex = {attribute, A, export, [{other_transform_ran, 0}]},
    {Head, Tail} = lists:splitwith(fun(F) -> element(1, F) =/= function end, Forms),
    {Body, Eof} = lists:splitwith(fun(F) -> element(1, F) =/= eof end, Tail),
    Head ++ [Ex] ++ Body ++ [Fn] ++ Eof.

%%%===================================================================
%%% Entry point
%%%===================================================================

transform_test_() ->
    {setup, fun() -> load_exts() end, fun(_) -> ok end,
     fun(_) ->
             [framework_cases(),
              position_cases(),
              literal_cases(),
              inline_semantics_cases(),
              inline_error_cases(),
              diff_test_cases(),
              tag_cases(),
              template_cases(),
              diagnostic_cases()]
     end}.

%%%===================================================================
%%% Pass framework (T22)
%%%===================================================================

plain_forms() ->
    A = erl_anno:new(1),
    [{attribute, A, file, {"plain.erl", 1}},
     {attribute, A, module, tf_plain},
     {attribute, A, export, [{f, 1}]},
     {function, A, f, 1, [{clause, A, [{var, A, 'X'}], [], [{var, A, 'X'}]}]},
     {eof, A}].

framework_cases() ->
    [{"a module with no mustache constructs comes back untouched",
      fun() ->
              Forms = plain_forms(),
              ?assertEqual(Forms, ai_mustache_transform:parse_transform(Forms, []))
      end},
     {"running the transform twice changes nothing the second time",
      fun() ->
              Src = "-module(tf_idem).\n"
                    "-compile({parse_transform, ai_mustache_transform}).\n"
                    "-mustache_tag(my_i18n).\n"
                    "-export([f/1]).\n"
                    "f(N) -> ai_mustache:inline(<<\"x{{n}}\">>, #{n => N}).\n",
              File = filename:join(tmp_dir(), "tf_idem.erl"),
              ok = file:write_file(File, Src),
              {ok, Forms} = epp:parse_file(File, [], []),
              Once  = ai_mustache_transform:parse_transform(Forms, []),
              Twice = ai_mustache_transform:parse_transform(Once, []),
              ?assertEqual(Once, Twice)
      end},
     {"the module name is found wherever another transform left it",
      fun() ->
              A = erl_anno:new(1),
              Forms0 = plain_forms(),
              %% -module is no longer the first form.
              Forms = [{attribute, A, file, {"plain.erl", 1}},
                       {attribute, A, compile, []} | tl(Forms0)],
              ?assertEqual(Forms,
                           ai_mustache_transform:parse_transform(Forms, []))
      end},
     {"another parse_transform running first is fine",
      fun() ->
              Src = "-module(tf_coexist_a).\n"
                    "-compile({parse_transform, ai_mustache_transform_tests}).\n"
                    "-compile({parse_transform, ai_mustache_transform}).\n"
                    "-export([f/0]).\n"
                    "f() -> ai_mustache:inline(<<\"a{{n}}\">>, #{n => 1}).\n",
              M = write_and_load(tf_coexist_a, Src),
              ?assertEqual(<<"a1">>, M:f()),
              ?assertEqual(yes, M:other_transform_ran())
      end},
     {"another parse_transform running second is fine too",
      fun() ->
              Src = "-module(tf_coexist_b).\n"
                    "-compile({parse_transform, ai_mustache_transform}).\n"
                    "-compile({parse_transform, ai_mustache_transform_tests}).\n"
                    "-export([f/0]).\n"
                    "f() -> ai_mustache:inline(<<\"b{{n}}\">>, #{n => 2}).\n",
              M = write_and_load(tf_coexist_b, Src),
              ?assertEqual(<<"b2">>, M:f()),
              ?assertEqual(yes, M:other_transform_ran())
      end},
     {"only warnings means {warning, Forms, Ws} and the build continues",
      fun() ->
              Src = "-module(tf_warn_only).\n"
                    "-compile({parse_transform, ai_mustache_transform}).\n"
                    "-export([f/2]).\n"
                    "f(T, C) -> ai_mustache:inline(T, C).\n",
              {ok, tf_warn_only, _Bin, Ws} = write_and_compile(tf_warn_only, Src),
              ?assertEqual([inline_not_literal], reasons(Ws))
      end}].

%%%===================================================================
%%% Expression positions (T22 section 4)
%%%===================================================================

position_cases() ->
    Results =
        [{t_case, [true], <<"c1">>},
         {t_if, [true], <<"i2">>},
         {t_try, [], <<"t3">>},
         {t_catch_clause, [], <<"k4">>},
         {t_after, [], <<"a5">>},
         {t_receive, [], <<"r6">>},
         {t_lc, [[1, 2]], [<<"l1">>, <<"l2">>]},
         {t_lc_filter, [[1, 2, 3]], [1, 3]},
         {t_lc_source, [], "77"},
         {t_bc, [[1, 2]], <<"b1b2">>},
         {t_fun, [], <<"f7">>},
         {t_named_fun, [], <<"n8">>},
         {t_block, [], <<"g9">>},
         {t_bin, [], <<"x10!">>},
         {t_map, [], #{k => <<"m11">>}},
         {t_map_update, [#{k => 0}], #{k => <<"u12">>}},
         {t_record, [], <<"d13">>},
         {t_arg, [], <<"p15">>},
         {t_tuple, [], {<<"q16">>}},
         {t_cons, [], [<<"s17">>]},
         {t_op, [], true},
         {t_catch, [], <<"h19">>},
         {t_match, [], <<"w20">>},
         {t_nested, [], <<"[21]">>}],
    [{"every inline call in tf_positions was expanded at compile time",
      fun() ->
              Forms = fixture_forms(tf_positions),
              ?assert(has_inline(Forms)),
              Out = ai_mustache_transform:parse_transform(Forms, []),
              ?assertNot(has_inline(Out))
      end}]
        ++ [{"inline in " ++ atom_to_list(F) ++ " renders correctly",
             fun() ->
                     M = load_fixture(tf_positions),
                     ?assertEqual(Exp, apply(M, F, Args))
             end}
            || {F, Args, Exp} <- Results]
        ++ [{"a record update field is an expression position too",
             fun() ->
                     M = load_fixture(tf_positions),
                     ?assertEqual(<<"e14">>,
                                  M:t_record_update({r, undefined, undefined}))
             end}].

%%%===================================================================
%%% What counts as a literal (T24 section 1)
%%%===================================================================

literal_cases() ->
    [{"~\"...\" and <<\"...\">> are both literals",
      fun() ->
              Src = "-module(tf_lit).\n"
                    "-compile({parse_transform, ai_mustache_transform}).\n"
                    "-export([a/0, b/0, c/0]).\n"
                    "a() -> ai_mustache:inline(~\"s{{v}}\", #{v => 1}).\n"
                    "b() -> ai_mustache:inline(<<\"d{{v}}\">>, #{v => 2}).\n"
                    "c() -> ai_mustache:inline(<<\"e\", \"{{v}}\">>, #{v => 3}).\n",
              {ok, tf_lit, Bin, Ws} = write_and_compile(tf_lit, Src),
              ?assertEqual([], reasons(Ws)),
              _ = code:purge(tf_lit),
              {module, tf_lit} = code:load_binary(tf_lit, "tf_lit", Bin),
              ?assertEqual({<<"s1">>, <<"d2">>, <<"e3">>},
                           {tf_lit:a(), tf_lit:b(), tf_lit:c()})
      end},
     {"a macro that expands to a literal binary is expanded too",
      fun() ->
              %% epp has already run when the transform sees the forms, so this
              %% costs nothing extra -- but it is worth pinning down.
              ?assertEqual(<<"macro 5">>,
                           ai_mustache:inline(?TPL, #{v => 5})),
              Src = "-module(tf_macro).\n"
                    "-compile({parse_transform, ai_mustache_transform}).\n"
                    "-define(T, <<\"macro {{v}}\">>).\n"
                    "-export([f/0]).\n"
                    "f() -> ai_mustache:inline(?T, #{v => 5}).\n",
              File = filename:join(tmp_dir(), "tf_macro.erl"),
              ok = file:write_file(File, Src),
              {ok, Forms} = epp:parse_file(File, [], []),
              ?assertNot(has_inline(
                           ai_mustache_transform:parse_transform(Forms, []))),
              M = write_and_load(tf_macro, Src),
              ?assertEqual(<<"macro 5">>, M:f())
      end},
     {"non-literal first arguments are left alone and warn once each",
      fun() ->
              Src = "-module(tf_nonlit).\n"
                    "-compile({parse_transform, ai_mustache_transform}).\n"
                    "-export([a/2, b/2, c/2, d/2]).\n"
                    "a(T, C) -> ai_mustache:inline(T, C).\n"
                    "b(T, C) -> ai_mustache:inline(<<T/binary>>, C).\n"
                    "c(T, C) -> ai_mustache:inline(list_to_binary(T), C).\n"
                    "d(T, C) -> ai_mustache:inline(<<T:8>>, C).\n",
              {ok, tf_nonlit, Bin, Ws} = write_and_compile(tf_nonlit, Src),
              ?assertEqual([inline_not_literal, inline_not_literal,
                            inline_not_literal, inline_not_literal],
                           reasons(Ws)),
              ?assertEqual([4, 5, 6, 7], [line(L) || L <- locs(Ws)]),
              _ = code:purge(tf_nonlit),
              {module, tf_nonlit} = code:load_binary(tf_nonlit, "tf_nonlit", Bin),
              %% Still works: it went to the runtime path.
              ?assertEqual(<<"z1">>, tf_nonlit:a(<<"z{{v}}">>, #{v => 1}))
      end},
     {"nowarn_mustache_inline silences the warning",
      fun() ->
              Src = "-module(tf_nowarn).\n"
                    "-compile({parse_transform, ai_mustache_transform}).\n"
                    "-export([a/2]).\n"
                    "a(T, C) -> ai_mustache:inline(T, C).\n",
              {ok, tf_nowarn, _Bin, Ws} =
                  write_and_compile(tf_nowarn, Src, [nowarn_mustache_inline]),
              ?assertEqual([], reasons(Ws))
      end},
     {"other arities and a local inline/2 are not touched",
      fun() ->
              Src = "-module(tf_notmine).\n"
                    "-compile({parse_transform, ai_mustache_transform}).\n"
                    "-export([a/0, b/0]).\n"
                    "a() -> inline(<<\"{{v}}\">>, #{v => 1}).\n"
                    "b() -> ai_mustache:render_string(<<\"{{v}}\">>, #{v => 2}).\n"
                    "inline(_T, _C) -> local.\n",
              {ok, tf_notmine, Bin, Ws} = write_and_compile(tf_notmine, Src),
              ?assertEqual([], reasons(Ws)),
              _ = code:purge(tf_notmine),
              {module, tf_notmine} = code:load_binary(tf_notmine, "x", Bin),
              ?assertEqual(local, tf_notmine:a()),
              ?assertEqual(<<"2">>, tf_notmine:b())
      end}].

%%%===================================================================
%%% Inline semantics (T24)
%%%===================================================================

inline_semantics_cases() ->
    [{"the context expression is evaluated exactly once",
      fun() ->
              Src = "-module(tf_once).\n"
                    "-compile({parse_transform, ai_mustache_transform}).\n"
                    "-export([f/0]).\n"
                    "f() ->\n"
                    "    put(tf_once_n, 0),\n"
                    "    R = ai_mustache:inline(<<\"{{a}}{{a}}{{a}}\">>,\n"
                    "                           begin\n"
                    "                               put(tf_once_n,"
                    " get(tf_once_n) + 1),\n"
                    "                               #{a => 1}\n"
                    "                           end),\n"
                    "    {R, get(tf_once_n)}.\n",
              M = write_and_load(tf_once, Src),
              ?assertEqual({<<"111">>, 1}, M:f())
      end},
     {"two inline calls in one function do not shadow each other",
      fun() ->
              Src = "-module(tf_two).\n"
                    "-compile({parse_transform, ai_mustache_transform}).\n"
                    "-export([f/0]).\n"
                    "f() ->\n"
                    "    S = outer,\n"
                    "    I = also_outer,\n"
                    "    A = ai_mustache:inline(<<\"a{{v}}\">>, #{v => 1}),\n"
                    "    B = ai_mustache:inline(<<\"b{{v}}\">>, #{v => 2}),\n"
                    "    {S, I, A, B}.\n",
              M = write_and_load(tf_two, Src),
              ?assertEqual({outer, also_outer, <<"a1">>, <<"b2">>}, M:f())
      end},
     {"a nested section still works when a user variable is called S",
      fun() ->
              Src = "-module(tf_shadow).\n"
                    "-compile({parse_transform, ai_mustache_transform}).\n"
                    "-export([f/1]).\n"
                    "f(S) ->\n"
                    "    ai_mustache:inline(<<\"[{{#xs}}({{n}}){{/xs}}]\">>,"
                    " #{xs => S}).\n",
              M = write_and_load(tf_shadow, Src),
              ?assertEqual(<<"[(1)(2)]">>, M:f([#{n => 1}, #{n => 2}]))
      end},
     {"the expansion keeps the line number of the original call",
      fun() ->
              %% The generated nodes carry the line of the call, so a crash
              %% inside the expansion still points at the user's own source.
              Src = "-module(tf_line).\n"
                    "-compile({parse_transform, ai_mustache_transform}).\n"
                    "-export([f/0]).\n"
                    "f() ->\n"
                    "    ai_mustache:inline(<<\"{{x}}\">>,\n"
                    "                       #{x => {not_renderable, thing}}).\n",
              M = write_and_load(tf_line, Src),
              try M:f() of
                  _ -> ?assert(false)
              catch
                  _:_:Stk ->
                      [{tf_line, f, 0, Info}] =
                          [Fr || {tf_line, f, 0, _} = Fr <- Stk],
                      ?assertEqual(5, proplists:get_value(line, Info))
              end
      end},
     {"an extension tag works in an inline template",
      fun() ->
              M = load_fixture(tf_wrap),
              ?assertEqual(<<"[<b>z</b>]">>, M:wrap(<<"z">>))
      end},
     {"the same extension tag has no runtime fallback",
      fun() ->
              %% compile_tag/4 returns an abstract expression, so the
              %% interpreted path cannot honour it. Failing loudly is the
              %% contract; rendering nothing would not be.
              ?assertError({ai_mustache, {error, {_, _, {unknown_marker, $%}}}},
                           ai_mustache:inline(<<"[{{%b}}x{{/b}}]">>, #{}))
      end}].

%%%===================================================================
%%% Inline errors (T24 section 3)
%%%===================================================================

inline_error_cases() ->
    [{"a partial in an inline template is a compile error on the right line",
      fun() ->
              Src = "-module(tf_partial).\n"
                    "-compile({parse_transform, ai_mustache_transform}).\n"
                    "-export([f/0]).\n"
                    "f() ->\n"
                    "    ai_mustache:inline(<<\"a\\n\"\n"
                    "                         \"b{{> item}}\\n\">>, #{}).\n",
              {error, Es, _Ws} = write_and_compile(tf_partial, Src),
              ?assertEqual([{inline_partial_unsupported, <<"item">>}],
                           reasons(Es)),
              %% Template line 2 inside a call that starts on .erl line 5.
              ?assertEqual([6], [line(L) || L <- locs(Es)])
      end},
     {"the interpreted path rejects an inline partial too",
      fun() ->
              %% Both paths must fail on a partial. The fallback cannot simply
              %% defer to render_string/3, which stubs an unknown partial to
              %% the empty string because the mustache spec requires that of
              %% file templates -- that would make a partial a build error with
              %% the transform and a silent empty string without it, which is
              %% the worst place for the two to disagree.
              ?assertError({ai_mustache,
                            {error, {_File, _Line, partial_in_inline_template}}},
                           ai_mustache:inline(<<"a\nb{{> item}}\n">>, #{}))
      end},
     {"a malformed inline template reports the template's line",
      fun() ->
              Src = "-module(tf_badtpl).\n"
                    "-compile({parse_transform, ai_mustache_transform}).\n"
                    "-export([f/0]).\n"
                    "f() ->\n"
                    "    ai_mustache:inline(<<\"one\\n\"\n"
                    "                         \"{{#x}}two\\n\">>, #{}).\n",
              {error, Es, _Ws} = write_and_compile(tf_badtpl, Src),
              ?assertMatch([{unclosed_tag, [x]}], reasons(Es)),
              ?assertEqual([6], [line(L) || L <- locs(Es)])
      end}].

%%%===================================================================
%%% Compile-time expansion vs runtime interpretation (T24 section 6)
%%%===================================================================

%% {Title, Template, Context, Expected}
diff_cases() ->
    [{"plain text", <<"hello">>, #{}, <<"hello">>},
     {"interpolation", <<"{{a}}">>, #{a => <<"x">>}, <<"x">>},
     {"interpolation escapes", <<"{{a}}">>, #{a => <<"<&>">>},
      <<"&lt;&amp;&gt;">>},
     {"triple mustache does not escape", <<"{{{a}}}">>, #{a => <<"<b>">>},
      <<"<b>">>},
     {"ampersand does not escape", <<"{{&a}}">>, #{a => <<"<b>">>}, <<"<b>">>},
     {"missing key is empty", <<"[{{a}}]">>, #{}, <<"[]">>},
     {"integer", <<"{{a}}">>, #{a => 42}, <<"42">>},
     {"float keeps its short form", <<"{{a}}">>, #{a => 1.25}, <<"1.25">>},
     {"comment vanishes", <<"a{{! note }}b">>, #{}, <<"ab">>},
     {"dotted path", <<"{{a.b}}">>, #{a => #{b => <<"v">>}}, <<"v">>},
     {"dotted path first segment backtracks",
      <<"{{#xs}}{{a.b}}{{/xs}}">>, #{xs => [#{}], a => #{b => <<"v">>}}, <<"v">>},
     {"implicit iterator", <<"{{#xs}}[{{.}}]{{/xs}}">>,
      #{xs => [1, 2]}, <<"[1][2]">>},
     {"section over a list", <<"[{{#xs}}({{n}}){{/xs}}]">>,
      #{xs => [#{n => 1}, #{n => 2}]}, <<"[(1)(2)]">>},
     {"section over an empty list", <<"[{{#xs}}x{{/xs}}]">>, #{xs => []}, <<"[]">>},
     {"section over a map pushes scope", <<"[{{#u}}{{name}}{{/u}}]">>,
      #{u => #{name => <<"D">>}}, <<"[D]">>},
     {"section on true renders once", <<"[{{#x}}{{y}}{{/x}}]">>,
      #{x => true, y => <<"v">>}, <<"[v]">>},
     {"section on a scalar exposes it as dot", <<"[{{#x}}{{.}}{{/x}}]">>,
      #{x => <<"v">>}, <<"[v]">>},
     {"section on a falsy value skips", <<"[{{#x}}b{{/x}}]">>,
      #{x => null}, <<"[]">>},
     {"section fun/1 result is dispatched", <<"[{{#x}}({{n}}){{/x}}]">>,
      #{x => fun(_F) -> [#{n => 1}, #{n => 2}] end}, <<"[(1)(2)]">>},
     {"section fun/2 receives the rendered body", <<"[{{#w}}inner{{/w}}]">>,
      #{w => fun(Body, _F) -> <<"<", Body/binary, ">">> end}, <<"[<inner>]">>},
     {"inverted section on falsy", <<"[{{^x}}b{{/x}}]">>, #{x => []}, <<"[b]">>},
     {"inverted section on truthy", <<"[{{^x}}b{{/x}}]">>, #{x => 0}, <<"[]">>},
     {"nested sections", <<"{{#a}}{{#b}}{{c}}{{/b}}{{/a}}">>,
      #{a => #{b => #{c => <<"deep">>}}}, <<"deep">>},
     {"has section is a conditional, not a scope",
      <<"{{+user}}{{user.name}}{{/user}}">>,
      #{user => #{name => <<"D">>}}, <<"D">>},
     {"has section does not iterate", <<"[{{+xs}}once{{/xs}}]">>,
      #{xs => [1, 2, 3]}, <<"[once]">>},
     {"inverted has section", <<"[{{-x}}no{{/x}}]">>, #{x => false}, <<"[no]">>},
     {"lambda tag", <<"[{{*f}}]">>,
      #{f => fun(F) -> maps:get(who, F) end, who => <<"w">>}, <<"[w]">>},
     {"lambda output is not escaped", <<"[{{*f}}]">>,
      #{f => fun(_F) -> <<"<b>">> end}, <<"[<b>]">>},
     {"falsy: undefined", <<"[{{^x}}b{{/x}}]">>, #{x => undefined}, <<"[b]">>},
     {"falsy: false", <<"[{{^x}}b{{/x}}]">>, #{x => false}, <<"[b]">>},
     {"falsy: empty list", <<"[{{^x}}b{{/x}}]">>, #{x => []}, <<"[b]">>},
     {"falsy: empty binary", <<"[{{^x}}b{{/x}}]">>, #{x => <<>>}, <<"[b]">>},
     {"falsy: null", <<"[{{^x}}b{{/x}}]">>, #{x => null}, <<"[b]">>},
     {"truthy: zero", <<"[{{#x}}b{{/x}}]">>, #{x => 0}, <<"[b]">>},
     {"truthy: empty map", <<"[{{#x}}b{{/x}}]">>, #{x => #{}}, <<"[b]">>},
     {"custom delimiters", <<"{{=<% %>=}}<%a%>">>, #{a => <<"v">>}, <<"v">>},
     {"standalone section tags eat their line",
      <<"|\n{{#x}}\ny\n{{/x}}\n|">>, #{x => true}, <<"|\ny\n|">>},
     {"standalone comment eats its line", <<"|\n{{! c }}\n|">>, #{}, <<"|\n|">>},
     {"multi-line template", <<"a\n{{x}}\nb">>, #{x => <<"m">>}, <<"a\nm\nb">>},
     {"a shadowing dotted lookup", <<"{{#a}}{{b}}{{/a}}">>,
      #{a => #{b => <<"inner">>}, b => <<"outer">>}, <<"inner">>},
     {"backtracking to an outer frame", <<"{{#a}}{{c}}{{/a}}">>,
      #{a => #{b => 1}, c => <<"outer">>}, <<"outer">>}].

%% Build a module whose Nth function is `ai_mustache:inline(<<Tpl>>, Ctx)' and
%% let the transform expand every one of them. Same table, same context, two
%% completely different evaluators.
diff_module_forms(Mod, Cases) ->
    A = erl_anno:new(1),
    Names = [fname(I) || I <- lists:seq(1, length(Cases))],
    [{attribute, A, file, {atom_to_list(Mod) ++ ".erl", 1}},
     {attribute, A, module, Mod},
     {attribute, A, export, [{N, 1} || N <- Names]}]
        ++ [{function, A, N, 1,
             [{clause, A, [{var, A, 'Ctx'}], [],
               [{call, A, {remote, A, {atom, A, ai_mustache}, {atom, A, inline}},
                 [bin_lit(A, Tpl), {var, A, 'Ctx'}]}]}]}
            || {N, {_T, Tpl, _C, _E}} <- lists:zip(Names, Cases)]
        ++ [{eof, A}].

fname(I) -> list_to_atom("case_" ++ integer_to_list(I)).

bin_lit(A, <<>>) -> {bin, A, []};
bin_lit(A, Bin)  -> {bin, A, [{bin_element, A, {string, A, binary_to_list(Bin)},
                               default, default}]}.

diff_test_cases() ->
    Cases = diff_cases(),
    Mod = tf_diff,
    Forms = diff_module_forms(Mod, Cases),
    Transformed = ai_mustache_transform:parse_transform(Forms, []),
    {ok, Mod, Bin} = compile:forms(Transformed, [binary, return_errors]),
    _ = code:purge(Mod),
    {module, Mod} = code:load_binary(Mod, "tf_diff", Bin),
    [{"the expanded module contains no surviving inline call",
      fun() -> ?assertNot(has_inline(Transformed)) end},
     {"case titles are unique",
      fun() ->
              Titles = [T || {T, _, _, _} <- Cases],
              ?assertEqual([], Titles -- lists:usort(Titles))
      end}]
        ++ [{"expanded: " ++ Title,
             fun() -> ?assertEqual(Exp, apply(Mod, fname(I), [Ctx])) end}
            || {I, {Title, _Tpl, Ctx, Exp}} <-
                   lists:zip(lists:seq(1, length(Cases)), Cases)]
        ++ [{"interpreted: " ++ Title,
             fun() -> ?assertEqual(Exp, ai_mustache:inline(Tpl, Ctx)) end}
            || {Title, Tpl, Ctx, Exp} <- Cases]
        ++ [{"byte-identical: " ++ Title,
             fun() ->
                     ?assertEqual(ai_mustache:inline(Tpl, Ctx),
                                  apply(Mod, fname(I), [Ctx]))
             end}
            || {I, {Title, Tpl, Ctx, _Exp}} <-
                   lists:zip(lists:seq(1, length(Cases)), Cases)].

%%%===================================================================
%%% Form (a): -mustache_tag (T23)
%%%===================================================================

tag_src(Name, Attrs) ->
    "-module(" ++ atom_to_list(Name) ++ ").\n"
    "-compile({parse_transform, ai_mustache_transform}).\n"
    ++ Attrs ++
    "-export([f/0]).\n"
    "f() -> ok.\n".

tag_cases() ->
    [{"all three attribute shapes are accepted",
      fun() ->
              Src = tag_src(tf_tag_shapes,
                            "-mustache_tag(my_i18n).\n"
                            "-mustache_tag({$%, my_wrap}).\n"),
              {ok, tf_tag_shapes, _B, Ws} = write_and_compile(tf_tag_shapes, Src),
              ?assertEqual([], reasons(Ws))
      end},
     {"the {Module, Fun} shape warns that the function name is ignored",
      fun() ->
              Src = tag_src(tf_tag_fun, "-mustache_tag({$@, {my_i18n, compile_at}}).\n"),
              {ok, tf_tag_fun, _B, Ws} = write_and_compile(tf_tag_fun, Src),
              ?assertEqual([{ext_callback_name_ignored, my_i18n, compile_at}],
                           reasons(Ws))
      end},
     {"any other shape is rejected with the correct spellings",
      fun() ->
              Src = tag_src(tf_tag_bad, "-mustache_tag([$@, my_i18n]).\n"),
              {error, Es, _Ws} = write_and_compile(tf_tag_bad, Src),
              ?assertEqual([{bad_mustache_tag, [$@, my_i18n]}], reasons(Es)),
              Text = lists:flatten(
                       ai_mustache_transform:format_error(
                         {bad_mustache_tag, [$@, my_i18n]})),
              ?assert(string:find(Text, "-mustache_tag(my_ext).") =/= nomatch)
      end},
     {"a module that is not an extension is caught at compile time",
      fun() ->
              Src = tag_src(tf_tag_nobeh, "-mustache_tag(bad_ext_no_behaviour).\n"),
              {error, Es, _Ws} = write_and_compile(tf_tag_nobeh, Src),
              ?assertEqual([{not_an_ext_module, bad_ext_no_behaviour}],
                           reasons(Es))
      end},
     {"a missing callback is caught at compile time",
      fun() ->
              Src = tag_src(tf_tag_nocb, "-mustache_tag(bad_ext_missing_cb).\n"),
              {error, Es, _Ws} = write_and_compile(tf_tag_nocb, Src),
              ?assertEqual([{ext_missing_callback, bad_ext_missing_cb,
                             {compile_tag, 4}}], reasons(Es))
      end},
     {"an unknown extension module names the compile-order trap",
      fun() ->
              Src = tag_src(tf_tag_nomod, "-mustache_tag(no_such_ext_module_x).\n"),
              {error, Es, _Ws} = write_and_compile(tf_tag_nomod, Src),
              ?assertEqual([{ext_module_not_found, no_such_ext_module_x}],
                           reasons(Es))
      end},
     {"a reserved marker is refused before the module is even consulted",
      fun() ->
              Src = tag_src(tf_tag_reserved, "-mustache_tag({$#, my_i18n}).\n"),
              {error, Es, _Ws} = write_and_compile(tf_tag_reserved, Src),
              ?assertEqual([{marker_reserved, $#, my_i18n}], reasons(Es))
      end},
     {"an unusable marker character is refused",
      fun() ->
              Src = tag_src(tf_tag_invalid, "-mustache_tag({${, my_i18n}).\n"),
              {error, Es, _Ws} = write_and_compile(tf_tag_invalid, Src),
              ?assertEqual([{invalid_marker, ${, my_i18n}], reasons(Es))
      end},
     {"a marker the module does not claim is refused, listing what it claims",
      fun() ->
              Src = tag_src(tf_tag_undecl, "-mustache_tag({$~, my_i18n}).\n"),
              {error, Es, _Ws} = write_and_compile(tf_tag_undecl, Src),
              ?assertEqual([{marker_not_declared, $~, my_i18n, [$@]}],
                           reasons(Es))
      end},
     {"two extensions claiming one marker lists both",
      fun() ->
              Src = tag_src(tf_tag_conflict,
                            "-mustache_tag(bad_ext_conflict_a).\n"
                            "-mustache_tag(bad_ext_conflict_b).\n"),
              {error, Es, _Ws} = write_and_compile(tf_tag_conflict, Src),
              ?assertEqual([{marker_conflict, $~, [bad_ext_conflict_a,
                                                   bad_ext_conflict_b]}],
                           reasons(Es))
      end},
     {"an extension with no beam is validated from its source",
      fun() ->
              ?assertEqual(false, code:is_loaded(src_only_ext)),
              {ok, tf_src_only, _B, Ws} =
                  compile_fixture(tf_src_only, [{i, tmp_dir()}]),
              ?assertEqual([], reasons(Ws))
      end},
     {"the attribute never reaches the beam",
      fun() ->
              M = load_fixture(tf_all),
              Attrs = M:module_info(attributes),
              ?assertEqual([], proplists:get_all_values(mustache_tag, Attrs)),
              %% -mustache_template is kept on purpose; see inject_all/2.
              ?assertNotEqual([],
                              proplists:get_all_values(mustache_template, Attrs))
      end},
     {"a declared extension that the build does not assemble warns",
      fun() ->
              Src = tag_src(tf_tag_unassembled, "-mustache_tag(my_i18n).\n"),
              {ok, tf_tag_unassembled, _B, Ws} =
                  write_and_compile(tf_tag_unassembled, Src,
                                    [{mustache_opts, [{extensions, [my_wrap]}]}]),
              ?assertEqual([{ext_not_assembled, my_i18n}], reasons(Ws))
      end},
     {"no such warning when the extension is assembled",
      fun() ->
              Src = tag_src(tf_tag_assembled, "-mustache_tag(my_i18n).\n"),
              {ok, tf_tag_assembled, _B, Ws} =
                  write_and_compile(tf_tag_assembled, Src,
                                    [{mustache_opts, [{extensions, [my_i18n]}]}]),
              ?assertEqual([], reasons(Ws))
      end},
     {"a declaration reaches this module's own inline templates",
      fun() ->
              M = load_fixture(tf_wrap),
              ?assertEqual(<<"[<b>q</b>]">>, M:wrap(<<"q">>))
      end},
     {"but not a template compiled anywhere else",
      fun() ->
              %% Same template text, no extensions configured: the marker is
              %% unknown. This is the scope limit -mustache_tag cannot cross.
              ?assertMatch({error, {_, _, {unknown_marker, $%}}},
                           ai_mustache_parser:parse(<<"{{%b}}x{{/b}}">>,
                                                    #{source => <<"t">>}))
      end}].

%%%===================================================================
%%% Form (c): -mustache_template (T25)
%%%===================================================================

template_cases() ->
    [{"the generated functions render and are exported",
      fun() ->
              M = load_fixture(tf_all),
              ?assertEqual(<<"Hello D!">>, M:greet(#{name => <<"D">>})),
              ?assertEqual(<<"<h1>T</h1>\n<li>a</li>\n">>,
                           M:index(#{title => <<"T">>,
                                     items => [#{name => <<"a">>}]})),
              Exports = M:module_info(exports),
              [?assert(lists:member({F, 1}, Exports))
               || F <- [index, index_iolist, greet, greet_iolist, i18n,
                        i18n_iolist]]
      end},
     {"the iolist variant skips the final copy",
      fun() ->
              M = load_fixture(tf_all),
              ?assert(is_list(M:greet_iolist(#{name => <<"D">>}))),
              ?assertEqual(<<"Hello D!">>,
                           iolist_to_binary(M:greet_iolist(#{name => <<"D">>})))
      end},
     {"a bare path derives the function name from the basename",
      fun() ->
              M = load_fixture(tf_all),
              ?assert(erlang:function_exported(M, greet, 1))
      end},
     {"several templates in one module do not collide",
      fun() ->
              M = load_fixture(tf_all),
              ?assertEqual(<<"Hello D!">>, M:greet(#{name => <<"D">>})),
              ?assertEqual(<<"Hello, D!">>,
                           M:i18n(#{name => <<"D">>, locale => <<"en">>}))
      end},
     {"an extension declared in the module is used by its file templates",
      fun() ->
              M = load_fixture(tf_all),
              ?assertEqual(<<"Bonjour, D!">>,
                           M:i18n(#{name => <<"D">>, locale => <<"fr">>}))
      end},
     {"an inline template and a file template agree",
      fun() ->
              M = load_fixture(tf_all),
              ?assertEqual(M:i18n(#{name => <<"D">>, locale => <<"fr">>}),
                           M:hi(<<"D">>, <<"fr">>))
      end},
     {"a bad attribute shape is reported with the right spellings",
      fun() ->
              Src = "-module(tf_tpl_bad).\n"
                    "-compile({parse_transform, ai_mustache_transform}).\n"
                    "-mustache_template(42).\n",
              {error, Es, _Ws} = write_and_compile(tf_tpl_bad, Src),
              ?assertEqual([{bad_mustache_template, 42}], reasons(Es))
      end},
     {"a missing template lists every directory that was searched",
      fun() ->
              Src = "-module(tf_tpl_missing).\n"
                    "-compile({parse_transform, ai_mustache_transform}).\n"
                    "-mustache_template({gone, \"no/such.mustache\"}).\n",
              {error, Es, _Ws} = write_and_compile(tf_tpl_missing, Src),
              [{template_not_found, "no/such.mustache", Tried}] = reasons(Es),
              ?assert(lists:member(tmp_dir(), Tried)),
              ?assert(lists:member(".", Tried)),
              Text = lists:flatten(
                       ai_mustache_transform:format_error(
                         {template_not_found, "no/such.mustache", Tried})),
              ?assert(string:find(Text, tmp_dir()) =/= nomatch)
      end},
     {"the views option is one of the directories searched",
      fun() ->
              Src = "-module(tf_tpl_views).\n"
                    "-compile({parse_transform, ai_mustache_transform}).\n"
                    "-mustache_template({greet, \"greet.mustache\"}).\n",
              Views = filename:join(fixture_dir(), "views"),
              M = write_and_load(tf_tpl_views, Src,
                                 [{mustache_opts, [{views, Views}]}]),
              ?assertEqual(<<"Hello D!">>, M:greet(#{name => <<"D">>}))
      end},
     {"an include path is one of the directories searched",
      fun() ->
              Src = "-module(tf_tpl_ipath).\n"
                    "-compile({parse_transform, ai_mustache_transform}).\n"
                    "-mustache_template({greet, \"greet.mustache\"}).\n",
              Views = filename:join(fixture_dir(), "views"),
              M = write_and_load(tf_tpl_ipath, Src, [{i, Views}]),
              ?assertEqual(<<"Hello D!">>, M:greet(#{name => <<"D">>}))
      end},
     {"two templates generating the same function name is an error",
      fun() ->
              Src = "-module(tf_tpl_dup).\n"
                    "-compile({parse_transform, ai_mustache_transform}).\n"
                    "-mustache_template({greet, \"greet.mustache\"}).\n"
                    "-mustache_template({greet, \"index.mustache\"}).\n",
              Views = filename:join(fixture_dir(), "views"),
              {error, Es, _Ws} =
                  write_and_compile(tf_tpl_dup, Src, [{i, Views}]),
              ?assertEqual([{duplicate_template_name, greet}], reasons(Es))
      end},
     {"clashing with a function the user wrote is an error, not an overwrite",
      fun() ->
              Src = "-module(tf_tpl_clash).\n"
                    "-compile({parse_transform, ai_mustache_transform}).\n"
                    "-mustache_template({greet, \"greet.mustache\"}).\n"
                    "-export([greet/1]).\n"
                    "greet(_) -> mine.\n",
              Views = filename:join(fixture_dir(), "views"),
              {error, Es, _Ws} =
                  write_and_compile(tf_tpl_clash, Src, [{i, Views}]),
              ?assertEqual([{template_name_clash, {greet, 1}}], reasons(Es))
      end},
     {"an error inside the template is reported against the .mustache",
      fun() ->
              Src = "-module(tf_tpl_broken).\n"
                    "-compile({parse_transform, ai_mustache_transform}).\n"
                    "-mustache_template({broken, \"broken.mustache\"}).\n",
              Views = filename:join(fixture_dir(), "views"),
              {error, Es, _Ws} =
                  write_and_compile(tf_tpl_broken, Src, [{i, Views}]),
              ?assertEqual([{unclosed_tag, [x]}], reasons(Es)),
              ?assertEqual([filename:join(Views, "broken.mustache")], files(Es)),
              ?assertEqual([3], [line(L) || L <- locs(Es)])
      end},
     {"a template with partials warns that the plugin is needed",
      fun() ->
              Src = "-module(tf_tpl_partial).\n"
                    "-compile({parse_transform, ai_mustache_transform}).\n"
                    "-mustache_template({wp, \"withpartial.mustache\"}).\n",
              Views = filename:join(fixture_dir(), "views"),
              {ok, tf_tpl_partial, _B, Ws} =
                  write_and_compile(tf_tpl_partial, Src, [{i, Views}]),
              ?assertMatch([{template_partial_needs_plugin, wp, [view_item]}],
                           reasons(Ws))
      end},
     {"without the parse_transform the functions simply do not exist",
      fun() ->
              %% The one hard failure of the three forms, and it shows up at
              %% the call site rather than at the attribute.
              Src = "-module(tf_tpl_notransform).\n"
                    "-mustache_template({greet, \"greet.mustache\"}).\n"
                    "-export([f/0]).\n"
                    "f() -> ok.\n",
              M = write_and_load(tf_tpl_notransform, Src),
              ?assertNot(erlang:function_exported(M, greet, 1)),
              ?assertError(undef, M:greet(#{}))
      end},
     {"ai_mustache_path:scan/1 sees exactly what the transform collected",
      fun() ->
              %% The plugin's staleness fallback reads the .erl statically. If
              %% its idea of {Name, Path} differed from the transform's it would
              %% touch the wrong file and the fallback would do nothing.
              File = fixture_path(tf_all),
              ?assertEqual({ok, [{index, "views/index.mustache"},
                                 {greet, "views/greet.mustache"},
                                 {i18n,  "views/i18n.mustache"}]},
                           ai_mustache_path:scan(File))
      end},
     {"resolve/2 reports every directory it tried",
      fun() ->
              ?assertEqual({error, {not_found, ["a", "b"]}},
                           ai_mustache_path:resolve("x.mustache", ["a", "b"])),
              Views = filename:join(fixture_dir(), "views"),
              ?assertEqual({ok, filename:join(Views, "greet.mustache")},
                           ai_mustache_path:resolve("greet.mustache",
                                                    ["nope", Views]))
      end},
     {"template_spec/1 derives a name from the basename",
      fun() ->
              ?assertEqual({ok, {index, "a/b/index.mustache"}},
                           ai_mustache_path:template_spec("a/b/index.mustache")),
              ?assertEqual({ok, {my_page, "x.mustache"}},
                           ai_mustache_path:template_spec({my_page, "x.mustache"})),
              ?assertEqual({ok, {a_b, "a-b.mustache"}},
                           ai_mustache_path:template_spec("a-b.mustache")),
              ?assertEqual(error, ai_mustache_path:template_spec(42))
      end}].

%%%===================================================================
%%% Diagnostics (T22 section 3)
%%%===================================================================

diagnostic_cases() ->
    [{"every problem in a module is reported by one compilation",
      fun() ->
              Src = "-module(tf_many).\n"
                    "-compile({parse_transform, ai_mustache_transform}).\n"
                    "-mustache_tag(bad_ext_no_behaviour).\n"
                    "-mustache_tag(42).\n"
                    "-mustache_template({gone, \"no/such.mustache\"}).\n"
                    "-export([f/0]).\n"
                    "f() -> ai_mustache:inline(<<\"{{> p}}\">>, #{}).\n",
              {error, Es, _Ws} = write_and_compile(tf_many, Src),
              Rs = reasons(Es),
              ?assertEqual(4, length(Rs)),
              ?assert(lists:member({not_an_ext_module, bad_ext_no_behaviour}, Rs)),
              ?assert(lists:member({bad_mustache_tag, 42}, Rs)),
              ?assert(lists:member({inline_partial_unsupported, <<"p">>}, Rs)),
              ?assertMatch([_], [R || {template_not_found, _, _} = R <- Rs])
      end},
     {"diagnostics are grouped by the file they came from",
      fun() ->
              Hrl = filename:join(tmp_dir(), "tf_inc.hrl"),
              ok = file:write_file(Hrl, "-mustache_tag(bad_ext_no_behaviour).\n"),
              Src = "-module(tf_grouped).\n"
                    "-compile({parse_transform, ai_mustache_transform}).\n"
                    "-include(\"tf_inc.hrl\").\n"
                    "-mustache_tag(42).\n",
              {error, Es, _Ws} = write_and_compile(tf_grouped, Src),
              Files = lists:sort([filename:basename(F) || F <- files(Es)]),
              ?assertEqual(["tf_grouped.erl", "tf_inc.hrl"], Files)
      end},
     {"format_error covers every reason the transform raises",
      fun() ->
              Reasons =
                  [{bad_mustache_tag, x},
                   {ext_not_assembled, m},
                   {ext_callback_name_ignored, m, f},
                   inline_not_literal,
                   {inline_partial_unsupported, <<"p">>},
                   partial_in_inline_template,
                   {bad_mustache_template, x},
                   {duplicate_template_name, n},
                   {template_name_clash, {n, 1}},
                   {template_not_found, "p", ["a"]},
                   {template_unreadable, enoent},
                   {template_partial_needs_plugin, n, [view_x]},
                   {unclosed_tag, [a, b]},
                   {mismatched_close, [a], [b]},
                   {partial_not_found, <<"p">>},
                   {unknown_marker, $@},
                   {not_an_ext_module, m}],
              [begin
                   Text = lists:flatten(
                            ai_mustache_transform:format_error(R)),
                   ?assert(length(Text) > 10),
                   ?assertEqual(nomatch, string:find(Text, "\n\n"))
               end || R <- Reasons]
      end}].
