%%%-------------------------------------------------------------------
%%% @doc Tests for ai_mustache_ext: the extension behaviour, the validation
%%% rules and the marker dispatch table.
%%%
%%% Every fixture under test/fixtures_transform is a real module that really
%%% compiles; the wrong ones are wrong as *extensions*, not as Erlang. That is
%%% the only way to exercise the checks the way a user meets them.
%%%
%%% See tasks/T21.md.
%%% @end
%%%-------------------------------------------------------------------
-module(ai_mustache_ext_reg_tests).

-include_lib("eunit/include/eunit.hrl").
-include("ai_mustache.hrl").

%%%===================================================================
%%% Fixture handling
%%%===================================================================

%% Fixture modules are checked in as `<name>.erl.src', not `.erl'. Anything
%% ending in .erl anywhere under test/ is picked up by rebar3's eunit provider
%% as a module of this project, and these are deliberately not compiled by the
%% build -- some of them are meant to fail validation. epp:parse_file/3 does not
%% care about the extension, and compile:forms/2 honours the -compile
%% attributes inside, so nothing is lost.
fixture_dir() ->
    filename:join(code:lib_dir(aihtml), "test/fixtures_transform").

fixture_path(Mod) ->
    filename:join(fixture_dir(), atom_to_list(Mod) ++ ".erl.src").

compile_fixture(Mod, Opts) ->
    {ok, Forms} = epp:parse_file(fixture_path(Mod), [fixture_dir()], []),
    compile:forms(Forms, [binary, return | Opts]).

%% src_only_ext is deliberately absent: one test needs a module with no beam.
loadable_fixtures() ->
    [my_i18n, my_wrap,
     bad_ext_no_behaviour, bad_ext_missing_cb, bad_ext_reserved,
     bad_ext_invalid_char, bad_ext_no_markers, bad_ext_bad_block,
     bad_ext_conflict_a, bad_ext_conflict_b].

load_fixtures() ->
    [load_fixture(M) || M <- loadable_fixtures()],
    ok.

load_fixture(Mod) ->
    {ok, Mod, Bin, _Ws} = compile_fixture(Mod, []),
    _ = code:purge(Mod),
    {module, Mod} = code:load_binary(Mod, fixture_path(Mod), Bin),
    ok.

setup_test_() ->
    {setup, fun() -> load_fixtures() end, fun(_) -> ok end, fun all/1}.

all(_) ->
    [builtin_markers_cases(),
     valid_marker_cases(),
     describe_cases(),
     registry_cases(),
     source_fallback_cases(),
     helper_cases(),
     format_error_cases()].

%%%===================================================================
%%% The reserved set
%%%===================================================================

builtin_markers_cases() ->
    [{"builtin markers come from the header, not a second copy",
      fun() ->
              ?assertEqual(?AI_MUSTACHE_BUILTIN_MARKERS,
                           ai_mustache_ext:builtin_markers())
      end},
     {"every builtin marker is reserved",
      fun() ->
              [?assert(ai_mustache_ext:is_reserved(C))
               || C <- ?AI_MUSTACHE_BUILTIN_MARKERS]
      end},
     {"the reserved set is exactly the documented characters",
      fun() ->
              ?assertEqual(lists:sort("#^/>!=&{}+-*"),
                           lists:sort(ai_mustache_ext:builtin_markers()))
      end},
     {"a character outside the set is not reserved",
      fun() -> ?assertNot(ai_mustache_ext:is_reserved($@)) end}].

valid_marker_cases() ->
    Good = "@%~$:;,?|\\'\"`^",
    Bad  = [${, $}, $\s, $\t, $\n, $a, $Z, $0, $_, 16#4E2D, foo, "s"],
    [{"punctuation is usable as a marker",
      fun() ->
              [?assert(ai_mustache_ext:is_valid_marker(C))
               || C <- Good, not ai_mustache_ext:is_reserved(C)]
      end},
     {"braces, whitespace, word characters and non-ASCII are not",
      fun() ->
              [?assertNot(ai_mustache_ext:is_valid_marker(C)) || C <- Bad]
      end}].

%%%===================================================================
%%% describe/validate
%%%===================================================================

describe_cases() ->
    [{"a well-formed extension validates and reports its markers",
      fun() ->
              ?assertEqual({ok, #{markers => [$@], block_markers => []}},
                           ai_mustache_ext:describe(my_i18n)),
              ?assertEqual(ok, ai_mustache_ext:validate(my_i18n))
      end},
     {"block_markers/0 is reported when present",
      fun() ->
              ?assertEqual({ok, #{markers => [$%], block_markers => [$%]}},
                           ai_mustache_ext:describe(my_wrap))
      end},
     {"is_block/2 is false for a module without block_markers/0",
      fun() ->
              ?assertNot(ai_mustache_ext:is_block(my_i18n, $@)),
              ?assert(ai_mustache_ext:is_block(my_wrap, $%)),
              ?assertNot(ai_mustache_ext:is_block(my_wrap, $@))
      end},
     {"a module that never declares the behaviour is rejected",
      fun() ->
              ?assertEqual({error, {not_an_ext_module, bad_ext_no_behaviour}},
                           ai_mustache_ext:validate(bad_ext_no_behaviour))
      end},
     {"a missing callback names the callback",
      fun() ->
              ?assertEqual({error, {ext_missing_callback, bad_ext_missing_cb,
                                    {compile_tag, 4}}},
                           ai_mustache_ext:validate(bad_ext_missing_cb))
      end},
     {"claiming a builtin marker is an error",
      fun() ->
              ?assertEqual({error, {marker_reserved, $#, bad_ext_reserved}},
                           ai_mustache_ext:validate(bad_ext_reserved))
      end},
     {"claiming a character the scanner cannot split on is an error",
      fun() ->
              ?assertEqual({error, {invalid_marker, ${, bad_ext_invalid_char}},
                           ai_mustache_ext:validate(bad_ext_invalid_char))
      end},
     {"claiming nothing at all is an error",
      fun() ->
              ?assertEqual({error, {ext_no_markers, bad_ext_no_markers}},
                           ai_mustache_ext:validate(bad_ext_no_markers))
      end},
     {"a block marker outside markers/0 is an error",
      fun() ->
              ?assertEqual({error, {block_marker_not_declared, $~,
                                    bad_ext_bad_block}},
                           ai_mustache_ext:validate(bad_ext_bad_block))
      end},
     {"a module that does not exist anywhere is reported as such",
      fun() ->
              ?assertEqual({error, {ext_module_not_found, no_such_ext_module}},
                           ai_mustache_ext:validate(no_such_ext_module))
      end}].

%%%===================================================================
%%% registry/1,2
%%%===================================================================

registry_cases() ->
    [{"a bare module claims every marker it declares",
      fun() ->
              ?assertEqual({ok, #{$@ => my_i18n, $% => my_wrap}},
                           ai_mustache_ext:registry([my_i18n, my_wrap]))
      end},
     {"a {Marker, Module} spec claims exactly one character",
      fun() ->
              ?assertEqual({ok, #{$@ => my_i18n}},
                           ai_mustache_ext:registry([{$@, my_i18n}]))
      end},
     {"a {Marker, {Module, Fun}} spec is accepted too",
      fun() ->
              ?assertEqual({ok, #{$@ => my_i18n}},
                           ai_mustache_ext:registry([{$@, {my_i18n, compile_tag}}]))
      end},
     {"a marker the module does not declare is refused, showing what it does",
      fun() ->
              ?assertEqual({error, {marker_not_declared, $%, my_i18n, [$@]}},
                           ai_mustache_ext:registry([{$%, my_i18n}]))
      end},
     {"two modules claiming the same marker lists both",
      fun() ->
              ?assertEqual({error, {marker_conflict, $~,
                                    [bad_ext_conflict_a, bad_ext_conflict_b]}},
                           ai_mustache_ext:registry([bad_ext_conflict_a,
                                                     bad_ext_conflict_b]))
      end},
     {"the conflict report does not depend on the input order",
      fun() ->
              A = ai_mustache_ext:registry([bad_ext_conflict_a,
                                            bad_ext_conflict_b]),
              B = ai_mustache_ext:registry([bad_ext_conflict_b,
                                            bad_ext_conflict_a]),
              ?assertEqual(A, B)
      end},
     {"the same module listed twice is not a conflict with itself",
      fun() ->
              ?assertEqual({ok, #{$@ => my_i18n}},
                           ai_mustache_ext:registry([my_i18n, my_i18n]))
      end},
     {"an invalid module fails the whole registry",
      fun() ->
              ?assertEqual({error, {not_an_ext_module, bad_ext_no_behaviour}},
                           ai_mustache_ext:registry([my_i18n,
                                                     bad_ext_no_behaviour]))
      end},
     {"an empty list yields an empty table",
      fun() -> ?assertEqual({ok, #{}}, ai_mustache_ext:registry([])) end},
     {"spec_module/1 and spec_markers/1 read the three shapes",
      fun() ->
              ?assertEqual(my_i18n, ai_mustache_ext:spec_module(my_i18n)),
              ?assertEqual(my_i18n, ai_mustache_ext:spec_module({$@, my_i18n})),
              ?assertEqual(my_i18n,
                           ai_mustache_ext:spec_module({$@, {my_i18n, f}})),
              ?assertEqual(all, ai_mustache_ext:spec_markers(my_i18n)),
              ?assertEqual([$@], ai_mustache_ext:spec_markers({$@, my_i18n}))
      end}].

%%%===================================================================
%%% Validation without a beam
%%%===================================================================

source_fallback_cases() ->
    [{"an extension with no beam is validated from its source",
      fun() ->
              %% The premise of the test: it really is not loaded.
              ?assertEqual(false, code:is_loaded(src_only_ext)),
              Dir = src_only_dir(),
              ?assertEqual({ok, #{markers => [$$], block_markers => []}},
                           ai_mustache_ext:describe(
                             src_only_ext, #{search_dirs => [Dir]}))
      end},
     {"without a search dir there is nothing to read and it is not found",
      fun() ->
              ?assertEqual({error, {ext_module_not_found, no_such_ext_at_all}},
                           ai_mustache_ext:describe(
                             no_such_ext_at_all,
                             #{search_dirs => [fixture_dir()]}))
      end}].

%% The source-fallback path looks for `<Mod>.erl', so this one fixture has to
%% exist under its real name. It is materialised outside test/ for the reason
%% given on fixture_dir/0.
src_only_dir() ->
    Dir = filename:join(code:lib_dir(aihtml), "tmp_transform_tests"),
    ok = filelib:ensure_path(Dir),
    {ok, Src} = file:read_file(fixture_path(src_only_ext)),
    ok = file:write_file(filename:join(Dir, "src_only_ext.erl"), Src),
    Dir.

%%%===================================================================
%%% Builders
%%%===================================================================

helper_cases() ->
    [{"the stack variable defaults to S and is overridable",
      fun() ->
              ?assertEqual('S', ai_mustache_ext:stack_var(#{})),
              ?assertEqual('S__7', ai_mustache_ext:stack_var(#{stack_var => 'S__7'})),
              ?assertEqual('I', ai_mustache_ext:indent_var(#{}))
      end},
     {"stack_expr/1 tracks the configured name instead of hard-coding S",
      fun() ->
              {var, _, V} = ai_mustache_ext:stack_expr(#{stack_var => 'Z'}),
              ?assertEqual('Z', V),
              {var, _, D} = ai_mustache_ext:stack_expr(#{}),
              ?assertEqual('S', D)
      end},
     {"anno/1 uses loc when it is there and 0 when it is not",
      fun() ->
              ?assertEqual(3, erl_anno:line(ai_mustache_ext:anno(#{loc => {3, 5}}))),
              ?assertEqual(0, erl_anno:line(ai_mustache_ext:anno(#{})))
      end},
     {"lookup_expr/2 builds the runtime call and evaluates correctly",
      fun() ->
              E = ai_mustache_ext:lookup_expr([a, b], #{}),
              ?assertMatch({call, _, {remote, _, {atom, _, ai_mustache_rt},
                                      {atom, _, lookup}}, [_, {var, _, 'S'}]}, E),
              ?assertEqual(<<"v">>, eval_with_stack(E, [#{a => #{b => <<"v">>}}]))
      end},
     {"escape_expr/2 and to_binary_expr/2 build the runtime calls",
      fun() ->
              Lit = {bin, erl_anno:new(0),
                     [{bin_element, erl_anno:new(0), {string, erl_anno:new(0), "<a>"},
                       default, default}]},
              ?assertEqual(<<"&lt;a&gt;">>,
                           iolist_to_binary(
                             eval_with_stack(
                               ai_mustache_ext:escape_expr(Lit, #{}), []))),
              ?assertEqual(<<"<a>">>,
                           eval_with_stack(ai_mustache_ext:to_binary_expr(Lit, #{}), []))
      end},
     {"ext_opt/3 reads the module's own slice of ext_opts",
      fun() ->
              ?assertEqual(dflt, ai_mustache_ext:ext_opt(my_i18n, #{}, dflt)),
              ?assertEqual(cfg,
                           ai_mustache_ext:ext_opt(
                             my_i18n, #{ext_opts => #{my_i18n => cfg}}, dflt))
      end}].

eval_with_stack(Expr, Stack) ->
    Bindings = erl_eval:add_binding('S', Stack, erl_eval:new_bindings()),
    {value, V, _} = erl_eval:expr(Expr, Bindings),
    V.

%%%===================================================================
%%% Diagnostics
%%%===================================================================

format_error_cases() ->
    Reasons =
        [{{ext_module_not_found, my_ext}, ["my_ext", "erl_first_files"]},
         {{not_an_ext_module, my_ext},    ["my_ext", "ai_mustache_ext"]},
         {{ext_missing_callback, my_ext, {compile_tag, 4}},
          ["my_ext", "compile_tag"]},
         {{ext_no_markers, my_ext},       ["my_ext", "markers/0"]},
         {{invalid_marker, ${, my_ext},   ["my_ext", "{"]},
         {{marker_reserved, $#, my_ext},  ["my_ext", "'#'"]},
         {{marker_conflict, $@, [a_ext, b_ext]}, ["a_ext", "b_ext", "'@'"]},
         {{marker_not_declared, $%, my_ext, [$@]}, ["my_ext", "'%'", "'@'"]},
         {{block_marker_not_declared, $~, my_ext}, ["my_ext", "block_markers/0"]}],
    [{lists:flatten(io_lib:format("format_error ~p mentions ~p", [R, Words])),
      fun() ->
              Text = lists:flatten(ai_mustache_ext:format_error(R)),
              [?assert(string:find(Text, W) =/= nomatch) || W <- Words]
      end}
     || {R, Words} <- Reasons].
