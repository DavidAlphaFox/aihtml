%%%-------------------------------------------------------------------
%%% @doc End-to-end tests for the rebar3 plugin.
%%%
%%% Common Test rather than EUnit because these cases drive a real rebar3
%%% subprocess against a real project: they need an isolated directory per
%%% case (priv_dir) and a place to put a rebar3 cache that is not the
%%% developer's own. The handful of pure-algorithm cases at the end run in
%%% process against the plugin modules, which init_per_suite compiles into
%%% priv_dir.
%%%
%%% Every case works on its own copy of a fixture, so the checked-in
%%% test/fixtures tree is never written to; fixtures_stay_clean asserts that.
%%%
%%% aihtml and rebar3_aihtml reach the fixture through _checkouts/. rebar3 has
%%% no `path' resource -- {plugins, [{name, {path, Dir}}]} fails with
%%% fetch_fail -- so _checkouts is the only way to build against a local tree
%%% without a network.
%%% @end
%%%-------------------------------------------------------------------
-module(rebar3_aihtml_SUITE).

-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("../rebar3_aihtml/include/rebar3_aihtml.hrl").

suite() -> [{timetrap, {minutes, 5}}].

all() ->
    [%% pure, in process
     name_mapping,
     illegal_name_detected,
     collision_detection,
     partials_exist_check,
     rewrite_strips_one_level,
     rewrite_leaves_hard_cases,
     rewrite_is_idempotent,
     diff_is_unified,
     %% end to end, rebar3 subprocess
     basic_compile,
     generated_file_shape,
     second_run_skips_everything,
     touch_does_not_rebuild,
     content_change_rebuilds_only_that_template,
     opts_change_rebuilds_everything,
     partial_change_does_not_rebuild_parent,
     force_does_not_churn_mtimes,
     corrupt_generated_file_heals,
     orphan_is_removed,
     handwritten_file_is_kept,
     module_name_conflict_is_reported,
     illegal_module_name_is_reported,
     every_broken_template_is_reported,
     missing_partial_is_reported,
     mutual_partials_compile,
     umbrella_apps_are_independent,
     mustache_template_staleness,
     %% jinja, and the two engines together
     jinja_compile,
     jinja_second_run_skips_everything,
     jinja_base_change_does_not_rebuild_child,
     dual_engines_share_an_out_dir,
     dual_engines_are_order_independent,
     dual_engine_module_conflict_is_reported,
     migrate_prints_a_diff,
     migrate_write_then_idempotent,
     fixtures_stay_clean].

%%%===================================================================
%%% Suite setup
%%%===================================================================

init_per_suite(Config) ->
    case os:find_executable("rebar3") of
        false ->
            {skip, "rebar3 not on PATH"};
        Rebar3 ->
            Priv = ?config(priv_dir, Config),
            Repo = repo_dir(Config),
            Seed = seed(Priv, Repo),
            Ebin = compile_plugin(Priv, Repo),
            true = code:add_pathz(Ebin),
            [{rebar3, Rebar3}, {repo, Repo}, {seed, Seed} | Config]
    end.

end_per_suite(_Config) ->
    ok.

%% The repository root, found by walking up from this module's beam until a
%% directory holds both halves of the project. Neither data_dir nor the cwd can
%% be used: CT runs the suite out of _build and moves the cwd per case.
repo_dir(_Config) ->
    up(filename:dirname(code:which(?MODULE))).

up("/") ->
    ct:fail(cannot_find_repo_root);
up(Dir) ->
    case filelib:is_regular(filename:join([Dir, "rebar3_aihtml", "rebar.config"]))
        andalso filelib:is_regular(filename:join([Dir, "src",
                                                  "ai_mustache_compiler.erl"])) of
        true  -> Dir;
        false -> up(filename:dirname(Dir))
    end.

%% A clean copy of the two local apps for the fixtures' _checkouts to link to.
%% Copied rather than referenced so that no test can write into the repository,
%% and so that no _build directory comes along for the ride.
seed(Priv, Repo) ->
    Seed = filename:join(Priv, "seed"),
    ok = filelib:ensure_dir(filename:join(Seed, "x")),
    copy_app(Repo, filename:join(Seed, "aihtml"), ["src", "include"]),
    ok = file:write_file(filename:join([Seed, "aihtml", "rebar.config"]),
                         <<"{erl_opts, [debug_info]}.\n{deps, []}.\n">>),
    copy_app(filename:join(Repo, "rebar3_aihtml"),
             filename:join(Seed, "rebar3_aihtml"), ["src", "include"]),
    {ok, _} = file:copy(filename:join([Repo, "rebar3_aihtml", "rebar.config"]),
                        filename:join([Seed, "rebar3_aihtml", "rebar.config"])),
    Seed.

copy_app(From, To, Dirs) ->
    ok = filelib:ensure_dir(filename:join(To, "x")),
    _ = file:make_dir(To),
    [cp_r(filename:join(From, D), filename:join(To, D)) || D <- Dirs],
    ok.

%% Copy the CONTENTS of From into To. `cp -R From To' would nest From inside
%% To whenever To already exists, which is exactly the case here.
cp_r(From, To) ->
    case filelib:is_dir(From) of
        false -> ok;
        true ->
            ok = filelib:ensure_dir(filename:join(To, "x")),
            _ = file:make_dir(To),
            [] = os:cmd(lists:flatten(
                          io_lib:format("cp -R ~ts/. ~ts/ 2>&1", [From, To]))),
            ok
    end.

%% The plugin's own modules, built into priv_dir so the pure cases can call
%% them. Nothing here needs rebar3 to be running: the modules that talk to
%% rebar_state are not exercised in process.
compile_plugin(Priv, Repo) ->
    Ebin = filename:join(Priv, "plugin_ebin"),
    _ = file:make_dir(Ebin),
    Src = filename:join([Repo, "rebar3_aihtml", "src"]),
    Inc = filename:join([Repo, "rebar3_aihtml", "include"]),
    Files = filelib:wildcard(filename:join(Src, "*.erl")),
    lists:foreach(
      fun(F) ->
              case compile:file(F, [{outdir, Ebin}, {i, Inc},
                                    debug_info, return_errors]) of
                  {ok, _} -> ok;
                  Other   -> ct:fail({plugin_compile_failed, F, Other})
              end
      end, Files),
    Ebin.

%%%===================================================================
%%% Per case scaffolding
%%%===================================================================

%% Copy a fixture into this case's priv_dir and wire _checkouts up to the seed.
project(Fixture, Config) ->
    Priv = ?config(priv_dir, Config),
    Seed = ?config(seed, Config),
    Case = atom_to_list(?config(tc_name, Config)),
    Dir = filename:join([Priv, Case, Fixture]),
    ok = filelib:ensure_dir(filename:join(Dir, "x")),
    cp_r(filename:join([repo_dir(Config), "test", "fixtures", Fixture]), Dir),
    unhide_sources(Dir),
    Checkouts = filename:join(Dir, "_checkouts"),
    ok = filelib:ensure_dir(filename:join(Checkouts, "x")),
    _ = file:make_dir(Checkouts),
    cp_r(filename:join(Seed, "aihtml"), filename:join(Checkouts, "aihtml")),
    cp_r(filename:join(Seed, "rebar3_aihtml"),
         filename:join(Checkouts, "rebar3_aihtml")),
    Dir.

%% Fixture Erlang sources are checked in as .erl.in. `rebar3 eunit' scans the
%% whole of test/ for .erl files and would report these -- sources of the
%% fixture projects, not of aihtml -- as test modules it cannot find. Renaming
%% them here is what keeps the two worlds apart.
unhide_sources(Dir) ->
    [begin
         ok = file:rename(F, filename:rootname(F)),
         ok
     end || F <- filelib:wildcard(filename:join([Dir, "**", "*.erl.in"]))],
    ok.

init_per_testcase(Case, Config) ->
    [{tc_name, Case} | Config].

end_per_testcase(_Case, _Config) ->
    ok.

%% Run rebar3 in Dir. Returns {ExitCode, Output}.
%%
%% A subprocess rather than rebar3's in-process API: rebar_state is global
%% enough that running it inside the CT node would leak configuration from one
%% case into the next. The environment is scrubbed so that the inner rebar3
%% neither reads nor writes the developer's caches.
run(Dir, Args, Config) ->
    Rebar3 = ?config(rebar3, Config),
    Priv = ?config(priv_dir, Config),
    Env = [{"REBAR_CACHE_DIR", filename:join(Priv, "rebar3_cache")},
           {"REBAR_GLOBAL_CONFIG_DIR", filename:join(Priv, "rebar3_global")},
           {"REBAR_BASE_DIR", false},
           {"REBAR_PROFILE", false},
           {"REBAR_CONFIG", false},
           {"REBAR_COLOR", "none"},
           {"TERM", "dumb"}],
    Port = open_port({spawn_executable, Rebar3},
                     [{cd, Dir}, {args, Args}, {env, Env},
                      exit_status, stderr_to_stdout, binary, hide]),
    collect(Port, []).

collect(Port, Acc) ->
    receive
        {Port, {data, D}}        -> collect(Port, [D | Acc]);
        {Port, {exit_status, S}} -> {S, iolist_to_binary(lists:reverse(Acc))}
    after 180000 ->
            catch port_close(Port),
            {timeout, iolist_to_binary(lists:reverse(Acc))}
    end.

%% Build the project once before driving `rebar3 mustache' directly.
%%
%% The plugin carries its own copy of the compile core when it is installed the
%% normal way, but the fixtures wire it in through _checkouts, where that copy
%% is not reachable and the plugin has to borrow the project's aihtml instead.
%% That dependency has to have been compiled once, which `rebar3 compile' does.
bootstrap(Dir, Config) ->
    ok_run(Dir, ["compile"], Config).

ok_run(Dir, Args, Config) ->
    case run(Dir, Args, Config) of
        {0, Out} -> Out;
        Other    -> ct:fail({rebar3_failed, Args, Other})
    end.

fail_run(Dir, Args, Config) ->
    case run(Dir, Args, Config) of
        {0, Out} -> ct:fail({expected_failure, Args, Out});
        {S, Out} when is_integer(S) -> Out;
        Other -> ct:fail({rebar3_failed, Args, Other})
    end.

%% {RelName => {Mtime, Md5}} for every generated file.
snapshot(Dir) ->
    Gen = filename:join(Dir, "_gen"),
    maps:from_list(
      [{filename:basename(F), {mtime(F), erlang:md5(read(F))}}
       || F <- filelib:wildcard(filename:join(Gen, "*.erl"))]).

mtime(F) ->
    {ok, Info} = file:read_file_info(F, [{time, posix}]),
    element(6, Info).

read(F) -> {ok, B} = file:read_file(F), B.

%% mtime has one second resolution, so a case that wants to observe a write
%% has to leave a second between the two runs it compares.
tick() -> timer:sleep(1100).

%% Render inside a fresh VM: loading generated modules into the CT node would
%% carry one case's code into the next.
render(Dir, Expr) ->
    Erl = os:find_executable("erl"),
    Paths = filelib:wildcard(filename:join([Dir, "_build", "default", "*", "*",
                                            "ebin"])),
    Args = lists:append([["-pa", P] || P <- Paths])
        ++ ["-noshell", "-eval", Expr, "-s", "init", "stop"],
    Port = open_port({spawn_executable, Erl},
                     [{cd, Dir}, {args, Args}, exit_status,
                      stderr_to_stdout, binary, hide]),
    case collect(Port, []) of
        {0, Out} -> Out;
        Other    -> ct:fail({render_failed, Expr, Other})
    end.

contains(Haystack, Needle) ->
    binary:match(Haystack, list_to_binary(Needle)) =/= nomatch.

%%%===================================================================
%%% Pure cases
%%%===================================================================

name_mapping(_Config) ->
    Opts = eopts(ai_mustache_engine, <<"view_">>),
    ?assertEqual(view_index, rebar3_aihtml_name:module_of(<<"index">>, Opts)),
    ?assertEqual(view_shared_item,
                 rebar3_aihtml_name:module_of(<<"shared/item">>, Opts)),
    ?assertEqual(view_layout_default,
                 rebar3_aihtml_name:module_of(<<"layout/default">>, Opts)),
    ?assertEqual(view_a_b, rebar3_aihtml_name:module_of(<<"a-b">>, Opts)),
    %% shared/item and shared_item really do collide; that is what makes the
    %% collision check necessary rather than paranoid.
    ?assertEqual(rebar3_aihtml_name:module_of(<<"shared/item">>, Opts),
                 rebar3_aihtml_name:module_of(<<"shared_item">>, Opts)),
    %% A different prefix moves every module.
    ?assertEqual(tpl_index,
                 rebar3_aihtml_name:module_of(<<"index">>,
                                              eopts(ai_mustache_engine, <<"tpl_">>))),
    %% The name is derived from the path exactly as a {{> ...}} would spell it.
    ?assertEqual(<<"shared/item">>,
                 rebar3_aihtml_name:name_of("/v/shared/item.mustache", "/v",
                                            ".mustache")),
    %% The mapping is the engine's, so a jinja template lands where its own
    %% {% extends %} would look for it.
    JOpts = eopts(ai_jinja_engine, <<"j2_">>),
    ?assertEqual(j2_index, rebar3_aihtml_name:module_of(<<"index">>, JOpts)),
    ?assertEqual(j2_layout_base,
                 rebar3_aihtml_name:module_of(<<"layout/base.j2">>, JOpts)),
    ?assertEqual(<<"layout/base">>,
                 rebar3_aihtml_name:name_of("/v/layout/base.j2", "/v", ".j2")).

eopts(Engine, Prefix) ->
    #mopts{engine = Engine, prefix = Prefix,
           engine_opts = #{prefix => Prefix}}.

illegal_name_detected(_Config) ->
    Opts = #mopts{prefix = <<"view_">>},
    ?assertEqual(ok, rebar3_aihtml_name:validate(view_ok, "views/ok.mustache", Opts)),
    ?assertMatch({error, {_, 0, {illegal_module_name, _, _}}},
                 rebar3_aihtml_name:validate('view_a+b', "views/a+b.mustache", Opts)),
    %% An empty prefix is legal, but then the name has to carry the lowercase
    %% first letter itself.
    ?assertMatch({error, {_, 0, {illegal_module_name, _, _}}},
                 rebar3_aihtml_name:validate('Index', "views/Index.mustache",
                                             #mopts{prefix = <<>>})).

collision_detection(_Config) ->
    A = #tpl{module = view_shared_item, rel_path = "views/shared/item.mustache"},
    B = #tpl{module = view_shared_item, rel_path = "views/shared_item.mustache"},
    C = #tpl{module = view_other, rel_path = "views/other.mustache"},
    Errors = rebar3_aihtml_check:collisions([A, B, C], mopts()),
    %% Both offending paths are named, not just the second one found.
    ?assertEqual(2, length(Errors)),
    Text = iolist_to_binary([io_lib:format("~p", [E]) || E <- Errors]),
    ?assert(contains(Text, "views/shared/item.mustache")),
    ?assert(contains(Text, "views/shared_item.mustache")),
    ?assertEqual([], rebar3_aihtml_check:collisions([C], mopts())),
    %% Cross-app duplicates are reported per module with the apps involved.
    ?assertEqual([{view_index, [app_a, app_b]}],
                 rebar3_aihtml_check:cross_app(
                   [{app_a, [#tpl{module = view_index}]},
                    {app_b, [#tpl{module = view_index}]}])).

partials_exist_check(_Config) ->
    {ok, Nodes} = ai_mustache_parser:parse(
                    <<"a\n{{#s}}\n{{> shared/gone}}\n{{/s}}\n">>, #{}),
    Avail = sets:from_list([<<"shared/here">>], [{version, 2}]),
    ?assertEqual([{"views/x.mustache", 3, {partial_not_found, <<"shared/gone">>}}],
                 rebar3_aihtml_check:partials_exist(Nodes, Avail,
                                                    "views/x.mustache", mopts())),
    Avail2 = sets:from_list([<<"shared/gone">>], [{version, 2}]),
    ?assertEqual([], rebar3_aihtml_check:partials_exist(Nodes, Avail2,
                                                        "views/x.mustache",
                                                        mopts())),
    %% The jinja engine checks its own targets against views_abs, so the
    %% plugin has nothing to add and must not invent a mustache-shaped walk
    %% over a jinja AST.
    ?assertEqual([], rebar3_aihtml_check:partials_exist(
                       Nodes, Avail, "views/x.j2",
                       (mopts())#mopts{engine = ai_jinja_engine})).

%% A #mopts{} good enough for the pure checks, which look at two fields.
mopts() -> #mopts{engine = ai_mustache_engine, prefix = <<"view_">>}.

rewrite_strips_one_level(_Config) ->
    Opts = #{module => m, source => <<"t">>},
    {Out1, N1, []} = rebar3_aihtml_rewrite:file(
                       <<"{{#user}}{{user.name}}{{/user}}">>, Opts),
    ?assertEqual(<<"{{#user}}{{name}}{{/user}}">>, Out1),
    ?assertEqual(1, N1),
    %% Whitespace style inside the tag survives.
    {Out2, 1, []} = rebar3_aihtml_rewrite:file(
                      <<"{{#user}}{{ user.name }}{{/user}}">>, Opts),
    ?assertEqual(<<"{{#user}}{{ name }}{{/user}}">>, Out2),
    %% All three prefixes go, and the closing tag follows its opening tag.
    {Out3, 3, []} = rebar3_aihtml_rewrite:file(
                      <<"{{# items}}{{+ items.current}}<li>{{items.name}}</li>"
                        "{{/ items.current}}{{/ items}}">>, Opts),
    ?assertEqual(<<"{{# items}}{{+ current}}<li>{{name}}</li>"
                   "{{/ current}}{{/ items}}">>, Out3),
    %% A comment is not a reference.
    {Out4, 0, []} = rebar3_aihtml_rewrite:file(
                      <<"{{#a}}{{! a.x is prose }}{{/a}}">>, Opts),
    ?assertEqual(<<"{{#a}}{{! a.x is prose }}{{/a}}">>, Out4),
    %% {{^x}} and {{+x}} do not push a scope, so their bodies keep their
    %% prefixes; only their own key is subject to stripping.
    {Out5, 0, []} = rebar3_aihtml_rewrite:file(<<"{{^a}}{{a.x}}{{/a}}">>, Opts),
    ?assertEqual(<<"{{^a}}{{a.x}}{{/a}}">>, Out5),
    %% Custom delimiters are handled by position, not by pattern.
    {Out6, 1, []} = rebar3_aihtml_rewrite:file(
                      <<"{{=<% %>=}}<%#a%><%a.x%><%/a%>">>, Opts),
    ?assertEqual(<<"{{=<% %>=}}<%#a%><%x%><%/a%>">>, Out6).

rewrite_leaves_hard_cases(_Config) ->
    Opts = #{module => m, source => <<"t">>},
    Src = <<"{{#a}}{{b.x}}{{/a}}\n{{#b}}ok{{/b}}\n{{#q}}{{*yield}}{{/q}}\n"
            "{{#p}}{{> shared/item}}{{/p}}\n{{#m.n}}{{m.n.v}}{{/m.n}}\n">>,
    {Out, _N, Manual} = rebar3_aihtml_rewrite:file(Src, Opts),
    Text = iolist_to_binary([[integer_to_list(L), " ", T, "\n"]
                             || {L, T} <- Manual]),
    ?assert(contains(Text, "sibling section b")),
    ?assert(contains(Text, "lambda")),
    ?assert(contains(Text, "partial")),
    ?assert(contains(Text, "dotted section key")),
    %% The sibling reference is left exactly as it was.
    ?assert(contains(Out, "{{#a}}{{b.x}}{{/a}}")).

rewrite_is_idempotent(_Config) ->
    Opts = #{module => m, source => <<"t">>},
    Src = <<"{{! items.x }}\n{{#user}}Hello {{ user.name }}!{{/user}}\n"
            "{{# items}}{{+ items.current}}<li>{{items.name}}</li>"
            "{{/ items.current}}{{/ items}}\n">>,
    {Once, N, _} = rebar3_aihtml_rewrite:file(Src, Opts),
    ?assert(N > 0),
    ?assertMatch({Once, 0, _}, rebar3_aihtml_rewrite:file(Once, Opts)).

diff_is_unified(_Config) ->
    Old = <<"one\ntwo\nthree\n">>,
    New = <<"one\nTWO\nthree\n">>,
    Out = iolist_to_binary(rebar3_aihtml_diff:unified("views/x.mustache",
                                                      Old, New)),
    ?assert(contains(Out, "--- a/views/x.mustache")),
    ?assert(contains(Out, "+++ b/views/x.mustache")),
    ?assert(contains(Out, "@@ ")),
    ?assert(contains(Out, "-two")),
    ?assert(contains(Out, "+TWO")),
    ?assert(contains(Out, " one")),
    ?assertEqual([], rebar3_aihtml_diff:unified("x", Old, Old)).

%%%===================================================================
%%% End to end
%%%===================================================================

basic_compile(Config) ->
    Dir = project("plugin_basic", Config),
    Out = ok_run(Dir, ["compile"], Config),
    ?assert(contains(Out, "mustache: compiled 4")),
    [?assert(filelib:is_regular(filename:join([Dir, "_gen", F])))
     || F <- ["view_complex.erl", "view_static.erl",
              "view_shared_item.erl", "view_shared_user.erl"]],
    Ebin = filename:join([Dir, "_build", "default", "lib", "plugin_basic",
                          "ebin"]),
    ?assert(filelib:is_regular(filename:join(Ebin, "view_complex.beam"))),
    %% The generated modules render, and no application needs starting first:
    %% aihtml is a library app and the generated code only calls into it.
    Rendered = render(Dir, "io:format(\"~ts\", [pb_render:complex()])"),
    ?assertEqual(<<"<h1>Hi &amp; &lt;bye&gt;</h1>\n"
                   "  <p>ada (admin)</p>\n"
                   "<ul>\n"
                   "  <li>one</li>\n"
                   "  <li>two</li>\n"
                   "</ul>\n">>, Rendered),
    %% Partials are cross-module calls, and the callee list is on record.
    ?assertEqual(<<"[view_shared_item,view_shared_user]">>,
                 render(Dir, "io:format(\"~p\", [view_complex:partials()])")).

generated_file_shape(Config) ->
    Dir = project("plugin_basic", Config),
    _ = ok_run(Dir, ["compile"], Config),
    Src = read(filename:join([Dir, "_gen", "view_complex.erl"])),
    [First | _] = binary:split(Src, <<"\n">>),
    ?assert(contains(First, "Generated by rebar3_aihtml")),
    ?assert(contains(First, "DO NOT EDIT")),
    ?assert(contains(First, "views/complex.mustache")),
    ?assert(contains(Src, "-mustache_source(#{")),
    %% Every field of the self description is present.
    [?assert(contains(Src, F))
     || F <- ["path =>", "stamp =>", "mtime =>", "vsn =>", "opts =>"]],
    %% Nothing machine specific leaks into the output: no absolute path, so
    %% the same repository produces the same bytes anywhere.
    ?assertNot(contains(Src, Dir)),
    ?assertNot(contains(Src, "/home/")),
    %% And the plugin's view of the stamp is the compiler's view of it.
    {ok, #{stamp := Stamp}} =
        rebar3_aihtml_scan:meta(filename:join([Dir, "_gen",
                                               "view_complex.erl"]),
                                ai_mustache_engine),
    Body = read(filename:join([Dir, "views", "complex.mustache"])),
    ?assertEqual(ai_mustache_compiler:source_hash(
                   Body, #{prefix => <<"view_">>, extensions => [],
                           ext_opts => #{}, line_map => true}),
                 Stamp).

second_run_skips_everything(Config) ->
    Dir = project("plugin_basic", Config),
    _ = ok_run(Dir, ["compile"], Config),
    Before = snapshot(Dir),
    tick(),
    Out = ok_run(Dir, ["mustache"], Config),
    ?assert(contains(Out, "compiled 0, skipped 4")),
    ?assertEqual(Before, snapshot(Dir)).

touch_does_not_rebuild(Config) ->
    %% The load bearing case for "stamps, not mtimes". A git checkout or a CI
    %% cache restore rewrites mtimes wholesale; if the plugin looked at them it
    %% would rebuild the world on every clean build.
    Dir = project("plugin_basic", Config),
    _ = ok_run(Dir, ["compile"], Config),
    Before = snapshot(Dir),
    tick(),
    Now = calendar:local_time(),
    [ok = file:change_time(filename:join([Dir, "views", F]), Now)
     || F <- ["complex.mustache", "static.mustache"]],
    Out = ok_run(Dir, ["mustache"], Config),
    ?assert(contains(Out, "compiled 0, skipped 4")),
    ?assertEqual(Before, snapshot(Dir)).

content_change_rebuilds_only_that_template(Config) ->
    Dir = project("plugin_basic", Config),
    _ = ok_run(Dir, ["compile"], Config),
    Before = snapshot(Dir),
    tick(),
    ok = file:write_file(filename:join([Dir, "views", "static.mustache"]),
                         <<"plain text, now different\n">>),
    _ = ok_run(Dir, ["mustache"], Config),
    After = snapshot(Dir),
    ?assertNotEqual(maps:get("view_static.erl", Before),
                    maps:get("view_static.erl", After)),
    [?assertEqual(maps:get(F, Before), maps:get(F, After))
     || F <- ["view_complex.erl", "view_shared_item.erl",
              "view_shared_user.erl"]].

opts_change_rebuilds_everything(Config) ->
    Dir = project("plugin_basic", Config),
    _ = ok_run(Dir, ["compile"], Config),
    Before = snapshot(Dir),
    tick(),
    Cfg = filename:join(Dir, "rebar.config"),
    Text = read(Cfg),
    ok = file:write_file(Cfg, binary:replace(Text, <<"{line_map, true}">>,
                                             <<"{line_map, false}">>)),
    Out = ok_run(Dir, ["mustache"], Config),
    ?assert(contains(Out, "compiled 4, skipped 0")),
    After = snapshot(Dir),
    %% The options are part of the stamp, so all four are genuinely rewritten.
    [?assertNotEqual(maps:get(F, Before), maps:get(F, After))
     || F <- maps:keys(Before)].

partial_change_does_not_rebuild_parent(Config) ->
    %% The observable proof that partials compile to cross-module calls: the
    %% caller is not invalidated, and it does not need to be, because the new
    %% partial module is picked up at run time.
    Dir = project("plugin_basic", Config),
    _ = ok_run(Dir, ["compile"], Config),
    Before = snapshot(Dir),
    tick(),
    ok = file:write_file(filename:join([Dir, "views", "shared", "item.mustache"]),
                         <<"<li>ITEM {{name}}</li>\n">>),
    Out = ok_run(Dir, ["mustache"], Config),
    ?assert(contains(Out, "compiled 1, skipped 3")),
    After = snapshot(Dir),
    ?assertNotEqual(maps:get("view_shared_item.erl", Before),
                    maps:get("view_shared_item.erl", After)),
    %% Byte identical and untouched, mtime included.
    ?assertEqual(maps:get("view_complex.erl", Before),
                 maps:get("view_complex.erl", After)),
    _ = ok_run(Dir, ["compile"], Config),
    Rendered = render(Dir, "io:format(\"~ts\", [pb_render:complex()])"),
    ?assert(contains(Rendered, "<li>ITEM one</li>")).

force_does_not_churn_mtimes(Config) ->
    Dir = project("plugin_basic", Config),
    _ = ok_run(Dir, ["compile"], Config),
    Before = snapshot(Dir),
    tick(),
    Out = ok_run(Dir, ["mustache", "--force"], Config),
    ?assert(contains(Out, "compiled 4, skipped 0")),
    %% --force recompiles, but identical output is not written back: rebar3's
    %% own erl -> beam step is mtime driven and would otherwise rebuild too.
    ?assertEqual(Before, snapshot(Dir)).

corrupt_generated_file_heals(Config) ->
    Dir = project("plugin_basic", Config),
    _ = ok_run(Dir, ["compile"], Config),
    Gen = filename:join([Dir, "_gen", "view_complex.erl"]),
    Good = read(Gen),
    ok = file:write_file(Gen, binary:part(Good, 0, 200)),
    Out = ok_run(Dir, ["mustache"], Config),
    ?assert(contains(Out, "compiled 1")),
    ?assertEqual(Good, read(Gen)).

orphan_is_removed(Config) ->
    Dir = project("plugin_basic", Config),
    _ = ok_run(Dir, ["compile"], Config),
    Beam = filename:join([Dir, "_build", "default", "lib", "plugin_basic",
                          "ebin", "view_static.beam"]),
    ?assert(filelib:is_regular(Beam)),
    ok = file:delete(filename:join([Dir, "views", "static.mustache"])),
    Out = ok_run(Dir, ["mustache"], Config),
    ?assert(contains(Out, "removing orphan")),
    ?assert(contains(Out, "_gen/view_static.erl")),
    ?assertNot(filelib:is_regular(filename:join([Dir, "_gen",
                                                 "view_static.erl"]))),
    %% The stale beam goes too. Left behind it stays on the code path and the
    %% deleted template keeps rendering, which is a miserable bug to find.
    ?assertNot(filelib:is_regular(Beam)).

handwritten_file_is_kept(Config) ->
    Dir = project("plugin_basic", Config),
    _ = ok_run(Dir, ["compile"], Config),
    Hand = filename:join([Dir, "_gen", "pb_hand_written.erl"]),
    ok = file:write_file(Hand, <<"-module(pb_hand_written).\n">>),
    Out = ok_run(Dir, ["mustache"], Config),
    ?assert(filelib:is_regular(Hand)),
    ?assert(contains(Out, "not a generated file")).

module_name_conflict_is_reported(Config) ->
    Dir = project("plugin_basic", Config),
    _ = ok_run(Dir, ["compile"], Config),
    Before = snapshot(Dir),
    tick(),
    ok = file:write_file(filename:join([Dir, "views", "shared_item.mustache"]),
                         <<"clash\n">>),
    Out = fail_run(Dir, ["mustache"], Config),
    ?assert(contains(Out, "module name conflict")),
    %% Both sides of the clash are named.
    ?assert(contains(Out, "views/shared/item.mustache")),
    ?assert(contains(Out, "views/shared_item.mustache")),
    %% And it is caught before anything is written, so out_dir is untouched.
    ?assertEqual(Before, snapshot(Dir)).

illegal_module_name_is_reported(Config) ->
    Dir = project("plugin_errors", Config),
    _ = bootstrap(Dir, Config),
    ok = file:write_file(filename:join([Dir, "views", "a+b.mustache"]),
                         <<"nope\n">>),
    Out = fail_run(Dir, ["mustache"], Config),
    ?assert(contains(Out, "views/a+b.mustache:0:")),
    ?assert(contains(Out, "not a bare Erlang atom")).

every_broken_template_is_reported(Config) ->
    %% Reporting only the first error would make fixing N templates take N
    %% runs.
    Dir = project("plugin_errors", Config),
    _ = bootstrap(Dir, Config),
    ok = file:write_file(filename:join([Dir, "views", "bad1.mustache"]),
                         <<"a\nb\n{{#items}}\nc\n">>),
    ok = file:write_file(filename:join([Dir, "views", "bad2.mustache"]),
                         <<"x\n{{#oops}}\n">>),
    Out = fail_run(Dir, ["mustache"], Config),
    %% The message names the key but not the marker: the parser's reason does
    %% not carry which of #/^/+/- opened the block, and inventing one would
    %% print {{#oops}} for an unclosed {{^oops}}.
    ?assert(contains(Out, "views/bad1.mustache:3: unclosed section tag for items")),
    ?assert(contains(Out, "views/bad2.mustache:2: unclosed section tag for oops")).

missing_partial_is_reported(Config) ->
    %% The caller's stamp does not change when a partial disappears, so it
    %% would be skipped. The cross check forces it back into the build, and
    %% the error then comes from the compiler with a real line number.
    Dir = project("plugin_errors", Config),
    _ = bootstrap(Dir, Config),
    _ = ok_run(Dir, ["mustache"], Config),
    ok = file:delete(filename:join([Dir, "views", "shared", "thing.mustache"])),
    Out = fail_run(Dir, ["mustache"], Config),
    ?assert(contains(Out, "views/uses_partial.mustache:1:")),
    ?assert(contains(Out, "partial not found: {{> shared/thing}}")).

mutual_partials_compile(Config) ->
    %% Cross-module calls make mutual recursion between templates legal, so
    %% there is no cycle check here to false-positive on. Note that the
    %% recursion is only bounded by the data: a context whose section key stays
    %% truthy renders for ever, exactly as two mutually recursive functions
    %% would. That is the data's problem, not the compiler's.
    Dir = project("plugin_errors", Config),
    _ = bootstrap(Dir, Config),
    ok = file:write_file(filename:join([Dir, "views", "ping.mustache"]),
                         <<"P{{#go}}{{> pong}}{{/go}}">>),
    ok = file:write_file(filename:join([Dir, "views", "pong.mustache"]),
                         <<"Q{{#go}}{{> ping}}{{/go}}">>),
    _ = ok_run(Dir, ["mustache"], Config),
    _ = ok_run(Dir, ["compile"], Config),
    ?assertEqual(<<"[view_pong] [view_ping]">>,
                 render(Dir, "io:format(\"~p ~p\", [view_ping:partials(), "
                             "view_pong:partials()])")),
    ?assertEqual(<<"P">>,
                 render(Dir, "io:format(\"~ts\", [view_ping:render(#{})])")).

umbrella_apps_are_independent(Config) ->
    Dir = project("plugin_umbrella", Config),
    _ = bootstrap(Dir, Config),
    %% Same prefix in both apps, so both views/index.mustache land on
    %% view_index. That is a real global clash and the plugin says so.
    Out = ok_run(Dir, ["mustache"], Config),
    ?assert(contains(Out, "generated by ")),
    ?assert(contains(Out, "prefixes")),
    %% Each app generates into its own out_dir.
    ?assert(filelib:is_regular(filename:join([Dir, "apps", "app_a", "_gen",
                                              "view_index.erl"]))),
    ?assert(filelib:is_regular(filename:join([Dir, "apps", "app_b", "_gen",
                                              "view_index.erl"]))),
    %% Give app_b its own prefix and the warning goes away, the module name
    %% changes and the old generated file is collected as an orphan.
    ok = file:write_file(
           filename:join([Dir, "apps", "app_b", "rebar.config"]),
           <<"{erl_opts, [debug_info, {src_dirs, [\"src\", \"_gen\"]}]}.\n"
             "{mustache_opts, [{views, \"views\"}, {out_dir, \"_gen\"},"
             " {prefix, \"b_\"}]}.\n">>),
    Out2 = ok_run(Dir, ["mustache"], Config),
    ?assertNot(contains(Out2, "generated by apps")),
    ?assert(filelib:is_regular(filename:join([Dir, "apps", "app_b", "_gen",
                                              "b_index.erl"]))),
    ?assertNot(filelib:is_regular(filename:join([Dir, "apps", "app_b", "_gen",
                                                 "view_index.erl"]))),
    %% app_a is unaffected by app_b's configuration.
    ?assert(filelib:is_regular(filename:join([Dir, "apps", "app_a", "_gen",
                                              "view_index.erl"]))).

mustache_template_staleness(Config) ->
    %% A parse_transform cannot tell rebar3 that a module depends on a
    %% template, so the plugin touches the .erl instead. Both directions
    %% matter: touching unconditionally would rebuild these modules forever.
    Dir = project("plugin_basic", Config),
    _ = ok_run(Dir, ["compile"], Config),
    Erl = filename:join([Dir, "src", "pb_uses_tpl.erl"]),
    Before = mtime(Erl),
    tick(),
    _ = ok_run(Dir, ["mustache"], Config),
    ?assertEqual(Before, mtime(Erl)),
    tick(),
    ok = file:write_file(filename:join([Dir, "views", "shared", "user.mustache"]),
                         <<"<p>{{name}} edited</p>\n">>),
    _ = ok_run(Dir, ["mustache"], Config),
    ?assert(mtime(Erl) > Before).

migrate_prints_a_diff(Config) ->
    Dir = project("plugin_legacy", Config),
    _ = bootstrap(Dir, Config),
    Flat = filename:join([Dir, "views", "flat.mustache"]),
    Before = read(Flat),
    Out = ok_run(Dir, ["mustache", "migrate"], Config),
    ?assert(contains(Out, "--- a/views/flat.mustache")),
    ?assert(contains(Out, "+{{#user}}Hello {{ name }}!{{/user}}")),
    ?assert(contains(Out, "+{{# items}}{{+ current}}<li>{{name}}</li>"
                          "{{/ current}}{{/ items}}")),
    %% Custom delimiters are rewritten, comments are not.
    ?assert(contains(Out, "+<%#thing%><%value%><%/thing%>")),
    ?assert(contains(Out, " {{! items.x is a comment and must not be touched }}")),
    %% The things it declines to decide are listed rather than guessed at.
    ?assert(contains(Out, "manual review needed")),
    ?assert(contains(Out, "sibling section b")),
    ?assert(contains(Out, "lambda")),
    %% Without --write nothing is written.
    ?assertEqual(Before, read(Flat)).

migrate_write_then_idempotent(Config) ->
    Dir = project("plugin_legacy", Config),
    _ = bootstrap(Dir, Config),
    Flat = filename:join([Dir, "views", "flat.mustache"]),
    _ = ok_run(Dir, ["mustache", "migrate", "--write"], Config),
    ?assertEqual(<<"{{! items.x is a comment and must not be touched }}\n"
                   "{{#user}}Hello {{ name }}!{{/user}}\n"
                   "{{# items}}{{+ current}}<li>{{name}}</li>"
                   "{{/ current}}{{/ items}}\n"
                   "{{=<% %>=}}\n"
                   "<%#thing%><%value%><%/thing%>\n">>, read(Flat)),
    After = read(Flat),
    %% Idempotent: a rewritten reference no longer carries the prefix, so the
    %% second pass matches nothing.
    Out = ok_run(Dir, ["mustache", "migrate"], Config),
    ?assertEqual(After, read(Flat)),
    ?assertNot(contains(Out, "--- a/views/flat.mustache")),
    %% And what it produced still compiles. This fixture has no provider hook,
    %% so the compiler has to be asked for explicitly.
    _ = ok_run(Dir, ["mustache"], Config),
    _ = ok_run(Dir, ["compile"], Config),
    ?assert(filelib:is_regular(filename:join([Dir, "_gen", "view_flat.erl"]))),
    ?assert(filelib:is_regular(
              filename:join([Dir, "_build", "default", "lib", "plugin_legacy",
                             "ebin", "view_flat.beam"]))).

fixtures_stay_clean(Config) ->
    %% Every case works on a copy, so the checked-in fixtures must show no
    %% sign of ever having been built.
    Fixtures = filename:join([repo_dir(Config), "test", "fixtures"]),
    Dirty = [P || P <- walk(Fixtures),
                  lists:member(filename:basename(P),
                               ["_build", "_gen", "_checkouts"])
                      orelse filename:extension(P) =:= ".beam"
                      orelse filename:extension(P) =:= ".erl"],
    ?assertEqual([], Dirty).

walk(Dir) ->
    lists:append([[P | case filelib:is_dir(P) of
                           true  -> walk(P);
                           false -> []
                       end]
                  || F <- filelib:wildcard(filename:join(Dir, "*")),
                     P <- [F]]).

%%%===================================================================
%%% Jinja
%%%===================================================================

jinja_compile(Config) ->
    Dir = project("plugin_jinja", Config),
    Out = ok_run(Dir, ["compile"], Config),
    ?assert(contains(Out, "jinja: compiled 4")),
    [?assert(filelib:is_regular(filename:join([Dir, "_gen", F])))
     || F <- ["j2_page.erl", "j2_layout_base.erl", "j2_lib.erl",
              "j2_widgets_box.erl"]],
    %% Inheritance, super(), an imported macro and an include, all resolved to
    %% direct cross-module calls at build time.
    %% Byte for byte what CPython jinja2 renders from the same four files
    %% with the same environment -- including `bo ()\', where an explicitly
    %% passed undefined argument does NOT fall back to the macro default.
    Rendered = render(Dir, "io:format(\"~ts\", [pj_render:page()])"),
    ?assertEqual(<<"<html>\n"
                   "[base title] ada &amp; co|"
                   "<li>ada (admin)</li>\n"
                   "<li>bo ()</li>\n"
                   "<div>WIDGETS</div></html>">>, Rendered),
    ?assertEqual(<<"<div>UNTITLED</div>">>,
                 render(Dir, "io:format(\"~ts\", [pj_render:box()])")),
    %% The banner names the engine, which is what keeps two engines from
    %% collecting each other's output.
    Src = read(filename:join([Dir, "_gen", "j2_page.erl"])),
    ?assert(contains(Src, "(jinja) from views/page.j2")),
    ?assert(contains(Src, "-jinja_source(")),
    %% Nothing machine specific: the same repository builds the same bytes.
    ?assertNot(contains(Src, Dir)),
    ?assertNot(contains(Src, "/home/")).

jinja_second_run_skips_everything(Config) ->
    Dir = project("plugin_jinja", Config),
    _ = ok_run(Dir, ["compile"], Config),
    Out = ok_run(Dir, ["compile"], Config),
    ?assert(contains(Out, "jinja: compiled 0, skipped 4")).

%% Architecture invariant 9, observed end to end: a module names only its
%% direct parent, so editing a base template must not rewrite its children.
jinja_base_change_does_not_rebuild_child(Config) ->
    Dir = project("plugin_jinja", Config),
    _ = ok_run(Dir, ["compile"], Config),
    Child = filename:join([Dir, "_gen", "j2_page.erl"]),
    Before = read(Child),
    ok = file:write_file(filename:join([Dir, "views", "layout", "base.j2"]),
                         <<"<html>{% block title %}new{% endblock %}"
                           "|{% block body %}b{% endblock %}</html>\n">>),
    Out = ok_run(Dir, ["compile"], Config),
    ?assert(contains(Out, "jinja: compiled 1")),
    ?assertEqual(Before, read(Child)),
    %% ... and the child still renders the new base.
    ?assert(contains(render(Dir, "io:format(\"~ts\", [pj_render:page()])"),
                     "[new]")).

%%%===================================================================
%%% Both engines at once
%%%===================================================================

%% The failure this guards against is subtle: each provider's orphan collector
%% used to see the other's output as an orphan, so every build deleted half
%% the generated code and the result depended on which provider ran last.
dual_engines_share_an_out_dir(Config) ->
    Dir = project("plugin_dual", Config),
    Out1 = ok_run(Dir, ["compile"], Config),
    ?assert(contains(Out1, "mustache: compiled 1")),
    ?assert(contains(Out1, "jinja: compiled 1")),
    Files = fun() -> lists:sort(filelib:wildcard(
                                  filename:join([Dir, "_gen", "*.erl"]))) end,
    After1 = Files(),
    ?assertEqual(2, length(After1)),
    %% Three more runs must not add, remove or rewrite anything.
    [begin
         _ = ok_run(Dir, ["compile"], Config),
         ?assertEqual(After1, Files())
     end || _ <- lists:seq(1, 3)],
    %% jinja drops the template's trailing newline by default; mustache does not.
    ?assertEqual(<<"M:a\nJ:b x">>,
                 render(Dir, "io:format(\"~ts~ts\", pd_render:both())")).

%% Neither provider may depend on having run first.
dual_engines_are_order_independent(Config) ->
    Dir = project("plugin_dual", Config),
    ok_bootstrap(Dir, Config),
    _ = ok_run(Dir, ["mustache"], Config),
    _ = ok_run(Dir, ["jinja"], Config),
    Forward = generated(Dir),
    [ok = file:delete(F) || F <- filelib:wildcard(
                                   filename:join([Dir, "_gen", "*.erl"]))],
    _ = ok_run(Dir, ["jinja"], Config),
    _ = ok_run(Dir, ["mustache"], Config),
    ?assertEqual(Forward, generated(Dir)).

ok_bootstrap(Dir, Config) -> _ = bootstrap(Dir, Config), ok.

generated(Dir) ->
    lists:sort([{filename:basename(F), read(F)}
                || F <- filelib:wildcard(filename:join([Dir, "_gen", "*.erl"]))]).

%% Erlang module names are global, so the collision check has to span engines.
dual_engine_module_conflict_is_reported(Config) ->
    Dir = project("plugin_dual", Config),
    %% Give both engines the same prefix, so greet.mustache and greet.j2 both
    %% want to be `tpl_greet'.
    ok = file:write_file(
           filename:join(Dir, "rebar.config"),
           <<"{plugins, [rebar3_aihtml]}.\n"
             "{deps, [aihtml]}.\n"
             "{provider_hooks, [{pre, [{compile, mustache}, {compile, jinja}]}]}.\n"
             "{mustache_opts, [{views, \"views\"}, {prefix, \"tpl_\"}]}.\n"
             "{jinja_opts, [{views, \"views\"}, {suffix, \".j2\"}, "
             "{prefix, \"tpl_\"}]}.\n"
             "{erl_opts, [debug_info, {src_dirs, [\"src\", \"_gen\"]}]}.\n">>),
    Out = ok_run(Dir, ["compile"], Config),
    ?assert(contains(Out, "generated more than once")),
    ?assert(contains(Out, "tpl_greet")).
