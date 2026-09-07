%%%-------------------------------------------------------------------
%%% @doc Tests for development-time hot reloading.
%%%
%%% The point of these is not just that reloading works, but that it works
%%% without any state of its own: no ets table, no persistent_term key and no
%%% extra process may appear as a result of using this module. That is
%%% asserted explicitly. See tasks/T27.md.
%%% @end
%%%-------------------------------------------------------------------
-module(ai_mustache_dev_tests).

-include_lib("eunit/include/eunit.hrl").

-define(MOD, view_dev_probe).

%%%===================================================================
%%% Fixtures
%%%===================================================================

setup() ->
    Dir = filename:join(["/tmp", "aihtml_dev_" ++ integer_to_list(erlang:unique_integer([positive]))]),
    ok = filelib:ensure_dir(filename:join(Dir, "x")),
    Dir.

cleanup(Dir) ->
    _ = code:purge(?MOD),
    _ = code:delete(?MOD),
    _ = code:purge(?MOD),
    [file:delete(F) || F <- filelib:wildcard(filename:join(Dir, "*"))],
    _ = file:del_dir(Dir),
    ok.

write(Dir, Body) ->
    Path = filename:join(Dir, "probe.mustache"),
    ok = file:write_file(Path, Body),
    Path.

%% Build the module the way the plugin would, so the attribute it carries is
%% the real thing rather than something hand-made for the test.
build(Path, Body) ->
    Opts0 = #{module => ?MOD, views => filename:dirname(Path),
              prefix => <<"view_">>, line_map => true},
    Opts = Opts0#{source => list_to_binary(Path),
                  stamp  => ai_mustache_compiler:source_hash(Body, Opts0),
                  mtime  => mtime(Path)},
    {ok, Ast} = ai_mustache_parser:parse(Body, Opts),
    {ok, Forms, _} = ai_mustache_compiler:forms(Ast, Opts),
    {ok, ?MOD, Bin} = compile:forms(Forms, [return_errors, binary, debug_info]),
    _ = code:purge(?MOD),
    {module, ?MOD} = code:load_binary(?MOD, Path, Bin),
    ok.

mtime(Path) ->
    {ok, FI} = file:read_file_info(Path, [{time, posix}]),
    element(6, FI).

%%%===================================================================
%%% Tests
%%%===================================================================

dev_test_() ->
    {foreach, fun setup/0, fun cleanup/1,
     [fun(Dir) -> {"source/1 reads the generated attribute", fun() -> t_source(Dir) end} end,
      fun(_Dir) -> {"source/1 rejects a non-template module", fun t_not_template/0} end,
      fun(Dir) -> {"unchanged file is not stale", fun() -> t_fresh(Dir) end} end,
      fun(Dir) -> {"touch without an edit is not stale", fun() -> t_touch(Dir) end} end,
      fun(Dir) -> {"edited file is stale", fun() -> t_edited(Dir) end} end,
      fun(Dir) -> {"edit within the same second is still stale", fun() -> t_same_second(Dir) end} end,
      fun(Dir) -> {"reload/1 picks up the new content", fun() -> t_reload(Dir) end} end,
      fun(Dir) -> {"reload/1 recovers options from the attribute", fun() -> t_reload_opts(Dir) end} end,
      fun(Dir) -> {"a broken template reports a structured error", fun() -> t_broken(Dir) end} end,
      fun(Dir) -> {"reloading creates no ets table, no process, no persistent_term",
                   fun() -> t_no_state(Dir) end} end,
      fun(Dir) -> {"template_modules/0 finds the module", fun() -> t_listing(Dir) end} end,
      fun(Dir) -> {"check/0 passes when the template exists", fun() -> t_check(Dir) end} end]}.

t_source(Dir) ->
    Path = write(Dir, <<"hi {{x}}">>),
    ok = build(Path, <<"hi {{x}}">>),
    {ok, S} = ai_mustache_dev:source(?MOD),
    ?assertMatch(#{path := _, stamp := _, mtime := _, vsn := _, opts := _}, S),
    ?assertEqual(list_to_binary(Path), maps:get(path, S)).

t_not_template() ->
    ?assertEqual({error, not_a_template_module}, ai_mustache_dev:source(lists)).

t_fresh(Dir) ->
    Body = <<"hi {{x}}">>,
    Path = write(Dir, Body),
    ok = build(Path, Body),
    ?assertEqual(false, ai_mustache_dev:stale(?MOD)).

%% Rewriting identical content, and bumping the mtime on top of it, must not
%% register as stale: only the content stamp decides.
t_touch(Dir) ->
    Body = <<"hi {{x}}">>,
    Path = write(Dir, Body),
    ok = build(Path, Body),
    ok = file:write_file(Path, Body),
    {ok, FI} = file:read_file_info(Path, [{time, posix}]),
    ok = file:write_file_info(Path, setelement(6, FI, element(6, FI) + 100),
                              [{time, posix}]),
    ?assertEqual(false, ai_mustache_dev:stale(?MOD)).

t_edited(Dir) ->
    Path = write(Dir, <<"hi {{x}}">>),
    ok = build(Path, <<"hi {{x}}">>),
    _ = write(Dir, <<"bye {{x}}">>),
    ?assertEqual(true, ai_mustache_dev:stale(?MOD)).

%% POSIX mtime has one-second granularity, so an mtime-based freshness filter
%% would call this file fresh. The edit-then-reload loop lives entirely inside
%% one second, which is why stale/1 hashes the content instead.
t_same_second(Dir) ->
    Path = write(Dir, <<"a{{x}}">>),
    ok = build(Path, <<"a{{x}}">>),
    {ok, Before} = file:read_file_info(Path, [{time, posix}]),
    _ = write(Dir, <<"b{{x}}">>),
    ok = file:write_file_info(Path, Before, [{time, posix}]),
    {ok, After} = file:read_file_info(Path, [{time, posix}]),
    ?assertEqual(element(6, Before), element(6, After)),
    ?assertEqual(true, ai_mustache_dev:stale(?MOD)).

t_reload(Dir) ->
    Path = write(Dir, <<"hi {{x}}">>),
    ok = build(Path, <<"hi {{x}}">>),
    ?assertEqual(<<"hi V">>, ?MOD:render(#{x => <<"V">>})),
    _ = write(Dir, <<"bye {{x}}!">>),
    ok = ai_mustache_dev:reload(?MOD),
    ?assertEqual(<<"bye V!">>, ?MOD:render(#{x => <<"V">>})),
    ?assertEqual(false, ai_mustache_dev:stale(?MOD)).

%% The whole reason -mustache_source carries opts: without them reload/1 could
%% not reproduce the original compilation.
t_reload_opts(Dir) ->
    Path = write(Dir, <<"{{x}}">>),
    ok = build(Path, <<"{{x}}">>),
    {ok, #{opts := Opts}} = ai_mustache_dev:source(?MOD),
    ?assert(maps:is_key(prefix, Opts)),
    ?assert(maps:is_key(line_map, Opts)),
    _ = write(Dir, <<"[{{x}}]">>),
    ok = ai_mustache_dev:reload(?MOD),
    ?assertEqual(<<"[V]">>, ?MOD:render(#{x => <<"V">>})).

t_broken(Dir) ->
    Path = write(Dir, <<"{{x}}">>),
    ok = build(Path, <<"{{x}}">>),
    _ = write(Dir, <<"{{#a}}oops">>),
    ?assertMatch({error, {_File, _Line, {unclosed_tag, [a]}}},
                 ai_mustache_dev:reload(?MOD)),
    %% The old module must still be usable after a failed reload.
    ?assertEqual(<<"V">>, ?MOD:render(#{x => <<"V">>})).

t_no_state(Dir) ->
    Path = write(Dir, <<"{{x}}">>),
    ok = build(Path, <<"{{x}}">>),
    Tabs0 = length(ets:all()),
    Procs0 = length(erlang:processes()),
    Pts0 = length(persistent_term:get()),
    _ = write(Dir, <<"[{{x}}]">>),
    ok = ai_mustache_dev:reload(?MOD),
    ok = ai_mustache_dev:reload(all),
    ?assertEqual(Tabs0, length(ets:all())),
    ?assertEqual(Procs0, length(erlang:processes())),
    ?assertEqual(Pts0, length(persistent_term:get())).

t_listing(Dir) ->
    Path = write(Dir, <<"{{x}}">>),
    ok = build(Path, <<"{{x}}">>),
    ?assert(lists:member(?MOD, ai_mustache_dev:template_modules())).

t_check(Dir) ->
    Path = write(Dir, <<"{{x}}">>),
    ok = build(Path, <<"{{x}}">>),
    ?assertEqual(ok, ai_mustache_dev:check()).
