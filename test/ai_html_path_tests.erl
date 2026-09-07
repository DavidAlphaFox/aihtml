%%%-------------------------------------------------------------------
%%% Tests for the shared path resolution and attribute scanning.
%%%
%%% The property that matters is that the attribute name is a parameter: the
%%% two engines must share one search order and one scanner, or the plugin
%%% will touch one file while a parse_transform reads another.
%%%-------------------------------------------------------------------
-module(ai_html_path_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% resolve/2
%%%===================================================================

resolve_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(Dir) ->
             [{"absolute path used as given",
               ?_assertEqual({ok, filename:join(Dir, "a.tpl")},
                             ai_html_path:resolve(filename:join(Dir, "a.tpl"), []))},
              {"absolute path that is not there",
               ?_assertMatch({error, {not_found, _}},
                             ai_html_path:resolve(filename:join(Dir, "no.tpl"), []))},
              {"first directory that has it wins",
               ?_assertEqual({ok, filename:join(Dir, "a.tpl")},
                             ai_html_path:resolve("a.tpl", ["/nowhere", Dir]))},
              {"the error lists every directory actually tried",
               ?_assertEqual({error, {not_found, ["/nowhere", Dir]}},
                             ai_html_path:resolve("no.tpl", ["/nowhere", Dir]))},
              {"empty and undefined entries are skipped, not searched",
               ?_assertEqual({error, {not_found, [Dir]}},
                             ai_html_path:resolve("no.tpl", ["", undefined, Dir]))}]
     end}.

%%%===================================================================
%%% template_spec/1
%%%===================================================================

template_spec_test() ->
    ?assertEqual({ok, {index, "views/index.mustache"}},
                 ai_html_path:template_spec({index, "views/index.mustache"})),
    %% Bare path: the name comes from the basename.
    ?assertEqual({ok, {index, "views/index.j2"}},
                 ai_html_path:template_spec("views/index.j2")),
    ?assertEqual({ok, {my_page, "a/My-Page.j2"}},
                 ai_html_path:template_spec("a/My-Page.j2")),
    ?assertEqual({ok, {index, "views/index.j2"}},
                 ai_html_path:template_spec(<<"views/index.j2">>)),
    ?assertEqual(error, ai_html_path:template_spec("")),
    ?assertEqual(error, ai_html_path:template_spec(42)).

%%%===================================================================
%%% scan/2 -- one implementation, two attribute names
%%%===================================================================

scan_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(Dir) ->
             File = filename:join(Dir, "m.erl"),
             ok = file:write_file(File,
                                  "-module(m).\n"
                                  "-mustache_template({a, \"v/a.mustache\"}).\n"
                                  "-jinja_template({b, \"v/b.j2\"}).\n"
                                  "-jinja_template(\"v/c.j2\").\n"
                                  "f() -> ok.\n"),
             [{"only the mustache attributes",
               ?_assertEqual({ok, [{a, "v/a.mustache"}]},
                             ai_html_path:scan(File, mustache_template))},
              {"only the jinja attributes, both shapes",
               ?_assertEqual({ok, [{b, "v/b.j2"}, {c, "v/c.j2"}]},
                             ai_html_path:scan(File, jinja_template))},
              {"an attribute nobody declared",
               ?_assertEqual({ok, []}, ai_html_path:scan(File, other_template))},
              {"a file that is not there",
               ?_assertMatch({error, _},
                             ai_html_path:scan(filename:join(Dir, "no.erl"),
                                               jinja_template))}]
     end}.

%%%===================================================================
%%% The deprecated shell must keep working
%%%===================================================================

mustache_shell_forwards_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(Dir) ->
             File = filename:join(Dir, "m.erl"),
             ok = file:write_file(File,
                                  "-module(m).\n"
                                  "-mustache_template({a, \"v/a.mustache\"}).\n"),
             [?_assertEqual({ok, [{a, "v/a.mustache"}]},
                            ai_mustache_path:scan(File)),
              ?_assertEqual({ok, {index, "views/index.mustache"}},
                            ai_mustache_path:template_spec("views/index.mustache"))]
     end}.

text_shell_forwards_test() ->
    ?assertEqual({ok, <<"x">>}, ai_mustache_text:template("x")),
    ?assertEqual(<<"x">>, ai_mustache_text:path("x")),
    ?assertEqual(<<"nofile">>, ai_mustache_text:source(#{})),
    ?assertEqual(#{source => <<"a">>}, ai_mustache_text:opts(#{source => "a"})),
    ?assertEqual("x", ai_mustache_text:to_list(<<"x">>)).

%%%===================================================================

setup() ->
    Dir = filename:join(["/tmp", "aihtml_path_tests",
                         integer_to_list(erlang:unique_integer([positive]))]),
    ok = filelib:ensure_path(Dir),
    ok = file:write_file(filename:join(Dir, "a.tpl"), <<>>),
    Dir.

cleanup(Dir) -> _ = file:del_dir_r(Dir), ok.
