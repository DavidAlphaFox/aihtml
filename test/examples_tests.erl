%%%-------------------------------------------------------------------
%%% @doc Golden test for the worked example under examples/.
%%%
%%% The example is the only place where partial indentation, the has/inverted
%%% extensions, the truthiness of 0 and the implicit iterator all appear in one
%%% template, so its rendered output is worth pinning down. It also compiles
%%% the templates the way the plugin does rather than through render_string/3,
%%% which keeps the cross-module partial calls in the picture.
%%% @end
%%%-------------------------------------------------------------------
-module(examples_tests).

-include_lib("eunit/include/eunit.hrl").

-define(VIEWS, "examples/views").
-define(NAMES, [<<"complex">>, <<"shared/item">>, <<"shared/user">>, <<"shared/level">>]).

%%%===================================================================
%%% Setup
%%%===================================================================

build_all() ->
    Base = #{views    => list_to_binary(?VIEWS),
             prefix   => <<"view_">>,
             suffix   => <<".mustache">>,
             line_map => true},
    [build(N, Base) || N <- ?NAMES],
    ok.

build(Name, Base) ->
    Path = filename:join(?VIEWS, binary_to_list(Name) ++ ".mustache"),
    {ok, Body} = file:read_file(Path),
    Mod = ai_mustache_ast:module_name(Name, Base),
    Opts = Base#{module => Mod,
                 source => list_to_binary(Path),
                 stamp  => ai_mustache_compiler:source_hash(Body, Base),
                 mtime  => 0},
    {ok, Ast} = ai_mustache_parser:parse(Body, Opts),
    {ok, Forms, _Deps} = ai_mustache_compiler:forms(Ast, Opts),
    {ok, Mod, Bin} = compile:forms(Forms, [return_errors, binary, debug_info]),
    _ = code:purge(Mod),
    {module, Mod} = code:load_binary(Mod, Path, Bin),
    Mod.

context() ->
    A = #{name => <<"red">>,   current => true,  url => <<"#Red">>,   link => true},
    B = #{name => <<"green">>, current => true,  url => <<"#Green">>, link => true},
    C = #{name => <<"blue">>,  current => false, url => <<"#Blue">>,  link => true},
    #{items => [A, B, C], header => <<"Colors">>, list => true, empty => false,
      count => 0, user => #{name => <<"David Gao">>},
      level => #{name => <<"VIP User">>},
      yield => [fun(N, F) ->
                        H = maps:get(header, F),
                        <<N/binary, " is ", H/binary>>
                end, <<"Test">>]}.

%%%===================================================================
%%% Tests
%%%===================================================================

examples_test_() ->
    {setup, fun build_all/0,
     [{"the example renders exactly", fun t_golden/0},
      {"partial dependencies are recorded", fun t_deps/0},
      {"render_iolist/1 flattens to the same bytes", fun t_iolist/0},
      {"the facade agrees with the generated module", fun t_facade/0}]}.

%% Points worth noting in this fixture:
%%   - blue has current => false, so its {{+ current}} block is absent
%%   - every line of the partial carries the caller's 4-space indent on top of
%%     its own 6, hence 10; the old implementation indented only the first line
%%   - </ul> is NOT indented: the partial's output must not leak a trailing
%%     indent onto the caller's next line
%%   - "#Red" survives intact: / and = are no longer escaped
%%   - count => 0 renders, because 0 is truthy, and {{.}} reaches the stack top
t_golden() ->
    Expected =
        <<"<h1>Colors</h1>\n"
          "<ul>\n"
          "          <li><strong>red</strong></li>\n"
          "          <li> \xe4\xb8\xad\xe6\x96\x87 </li>\n"
          "    \n"
          "          <li><a href=\"#Red\">red</a></li>\n"
          "            <li>\xe6\xad\xa4\xe5\xa4\x84\xe5\xba\x94\xe8\xaf\xa5\xe6\x98\xbe\xe7\xa4\xba</li>\n"
          "          <li><strong>green</strong></li>\n"
          "          <li> \xe4\xb8\xad\xe6\x96\x87 </li>\n"
          "    \n"
          "          <li><a href=\"#Green\">green</a></li>\n"
          "            <li>\xe6\xad\xa4\xe5\xa4\x84\xe5\xba\x94\xe8\xaf\xa5\xe6\x98\xbe\xe7\xa4\xba</li>\n"
          "    \n"
          "          <li><a href=\"#Blue\">blue</a></li>\n"
          "            <li>\xe6\xad\xa4\xe5\xa4\x84\xe5\xba\x94\xe8\xaf\xa5\xe6\x98\xbe\xe7\xa4\xba</li>\n"
          "</ul>\n"
          "    David Gao\n"
          "    VIP User\n"
          "list \xe6\x98\xaf true\xef\xbc\x9a\xe6\x89\xa7\xe8\xa1\x8c\xe4\xb8\x80\xe6\xac\xa1\xef\xbc\x8c"
          "\xe4\xb8\x8d\xe5\x8e\x8b\xe6\xa0\x88\xef\xbc\x8cColors \xe4\xbb\x8d\xe5\x8f\xaf\xe8\xa7\x81\n"
          "empty \xe6\x98\xaf false\xef\xbc\x8c\xe6\x89\x80\xe4\xbb\xa5 inverted section \xe6\x89\xa7\xe8\xa1\x8c\n"
          "count \xe6\x98\xaf 0\xef\xbc\x8c\xe4\xbd\x86\xe4\xbb\x8d\xe7\x84\xb6 truthy\xef\xbc\x8c0 "
          "\xe5\x8f\x96\xe5\x88\xb0\xe6\xa0\x88\xe9\xa1\xb6\n"
          "Test is Colors\n">>,
    ?assertEqual(Expected, view_complex:render(context())).

t_deps() ->
    ?assertEqual([view_shared_item, view_shared_level, view_shared_user],
                 lists:sort(view_complex:partials())),
    ?assertEqual([], view_shared_item:partials()).

t_iolist() ->
    Ctx = context(),
    ?assertEqual(view_complex:render(Ctx),
                 iolist_to_binary(view_complex:render_iolist(Ctx))).

t_facade() ->
    Ctx = context(),
    ?assertEqual(view_complex:render(Ctx), ai_mustache:render(view_complex, Ctx)),
    ?assertEqual(view_complex:render_iolist(Ctx),
                 ai_mustache:render_iolist(view_complex, Ctx)).
