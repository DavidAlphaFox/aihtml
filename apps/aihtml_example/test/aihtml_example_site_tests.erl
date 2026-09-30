%% The demo site: every component has demos, every demo renders, its
%% source can be shown, and every docs page and the home page render.
-module(aihtml_example_site_tests).

-include_lib("eunit/include/eunit.hrl").

site_test_() ->
    {setup,
     fun() -> {ok, Apps} = application:ensure_all_started(aihtml), Apps end,
     fun(Apps) -> [application:stop(A) || A <- lists:reverse(Apps)] end,
     [{"every component has demos", fun every_component_has_demos/0},
      {"every demo renders and shows its source", {timeout, 60, fun demos_render/0}},
      %% each renders every docs page (about 90), longer than eunit's 5 s
      {"every docs page renders", {timeout, 60, fun docs_pages_render/0}},
      {"the home page renders", fun home_renders/0},
      {"?lang= picks a language with a catalog, remembered in a cookie", fun lang_param/0},
      {"the language switch marks the page's language", fun lang_switch_marks_current/0},
      {"the API tab shows each component's record", {timeout, 60, fun records_shown/0}}]}.

components() ->
    [N || #{name := N} <- aihtml_example_site:components()].

lang_param() ->
    Req = fun(Qs, Cookie) ->
                  #{qs => Qs, headers => case Cookie of
                                             undefined -> #{};
                                             C -> #{<<"cookie">> => <<"aihtml_lang=", C/binary>>}
                                         end}
          end,
    Lang = fun(Qs, Cookie) -> element(1, aihtml_example_site:lang(Req(Qs, Cookie))) end,
    Remembered = fun(Qs, Cookie) ->
                         maps:is_key(<<"aihtml_lang">>,
                                     maps:get(resp_cookies, element(2, aihtml_example_site:lang(Req(Qs, Cookie))), #{}))
                 end,
    %% ?lang= switches and is remembered in a cookie
    ?assertEqual(<<"zh">>, Lang(<<"lang=zh">>, undefined)),
    ?assertEqual(<<"zh-cn">>, Lang(<<"lang=zh-CN">>, undefined)),
    ?assert(Remembered(<<"lang=zh">>, undefined)),
    ?assertEqual(<<"en">>, Lang(<<"lang=en">>, <<"zh">>)),
    %% later pages read the cookie, and do not set it again
    ?assertEqual(<<"zh">>, Lang(<<>>, <<"zh">>)),
    ?assertNot(Remembered(<<>>, <<"zh">>)),
    %% no catalog, malformed, or absent: the cookie, else the default
    ?assertEqual(<<"zh">>, Lang(<<"lang=fr">>, <<"zh">>)),
    [?assertEqual(<<"en">>, Lang(Q, C))
     || {Q, C} <- [{<<"lang=fr">>, undefined}, {<<"lang=%3Cscript%3E">>, undefined},
                   {<<"lang=">>, undefined}, {<<>>, undefined}, {<<"x=1">>, <<"xx">>}]].

%% the switch marks the page's language
lang_switch_marks_current() ->
    Html = fun(L) -> aihtml_i18n:with(L, fun() -> aihtml:render_binary(aihtml_example_site:lang_switch()) end) end,
    ?assertMatch({_, _}, binary:match(Html(<<"zh">>), <<"href=\"?lang=zh\" hreflang=\"zh\" lang=\"zh\" aria-current=\"true\"">>)),
    ?assertMatch({_, _}, binary:match(Html(<<"en">>), <<"href=\"?lang=en\" hreflang=\"en\" lang=\"en\" aria-current=\"true\"">>)).

every_component_has_demos() ->
    Missing = [N || N <- components(), aihtml_example_demos:for(N) =:= []],
    ?assertEqual([], Missing),
    %% and no demo entry names a component the catalog does not know
    Known = components(),
    ?assertEqual([], [C || #{component := C} <- aihtml_example_demos:all(),
                           not lists:member(C, Known)]).

demos_render() ->
    [begin
         Html = aihtml:render_binary(M:F()),
         ?assert(byte_size(Html) > 0),
         Src = aihtml_example_source:function(M, F),
         ?assertMatch({match, _}, re:run(Src, <<"^", (atom_to_binary(F))/binary, "\\(\\)">>)),
         ?assert(is_binary(aihtml:render_binary(aihtml_example_source:highlight(Src))))
     end || N <- components(), {_, M, F} <- aihtml_example_demos:for(N)].

docs_pages_render() ->
    [?assertMatch(<<_/binary>>, aihtml:render_binary(aihtml_example_docs:render(N)))
     || N <- components()].

home_renders() ->
    Html = aihtml:render_binary(aihtml_example_home:render()),
    [?assertMatch({_, _}, binary:match(Html, <<"/components/", (atom_to_binary(N))/binary, "\"">>))
     || N <- components()].

%% Components without an element record: the theme switcher and toast
%% (an action, not an element).
-define(NO_RECORD, [theme_switcher, toast]).

records_shown() ->
    [case lists:member(N, ?NO_RECORD) of
         true ->
             ?assertEqual(undefined, aihtml_example_records:record(N));
         false ->
             #{record := Rec, header := <<"aihtml_", _/binary>>, doc := Doc,
               fields := Fields} = aihtml_example_records:record(N),
             ?assertNotEqual(<<>>, Doc),
             ?assertNotEqual([], Fields),
             [?assertNot(lists:member(F, aihtml_example_records:base_fields()))
              || #{name := F} <- Fields],
             Html = aihtml:render_binary(aihtml_example_docs:render(N)),
             ?assertMatch({_, _}, binary:match(Html, <<"#", (atom_to_binary(Rec))/binary, "{}">>))
     end || N <- components()].
