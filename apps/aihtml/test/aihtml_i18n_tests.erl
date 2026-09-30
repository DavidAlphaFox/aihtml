-module(aihtml_i18n_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_element.hrl").

-behaviour(aihtml_element).
-behaviour(aihtml_action).
-export([render/1, action/4]).

-define(M, aihtml_i18n).

%% An element that renders the language it is rendered in.
-record(i18n_probe, {?AH_BASE(aihtml_i18n_tests)}).

render(#i18n_probe{}) -> ?M:locale().

action(probe, _Args, _Event, Ctx) ->
    aihtml_action:html(Ctx, {id, x}, #i18n_probe{}),
    aihtml_action:attr(Ctx, {id, x}, lang, aihtml_action:lang(Ctx)).

%% Catalogs configured for a test: #{Lang => JsonTerm}, written to files.
with_catalogs(Catalogs, Fun) ->
    Dir = filename:join(os:getenv("TMPDIR", "/tmp"),
                        "aihtml_i18n_tests_" ++ integer_to_list(erlang:unique_integer([positive]))),
    ok = filelib:ensure_path(Dir),
    Locales = maps:map(fun(Lang, Term) ->
                               F = filename:join(Dir, binary_to_list(Lang) ++ ".json"),
                               ok = file:write_file(F, json:encode(Term)),
                               F
                       end, Catalogs),
    application:set_env(aihtml, locales, Locales),
    ?M:reload(),
    try Fun()
    after
        application:unset_env(aihtml, locales),
        ?M:reload(),
        file:del_dir_r(Dir)
    end.

%%%===================================================================
%%% The current language
%%%===================================================================

default_is_en_test() ->
    ?assertEqual(<<"en">>, ?M:locale()),
    ?assertEqual([<<"en">>, <<"zh">>], ?M:locales()).

%% A bundled catalog translates exactly what en has: the same scopes, keys
%% and format settings, the same placeholders, and list values (month and
%% weekday names) of the same length.
bundled_catalogs_match_en_test() ->
    Dir = filename:join(code:priv_dir(aihtml), "i18n"),
    Read = fun(L) -> {ok, B} = file:read_file(filename:join(Dir, L ++ ".json")), json:decode(B) end,
    En = Read("en"),
    [begin
         C = Read(L),
         [?assert(is_map_key(K, maps:get(<<"format">>, En))) || K := _ <- maps:get(<<"format">>, C, #{})],
         [?assertEqual({L, K, length(maps:get(K, maps:get(<<"format">>, En)))}, {L, K, length(V)})
          || K := V <- maps:get(<<"format">>, C, #{}), is_list(V)],
         [?assertEqual({L, S, K, true}, {L, S, K, is_map_key(K, maps:get(S, maps:get(<<"messages">>, En), #{}))})
          || S := M <- maps:get(<<"messages">>, C, #{}), K := _ <- M],
         %% complete: nothing of en is missing
         ?assertEqual({L, lists:sort(maps:keys(maps:get(<<"format">>, En)))},
                      {L, lists:sort(maps:keys(maps:get(<<"format">>, C, #{})))}),
         [?assertEqual({L, S, K, true}, {L, S, K, is_map_key(K, maps:get(S, maps:get(<<"messages">>, C), #{}))})
          || S := M <- maps:get(<<"messages">>, En), K := _ <- M],
         %% the same placeholders ({0}, {n}, {start} ...)
         [?assertEqual({L, S, K, placeholders(V)}, {L, S, K, placeholders(maps:get(K, maps:get(S, maps:get(<<"messages">>, C))))})
          || S := M <- maps:get(<<"messages">>, En), K := V <- M]
     end || L <- ["zh"]].

placeholders(T) ->
    case re:run(T, <<"\\{\\w+\\}">>, [global, {capture, all, binary}]) of
        {match, Ms} -> lists:usort([X || [X] <- Ms]);
        nomatch -> []
    end.

normalize_test() ->
    ?assertEqual(<<"zh-cn">>, ?M:normalize(<<"zh-CN">>)),
    ?assertEqual(<<"zh-tw">>, ?M:normalize("zh_TW")),
    ?assertEqual(<<"zh">>, ?M:normalize(zh)),
    %% anything else (it may come from a browser) is the default language
    [?assertEqual(<<"en">>, ?M:normalize(Bad))
     || Bad <- [undefined, null, <<>>, <<"x">>, <<"zh CN">>, <<"../etc">>,
                binary:copy(<<"a">>, 100), 42]].

with_restores_test() ->
    ?assertEqual({<<"zh">>, <<"fr-ca">>, <<"zh">>},
                 ?M:with(zh, fun() ->
                                     Outer = ?M:locale(),
                                     Inner = ?M:with(<<"fr-CA">>, fun ?M:locale/0),
                                     {Outer, Inner, ?M:locale()}
                             end)),
    ?assertEqual(<<"en">>, ?M:locale()),
    %% also when the function raises
    ?assertError(boom, ?M:with(zh, fun() -> error(boom) end)),
    ?assertEqual(<<"en">>, ?M:locale()).

default_locale_setting_test() ->
    application:set_env(aihtml, default_locale, <<"zh_CN">>),
    try
        ?assertEqual(<<"zh-cn">>, ?M:locale()),
        ?assertEqual(<<"zh-cn">>, ?M:normalize(<<"not a tag">>))
    after
        application:unset_env(aihtml, default_locale)
    end,
    ?assertEqual(<<"en">>, ?M:locale()).

%%%===================================================================
%%% Texts
%%%===================================================================

bundled_en_test() ->
    ?assertEqual(<<"Close">>, ?M:text(common, close)),
    ?assertEqual(<<".">>, ?M:format(decimal)),
    ?assertEqual(0, ?M:format(first_day)),
    ?assertError({aihtml, {no_text, common, nope}}, ?M:text(common, nope)),
    ?assertError({aihtml, {no_text, nope, close}}, ?M:text(nope, close)),
    ?assertError({aihtml, {no_format, nope}}, ?M:format(nope)).

fallback_test() ->
    with_catalogs(
      #{<<"xx">> => #{<<"format">> => #{<<"decimal">> => <<",">>},
                      <<"messages">> => #{<<"common">> => #{<<"close">> => <<"XX close">>},
                                          <<"demo">> => #{<<"a">> => <<"XX a">>}}},
        <<"xx-yy">> => #{<<"messages">> => #{<<"demo">> => #{<<"b">> => <<"XX-YY b">>}}}},
      fun() ->
              ?assertEqual([<<"en">>, <<"xx">>, <<"xx-yy">>, <<"zh">>], ?M:locales()),
              ?M:with(<<"xx-YY">>, fun() ->
                  %% most specific first, then the language, then en
                  ?assertEqual(<<"XX-YY b">>, ?M:text(demo, b)),
                  ?assertEqual(<<"XX a">>, ?M:text(demo, a)),
                  ?assertEqual(<<"XX close">>, ?M:text(common, close)),
                  ?assertEqual(<<"Loading...">>, ?M:text(common, loading)),
                  ?assertEqual(<<",">>, ?M:format(decimal)),
                  ?assertEqual(<<",">>, ?M:format(group)),
                  ?assertEqual(#{a => <<"XX a">>, b => <<"XX-YY b">>}, ?M:texts(demo))
              end),
              %% a language without a catalog is all en
              ?M:with(fr, fun() -> ?assertEqual(<<"Close">>, ?M:text(common, close)) end)
      end).

override_bundled_test() ->
    with_catalogs(
      #{<<"en">> => #{<<"messages">> => #{<<"common">> => #{<<"close">> => <<"Dismiss">>}}}},
      fun() ->
              %% merged entry by entry into the bundled catalog
              ?assertEqual(<<"Dismiss">>, ?M:text(common, close)),
              ?assertEqual(<<"Loading...">>, ?M:text(common, loading)),
              ?assertEqual(<<".">>, ?M:format(decimal))
      end),
    ?assertEqual(<<"Close">>, ?M:text(common, close)).

placeholders_test() ->
    with_catalogs(
      #{<<"en">> => #{<<"messages">> => #{<<"demo">> => #{<<"page">> => <<"Page {0} / {1} ({0})">>}}}},
      fun() ->
              ?assertEqual(<<"Page 2 / 10 (2)">>, ?M:text(demo, page, [2, <<"10">>])),
              ?assertEqual(<<"Page {0} / {1} ({0})">>, ?M:text(demo, page))
      end).

labels_override_test() ->
    ?assertMatch(#{close := <<"X">>, loading := <<"Loading...">>},
                 ?M:texts(common, #{close => "X"})),
    %% a misspelt label fails instead of being ignored
    ?assertError({aihtml, {unknown_label, common, clsoe}}, ?M:texts(common, #{clsoe => <<"X">>})).

bad_configuration_test() ->
    application:set_env(aihtml, locales, #{<<"not a tag">> => "x.json"}),
    ?M:reload(),
    try ?assertError({aihtml, {bad_locale, <<"not a tag">>}}, ?M:locales())
    after application:unset_env(aihtml, locales), ?M:reload()
    end,
    application:set_env(aihtml, locales, #{<<"xx">> => "/nonexistent/xx.json"}),
    ?M:reload(),
    try ?assertError({aihtml, {i18n_catalog, "/nonexistent/xx.json", enoent}}, ?M:locales())
    after application:unset_env(aihtml, locales), ?M:reload()
    end.

%%%===================================================================
%%% Entry points
%%%===================================================================

%% the body renders in the page's language, and <html lang> keeps the
%% value as given
page_test() ->
    H = iolist_to_binary(aihtml:page(#i18n_probe{}, #{lang => <<"zh-CN">>})),
    ?assertMatch({_, _}, binary:match(H, <<"<html lang=\"zh-CN\"">>)),
    ?assertMatch({_, _}, binary:match(H, <<"zh-cn<script">>)),
    H2 = iolist_to_binary(aihtml:page(#i18n_probe{}, #{})),
    ?assertMatch({_, _}, binary:match(H2, <<"<html lang=\"en\"">>)),
    ?assertMatch({_, _}, binary:match(H2, <<"en<script">>)),
    ?assertEqual(<<"en">>, ?M:locale()).

%% A page carries the browser's texts when its language has any other than
%% English, and nothing otherwise.
page_client_texts_test() ->
    Script = fun(H) ->
                     case re:run(H, <<"<script type=\"application/json\" id=\"ah-labels\">(.*?)</script>">>,
                                 [{capture, all_but_first, binary}]) of
                         {match, [J]} -> json:decode(J);
                         nomatch -> none
                     end
             end,
    Page = fun(Lang) -> iolist_to_binary(aihtml:page(<<"x">>, #{lang => Lang})) end,
    ?assertEqual(none, Script(Page(<<"en">>))),
    #{<<"messages">> := #{<<"common">> := #{<<"close">> := <<"关闭"/utf8>>}},
      <<"format">> := #{<<"first_day">> := 1}} = Script(Page(<<"zh-CN">>)),
    with_catalogs(
      #{<<"xx">> => #{<<"messages">> => #{<<"common">> => #{<<"close">> => <<"</script> XX">>}},
                      <<"format">> => #{<<"am">> => <<"a.m.">>}}},
      fun() ->
              H = Page(<<"xx">>),
              %% "<" is written as \u003c, so the text cannot end the script
              ?assertEqual(nomatch, binary:match(H, <<"</script> XX">>)),
              #{<<"messages">> := #{<<"common">> := C} = M, <<"format">> := F} = Script(H),
              ?assertEqual(<<"</script> XX">>, maps:get(<<"close">>, C)),
              ?assertEqual(<<"Loading...">>, maps:get(<<"loading">>, C)),
              ?assertEqual(<<"a.m.">>, maps:get(<<"am">>, F)),
              ?assertEqual(<<"PM">>, maps:get(<<"pm">>, F)),
              %% only the scopes en.json lists under "client"
              ?assertNot(is_map_key(<<"pivotgrid">>, M)),
              ?assert(is_map_key(<<"node_graph">>, M))
      end).

render_uses_current_test() ->
    ?assertEqual(<<"en">>, aihtml:render_binary(#i18n_probe{})),
    ?assertEqual(<<"zh">>, ?M:with(zh, fun() -> aihtml:render_binary(#i18n_probe{}) end)).

%% an action renders in the language of its request (this module also
%% declares aihtml_element first: every -behaviour attribute counts)
action_test() ->
    {ok, Ref} = aihtml_action:verify(aihtml_action:token({?MODULE, probe, #{}})),
    Run = fun(Opts) ->
                  {ok, Ops} = aihtml_action:execute(Ref, #{}, Opts#{send => fun(_) -> ok end}),
                  [#{html := Html}, #{value := Lang}] = Ops,
                  {iolist_to_binary(Html), Lang}
          end,
    ?assertEqual({<<"zh-tw">>, <<"zh-tw">>}, Run(#{lang => <<"zh-TW">>})),
    %% no lang, or a bad one: the default
    ?assertEqual({<<"en">>, <<"en">>}, Run(#{})),
    ?assertEqual({<<"en">>, <<"en">>}, Run(#{lang => null})),
    ?assertEqual({<<"en">>, <<"en">>}, Run(#{lang => <<"<script>">>})),
    ?assertEqual(<<"en">>, ?M:locale()).

%% pushed HTML renders in the language given, not the caller's
render_ops_test() ->
    Probe = fun(Ctx) -> aihtml_action:html(Ctx, {id, x}, #i18n_probe{}) end,
    Html = fun([#{html := H}]) -> iolist_to_binary(H) end,
    ?assertEqual(<<"zh">>, Html(aihtml_action:render_ops(Probe, zh))),
    ?assertEqual(<<"en">>, Html(aihtml_action:render_ops(Probe))),
    ?M:with(zh, fun() ->
                        ?assertEqual(<<"zh">>, Html(aihtml_action:render_ops(Probe))),
                        ?assertEqual(<<"en">>, Html(aihtml_action:render_ops(Probe, undefined)))
                end).
