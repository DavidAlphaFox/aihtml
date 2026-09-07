%%%-------------------------------------------------------------------
%%% @doc Tests for the canonical UTF-8 binary representation.
%%%
%%% The point of ai_mustache_text is that no module downstream has to ask what
%%% shape of text it is holding, and that the same logical options always hash
%%% the same regardless of how the caller spelled them. Both are asserted here.
%%% @end
%%%-------------------------------------------------------------------
-module(ai_mustache_text_tests).

-include_lib("eunit/include/eunit.hrl").

-define(CJK, <<228, 184, 173, 230, 150, 135>>).          % 中文, UTF-8
-define(LATIN1, <<"caf", 233>>).                          % "café" in latin-1

%%%===================================================================
%%% template/1
%%%===================================================================

template_accepts_binary_test() ->
    ?assertEqual({ok, <<"a{{x}}b">>}, ai_mustache_text:template(<<"a{{x}}b">>)).

template_accepts_string_test() ->
    ?assertEqual({ok, <<"abc">>}, ai_mustache_text:template("abc")).

template_accepts_iolist_test() ->
    ?assertEqual({ok, <<"abc">>}, ai_mustache_text:template([<<"a">>, "b", [<<"c">>]])).

template_keeps_utf8_test() ->
    ?assertEqual({ok, ?CJK}, ai_mustache_text:template(?CJK)).

%% A template that is not UTF-8 is refused outright. Passing the bytes through
%% would surface much later as mangled output from a generated module, which
%% is far harder to diagnose than a rejection at the door.
template_rejects_invalid_utf8_test() ->
    ?assertEqual({error, {invalid_utf8, 3}}, ai_mustache_text:template(?LATIN1)).

template_reports_offset_test() ->
    ?assertMatch({error, {invalid_utf8, 0}},
                 ai_mustache_text:template(<<255, 254>>)).

%%%===================================================================
%%% path/1 and opts/1
%%%===================================================================

%% Paths are normalised but never rejected: they come from the file system and
%% from rebar3, which on a non-UTF-8 filesystem may hand back raw bytes that
%% still name a real file.
path_normalises_test_() ->
    [?_assertEqual(<<"views">>, ai_mustache_text:path("views")),
     ?_assertEqual(<<"views">>, ai_mustache_text:path(<<"views">>)),
     ?_assertEqual(?CJK, ai_mustache_text:path(?CJK)),
     ?_assertEqual(?LATIN1, ai_mustache_text:path(?LATIN1))].

opts_normalises_every_path_key_test() ->
    In = #{module => m, source => "views/a.mustache", views => "views",
           prefix => "view_", suffix => ".mustache", line_map => true},
    Out = ai_mustache_text:opts(In),
    ?assertEqual(<<"views/a.mustache">>, maps:get(source, Out)),
    ?assertEqual(<<"views">>, maps:get(views, Out)),
    ?assertEqual(<<"view_">>, maps:get(prefix, Out)),
    ?assertEqual(<<".mustache">>, maps:get(suffix, Out)),
    %% Non-path values are untouched.
    ?assertEqual(m, maps:get(module, Out)),
    ?assertEqual(true, maps:get(line_map, Out)).

opts_leaves_absent_keys_absent_test() ->
    ?assertEqual(#{module => m}, ai_mustache_text:opts(#{module => m})).

source_defaults_test_() ->
    [?_assertEqual(<<"nofile">>, ai_mustache_text:source(#{})),
     ?_assertEqual(<<"x.mustache">>, ai_mustache_text:source(#{source => "x.mustache"}))].

to_list_round_trip_test_() ->
    [?_assertEqual("abc", ai_mustache_text:to_list(<<"abc">>)),
     ?_assertEqual(?CJK, unicode:characters_to_binary(
                           ai_mustache_text:to_list(?CJK), utf8))].

%%%===================================================================
%%% The representation is uniform end to end
%%%===================================================================

%% A string and a binary template must produce the same AST, and a string and
%% a binary path must produce the same module name.
string_and_binary_agree_test() ->
    Opts = #{source => <<"t">>},
    {ok, A} = ai_mustache_parser:parse(<<"a{{x}}b">>, Opts),
    {ok, B} = ai_mustache_parser:parse("a{{x}}b", Opts),
    ?assertEqual(A, B).

module_name_accepts_string_prefix_test() ->
    %% ai_mustache_ast:module_name/2 concatenates binaries, so a string prefix
    %% used to crash. opts/1 is what makes it work.
    Opts = ai_mustache_text:opts(#{prefix => "view_"}),
    ?assertEqual(view_shared_item,
                 ai_mustache_ast:module_name(<<"shared/item">>, Opts)).

%% The build stamp must not depend on how a path was spelled, or the plugin
%% and ai_mustache_dev would disagree about staleness the moment one of them
%% passed a string.
stamp_is_representation_independent_test() ->
    Body = <<"{{x}}">>,
    A = ai_mustache_compiler:source_hash(Body, #{views => "views", prefix => "view_"}),
    B = ai_mustache_compiler:source_hash(Body, #{views => <<"views">>,
                                                 prefix => <<"view_">>}),
    ?assertEqual(A, B).

stamp_still_changes_with_options_test() ->
    Body = <<"{{x}}">>,
    A = ai_mustache_compiler:source_hash(Body, #{prefix => <<"view_">>}),
    B = ai_mustache_compiler:source_hash(Body, #{prefix => <<"tpl_">>}),
    ?assertNotEqual(A, B).

%% An invalid-UTF-8 template is reported through the ordinary error channel,
%% with the source path the caller supplied.
invalid_utf8_template_is_a_normal_error_test() ->
    ?assertEqual({error, {<<"views/bad.mustache">>, 1, {invalid_utf8, 3}}},
                 ai_mustache_parser:parse(?LATIN1, #{source => "views/bad.mustache"})).

utf8_template_renders_intact_test() ->
    Tpl = <<"[", (?CJK)/binary, "{{x}}]">>,
    ?assertEqual(<<"[", (?CJK)/binary, "v]">>,
                 ai_mustache:render_string(Tpl, #{x => <<"v">>})).
