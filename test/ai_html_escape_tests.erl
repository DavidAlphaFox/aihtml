%%%-------------------------------------------------------------------
%%% Tests for the shared escape/format pair.
%%%
%%% The behaviour itself is already covered from the mustache side; what is
%%% asserted here is that it is the SAME implementation and that the error tag
%%% follows the caller rather than being hard-coded to one engine.
%%%-------------------------------------------------------------------
-module(ai_html_escape_tests).

-include_lib("eunit/include/eunit.hrl").

escape_set_test() ->
    ?assertEqual(<<"&amp;&lt;&gt;&quot;&#39;">>,
                 iolist_to_binary(ai_html_escape:escape(<<"&<>\"'">>))),
    %% Not escaped on purpose: escaping these corrupts URLs and prose.
    ?assertEqual(<<"/=`">>, iolist_to_binary(ai_html_escape:escape(<<"/=`">>))).

escape_returns_the_same_binary_when_clean_test() ->
    Bin = <<"nothing to do here">>,
    ?assert(erts_debug:same(Bin, ai_html_escape:escape(Bin))).

escape_is_utf8_safe_test() ->
    ?assertEqual(<<"中&amp;文"/utf8>>,
                 iolist_to_binary(ai_html_escape:escape(<<"中&文"/utf8>>))).

to_binary_numbers_test() ->
    ?assertEqual(<<"85">>,   ai_html_escape:to_binary(85)),
    ?assertEqual(<<"1.21">>, ai_html_escape:to_binary(1.21)),
    ?assertEqual(<<"1.1">>,  ai_html_escape:to_binary(1.1)).

to_binary_empties_test() ->
    ?assertEqual(<<>>, ai_html_escape:to_binary(undefined)),
    ?assertEqual(<<>>, ai_html_escape:to_binary(null)).

to_binary_binary_is_zero_copy_test() ->
    Bin = <<"x">>,
    ?assert(erts_debug:same(Bin, ai_html_escape:to_binary(Bin))).

%% The tag travels with the caller: a jinja template must not raise
%% {ai_mustache, ...}, and neither may be hard-coded here.
error_tag_follows_the_caller_test() ->
    ?assertError({ai_html, {not_renderable, _}},
                 ai_html_escape:to_binary(self())),
    ?assertError({ai_mustache, {not_renderable, _}},
                 ai_html_escape:to_binary(self(), ai_mustache)),
    ?assertError({ai_jinja, {not_renderable, _}},
                 ai_html_escape:to_binary(self(), ai_jinja)).

%% ai_mustache_rt must keep forwarding under its own tag: generated modules
%% call it by name and the exception shape is part of the contract.
mustache_rt_still_forwards_test() ->
    ?assertEqual(<<"&amp;">>, iolist_to_binary(ai_mustache_rt:escape(<<"&">>))),
    ?assertEqual(<<"1.21">>, ai_mustache_rt:to_binary(1.21)),
    ?assertError({ai_mustache, {not_renderable, _}},
                 ai_mustache_rt:to_binary(self())).

depends_only_on_otp_test() ->
    {ok, {_, [{imports, Imports}]}} =
        beam_lib:chunks(code:which(ai_html_escape), [imports]),
    ?assertEqual([], [M || {M, _, _} <- Imports,
                           lists:prefix("ai_mustache", atom_to_list(M))
                               orelse lists:prefix("ai_jinja", atom_to_list(M))]).
