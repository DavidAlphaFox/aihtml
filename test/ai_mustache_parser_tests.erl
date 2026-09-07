%%%-------------------------------------------------------------------
%%% @doc Unit tests for ai_mustache_parser.
%%%
%%% The parser turns a token stream into the raw AST: no merging, no pruning,
%%% no partial resolution (those are ai_mustache_ast's three passes, tested in
%%% ai_mustache_ast_tests). So the assertions here are about node shape and
%%% about error diagnostics -- in particular that every failure comes back as
%%% {error, {File, Line, Reason}} rather than as an exception, and that the
%%% Line points at the tag that is actually wrong.
%%%
%%% See tasks/T08.md and tasks/T10.md.
%%% @end
%%%-------------------------------------------------------------------
-module(ai_mustache_parser_tests).

-include_lib("eunit/include/eunit.hrl").
-include("ai_mustache.hrl").

%% Placeholder loc() used by strip_loc/1 below.
-define(L, {0, 0}).

%%%===================================================================
%%% Helpers
%%%===================================================================

%% Parse and unwrap, turning a parse failure into a readable test error.
ast(Input) ->
    case ai_mustache_parser:parse(Input) of
        {ok, Ns}       -> Ns;
        {error, _} = E -> erlang:error({parse_failed, Input, E})
    end.

%% Replace every loc() with {0, 0} so a case can assert on shape alone.
strip_loc(Ns) when is_list(Ns) -> [strip_one(N) || N <- Ns].

strip_one({text,     _, B})       -> {text, ?L, B};
strip_one({var,      _, K, Mode}) -> {var, ?L, K, Mode};
strip_one({lambda,   _, K})       -> {lambda, ?L, K};
strip_one({partial,  _, N, I})    -> {partial, ?L, N, I};
strip_one({section,  _, K, B})    -> {section, ?L, K, strip_loc(B)};
strip_one({inverted, _, K, B})    -> {inverted, ?L, K, strip_loc(B)};
strip_one({has,      _, K, B, P}) -> {has, ?L, K, strip_loc(B), P};
strip_one({ext,      _, M, K, B}) -> {ext, ?L, M, K, strip_loc(B)}.

%% Assert the parser reports rather than raises, and hand back the triple.
parse_error(Input) ->
    Got = try
              ai_mustache_parser:parse(Input)
          catch
              Cl:Rs:Stack ->
                  erlang:error({parser_raised, Input, Cl, Rs, Stack})
          end,
    case Got of
        {error, {File, Line, Reason}} -> {File, Line, Reason};
        {ok, Ns}                      -> erlang:error({unexpected_ok, Input, Ns})
    end.

%%%===================================================================
%%% Token -> node mapping, one case per row of the table
%%%===================================================================
%% {Title, Template, ExpectedNodes}

node_shape_cases() ->
    [{"marker none -> {var, _, Keys, escape}",
      <<"{{x}}">>, [{var, ?L, [x], escape}]},
     {"marker ${ -> {var, _, Keys, raw} (triple form)",
      <<"{{{x}}}">>, [{var, ?L, [x], raw}]},
     {"marker ${ -> {var, _, Keys, raw} (ampersand form)",
      <<"{{&x}}">>, [{var, ?L, [x], raw}]},
     {"marker $# -> {section, _, Keys, Body}",
      <<"{{#a}}b{{/a}}">>,
      [{section, ?L, [a], [{text, ?L, <<"b">>}]}]},
     {"marker $^ -> {inverted, _, Keys, Body}",
      <<"{{^a}}b{{/a}}">>,
      [{inverted, ?L, [a], [{text, ?L, <<"b">>}]}]},
     {"marker $+ -> {has, _, Keys, Body, true}",
      <<"{{+a}}b{{/a}}">>,
      [{has, ?L, [a], [{text, ?L, <<"b">>}], true}]},
     {"marker $- -> {has, _, Keys, Body, false}",
      <<"{{-a}}b{{/a}}">>,
      [{has, ?L, [a], [{text, ?L, <<"b">>}], false}]},
     {"marker $* -> {lambda, _, Keys}",
      <<"{{*f}}">>, [{lambda, ?L, [f]}]},
     {"marker $> -> {partial, _, Name, Indent}",
      <<"{{>p}}">>, [{partial, ?L, <<"p">>, <<>>}]},
     {"text token -> {text, _, Bin}",
      <<"plain">>, [{text, ?L, <<"plain">>}]}].

node_shape_test_() ->
    [{Title, ?_assertEqual(Expected, strip_loc(ast(Input)))}
     || {Title, Input, Expected} <- node_shape_cases()].

%% Dotted names become key paths on every node type that carries keys.
dotted_keys_test() ->
    ?assertEqual([{var, ?L, [a, b], escape}], strip_loc(ast(<<"{{a.b}}">>))),
    ?assertEqual([{var, ?L, [a, b], raw}], strip_loc(ast(<<"{{{a.b}}}">>))),
    ?assertEqual([{lambda, ?L, [a, b]}], strip_loc(ast(<<"{{*a.b}}">>))),
    ?assertEqual([{section, ?L, [a, b], []}],
                 strip_loc(ast(<<"{{#a.b}}{{/a.b}}">>))).

%% A partial's Indent comes straight from the standalone pass in the scanner.
partial_indent_survives_the_parser_test() ->
    ?assertEqual([{partial, ?L, <<"p">>, <<"  ">>}],
                 strip_loc(ast(<<"  {{> p}}\n">>))).

%%%===================================================================
%%% parse/2 on a token list
%%%===================================================================
%% parse/2 accepts a token list as well as a binary, so the parser can be
%% driven without the scanner. This is the interface the extension tests and
%% the compiler use.

parse_accepts_a_token_list_test() ->
    Tokens = [{text, {1, 1}, <<"a">>},
              {tag,  {1, 2}, $#, <<"s">>, <<>>},
              {tag,  {1, 8}, none, <<"x">>, <<>>},
              {tag,  {1, 13}, $/, <<"s">>, <<>>}],
    ?assertEqual({ok, [{text, {1, 1}, <<"a">>},
                       {section, {1, 2}, [s], [{var, {1, 8}, [x], escape}]}]},
                 ai_mustache_parser:parse(Tokens, #{})).

parse_1_accepts_a_token_list_test() ->
    ?assertEqual({ok, [{var, {3, 7}, [x], escape}]},
                 ai_mustache_parser:parse([{tag, {3, 7}, none, <<"x">>, <<>>}])).

parse_empty_token_list_test() ->
    ?assertEqual({ok, []}, ai_mustache_parser:parse([])),
    ?assertEqual({ok, []}, ai_mustache_parser:parse(<<>>)).

%% Locations from the token list are carried through untouched.
parse_preserves_token_locations_test() ->
    ?assertEqual({ok, [{section, {2, 5}, [a], [{text, {3, 1}, <<"b">>}]}]},
                 ai_mustache_parser:parse([{tag,  {2, 5}, $#, <<"a">>, <<>>},
                                           {text, {3, 1}, <<"b">>},
                                           {tag,  {4, 1}, $/, <<"a">>, <<>>}])).

%%%===================================================================
%%% Nesting
%%%===================================================================

nested_sections_test() ->
    ?assertEqual([{section, ?L, [a],
                   [{section, ?L, [b],
                     [{var, ?L, [x], escape}]}]}],
                 strip_loc(ast(<<"{{#a}}{{#b}}{{x}}{{/b}}{{/a}}">>))).

nested_mixed_blocks_test() ->
    ?assertEqual([{section, ?L, [a],
                   [{inverted, ?L, [b],
                     [{has, ?L, [c], [{text, ?L, <<"z">>}], true}]}]}],
                 strip_loc(ast(<<"{{#a}}{{^b}}{{+c}}z{{/c}}{{/b}}{{/a}}">>))).

siblings_at_top_level_test() ->
    ?assertEqual([{section, ?L, [a], []},
                  {text, ?L, <<"m">>},
                  {section, ?L, [b], []}],
                 strip_loc(ast(<<"{{#a}}{{/a}}m{{#b}}{{/b}}">>))).

%% Closing compares key paths, not the raw binary, so whitespace inside the
%% close tag is irrelevant.
close_compares_keys_not_binaries_test() ->
    ?assertEqual([{section, ?L, [a, b], [{text, ?L, <<"x">>}]}],
                 strip_loc(ast(<<"{{#a.b}}x{{/ a.b }}">>))),
    ?assertEqual([{section, ?L, [a], [{text, ?L, <<"x">>}]}],
                 strip_loc(ast(<<"{{# a }}x{{/\ta\t}}">>))),
    %% consecutive dots are filtered on both sides, so these still match
    ?assertEqual([{section, ?L, [a, b], []}],
                 strip_loc(ast(<<"{{#a..b}}{{/a.b}}">>))).

%%%===================================================================
%%% Errors
%%%===================================================================
%% {Title, Template, ExpectedLine, ExpectedReason}

error_cases() ->
    [{"unclosed section reports the line of the OPENING tag",
      <<"one\ntwo{{#a}}x\nthree\nfour\n">>, 2, {unclosed_tag, [a]}},
     {"unclosed section on a single line",
      <<"{{#a}}x">>, 1, {unclosed_tag, [a]}},
     {"unclosed inverted section",
      <<"\n\n{{^a}}x">>, 3, {unclosed_tag, [a]}},
     {"unclosed has section",
      <<"\n{{+a}}x">>, 2, {unclosed_tag, [a]}},
     {"the innermost unclosed block is reported",
      <<"{{#a}}\n{{#b}}\n{{#c}}\n">>, 3, {unclosed_tag, [c]}},
     {"mismatched close reports the line of the CLOSE tag",
      <<"{{#a}}x{{/b}}">>, 1, {mismatched_close, [a], [b]}},
     {"mismatched close on a later line",
      <<"z\n{{#a}}x\ny{{/b}}">>, 3, {mismatched_close, [a], [b]}},
     {"a stray close tag at top level",
      <<"{{/a}}">>, 1, {mismatched_close, [], [a]}},
     {"a stray close tag at top level on a later line",
      <<"a\nb\n{{/a}}">>, 3, {mismatched_close, [], [a]}},
     {"an empty partial name is rejected",
      <<"{{> }}">>, 1, {partial_not_found, <<>>}},
     {"a partial path containing .. is rejected",
      <<"{{> ../x}}">>, 1, {partial_not_found, <<"../x">>}},
     {"a partial path with .. in the middle is rejected",
      <<"\n{{> a/../b}}">>, 2, {partial_not_found, <<"a/../b">>}},
     %% Scanner errors travel through parse/1 unchanged.
     {"an unterminated tag comes back from the scanner",
      <<"{{x">>, 1, {unclosed_tag, []}},
     {"an unregistered marker comes back from the scanner",
      <<"{{@k}}">>, 1, {unknown_marker, $@}},
     {"an invalid delimiter switch comes back from the scanner",
      <<"{{=a b c=}}">>, 1, {invalid_delimiter, <<"a b c=">>}}].

error_test_() ->
    [{Title, ?_assertEqual({<<"nofile">>, Line, Reason}, parse_error(Input))}
     || {Title, Input, Line, Reason} <- error_cases()].

errors_are_never_raised_test() ->
    lists:foreach(
      fun({_Title, Input, _Line, _Reason}) ->
              ?assertMatch({error, {_, _, _}}, ai_mustache_parser:parse(Input))
      end, error_cases()).

error_file_comes_from_the_source_option_test() ->
    ?assertMatch({error, {<<"views/x.mustache">>, 1, {unclosed_tag, [a]}}},
                 ai_mustache_parser:parse(<<"{{#a}}">>,
                                          #{source => <<"views/x.mustache">>})),
    ?assertMatch({error, {<<"views/x.mustache">>, 1, {partial_not_found, <<>>}}},
                 ai_mustache_parser:parse(<<"{{>}}">>,
                                          #{source => "views/x.mustache"})).

%%%===================================================================
%%% Regressions
%%%===================================================================

%% B4 regression: a partial name is a file path, not a key path. The old
%% parser ran keys/1 over it, so `{{> layout.default}}' split into
%% [layout, default] and raised function_clause. The binary must survive
%% verbatim, dots and slashes included.
b4_regression_partial_name_is_not_a_key_path_test() ->
    ?assertMatch({ok, [{partial, _, <<"layout.default">>, _}]},
                 ai_mustache_parser:parse(<<"{{> layout.default}}">>)),
    ?assertMatch({ok, [{partial, _, <<"shared/item">>, _}]},
                 ai_mustache_parser:parse(<<"{{> shared/item}}">>)),
    ?assertMatch({ok, [{partial, _, <<"a/b/c.d-e">>, _}]},
                 ai_mustache_parser:parse(<<"{{> a/b/c.d-e}}">>)).

%% B4 regression: the same inside a section, where the old code path was hit
%% by the recursive descent rather than at the top level.
b4_regression_partial_inside_a_section_test() ->
    ?assertMatch({ok, [{section, _, [s], [{partial, _, <<"layout.default">>, _}]}]},
                 ai_mustache_parser:parse(<<"{{#s}}{{> layout.default}}{{/s}}">>)).

%% B5 regression: {{.}} is the implicit iterator and must produce the ['.']
%% sentinel that ai_mustache_rt:lookup/2 recognises, not an empty key list.
b5_regression_implicit_iterator_test() ->
    ?assertEqual({ok, [{var, {1, 1}, [?AI_MUSTACHE_DOT], escape}]},
                 ai_mustache_parser:parse(<<"{{.}}">>)),
    ?assertEqual({ok, [{var, {1, 1}, [?AI_MUSTACHE_DOT], raw}]},
                 ai_mustache_parser:parse(<<"{{{.}}}">>)),
    ?assertEqual({ok, [{var, {1, 1}, [?AI_MUSTACHE_DOT], raw}]},
                 ai_mustache_parser:parse(<<"{{&.}}">>)).

%% B5 regression: and the same inside a section body.
b5_regression_implicit_iterator_inside_a_section_test() ->
    ?assertEqual([{section, ?L, [a], [{var, ?L, [?AI_MUSTACHE_DOT], escape}]}],
                 strip_loc(ast(<<"{{#a}}{{.}}{{/a}}">>))).

%%%===================================================================
%%% keys/1
%%%===================================================================
%% {Title, Content, ExpectedKeys}

keys_cases() ->
    [{"a dotted name splits into segments", <<"a.b">>,     [a, b]},
     {"a three-segment name",               <<"a.b.c">>,   [a, b, c]},
     {"a single name",                      <<"a">>,       [a]},
     {"a lone dot is the implicit iterator", <<".">>,      [?AI_MUSTACHE_DOT]},
     {"empty content is the implicit iterator", <<>>,      [?AI_MUSTACHE_DOT]},
     {"inner whitespace is stripped",       <<" a . b ">>, [a, b]},
     {"tabs and newlines are stripped",     <<"a\t.\nb">>, [a, b]},
     {"consecutive dots produce no empty segment", <<"a..b">>, [a, b]},
     {"a leading dot produces no empty segment",   <<".a">>,   [a]},
     {"a trailing dot produces no empty segment",  <<"a.">>,   [a]},
     {"whitespace-only content is the implicit iterator", <<"  ">>,
      [?AI_MUSTACHE_DOT]}].

keys_test_() ->
    [{Title, ?_assertEqual(Expected, ai_mustache_parser:keys(Input))}
     || {Title, Input, Expected} <- keys_cases()].

%%%===================================================================
%%% partial_name/1
%%%===================================================================
%% {Title, Content, Expected}

partial_name_cases() ->
    [{"a plain name",           <<"index">>,   {ok, <<"index">>}},
     {"a path keeps its slash", <<"shared/item">>, {ok, <<"shared/item">>}},
     {"a name keeps its dots",  <<"layout.default">>, {ok, <<"layout.default">>}},
     {"outer whitespace is trimmed", <<"  shared/item  ">>,
      {ok, <<"shared/item">>}},
     {"a tab-padded name is trimmed", <<"\tindex\t">>, {ok, <<"index">>}},
     {"an empty name is an error", <<>>, {error, {partial_not_found, <<>>}}},
     {"a whitespace-only name is an error", <<"   ">>,
      {error, {partial_not_found, <<>>}}},
     {"a leading .. is an error", <<"../x">>,
      {error, {partial_not_found, <<"../x">>}}},
     {"a .. in the middle is an error", <<"a/../b">>,
      {error, {partial_not_found, <<"a/../b">>}}},
     {"a trailing .. is an error", <<"a/..">>,
      {error, {partial_not_found, <<"a/..">>}}},
     %% Only a whole ".." segment is rejected; ".." inside a name is fine.
     {"a name merely containing .. is allowed", <<"a..b">>,
      {ok, <<"a..b">>}}].

partial_name_test_() ->
    [{Title, ?_assertEqual(Expected, ai_mustache_parser:partial_name(Input))}
     || {Title, Input, Expected} <- partial_name_cases()].
