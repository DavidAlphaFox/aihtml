%%%-------------------------------------------------------------------
%%% Tests for the expression lexer.
%%%
%%% The rendering behaviour of every literal form is already pinned by the
%%% `lexer' fixture group; what is asserted here is the part the fixtures
%%% cannot see -- token shapes, positions, and where the tag was decided to
%%% end.
%%%-------------------------------------------------------------------
-module(ai_jinja_lexer_tests).

-include_lib("eunit/include/eunit.hrl").

lex(Bin) -> ai_jinja_lexer:tokens(Bin, {1, 1}, expr_end).
lex(Bin, Stop) -> ai_jinja_lexer:tokens(Bin, {1, 1}, Stop).

toks(Bin) ->
    {ok, T, _Trim, _Rest, _Loc} = lex(<<Bin/binary, " }}">>),
    T.

%%%===================================================================
%%% Where the tag ends
%%%===================================================================

stops_at_delimiter_test() ->
    ?assertMatch({ok, [{int, _, 1}], none, <<"tail">>, _}, lex(<<"1 }}tail">>)).

stop_inside_a_string_is_not_a_stop_test() ->
    ?assertMatch({ok, [{str, _, <<"}}">>}], none, <<>>, _}, lex(<<"\"}}\" }}">>)),
    ?assertMatch({ok, [{str, _, <<"%}">>}], none, <<>>, _},
                 lex(<<"\"%}\" %}">>, stmt_end)).

stop_needs_bracket_depth_zero_test() ->
    {ok, T, none, <<>>, _} = lex(<<"{\"a\": 1} }}">>),
    ?assertMatch([{open, _, ${}, {str, _, <<"a">>}, {op, _, ':'},
                   {int, _, 1}, {close, _, $}}], T).

whitespace_markers_are_reported_not_lexed_test() ->
    ?assertMatch({ok, [{int, _, 1}], minus, <<>>, _}, lex(<<"1 -}}">>)),
    ?assertMatch({ok, [{int, _, 1}], plus, <<>>, _}, lex(<<"1 +}}">>)),
    ?assertMatch({ok, [{int, _, 1}], minus, <<>>, _}, lex(<<"1 -%}">>, stmt_end)).

%% A minus that is not against the delimiter is still subtraction.
minus_is_still_an_operator_test() ->
    ?assertMatch([{int, _, 1}, {op, _, '-'}, {int, _, 2}], toks(<<"1 - 2">>)).

unterminated_tag_test() ->
    ?assertMatch({error, _, {unclosed_block, expr, _}}, lex(<<"1 + 2">>)),
    ?assertMatch({error, _, {unclosed_block, stmt, _}}, lex(<<"if x">>, stmt_end)).

stray_close_bracket_test() ->
    ?assertMatch({error, _, {unexpected_token, <<")">>}}, lex(<<"1) }}">>)).

%%%===================================================================
%%% Numbers
%%%===================================================================

number_forms_test() ->
    ?assertMatch([{int, _, 123}],   toks(<<"123">>)),
    ?assertMatch([{int, _, 1000}],  toks(<<"1_000">>)),
    ?assertMatch([{int, _, 31}],    toks(<<"0x1f">>)),
    ?assertMatch([{int, _, 15}],    toks(<<"0o17">>)),
    ?assertMatch([{int, _, 10}],    toks(<<"0b1010">>)),
    ?assertMatch([{float, _, 1.5}], toks(<<"1.5">>)),
    ?assertMatch([{float, _, 1000.0}], toks(<<"1e3">>)),
    ?assertMatch([{float, _, 0.0015}], toks(<<"1.5e-3">>)),
    ?assertMatch([{float, _, 1000.5}], toks(<<"1_000.5">>)).

%% `1.foo' is an attribute access on an integer, not a malformed float.
dot_after_integer_is_an_operator_test() ->
    ?assertMatch([{int, _, 1}, {op, _, '.'}, {name, _, foo}], toks(<<"1.foo">>)).

%%%===================================================================
%%% Strings
%%%===================================================================

string_escapes_test() ->
    ?assertMatch([{str, _, <<"a\nb">>}],  toks(<<"\"a\\nb\"">>)),
    ?assertMatch([{str, _, <<"a\tb">>}],  toks(<<"\"a\\tb\"">>)),
    ?assertMatch([{str, _, <<"a\\b">>}],  toks(<<"\"a\\\\b\"">>)),
    ?assertMatch([{str, _, <<"a\"b">>}],  toks(<<"\"a\\\"b\"">>)),
    ?assertMatch([{str, _, <<"aAb">>}],   toks(<<"\"a\\x41b\"">>)),
    ?assertMatch([{str, _, <<"a中b"/utf8>>}], toks(<<"\"a\\u4e2db\"">>)).

%% Python keeps the backslash for an escape it does not know.
unknown_escape_keeps_the_backslash_test() ->
    ?assertMatch([{str, _, <<"a\\qb">>}], toks(<<"\"a\\qb\"">>)).

utf8_survives_intact_test() ->
    ?assertMatch([{str, _, <<"中文"/utf8>>}], toks(<<"\"中文\""/utf8>>)).

unterminated_string_test() ->
    ?assertMatch({error, _, {unclosed_block, string, _}}, lex(<<"\"abc }}">>)).

%%%===================================================================
%%% Operators and names
%%%===================================================================

%% Longest match. Every one of these has a shorter operator as a prefix, and
%% getting the order wrong produces parse errors that are hard to trace.
longest_match_test() ->
    [?assertMatch([{op, _, Op}], toks(atom_to_binary(Op, utf8)))
     || Op <- ['**', '//', '==', '!=', '<=', '>=']].

keywords_are_their_own_tokens_test() ->
    ?assertMatch([{kw, _, 'and'}], toks(<<"and">>)),
    ?assertMatch([{kw, _, 'true'}], toks(<<"true">>)),
    ?assertMatch([{kw, _, 'true'}], toks(<<"True">>)),
    ?assertMatch([{kw, _, 'none'}], toks(<<"None">>)).

%% Deviation J10: identifiers are ASCII, and a non-ASCII one is refused rather
%% than silently truncated at the first byte over 127.
non_ascii_identifier_is_rejected_test() ->
    ?assertMatch({error, _, {unexpected_token, <<"中"/utf8>>}},
                 lex(<<"中 }}"/utf8>>)).

%%%===================================================================
%%% Positions
%%%===================================================================

columns_are_tracked_test() ->
    {ok, T, _, _, _} = ai_jinja_lexer:tokens(<<"a + bb }}">>, {1, 3}, expr_end),
    ?assertMatch([{name, {1, 3}, a}, {op, {1, 5}, '+'}, {name, {1, 7}, bb}], T).

newlines_inside_a_tag_advance_the_line_test() ->
    {ok, T, _, _, _} = ai_jinja_lexer:tokens(<<"a\n  b }}">>, {1, 3}, expr_end),
    ?assertMatch([{name, {1, 3}, a}, {name, {2, 3}, b}], T).
