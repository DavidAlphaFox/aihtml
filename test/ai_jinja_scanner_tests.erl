%%%-------------------------------------------------------------------
%%% Tests for the scanner.
%%%
%%% The whitespace rules themselves are pinned against CPython by the
%%% `whitespace' fixture group; here the token stream is inspected directly,
%%% for the things a rendered string cannot show.
%%%-------------------------------------------------------------------
-module(ai_jinja_scanner_tests).

-include_lib("eunit/include/eunit.hrl").

scan(Bin)       -> ai_jinja_scanner:scan(Bin, #{source => <<"t">>}).
scan(Bin, Opts) -> ai_jinja_scanner:scan(Bin, Opts#{source => <<"t">>}).

text_of(Bin) ->
    {ok, Toks} = scan(Bin),
    iolist_to_binary([B || {text, _, B} <- Toks]).

%%%===================================================================
%%% The four shapes
%%%===================================================================

shapes_test() ->
    {ok, Toks} = scan(<<"a{{ x }}b{% if y %}c{# z #}d">>),
    ?assertMatch([{text, _, <<"a">>},
                  {expr, _, [{name, _, x}]},
                  {text, _, <<"b">>},
                  {stmt, _, 'if', [{name, _, y}]},
                  {text, _, <<"c">>},
                  {text, _, <<"d">>}], Toks).

%% A comment may contain anything at all, including things that would be
%% syntax errors as tags. Looking for `{#' after `{%' would report those.
comment_swallows_everything_test() ->
    ?assertEqual(<<"ab">>, text_of(<<"a{# {{ {% unclosed #}b">>)),
    ?assertEqual(<<"ab">>, text_of(<<"a{#\nmulti\nline\n#}b">>)).

raw_is_consumed_by_the_scanner_test() ->
    {ok, Toks} = scan(<<"{% raw %}{{ x }}{% endraw %}">>),
    ?assertMatch([{text, _, <<"{{ x }}">>}], Toks).

raw_does_not_nest_test() ->
    ?assertEqual(<<"{% raw %}">>,
                 text_of(<<"{% raw %}{% raw %}{% endraw %}">>)).

%%%===================================================================
%%% Whitespace
%%%===================================================================

%% Surprising but correct: a `-' eats across as many blank lines as there are.
minus_crosses_blank_lines_test() ->
    ?assertEqual(<<"a">>, text_of(<<"a\n\n\n   {{- 1 }}">>)),
    ?assertEqual(<<"b">>, text_of(<<"{{ 1 -}}\n\n\n   b">>)).

%% lstrip_blocks and trim_blocks are measured on the ORIGINAL text and cut
%% together; applied one after the other, the second would no longer see the
%% line start the first consumed.
both_cuts_are_measured_before_either_applies_test() ->
    ?assertEqual(<<>>, text_of(<<"{% if true %}\n  {% endif %}">>)).

lstrip_needs_only_whitespace_before_the_tag_test() ->
    ?assertEqual(<<"a\n  x Y">>, text_of(<<"a\n  x {% if true %}Y{% endif %}">>)),
    ?assertEqual(<<"a\nY">>,  text_of(<<"a\n  {% if true %}Y{% endif %}">>)).

trim_blocks_does_not_apply_to_interpolation_test() ->
    ?assertEqual(<<"\nb">>, text_of(<<"{{ 1 }}\nb">>)),
    ?assertEqual(<<"b">>,   text_of(<<"{% if true %}\nb{% endif %}">>)).

%% The reference implementation spells its raw-open rule without the block
%% suffix, so trim_blocks does not eat the newline after {% raw %}.
trim_blocks_skips_raw_open_test() ->
    ?assertEqual(<<"\na\n">>, text_of(<<"{% raw %}\na\n{% endraw %}">>)).

options_can_be_turned_off_test() ->
    Off = #{trim_blocks => false, lstrip_blocks => false},
    {ok, Toks} = scan(<<"a\n  {% if true %}\nb{% endif %}">>, Off),
    ?assertEqual(<<"a\n  \nb">>, iolist_to_binary([B || {text, _, B} <- Toks])).

keep_trailing_newline_test() ->
    ?assertEqual(<<"abc">>,   text_of(<<"abc\n">>)),
    ?assertEqual(<<"abc\n">>, text_of(<<"abc\n\n">>)),
    {ok, Toks} = scan(<<"abc\n">>, #{keep_trailing_newline => true}),
    ?assertEqual(<<"abc\n">>, iolist_to_binary([B || {text, _, B} <- Toks])).

%% Trimming applies to the static text around a tag, never to what the tag
%% evaluates to. Only visible when a value has padding of its own.
trimming_never_touches_a_value_test() ->
    {ok, Toks} = scan(<<"[{{- s -}}]">>),
    ?assertMatch([{text, _, <<"[">>}, {expr, _, _}, {text, _, <<"]">>}], Toks).

crlf_is_normalised_test() ->
    ?assertEqual(<<"a\nb">>, text_of(<<"a\r\nb">>)).

%%%===================================================================
%%% Diagnostics
%%%===================================================================

unclosed_test() ->
    ?assertMatch({error, {_, _, {unclosed_block, expr, _}}}, scan(<<"{{ x">>)),
    ?assertMatch({error, {_, _, {unclosed_block, stmt, _}}}, scan(<<"{% if">>)),
    ?assertMatch({error, {_, _, {unclosed_block, comment, _}}}, scan(<<"{# c">>)),
    ?assertMatch({error, {_, _, {unclosed_block, raw, _}}}, scan(<<"{% raw %}x">>)).

invalid_utf8_is_refused_test() ->
    ?assertMatch({error, {_, _, {invalid_utf8, _}}}, scan(<<"{{ ", 255, " }}">>)).

%% A tag spanning several lines must not throw off everything after it.
line_numbers_survive_a_multiline_tag_test() ->
    {ok, Toks} = scan(<<"a\n{{\n\n x }}\nb">>),
    ?assertMatch([{text, _, <<"a\n">>}, {expr, {2, 1}, _}, {text, {4, 6}, <<"\nb">>}],
                 Toks).
