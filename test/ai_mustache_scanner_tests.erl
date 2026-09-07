%%%-------------------------------------------------------------------
%%% @doc Unit tests for ai_mustache_scanner.
%%%
%%% The scanner is the only producer of ai_mustache_token(), so these tests
%%% assert on the token stream itself rather than on rendered output: at this
%%% point in the build there is nothing that renders. Three groups:
%%%
%%% <ul>
%%%   <li>tag classification -- one case per marker</li>
%%%   <li>positions -- every token's loc() against a hand-computed
%%%       {Line, Col}</li>
%%%   <li>standalone lines -- the matrix of marker x context, including the
%%%       markers that must NEVER be standalone</li>
%%% </ul>
%%%
%%% See tasks/T07.md and tasks/T10.md, designs/03-semantics.md section 6.
%%% @end
%%%-------------------------------------------------------------------
-module(ai_mustache_scanner_tests).

-include_lib("eunit/include/eunit.hrl").
-include("ai_mustache.hrl").

%%%===================================================================
%%% Helpers
%%%===================================================================

%% Scan and unwrap, turning a scan failure into a readable test error.
tokens(Bin) ->
    case ai_mustache_scanner:scan(Bin) of
        {ok, Ts}       -> Ts;
        {error, _} = E -> erlang:error({scan_failed, Bin, E})
    end.

%% Drop loc() so a case can assert on shape alone. Positions get their own
%% group below, where they are the point rather than noise.
strip_loc(Ts) -> [strip_one(T) || T <- Ts].

strip_one({text, _Loc, Bin})            -> {text, Bin};
strip_one({tag, _Loc, M, Body, Indent}) -> {tag, M, Body, Indent}.

%% Just the loc() of each token, in order.
locs(Ts) -> [element(2, T) || T <- Ts].

%% Assert the scanner reports rather than raises, and hand back the error
%% triple. Every error path in the module is required to come back this way.
scan_error(Bin, Opts) ->
    Got = try
              ai_mustache_scanner:scan(Bin, Opts)
          catch
              Cl:Rs:Stack ->
                  erlang:error({scanner_raised, Bin, Cl, Rs, Stack})
          end,
    case Got of
        {error, {File, Line, Reason}} -> {File, Line, Reason};
        {ok, Ts}                      -> erlang:error({unexpected_ok, Bin, Ts})
    end.

%%%===================================================================
%%% Tag classification: one case per marker
%%%===================================================================
%% {Title, Template, ExpectedStrippedTokens}

tag_kind_cases() ->
    [{"{{x}} is a plain interpolation",
      <<"{{x}}">>, [{tag, none, <<"x">>, <<>>}]},
     {"{{{x}}} normalises to the ${ marker",
      <<"{{{x}}}">>, [{tag, ${, <<"x">>, <<>>}]},
     {"{{&x}} normalises to the same ${ marker as {{{x}}}",
      <<"{{&x}}">>, [{tag, ${, <<"x">>, <<>>}]},
     {"{{#x}} is a section open",
      <<"{{#x}}">>, [{tag, $#, <<"x">>, <<>>}]},
     {"{{^x}} is an inverted section open",
      <<"{{^x}}">>, [{tag, $^, <<"x">>, <<>>}]},
     {"{{/x}} is a close tag",
      <<"{{/x}}">>, [{tag, $/, <<"x">>, <<>>}]},
     {"{{>x}} is a partial",
      <<"{{>x}}">>, [{tag, $>, <<"x">>, <<>>}]},
     {"{{!c}} is a comment and produces no token",
      <<"a{{!c}}b">>, [{text, <<"a">>}, {text, <<"b">>}]},
     {"{{+x}} is a positive has",
      <<"{{+x}}">>, [{tag, $+, <<"x">>, <<>>}]},
     {"{{-x}} is a negative has",
      <<"{{-x}}">>, [{tag, $-, <<"x">>, <<>>}]},
     {"{{*x}} is a lambda",
      <<"{{*x}}">>, [{tag, $*, <<"x">>, <<>>}]},
     {"{{=<% %>=}} is a delimiter switch and produces no token",
      <<"a{{=<% %>=}}b">>, [{text, <<"a">>}, {text, <<"b">>}]}].

tag_kinds_test_() ->
    [{Title, ?_assertEqual(Expected, strip_loc(tokens(Input)))}
     || {Title, Input, Expected} <- tag_kind_cases()].

%% Outer whitespace inside a tag is trimmed; the parser never sees it.
content_trim_cases() ->
    [{"{{ x }} trims to x",        <<"{{ x }}">>,     <<"x">>},
     {"{{\tx\t}} trims tabs",      <<"{{\tx\t}}">>,   <<"x">>},
     {"{{# x }} trims after the marker", <<"{{# x }}">>, <<"x">>},
     {"{{{ x }}} trims",           <<"{{{ x }}}">>,   <<"x">>},
     {"{{& x }} trims",            <<"{{& x }}">>,    <<"x">>},
     {"{{> shared/item }} trims but keeps the path",
      <<"{{> shared/item }}">>, <<"shared/item">>},
     {"{{a.b}} keeps the dots",    <<"{{a.b}}">>,     <<"a.b">>},
     {"{{.}} keeps the lone dot",  <<"{{.}}">>,       <<".">>},
     {"{{}} is empty content",     <<"{{}}">>,        <<>>}].

content_trim_test_() ->
    [{Title, ?_assertEqual([Expected],
                           [B || {tag, _, _, B, _} <- tokens(Input)])}
     || {Title, Input, Expected} <- content_trim_cases()].

%% A tag whose name merely starts with a letter that could be mistaken for a
%% marker is still a plain interpolation.
name_starting_with_letter_test() ->
    ?assertEqual([{tag, none, <<"a-b">>, <<>>}], strip_loc(tokens(<<"{{a-b}}">>))),
    ?assertEqual([{tag, none, <<"_x">>, <<>>}], strip_loc(tokens(<<"{{_x}}">>))),
    ?assertEqual([{tag, none, <<"9">>, <<>>}], strip_loc(tokens(<<"{{9}}">>))).

%% Comments and delimiter switches are consumed inside the scanner; nothing
%% downstream should ever have to know they existed.
comment_produces_no_token_test() ->
    ?assertEqual([], tokens(<<"{{!just a comment}}">>)),
    ?assertEqual([], tokens(<<"{{! a\nb\nc }}">>)).

delimiter_switch_produces_no_token_test() ->
    ?assertEqual([], tokens(<<"{{=<% %>=}}">>)),
    ?assertEqual([], tokens(<<"{{=<% %>=}}<%={{ }}=%>">>)).

%% The scanner does not merge adjacent text; that is ai_mustache_ast's job.
adjacent_text_is_not_merged_test() ->
    ?assertEqual([{text, <<"a">>}, {text, <<"b">>}],
                 strip_loc(tokens(<<"a{{!c}}b">>))).

%%%===================================================================
%%% Positions
%%%===================================================================
%% {Title, Template, [ExpectedLoc]}

loc_cases() ->
    [{"first token starts at {1, 1}",
      <<"{{x}}">>, [{1, 1}]},
     {"a tag's loc points at the opening delimiter, not the content",
      <<"ab{{x}}">>, [{1, 1}, {1, 3}]},
     %% "ab\ncd{{x}}ef\n{{y}}"
     %%  line 1: ab                    text starts {1,1}
     %%  line 2: cd{{x}}ef             {{x}} is the 3rd byte of line 2
     %%          {{x}} ends at col 7, so "ef\n" starts at {2,8}
     %%  line 3: {{y}}                 {3,1}
     {"multi-line text keeps line and column in step",
      <<"ab\ncd{{x}}ef\n{{y}}">>, [{1, 1}, {2, 3}, {2, 8}, {3, 1}]},
     {"a lone \\r does not start a new line",
      <<"a\rb{{x}}">>, [{1, 1}, {1, 4}]},
     {"\\r\\n counts as exactly one line break",
      <<"a\r\n{{x}}">>, [{1, 1}, {2, 1}]},
     {"repeated \\r\\n line endings stay accurate",
      <<"one\r\ntwo\r\n{{x}}">>, [{1, 1}, {3, 1}]},
     %% The comment spans lines 1..3 and ends with "c }}" on line 3, so the
     %% text that follows starts at {3,5} -- but the comment line is
     %% standalone, so the newline is eaten and X lands at {4,1}.
     {"a multi-line comment still advances the line counter",
      <<"{{! a\nb\nc }}\nX{{y}}">>, [{4, 1}, {4, 2}]},
     {"a multi-line comment that is not standalone",
      <<"z{{! a\nb\nc }}X{{y}}">>, [{1, 1}, {3, 5}, {3, 6}]},
     %% "{{=<% %>=}}" is 11 bytes, so "A" is at column 12 on line 1.
     {"positions stay correct across a delimiter switch",
      <<"{{=<% %>=}}A\n<%x%>">>, [{1, 12}, {2, 1}]},
     %% The switch line is standalone here, so it disappears entirely and
     %% "A" is the first byte of line 2.
     {"a standalone delimiter switch line does not shift the next line",
      <<"{{=( )=}}\nA(x)B\n">>, [{2, 1}, {2, 2}, {2, 5}]},
     {"a multi-line section body keeps the close tag's line",
      <<"line one\nline two{{#s}}\n  {{> p }}\nend">>,
      [{1, 1}, {2, 9}, {2, 15}, {3, 3}, {4, 1}]},
     {"utf-8 columns count bytes",
      %% "中" is three bytes, so the tag begins at column 4.
      <<"中{{x}}"/utf8>>, [{1, 1}, {1, 4}]}].

loc_test_() ->
    [{Title, ?_assertEqual(Expected, locs(tokens(Input)))}
     || {Title, Input, Expected} <- loc_cases()].

%%%===================================================================
%%% Standalone lines
%%%===================================================================
%% {Title, Template, ExpectedStrippedTokens}
%%
%% A standalone tag loses its line's leading whitespace, its trailing
%% whitespace and its line ending; the leading whitespace comes back as the
%% token's Indent field.

standalone_cases() ->
    [{"standalone {{#}}: the whole tag line disappears",
      <<"a\n  {{#x}}\nb\n  {{/x}}\nc\n">>,
      [{text, <<"a\n">>},
       {tag, $#, <<"x">>, <<"  ">>},
       {text, <<"b\n">>},
       {tag, $/, <<"x">>, <<"  ">>},
       {text, <<"c\n">>}]},
     {"standalone {{^}}",
      <<"  {{^x}}  \n">>, [{tag, $^, <<"x">>, <<"  ">>}]},
     {"standalone {{/}}",
      <<"  {{/x}}  \n">>, [{tag, $/, <<"x">>, <<"  ">>}]},
     {"standalone {{>}} carries the line indent",
      <<"    {{> p}}\n">>, [{tag, $>, <<"p">>, <<"    ">>}]},
     {"standalone {{!}} leaves nothing at all",
      <<"  {{!c}}  \n">>, []},
     {"standalone {{=}} leaves nothing at all",
      <<"  {{=<% %>=}}  \n">>, []},
     {"standalone {{+}}",
      <<"  {{+x}}\ny\n  {{/x}}\n">>,
      [{tag, $+, <<"x">>, <<"  ">>},
       {text, <<"y\n">>},
       {tag, $/, <<"x">>, <<"  ">>}]},
     {"standalone {{-}}",
      <<"  {{-x}}  \n">>, [{tag, $-, <<"x">>, <<"  ">>}]},
     {"standalone with a tab indent",
      <<"\t{{#x}}\n">>, [{tag, $#, <<"x">>, <<"\t">>}]},
     {"standalone with no indent has an empty Indent",
      <<"{{#x}}\n">>, [{tag, $#, <<"x">>, <<>>}]},
     {"standalone swallows a \\r\\n line ending whole",
      <<"  {{#x}}  \r\n">>, [{tag, $#, <<"x">>, <<"  ">>}]},
     {"standalone at end of input with no line ending",
      <<"  {{#x}}  ">>, [{tag, $#, <<"x">>, <<"  ">>}]},
     {"standalone as the entire template",
      <<"{{#x}}">>, [{tag, $#, <<"x">>, <<>>}]}].

standalone_test_() ->
    [{Title, ?_assertEqual(Expected, strip_loc(tokens(Input)))}
     || {Title, Input, Expected} <- standalone_cases()].

%% The other half of the matrix: contexts in which an otherwise eligible
%% marker is NOT standalone.
not_standalone_cases() ->
    [{"non-blank text before the tag defeats standalone",
      <<"a{{#x}}\n">>,
      [{text, <<"a">>}, {tag, $#, <<"x">>, <<>>}, {text, <<"\n">>}]},
     {"non-blank text after the tag defeats standalone",
      <<"  {{#x}}  y\n">>,
      [{text, <<"  ">>}, {tag, $#, <<"x">>, <<>>}, {text, <<"  y\n">>}]},
     {"a bare \\r is not a line ending, so this is not standalone",
      <<"  {{#x}}  \rz">>,
      [{text, <<"  ">>}, {tag, $#, <<"x">>, <<>>}, {text, <<"  \rz">>}]},
     {"a partial that is not standalone gets an empty Indent",
      <<"x {{> p}}\n">>,
      [{text, <<"x ">>}, {tag, $>, <<"p">>, <<>>}, {text, <<"\n">>}]},
     {"a comment that is not standalone keeps the surrounding whitespace",
      <<"x  {{!c}}  \n">>,
      [{text, <<"x  ">>}, {text, <<"  \n">>}]}].

not_standalone_test_() ->
    [{Title, ?_assertEqual(Expected, strip_loc(tokens(Input)))}
     || {Title, Input, Expected} <- not_standalone_cases()].

%% Interpolation markers are never standalone, whatever the line looks like.
%% This is the single easiest thing to get wrong in the whole scanner: the
%% line's whitespace and line ending are part of the output and must survive
%% verbatim.
never_standalone_cases() ->
    [{"{{x}} alone on its line is NOT standalone",
      <<"  {{x}}  \n">>,
      [{text, <<"  ">>}, {tag, none, <<"x">>, <<>>}, {text, <<"  \n">>}]},
     {"{{x}} alone on its line with no surrounding blanks keeps the newline",
      <<"{{x}}\n">>,
      [{tag, none, <<"x">>, <<>>}, {text, <<"\n">>}]},
     {"{{{x}}} alone on its line is NOT standalone",
      <<"  {{{x}}}  \n">>,
      [{text, <<"  ">>}, {tag, ${, <<"x">>, <<>>}, {text, <<"  \n">>}]},
     {"{{&x}} alone on its line is NOT standalone",
      <<"  {{&x}}  \n">>,
      [{text, <<"  ">>}, {tag, ${, <<"x">>, <<>>}, {text, <<"  \n">>}]},
     {"{{*f}} alone on its line is NOT standalone",
      <<"  {{*f}}  \n">>,
      [{text, <<"  ">>}, {tag, $*, <<"f">>, <<>>}, {text, <<"  \n">>}]},
     {"a never-standalone tag between two blank-only lines keeps both",
      <<"\n  {{x}}\n\n">>,
      [{text, <<"\n  ">>}, {tag, none, <<"x">>, <<>>}, {text, <<"\n\n">>}]}].

never_standalone_test_() ->
    [{Title, ?_assertEqual(Expected, strip_loc(tokens(Input)))}
     || {Title, Input, Expected} <- never_standalone_cases()].

%% The Indent field is only ever populated by standalone processing.
indent_is_the_line_prefix_test() ->
    ?assertEqual([{tag, $>, <<"p">>, <<"      ">>}],
                 strip_loc(tokens(<<"      {{> p}}\n">>))),
    ?assertEqual([{tag, $>, <<"p">>, <<" \t ">>}],
                 strip_loc(tokens(<<" \t {{> p}}\n">>))),
    ?assertEqual([{text, <<"z">>}, {tag, $>, <<"p">>, <<>>}, {text, <<"\n">>}],
                 strip_loc(tokens(<<"z{{> p}}\n">>))).

%%%===================================================================
%%% Custom delimiters
%%%===================================================================
%% {Title, Template, ExpectedStrippedTokens}

delimiter_cases() ->
    [{"two-byte delimiters: <% %>",
      <<"{{=<% %>=}}<%x%>">>, [{tag, none, <<"x">>, <<>>}]},
     {"one-byte delimiters: ( )",
      <<"{{=( )=}}(x)">>, [{tag, none, <<"x">>, <<>>}]},
     {"multi-byte delimiters: || ||",
      <<"{{=|| ||=}}||x||">>, [{tag, none, <<"x">>, <<>>}]},
     {"asymmetric lengths: [[[ ]",
      <<"{{=[[[ ]=}}[[[x]">>, [{tag, none, <<"x">>, <<>>}]},
     {"the old delimiters stop working after a switch",
      <<"{{=<% %>=}}{{x}}">>, [{text, <<"{{x}}">>}]},
     {"switching back restores {{ }}",
      <<"{{=<% %>=}}<%={{ }}=%>{{x}}">>, [{tag, none, <<"x">>, <<>>}]},
     {"markers still work under custom delimiters",
      <<"{{=<% %>=}}<%#a%>b<%/a%>">>,
      [{tag, $#, <<"a">>, <<>>},
       {text, <<"b">>},
       {tag, $/, <<"a">>, <<>>}]},
     {"& still gives a raw interpolation under custom delimiters",
      <<"{{=<% %>=}}<%&x%>">>, [{tag, ${, <<"x">>, <<>>}]},
     %% Delimiters are stream state, not block state: a switch inside a
     %% section is still in force after the section closes.
     {"a switch inside a section survives the close tag",
      <<"{{#a}}{{=<% %>=}}<%x%><%/a%>{{y}}">>,
      [{tag, $#, <<"a">>, <<>>},
       {tag, none, <<"x">>, <<>>},
       {tag, $/, <<"a">>, <<>>},
       {text, <<"{{y}}">>}]}].

delimiter_test_() ->
    [{Title, ?_assertEqual(Expected, strip_loc(tokens(Input)))}
     || {Title, Input, Expected} <- delimiter_cases()].

%% triple/2 requires the opening delimiter to be all `{'. Once the delimiters
%% have been switched the three-brace form is gone and only `&' produces a raw
%% interpolation; `<%{x}%>' is read as marker `{' over the body `x}'.
triple_rule_is_off_after_a_delimiter_switch_test() ->
    ?assertEqual([{tag, ${, <<"x}">>, <<>>}],
                 strip_loc(tokens(<<"{{=<% %>=}}<%{x}%>">>))),
    %% and it is back on once {{ }} is restored
    ?assertEqual([{tag, ${, <<"x">>, <<>>}],
                 strip_loc(tokens(<<"{{=<% %>=}}<%={{ }}=%>{{{x}}}">>))).

%%%===================================================================
%%% Errors
%%%===================================================================
%% Every failure is reported as {error, {File, Line, Reason}}; the scanner
%% must not raise.
%% {Title, Template, ExpectedLine, ExpectedReason}

error_cases() ->
    [{"missing closing delimiter",
      <<"{{x">>, 1, {unclosed_tag, []}},
     {"missing closing delimiter on a later line",
      <<"a\nb\n{{x">>, 3, {unclosed_tag, []}},
     {"missing closing delimiter on a triple tag",
      <<"{{{x">>, 1, {unclosed_tag, []}},
     {"missing closing delimiter under custom delimiters",
      <<"{{=<% %>=}}<%x">>, 1, {unclosed_tag, []}},
     {"delimiter switch with three segments",
      <<"{{=a b c=}}">>, 1, {invalid_delimiter, <<"a b c=">>}},
     {"delimiter switch containing an =",
      <<"{{=a=b c=}}">>, 1, {invalid_delimiter, <<"a=b c=">>}},
     {"delimiter switch with only one segment",
      <<"{{= x =}}">>, 1, {invalid_delimiter, <<"x =">>}},
     {"delimiter switch missing its trailing =",
      <<"{{=<% %>}}">>, 1, {invalid_delimiter, <<"<% %>">>}},
     {"unregistered marker @",
      <<"{{@k}}">>, 1, {unknown_marker, $@}},
     {"unregistered marker on a later line",
      <<"a\n{{@k}}">>, 2, {unknown_marker, $@}},
     {"unregistered marker $",
      <<"{{$block}}">>, 1, {unknown_marker, $$}},
     {"unregistered marker <",
      <<"{{<parent}}">>, 1, {unknown_marker, $<}}].

error_test_() ->
    [{Title,
      ?_assertEqual({<<"nofile">>, Line, Reason}, scan_error(Input, #{}))}
     || {Title, Input, Line, Reason} <- error_cases()].

%% scan/2 puts the configured source into the File slot of every error.
error_file_comes_from_the_source_option_test() ->
    ?assertEqual({<<"views/x.mustache">>, 1, {unclosed_tag, []}},
                 scan_error(<<"{{x">>, #{source => <<"views/x.mustache">>})),
    ?assertEqual({<<"views/x.mustache">>, 1, {unknown_marker, $@}},
                 scan_error(<<"{{@k}}">>, #{source => "views/x.mustache"})).

%% Belt and braces: no input in the error table may escape as an exception.
errors_are_never_raised_test() ->
    lists:foreach(
      fun({_Title, Input, _Line, _Reason}) ->
              ?assertMatch({error, {_, _, _}}, ai_mustache_scanner:scan(Input))
      end, error_cases()).

%%%===================================================================
%%% Miscellaneous
%%%===================================================================

empty_template_test() ->
    ?assertEqual([], tokens(<<>>)).

text_only_template_test() ->
    ?assertEqual([{text, <<"hello\nworld">>}],
                 strip_loc(tokens(<<"hello\nworld">>))).

%% No empty text token is ever emitted for the gap between two adjacent tags.
no_empty_text_between_adjacent_tags_test() ->
    ?assertEqual([{tag, none, <<"a">>, <<>>}, {tag, none, <<"b">>, <<>>}],
                 strip_loc(tokens(<<"{{a}}{{b}}">>))).

%%%===================================================================
%%% Standalone lines in sequence
%%%===================================================================
%% Two standalone tags on consecutive lines. Once a standalone line has been
%% consumed there is no text token left to prove that the next tag sits at
%% column 1, so the scanner has to carry that fact itself; without it the
%% second tag is not recognised as standalone, its line ending survives into
%% the output, and an empty text token is left behind where the eaten line
%% used to be. Both symptoms are asserted here.
%% {Title, Template, ExpectedStrippedTokens}

consecutive_standalone_cases() ->
    [{"two section opens on consecutive lines",
      <<"{{#a}}\n{{#b}}\nx\n{{/b}}\n{{/a}}\n">>,
      [{tag, $#, <<"a">>, <<>>},
       {tag, $#, <<"b">>, <<>>},
       {text, <<"x\n">>},
       {tag, $/, <<"b">>, <<>>},
       {tag, $/, <<"a">>, <<>>}]},
     {"a standalone comment line followed by a standalone partial line",
      <<"{{!c}}\n{{>p}}\n">>, [{tag, $>, <<"p">>, <<>>}]},
     {"two indented standalone lines keep their own indents",
      <<"  {{!c}}\n  {{#a}}\n">>, [{tag, $#, <<"a">>, <<"  ">>}]},
     {"a standalone delimiter switch line followed by a standalone section",
      <<"{{=<% %>=}}\n<%#a%>\nb\n<%/a%>\n">>,
      [{tag, $#, <<"a">>, <<>>},
       {text, <<"b\n">>},
       {tag, $/, <<"a">>, <<>>}]},
     {"consecutive standalone lines with \\r\\n endings",
      <<"{{^b}}\r\n{{/b}}\r\n">>,
      [{tag, $^, <<"b">>, <<>>}, {tag, $/, <<"b">>, <<>>}]},
     %% ... and the flag must not leak: {{#b}} here is on the same line as
     %% {{#a}}, which was not standalone, so neither is {{#b}}.
     {"two eligible tags on one line are still not standalone",
      <<"{{#a}}{{#b}}\n">>,
      [{tag, $#, <<"a">>, <<>>},
       {tag, $#, <<"b">>, <<>>},
       {text, <<"\n">>}]}].

consecutive_standalone_test_() ->
    [{Title, ?_assertEqual(Expected, strip_loc(tokens(Input)))}
     || {Title, Input, Expected} <- consecutive_standalone_cases()].
