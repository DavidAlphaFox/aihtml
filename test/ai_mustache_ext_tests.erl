%%%-------------------------------------------------------------------
%%% @doc Tests for the aihtml-specific semantics the official spec does not
%%% cover: the `{{+}}' / `{{-}}' has sections, the `{{*}}' lambda, the section
%%% dispatch table, the falsy set and the escape character set.
%%%
%%% The cases are written against the NEW semantics (standard context stack,
%%% decision D2) so they light up unchanged once phases 2 and 3 land.
%%% See tasks/T05.md and designs/03-semantics.md.
%%% @end
%%%-------------------------------------------------------------------
-module(ai_mustache_ext_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Case table
%%%===================================================================
%% {Title, Template, Context, Expected}

%% {{+x}} is a pure conditional: it tests truthiness and does NOT push a
%% scope and does NOT iterate. This is what distinguishes it from {{#x}}.
%% See designs/03-semantics.md section 4.
has_cases() ->
    [{"has: true renders body",
      <<"[{{+x}}yes{{/x}}]">>, #{x => true}, <<"[yes]">>},
     {"has: non-empty binary is truthy",
      <<"[{{+x}}yes{{/x}}]">>, #{x => <<"v">>}, <<"[yes]">>},
     {"has: map is truthy",
      <<"[{{+x}}yes{{/x}}]">>, #{x => #{a => 1}}, <<"[yes]">>},
     {"has: empty map is truthy",
      <<"[{{+x}}yes{{/x}}]">>, #{x => #{}}, <<"[yes]">>},
     {"has: non-empty list is truthy",
      <<"[{{+x}}yes{{/x}}]">>, #{x => [1, 2]}, <<"[yes]">>},
     {"has: zero is truthy",
      <<"[{{+x}}yes{{/x}}]">>, #{x => 0}, <<"[yes]">>},
     {"has: missing key is falsy",
      <<"[{{+x}}yes{{/x}}]">>, #{}, <<"[]">>},
     {"has: false is falsy",
      <<"[{{+x}}yes{{/x}}]">>, #{x => false}, <<"[]">>},
     {"has: undefined is falsy",
      <<"[{{+x}}yes{{/x}}]">>, #{x => undefined}, <<"[]">>},
     {"has: null is falsy",
      <<"[{{+x}}yes{{/x}}]">>, #{x => null}, <<"[]">>},
     {"has: empty list is falsy",
      <<"[{{+x}}yes{{/x}}]">>, #{x => []}, <<"[]">>},
     {"has: empty binary is falsy",
      <<"[{{+x}}yes{{/x}}]">>, #{x => <<>>}, <<"[]">>},
     {"has: fun/1 returning true renders body",
      <<"[{{+x}}yes{{/x}}]">>, #{x => fun(_Frame) -> true end}, <<"[yes]">>},
     {"has: fun/1 returning false skips body",
      <<"[{{+x}}yes{{/x}}]">>, #{x => fun(_Frame) -> false end}, <<"[]">>},
     {"has: fun/1 receives the current frame",
      <<"[{{+x}}yes{{/x}}]">>,
      #{x => fun(F) -> maps:get(flag, F, false) end, flag => true}, <<"[yes]">>}].

%% The scope-neutrality of {{+}} is the whole reason it survives the move to
%% standard context stack semantics, so it gets its own dedicated cases.
has_scope_cases() ->
    [{"has: does not push scope, full path resolves",
      <<"{{+user}}{{user.name}}{{/user}}">>,
      #{user => #{name => <<"David">>}}, <<"David">>},
     {"has: does not push scope, bare name does not resolve",
      <<"[{{+user}}{{name}}{{/user}}]">>,
      #{user => #{name => <<"David">>}}, <<"[]">>},
     {"has: does not iterate over a list",
      <<"[{{+xs}}once{{/xs}}]">>, #{xs => [1, 2, 3]}, <<"[once]">>}].

%% {{-x}} is {{+x}} negated.
inverted_has_cases() ->
    [{"inverted has: false renders body",
      <<"[{{-x}}no{{/x}}]">>, #{x => false}, <<"[no]">>},
     {"inverted has: missing key renders body",
      <<"[{{-x}}no{{/x}}]">>, #{}, <<"[no]">>},
     {"inverted has: null renders body",
      <<"[{{-x}}no{{/x}}]">>, #{x => null}, <<"[no]">>},
     {"inverted has: empty list renders body",
      <<"[{{-x}}no{{/x}}]">>, #{x => []}, <<"[no]">>},
     {"inverted has: true skips body",
      <<"[{{-x}}no{{/x}}]">>, #{x => true}, <<"[]">>},
     {"inverted has: zero is truthy so body is skipped",
      <<"[{{-x}}no{{/x}}]">>, #{x => 0}, <<"[]">>},
     {"inverted has: fun/1 returning false renders body",
      <<"[{{-x}}no{{/x}}]">>, #{x => fun(_F) -> false end}, <<"[no]">>},
     {"inverted has: fun/1 returning true skips body",
      <<"[{{-x}}no{{/x}}]">>, #{x => fun(_F) -> true end}, <<"[]">>}].

%% {{*f}} is the lambda extension. Its output is NOT escaped -- producing HTML
%% is the entire point. See designs/03-semantics.md section 5.
lambda_cases() ->
    [{"lambda: fun/1 receives the current frame",
      <<"[{{*f}}]">>,
      #{f => fun(F) -> maps:get(who, F) end, who => <<"world">>}, <<"[world]">>},
     {"lambda: fun/2 with a value",
      <<"[{{*f}}]">>,
      #{f => [fun(V, _F) -> <<"v=", V/binary>> end, <<"x">>]}, <<"[v=x]">>},
     {"lambda: fun/2 also sees the frame",
      <<"[{{*f}}]">>,
      #{f => [fun(V, F) -> <<V/binary, (maps:get(sep, F))/binary>> end, <<"a">>],
        sep => <<"!">>}, <<"[a!]">>},
     {"lambda: output is not escaped",
      <<"[{{*f}}]">>,
      #{f => fun(_F) -> <<"<b>&</b>">> end}, <<"[<b>&</b>]">>},
     {"lambda: iolist return is accepted",
      <<"[{{*f}}]">>,
      #{f => fun(_F) -> [<<"a">>, [<<"b">>], <<"c">>] end}, <<"[abc]">>},
     {"lambda: missing key renders nothing",
      <<"[{{*f}}]">>, #{}, <<"[]">>}].

%% The eight branches of the section dispatch table in
%% designs/03-semantics.md section 3, one case each.
section_dispatch_cases() ->
    [{"section: empty list skips body",
      <<"[{{#x}}b{{/x}}]">>, #{x => []}, <<"[]">>},
     {"section: non-empty list iterates and pushes each element",
      <<"[{{#xs}}({{n}}){{/xs}}]">>,
      #{xs => [#{n => 1}, #{n => 2}]}, <<"[(1)(2)]">>},
     {"section: map pushes scope once",
      <<"[{{#u}}{{name}}{{/u}}]">>,
      #{u => #{name => <<"D">>}}, <<"[D]">>},
     {"section: true renders once without pushing",
      <<"[{{#x}}{{y}}{{/x}}]">>, #{x => true, y => <<"v">>}, <<"[v]">>},
     {"section: fun/2 receives the rendered body",
      <<"[{{#w}}inner{{/w}}]">>,
      #{w => fun(Body, _F) -> <<"<", Body/binary, ">">> end}, <<"[<inner>]">>},
     {"section: fun/1 result is dispatched recursively",
      <<"[{{#x}}({{n}}){{/x}}]">>,
      #{x => fun(_F) -> [#{n => 1}, #{n => 2}] end}, <<"[(1)(2)]">>},
     {"section: falsy value skips body",
      <<"[{{#x}}b{{/x}}]">>, #{x => null}, <<"[]">>},
     {"section: scalar renders once and is reachable as {{.}}",
      <<"[{{#x}}{{.}}{{/x}}]">>, #{x => <<"v">>}, <<"[v]">>}].

%% Falsy is exactly: undefined, false, [], <<>>, null.
%% Everything else is truthy -- notably 0 and #{}, which trip people up.
%% See designs/03-semantics.md section 2.1.
falsy_cases() ->
    [{"falsy: zero is truthy in a section",
      <<"[{{#x}}b{{/x}}]">>, #{x => 0}, <<"[b]">>},
     {"falsy: zero is truthy in an inverted section",
      <<"[{{^x}}b{{/x}}]">>, #{x => 0}, <<"[]">>},
     {"falsy: empty map is truthy",
      <<"[{{#x}}b{{/x}}]">>, #{x => #{}}, <<"[b]">>},
     {"falsy: empty binary is falsy",
      <<"[{{^x}}b{{/x}}]">>, #{x => <<>>}, <<"[b]">>},
     {"falsy: null is falsy",
      <<"[{{^x}}b{{/x}}]">>, #{x => null}, <<"[b]">>},
     {"falsy: undefined is falsy",
      <<"[{{^x}}b{{/x}}]">>, #{x => undefined}, <<"[b]">>}].

%% The escape set is exactly & < > " '. The old implementation also escaped
%% / = and backtick, which corrupts URLs, and emitted &amp without the
%% semicolon (bug B3). Both behaviours are locked out here.
%% See designs/04-codegen.md section 5.1.
escape_cases() ->
    [{"escape: ampersand gains its semicolon",
      <<"{{x}}">>, #{x => <<"&">>}, <<"&amp;">>},
     {"escape: angle brackets",
      <<"{{x}}">>, #{x => <<"<a>">>}, <<"&lt;a&gt;">>},
     {"escape: double quote",
      <<"{{x}}">>, #{x => <<"\"">>}, <<"&quot;">>},
     {"escape: single quote",
      <<"{{x}}">>, #{x => <<"'">>}, <<"&#39;">>},
     {"escape: slash is NOT escaped",
      <<"{{x}}">>, #{x => <<"/a/b">>}, <<"/a/b">>},
     {"escape: equals sign is NOT escaped",
      <<"{{x}}">>, #{x => <<"a=b">>}, <<"a=b">>},
     {"escape: backtick is NOT escaped",
      <<"{{x}}">>, #{x => <<"`">>}, <<"`">>},
     {"escape: a URL survives intact",
      <<"<a href=\"{{u}}\">">>, #{u => <<"/x/y?a=1">>},
      <<"<a href=\"/x/y?a=1\">">>},
     {"escape: triple mustache does not escape",
      <<"{{{x}}}">>, #{x => <<"<b>&</b>">>}, <<"<b>&</b>">>},
     {"escape: ampersand sigil does not escape",
      <<"{{&x}}">>, #{x => <<"<b>&</b>">>}, <<"<b>&</b>">>}].

%% Numbers must render the way the spec demands: integers plainly, floats via
%% float_to_binary/2 with `short'. See designs/02-architecture.md section 6.2.
number_cases() ->
    [{"number: integer",
      <<"{{x}}">>, #{x => 85}, <<"85">>},
     {"number: decimal keeps its short form",
      <<"{{x}}">>, #{x => 1.21}, <<"1.21">>},
     {"number: one decimal place",
      <<"{{x}}">>, #{x => 1.1}, <<"1.1">>},
     {"number: negative integer",
      <<"{{x}}">>, #{x => -7}, <<"-7">>}].

all_cases() ->
    [{"has", has_cases()},
     {"has scope", has_scope_cases()},
     {"inverted has", inverted_has_cases()},
     {"lambda", lambda_cases()},
     {"section dispatch", section_dispatch_cases()},
     {"falsy", falsy_cases()},
     {"escape", escape_cases()},
     {"number", number_cases()}].

%%%===================================================================
%%% Table well-formedness -- always runs
%%%===================================================================

table_is_well_formed_test_() ->
    [{Group ++ ": " ++ Title,
      fun() ->
              ?assert(is_binary(Tpl) andalso byte_size(Tpl) > 0),
              ?assert(is_binary(Exp)),
              ?assert(is_map(Ctx) orelse is_list(Ctx) orelse is_number(Ctx))
      end}
     || {Group, Cases} <- all_cases(), {Title, Tpl, Ctx, Exp} <- Cases].

titles_are_unique_test() ->
    Titles = [Title || {_G, Cases} <- all_cases(), {Title, _, _, _} <- Cases],
    ?assertEqual([], Titles -- lists:usort(Titles)).

%%%===================================================================
%%% Behaviour -- generated once ai_mustache exists
%%%===================================================================

extensions_test_() ->
    case ai_mustache_test_lib:implemented() of
        false ->
            {"aihtml extension semantics: PENDING -- ai_mustache:render_string/3 "
             "not implemented yet (phase 1 baseline, see tasks/T05.md)",
             fun() -> ok end};
        true ->
            [{Group, [{Title, fun() -> exec(Tpl, Ctx, Exp) end}
                      || {Title, Tpl, Ctx, Exp} <- Cases]}
             || {Group, Cases} <- all_cases()]
    end.

exec(Tpl, Ctx, Expected) ->
    Got = ai_mustache:render_string(Tpl, Ctx, #{}),
    ?assertEqual({Expected, Tpl}, {Got, Tpl}).
