%%%-------------------------------------------------------------------
%%% Tests for the statement parser.
%%%
%%% Each of the diagnostics in designs/09-jinja-syntax.md section 4.4 gets a
%%% case, plus the AST shapes the fixtures cannot see -- most importantly the
%%% flat elif chain, which exists so codegen does not recurse once per branch.
%%%-------------------------------------------------------------------
-module(ai_jinja_parser_tests).

-include_lib("eunit/include/eunit.hrl").

p(Bin) -> ai_jinja_parser:parse(Bin, #{source => <<"t">>}).
ok_(Bin) -> {ok, N} = p(Bin), N.
err(Bin) -> {error, {_, _, R}} = p(Bin), R.

%%%===================================================================
%%% Shapes
%%%===================================================================

%% An elif chain is one node with a list of branches, not nested ifs.
elif_chain_is_flat_test() ->
    [{'if', _, Branches, Else}] =
        ok_(<<"{% if a %}1{% elif b %}2{% elif c %}3{% else %}4{% endif %}">>),
    ?assertEqual(3, length(Branches)),
    ?assertMatch([{text, _, <<"4">>}], Else).

deep_elif_chain_test() ->
    Chain = iolist_to_binary(
              [<<"{% if a0 %}x">>,
               [io_lib:format("{% elif a~p %}x", [I]) || I <- lists:seq(1, 30)],
               <<"{% endif %}">>]),
    [{'if', _, Branches, []}] = ok_(Chain),
    ?assertEqual(31, length(Branches)).

for_modifiers_test() ->
    ?assertMatch([{'for', _, [x], _, undefined, false, _, []}],
                 ok_(<<"{% for x in xs %}a{% endfor %}">>)),
    ?assertMatch([{'for', _, [k, v], _, undefined, false, _, []}],
                 ok_(<<"{% for k, v in m.items() %}a{% endfor %}">>)),
    ?assertMatch([{'for', _, [x], _, {name, _, c}, false, _, []}],
                 ok_(<<"{% for x in xs if c %}a{% endfor %}">>)),
    ?assertMatch([{'for', _, [x], _, undefined, true, _, []}],
                 ok_(<<"{% for x in xs recursive %}a{% endfor %}">>)),
    ?assertMatch([{'for', _, [x], _, {name, _, c}, true, _, []}],
                 ok_(<<"{% for x in xs if c recursive %}a{% endfor %}">>)),
    ?assertMatch([{'for', _, [x], _, undefined, false, _, [{text, _, <<"e">>}]}],
                 ok_(<<"{% for x in xs %}a{% else %}e{% endfor %}">>)).

set_forms_test() ->
    ?assertMatch([{set, _, a, {lit, _, 1}}], ok_(<<"{% set a = 1 %}">>)),
    ?assertMatch([{set_block, _, a, _, undefined}],
                 ok_(<<"{% set a %}b{% endset %}">>)),
    ?assertMatch([{set_block, _, a, _, {filter, _, upper, _, _}}],
                 ok_(<<"{% set a | upper %}b{% endset %}">>)),
    ?assertMatch([{set, _, {attr, _, _}, _}], ok_(<<"{% set ns.t = 1 %}">>)).

%% The {% filter %} chain is an ordinary expression over a placeholder, which
%% is what lets `{% filter a|b(1) %}' work with no parsing of its own.
filter_block_is_a_chain_test() ->
    Body = ai_jinja_parser:body_placeholder(),
    ?assertMatch([{filter, _, {filter, _, b,
                               {filter, _, a, {name, _, Body}, _}, _}, _}],
                 ok_(<<"{% filter a|b(1) %}x{% endfilter %}">>)).

include_modifiers_test() ->
    ?assertMatch([{include, _, <<"a.j2">>, false, true}],
                 ok_(<<"{% include \"a.j2\" %}">>)),
    ?assertMatch([{include, _, <<"a.j2">>, true, true}],
                 ok_(<<"{% include \"a.j2\" ignore missing %}">>)),
    ?assertMatch([{include, _, <<"a.j2">>, false, false}],
                 ok_(<<"{% include \"a.j2\" without context %}">>)).

%% include carries the context by default, import and from do not.
context_defaults_differ_test() ->
    ?assertMatch([{include, _, _, _, true}],  ok_(<<"{% include \"a.j2\" %}">>)),
    ?assertMatch([{import, _, _, m, false}],  ok_(<<"{% import \"a.j2\" as m %}">>)),
    ?assertMatch([{from, _, _, _, false}],    ok_(<<"{% from \"a.j2\" import m %}">>)),
    ?assertMatch([{import, _, _, m, true}],
                 ok_(<<"{% import \"a.j2\" as m with context %}">>)).

macro_and_call_test() ->
    ?assertMatch([{macro, _, m, [{a, undefined}, {b, {lit, _, 1}}], _}],
                 ok_(<<"{% macro m(a, b=1) %}x{% endmacro %}">>)),
    ?assertMatch([{macro, _, m, [], _}], ok_(<<"{% macro m() %}x{% endmacro %}">>)),
    ?assertMatch([{call, _, [{v, undefined}], {call, _, {name, _, m}, _}, _}],
                 ok_(<<"{% call(v) m() %}x{% endcall %}">>)).

block_modifiers_test() ->
    ?assertMatch([{block, _, b, false, false, _}],
                 ok_(<<"{% block b %}x{% endblock %}">>)),
    ?assertMatch([{block, _, b, true, false, _}],
                 ok_(<<"{% block b scoped %}x{% endblock %}">>)),
    ?assertMatch([{block, _, b, false, true, _}],
                 ok_(<<"{% block b required %}{% endblock %}">>)),
    ?assertMatch([{block, _, b, _, _, _}],
                 ok_(<<"{% block b %}x{% endblock b %}">>)).

%%%===================================================================
%%% Diagnostics
%%%===================================================================

unclosed_names_the_block_it_opened_test() ->
    ?assertMatch({unclosed_block, 'if', _},  err(<<"{% if x %}">>)),
    ?assertMatch({unclosed_block, 'for', _}, err(<<"{% for x in y %}">>)),
    ?assertMatch({unclosed_block, block, _}, err(<<"{% block a %}">>)),
    ?assertMatch({unclosed_block, macro, _}, err(<<"{% macro m() %}">>)),
    ?assertMatch({unclosed_block, with, _},  err(<<"{% with a = 1 %}">>)).

mismatched_end_names_what_was_expected_test() ->
    ?assertEqual({mismatched_end, endif, endfor}, err(<<"{% if x %}{% endfor %}">>)),
    ?assertEqual({mismatched_end, endblock, endif}, err(<<"{% block a %}{% endif %}">>)).

%% An {% else %} in the wrong place is an orphan clause whatever surrounds it;
%% only a closing keyword can be a MISmatch.
orphan_clauses_test() ->
    ?assertEqual({orphan_clause, endif}, err(<<"{% endif %}">>)),
    ?assertEqual({orphan_clause, 'else'}, err(<<"{% else %}">>)),
    ?assertEqual({orphan_clause, 'elif'}, err(<<"{% elif x %}">>)),
    ?assertEqual({orphan_clause, 'else'},
                 err(<<"{% if a %}1{% else %}2{% else %}3{% endif %}">>)),
    ?assertEqual({orphan_clause, 'elif'},
                 err(<<"{% if a %}1{% else %}2{% elif b %}3{% endif %}">>)).

block_name_and_duplicates_test() ->
    ?assertEqual({block_name_mismatch, a, b},
                 err(<<"{% block a %}{% endblock b %}">>)),
    ?assertMatch({duplicate_block, a, _},
                 err(<<"{% block a %}{% endblock %}{% block a %}{% endblock %}">>)),
    %% ... including one nested inside another construct.
    ?assertMatch({duplicate_block, a, _},
                 err(<<"{% block a %}{% endblock %}"
                       "{% for x in y %}{% block a %}{% endblock %}{% endfor %}">>)).

unimplemented_statements_test() ->
    [?assertMatch({unknown_statement, _}, err(T))
     || T <- [<<"{% autoescape true %}x{% endautoescape %}">>,
              <<"{% trans %}x{% endtrans %}">>,
              <<"{% debug %}">>,
              <<"{% for x in [1] %}{% break %}{% endfor %}">>]].

%% Deviation J15: flat destructuring only.
nested_destructuring_is_refused_test() ->
    ?assertMatch({unexpected_token, _},
                 err(<<"{% for (a, b), c in xs %}x{% endfor %}">>)).

%%%===================================================================
%%% Warnings
%%%===================================================================

extends_warnings_test() ->
    Nodes = ok_(<<"a{% extends \"b.j2\" %}{% block t %}x{% endblock %}">>),
    W = ai_jinja_parser:warnings(Nodes),
    ?assert(lists:keymember(extends_not_first, 2, W)).

content_after_extends_warns_test() ->
    Nodes = ok_(<<"{% extends \"b.j2\" %}dropped{% block t %}x{% endblock %}">>),
    W = ai_jinja_parser:warnings(Nodes),
    ?assertMatch([{_, {content_after_extends, text}}], W).

%% Whitespace between blocks is not "content"; warning about it would make the
%% warning useless.
blank_text_after_extends_is_fine_test() ->
    Nodes = ok_(<<"{% extends \"b.j2\" %}\n  {% block t %}x{% endblock %}">>),
    ?assertEqual([], ai_jinja_parser:warnings(Nodes)).

shadowing_a_reserved_name_warns_test() ->
    Nodes = ok_(<<"{% for loop in xs %}x{% endfor %}">>),
    ?assertMatch([{_, {shadows_reserved, loop}}], ai_jinja_parser:warnings(Nodes)),
    ?assertMatch([{_, {shadows_reserved, caller}}],
                 ai_jinja_parser:warnings(ok_(<<"{% set caller = 1 %}">>))).
