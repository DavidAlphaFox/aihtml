%%%-------------------------------------------------------------------
%%% Tests for compile_inline/3 and its run-time counterpart.
%%%
%%% The property that matters most is that the two paths agree. A template
%%% that is a build error with the parse_transform and silently renders empty
%%% without it would disagree exactly where it is hardest to notice.
%%%-------------------------------------------------------------------
-module(ai_jinja_inline_tests).

-include_lib("eunit/include/eunit.hrl").

%% Templates that both paths must handle identically, whether by rendering the
%% same bytes or by refusing for the same reason.
-define(CASES,
        [{<<"plain text">>, #{}},
         {<<"Hi {{ n }}!">>, #{n => <<"a">>}},
         {<<"{{ x }}">>, #{x => <<"<b>">>}},
         {<<"{{ 1 + 2 }}">>, #{}},
         {<<"{{ 'ab'|upper }}">>, #{}},
         {<<"{% if a %}Y{% else %}N{% endif %}">>, #{a => 1}},
         {<<"{% for x in xs %}{{ x }},{% endfor %}">>, #{xs => [1, 2]}},
         {<<"{% for x in xs %}{{ loop.index }}{% endfor %}">>, #{xs => [1, 2]}},
         {<<"{% set a = 1 %}{{ a }}{% set a = 2 %}{{ a }}">>, #{}},
         {<<"{% with v = 1 %}{{ v }}{% endwith %}">>, #{}},
         {<<"{% filter upper %}x{% endfilter %}">>, #{}},
         {<<"{% macro m(a) %}[{{ a }}]{% endmacro %}{{ m(2) }}">>, #{}},
         {<<"{{ 'a' if x else 'b' }}">>, #{x => 1}},
         %% ... and the ones both must refuse
         {<<"{% include \"x.j2\" %}">>, #{}},
         {<<"{% extends \"x.j2\" %}">>, #{}},
         {<<"{% import \"x.j2\" as m %}">>, #{}},
         {<<"{% from \"x.j2\" import m %}">>, #{}},
         {<<"{% block b %}x{% endblock %}">>, #{}},
         {<<"{% if x %}">>, #{}}]).

expanded(T, C) ->
    Opts = #{module => j2_inline_probe, source => <<"inline">>},
    case ai_jinja_parser:parse(T, Opts) of
        {error, {_, _, R}} -> {error, tag(R)};
        {ok, Nodes} ->
            case ai_jinja_compiler:compile_inline(Nodes, erl_parse:abstract(C),
                                                  Opts) of
                {error, {_, _, R}} -> {error, tag(R)};
                {ok, Expr} ->
                    {value, V, _} = erl_eval:expr(Expr, erl_eval:new_bindings()),
                    {ok, V}
            end
    end.

interpreted(T, C) ->
    try {ok, ai_jinja:inline(T, C)}
    catch error:{ai_jinja, {error, {_, _, R}}} -> {error, tag(R)};
          error:{ai_jinja, R}                  -> {error, tag(R)}
    end.

tag(R) when is_tuple(R) -> element(1, R);
tag(R)                  -> R.

%%%===================================================================
%%% The consistency requirement
%%%===================================================================

expansion_and_fallback_agree_test_() ->
    [{binary_to_list(T),
      ?_assertEqual(interpreted(T, C), expanded(T, C))} || {T, C} <- ?CASES].

%% Both must produce the right answer, not merely the same wrong one.
expansion_renders_correctly_test() ->
    ?assertEqual({ok, <<"Hi a!">>}, expanded(<<"Hi {{ n }}!">>, #{n => <<"a">>})),
    ?assertEqual({ok, <<"1,2,">>},
                 expanded(<<"{% for x in xs %}{{ x }},{% endfor %}">>,
                          #{xs => [1, 2]})),
    ?assertEqual({ok, <<"&lt;b&gt;">>}, expanded(<<"{{ x }}">>, #{x => <<"<b>">>})).

%%%===================================================================
%%% What an inline template cannot carry
%%%===================================================================

forbidden_statements_test() ->
    [?assertEqual(true, ai_jinja_compiler:inline_forbidden(K))
     || K <- [include, extends, import, from, block]],
    [?assertEqual(false, ai_jinja_compiler:inline_forbidden(K))
     || K <- ['if', 'for', set, with, filter, macro, call, do, output, text]].

forbidden_statements_are_refused_on_both_paths_test() ->
    [begin
         ?assertEqual({error, target_in_inline_template}, expanded(T, #{})),
         ?assertEqual({error, target_in_inline_template}, interpreted(T, #{}))
     end
     || T <- [<<"{% include \"x.j2\" %}">>,
              <<"{% extends \"x.j2\" %}">>,
              <<"{% import \"x.j2\" as m %}">>,
              <<"{% from \"x.j2\" import m %}">>,
              <<"{% block b %}x{% endblock %}">>]].

%% Nested is just as forbidden as top level, or the check would be trivially
%% bypassed by wrapping it in an {% if %}.
nested_forbidden_statement_test() ->
    T = <<"{% if a %}{% for x in y %}{% include \"x.j2\" %}{% endfor %}{% endif %}">>,
    ?assertEqual({error, target_in_inline_template}, expanded(T, #{})),
    ?assertEqual({error, target_in_inline_template}, interpreted(T, #{})).

%% A bound fun cannot refer to itself or to one bound after it, so mutual and
%% self recursion are impossible here and are reported rather than left to the
%% Erlang compiler's "unbound variable".
mutual_macros_are_refused_test() ->
    ?assertEqual({error, mutual_macro_in_inline},
                 expanded(<<"{% macro a() %}{{ b() }}{% endmacro %}"
                            "{% macro b() %}{{ a() }}{% endmacro %}">>, #{})),
    ?assertEqual({error, mutual_macro_in_inline},
                 expanded(<<"{% macro a() %}{{ a() }}{% endmacro %}">>, #{})).

%% Calling a macro defined earlier is fine, which is the ordinary case.
forward_macro_reference_is_fine_test() ->
    ?assertEqual({ok, <<"x">>},
                 expanded(<<"{% macro a() %}x{% endmacro %}"
                            "{% macro b() %}{{ a() }}{% endmacro %}{{ b() }}">>,
                          #{})).

%%%===================================================================
%%% Hygiene
%%%===================================================================

%% The expansion lives inside somebody else's function, so its variables must
%% not collide with theirs.
generated_variables_do_not_collide_test() ->
    Opts = #{module => j2_inline_probe, source => <<"inline">>},
    {ok, Nodes} = ai_jinja_parser:parse(
                    <<"{% for x in xs %}{{ loop.index }}{% endfor %}">>, Opts),
    {ok, Expr} = ai_jinja_compiler:compile_inline(
                   Nodes, erl_parse:abstract(#{xs => [1, 2]}), Opts),
    Bound = erl_eval:add_binding('V', mine,
                                 erl_eval:add_binding('Ctx', mine,
                                                      erl_eval:new_bindings())),
    %% Evaluating with those names already bound must not badmatch.
    ?assertMatch({value, <<"12">>, _}, erl_eval:expr(Expr, Bound)).
