%%%-------------------------------------------------------------------
%%% Structural tests for the filter and test libraries.
%%%
%%% Behaviour is covered by the `filters' and `tests' fixture groups; these
%%% assert the properties that hold across the whole library at once, which is
%%% where a newly added filter is most likely to be inconsistent.
%%%-------------------------------------------------------------------
-module(ai_jinja_filters_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% The registry matches reality
%%%===================================================================

filters_table_matches_the_exports_test() ->
    Declared = lists:sort(maps:keys(ai_jinja_filters:filters())),
    Exported = lists:sort([N || {N, 2} <- ai_jinja_filters:module_info(exports),
                                not lists:member(N, [filters, params, tests])]),
    ?assertEqual(Exported, Declared).

tests_table_matches_the_exports_test() ->
    Declared = lists:sort(maps:keys(ai_jinja_tests:tests())),
    Exported = lists:sort([N || {N, 2} <- ai_jinja_tests:module_info(exports),
                                not lists:member(N, [tests, params])]),
    ?assertEqual(Exported, Declared).

every_table_entry_points_at_a_real_function_test() ->
    [?assert(erlang:function_exported(M, F, 2))
     || {M, F} <- maps:values(ai_jinja_filters:filters())
                      ++ maps:values(ai_jinja_tests:tests())].

%% params/0 names positional parameters, and a name that is not a parameter of
%% anything would silently never bind.
params_only_name_declared_filters_test() ->
    Known = maps:keys(ai_jinja_filters:filters()),
    ?assertEqual([], [N || N <- maps:keys(ai_jinja_filters:params()),
                           not lists:member(N, Known)]),
    KnownT = maps:keys(ai_jinja_tests:tests()),
    ?assertEqual([], [N || N <- maps:keys(ai_jinja_tests:params()),
                           not lists:member(N, KnownT)]).

%%%===================================================================
%%% Uniform behaviour
%%%===================================================================

%% An undefined reaching a filter is ordinary -- a missing context key -- and
%% no filter may crash on it. `random' is excluded only because it is the one
%% impure filter and is exercised by its own fixtures.
no_filter_crashes_on_undefined_test() ->
    Bad = [N || N <- maps:keys(ai_jinja_filters:filters()),
                not survives(fun() ->
                                     {M, F} = maps:get(N, ai_jinja_filters:filters()),
                                     M:F(undefined, default_args(N))
                             end)],
    ?assertEqual([], Bad).

no_test_crashes_on_undefined_test() ->
    Bad = [N || N <- maps:keys(ai_jinja_tests:tests()),
                not survives(fun() ->
                                     {M, F} = maps:get(N, ai_jinja_tests:tests()),
                                     M:F(undefined, test_args(N))
                             end)],
    ?assertEqual([], Bad).

%% Reasonable arguments for the filters that require one.
default_args(replace)        -> #{old => <<"a">>, new => <<"b">>};
default_args(attr)           -> #{name => <<"a">>};
default_args(N) when N =:= slice; N =:= batch -> #{slices => 1, linecount => 1};
default_args(groupby)        -> #{attribute => <<"a">>};
default_args(N) when N =:= map; N =:= select; N =:= reject -> #{};
default_args(N) when N =:= selectattr; N =:= rejectattr -> #{attribute => <<"a">>};
default_args(_)              -> #{}.

test_args(N) when N =:= divisibleby -> #{num => 2};
test_args(N) when N =:= sameas; N =:= eq; N =:= ne; N =:= lt; N =:= le;
                  N =:= gt; N =:= ge; N =:= equalto; N =:= greaterthan;
                  N =:= lessthan -> #{other => undefined};
test_args(in) -> #{seq => []};
test_args(_)  -> #{}.

survives(F) ->
    try _ = F(), true
    catch error:{ai_jinja, _} -> true;       % a deliberate diagnosis is fine
          _:_ -> false
    end.

%%%===================================================================
%%% The tricky ones
%%%===================================================================

%% Python's round is half-to-even on the exact binary value. Rounding the
%% shortest decimal representation instead gets 2.345 wrong, which is why the
%% implementation goes through a 20-digit expansion.
round_matches_python_test() ->
    R = fun(V, P) -> ai_jinja_filters:round(V, #{precision => P}) end,
    ?assertEqual(0.0, R(0.5, 0)),
    ?assertEqual(2.0, R(1.5, 0)),
    ?assertEqual(2.0, R(2.5, 0)),
    ?assertEqual(2.35, R(2.345, 2)),
    ?assertEqual(-0.0, R(-0.5, 0)),
    ?assertEqual(2.0, ai_jinja_filters:round(1.4, #{method => <<"ceil">>})),
    ?assertEqual(1.0, ai_jinja_filters:round(1.6, #{method => <<"floor">>})).

%% A `</script>' in a value is the case that matters: left alone it ends the
%% script element that the JSON was embedded in.
tojson_is_html_safe_test() ->
    {safe, Out} = ai_jinja_filters:tojson(#{a => <<"</script>">>}, #{}),
    ?assertEqual(<<"{\"a\": \"\\u003c/script\\u003e\"}">>, Out),
    [?assertEqual(nomatch, binary:match(Out, C)) || C <- [<<"<">>, <<">">>]].

%% `|map("upper")' takes a name as data. Turning that into apply/3 would let a
%% template reach any exported function in the VM.
named_dispatch_is_not_apply_test() ->
    Src = filters_source(),
    ?assertEqual(nomatch, string:find(Src, "erlang:apply(")),
    ?assertEqual(nomatch, string:find(Src, "apply(M, F")).

filters_source() ->
    Dir = filename:dirname(code:which(?MODULE)),
    {ok, Bin} = file:read_file(up(Dir, "src/ai_jinja_filters.erl", 6)),
    unicode:characters_to_list(Bin).

up(_D, Rel, 0) -> erlang:error({not_found, Rel});
up(D, Rel, N) ->
    C = filename:join(D, Rel),
    case filelib:is_regular(C) of true -> C; false -> up(filename:dirname(D), Rel, N - 1) end.

%% Deviation J2, and the visible price of it: `is defined' is false for a
%% context that explicitly passes none.
defined_is_false_for_none_test() ->
    ?assertNot(ai_jinja_tests:defined(undefined, #{})),
    ?assert(ai_jinja_tests:defined(false, #{})),
    ?assert(ai_jinja_tests:none(undefined, #{})).
