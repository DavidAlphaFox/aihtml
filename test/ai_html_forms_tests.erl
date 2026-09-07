%%%-------------------------------------------------------------------
%%% Tests for the shared abstract-forms constructors.
%%%
%%% The two things worth guarding here are the ones that were got wrong
%%% before: the bin_element type of a string literal, and the round trip of
%%% non-ASCII text through erl_prettypr and back through the scanner. Both are
%%% asserted on the actual bytes, not on the shape of the term.
%%%-------------------------------------------------------------------
-module(ai_html_forms_tests).

-include_lib("eunit/include/eunit.hrl").

-define(L, (ai_html_forms:anno(0))).

%%%===================================================================
%%% bin/2
%%%===================================================================

bin_empty_test() ->
    ?assertEqual({bin, ?L, []}, ai_html_forms:bin(?L, <<>>)).

bin_ascii_uses_default_type_test() ->
    {bin, _, [{bin_element, _, {string, _, "hi"}, Size, Type}]} =
        ai_html_forms:bin(?L, <<"hi">>),
    %% [binary] here would mean <<"hi"/binary>>, a runtime badarg.
    ?assertEqual(default, Size),
    ?assertEqual(default, Type).

bin_non_ascii_uses_utf8_type_test() ->
    {bin, _, [{bin_element, _, {string, _, Chars}, default, Type}]} =
        ai_html_forms:bin(?L, <<"中文"/utf8>>),
    ?assertEqual([utf8], Type),
    ?assertEqual("中文", Chars).

bin_invalid_utf8_falls_back_to_bytes_test() ->
    {bin, _, Elems} = ai_html_forms:bin(?L, <<255, 254>>),
    ?assertEqual([{bin_element, ?L, {integer, ?L, 255}, default, default},
                  {bin_element, ?L, {integer, ?L, 254}, default, default}],
                 Elems).

%% The whole point of the utf8 type: text has to survive being printed to an
%% .erl file and read back by the Erlang scanner.
bin_round_trips_through_source_test() ->
    [?assertEqual(Bin, eval_source(ai_html_forms:bin(?L, Bin)))
     || Bin <- [<<>>, <<"plain">>, <<"中文"/utf8>>, <<"a\"b'c&d">>,
                <<"emoji ", 240, 159, 142, 137>>]].

eval_source(Expr) ->
    Src = erl_prettypr:format(erl_syntax:form_list([Expr])) ++ ".",
    {ok, Toks, _} = erl_scan:string(unicode:characters_to_list(Src), 1,
                                    [{text, false}]),
    {ok, [Parsed]} = erl_parse:parse_exprs(Toks),
    {value, V, _} = erl_eval:expr(Parsed, []),
    V.

%%%===================================================================
%%% concat_bin/3, mklist/2, atoms/2, abstract/2
%%%===================================================================

concat_bin_test() ->
    {bin, _, [Head | _]} = ai_html_forms:concat_bin(?L, 'I', <<"  ">>),
    ?assertEqual({bin_element, ?L, {var, ?L, 'I'}, default, [binary]}, Head).

mklist_test() ->
    ?assertEqual({nil, ?L}, ai_html_forms:mklist(?L, [])),
    ?assertEqual({cons, ?L, {atom, ?L, a}, {nil, ?L}},
                 ai_html_forms:mklist(?L, [{atom, ?L, a}])).

atoms_test() ->
    ?assertEqual([a, b], eval_source(ai_html_forms:atoms(?L, [a, b]))).

abstract_carries_the_anno_test() ->
    E = ai_html_forms:abstract(ai_html_forms:anno(7), #{a => [1, <<"x">>]}),
    ?assertEqual(#{a => [1, <<"x">>]}, eval_source(E)),
    %% erl_parse:abstract/1 stamps bare integers; every node must carry the
    %% anno we asked for, or dialyzer rejects the forms as non-opaque.
    ?assert(lists:all(fun(A) -> A =:= ai_html_forms:anno(7) end, annos(E))).

annos(T) when is_tuple(T), tuple_size(T) >= 2, is_atom(element(1, T)) ->
    [element(2, T) | lists:flatmap(fun annos/1, tl(tl(tuple_to_list(T))))];
annos(L) when is_list(L) -> lists:flatmap(fun annos/1, L);
annos(_)                 -> [].

%%%===================================================================
%%% param/3 and uses_var/2
%%%===================================================================

param_underscores_unused_test() ->
    Used   = {var, ?L, 'S'},
    Unused = {atom, ?L, ok},
    ?assertEqual({var, ?L, 'S'},  ai_html_forms:param(?L, 'S', Used)),
    ?assertEqual({var, ?L, '_S'}, ai_html_forms:param(?L, 'S', Unused)).

uses_var_descends_test() ->
    Deep = {call, ?L, {atom, ?L, f}, [{cons, ?L, {var, ?L, 'X'}, {nil, ?L}}]},
    ?assert(ai_html_forms:uses_var(Deep, 'X')),
    ?assertNot(ai_html_forms:uses_var(Deep, 'Y')).

%%%===================================================================
%%% remotes/2
%%%===================================================================

remotes_test() ->
    E = ai_html_forms:rem_call(?L, mod_a, f,
                               [ai_html_forms:rem_call(?L, mod_b, g, [])]),
    ?assertEqual([mod_a, mod_b], lists:sort(ai_html_forms:remotes(E, []))).

remotes_ignores_local_calls_test() ->
    ?assertEqual([], ai_html_forms:remotes(ai_html_forms:loc_call(?L, f, []), [])).

%%%===================================================================
%%% literal_of/1 and static_text/2
%%%===================================================================

literal_of_test() ->
    ?assertEqual(<<>>,       ai_html_forms:literal_of(ai_html_forms:bin(?L, <<>>))),
    ?assertEqual(<<"hi">>,   ai_html_forms:literal_of(ai_html_forms:bin(?L, <<"hi">>))),
    ?assertEqual(<<"中"/utf8>>,
                 ai_html_forms:literal_of(ai_html_forms:bin(?L, <<"中"/utf8>>))),
    ?assertEqual(<<255>>,    ai_html_forms:literal_of(ai_html_forms:bin(?L, <<255>>))),
    ?assertEqual(error,      ai_html_forms:literal_of({var, ?L, 'X'})).

static_text_folds_literals_and_the_indent_var_test() ->
    Exprs = [ai_html_forms:bin(?L, <<"a">>),
             {var, ?L, 'I'},
             ai_html_forms:bin(?L, <<"b">>)],
    ?assertEqual({yes, <<"ab">>}, ai_html_forms:static_text(Exprs, 'I')).

static_text_refuses_anything_dynamic_test() ->
    Exprs = [ai_html_forms:bin(?L, <<"a">>), {var, ?L, 'X'}],
    ?assertEqual(no, ai_html_forms:static_text(Exprs, 'I')).

%%%===================================================================
%%% Architecture invariant 10
%%%===================================================================

%% The shared layer may not know about any engine. Checked on the beam rather
%% than on the source so that a call added through a macro is caught too.
depends_only_on_otp_test() ->
    {ok, {_, [{imports, Imports}]}} =
        beam_lib:chunks(code:which(ai_html_forms), [imports]),
    Engines = [M || {M, _, _} <- Imports,
                    lists:prefix("ai_mustache", atom_to_list(M))
                        orelse lists:prefix("ai_jinja", atom_to_list(M))],
    ?assertEqual([], Engines).
