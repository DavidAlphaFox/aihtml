%%%-------------------------------------------------------------------
%%% @doc Unit tests for ai_mustache_rt.
%%%
%%% These exercise the runtime functions directly rather than through
%%% ai_mustache: the facade and the compiler do not exist yet, and the
%%% runtime is the one piece generated code links against, so its contract
%%% is worth pinning down on its own.
%%%
%%% The behavioural reference is designs/03-semantics.md (falsy set, section
%%% dispatch table, lambda shapes) and designs/04-codegen.md section 5
%%% (escape set, zero copy). See tasks/T11.md and tasks/T12.md.
%%% @end
%%%-------------------------------------------------------------------
-module(ai_mustache_rt_tests).

-include_lib("eunit/include/eunit.hrl").
-include("ai_mustache.hrl").

%%%===================================================================
%%% lookup/2
%%%===================================================================

lookup_single_segment_test() ->
    ?assertEqual(1, ai_mustache_rt:lookup([a], [#{a => 1}])).

lookup_multi_segment_test() ->
    ?assertEqual(2, ai_mustache_rt:lookup([a, b], [#{a => #{b => 2}}])).

lookup_deep_segment_test() ->
    Ctx = #{a => #{b => #{c => 3}}},
    ?assertEqual(3, ai_mustache_rt:lookup([a, b, c], [Ctx])).

%% The first segment walks outwards through the stack.
lookup_first_segment_backtracks_test() ->
    ?assertEqual(2, ai_mustache_rt:lookup([a], [#{b => 1}, #{a => 2}])).

%% ... and stops at the first frame that has the key, innermost wins.
lookup_innermost_frame_wins_test() ->
    ?assertEqual(1, ai_mustache_rt:lookup([a], [#{a => 1}, #{a => 2}])).

%% Following segments descend strictly inside the value found: `b' is NOT
%% looked for further out on the stack.
lookup_rest_does_not_backtrack_test() ->
    ?assertEqual(undefined,
                 ai_mustache_rt:lookup([a, b], [#{a => #{c => 1}}, #{b => 2}])).

lookup_rest_through_non_map_test() ->
    ?assertEqual(undefined, ai_mustache_rt:lookup([a, b], [#{a => 7}])).

%% {{.}} is the top frame itself.
lookup_dot_test() ->
    ?assertEqual(<<"v">>,
                 ai_mustache_rt:lookup([?AI_MUSTACHE_DOT], [<<"v">>, #{a => 1}])).

lookup_dot_map_frame_test() ->
    M = #{a => 1},
    ?assertEqual(M, ai_mustache_rt:lookup([?AI_MUSTACHE_DOT], [M])).

lookup_dot_empty_stack_test() ->
    ?assertEqual(undefined, ai_mustache_rt:lookup([?AI_MUSTACHE_DOT], [])).

%% A section pushes scalars as-is, so the stack holds non-map frames. They
%% must be skipped, not crash the lookup.
lookup_skips_non_map_frames_test() ->
    ?assertEqual(1, ai_mustache_rt:lookup([a], [<<"x">>, #{a => 1}])),
    ?assertEqual(1, ai_mustache_rt:lookup([a], [42, [1, 2], #{a => 1}])).

%% The root context is allowed to be a bare scalar (spec: implicit iterator
%% over the integer 85).
lookup_bare_scalar_root_test() ->
    ?assertEqual(85, ai_mustache_rt:lookup([?AI_MUSTACHE_DOT], [85])),
    ?assertEqual(undefined, ai_mustache_rt:lookup([a], [85])).

lookup_missing_test() ->
    ?assertEqual(undefined, ai_mustache_rt:lookup([a], [])),
    ?assertEqual(undefined, ai_mustache_rt:lookup([a], [#{}])),
    ?assertEqual(undefined, ai_mustache_rt:lookup([a], [#{b => 1}, #{c => 2}])).

%% A stored `undefined' is indistinguishable from a miss, and that is fine --
%% both render as the empty binary.
lookup_stored_undefined_test() ->
    ?assertEqual(undefined, ai_mustache_rt:lookup([a], [#{a => undefined}])).

lookup_empty_keys_test() ->
    ?assertEqual(#{a => 1}, ai_mustache_rt:lookup([], [#{a => 1}])),
    ?assertEqual(undefined, ai_mustache_rt:lookup([], [])).

%%%===================================================================
%%% truthy/1
%%%===================================================================

truthy_falsy_set_test() ->
    ?assertEqual(false, ai_mustache_rt:truthy(undefined)),
    ?assertEqual(false, ai_mustache_rt:truthy(false)),
    ?assertEqual(false, ai_mustache_rt:truthy([])),
    ?assertEqual(false, ai_mustache_rt:truthy(<<>>)),
    ?assertEqual(false, ai_mustache_rt:truthy(null)).

truthy_zero_is_true_test() ->
    ?assertEqual(true, ai_mustache_rt:truthy(0)),
    ?assertEqual(true, ai_mustache_rt:truthy(0.0)).

truthy_empty_map_is_true_test() ->
    ?assertEqual(true, ai_mustache_rt:truthy(#{})).

truthy_other_values_test() ->
    ?assertEqual(true, ai_mustache_rt:truthy(true)),
    ?assertEqual(true, ai_mustache_rt:truthy(<<"a">>)),
    ?assertEqual(true, ai_mustache_rt:truthy([1])),
    ?assertEqual(true, ai_mustache_rt:truthy(atom)).

%%%===================================================================
%%% to_binary/1
%%%===================================================================

to_binary_integer_test() ->
    ?assertEqual(<<"85">>, ai_mustache_rt:to_binary(85)),
    ?assertEqual(<<"0">>, ai_mustache_rt:to_binary(0)).

to_binary_negative_test() ->
    ?assertEqual(<<"-7">>, ai_mustache_rt:to_binary(-7)).

%% float_to_binary/2 with `short'; the default format would produce
%% <<"1.21000000000000000000e+00">> and fail the spec.
to_binary_float_short_test() ->
    ?assertEqual(<<"1.21">>, ai_mustache_rt:to_binary(1.21)),
    ?assertEqual(<<"1.1">>, ai_mustache_rt:to_binary(1.1)),
    ?assertEqual(<<"0.0">>, ai_mustache_rt:to_binary(0.0)),
    ?assertEqual(<<"-1.5">>, ai_mustache_rt:to_binary(-1.5)).

to_binary_binary_is_identity_test() ->
    B = <<"hello">>,
    ?assert(ai_mustache_rt:to_binary(B) =:= B),
    ?assert(erts_debug:same(ai_mustache_rt:to_binary(B), B)).

to_binary_atom_test() ->
    ?assertEqual(<<"abc">>, ai_mustache_rt:to_binary(abc)),
    ?assertEqual(<<"true">>, ai_mustache_rt:to_binary(true)),
    ?assertEqual(<<"false">>, ai_mustache_rt:to_binary(false)).

to_binary_undefined_and_null_test() ->
    ?assertEqual(<<>>, ai_mustache_rt:to_binary(undefined)),
    ?assertEqual(<<>>, ai_mustache_rt:to_binary(null)).

to_binary_list_test() ->
    ?assertEqual(<<"hi">>, ai_mustache_rt:to_binary("hi")),
    ?assertEqual(<<"ab">>, ai_mustache_rt:to_binary([<<"a">>, [<<"b">>]])),
    ?assertEqual(<<>>, ai_mustache_rt:to_binary([])).

to_binary_utf8_test() ->
    ?assertEqual(<<"中文"/utf8>>, ai_mustache_rt:to_binary(<<"中文"/utf8>>)),
    ?assertEqual(<<"中文"/utf8>>, ai_mustache_rt:to_binary([20013, 25991])).

%% Unrenderable values are a template bug and say so instead of being
%% silently formatted with ~p.
to_binary_not_renderable_test() ->
    ?assertError({ai_mustache, {not_renderable, _}}, ai_mustache_rt:to_binary(self())),
    ?assertError({ai_mustache, {not_renderable, _}}, ai_mustache_rt:to_binary({a, b})).

%%%===================================================================
%%% escape/1
%%%===================================================================

flat(V) -> iolist_to_binary(ai_mustache_rt:escape(V)).

escape_ampersand_test() ->
    ?assertEqual(<<"&amp;">>, flat(<<"&">>)).

escape_lt_test() ->
    ?assertEqual(<<"&lt;">>, flat(<<"<">>)).

escape_gt_test() ->
    ?assertEqual(<<"&gt;">>, flat(<<">">>)).

escape_double_quote_test() ->
    ?assertEqual(<<"&quot;">>, flat(<<"\"">>)).

escape_single_quote_test() ->
    ?assertEqual(<<"&#39;">>, flat(<<"'">>)).

%% B3 behaviour change: these three used to be escaped and must not be.
escape_slash_is_untouched_test() ->
    ?assertEqual(<<"/a/b">>, flat(<<"/a/b">>)).

escape_equals_is_untouched_test() ->
    ?assertEqual(<<"a=b">>, flat(<<"a=b">>)).

escape_backtick_is_untouched_test() ->
    ?assertEqual(<<"`x`">>, flat(<<"`x`">>)).

escape_url_survives_test() ->
    U = <<"/x/y?a=1&b=2">>,
    ?assertEqual(<<"/x/y?a=1&amp;b=2">>, flat(U)).

%% Nothing to escape: the very same binary comes back, uncopied.
escape_zero_copy_test() ->
    B = <<"plain text, nothing special here">>,
    Got = ai_mustache_rt:escape(B),
    ?assert(Got =:= B),
    ?assert(erts_debug:same(Got, B)).

escape_empty_binary_test() ->
    B = <<>>,
    Got = ai_mustache_rt:escape(B),
    ?assert(Got =:= B),
    ?assert(erts_debug:same(Got, B)).

escape_mixed_test() ->
    ?assertEqual(<<"a&amp;b&lt;c&gt;d&quot;e&#39;f">>,
                 flat(<<"a&b<c>d\"e'f">>)).

escape_adjacent_specials_test() ->
    ?assertEqual(<<"&amp;&lt;&gt;">>, flat(<<"&<>">>)).

escape_leading_and_trailing_special_test() ->
    ?assertEqual(<<"&lt;mid&gt;">>, flat(<<"<mid>">>)),
    ?assertEqual(<<"tail&amp;">>, flat(<<"tail&">>)).

%% A single pass cannot double-escape, unlike the old chain of re:replace.
escape_is_not_applied_twice_test() ->
    ?assertEqual(<<"&amp;lt;">>, flat(<<"&lt;">>)).

%% Byte-wise scanning is safe for UTF-8: continuation bytes are all >= 0x80.
escape_utf8_passthrough_test() ->
    Cn = <<"中文 & 表情 🙂"/utf8>>,
    ?assertEqual(<<"中文 &amp; 表情 🙂"/utf8>>, flat(Cn)),
    Plain = <<"中文🙂"/utf8>>,
    ?assert(erts_debug:same(ai_mustache_rt:escape(Plain), Plain)).

escape_returns_iodata_test() ->
    ?assert(is_binary(ai_mustache_rt:escape(<<"plain">>))),
    ?assert(is_list(ai_mustache_rt:escape(<<"a<b">>))).

%% Non-binary input goes through to_binary/1 first.
escape_non_binary_input_test() ->
    ?assertEqual(<<"85">>, flat(85)),
    ?assertEqual(<<>>, flat(undefined)),
    ?assertEqual(<<>>, flat(null)),
    ?assertEqual(<<"1.21">>, flat(1.21)),
    ?assertEqual(<<"&amp;">>, flat('&')).

%%%===================================================================
%%% section/4
%%%===================================================================

%% Body helpers. `dot_body' renders the top frame, so it doubles as a probe
%% for whether the value was pushed; `depth_body' reports the stack depth.
dot_body() ->
    fun(S, I) -> [I, ai_mustache_rt:to_binary(
                       ai_mustache_rt:lookup([?AI_MUSTACHE_DOT], S))] end.

depth_body() ->
    fun(S, _I) -> integer_to_binary(length(S)) end.

boom_body() ->
    fun(_S, _I) -> erlang:error(body_must_not_run) end.

sec(V, Body) -> iolist_to_binary(ai_mustache_rt:section(V, Body, [#{}], <<>>)).

%% Row 1: the five falsy values run nothing at all. <<>> and false matter
%% most -- generated code has no explicit branch for them.
section_falsy_test() ->
    B = boom_body(),
    ?assertEqual([], ai_mustache_rt:section(undefined, B, [#{}], <<>>)),
    ?assertEqual([], ai_mustache_rt:section(false, B, [#{}], <<>>)),
    ?assertEqual([], ai_mustache_rt:section(null, B, [#{}], <<>>)),
    ?assertEqual([], ai_mustache_rt:section(<<>>, B, [#{}], <<>>)),
    ?assertEqual([], ai_mustache_rt:section([], B, [#{}], <<>>)).

%% Row 2: a non-empty list iterates, pushing each element.
section_list_pushes_each_element_test() ->
    Body = fun(S, _I) -> ai_mustache_rt:to_binary(ai_mustache_rt:lookup([n], S)) end,
    ?assertEqual(<<"12">>, sec([#{n => 1}, #{n => 2}], Body)).

section_list_of_scalars_test() ->
    ?assertEqual(<<"ab">>, sec([<<"a">>, <<"b">>], dot_body())).

%% Row 3: a map is pushed once.
section_map_pushes_once_test() ->
    Body = fun(S, _I) -> ai_mustache_rt:to_binary(ai_mustache_rt:lookup([n], S)) end,
    ?assertEqual(<<"7">>, sec(#{n => 7}, Body)),
    %% depth grows from 1 to 2: the map really was pushed
    ?assertEqual(<<"2">>, sec(#{n => 7}, depth_body())).

%% Row 4: `true' runs the body once WITHOUT pushing.
section_true_does_not_push_test() ->
    ?assertEqual(<<"1">>, sec(true, depth_body())),
    Body = fun(S, _I) -> ai_mustache_rt:to_binary(ai_mustache_rt:lookup([n], S)) end,
    ?assertEqual(<<"9">>,
                 iolist_to_binary(
                   ai_mustache_rt:section(true, Body, [#{n => 9}], <<>>))).

%% Row 5: fun/2 gets the rendered body as a binary plus the top frame.
section_fun2_test() ->
    Body = fun(_S, _I) -> [<<"in">>, [<<"ner">>]] end,
    F = fun(Rendered, Frame) ->
                ?assert(is_binary(Rendered)),
                ?assertEqual(#{k => v}, Frame),
                [<<"<">>, Rendered, <<">">>]
        end,
    ?assertEqual(<<"<inner>">>,
                 iolist_to_binary(
                   ai_mustache_rt:section(F, Body, [#{k => v}], <<>>))).

%% Row 6: the result of a fun/1 is dispatched recursively.
section_fun1_dispatches_result_test() ->
    Body = fun(S, _I) -> ai_mustache_rt:to_binary(ai_mustache_rt:lookup([n], S)) end,
    ?assertEqual(<<"12">>, sec(fun(_Frame) -> [#{n => 1}, #{n => 2}] end, Body)),
    ?assertEqual(<<"">>, sec(fun(_Frame) -> false end, boom_body())),
    ?assertEqual(<<"v">>, sec(fun(_Frame) -> <<"v">> end, dot_body())).

section_fun1_receives_top_frame_test() ->
    F = fun(Frame) -> maps:get(n, Frame) end,
    ?assertEqual(<<"3">>,
                 iolist_to_binary(
                   ai_mustache_rt:section(F, dot_body(), [#{n => 3}], <<>>))).

%% A fun/1 returning a fun/1 forever must be reported, not blow the stack.
section_fun1_depth_guard_test() ->
    F = fun Self(_Frame) -> Self end,
    ?assertError({ai_mustache, {lambda_depth_exceeded, _}},
                 ai_mustache_rt:section(F, dot_body(), [#{}], <<>>)).

%% Row 7: any other scalar is pushed so that {{.}} can reach it.
section_scalar_pushes_test() ->
    ?assertEqual(<<"v">>, sec(<<"v">>, dot_body())),
    ?assertEqual(<<"0">>, sec(0, dot_body())),
    ?assertEqual(<<"1.5">>, sec(1.5, dot_body())),
    ?assertEqual(<<"atom">>, sec(atom, dot_body())),
    ?assertEqual(<<"2">>, sec(<<"v">>, depth_body())).

%% The indent is handed straight to the body, once per iteration.
section_passes_indent_test() ->
    ?assertEqual(<<"  a  b">>,
                 iolist_to_binary(
                   ai_mustache_rt:section([<<"a">>, <<"b">>], dot_body(),
                                          [#{}], <<"  ">>))).

%%%===================================================================
%%% lambda/2
%%%===================================================================

lambda_undefined_test() ->
    ?assertEqual([], ai_mustache_rt:lambda(undefined, #{})).

lambda_fun1_test() ->
    F = fun(Frame) -> maps:get(who, Frame) end,
    ?assertEqual(<<"world">>,
                 iolist_to_binary(
                   ai_mustache_rt:lambda(F, #{who => <<"world">>}))).

lambda_fun2_with_value_test() ->
    F = fun(V, Frame) -> [V, maps:get(sep, Frame)] end,
    ?assertEqual(<<"a!">>,
                 iolist_to_binary(
                   ai_mustache_rt:lambda([F, <<"a">>], #{sep => <<"!">>}))).

lambda_bare_fun2_test() ->
    F = fun(V, _Frame) -> [<<"[">>, V, <<"]">>] end,
    ?assertEqual(<<"[]">>, iolist_to_binary(ai_mustache_rt:lambda(F, #{}))).

lambda_other_values_render_nothing_test() ->
    ?assertEqual([], ai_mustache_rt:lambda(<<"str">>, #{})),
    ?assertEqual([], ai_mustache_rt:lambda(null, #{})),
    ?assertEqual([], ai_mustache_rt:lambda(42, #{})),
    ?assertEqual([], ai_mustache_rt:lambda([1, 2], #{})).

%% Lambda output is HTML by design and must not be escaped.
lambda_output_is_not_escaped_test() ->
    F = fun(_Frame) -> <<"<b>&</b>">> end,
    ?assertEqual(<<"<b>&</b>">>,
                 iolist_to_binary(ai_mustache_rt:lambda(F, #{}))).

lambda_iolist_return_test() ->
    F = fun(_Frame) -> [<<"a">>, [<<"b">>], <<"c">>] end,
    ?assertEqual(<<"abc">>, iolist_to_binary(ai_mustache_rt:lambda(F, #{}))).

%% The frame handed to a lambda is the top of the stack, not the root
%% context -- a deliberate semantic change, see designs/03-semantics.md 5.
lambda_receives_given_frame_test() ->
    F = fun(Frame) -> ai_mustache_rt:to_binary(Frame) end,
    ?assertEqual(<<"top">>, iolist_to_binary(ai_mustache_rt:lambda(F, <<"top">>))).
