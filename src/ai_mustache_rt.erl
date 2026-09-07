%%%-------------------------------------------------------------------
%%% @doc Runtime support for compiled mustache templates.
%%%
%%% This module is the ONLY run-time dependency of generated code, and it
%%% depends on nothing but OTP (decision D5, see designs/02-architecture.md
%%% section 6 and architecture invariant #3). No processes, no ets, no
%%% persistent_term, no process dictionary: every function here is pure.
%%%
%%% The generated code expands the section dispatch table at compile time
%%% (designs/04-codegen.md section 4), so `section/4' only ever sees the
%%% cases that cannot be known statically -- most notably the return value
%%% of a `fun/1' lambda.
%%%
%%% Behaviour decisions taken here and worth knowing about:
%%%
%%%   * `to_binary/1' raises `{ai_mustache, {not_renderable, Term}}' for
%%%     values it cannot render (pid, ref, fun, tuple, ...) rather than
%%%     silently formatting them with `~p'. A template that interpolates
%%%     such a value is a bug and should say so loudly.
%%%   * `to_binary/1' treats a list as a unicode character list / iolist,
%%%     see the comment on that clause for the ambiguity involved.
%%%   * `escape/1' escapes exactly `&amp; &lt; &gt; &quot; &#39;' and returns
%%%     the argument binary itself, uncopied, when nothing needs escaping.
%%%
%%% See designs/03-semantics.md, designs/04-codegen.md section 5,
%%% tasks/T11.md and tasks/T12.md.
%%% @end
%%%-------------------------------------------------------------------
-module(ai_mustache_rt).

-include("ai_mustache.hrl").

-export([lookup/2, escape/1, to_binary/1, truthy/1, section/4, lambda/2]).

%% A `fun/1' section may return another `fun/1'; that is lazy evaluation, not
%% an error, but it must not be allowed to recurse forever.
-define(MAX_LAMBDA_DEPTH, 16).

%%%===================================================================
%%% lookup/2 -- context stack resolution
%%%===================================================================

%% @doc Resolve a dotted key path against the context stack.
%%
%% The first segment walks the stack from the top outwards and resolves in
%% the first frame that is a map and holds that key. Every following segment
%% descends strictly inside that value and never walks the stack again --
%% `{{a.b}}' means "find `a' anywhere, then take `b' from it".
%%
%% `[?AI_MUSTACHE_DOT]' is the implicit iterator `{{.}}' and yields the top
%% frame itself. It is matched first on purpose: the old implementation
%% treated `'.'' as an ordinary key and looked it up in the map, which could
%% never succeed (bug B5).
%%
%% Frames are not necessarily maps -- a section pushes scalars as-is and the
%% root context may be a bare integer -- so non-map frames are skipped rather
%% than crashing. Anything not found is `undefined'.
-spec lookup(ai_mustache_keys(), Stack :: [term()]) -> term().
lookup([?AI_MUSTACHE_DOT], [Top | _]) -> Top;
lookup([?AI_MUSTACHE_DOT], [])        -> undefined;
lookup([K | Rest], Stack) ->
    case find_frame(K, Stack) of
        {ok, V} -> descend(Rest, V);
        error   -> undefined
    end;
lookup([], [Top | _]) -> Top;
lookup([], [])        -> undefined.

%% `#{K := V}' rather than maps:get/3: a single match, and no ambiguity with
%% a stored value that happens to be `undefined'.
-spec find_frame(ai_mustache_key(), [term()]) -> {ok, term()} | error.
find_frame(_K, []) -> error;
find_frame(K, [F | Rest]) when is_map(F) ->
    case F of
        #{K := V} -> {ok, V};
        _         -> find_frame(K, Rest)
    end;
find_frame(K, [_NonMap | Rest]) -> find_frame(K, Rest).

-spec descend(ai_mustache_keys(), term()) -> term().
descend([], V) -> V;
descend([K | Rest], M) when is_map(M) ->
    case M of
        #{K := V} -> descend(Rest, V);
        _         -> undefined
    end;
descend(_Keys, _NotAMap) -> undefined.

%%%===================================================================
%%% truthy/1
%%%===================================================================

%% @doc The falsy set is exactly these five values.
%%
%% Everything else is true, notably `0', `0.0' and `#{}'. This matches
%% standard mustache and is a well known foot-gun, see
%% designs/03-semantics.md section 2.1.
%%
%% Written as separate clauses rather than a `case' or `lists:member/2' so
%% the emulator can generate a jump table.
-spec truthy(term()) -> boolean().
truthy(undefined) -> false;
truthy(false)     -> false;
truthy([])        -> false;
truthy(<<>>)      -> false;
truthy(null)      -> false;
truthy(_)         -> true.

%%%===================================================================
%%% to_binary/1
%%%===================================================================

%% @doc Render a value as a binary.
%%
%% Floats use `float_to_binary/2' with `short' (OTP 24+): the default
%% formatting turns `1.21' into `"1.21000000000000000000e+00"' and fails the
%% spec's decimal interpolation cases (designs/02-architecture.md 6.2).
%%
%% `undefined' and `null' render as the empty binary -- a missing variable
%% renders as nothing, which is standard mustache -- so both clauses have to
%% sit above the generic atom clause.
-spec to_binary(term()) -> binary().
to_binary(B) when is_binary(B)  -> B;          % zero copy, the hot path
to_binary(I) when is_integer(I) -> integer_to_binary(I);
to_binary(F) when is_float(F)   -> float_to_binary(F, [short]);
to_binary(undefined)            -> <<>>;
to_binary(null)                 -> <<>>;
to_binary(A) when is_atom(A)    -> atom_to_binary(A, utf8);
%% Lists are ambiguous by nature: `[104, 105]' is both a two element list of
%% integers and the string "hi", and nothing in the value itself tells the
%% two apart. Templates are not supposed to interpolate lists at all -- a
%% list means "section" everywhere else in this engine -- but a defined
%% behaviour beats a crash, so a list is read as a unicode character list,
%% falling back to a plain iolist (byte list) when that fails.
to_binary(L) when is_list(L) ->
    case unicode:characters_to_binary(L) of
        B when is_binary(B) -> B;
        _Error              -> iolist_to_binary(L)
    end;
to_binary(T) -> error({ai_mustache, {not_renderable, T}}).

%%%===================================================================
%%% escape/1
%%%===================================================================

%% @doc HTML-escape a value, returning iodata.
%%
%% The escape set is exactly five characters:
%%
%%   `&' -> `&amp;'   `<' -> `&lt;'   `>' -> `&gt;'
%%   `"' -> `&quot;'  `'' -> `&#39;'
%%
%% Two deliberate differences from the pre-0.4 behaviour:
%%
%%   1. `&' becomes `&amp;' WITH the semicolon. The old ailib table emitted
%%      `&amp' (bug B3).
%%   2. `/', `=' and the backtick are no longer escaped. Escaping them
%%      corrupts URLs (`href="/a/b"' became `href="&#x2F;a&#x2F;b"') and
%%      ordinary prose; standard mustache only asks for the five above.
%%
%% One pass over the binary, slicing with binary:part/3 and building an
%% iolist -- no `re:replace', no byte-wise append. Beyond being far cheaper
%% than the old eight global regex passes, a single pass is also what makes
%% the escape order irrelevant: with multiple passes, `&' having to run
%% first or last is a correctness trap (`&lt;' getting re-escaped into
%% `&amp;lt;'), and here it simply cannot happen.
%%
%% Scanning byte by byte is safe for UTF-8: all five characters are ASCII
%% and every continuation byte of a multi-byte sequence is >= 0x80, so no
%% multi-byte character can be hit by accident. No decoding is needed.
-spec escape(term()) -> iodata().
escape(B) when is_binary(B) -> escape(B, 0, 0, []);
escape(V)                   -> escape(to_binary(V)).

%% escape(Bin, SegStart, Pos, RevAcc): `SegStart' is where the current
%% verbatim run started, `Pos' the byte being looked at, `RevAcc' the output
%% in reverse order.
-spec escape(binary(), non_neg_integer(), non_neg_integer(), [iodata()]) ->
          iodata().
escape(Bin, Start, Pos, Acc) when Pos < byte_size(Bin) ->
    case binary:at(Bin, Pos) of
        $&  -> escape_hit(Bin, Start, Pos, Acc, <<"&amp;">>);
        $<  -> escape_hit(Bin, Start, Pos, Acc, <<"&lt;">>);
        $>  -> escape_hit(Bin, Start, Pos, Acc, <<"&gt;">>);
        $\" -> escape_hit(Bin, Start, Pos, Acc, <<"&quot;">>);
        $'  -> escape_hit(Bin, Start, Pos, Acc, <<"&#39;">>);
        _   -> escape(Bin, Start, Pos + 1, Acc)
    end;
%% Nothing was escaped: hand back the very same binary, no allocation. This
%% is why escape/1 returns iodata() and not binary().
escape(Bin, 0, _Pos, []) -> Bin;
escape(_Bin, Pos, Pos, Acc) -> lists:reverse(Acc);
escape(Bin, Start, Pos, Acc) ->
    lists:reverse(Acc, [binary:part(Bin, Start, Pos - Start)]).

-spec escape_hit(binary(), non_neg_integer(), non_neg_integer(), [iodata()],
                 binary()) -> iodata().
escape_hit(Bin, Pos, Pos, Acc, Rep) ->
    escape(Bin, Pos + 1, Pos + 1, [Rep | Acc]);
escape_hit(Bin, Start, Pos, Acc, Rep) ->
    Seg = binary:part(Bin, Start, Pos - Start),
    escape(Bin, Pos + 1, Pos + 1, [Rep, Seg | Acc]).

%%%===================================================================
%%% section/4
%%%===================================================================

%% @doc Run-time fallback for the `{{#x}}' dispatch table.
%%
%% Generated code expands this table into a `case' at compile time and only
%% calls back here for what it cannot decide statically: the value returned
%% by a `fun/1' section, which is dispatched recursively.
%%
%% `BodyFun' is the generated `sec_N_body/2', i.e. fun((Stack, Indent) ->
%% iodata()).
%%
%%   falsy         -> `[]', body not run
%%   non-empty list-> body once per element, element pushed
%%   map           -> body once, map pushed
%%   `true'        -> body once, nothing pushed
%%   `fun/2'       -> `F(iolist_to_binary(Body(Stack, Indent)), hd(Stack))'
%%   `fun/1'       -> dispatch `F(hd(Stack))' recursively
%%   other scalar  -> body once, value pushed so `{{.}}' works
%%
%% The falsy clauses MUST come first: `<<>>', `false', `undefined' and
%% `null' are neither list nor map and would otherwise reach the scalar
%% clause and get their body rendered.
%%
%% Note that the lambda receives the TOP FRAME, not the whole stack and not
%% the flat global context of v0.3.x (designs/03-semantics.md section 5).
-spec section(Value :: term(), BodyFun :: fun((list(), binary()) -> iodata()),
              Stack :: [term()], Indent :: binary()) -> iodata().
section(Value, BodyFun, Stack, Indent) ->
    section(Value, BodyFun, Stack, Indent, ?MAX_LAMBDA_DEPTH).

-spec section(term(), fun((list(), binary()) -> iodata()), [term()], binary(),
              non_neg_integer()) -> iodata().
section(undefined, _Body, _Stack, _Indent, _D) -> [];
section(false,     _Body, _Stack, _Indent, _D) -> [];
section(null,      _Body, _Stack, _Indent, _D) -> [];
section(<<>>,      _Body, _Stack, _Indent, _D) -> [];
section([],        _Body, _Stack, _Indent, _D) -> [];
section(L, Body, Stack, Indent, _D) when is_list(L) ->
    [Body([E | Stack], Indent) || E <- L];
section(M, Body, Stack, Indent, _D) when is_map(M) ->
    Body([M | Stack], Indent);
section(true, Body, Stack, Indent, _D) ->
    Body(Stack, Indent);
section(F, Body, Stack, Indent, _D) when is_function(F, 2) ->
    F(iolist_to_binary(Body(Stack, Indent)), top(Stack));
section(F, Body, Stack, Indent, D) when is_function(F, 1), D > 0 ->
    section(F(top(Stack)), Body, Stack, Indent, D - 1);
section(F, _Body, _Stack, _Indent, _D) when is_function(F, 1) ->
    error({ai_mustache, {lambda_depth_exceeded, ?MAX_LAMBDA_DEPTH}});
section(V, Body, Stack, Indent, _D) ->
    Body([V | Stack], Indent).

%% hd/1 of the stack, tolerating the empty stack that render_stack/2 never
%% actually produces.
-spec top([term()]) -> term().
top([Top | _]) -> Top;
top([])        -> undefined.

%%%===================================================================
%%% lambda/2
%%%===================================================================

%% @doc Evaluate the `{{*f}}' extension tag against the current frame.
%%
%% Two shapes are supported (designs/03-semantics.md section 5):
%%
%%   `#{yield => fun(Frame) -> ... end}'              -> `F(Frame)'
%%   `#{yield => [fun render_layout/2, <<"index">>]}' -> `F(Value, Frame)'
%%
%% The return value may be a binary or an iolist and is NOT escaped --
%% emitting HTML is the entire point of a lambda. A missing key, or anything
%% that is not one of the shapes above, renders as nothing; the old runner
%% likewise skipped the instruction.
-spec lambda(Value :: term(), Frame :: term()) -> iodata().
lambda(undefined, _Frame) -> [];
lambda([F, V], Frame) when is_function(F, 2) -> F(V, Frame);
lambda(F, Frame) when is_function(F, 1) -> F(Frame);
%% A bare fun/2 has no value to pass, so it gets the empty binary.
lambda(F, Frame) when is_function(F, 2) -> F(<<>>, Frame);
lambda(_V, _Frame) -> [].
