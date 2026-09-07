%%%-------------------------------------------------------------------
%%% @doc Runtime support for compiled jinja templates.
%%%
%%% Together with ai_jinja_filters and ai_jinja_tests this is the whole
%%% run-time dependency of generated code, and it depends on nothing but OTP
%%% and ai_html_escape. No processes, no ets, no persistent_term, no process
%%% dictionary (architecture invariant 1).
%%%
%%% == Not a copy of ai_mustache_rt ==
%%%
%%% `resolve/2' looks like `ai_mustache_rt:lookup/2' and means something
%%% different: mustache walks a stack of DATA, jinja walks a stack of
%%% lexical SCOPES. `truthy/1' differs outright -- 0 and #{} are false here
%%% and true there. Sharing either would make one of the two engines quietly
%%% wrong (designs/10-jinja-semantics.md sections 2 and 5).
%%%
%%% == Output形式 follows the reference implementation ==
%%%
%%% `to_binary/1' renders booleans as `True'/`False' and containers with
%%% Python's repr, because that is what a template author porting from jinja2
%%% expects to see. See designs/10-jinja-semantics.md section 1.1.
%%% @end
%%%-------------------------------------------------------------------
-module(ai_jinja_rt).

-include("ai_jinja.hrl").

%% Scope
-export([resolve/2, bind/3, push/2, push2/5, root/1, new_scope/1,
         globals/1, arg/5, varargs/2, kwargs/2]).
%% Values
-export([truthy/1, to_binary/1, escape/1, safe/1, unsafe/1, repr/1]).
%% Access
-export([attr/2, subscript/2, slice/4, items/1, keys/1, values/1]).
%% Operators
-export(['add'/2, 'sub'/2, 'mul'/2, 'divide'/2, 'floordiv'/2, 'mod'/2,
         'pow'/2, 'concat'/2,
         'eq'/2, 'ne'/2, 'lt'/2, 'le'/2, 'gt'/2, 'ge'/2, 'contains'/2,
         binop/3]).
%% Iteration
-export([to_list/1, loop/5, changed/3]).
%% Calls and builtins
-export([call/3, apply_named/3, apply_named/4, is_builtin/1, builtin/3]).
-export([resolve_or_builtin/2]).
-export([range/1, range/2, range/3, dict/1, namespace/1]).

-define(TAG, ai_jinja).

%%%===================================================================
%%% Scope
%%%===================================================================

%% A scope is a stack of frames, innermost first, with the render context at
%% the bottom. The bottom frame is whatever the caller passed and need not be
%% a map, so non-map frames are skipped rather than crashing.
-spec new_scope(term()) -> [term()].
new_scope(Ctx) -> [#{}, Ctx].

-spec resolve(atom(), [term()]) -> term().
resolve(_Name, []) -> undefined;
resolve(Name, [F | Rest]) when is_map(F) ->
    case F of
        #{Name := V} -> V;
        _            -> resolve(Name, Rest)
    end;
resolve(Name, [_NonMap | Rest]) -> resolve(Name, Rest).

%% {% set %} rebinds in the CURRENT frame rather than pushing a new one:
%% pushing would make the scope grow without bound in a loop that sets on
%% every iteration, and would change what an enclosing {% set %} of the same
%% name means.
-spec bind([term()], atom(), term()) -> [term()].
bind([Top | Rest], Name, V) when is_map(Top) -> [Top#{Name => V} | Rest];
bind(Scope, Name, V)                         -> [#{Name => V} | Scope].

-spec push([term()], map()) -> [term()].
push(Scope, Frame) -> [Frame | Scope].

%% A for loop pushes rather than binds, so that a {% set %} inside the body
%% lands in the loop's own frame and is gone on the next iteration -- which is
%% exactly the scoping rule (designs/10-jinja-semantics.md 2.4). Two bindings
%% in one push because a loop does this on every iteration.
-spec push2([term()], atom(), term(), atom(), term()) -> [term()].
push2(Scope, N1, V1, N2, V2) -> [#{N1 => V1, N2 => V2} | Scope].

%% @doc The template-level scope: the render context and the frame that
%% top-level {% set %} writes into, with every local frame dropped.
%%
%% This is what a macro is called with. The reference implementation captures
%% the scope as it stood where the macro was DEFINED; reproducing that would
%% mean passing a closure into a module-level function, so a macro here sees
%% template-level names regardless of definition order (deviation J17).
-spec globals([term()]) -> [term()].
globals(Scope) ->
    case length(Scope) of
        N when N =< 2 -> Scope;
        N             -> lists:nthtail(N - 2, Scope)
    end.

%%%===================================================================
%%% Macro arguments
%%%===================================================================

%% @doc One macro parameter: keyword first, then position, then the default.
%%
%% The binding happens inside the macro rather than at the call site because
%% only the macro knows its own parameter names, and a macro reached through
%% {% import %} lives in another module.
-spec arg(map(), [term()], non_neg_integer(), atom(), term()) -> term().
arg(Args, Positional, Index, Name, Default) ->
    case Args of
        #{Name := V} -> V;
        _ ->
            case Index < length(Positional) of
                true  -> lists:nth(Index + 1, Positional);
                false -> Default
            end
    end.

-spec varargs([term()], non_neg_integer()) -> [term()].
varargs(Positional, From) ->
    case From < length(Positional) of
        true  -> lists:nthtail(From, Positional);
        false -> []
    end.

-spec kwargs(map(), [atom()]) -> map().
kwargs(Args, Named) -> maps:without(['$positional', caller | Named], Args).

%% The render context alone, for {% include ... without context %}.
-spec root([term()]) -> term().
root([Ctx])      -> Ctx;
root([_ | Rest]) -> root(Rest);
root([])         -> undefined.

%%%===================================================================
%%% Truth
%%%===================================================================

%% Python's rule, not mustache's: 0, 0.0, #{} and {} are all false.
-spec truthy(term()) -> boolean().
truthy(undefined) -> false;
truthy(false)     -> false;
truthy(null)      -> false;
truthy(true)      -> true;
truthy(0)         -> false;
truthy(+0.0)      -> false;
truthy([])        -> false;
truthy(<<>>)      -> false;
truthy({safe, D}) -> truthy(iolist_to_binary(D));
truthy(M) when is_map(M)   -> map_size(M) > 0;
truthy(T) when is_tuple(T) -> tuple_size(T) > 0;
truthy(_)         -> true.

%%%===================================================================
%%% Rendering
%%%===================================================================

-spec escape(term()) -> iodata().
escape({safe, D}) -> D;
escape(V)         -> ai_html_escape:escape(to_binary(V), ?TAG, <<"&#34;">>).

-spec safe(term()) -> {safe, iodata()}.
safe({safe, _} = S) -> S;
safe(V)             -> {safe, to_binary(V)}.

%% Strip the safe marker, for a filter that has to look at the text itself.
-spec unsafe(term()) -> term().
unsafe({safe, D}) -> iolist_to_binary(D);
unsafe(V)         -> V.

%% Top level rendering. A bare string prints itself; everything else prints
%% the way Python's str/1 would, which is what the reference implementation
%% produces.
-spec to_binary(term()) -> binary().
to_binary(B) when is_binary(B)  -> B;
to_binary({safe, D})            -> iolist_to_binary(D);
to_binary(undefined)            -> <<>>;
to_binary(null)                 -> <<>>;
to_binary(true)                 -> <<"True">>;
to_binary(false)                -> <<"False">>;
%% A bare atom is a map key or an enumeration value; printing it as a quoted
%% string would make `{% for k in mapping %}{{ k }}{% endfor %}' render
%% `'a'' instead of `a'. Inside a container repr/1 quotes it, exactly as
%% Python quotes a string there.
to_binary(A) when is_atom(A)    -> atom_to_binary(A, utf8);
to_binary(V)                    -> repr(V).

%% Python's repr, as far as the value domain reaches. Used both for top-level
%% output of non-strings and for the elements of a container, which is why
%% strings gain quotes here but not in to_binary/1.
-spec repr(term()) -> binary().
repr(true)                     -> <<"True">>;
repr(false)                    -> <<"False">>;
repr(undefined)                -> <<"None">>;
repr(null)                     -> <<"None">>;
repr(I) when is_integer(I)     -> integer_to_binary(I);
repr(F) when is_float(F)       -> pyfloat(F);
repr(B) when is_binary(B)      -> quote(B);
repr({safe, D})                -> quote(iolist_to_binary(D));
repr(L) when is_list(L)        -> wrap($[, $], [repr(E) || E <- L]);
repr({ns, M})                  -> repr(M);
repr(T) when is_tuple(T)       -> tuple_repr(tuple_to_list(T));
repr(M) when is_map(M) ->
    wrap(${, $}, [<<(repr(K))/binary, ": ", (repr(V))/binary>>
                  || {K, V} <- sorted(M)]);
repr(A) when is_atom(A)        -> quote(atom_to_binary(A, utf8));
repr(V)                        -> error({?TAG, {not_renderable, V}}).

%% Python prints a float positionally unless the exponent leaves the range
%% [-4, 16); Erlang's `short' switches to exponent notation far earlier, so
%% `1e3' would render as `1.0e3' instead of `1000.0'.
-spec pyfloat(float()) -> binary().
pyfloat(F) ->
    S = float_to_binary(F, [short]),
    case binary:split(S, <<"e">>) of
        [_]         -> S;
        [Mant, Exp] -> reformat(Mant, binary_to_integer(Exp))
    end.

reformat(Mant, Exp) when Exp >= 16; Exp < -4 ->
    %% Python's own exponent form: no trailing `.0' on the mantissa and a
    %% signed, at-least-two-digit exponent.
    Sign = case Exp < 0 of true -> <<"-">>; false -> <<"+">> end,
    Digits = integer_to_binary(abs(Exp)),
    Padded = case byte_size(Digits) of
                 1 -> <<"0", Digits/binary>>;
                 _ -> Digits
             end,
    <<(strip_dot_zero(Mant))/binary, "e", Sign/binary, Padded/binary>>;
reformat(Mant, Exp) ->
    {Sign, Rest} = case Mant of
                       <<"-", R/binary>> -> {<<"-">>, R};
                       _                 -> {<<>>, Mant}
                   end,
    [Int, Frac] = binary:split(Rest, <<".">>),
    Digits = <<Int/binary, Frac/binary>>,
    Point = byte_size(Int) + Exp,
    <<Sign/binary, (place_point(Digits, Point))/binary>>.

place_point(Digits, Point) when Point =< 0 ->
    <<"0.", (binary:copy(<<"0">>, -Point))/binary, Digits/binary>>;
place_point(Digits, Point) when Point >= byte_size(Digits) ->
    <<Digits/binary, (binary:copy(<<"0">>, Point - byte_size(Digits)))/binary, ".0">>;
place_point(Digits, Point) ->
    <<(binary:part(Digits, 0, Point))/binary, ".",
      (binary:part(Digits, Point, byte_size(Digits) - Point))/binary>>.

strip_dot_zero(B) ->
    case binary:match(B, <<".0">>) of
        {P, 2} when P + 2 =:= byte_size(B) -> binary:part(B, 0, P);
        _                                  -> B
    end.

%% A one element tuple is `(x,)' in Python, and the trailing comma matters:
%% without it the fixture for tuple output would silently accept `(x)'.
tuple_repr([One]) -> <<"(", (repr(One))/binary, ",)">>;
tuple_repr(L)     -> wrap($(, $), [repr(E) || E <- L]).

wrap(Open, Close, Parts) ->
    iolist_to_binary([Open, lists:join(<<", ">>, Parts), Close]).

%% Erlang gives no insertion order for maps, so a deterministic one is chosen.
%% Without this the rendering of a map would depend on the map's internal
%% layout and change as it grows past 32 keys.
sorted(M) -> lists:sort(maps:to_list(M)).

%% Python picks the quote that needs least escaping.
quote(B) ->
    HasSingle = binary:match(B, <<"'">>) =/= nomatch,
    HasDouble = binary:match(B, <<"\"">>) =/= nomatch,
    Q = case {HasSingle, HasDouble} of
            {true, false} -> $";
            _             -> $'
        end,
    iolist_to_binary([Q, [esc_char(C, Q) || <<C/utf8>> <= B], Q]).

esc_char($\\, _Q) -> <<"\\\\">>;
esc_char($\n, _Q) -> <<"\\n">>;
esc_char($\t, _Q) -> <<"\\t">>;
esc_char($\r, _Q) -> <<"\\r">>;
esc_char(Q, Q)    -> <<$\\, Q>>;
esc_char(C, _Q)   -> <<C/utf8>>.

%%%===================================================================
%%% Attribute and subscript access
%%%===================================================================

%% Undefined is chainable: a.b.c never raises, it just stays undefined
%% (deviation J14). Making it raise, as the reference implementation's default
%% Undefined does, turns a typo deep in a template into a 500.
-spec attr(term(), atom()) -> term().
attr(M, K) when is_map(M)  -> maps:get(K, M, undefined);
attr({ns, M}, K)           -> maps:get(K, M, undefined);
attr(undefined, _K)        -> undefined;
attr(null, _K)             -> undefined;
attr(_V, _K)               -> undefined.

-spec subscript(term(), term()) -> term().
subscript(undefined, _)              -> undefined;
subscript(L, I) when is_list(L), is_integer(I) -> nth(L, I);
subscript(T, I) when is_tuple(T), is_integer(I) -> nth(tuple_to_list(T), I);
subscript(B, I) when is_binary(B), is_integer(I) -> char_at(B, I);
subscript(M, K) when is_map(M), is_atom(K) -> maps:get(K, M, undefined);
subscript(M, K) when is_map(M), is_binary(K) -> maps:get(key_atom(K), M, undefined);
subscript(M, K) when is_map(M) -> maps:get(K, M, undefined);
subscript({ns, M}, K)          -> subscript(M, K);
subscript(_V, _K)              -> undefined.

%% binary_to_existing_atom/2, never binary_to_atom/2: a subscript key can come
%% straight from template data, and creating atoms from it would be an
%% unbounded atom table (designs/10-jinja-semantics.md section 1).
key_atom(B) ->
    try binary_to_existing_atom(B, utf8)
    catch error:badarg -> '$no_such_key' end.

nth(L, I) when I < 0 ->
    N = length(L),
    case N + I of
        J when J >= 0 -> lists:nth(J + 1, L);
        _             -> undefined
    end;
nth(L, I) ->
    case I < length(L) of
        true  -> lists:nth(I + 1, L);
        false -> undefined
    end.

char_at(B, I) ->
    case unicode:characters_to_list(B, utf8) of
        Chars when is_list(Chars) ->
            case nth(Chars, I) of
                undefined -> undefined;
                C         -> unicode:characters_to_binary([C], utf8)
            end;
        _ -> undefined
    end.

-spec slice(term(), term(), term(), term()) -> term().
slice(undefined, _, _, _) -> undefined;
slice(B, From, To, Step) when is_binary(B) ->
    case unicode:characters_to_list(B, utf8) of
        Chars when is_list(Chars) ->
            unicode:characters_to_binary(do_slice(Chars, From, To, Step), utf8);
        _ -> undefined
    end;
slice(T, From, To, Step) when is_tuple(T) ->
    list_to_tuple(do_slice(tuple_to_list(T), From, To, Step));
slice(L, From, To, Step) when is_list(L) ->
    do_slice(L, From, To, Step);
slice(_V, _, _, _) -> undefined.

do_slice(L, From, To, Step) ->
    N = length(L),
    St = case Step of undefined -> 1; S when is_integer(S), S =/= 0 -> S;
             _ -> error({?TAG, {unsupported_operands, slice, Step, 0}}) end,
    {A, B} = slice_bounds(N, From, To, St),
    Picked = [E || {I, E} <- lists:zip(lists:seq(0, N - 1), L),
                   in_slice(I, A, B, St)],
    case St < 0 of
        true  -> lists:reverse(Picked);
        false -> Picked
    end.

slice_bounds(N, From, To, Step) when Step > 0 ->
    {clamp(N, From, 0), clamp(N, To, N)};
slice_bounds(N, From, To, _Step) ->
    {clamp(N, To, -1), clamp(N, From, N - 1)}.

clamp(_N, undefined, Default) -> Default;
clamp(N, I, _Default) when I < 0 -> max(N + I, 0);
clamp(N, I, _Default) -> min(I, N).

in_slice(I, A, B, Step) when Step > 0 ->
    I >= A andalso I < B andalso (I - A) rem Step =:= 0;
in_slice(I, A, B, Step) ->
    I > A andalso I =< B andalso (B - I) rem (-Step) =:= 0.

%% The three mapping methods the reference implementation offers. Erlang maps
%% have no methods, so these are recognised by the compiler and routed here
%% rather than going through attr/2 and a call (deviation J13).
-spec items(term()) -> [{term(), term()}].
items(M) when is_map(M) -> sorted(M);
items({ns, M})          -> sorted(M);
items(undefined)        -> [];
items(V)                -> error({?TAG, {not_iterable, V}}).

-spec keys(term()) -> [term()].
keys(M) when is_map(M) -> lists:sort(maps:keys(M));
keys({ns, M})          -> lists:sort(maps:keys(M));
keys(undefined)        -> [];
keys(V)                -> error({?TAG, {not_iterable, V}}).

-spec values(term()) -> [term()].
values(M) when is_map(M) -> [V || {_, V} <- sorted(M)];
values({ns, M})          -> [V || {_, V} <- sorted(M)];
values(undefined)        -> [];
values(V)                -> error({?TAG, {not_iterable, V}}).

%%%===================================================================
%%% Operators
%%%===================================================================

%% Dispatch for the constant folder, which has an operator atom in hand rather
%% than a call site. Everything else calls the individual functions directly.
-spec binop(atom(), term(), term()) -> term().
binop('+', A, B)      -> add(A, B);
binop('-', A, B)      -> sub(A, B);
binop('*', A, B)      -> mul(A, B);
binop('/', A, B)      -> divide(A, B);
binop('//', A, B)     -> floordiv(A, B);
binop('%', A, B)      -> mod(A, B);
binop('**', A, B)     -> pow(A, B);
binop('~', A, B)      -> concat(A, B);
binop('==', A, B)     -> eq(A, B);
binop('!=', A, B)     -> ne(A, B);
binop('<', A, B)      -> lt(A, B);
binop('<=', A, B)     -> le(A, B);
binop('>', A, B)      -> gt(A, B);
binop('>=', A, B)     -> ge(A, B);
binop('in', A, B)     -> contains(B, A);
binop('not in', A, B) -> not contains(B, A).

%% `+' is overloaded exactly as in Python: numbers add, strings and lists
%% concatenate, and a mixture is an error rather than a silent coercion.
-spec add(term(), term()) -> term().
add(A, B) when is_number(A), is_number(B) -> A + B;
add(A, B) when is_binary(A), is_binary(B) -> <<A/binary, B/binary>>;
add(A, B) when is_list(A), is_list(B)     -> A ++ B;
add(A, B) -> bad('+', A, B).

-spec sub(term(), term()) -> term().
sub(A, B) when is_number(A), is_number(B) -> A - B;
sub(A, B) -> bad('-', A, B).

-spec mul(term(), term()) -> term().
mul(A, B) when is_number(A), is_number(B) -> A * B;
mul(A, B) when is_binary(A), is_integer(B) -> binary:copy(A, max(B, 0));
mul(A, B) when is_integer(A), is_binary(B) -> binary:copy(B, max(A, 0));
mul(A, B) when is_list(A), is_integer(B) -> lists:append(lists:duplicate(max(B, 0), A));
mul(A, B) -> bad('*', A, B).

%% True division, so 1/2 is 0.5 -- Erlang's `/' already agrees.
-spec divide(term(), term()) -> number().
divide(_A, 0)   -> error({?TAG, division_by_zero});
divide(_A, +0.0) -> error({?TAG, division_by_zero});
divide(A, B) when is_number(A), is_number(B) -> A / B;
divide(A, B) -> bad('/', A, B).

-spec floordiv(term(), term()) -> number().
floordiv(_A, 0)   -> error({?TAG, division_by_zero});
floordiv(_A, +0.0) -> error({?TAG, division_by_zero});
floordiv(A, B) when is_integer(A), is_integer(B) -> (A - mod(A, B)) div B;
floordiv(A, B) when is_number(A), is_number(B)   -> float(trunc(math:floor(A / B)));
floordiv(A, B) -> bad('//', A, B).

%% Python's modulo, which takes the sign of the divisor: -7 % 3 is 2, whereas
%% Erlang's rem gives -1.
-spec mod(term(), term()) -> number().
mod(_A, 0)   -> error({?TAG, division_by_zero});
mod(_A, +0.0) -> error({?TAG, division_by_zero});
mod(A, B) when is_integer(A), is_integer(B) ->
    case A rem B of
        R when R =/= 0, (R < 0) =/= (B < 0) -> R + B;
        R -> R
    end;
mod(A, B) when is_number(A), is_number(B) ->
    R = math:fmod(A, B),
    case R =/= +0.0 andalso (R < 0) =/= (B < 0) of
        true  -> R + B;
        false -> R
    end;
mod(A, B) -> bad('%', A, B).

-spec pow(term(), term()) -> number().
pow(A, B) when is_integer(A), is_integer(B), B >= 0 -> ipow(A, B, 1);
pow(A, B) when is_number(A), is_number(B)           -> math:pow(A, B);
pow(A, B) -> bad('**', A, B).

ipow(_A, 0, Acc) -> Acc;
ipow(A, N, Acc) when N band 1 =:= 1 -> ipow(A * A, N bsr 1, Acc * A);
ipow(A, N, Acc)                     -> ipow(A * A, N bsr 1, Acc).

%% `~' never fails: it renders both sides and joins them.
-spec concat(term(), term()) -> binary().
concat(A, B) -> <<(to_binary(A))/binary, (to_binary(B))/binary>>.

-spec eq(term(), term()) -> boolean().
eq(A, B) -> cmp_eq(A, B).
-spec ne(term(), term()) -> boolean().
ne(A, B) -> not cmp_eq(A, B).

cmp_eq(A, B) when is_number(A), is_number(B) -> A == B;
cmp_eq({safe, A}, B) -> cmp_eq(iolist_to_binary(A), B);
cmp_eq(A, {safe, B}) -> cmp_eq(A, iolist_to_binary(B));
cmp_eq(A, B)         -> A =:= B.

%% Ordering only between values Python would compare. Erlang would happily
%% answer `1 < <<"a">>' by its own term order, which is not a useful answer to
%% give a template author.
-spec lt(term(), term()) -> boolean().
lt(A, B) -> order(A, B, '<').
-spec le(term(), term()) -> boolean().
le(A, B) -> order(A, B, '=<').
-spec gt(term(), term()) -> boolean().
gt(A, B) -> order(A, B, '>').
-spec ge(term(), term()) -> boolean().
ge(A, B) -> order(A, B, '>=').

order(A, B, Op) when is_number(A), is_number(B) -> apply_order(Op, A, B);
order(A, B, Op) when is_binary(A), is_binary(B) -> apply_order(Op, A, B);
order(A, B, Op) when is_list(A), is_list(B)     -> apply_order(Op, A, B);
order(A, B, Op) when is_tuple(A), is_tuple(B)   -> apply_order(Op, A, B);
order(A, B, Op) -> bad(Op, A, B).

apply_order('<', A, B)  -> A < B;
apply_order('=<', A, B) -> A =< B;
apply_order('>', A, B)  -> A > B;
apply_order('>=', A, B) -> A >= B.

-spec contains(term(), term()) -> boolean().
contains(undefined, _V)             -> false;
contains(L, V) when is_list(L)      -> lists:any(fun(E) -> cmp_eq(E, V) end, L);
contains(T, V) when is_tuple(T)     -> contains(tuple_to_list(T), V);
contains(M, V) when is_map(M), is_atom(V)   -> maps:is_key(V, M);
contains(M, V) when is_map(M), is_binary(V) -> maps:is_key(key_atom(V), M);
contains(M, V) when is_map(M)       -> maps:is_key(V, M);
contains(B, V) when is_binary(B), is_binary(V) -> binary:match(B, V) =/= nomatch;
contains(B, V) when is_binary(B)    -> binary:match(B, to_binary(V)) =/= nomatch;
contains(C, V)                      -> bad('in', V, C).

%% Anything involving an undefined is reported as such: `{{ total + 1 }}' with
%% a missing `total' should say so, not say "unsupported operands".
bad(Op, undefined, _B) -> error({?TAG, {undefined_operation, Op}});
bad(Op, _A, undefined) -> error({?TAG, {undefined_operation, Op}});
bad(Op, A, B)          -> error({?TAG, {unsupported_operands, Op, A, B}}).

%%%===================================================================
%%% Iteration
%%%===================================================================

%% Materialising is deliberate: loop.length, loop.revindex, loop.last and
%% loop.nextitem all need the total up front (designs/10-jinja-semantics.md
%% section 6). Iterating a string is rejected because in a template it is
%% almost always a mistake (deviation J9).
-spec to_list(term()) -> [term()].
to_list(L) when is_list(L)  -> L;
to_list(undefined)          -> [];
to_list(null)               -> [];
to_list(M) when is_map(M)   -> lists:sort(maps:keys(M));
to_list({ns, M})            -> lists:sort(maps:keys(M));
to_list(T) when is_tuple(T) -> tuple_to_list(T);
to_list(V)                  -> error({?TAG, {not_iterable, V}}).

%% The `loop' variable for one iteration. Index is 1-based.
-spec loop(pos_integer(), non_neg_integer(), [term()], non_neg_integer(),
           term()) -> map().
loop(Index, Length, All, Depth, _Item) ->
    #{index     => Index,
      index0    => Index - 1,
      revindex  => Length - Index + 1,
      revindex0 => Length - Index,
      first     => Index =:= 1,
      last      => Index =:= Length,
      length    => Length,
      depth     => Depth + 1,
      depth0    => Depth,
      previtem  => nth_or_undefined(All, Index - 2),
      nextitem  => nth_or_undefined(All, Index),
      cycle     => fun(Args) -> cycle(Args, Index - 1) end,
      %% loop.changed/1 is rewritten by the compiler into changed/3, because
      %% deciding whether a value changed needs the previous iteration's value
      %% of the SAME expression, which only the compiler can produce.
      changed   => fun(_) -> error({?TAG, {not_callable, changed}}) end}.

nth_or_undefined(L, I) when I >= 0 ->
    case I < length(L) of
        true  -> lists:nth(I + 1, L);
        false -> undefined
    end;
nth_or_undefined(_L, _I) -> undefined.

cycle([], _I)   -> undefined;
cycle(Args, I)  -> lists:nth((I rem length(Args)) + 1, Args).

%% @doc loop.changed/1: did this value differ from the previous iteration?
%%
%% `First' short-circuits, because on the first iteration there is nothing to
%% compare against and Python's answer is True.
-spec changed(term(), term(), boolean()) -> boolean().
changed(_Now, _Before, true) -> true;
changed(Now, Before, false)  -> not cmp_eq(Now, Before).

%%%===================================================================
%%% Calls
%%%===================================================================

%% The dynamic path, used only when the callee is a value in scope rather
%% than a macro or a builtin the compiler could resolve.
-spec call(term(), [term()], [{atom(), term()}]) -> term().
call(F, Pos, []) when is_function(F, length(Pos)) ->
    erlang:apply(F, Pos);
call(F, Pos, Kw) when is_function(F) ->
    N = length(Pos),
    case erlang:fun_info(F, arity) of
        {arity, N}          -> erlang:apply(F, Pos);
        {arity, A} when A =:= N + 1 -> erlang:apply(F, Pos ++ [maps:from_list(Kw)]);
        {arity, 1}          -> F(maps:from_list(Kw));
        _                   -> error({?TAG, {not_callable, F}})
    end;
call(V, _Pos, _Kw) ->
    error({?TAG, {not_callable, V}}).

%% `map', `select' and friends take the NAME of a filter or test. The name is
%% checked against the registry rather than turned into a call with apply/3,
%% so a template cannot reach an arbitrary function.
%% @doc A builtin name used as a value rather than called.
%%
%% `{{ range is callable }}' has to see something callable, but a context key
%% of the same name must still win, so the lookup comes first.
-spec resolve_or_builtin(atom(), [term()]) -> term().
resolve_or_builtin(Name, Scope) ->
    case resolve(Name, Scope) of
        undefined -> fun(Args) -> builtin(Name, Args, []) end;
        V         -> V
    end.

%% @doc Call a named filter or test, binding extra positional arguments to
%% the callee's own parameter names.
%%
%% `|select("divisibleby", 3)' has to reach `divisibleby' with `num => 3', not
%% with a nameless list.
-spec apply_named(filter | test, atom(), term(), [term()]) -> term().
apply_named(Kind, Name, Value, Extra) ->
    Params = case Kind of
                 filter -> maps:get(Name, ai_jinja_filters:params(), []);
                 test   -> maps:get(Name, ai_jinja_tests:params(), [])
             end,
    apply_named(Kind, Name, [Value, ai_jinja_args:build(Params, Extra, [])]).

-spec apply_named(filter | test, atom(), [term()]) -> term().
apply_named(Kind, Name, Args) ->
    case ai_jinja_ext:lookup(Kind, Name) of
        {ok, {M, F}} -> erlang:apply(M, F, Args);
        error when Kind =:= filter -> error({?TAG, {unknown_filter, Name}});
        error                      -> error({?TAG, {unknown_test, Name}})
    end.

%%%===================================================================
%%% Builtins
%%%===================================================================

-spec is_builtin(atom()) -> boolean().
is_builtin(N) -> lists:member(N, [range, dict, namespace]).

-spec builtin(atom(), [term()], [{atom(), term()}]) -> term().
builtin(range, [A], _Kw)          -> range(A);
builtin(range, [A, B], _Kw)       -> range(A, B);
builtin(range, [A, B, C], _Kw)    -> range(A, B, C);
builtin(dict, [], Kw)             -> dict(Kw);
builtin(dict, [M], _Kw) when is_map(M) -> M;
builtin(namespace, [], Kw)        -> namespace(Kw);
builtin(namespace, [M], _Kw) when is_map(M) -> {ns, M};
builtin(N, _Pos, _Kw)             -> error({?TAG, {not_callable, N}}).

-spec range(integer()) -> [integer()].
range(N) when is_integer(N) -> lists:seq(0, N - 1);
range(V)                    -> bad('range', V, 0).

-spec range(integer(), integer()) -> [integer()].
range(A, B) when is_integer(A), is_integer(B), B > A -> lists:seq(A, B - 1);
range(A, B) when is_integer(A), is_integer(B)        -> [].

-spec range(integer(), integer(), integer()) -> [integer()].
range(_A, _B, 0) -> error({?TAG, {unsupported_operands, range, 0, 0}});
range(A, B, S) when is_integer(A), is_integer(B), is_integer(S), S > 0, B > A ->
    lists:seq(A, B - 1, S);
range(A, B, S) when is_integer(A), is_integer(B), is_integer(S), S < 0, B < A ->
    lists:seq(A, B + 1, S);
range(_A, _B, _S) -> [].

-spec dict([{atom(), term()}]) -> map().
dict(Kw) -> maps:from_list(Kw).

%% A namespace is a tagged map so that attribute access can reach it while
%% ordinary map operations cannot mistake it for data.
-spec namespace([{atom(), term()}]) -> {ns, map()}.
namespace(Kw) -> {ns, maps:from_list(Kw)}.
