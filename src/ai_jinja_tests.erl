%%%-------------------------------------------------------------------
%%% @doc The builtin test library, used by `x is <name>'.
%%%
%%% Same shape as a filter -- `t(Value, Args)' -- but always returning a
%%% boolean, so that `{% if x is odd %}' needs no truthiness conversion.
%%%
%%% `defined' is the one to read twice: `none' and undefined are a single
%%% value in this engine (deviation J2), so `x is defined' answers false for a
%%% context that explicitly passes none. That is the visible price of not
%%% inventing a second empty value, and it has its own fixture.
%%% @end
%%%-------------------------------------------------------------------
-module(ai_jinja_tests).

-export([tests/0, params/0]).

-export([defined/2, undefined/2, none/2, boolean/2, 'false'/2, 'true'/2,
         integer/2, float/2, number/2, string/2, sequence/2, mapping/2,
         iterable/2, callable/2, sameas/2, escaped/2, in/2,
         eq/2, ne/2, lt/2, le/2, gt/2, ge/2,
         equalto/2, greaterthan/2, lessthan/2,
         odd/2, even/2, divisibleby/2, lower/2, upper/2, filter/2, test/2]).

-spec tests() -> #{atom() => {module(), atom()}}.
tests() -> maps:from_list([{N, {?MODULE, N}} || N <- names()]).

names() ->
    [defined, undefined, none, boolean, 'false', 'true', integer, float,
     number, string, sequence, mapping, iterable, callable, sameas, escaped,
     in, eq, ne, lt, le, gt, ge, equalto, greaterthan, lessthan,
     odd, even, divisibleby, lower, upper, filter, test].

-spec params() -> #{atom() => [atom()]}.
params() ->
    #{sameas      => [other],
      in          => [seq],
      eq          => [other], ne => [other], lt => [other],
      le          => [other], gt => [other], ge => [other],
      equalto     => [other], greaterthan => [other], lessthan => [other],
      divisibleby => [num]}.

%%%===================================================================
%%% Presence
%%%===================================================================

-spec defined(term(), map()) -> boolean().
defined(undefined, _A) -> false;
defined(_V, _A)        -> true.

-spec undefined(term(), map()) -> boolean().
undefined(V, A) -> not defined(V, A).

-spec none(term(), map()) -> boolean().
none(undefined, _A) -> true;
none(null, _A)      -> true;
none(_V, _A)        -> false.

%%%===================================================================
%%% Types
%%%===================================================================

-spec boolean(term(), map()) -> boolean().
boolean(V, _A) -> is_boolean(V).

-spec 'false'(term(), map()) -> boolean().
'false'(V, _A) -> V =:= false.

-spec 'true'(term(), map()) -> boolean().
'true'(V, _A) -> V =:= true.

%% `is_boolean' first: Erlang booleans are atoms, but Python's bools are ints
%% and jinja's integer test excludes them, so this has to say no too.
-spec integer(term(), map()) -> boolean().
integer(V, _A) -> is_integer(V).

-spec float(term(), map()) -> boolean().
float(V, _A) -> is_float(V).

-spec number(term(), map()) -> boolean().
number(V, _A) -> is_number(V) orelse is_boolean(V).

-spec string(term(), map()) -> boolean().
string(V, _A) -> is_binary(V) orelse is_tuple(V) andalso element(1, V) =:= safe.

-spec sequence(term(), map()) -> boolean().
sequence(V, _A) -> is_list(V) orelse is_binary(V) orelse is_map(V)
                       orelse (is_tuple(V) andalso element(1, V) =/= safe).

-spec mapping(term(), map()) -> boolean().
mapping(V, _A) -> is_map(V) orelse (is_tuple(V) andalso element(1, V) =:= ns).

-spec iterable(term(), map()) -> boolean().
iterable(V, A) -> sequence(V, A).

-spec callable(term(), map()) -> boolean().
callable(V, _A) -> is_function(V).

-spec escaped(term(), map()) -> boolean().
escaped(V, _A) -> is_tuple(V) andalso element(1, V) =:= safe.

%%%===================================================================
%%% Comparison
%%%===================================================================

-spec sameas(term(), map()) -> boolean().
sameas(V, A) -> V =:= other(A).

-spec in(term(), map()) -> boolean().
in(V, A) -> ai_jinja_rt:contains(maps:get(seq, A, []), V).

-spec eq(term(), map()) -> boolean().
eq(V, A) -> ai_jinja_rt:eq(V, other(A)).
-spec ne(term(), map()) -> boolean().
ne(V, A) -> ai_jinja_rt:ne(V, other(A)).
-spec lt(term(), map()) -> boolean().
lt(V, A) -> ai_jinja_rt:lt(V, other(A)).
-spec le(term(), map()) -> boolean().
le(V, A) -> ai_jinja_rt:le(V, other(A)).
-spec gt(term(), map()) -> boolean().
gt(V, A) -> ai_jinja_rt:gt(V, other(A)).
-spec ge(term(), map()) -> boolean().
ge(V, A) -> ai_jinja_rt:ge(V, other(A)).

-spec equalto(term(), map()) -> boolean().
equalto(V, A) -> eq(V, A).
-spec greaterthan(term(), map()) -> boolean().
greaterthan(V, A) -> gt(V, A).
-spec lessthan(term(), map()) -> boolean().
lessthan(V, A) -> lt(V, A).

other(A) -> maps:get(other, A, undefined).

%%%===================================================================
%%% Numbers and strings
%%%===================================================================

-spec odd(term(), map()) -> boolean().
odd(V, _A) when is_integer(V) -> V rem 2 =/= 0;
odd(_V, _A)                   -> false.

-spec even(term(), map()) -> boolean().
even(V, _A) when is_integer(V) -> V rem 2 =:= 0;
even(_V, _A)                   -> false.

-spec divisibleby(term(), map()) -> boolean().
divisibleby(V, A) ->
    case {V, maps:get(num, A, undefined)} of
        {I, N} when is_integer(I), is_integer(N), N =/= 0 -> I rem N =:= 0;
        _ -> false
    end.

-spec lower(term(), map()) -> boolean().
lower(V, _A) -> case text(V) of
                    error -> false;
                    B     -> string:lowercase(B) =:= B
                end.

-spec upper(term(), map()) -> boolean().
upper(V, _A) -> case text(V) of
                    error -> false;
                    B     -> string:uppercase(B) =:= B
                end.

text(V) when is_binary(V) -> V;
text({safe, D})           -> iolist_to_binary(D);
text(_)                   -> error.

%%%===================================================================
%%% Registry introspection
%%%===================================================================

%% `'upper' is filter' asks whether a NAME is registered, which is how
%% `|map', `|select' and friends validate what they were handed.
-spec filter(term(), map()) -> boolean().
filter(V, _A) -> registered(filter, V).

-spec test(term(), map()) -> boolean().
test(V, _A) -> registered(test, V).

registered(Kind, V) ->
    case name_of(V) of
        error -> false;
        Name  -> ai_jinja_ext:lookup(Kind, Name) =/= error
    end.

name_of(V) when is_atom(V) -> V;
name_of(V) when is_binary(V) ->
    try binary_to_existing_atom(V, utf8) catch error:badarg -> error end;
name_of(_) -> error.
