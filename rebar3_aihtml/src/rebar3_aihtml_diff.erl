%%%-------------------------------------------------------------------
%%% @doc A minimal unified diff.
%%%
%%% Line based, longest common subsequence, three lines of context. Written
%%% out rather than shelling out to diff(1): the plugin has no third party
%%% dependencies and an external diff is neither guaranteed to exist nor to
%%% agree with itself across platforms.
%%% @end
%%%-------------------------------------------------------------------
-module(rebar3_aihtml_diff).

-export([unified/3]).

-define(CONTEXT, 3).

%% @doc Unified diff of two texts, as an iolist. Empty when they are equal.
unified(Path, Old, New) when Old =:= New ->
    _ = Path,
    [];
unified(Path, Old, New) ->
    A = lines(Old),
    B = lines(New),
    Ops = diff(A, B),
    case hunks(Ops) of
        [] -> [];
        Hs -> [io_lib:format("--- a/~ts~n+++ b/~ts~n", [Path, Path]),
               [hunk(H) || H <- Hs]]
    end.

lines(Bin) ->
    case binary:split(Bin, <<"\n">>, [global]) of
        L -> drop_last_empty(L)
    end.

drop_last_empty(L) ->
    case lists:reverse(L) of
        [<<>> | Rest] -> lists:reverse(Rest);
        _             -> L
    end.

%%%===================================================================
%%% LCS
%%%===================================================================

%% Ops are {eq | del | ins, Line}. The table is quadratic, which is fine:
%% these are hand written templates, not machine generated data.
diff(A, B) ->
    TA = list_to_tuple(A),
    TB = list_to_tuple(B),
    N = tuple_size(TA),
    M = tuple_size(TB),
    L = table(TA, TB, N, M),
    back(TA, TB, N, M, L, 1, 1, []).

%% L[{I, J}] is the length of the longest common subsequence of A[I..N] and
%% B[J..M]; the row past the end is all zeroes.
table(TA, TB, N, M) ->
    lists:foldl(
      fun(I, Acc0) ->
              lists:foldl(
                fun(J, Acc) ->
                        V = case element(I, TA) =:= element(J, TB) of
                                true  -> 1 + maps:get({I + 1, J + 1}, Acc, 0);
                                false -> max(maps:get({I + 1, J}, Acc, 0),
                                             maps:get({I, J + 1}, Acc, 0))
                            end,
                        Acc#{{I, J} => V}
                end, Acc0, lists:seq(M, 1, -1))
      end, #{}, lists:seq(N, 1, -1)).

back(_TA, _TB, N, M, _L, I, J, Acc) when I > N, J > M ->
    lists:reverse(Acc);
back(TA, TB, N, M, L, I, J, Acc) when I > N ->
    back(TA, TB, N, M, L, I, J + 1, [{ins, element(J, TB)} | Acc]);
back(TA, TB, N, M, L, I, J, Acc) when J > M ->
    back(TA, TB, N, M, L, I + 1, J, [{del, element(I, TA)} | Acc]);
back(TA, TB, N, M, L, I, J, Acc) ->
    case element(I, TA) =:= element(J, TB) of
        true ->
            back(TA, TB, N, M, L, I + 1, J + 1, [{eq, element(I, TA)} | Acc]);
        false ->
            case maps:get({I + 1, J}, L, 0) >= maps:get({I, J + 1}, L, 0) of
                true ->
                    back(TA, TB, N, M, L, I + 1, J,
                         [{del, element(I, TA)} | Acc]);
                false ->
                    back(TA, TB, N, M, L, I, J + 1,
                         [{ins, element(J, TB)} | Acc])
            end
    end.

%%%===================================================================
%%% Hunks
%%%===================================================================

hunks(Ops) ->
    Numbered = number(Ops, 1, 1, []),
    Changed = [I || {I, {Kind, _, _, _}} <- lists:zip(seq(Numbered), Numbered),
                    Kind =/= eq],
    group(Changed, Numbered).

seq(L) -> lists:seq(1, length(L)).

number([], _AN, _BN, Acc) ->
    lists:reverse(Acc);
number([{eq, L} | Rest], AN, BN, Acc) ->
    number(Rest, AN + 1, BN + 1, [{eq, L, AN, BN} | Acc]);
number([{del, L} | Rest], AN, BN, Acc) ->
    number(Rest, AN + 1, BN, [{del, L, AN, BN} | Acc]);
number([{ins, L} | Rest], AN, BN, Acc) ->
    number(Rest, AN, BN + 1, [{ins, L, AN, BN} | Acc]).

group([], _) -> [];
group(Changed, Numbered) ->
    Total = length(Numbered),
    Ranges = merge([{max(1, I - ?CONTEXT), min(Total, I + ?CONTEXT)}
                    || I <- Changed]),
    [[lists:nth(I, Numbered) || I <- lists:seq(S, E)] || {S, E} <- Ranges].

merge([]) -> [];
merge([H | T]) -> merge(T, [H]).

merge([], Acc) -> lists:reverse(Acc);
merge([{S, E} | T], [{S0, E0} | Acc]) when S =< E0 + 1 ->
    merge(T, [{S0, max(E, E0)} | Acc]);
merge([R | T], Acc) ->
    merge(T, [R | Acc]).

hunk(Lines) ->
    {AStart, ACount} = span(Lines, [eq, del], fun({_, _, A, _}) -> A end),
    {BStart, BCount} = span(Lines, [eq, ins], fun({_, _, _, B}) -> B end),
    [io_lib:format("@@ -~b,~b +~b,~b @@~n", [AStart, ACount, BStart, BCount]),
     [line(L) || L <- Lines]].

span(Lines, Kinds, F) ->
    Sel = [F(L) || L <- Lines, lists:member(element(1, L), Kinds)],
    case Sel of
        []  -> {0, 0};
        _   -> {hd(Sel), length(Sel)}
    end.

line({eq,  L, _, _}) -> [" ", L, "\n"];
line({del, L, _, _}) -> ["-", L, "\n"];
line({ins, L, _, _}) -> ["+", L, "\n"].
