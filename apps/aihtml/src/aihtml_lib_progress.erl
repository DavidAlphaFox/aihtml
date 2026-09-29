%%%-------------------------------------------------------------------
%%% @doc Internal helpers shared by progressbar, progress_circle and
%%% meter: clamping and percentages. Not part of the public API.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_lib_progress).

-export([clamp/3, pct/3]).

%% @doc `V' limited to [Lo, Hi]; a non-number fails (records may hold any
%% value, whatever their field types say).
-spec clamp(term(), number(), number()) -> number().
clamp(V, Lo, Hi) when is_number(V) -> max(Lo, min(Hi, V));
clamp(V, _, _) -> error({aihtml, {bad_number, V}}).

%% @doc Where `V' lies in [Min, Max], in percent (0 for an empty range).
-spec pct(number(), number(), number()) -> number().
pct(V, Min, Max) when Max > Min -> 100 * (V - Min) / (Max - Min);
pct(_, _, _) -> 0.
