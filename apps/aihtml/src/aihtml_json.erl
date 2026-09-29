%% @doc JSON with a stable key order.
%%
%% `json:encode/1' writes a map's keys in the map's internal order, which
%% for atom keys follows the order the atoms were created in, so the same
%% page could render differently on another node or after a restart.
%% `encode/1' writes every map, at any depth, with its keys sorted by
%% their text; everything else is encoded as by `json:encode/1'.
-module(aihtml_json).

-export([encode/1]).

%% @doc `json:encode/1' with the keys of every map sorted.
-spec encode(term()) -> iodata().
encode(Term) -> json:encode(Term, fun encode/2).

encode(M, Encode) when is_map(M) ->
    Sorted = lists:sort([{key_text(K), K, V} || K := V <- M]),
    json:encode_key_value_list([{K, V} || {_, K, V} <- Sorted], Encode);
encode(Other, Encode) -> json:encode_value(Other, Encode).

%% The key as JSON writes it; other terms are left to json's own error.
key_text(K) when is_binary(K) -> K;
key_text(K) when is_atom(K) -> atom_to_binary(K, utf8);
key_text(K) when is_integer(K) -> integer_to_binary(K);
key_text(K) when is_float(K) -> float_to_binary(K, [short]);
key_text(K) -> K.
