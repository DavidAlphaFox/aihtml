%%%-------------------------------------------------------------------
%%% @doc Helpers shared by aihtml_calendar and aihtml_datetime_input
%%% (internal).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_lib_calendar).

-export([labels/4]).

%% @doc Defaults merged with the custom texts, checked: unknown keys and
%% lists of the wrong length (`Lens': `[{Key, Length}]') raise
%% `error({aihtml, {Err, Key}})'. The keys become binaries (the JSON the
%% browser gets); list values are lists of binaries.
-spec labels(term(), #{atom() => term()}, atom(), [{atom(), pos_integer()}]) ->
          #{binary() => binary() | [binary()]}.
labels(Custom, Defaults, Err, Lens) ->
    is_map(Custom) orelse error({aihtml, {Err, Custom}}),
    maps:foreach(fun(K, _) -> maps:is_key(K, Defaults) orelse error({aihtml, {Err, K}}) end,
                 Custom),
    M = maps:merge(Defaults, Custom),
    [begin
         V = maps:get(K, M),
         (is_list(V) andalso length(V) =:= N andalso not is_integer(hd(V)))
             orelse error({aihtml, {Err, K}})
     end || {K, N} <- Lens],
    maps:fold(fun(K, V, Acc) ->
                      case lists:keymember(K, 1, Lens) of
                          true -> Acc#{atom_to_binary(K) => [text(X) || X <- V]};
                          false -> Acc#{atom_to_binary(K) => text(V)}
                      end
              end, #{}, M).

text(undefined) -> <<>>;
text(B) when is_binary(B) -> B;
text(L) when is_list(L) -> unicode:characters_to_binary(L);
text(X) -> beamai_html_escape:to_binary(X, aihtml).
