%% @doc Multi-values of the value-bearing components.
%%
%% Components holding several values (combobox, listbox, transfer,
%% checkbox_group, tag_input, the selection of the grids ...) write them
%% as one text: in `data-ah-value', in the hidden input and in the change
%% event's `Event.value'. The values are joined with commas; a comma or a
%% backslash inside a value is escaped with a backslash (`\,' and `\\'),
%% so values without them are written as before (`a,b,c').
%%
%% The browser side is `AH.lib.values' (components/_lib_values.ts), with
%% the same rules.
-module(aihtml_value).

-export([join/1, split/1]).

%% @doc Joins values (binaries, atoms, numbers or strings) into one text.
-spec join([term()]) -> binary().
join(Vs) when is_list(Vs) ->
    iolist_to_binary(lists:join($,, [escape(to_bin(V)) || V <- Vs])).

%% @doc The values of a joined text; `<<>>' and `undefined' are `[]'.
%% The inverse of `join/1', except that `join([<<>>])' is also `<<>>'.
-spec split(binary() | string() | undefined) -> [binary()].
split(undefined) -> [];
split(<<>>) -> [];
split(L) when is_list(L) -> split(unicode:characters_to_binary(L));
split(B) when is_binary(B) -> split(B, <<>>, []).

split(<<$\\, C, Rest/binary>>, Cur, Acc) -> split(Rest, <<Cur/binary, C>>, Acc);
split(<<$\\>>, Cur, Acc) -> split(<<>>, <<Cur/binary, $\\>>, Acc);
split(<<$,, Rest/binary>>, Cur, Acc) -> split(Rest, <<>>, [Cur | Acc]);
split(<<C, Rest/binary>>, Cur, Acc) -> split(Rest, <<Cur/binary, C>>, Acc);
split(<<>>, Cur, Acc) -> lists:reverse([Cur | Acc]).

escape(B) ->
    case binary:match(B, [<<"\\">>, <<",">>]) of
        nomatch -> B;
        _ -> << <<(esc(C))/binary>> || <<C>> <= B >>
    end.

esc($\\) -> <<"\\\\">>;
esc($,) -> <<"\\,">>;
esc(C) -> <<C>>.

to_bin(V) -> beamai_html_escape:to_binary(V, aihtml).
