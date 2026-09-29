%%%-------------------------------------------------------------------
%%% @doc Internal: what the scrolling components share (aihtml_scrollview,
%%% aihtml_scrollbar, aihtml_responsive_panel): option checks, the root id,
%%% the hidden input and CSS lengths, plus the field types of their
%%% records.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_lib_scroll).

-export([with_id/2, bool/2, one_of/3, pos_int/2, non_neg_int/2, hidden/2, num/1,
         len/1, style/1]).

-export_type([css_length/0, name/0, action/0]).

-define(H, aihtml_html).

%% Integer pixels or a CSS length.
-type css_length() :: undefined | integer() | binary() | string().
-type name() :: undefined | atom() | iodata().
%% An action reference {Module, Action, Args}.
-type action() :: undefined | {module(), atom(), term()}.

%% @doc The root id as a binary and the record carrying it: the `id'
%% field, an id among the attrs, or a generated Prefix-N.
-spec with_id(tuple(), binary()) -> {binary(), tuple()}.
with_id(R, Prefix) ->
    Id = case element(3, R) of
             undefined ->
                 case lists:keyfind(<<"id">>, 1, ?H:attrs(element(5, R))) of
                     {_, V} when is_binary(V) -> V;
                     _ -> <<Prefix/binary, "-",
                            (integer_to_binary(erlang:unique_integer([positive])))/binary>>
                 end;
             V -> bin(V)
         end,
    {Id, setelement(3, R, Id)}.

%% @doc `V' when it is a boolean, else a bad_option error for `Field'.
-spec bool(atom(), term()) -> boolean().
bool(Field, V) -> one_of(Field, V, [true, false]).

%% @doc `V' when it is one of `Allowed', else a bad_option error.
-spec one_of(atom(), T, [T]) -> T.
one_of(Field, V, Allowed) ->
    lists:member(V, Allowed) orelse error({aihtml, {bad_option, Field, V}}),
    V.

%% @doc `N' when it is a positive integer, else a bad_option error.
-spec pos_int(atom(), term()) -> pos_integer().
pos_int(_Field, N) when is_integer(N), N > 0 -> N;
pos_int(Field, V) -> error({aihtml, {bad_option, Field, V}}).

%% @doc `N' when it is a non-negative integer, else a bad_option error.
-spec non_neg_int(atom(), term()) -> non_neg_integer().
non_neg_int(_Field, N) when is_integer(N), N >= 0 -> N;
non_neg_int(Field, V) -> error({aihtml, {bad_option, Field, V}}).

%% @doc The hidden input submitting the value, when there is a name.
-spec hidden(name(), iodata()) -> aihtml_html:html().
hidden(undefined, _) -> [];
hidden(Name, Value) ->
    ?H:void(input, [], [{type, hidden}, {name, Name}, {value, Value}]).

%% @doc A number as text: an integer, or a float in its shortest form.
-spec num(number()) -> binary().
num(I) when is_integer(I) -> integer_to_binary(I);
num(F) when is_float(F) -> float_to_binary(F, [short]).

%% @doc A CSS length: integers are pixels.
-spec len(css_length()) -> undefined | binary().
len(undefined) -> undefined;
len(N) when is_integer(N) -> <<(integer_to_binary(N))/binary, "px">>;
len(V) when is_binary(V); is_list(V) -> bin(V);
len(V) -> error({aihtml, {bad_length, V}}).

%% @doc A style attribute from `{Property, Value}' pairs, leaving out
%% false and undefined values (undefined when nothing is left).
-spec style([{binary(), binary() | false | undefined}]) -> undefined | binary().
style(Decls) ->
    case [[K, $:, V, $;] || {K, V} <- Decls, V =/= false, V =/= undefined] of
        [] -> undefined;
        S -> iolist_to_binary(S)
    end.

%%%===================================================================
%%% Internal
%%%===================================================================

bin(B) when is_binary(B) -> B;
bin(A) when is_atom(A) -> atom_to_binary(A, utf8);
bin(L) when is_list(L) -> unicode:characters_to_binary(L).
