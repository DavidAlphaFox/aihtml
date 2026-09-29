%%%-------------------------------------------------------------------
%%% @doc Internal: what the ribbon (aihtml_ribbon) and the tile layout
%%% (aihtml_tile_layout) share: option checks, root ids, sizes, the
%%% hidden input and text conversion.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_lib_tiles).

-export([check/3, ensure_id/1, css_size/1, hidden_input/2, value_text/1, text/1]).

-define(H, aihtml_html).

%% @doc Fail with bad_option unless `V' is one of `Allowed'.
-spec check(atom(), term(), [term()]) -> true.
check(Field, V, Allowed) ->
    lists:member(V, Allowed) orelse error({aihtml, {bad_option, Field, V}}).

%% @doc The root id as a binary, and the record with it set. The parts
%% refer to each other by id, so a root without one gets one.
-spec ensure_id(aihtml_element:element()) -> {binary(), aihtml_element:element()}.
ensure_id(R) ->
    Id = case element(3, R) of
             undefined -> <<"ah-t", (integer_to_binary(erlang:unique_integer([positive])))/binary>>;
             Id0 -> text(Id0)
         end,
    {Id, setelement(3, R, Id)}.

%% @doc A CSS length: integer px or the value as given.
-spec css_size(integer() | iodata()) -> iodata().
css_size(N) when is_integer(N) -> [integer_to_binary(N), <<"px">>];
css_size(S) -> S.

%% @doc A hidden input carrying the value, when there is a name.
-spec hidden_input(undefined | atom() | iodata(), iodata()) -> aihtml_html:html().
hidden_input(undefined, _) -> [];
hidden_input(Name, Value) ->
    ?H:void(input, [], [{type, hidden}, {name, Name}, {value, Value}]).

%% @doc text/1, with undefined as the empty value.
-spec value_text(term()) -> binary().
value_text(undefined) -> <<>>;
value_text(V) -> text(V).

%% @doc A key, id or label as a binary.
-spec text(term()) -> binary().
text(undefined) -> <<>>;
text(B) when is_binary(B) -> B;
text(A) when is_atom(A) -> atom_to_binary(A, utf8);
text(I) when is_integer(I) -> integer_to_binary(I);
text(F) when is_float(F) -> float_to_binary(F, [short]);
text(L) when is_list(L) ->
    case unicode:characters_to_binary(L) of
        B when is_binary(B) -> B;
        _ -> error({aihtml, {bad_value, L}})
    end;
text(Other) -> error({aihtml, {bad_value, Other}}).
