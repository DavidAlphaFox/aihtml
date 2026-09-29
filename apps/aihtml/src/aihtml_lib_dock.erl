%%%-------------------------------------------------------------------
%%% @doc Internal: what the docking layouts share (aihtml_docking,
%%% aihtml_dock_layout): root ids, ids of windows, panels and groups,
%%% the hidden input, the layout JSON and value checks.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_lib_dock).

-export([ensure_id/2, hidden/2, decode/1, get_key/3, id_text/1, safe_id/1, num/1, bool/2,
         text/1]).

-export_type([id/0]).

-define(H, aihtml_html).

%% An id of a docking window, a dock layout panel or group (written as
%% text in the markup and the layout JSON).
-type id() :: atom() | binary() | integer().

%% @doc The root id as a binary, and the record with it set. The parts
%% refer to each other by ids derived from the root's, so a root without
%% an id gets one (`Prefix' followed by a unique integer).
-spec ensure_id(aihtml_element:element(), binary()) -> {binary(), aihtml_element:element()}.
ensure_id(R, Prefix) ->
    Id = case element(3, R) of
             undefined -> <<Prefix/binary, (integer_to_binary(erlang:unique_integer([positive])))/binary>>;
             Id0 -> text(Id0)
         end,
    {Id, setelement(3, R, Id)}.

%% @doc A hidden input carrying the value, when there is a name.
-spec hidden(undefined | atom() | iodata(), iodata()) -> aihtml_html:html().
hidden(undefined, _) -> [];
hidden(Name, Value) -> ?H:void(input, [], [{type, hidden}, {name, Name}, {value, Value}]).

%% @doc A saved layout value, decoded.
-spec decode(binary()) -> term().
decode(Json) ->
    try json:decode(Json)
    catch error:_ -> error({aihtml, {bad_dock_json, Json}})
    end.

%% @doc A key of a map written by hand (atom keys) or decoded from JSON
%% (binary keys).
-spec get_key(atom(), map(), term()) -> term().
get_key(K, M, Default) ->
    case maps:find(K, M) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(K), M, Default)
    end.

%% @doc An id as text.
-spec id_text(term()) -> binary().
id_text(I) when is_binary(I) -> I;
id_text(I) when is_atom(I), I =/= undefined, I =/= null -> atom_to_binary(I);
id_text(I) when is_integer(I) -> integer_to_binary(I);
id_text(I) -> error({aihtml, {bad_dock_id, I}}).

%% @doc An id usable inside an element id.
-spec safe_id(binary()) -> binary().
safe_id(I) ->
    re:replace(I, <<"[^A-Za-z0-9_-]">>, <<"_">>, [global, {return, binary}]).

%% @doc A number as text: integral values without decimals, others with
%% at most two.
-spec num(number()) -> binary().
num(N) when is_integer(N) -> integer_to_binary(N);
num(F) when is_float(F) ->
    case F == trunc(F) of
        true -> integer_to_binary(trunc(F));
        false -> float_to_binary(F, [{decimals, 2}, compact])
    end.

%% @doc `V' when it is a boolean, else a bad_option error for `K'.
-spec bool(atom(), term()) -> boolean().
bool(_, B) when is_boolean(B) -> B;
bool(K, V) -> error({aihtml, {bad_option, K, V}}).

%% @doc A title or label as a binary.
-spec text(term()) -> binary().
text(undefined) -> <<>>;
text(B) when is_binary(B) -> B;
text(L) when is_list(L) -> unicode:characters_to_binary(L);
text(X) -> beamai_html_escape:to_binary(X, aihtml).
