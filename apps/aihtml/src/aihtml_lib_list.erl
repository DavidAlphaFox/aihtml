%%%-------------------------------------------------------------------
%%% @doc Internal helpers shared by the list selection components
%%% (cascader, listbox, transfer, ported from sigil): list items, the
%%% generated root id and the ids of its parts, the hidden input and the
%%% joined value (aihtml_value). Not part of the public API.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_lib_list).

-export([item/1, ensure_id/1, sub_id/2, hidden/2, text/1, join/1, value/2]).

-export_type([item/0, norm_item/0]).

%% A listbox or transfer item: a text that is both value and label,
%% `{Value, Label}', or a map with `value' and optionally `label',
%% `disabled', `group' (listbox: a header per group) and `icon' (listbox:
%% an image URL; transfer: a short text such as an emoji).
-type item() :: binary() | atom() | integer() | {term(), term()}
              | #{value := term(), label => term(), disabled => boolean(),
                  group => term(), icon => iodata()}.
%% An item with its texts as binaries.
-type norm_item() :: #{value := binary(), label := binary(), disabled => boolean(),
                       group => binary(), icon => binary()}.

%% @doc An item as a map of binaries (see item()).
-spec item(item()) -> norm_item().
item(#{value := V} = M) ->
    maps:foreach(fun(K, _) ->
                         lists:member(K, [value, label, disabled, group, icon])
                             orelse error({aihtml, {bad_list_item, M}})
                 end, M),
    maps:merge(#{value => text(V), label => text(maps:get(label, M, V))},
               maps:map(fun(disabled, B) -> B =:= true;
                           (_, X) -> text(X)
                        end, maps:with([disabled, group, icon], M)));
item({V, L}) -> item(#{value => V, label => L});
item(V) when is_binary(V); is_atom(V); is_integer(V) -> item(#{value => V});
item(Other) -> error({aihtml, {bad_list_item, Other}}).

%% @doc The parts refer to each other by id (aria-controls, the actions'
%% event data), so a root without an id gets one. Returns the id and the
%% record holding it, for aihtml_element:root_attrs/2.
-spec ensure_id(aihtml_element:element()) -> {binary(), aihtml_element:element()}.
ensure_id(R) ->
    Id = case element(3, R) of
             undefined -> <<"ah-l", (integer_to_binary(erlang:unique_integer([positive])))/binary>>;
             Id0 -> text(Id0)
         end,
    {Id, setelement(3, R, Id)}.

%% @doc The id of a part: `<Id>-<Part>'.
-spec sub_id(binary(), binary()) -> binary().
sub_id(Id, Part) -> <<Id/binary, "-", Part/binary>>.

%% @doc The hidden input carrying the value, when there is a name.
-spec hidden(undefined | atom() | iodata(), binary()) -> aihtml_html:html().
hidden(undefined, _) -> [];
hidden(Name, Value) ->
    aihtml_html:void(input, [], [{type, hidden}, {name, Name}, {value, Value}]).

%% @doc A value or label as text (undefined is "").
-spec text(term()) -> binary().
text(undefined) -> <<>>;
text(B) when is_binary(B) -> B;
text(L) when is_list(L) -> unicode:characters_to_binary(L);
text(X) -> beamai_html_escape:to_binary(X, aihtml).

%% @doc Values joined with commas (data-ah-value), a comma inside a
%% value escaped (aihtml_value:join/1).
-spec join([binary()]) -> binary().
join(Vs) -> aihtml_value:join(Vs).

%% @doc The value text of a list: joined when it holds several values
%% (`Multi'), the single value itself (or "") otherwise.
-spec value(boolean(), [binary()]) -> binary().
value(true, Vs) -> join(Vs);
value(false, Vs) -> iolist_to_binary(Vs).
