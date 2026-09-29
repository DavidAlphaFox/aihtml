%%%-------------------------------------------------------------------
%%% @doc A reorderable list, ported from sigil (layout/sortable). DOM and
%%% class names are sigil's, so the styles in
%%% priv/css/sigil/components/sortable.css apply unchanged.
%%%
%%% Items are `{Key, Content}' or `{Key, Content, ItemAttrs}'. The value
%%% is the order of the keys: `data-ah-value="b,a,c"' on the root, a
%%% hidden input when `Attrs' has a `name', and `change' on the root after
%%% a drop that changed the order, so `on(change, Ref)' (or a record's
%%% `postback') gets the new order in `Event.value'. Lists with the same
%%% `group' option exchange items; both lists fire `change'.
%%%
%%% Items are dragged with the pointer (mouse, pen, touch), or with the
%%% `handle' flag only by the grip rendered in front of each item. From
%%% the keyboard: arrows move the focus, Space or Enter picks the item up,
%%% arrows move it, Space or Enter drops it and Escape puts it back;
%%% Alt+arrow moves the focused item at once.
%%%
%%% Behaviour: assets/js/components/sortable.js. sortable/4 builds an
%%% #ah_sortable{} (include/aihtml_sortable.hrl) and render/1 turns it
%%% into HTML.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_sortable).
-behaviour(aihtml_element).

-include("aihtml_sortable.hrl").

-export([sortable/4, render/1, fields/1, catalog/0]).

-export_type([item/0, orientation/0]).

-define(H, aihtml_html).
-define(E, aihtml_element).

%% {Key, Content} | {Key, Content, ItemAttrs}: Key identifies the item in
%% the order (data-value), ItemAttrs are HTML attributes of the item.
-type item() :: {term(), aihtml_html:html()}
              | {term(), aihtml_html:html(), aihtml_html:attrs()}.
-type orientation() :: vertical | horizontal | grid.

%% @doc A reorderable list. `Value' is the order of the item keys (a list
%% or "a,b,c"); items it does not name follow in their own order, and
%% `undefined' keeps the order of `Items'. Css: orientation `vertical'
%% (default) | `horizontal' | `grid', flag `handle' (drag by a grip only).
%% Options: `group' (lists of the same group exchange items). `name' goes
%% to a hidden input, `{disabled, true}' turns sorting off.
-spec sortable([item()], undefined | [term()] | iodata(), aihtml_html:css(),
               aihtml_html:attrs()) -> #ah_sortable{}.
sortable(Items, Value, Css, Attrs) ->
    ?E:build(?MODULE, #ah_sortable{items = Items, value = Value}, Css, Attrs).

%% @doc The field names of #ah_sortable{}.
-spec fields(atom()) -> [atom()].
fields(ah_sortable) -> record_info(fields, ah_sortable).

%%%===================================================================
%%% Rendering
%%%===================================================================

-spec render(#ah_sortable{}) -> aihtml_html:html().
render(#ah_sortable{items = Items0, value = Value, handle = Handle,
                    group = Group, name = Name, disabled = Disabled} = R) ->
    is_boolean(Disabled) orelse error({aihtml, {bad_option, disabled, Disabled}}),
    Items = order([item(I) || I <- Items0], Value),
    Order = join([K || {K, _, _} <- Items]),
    Rows = [sortable_item(K, Content, IA, Idx =:= 1, Handle, Disabled)
            || {Idx, {K, Content, IA}} <- lists:enumerate(Items)],
    ?H:el('div',
          [Rows,
           hidden_input(Name, Order),
           ?H:el(span, [], [<<"ah-sortable-live">>],
                 [{aria_live, <<"assertive">>}, {aria_atomic, <<"true">>}])],
          [?E:classes(?MODULE, R), [<<"ah-sortable-disabled">> || Disabled]],
          [[{role, list},
            {aria_disabled, Disabled andalso <<"true">>},
            {data_ah, <<"sortable">>}, {data_ah_value, Order},
            {data_ah_group, opt_text(group, Group)}],
           ?E:root_attrs(R, change)]).

sortable_item(Key, Content, IA, First, Handle, Disabled) ->
    Grip = [?H:el(span, <<"⠿"/utf8>>, [<<"ah-sortable-handle">>],
                  [{aria_hidden, <<"true">>}]) || Handle],
    ?H:el('div', [Grip, Content],
          [<<"ah-sortable-item">>, [<<"ah-sortable-handle-mode">> || Handle]],
          [[{role, listitem}, {data_value, Key},
            {tabindex, case First andalso not Disabled of
                           true -> <<"0">>;
                           false -> <<"-1">>
                       end},
            {aria_roledescription, <<"sortable item">>}],
           IA]).

%% Items in the order of Value; the ones it does not name follow.
order(Items, undefined) -> Items;
order(Items, Value) ->
    Keys = dedup(values(Value)),
    Named = [I || K <- Keys, {IK, _, _} = I <- Items, IK =:= K],
    Named ++ [I || {K, _, _} = I <- Items, not lists:member(K, Keys)].

dedup(L) -> dedup(L, []).
dedup([], _) -> [];
dedup([X | T], Seen) ->
    case lists:member(X, Seen) of
        true -> dedup(T, Seen);
        false -> [X | dedup(T, [X | Seen])]
    end.

item({K, Content}) -> item({K, Content, []});
item({K0, Content, IA}) ->
    K = bin(K0),
    %% the order is written as "a,b,c"
    binary:match(K, <<",">>) =:= nomatch orelse error({aihtml, {bad_item_key, K0}}),
    {K, Content, IA}.

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => sortable, category => layout,
       signature => <<"sortable(Items, Value, Css, Attrs)">>,
       root => <<"ah-sortable">>,
       groups => #{orientation => {[vertical, horizontal, grid], vertical}},
       flags => [handle],
       classes => #{handle => []},
       options => [group],
       behavior => <<"sortable">>,
       events => [<<"change">>, <<"ah:sort-start">>, <<"ah:sort-change">>,
                  <<"ah:sort-stop">>, <<"ah:sort-receive">>, <<"ah:sort-remove">>,
                  <<"ah:sort-cancel">>],
       doc => <<"A list reordered by dragging or from the keyboard; the value is the order "
                "of the item keys and change fires after a drop.">>,
       option_docs =>
           #{handle => <<"Render a grip in front of each item; only the grip starts a drag.">>,
             group => <<"Lists with the same group exchange items by dragging.">>},
       methods =>
           [#{name => getValue, args => <<"()">>, doc => <<"Return the order, \"a,b,c\".">>},
            #{name => setValue, args => <<"(Order)">>,
              doc => <<"Reorder the items (a list or \"a,b,c\") without firing change.">>},
            #{name => enable, args => <<"()">>, doc => <<"Turn sorting on.">>},
            #{name => disable, args => <<"()">>, doc => <<"Turn sorting off.">>},
            #{name => cancel, args => <<"()">>,
              doc => <<"Cancel a drag in progress and put the item back.">>}]}].

%%%===================================================================
%%% Internal
%%%===================================================================

hidden_input(undefined, _) -> [];
hidden_input(Name, Value) ->
    ?H:void(input, [], [{type, hidden}, {name, Name}, {value, Value},
                        {data_ah_input, true}]).

opt_text(_, undefined) -> undefined;
opt_text(K, V) ->
    try bin(V) catch error:_ -> error({aihtml, {bad_option, K, V}}) end.

values(B) when is_binary(B) -> binary:split(B, <<",">>, [global, trim_all]);
values(L) when is_list(L) ->
    case io_lib:printable_unicode_list(L) of
        true when L =/= [] -> values(bin(L));
        _ -> [bin(X) || X <- L]
    end;
values(Other) -> error({aihtml, {bad_value, Other}}).

join(Vs) -> iolist_to_binary(lists:join(<<",">>, Vs)).

bin(B) when is_binary(B) -> B;
bin(A) when is_atom(A) -> atom_to_binary(A, utf8);
bin(I) when is_integer(I) -> integer_to_binary(I);
bin(F) when is_float(F) -> float_to_binary(F, [short]);
bin(L) when is_list(L) ->
    case unicode:characters_to_binary(L) of
        B when is_binary(B) -> B;
        _ -> error({aihtml, {bad_value, L}})
    end;
bin(Other) -> error({aihtml, {bad_value, Other}}).
