%%%-------------------------------------------------------------------
%%% @doc Drag and drop, ported from sigil (layout/sortable and
%%% layout/dragdrop). DOM and class names are sigil's, so the styles in
%%% priv/css/sigil/components/{sortable,dragdrop}.css apply unchanged.
%%%
%%%   sortable(Items, Value, Css, Attrs)   a reorderable list
%%%   dragdrop(Children, Css, Attrs)       draggable items and drop zones
%%%   draggable_attrs(Key, Opts)           marks an element as draggable
%%%   drop_zone_attrs(Zone, Opts)          marks an element as a drop zone
%%%
%%% == sortable ==
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
%%% == dragdrop ==
%%%
%%% `dragdrop/3' is a scope. Inside it, elements carrying
%%% `draggable_attrs(Key, Opts)' can be dropped on elements carrying
%%% `drop_zone_attrs(Zone, Opts)'. A drop fires `ah:drop' on the zone; it
%%% bubbles to the root, which carries the drop in data attributes, so an
%%% action bound with `on('ah:drop', Ref)' on the root (or the record's
%%% `postback') receives
%%%
%%%   Event.data   #{<<"drag">> => Key, <<"drop">> => Zone,
%%%                  <<"from">> => the zone the item was in, or <<>>}
%%%
%%% With the `move' option the item is also moved into the zone in the
%%% browser. From the keyboard: Space or Enter picks a draggable up, the
%%% arrows walk through the zones that accept it, Space or Enter drops,
%%% Escape cancels.
%%%
%%% Behaviours: assets/js/components/layout_dnd.js. Each component
%%% function builds an element record (#ah_sortable{}, #ah_dragdrop{},
%%% include/aihtml_layout_dnd.hrl) and render/1 turns it into HTML.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_layout_dnd).
-behaviour(aihtml_element).

-include("aihtml_layout_dnd.hrl").

-export([sortable/4, dragdrop/3, draggable_attrs/2, drop_zone_attrs/2,
         render/1, fields/1, catalog/0, facade_extras/0]).

-export_type([item/0, element/0]).

-define(H, aihtml_html).
-define(E, aihtml_element).

-type html() :: aihtml_html:html().
-type css() :: aihtml_html:css().
-type attrs() :: aihtml_html:attrs().
-type item() :: ah_dnd_item().
-type element() :: #ah_sortable{} | #ah_dragdrop{}.

%%%===================================================================
%%% Builders
%%%===================================================================

%% @doc A reorderable list. `Value' is the order of the item keys (a list
%% or "a,b,c"); items it does not name follow in their own order, and
%% `undefined' keeps the order of `Items'. Css: orientation `vertical'
%% (default) | `horizontal' | `grid', flag `handle' (drag by a grip only).
%% Options: `group' (lists of the same group exchange items). `name' goes
%% to a hidden input, `{disabled, true}' turns sorting off.
-spec sortable([item()], undefined | [term()] | iodata(), css(), attrs()) -> #ah_sortable{}.
sortable(Items, Value, Css, Attrs) ->
    build(#ah_sortable{items = Items, value = Value}, Css, Attrs).

%% @doc A scope for drag and drop: `Children' hold the draggables
%% (`draggable_attrs/2') and the drop zones (`drop_zone_attrs/2'). Options:
%% `tolerance' (intersect, the default: the dragged copy overlaps the zone;
%% fit: it lies inside; pointer: the pointer is over the zone), `move'
%% (move the item into the zone on drop), `revert' (slide the copy back
%% when it is not dropped on a zone). `{disabled, true}' turns dragging off.
-spec dragdrop(html(), css(), attrs()) -> #ah_dragdrop{}.
dragdrop(Children, Css, Attrs) ->
    build(#ah_dragdrop{body = Children}, Css, Attrs).

%% @doc Attributes that make an element draggable inside `dragdrop/3':
%% `div(Label, [], [draggable_attrs(task_1, #{type => task})])'. Opts:
%% `type' (zones with `accept' only take listed types), `disabled'.
-spec draggable_attrs(term(), map()) -> attrs().
draggable_attrs(Key, Opts) when is_map(Opts) ->
    Disabled = maps:get(disabled, Opts, false),
    [{class, [<<"ah-draggable">>, [<<"ah-draggable-disabled">> || Disabled =:= true]]},
     {data_ah_drag, bin(Key)},
     {data_ah_drag_type, opt_bin(type, Opts)},
     {tabindex, <<"0">>},
     {aria_roledescription, <<"draggable">>},
     {aria_disabled, Disabled =:= true andalso <<"true">>}].

%% @doc Attributes that make an element a drop zone inside `dragdrop/3'.
%% Opts: `accept' (a list of draggable types; any type by default),
%% `disabled'.
-spec drop_zone_attrs(term(), map()) -> attrs().
drop_zone_attrs(Zone, Opts) when is_map(Opts) ->
    Accept = case maps:get(accept, Opts, undefined) of
                 undefined -> undefined;
                 L when is_list(L) -> join([bin(T) || T <- L]);
                 T -> bin(T)
             end,
    [{class, <<"ah-drop-zone">>},
     {data_ah_drop, bin(Zone)},
     {data_ah_drop_accept, Accept},
     {data_ah_drop_disabled, maps:get(disabled, Opts, false) =:= true andalso <<"true">>}].

build(R, Css, Attrs) ->
    Tag = element(1, R),
    ?E:build(R, fields(Tag), entry(?E:component_name(Tag)), Css, Attrs).

%% @doc The field names of one of this group's records.
-spec fields(atom()) -> [atom()].
fields(ah_sortable) -> record_info(fields, ah_sortable);
fields(ah_dragdrop) -> record_info(fields, ah_dragdrop).

%% @doc Attribute helpers re-exported by the aihtml facade.
-spec facade_extras() -> [{atom(), arity()}].
facade_extras() -> [{draggable_attrs, 2}, {drop_zone_attrs, 2}].

%%%===================================================================
%%% Rendering
%%%===================================================================

-spec render(element()) -> html().
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
          [classes(R), [<<"ah-sortable-disabled">> || Disabled]],
          [[{role, list},
            {aria_disabled, Disabled andalso <<"true">>},
            {data_ah, <<"sortable">>}, {data_ah_value, Order},
            {data_ah_group, opt_text(group, Group)}],
           ?E:root_attrs(R, change)]);

render(#ah_dragdrop{body = Body, tolerance = Tol, move = Move, revert = Revert,
                    disabled = Disabled} = R) ->
    lists:member(Tol, [intersect, fit, pointer])
        orelse error({aihtml, {bad_option, tolerance, Tol}}),
    [is_boolean(V) orelse error({aihtml, {bad_option, K, V}})
     || {K, V} <- [{move, Move}, {revert, Revert}, {disabled, Disabled}]],
    ?H:el('div', Body,
          [classes(R), [<<"ah-dragdrop-disabled">> || Disabled]],
          [[{data_ah, <<"dragdrop">>},
            {data_ah_tolerance, Tol},
            {data_ah_move, Move}, {data_ah_revert, Revert},
            {aria_disabled, Disabled andalso <<"true">>}],
           ?E:root_attrs(R, 'ah:drop')]).

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

classes(R) ->
    Tag = element(1, R),
    ?E:classes(R, fields(Tag), entry(?E:component_name(Tag))).

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
              doc => <<"Cancel a drag in progress and put the item back.">>}]},
     #{name => dragdrop, category => layout,
       signature => <<"dragdrop(Children, Css, Attrs)">>,
       root => <<"ah-dragdrop">>,
       options => [tolerance, move, revert],
       behavior => <<"dragdrop">>,
       events => [<<"ah:drop">>, <<"ah:drag-start">>, <<"ah:drag-end">>,
                  <<"ah:drag-cancel">>, <<"ah:drop-target-enter">>,
                  <<"ah:drop-target-leave">>],
       doc => <<"A scope of draggable items (draggable_attrs/2) and drop zones "
                "(drop_zone_attrs/2); a drop fires ah:drop with the item key and the zone.">>,
       option_docs =>
           #{tolerance => <<"When the dragged copy is over a zone: intersect (overlaps it, "
                            "default), fit (lies inside it) or pointer (the pointer is on it).">>,
             move => <<"Move the dropped item into the zone in the browser.">>,
             revert => <<"Slide the copy back to the item when it is not dropped on a zone.">>},
       methods =>
           [#{name => enable, args => <<"()">>, doc => <<"Turn dragging on.">>},
            #{name => disable, args => <<"()">>, doc => <<"Turn dragging off.">>},
            #{name => cancel, args => <<"()">>, doc => <<"Cancel a drag in progress.">>}]}].

%%%===================================================================
%%% Internal
%%%===================================================================

entry(Name) -> aihtml_catalog:entry(?MODULE, Name).

hidden_input(undefined, _) -> [];
hidden_input(Name, Value) ->
    ?H:void(input, [], [{type, hidden}, {name, Name}, {value, Value},
                        {data_ah_input, true}]).

opt_bin(K, Opts) ->
    case maps:get(K, Opts, undefined) of
        undefined -> undefined;
        V -> bin(V)
    end.

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
