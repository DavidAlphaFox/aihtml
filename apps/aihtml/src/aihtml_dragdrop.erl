%%%-------------------------------------------------------------------
%%% @doc Drag and drop, ported from sigil (layout/dragdrop). DOM and class
%%% names are sigil's, so the styles in
%%% priv/css/sigil/components/dragdrop.css apply unchanged.
%%%
%%%   dragdrop(Children, Css, Attrs)       draggable items and drop zones
%%%   draggable_attrs(Key, Opts)           marks an element as draggable
%%%   drop_zone_attrs(Zone, Opts)          marks an element as a drop zone
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
%%% Behaviour: assets/js/components/dragdrop.ts. dragdrop/3 builds an
%%% #ah_dragdrop{} (include/aihtml_dragdrop.hrl) and render/1 turns it
%%% into HTML.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_dragdrop).
-behaviour(aihtml_element).

-include("aihtml_dragdrop.hrl").

-export([dragdrop/3, draggable_attrs/2, drop_zone_attrs/2,
         render/1, fields/1, catalog/0, facade_extras/0]).

-export_type([tolerance/0]).

-define(H, aihtml_html).
-define(E, aihtml_element).

-type tolerance() :: intersect | fit | pointer.

%% @doc A scope for drag and drop: `Children' hold the draggables
%% (`draggable_attrs/2') and the drop zones (`drop_zone_attrs/2'). Options:
%% `tolerance' (intersect, the default: the dragged copy overlaps the zone;
%% fit: it lies inside; pointer: the pointer is over the zone), `move'
%% (move the item into the zone on drop), `revert' (slide the copy back
%% when it is not dropped on a zone). `{disabled, true}' turns dragging off.
-spec dragdrop(aihtml_html:html(), aihtml_html:css(), aihtml_html:attrs()) -> #ah_dragdrop{}.
dragdrop(Children, Css, Attrs) ->
    ?E:build(?MODULE, #ah_dragdrop{body = Children}, Css, Attrs).

%% @doc Attributes that make an element draggable inside `dragdrop/3':
%% `div(Label, [], [draggable_attrs(task_1, #{type => task})])'. Opts:
%% `type' (zones with `accept' only take listed types), `disabled'.
-spec draggable_attrs(term(), map()) -> aihtml_html:attrs().
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
-spec drop_zone_attrs(term(), map()) -> aihtml_html:attrs().
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

%% @doc The field names of #ah_dragdrop{}.
-spec fields(atom()) -> [atom()].
fields(ah_dragdrop) -> record_info(fields, ah_dragdrop).

%% @doc Attribute helpers re-exported by the aihtml facade.
-spec facade_extras() -> [{atom(), arity()}].
facade_extras() -> [{draggable_attrs, 2}, {drop_zone_attrs, 2}].

%%%===================================================================
%%% Rendering
%%%===================================================================

-spec render(#ah_dragdrop{}) -> aihtml_html:html().
render(#ah_dragdrop{body = Body, tolerance = Tol, move = Move, revert = Revert,
                    disabled = Disabled} = R) ->
    lists:member(Tol, [intersect, fit, pointer])
        orelse error({aihtml, {bad_option, tolerance, Tol}}),
    [is_boolean(V) orelse error({aihtml, {bad_option, K, V}})
     || {K, V} <- [{move, Move}, {revert, Revert}, {disabled, Disabled}]],
    ?H:el('div', Body,
          [?E:classes(?MODULE, R), [<<"ah-dragdrop-disabled">> || Disabled]],
          [[{data_ah, <<"dragdrop">>},
            {data_ah_tolerance, Tol},
            {data_ah_move, Move}, {data_ah_revert, Revert},
            {aria_disabled, Disabled andalso <<"true">>}],
           ?E:root_attrs(R, 'ah:drop')]).

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => dragdrop, category => layout,
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

opt_bin(K, Opts) ->
    case maps:get(K, Opts, undefined) of
        undefined -> undefined;
        V -> bin(V)
    end.

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
