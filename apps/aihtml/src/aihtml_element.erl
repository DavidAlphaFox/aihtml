%%%-------------------------------------------------------------------
%%% @doc Element records (designs/05-records.md).
%%%
%%% An element is a record that starts with the fields of `?AH_BASE'
%%% (include/aihtml_element.hrl): position 2 is the module that renders
%%% it, followed by `id', `css', `attrs', `postback' and `delegate'.
%%% `aihtml_html:render/1' hands such a tuple to `Module:render/1' and
%%% renders what comes back, which may be another element.
%%%
%%% Component group modules implement this behaviour, build their records
%%% from the classic `(Content, Value, Css, Attrs)' arguments with
%%% `build/5' and use `classes/3' and `root_attrs/2' in `render/1'.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_element).

-export([is_element/1, render/1, base/1, build/4, build/5, classes/2, classes/3,
         root_attrs/2, component_name/1]).

-export_type([element/0, postback/0]).

%% A record whose first fields are ?AH_BASE.
-type element() :: tuple().
%% Action | {Action, Args} | {Action, Args, OnOpts}; sent to `delegate'.
-type postback() :: atom() | {atom(), term()} | {atom(), term(), map()}.

-callback render(element()) -> aihtml_html:html().

%% ?AH_BASE fields and their positions.
-define(BASE, [module, id, css, attrs, postback, delegate]).
-define(BASE_SIZE, 7).
%% Base fields a builder never takes from Attrs.
-define(NOT_FROM_ATTRS, [module, css, attrs, postback, delegate]).

%% @doc Whether `T' looks like an element record.
-spec is_element(term()) -> boolean().
is_element(T) ->
    is_tuple(T) andalso tuple_size(T) >= ?BASE_SIZE
        andalso is_atom(element(1, T)) andalso is_atom(element(2, T)).

%% @doc One step of rendering: what the element's module makes of it.
-spec render(element()) -> aihtml_html:html().
render(R) ->
    Mod = element(2, R),
    case code:ensure_loaded(Mod) of
        {module, Mod} ->
            erlang:function_exported(Mod, render, 1)
                orelse error({aihtml, {no_render, Mod, element(1, R)}}),
            Mod:render(R);
        _ ->
            error({aihtml, {no_render, Mod, element(1, R)}})
    end.

%% @doc The ?AH_BASE fields of any element.
-spec base(element()) -> #{module := module(), id := term(), css := aihtml_html:css(),
                           attrs := aihtml_html:attrs(), postback := undefined | postback(),
                           delegate := module()}.
base(R) ->
    maps:from_list(lists:zip(?BASE, [element(I, R) || I <- lists:seq(2, ?BASE_SIZE)])).

%% @doc build/5 for a component module: `Mod:fields/1' gives the record's
%% fields and `Mod:catalog/0' its entry, found by the record's name
%% (ah_button -> button). Builders write `build(?MODULE, #ah_x{...}, Css, Attrs)'.
-spec build(module(), element(), aihtml_html:css(), aihtml_html:attrs()) -> element().
build(Mod, R, Css, Attrs) ->
    Tag = element(1, R),
    build(R, Mod:fields(Tag), aihtml_catalog:entry(Mod, component_name(Tag)), Css, Attrs).

%% @doc classes/3 for a component module, as build/4.
-spec classes(module(), element()) -> aihtml_html:css().
classes(Mod, R) ->
    Tag = element(1, R),
    classes(R, Mod:fields(Tag), aihtml_catalog:entry(Mod, component_name(Tag))).

%% @doc Fill a record from builder arguments. `Fields' is the record's
%% `record_info(fields, _)'. Modifier atoms in `Css' set the field named
%% after their catalog group (or flag, to `true'); binaries go to `css'.
%% Atom keys of `Attrs' that name a field (other than module, css, attrs,
%% postback, delegate) set that field; the rest stay HTML attributes.
%% module, postback and delegate are for records only and fail here.
-spec build(element(), [atom()], aihtml_catalog:entry(), aihtml_html:css(),
            aihtml_html:attrs()) -> element().
build(R, Fields, Entry, Css, Attrs) ->
    {Groups, Flags, Literal} = aihtml_catalog:parse_css(Entry, Css),
    Settable = Fields -- ?NOT_FROM_ATTRS,
    Flat = flat_attrs(Attrs),
    %% a builder runs in the component's module, so ?MODULE would be wrong
    [error({aihtml, {record_only_field, element(1, R), K}})
     || {K, _} <- Flat, lists:member(K, [module, postback, delegate])],
    {Taken, Rest} = lists:partition(fun({K, _}) -> is_atom(K) andalso lists:member(K, Settable);
                                       (_) -> false
                                    end, Flat),
    Updates = maps:to_list(Groups) ++ [{F, true} || F <- Flags]
        ++ [KV || {_, V} = KV <- Taken, V =/= undefined]
        ++ [{css, Literal}, {attrs, Rest}],
    lists:foldl(fun({K, V}, Acc) -> setelement(index(K, Fields, Acc), Acc, V) end,
                R, Updates).

%% @doc The class list of an element: its catalog root, the classes of its
%% modifier fields (each value checked against its group) and the literal
%% classes of `css'.
-spec classes(element(), [atom()], aihtml_catalog:entry()) -> aihtml_html:css().
classes(R, Fields, Entry) ->
    Values = maps:from_list(lists:zip(Fields, tl(tuple_to_list(R)))),
    aihtml_catalog:field_classes(Entry, Values, maps:get(css, Values)).

%% @doc The root attributes an element adds after the component's own:
%% `id', the postback bound to `Event' (the component's main event, or
%% `none') and the `attrs' field.
-spec root_attrs(element(), atom() | none) -> aihtml_html:attrs().
root_attrs(R, Event) ->
    #{id := Id, attrs := Attrs, postback := Postback, delegate := Delegate} = base(R),
    [{id, Id}, postback(Postback, Event, Delegate, element(1, R)), attrs_list(Attrs)].

%% @doc The catalog name of a component record: ah_button -> button.
-spec component_name(atom()) -> atom().
component_name(Tag) ->
    case atom_to_binary(Tag, utf8) of
        <<"ah_", Name/binary>> -> binary_to_existing_atom(Name, utf8);
        _ -> error({aihtml, {not_a_component_record, Tag}})
    end.

%%%===================================================================
%%% Internal
%%%===================================================================

postback(undefined, _, _, _) -> [];
postback(_, none, _, Name) -> error({aihtml, {no_postback_event, Name}});
postback({Action, Args, Opts}, Event, Delegate, _) when is_atom(Action), is_map(Opts) ->
    aihtml:on(Event, {Delegate, Action, Args}, Opts);
postback({Action, Args}, Event, Delegate, _) when is_atom(Action) ->
    aihtml:on(Event, {Delegate, Action, Args});
postback(Action, Event, Delegate, _) when is_atom(Action) ->
    aihtml:on(Event, {Delegate, Action, #{}});
postback(Other, _, _, Name) -> error({aihtml, {bad_postback, Name, Other}}).

index(K, Fields, R) ->
    case string:str(Fields, [K]) of
        0 -> error({aihtml, {no_such_field, element(1, R), K}});
        I -> I + 1
    end.

attrs_list(M) when is_map(M) -> lists:sort(maps:to_list(M));
attrs_list(L) -> L.

flat_attrs(M) when is_map(M) -> lists:sort(maps:to_list(M));
flat_attrs(L) when is_list(L) ->
    lists:flatmap(fun(X) when is_list(X); is_map(X) -> flat_attrs(X);
                     (X) -> [X]
                  end, L).
