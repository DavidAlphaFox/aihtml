%%%-------------------------------------------------------------------
%%% @doc Component metadata: the single source for modifier validation,
%%% documentation and tooling (the Erlang counterpart of sigil's per-
%%% component meta record).
%%%
%%% Each component group module exports `catalog/0' returning entries; this
%%% module aggregates them (see ?GROUPS) and resolves a component's `Css'
%%% argument, whose entries are:
%%%
%%%   atom     a semantic modifier, validated here and written as
%%%            `<root>-<modifier>', e.g. `primary' -> `ah-btn-primary'
%%%            (an entry may map a modifier to other classes, see `classes')
%%%   binary   literal classes, usually Tailwind utilities, written as is
%%%
%%% Modifiers come in groups (at most one per group, a group may have a
%%% default) and flags (any number). An unknown atom is an error, so a typo
%%% fails loudly instead of silently rendering an unstyled control.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_catalog).

-export([prefabs/0, prefab/1, entry/2, classes/2, flags/2, split_options/2, groups/0]).

-export_type([entry/0]).

%% Required keys: name, category, signature, root. Everything else has a
%% default (see normalize/1).
%%
%%   groups    #{Group => {[Modifier], Default | none}}
%%   flags     [Modifier]
%%   classes   #{Modifier => [binary()]}: classes to write instead of
%%             <root>-<modifier> (for sigil names that do not follow it)
%%   options   [atom()]: keys taken out of Attrs as component options
%%   behavior  the data-ah value aihtml.js attaches to, or none
%%   events    DOM events the component fires (for on/2 and docs)
%%   doc       one or two sentences
%%   option_docs  #{Option | Flag => binary()}: what each option / flag does
%%   methods   [#{name := atom(), args := binary(), doc := binary()}]: the
%%             behaviour methods aihtml_action:call/4 and AH.invoke reach
-type entry() :: #{name := atom(),
                   category := atom(),
                   signature := binary(),
                   root := binary(),
                   groups => #{atom() => {[atom()], atom() | none}},
                   flags => [atom()],
                   classes => #{atom() => [binary()]},
                   options => [atom()],
                   behavior => binary() | none,
                   events => [binary()],
                   doc => binary(),
                   option_docs => #{atom() => binary()},
                   methods => [#{name := atom(), args := binary(), doc := binary()}]}.

%% Component group modules, in catalog order. A module that is not there
%% (yet) is skipped.
-define(GROUPS, [aihtml_theme,
                 aihtml_form_buttons, aihtml_form_choice, aihtml_form_text,
                 aihtml_form_select, aihtml_form_pickers, aihtml_form_time_color,
                 aihtml_layout_basic,
                 aihtml_layout_nav,
                 aihtml_overlay, aihtml_display]).

-spec groups() -> [module()].
groups() -> ?GROUPS.

-spec prefabs() -> [entry()].
prefabs() ->
    [normalize(E) || M <- ?GROUPS, loaded(M), E <- M:catalog()].

-spec prefab(atom()) -> entry().
prefab(Name) ->
    case [P || #{name := N} = P <- prefabs(), N =:= Name] of
        [P | _] -> P;
        []  -> error({aihtml, {unknown_prefab, Name}})
    end.

%% @doc The entry `Name' of one group module's catalog, normalised. Group
%% modules use it so they never depend on the global order.
-spec entry(module(), atom()) -> entry().
entry(Mod, Name) ->
    case [E || #{name := N} = E <- Mod:catalog(), N =:= Name] of
        [E | _] -> normalize(E);
        [] -> error({aihtml, {unknown_prefab, Name}})
    end.

%% @doc Resolve a component's Css argument into its class list.
-spec classes(atom() | entry(), aihtml_html:css()) -> aihtml_html:css().
classes(Name, Css) when is_atom(Name) -> classes(prefab(Name), Css);
classes(Entry0, Css) ->
    #{name := Name, root := Root, groups := Groups, flags := Flags,
      classes := Map} = normalize(Entry0),
    {Mods, Literal} = lists:partition(fun is_atom/1, flatten(Css)),
    Chosen = choose(Name, Mods, Groups, Flags),
    [Root | lists:append([mod_classes(Root, M, Map) || M <- Chosen])] ++ Literal.

%% @doc The flags present in a Css argument.
-spec flags(atom() | entry(), aihtml_html:css()) -> [atom()].
flags(Name, Css) when is_atom(Name) -> flags(prefab(Name), Css);
flags(Entry, Css) ->
    #{flags := Flags} = normalize(Entry),
    [M || M <- flatten(Css), is_atom(M), lists:member(M, Flags)].

%% @doc Take the component's own options out of an attribute set, returning
%% them as a map and the rest as HTML attributes.
-spec split_options(atom() | entry(), aihtml_html:attrs()) ->
          {#{atom() => term()}, aihtml_html:attrs()}.
split_options(Name, Attrs) when is_atom(Name) -> split_options(prefab(Name), Attrs);
split_options(Entry, Attrs) ->
    #{options := Keys} = normalize(Entry),
    {Opts, Rest} = lists:partition(fun({K, _}) -> lists:member(K, Keys);
                                      (_) -> false
                                   end, flat_attrs(Attrs)),
    {maps:from_list(Opts), Rest}.

%%%===================================================================
%%% Internal
%%%===================================================================

normalize(E) ->
    maps:merge(#{groups => #{}, flags => [], classes => #{}, options => [],
                 behavior => none, events => [], doc => <<>>}, E).

loaded(M) ->
    case code:ensure_loaded(M) of
        {module, M} -> erlang:function_exported(M, catalog, 0);
        _ -> false
    end.

mod_classes(Root, M, Map) ->
    case Map of
        #{M := Cs} -> Cs;
        #{} -> [<<Root/binary, "-", (atom_to_binary(M, utf8))/binary>>]
    end.

choose(Name, Mods, Groups, Flags) ->
    Picked = lists:foldl(
               fun(M, Acc) ->
                       case group_of(M, Groups) of
                           {ok, G} ->
                               case Acc of
                                   #{G := Prev} when Prev =/= M ->
                                       error({aihtml, {conflicting_modifiers,
                                                       Name, G, [Prev, M]}});
                                   _ -> Acc#{G => M}
                               end;
                           error ->
                               lists:member(M, Flags) orelse
                                   error({aihtml, {unknown_modifier, Name, M,
                                                   allowed(Groups, Flags)}}),
                               Acc
                       end
               end, #{}, Mods),
    FromGroups = [M || {G, {_, Default}} <- lists:sort(maps:to_list(Groups)),
                       M <- [maps:get(G, Picked, Default)], M =/= none],
    FromGroups ++ lists:usort([M || M <- Mods, lists:member(M, Flags)]).

group_of(M, Groups) ->
    case [G || {G, {Ms, _}} <- maps:to_list(Groups), lists:member(M, Ms)] of
        [G | _] -> {ok, G};
        []      -> error
    end.

allowed(Groups, Flags) ->
    lists:usort(lists:append([Ms || {Ms, _} <- maps:values(Groups)]) ++ Flags).

flatten(L) when is_list(L) ->
    case L =/= [] andalso io_lib:printable_unicode_list(L) of
        true  -> [L];
        false -> lists:flatmap(fun flatten/1, L)
    end;
flatten(X) -> [X].

flat_attrs(M) when is_map(M) -> lists:sort(maps:to_list(M));
flat_attrs(L) when is_list(L) ->
    lists:flatmap(fun(X) when is_list(X); is_map(X) -> flat_attrs(X);
                     (X) -> [X]
                  end, L).
