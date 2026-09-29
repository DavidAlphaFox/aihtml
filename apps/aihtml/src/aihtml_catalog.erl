%%%-------------------------------------------------------------------
%%% @doc Prefab metadata: the single source for modifier validation,
%%% documentation and tooling (it is the Erlang counterpart of sigil's
%%% per-component meta record).
%%%
%%% A prefab's `Css' argument mixes two kinds of entries:
%%%
%%%   atom     a semantic modifier, validated here and written as
%%%            `<root>-<modifier>', e.g. `primary' -> `ah-btn-primary'
%%%   binary   literal classes, usually Tailwind utilities, written as is
%%%
%%% Modifiers come in groups (at most one per group, a group may have a
%%% default) and flags (any number). An unknown atom is an error, so a typo
%%% fails loudly instead of silently rendering an unstyled control.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_catalog).

-export([prefabs/0, prefab/1, classes/2, flags/2, split_options/2]).

-export_type([prefab/0]).

-type prefab() :: #{name := atom(),
                    category := form | display | theme,
                    signature := binary(),
                    root := binary(),
                    groups := #{atom() => {[atom()], atom() | none}},
                    flags := [atom()],
                    options := [atom()],
                    behavior := binary() | none,
                    events := [binary()],
                    doc := binary()}.

-spec prefabs() -> [prefab()].
prefabs() ->
    [p(button, form, <<"button(Content, Value, Css, Attrs)">>, <<"ah-btn">>,
       #{variant => {[primary, secondary, outline, ghost, danger], primary},
         size => {[sm, lg], none}},
       [block], [], none, [],
       <<"A button. Attrs go to the <button>; type defaults to \"button\".">>),
     p(checkbox, form, <<"checkbox(Content, Value, Css, Attrs)">>, <<"ah-check">>,
       #{}, [], [], <<"check">>, [<<"ah:change">>],
       <<"A labelled checkbox. Css styles the <label>, Attrs go to the <input>.">>),
     p(radio, form, <<"radio(Content, Value, Css, Attrs)">>, <<"ah-radio">>,
       #{}, [], [], <<"check">>, [<<"ah:change">>],
       <<"A labelled radio button. Css styles the <label>, Attrs go to the <input>.">>),
     p(switch, form, <<"switch(Content, Value, Css, Attrs)">>, <<"ah-switch">>,
       #{}, [], [], <<"check">>, [<<"ah:change">>],
       <<"A toggle switch (checkbox with role=switch). Attrs go to the <input>.">>),
     p(input, form, <<"input(Value, Css, Attrs)">>, <<"ah-input">>,
       #{size => {[sm, lg], none}}, [invalid], [], none, [],
       <<"A text input. type defaults to \"text\".">>),
     p(textarea, form, <<"textarea(Value, Css, Attrs)">>, <<"ah-textarea">>,
       #{}, [invalid], [], none, [],
       <<"A multi-line text input.">>),
     p(select, form, <<"select(Options, Value, Css, Attrs)">>, <<"ah-select">>,
       #{size => {[sm, lg], none}}, [invalid], [], none, [],
       <<"A native select. Options are [{Value, Label} | Value];"
         " the option equal to Value is selected.">>),
     p(field, form, <<"field(Label, Control, Css, Attrs)">>, <<"ah-field">>,
       #{}, [], [for, help, error], none, [],
       <<"Label, control and help or error text stacked together.">>),
     p(card, display, <<"card(Children, Css, Attrs)">>, <<"ah-card">>,
       #{}, [flat], [title], none, [],
       <<"A surface with an optional title.">>),
     p(alert, display, <<"alert(Children, Css, Attrs)">>, <<"ah-alert">>,
       #{variant => {[info, success, warning, error], info}},
       [dismissible], [], <<"alert">>, [<<"ah:dismiss">>],
       <<"An inline message with role=alert.">>),
     p(badge, display, <<"badge(Children, Css, Attrs)">>, <<"ah-badge">>,
       #{variant => {[neutral, primary, success, warning, error], neutral}},
       [], [], none, [],
       <<"A small status label.">>),
     p(tabs, display, <<"tabs(Tabs, Active, Css, Attrs)">>, <<"ah-tabs">>,
       #{}, [], [], <<"tabs">>, [<<"ah:tab">>],
       <<"Tabs = [{Key, Label, Panel}]. Panels are all rendered;"
         " the jQuery runtime switches them without a round trip.">>),
     p(theme_switcher, theme, <<"theme_switcher(Css, Attrs)">>, <<"ah-theme-switcher">>,
       #{}, [], [], <<"theme-switcher">>, [<<"ah:theme">>],
       <<"Four selects, one per theme axis.">>)].

-spec prefab(atom()) -> prefab().
prefab(Name) ->
    case [P || #{name := N} = P <- prefabs(), N =:= Name] of
        [P] -> P;
        []  -> error({aihtml, {unknown_prefab, Name}})
    end.

%% @doc Resolve a prefab's Css argument into its class list.
-spec classes(atom(), aihtml_html:css()) -> aihtml_html:css().
classes(Name, Css) ->
    #{root := Root, groups := Groups, flags := Flags} = prefab(Name),
    {Mods, Literal} = lists:partition(fun is_atom/1, flatten(Css)),
    Chosen = choose(Name, Mods, Groups, Flags),
    [Root | [<<Root/binary, "-", (atom_to_binary(M, utf8))/binary>> || M <- Chosen]]
        ++ Literal.

%% @doc The flags present in a Css argument (already validated by
%% `classes/2').
-spec flags(atom(), aihtml_html:css()) -> [atom()].
flags(Name, Css) ->
    #{flags := Flags} = prefab(Name),
    [M || M <- flatten(Css), is_atom(M), lists:member(M, Flags)].

%% @doc Take the prefab's own options (see `options' in the catalog) out of
%% an attribute set, returning them as a map and the rest as HTML attributes.
-spec split_options(atom(), aihtml_html:attrs()) ->
          {#{atom() => term()}, aihtml_html:attrs()}.
split_options(Name, Attrs) ->
    #{options := Keys} = prefab(Name),
    {Opts, Rest} = lists:partition(fun({K, _}) -> lists:member(K, Keys);
                                      (_) -> false
                                   end, flat_attrs(Attrs)),
    {maps:from_list(Opts), Rest}.

%%%===================================================================
%%% Internal
%%%===================================================================

p(Name, Cat, Sig, Root, Groups, Flags, Options, Behavior, Events, Doc) ->
    #{name => Name, category => Cat, signature => Sig, root => Root,
      groups => Groups, flags => Flags, options => Options,
      behavior => Behavior, events => Events, doc => Doc}.

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
