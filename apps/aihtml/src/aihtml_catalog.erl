%%%-------------------------------------------------------------------
%%% @doc Component metadata: the single source for modifier validation,
%%% documentation and tooling (the Erlang counterpart of sigil's per-
%%% component meta record).
%%%
%%% Each component module exports `catalog/0' returning its entry; this
%%% module aggregates them (see ?COMPONENTS) and resolves a component's `Css'
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

-export([prefabs/0, prefab/1, entry/2, classes/2, flags/2, split_options/2, modules/0,
         parse_css/2, field_classes/3]).

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

%% Component modules, in catalog order: one module per component
%% (aihtml_<name>), plus aihtml_theme for the theme switcher. A module that
%% is not loaded is skipped.
-define(COMPONENTS, [aihtml_theme,
                   aihtml_button, aihtml_link_button, aihtml_toggle_button,
                   aihtml_button_group, aihtml_segmented_control,
                   aihtml_dropdown_button, aihtml_split_button, aihtml_checkbox,
                   aihtml_radiobutton, aihtml_switch_button, aihtml_checkbox_group,
                   aihtml_radiobutton_group, aihtml_radio_cards,
                   aihtml_rating_group, aihtml_input, aihtml_textarea,
                   aihtml_password_input, aihtml_number_input, aihtml_input_otp,
                   aihtml_tag_input, aihtml_markdown_editor, aihtml_markdown_view,
                   aihtml_dropdownlist,
                   aihtml_select,
                   aihtml_slider, aihtml_field, aihtml_form_layout,
                   aihtml_datepicker, aihtml_combobox, aihtml_timepicker,
                   aihtml_colorpicker, aihtml_calendar, aihtml_datetime_input,
                   aihtml_cascader, aihtml_listbox, aihtml_transfer,
                   aihtml_masked_input, aihtml_formatted_input,
                   aihtml_range_selector, aihtml_repeat_button, aihtml_upload,
                   aihtml_card, aihtml_panel, aihtml_expander, aihtml_tabs,
                   aihtml_tab_bar, aihtml_breadcrumbs, aihtml_pagination,
                   aihtml_steps, aihtml_skeleton, aihtml_loader, aihtml_empty,
                   aihtml_menu, aihtml_navbar, aihtml_sidenav, aihtml_toolbar,
                   aihtml_splitter, aihtml_listmenu, aihtml_status_bar,
                   aihtml_scrollview, aihtml_scrollbar, aihtml_responsive_panel,
                   aihtml_activity_bar, aihtml_navigationbar, aihtml_command,
                   aihtml_sortable, aihtml_dragdrop, aihtml_docking,
                   aihtml_dock_layout, aihtml_ribbon, aihtml_tile_layout,
                   aihtml_tooltip, aihtml_popover, aihtml_drawer, aihtml_sheet,
                   aihtml_toast, aihtml_notification, aihtml_window, aihtml_avatar,
                   aihtml_badge, aihtml_chip, aihtml_aspect_ratio, aihtml_kbd,
                   aihtml_time_ago, aihtml_expandable_text, aihtml_alert,
                   aihtml_progressbar, aihtml_progress_circle, aihtml_meter,
                   aihtml_statistic, aihtml_kpi_card, aihtml_timeline,
                   aihtml_ranking_list, aihtml_tag_cloud, aihtml_tree,
                   aihtml_nav_tree, aihtml_diff, aihtml_heatmap_calendar,
                   aihtml_datagrid, aihtml_pivotgrid, aihtml_treegrid,
                   aihtml_datatable, aihtml_gantt, aihtml_scheduler,
                   aihtml_swimlane, aihtml_chart, aihtml_area_chart,
                   aihtml_bar_chart, aihtml_donut_chart, aihtml_radar_chart,
                   aihtml_relation_graph, aihtml_node_graph]).
%% @doc The modules that define components, in catalog order.
-spec modules() -> [module()].
modules() -> ?COMPONENTS.

-spec prefabs() -> [entry()].
prefabs() ->
    [normalize(E) || M <- modules(), loaded(M), E <- M:catalog()].

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

%% @doc Split a Css argument for an element record's builder: the chosen
%% modifier of each group that has one, the flags present, and the literal
%% classes. Unknown and conflicting modifiers fail as in classes/2.
-spec parse_css(atom() | entry(), aihtml_html:css()) ->
          {#{atom() => atom()}, [atom()], aihtml_html:css()}.
parse_css(Name, Css) when is_atom(Name) -> parse_css(prefab(Name), Css);
parse_css(Entry0, Css) ->
    #{name := Name, groups := Groups, flags := Flags} = normalize(Entry0),
    {Mods, Literal} = lists:partition(fun is_atom/1, flatten(Css)),
    _ = choose(Name, Mods, Groups, Flags),
    Chosen = maps:from_list([{G, M} || M <- Mods, {ok, G} <- [group_of(M, Groups)]]),
    {Chosen, lists:usort([M || M <- Mods, lists:member(M, Flags)]), Literal}.

%% @doc The class list of an element record: `Values' holds its fields by
%% name; each modifier group is read from the field of the same name
%% (undefined only where the group has no default), each flag from a
%% boolean field. `Literal' must hold classes only, no modifier atoms.
-spec field_classes(atom() | entry(), #{atom() => term()}, aihtml_html:css()) ->
          aihtml_html:css().
field_classes(Name, Values, Literal) when is_atom(Name) ->
    field_classes(prefab(Name), Values, Literal);
field_classes(Entry0, Values, Literal) ->
    #{name := Name, groups := Groups, flags := Flags} = Entry = normalize(Entry0),
    [error({aihtml, {modifier_in_css, Name, A}}) || A <- flatten(Literal), is_atom(A)],
    Mods = [case maps:get(G, Values, Default) of
                undefined when Default =:= none -> [];
                V -> lists:member(V, Ms) orelse
                         error({aihtml, {bad_modifier, Name, G, V, Ms}}),
                     [V]
            end || {G, {Ms, Default}} <- lists:sort(maps:to_list(Groups))],
    Set = [F || F <- Flags,
                case maps:get(F, Values, false) of
                    B when is_boolean(B) -> B;
                    V -> error({aihtml, {bad_flag, Name, F, V}})
                end],
    classes(Entry, [Mods, Set, Literal]).

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
