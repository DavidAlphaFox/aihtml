%%%-------------------------------------------------------------------
%%% @doc List selection components, ported from sigil (form/cascader,
%%% form/listbox and form/transfer). See designs/04-components.md.
%%%
%%%   cascader(Items, Value, Css, Attrs)   a multi-level picker in a popup
%%%   listbox(Items, Value, Css, Attrs)    a single/multi selection list
%%%   transfer(Items, Value, Css, Attrs)   two lists with move buttons
%%%   cascader_children(Ctx, Event, Children)   (in an action) a lazy level
%%%   listbox_items(Ctx, Event, Items)          (in an action) new list rows
%%%
%%% All three are value-bearing components: `Attrs' go to the root, which
%%% carries `data-ah-value' (values joined with commas) and fires
%%% `change'; `name' goes to a hidden input. Their behaviours live in
%%% assets/js/components/form_lists.js.
%%%
%%% Everything is rendered here: the cascader's menu columns (all of them,
%%% the behaviour shows the ones on the open path), its search panel, the
%%% listbox rows and both transfer lists (moving an item moves its node).
%%% The browser builds no HTML besides single shell elements (the loading
%%% and empty messages).
%%%
%%% == Lazy cascader levels ==
%%%
%%% A node whose children are `lazy' has an arrow but no column. Opening it
%%% fires the cascader's `load' action (`{Mod, Action, Args}') with
%%%
%%%   Event.value                  the path of the node, "v1,v2"
%%%   Event.data                   #{<<"cascader">> => <root id>}
%%%
%%% and the action answers with `cascader_children(Ctx, Event, Children)',
%%% which renders the column(s) here, appends them to the menus and calls
%%% the behaviour method `childrenLoaded'. An empty list makes the node a
%%% leaf (and picks it).
%%%
%%% == Server-side listbox search ==
%%%
%%% `listbox(Items, Value, [filterable], [{search, Ref}])' binds
%%% `aihtml:on(input, Ref, #{debounce => 250})' to the filter field; the
%%% action gets the query in `Event.value' and answers with
%%% `listbox_items(Ctx, Event, Items)', which morphs the rendered rows into
%%% the list and calls the behaviour method `itemsLoaded'.
%%%
%%% Each component function builds an element record (#ah_cascader{},
%%% #ah_listbox{}, #ah_transfer{}, defined in include/aihtml_form_lists.hrl)
%%% and render/1 turns it into HTML (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_form_lists).
-behaviour(aihtml_element).

-include("aihtml_form_lists.hrl").

-export([cascader/4, listbox/4, transfer/4,
         cascader_children/3, cascader_children/4, listbox_items/3, listbox_items/4,
         render/1, fields/1, catalog/0, facade_extras/0]).

-export_type([item/0, cascader_node/0, element/0]).

-define(H, aihtml_html).
-define(E, aihtml_element).

-type item() :: ah_fl_item().
-type cascader_node() :: ah_fl_node().
-type element() :: #ah_cascader{} | #ah_listbox{} | #ah_transfer{}.

%%%===================================================================
%%% cascader
%%%===================================================================

%% @doc A read-only text field with sigil's cascading menu popup. `Items'
%% are the top-level nodes; `Value' is the path of values from the top to
%% the chosen node (`[<<"zj">>, <<"hz">>, <<"xihu">>]') or `undefined'.
%% The field shows the labels of the path joined by the separator;
%% `data-ah-value' is the path joined with commas.
%%
%% Css: `sm' | `lg' (size), `disabled', `filterable' (typing searches all
%% leaf paths), `change_on_select' (a branch can be the value too),
%% `no_arrow', `no_clear'.
%% Options (in Attrs): `placeholder' (default "Please select"),
%% `separator' (default " / "), `popup_height' (px, default 240),
%% `empty_text' (search without results, default "No results found"),
%% `load' (an action ref for `lazy' children, see the module doc).
-spec cascader([cascader_node()], [term()] | undefined, aihtml_html:css(),
               aihtml_html:attrs()) -> #ah_cascader{}.
cascader(Items, Value, Css, Attrs) ->
    build(#ah_cascader{items = Items, value = Value}, Css, Attrs).

render_cascader(#ah_cascader{items = Items0, value = Value0, name = Name,
                             disabled = Disabled, separator = Sep0} = R0) ->
    {Id, R} = ensure_id(R0),
    Classes = classes(R),                       % checks the flag fields first
    Sep = text(Sep0),
    Height = R#ah_cascader.popup_height,
    (is_integer(Height) andalso Height > 0)
        orelse error({aihtml, {bad_option, popup_height, Height}}),
    Nodes = [cnode(N) || N <- Items0],
    (Value0 =:= undefined orelse is_list(Value0))
        orelse error({aihtml, {bad_option, value, Value0}}),
    Path = [text(V) || V <- case Value0 of undefined -> []; L -> L end],
    Labels = path_labels(Path, Nodes),
    Filterable = R#ah_cascader.filterable,
    Load = case R#ah_cascader.load of
               undefined -> [];
               Ref -> ?H:el(span, [], [<<"ah-cascader-loader">>],
                            [{hidden, true}, {data_cascader, Id},
                             aihtml:on('ah:load', Ref, #{sync => queue})])
           end,
    MenusId = sub_id(Id, <<"menus">>),
    Input = ?H:void(input, [<<"ah-cascader-input">>],
                    [{type, text}, {id, sub_id(Id, <<"input">>)},
                     {autocomplete, off}, {spellcheck, <<"false">>},
                     {readonly, not Filterable},
                     {placeholder, R#ah_cascader.placeholder},
                     {value, iolist_to_binary(lists:join(Sep, Labels))},
                     {disabled, Disabled},
                     {role, combobox}, {aria_haspopup, listbox},
                     {aria_expanded, <<"false">>}, {aria_controls, MenusId},
                     {aria_autocomplete, Filterable andalso list}]),
    Clear = [?H:el(span, <<"×"/utf8>>, [<<"ah-cascader-clear">>],
                   [{role, button}, {aria_label, <<"Clear">>}, {hidden, Path =:= []}])
             || not R#ah_cascader.no_clear],
    Arrow = [?H:el(span, ?H:el(span, <<"▼"/utf8>>, [<<"ah-cascader-arrow-icon">>], []),
                   [<<"ah-cascader-arrow">>], [{aria_hidden, <<"true">>}])
             || not R#ah_cascader.no_arrow],
    Search = [?H:el(ul, search_items(Nodes, Sep, R#ah_cascader.change_on_select),
                    [<<"ah-cascader-search-panel">>],
                    [{id, sub_id(Id, <<"search">>)}, {role, listbox}, {hidden, true}])
              || Filterable],
    Popup = ?H:el('div',
                  [?H:el('div', columns([], 0, Nodes, Path),
                         [<<"ah-cascader-menus">>],
                         [{id, MenusId}, {role, group},
                          {style, [<<"max-height:">>, integer_to_binary(Height), <<"px">>]}]),
                   Search],
                  [<<"ah-cascader-popup">>], [{id, sub_id(Id, <<"popup">>)}]),
    ?H:el('div',
          [?H:el('div', [Input, Clear, Arrow], [<<"ah-cascader-input-area">>], []),
           hidden(Name, join(Path)),
           Popup, Load],
          Classes,
          [[{id, Id}, {data_ah, <<"cascader">>}, {data_ah_value, join(Path)},
            {data_ah_separator, Sep},
            {data_ah_empty, R#ah_cascader.empty_text},
            {data_ah_change_on_select, R#ah_cascader.change_on_select},
            {aria_disabled, Disabled andalso <<"true">>}],
           ?E:root_attrs(R, change)]).

%% A node as #{value, label, disabled, children => [node] | lazy | leaf}.
cnode(#{value := V} = M) ->
    maps:foreach(fun(K, _) ->
                         lists:member(K, [value, label, disabled, children])
                             orelse error({aihtml, {bad_cascader_node, M}})
                 end, M),
    #{value => text(V), label => text(maps:get(label, M, V)),
      disabled => maps:get(disabled, M, false) =:= true,
      children => children(maps:get(children, M, leaf), M)};
cnode({V, L}) -> cnode(#{value => V, label => L});
cnode({V, L, C}) -> cnode(#{value => V, label => L, children => C});
cnode(V) when is_binary(V); is_atom(V); is_integer(V) -> cnode(#{value => V});
cnode(Other) -> error({aihtml, {bad_cascader_node, Other}}).

children(leaf, _) -> leaf;
children([], _) -> leaf;
children(lazy, _) -> lazy;
children(L, _) when is_list(L) -> [cnode(C) || C <- L];
children(_, M) -> error({aihtml, {bad_cascader_node, M}}).

%% The labels along a path; a value not found (below a lazy node, say)
%% shows as itself.
path_labels([], _) -> [];
path_labels([V | Rest], Nodes) ->
    case [N || #{value := V1} = N <- Nodes, V1 =:= V] of
        [#{label := L, children := C} | _] ->
            [L | path_labels(Rest, case C of [_ | _] -> C; _ -> [] end)];
        [] -> [V | path_labels(Rest, [])]
    end.

%% One column for `Nodes' (the children of `Parent') and, after it, the
%% columns of every branch below. Only the top column is visible; the
%% behaviour shows the columns on the open path.
columns(Parent, Level, Nodes, Path) ->
    Active = case length(Path) > Level of
                 true -> lists:nth(Level + 1, Path);
                 false -> undefined
             end,
    OnPath = lists:prefix(Parent, Path),
    Col = ?H:el('div',
                ?H:el(ul, [menu_item(N, Level, OnPath andalso V =:= Active)
                           || #{value := V} = N <- Nodes],
                      [<<"ah-cascader-menu">>], [{role, listbox}]),
                [<<"ah-cascader-menu-column">>],
                [{data_level, Level}, {data_parent, join(Parent)}, {hidden, Level > 0}]),
    [Col | [columns(Parent ++ [V], Level + 1, C, Path)
            || #{value := V, children := [_ | _] = C} <- Nodes]].

menu_item(#{value := V, label := L, disabled := Dis, children := C}, Level, Active) ->
    Branch = C =/= leaf,
    ?H:el(li,
          [?H:el(span, L, [<<"ah-cascader-menu-item-label">>], []),
           [?H:el(span, <<"▶"/utf8>>, [<<"ah-cascader-menu-item-arrow">>],
                  [{aria_hidden, <<"true">>}]) || Branch]],
          [[<<"has-children">> || Branch], [<<"active">> || Active]],
          [{role, option}, {aria_selected, atom_to_binary(Active)},
           {aria_disabled, Dis andalso <<"true">>},
           {aria_haspopup, Branch andalso <<"true">>},
           {data_value, V}, {data_level, Level}, {data_lazy, C =:= lazy}]).

%% The flat search list: every leaf path (every path with
%% change_on_select), labels joined; a path through a disabled node is
%% disabled. Lazy nodes are not searched.
search_items(Nodes, Sep, AnyLevel) ->
    [?H:el(li, ?H:el(span, lists:join(Sep, Ls), [<<"ah-cascader-search-item-label">>], []),
           [<<"ah-cascader-search-item">>],
           [{role, option}, {aria_selected, <<"false">>},
            {aria_disabled, Dis andalso <<"true">>},
            {data_path, join(Vs)}, {data_label, iolist_to_binary(lists:join(Sep, Ls))}])
     || {Vs, Ls, Dis} <- paths(Nodes, [], [], false, AnyLevel)].

paths(Nodes, Vs, Ls, Dis0, AnyLevel) ->
    lists:append(
      [begin
           Dis = Dis0 orelse D,
           P = {Vs ++ [V], Ls ++ [L], Dis},
           case C of
               leaf -> [P];
               lazy -> [P || AnyLevel];
               _ -> [P || AnyLevel] ++ paths(C, Vs ++ [V], Ls ++ [L], Dis, AnyLevel)
           end
       end || #{value := V, label := L, disabled := D, children := C} <- Nodes]).

%% @doc Answer a cascader's `load' action: `cascader_children(Ctx, Event,
%% Children)'. Renders the column of `Children' (and of their loaded
%% descendants), appends it to the cascader's menus and calls the
%% behaviour method `childrenLoaded', which shows it (or, for no
%% children, makes the node a leaf and picks it).
-spec cascader_children(aihtml_action:ctx(), aihtml_action:event(), [cascader_node()]) -> ok.
cascader_children(Ctx, #{data := #{<<"cascader">> := Id}, value := PathBin}, Children) ->
    Path = case text(PathBin) of
               <<>> -> [];
               B -> binary:split(B, <<",">>, [global])
           end,
    cascader_children(Ctx, {id, Id}, Path, Children).

%% @doc `cascader_children/3' for a cascader `{id, RootId}' and the path
%% (a list of values) of the node whose children these are.
-spec cascader_children(aihtml_action:ctx(), {id, iodata() | atom()}, [term()],
                        [cascader_node()]) -> ok.
cascader_children(Ctx, {id, Id0}, Path0, Children) ->
    Id = text(Id0),
    Path = [text(V) || V <- Path0],
    Nodes = [cnode(C) || C <- Children],
    %% the columns arrive hidden; childrenLoaded shows them
    case Nodes of
        [] -> ok;
        _ -> aihtml_action:html(Ctx, {id, sub_id(Id, <<"menus">>)},
                                columns(Path, length(Path), Nodes, []), append)
    end,
    aihtml_action:call(Ctx, {id, Id}, childrenLoaded, [join(Path)]).

%%%===================================================================
%%% listbox
%%%===================================================================

%% @doc A focusable list, sigil's listbox. `Value' is the selected value
%% (a list of values with `multiple' or `checkboxes'), or `undefined'.
%%
%% Css: `disabled', `multiple' (Ctrl/Shift click, Shift+arrows), `checkboxes'
%% (multiple, a click toggles a row), `check_all' (a "select all" row, with
%% checkboxes), `filterable' (a filter field above the list). Size the
%% list with Css classes on the root, e.g. `<<"w-64 h-72">>'.
%% Options (in Attrs): `empty_text' (default "No data"),
%% `filter_placeholder' (default "Search"), `check_all_label' (default
%% "Select all"), `search' (an action ref: the filter asks the server,
%% see the module doc).
-spec listbox([item()], term() | [term()] | undefined, aihtml_html:css(),
              aihtml_html:attrs()) -> #ah_listbox{}.
listbox(Items, Value, Css, Attrs) ->
    build(#ah_listbox{items = Items, value = Value}, Css, Attrs).

render_listbox(#ah_listbox{items = Items0, value = Value, name = Name,
                           disabled = Disabled, checkboxes = Checkboxes} = R0) ->
    {Id, R} = ensure_id(R0),
    Classes = classes(R),
    Multi = R#ah_listbox.multiple orelse Checkboxes,
    Items = [item(I) || I <- Items0],
    Selected = selected(Multi, Value),
    Search = case R#ah_listbox.search of
                 undefined -> [];
                 Ref -> aihtml:on(input, Ref, #{debounce => 250})
             end,
    Filter = [?H:el('div',
                    ?H:void(input, [<<"ah-listbox-filter-input">>],
                            [[{type, text}, {id, sub_id(Id, <<"filter">>)},
                              {autocomplete, off},
                              {placeholder, R#ah_listbox.filter_placeholder},
                              {aria_label, R#ah_listbox.filter_placeholder},
                              {aria_controls, Id}, {disabled, Disabled},
                              {data_listbox, Id},
                              {data_checkboxes, Checkboxes andalso <<"true">>}],
                             Search]),
                    [<<"ah-listbox-filter">>], [])
              || R#ah_listbox.filterable orelse Search =/= []],
    CheckAll = [?H:el('div',
                      [?H:el(span, [], [<<"ah-listbox-checkbox">>], []),
                       ?H:el(span, R#ah_listbox.check_all_label, [<<"ah-listbox-label">>], [])],
                      [<<"ah-listbox-check-all">>], [{role, button}, {aria_pressed, <<"false">>}])
                || Checkboxes, R#ah_listbox.check_all],
    ?H:el('div',
          [Filter, CheckAll,
           ?H:el('div',
                 [?H:el(ul, listbox_rows(Items, Selected, Checkboxes, Id),
                        [<<"ah-listbox-list">>], [{id, sub_id(Id, <<"list">>)}, {role, none}]),
                  ?H:el('div', R#ah_listbox.empty_text, [<<"ah-listbox-empty">>],
                        [{hidden, Items =/= []}])],
                 [<<"ah-listbox-content">>], []),
           hidden(Name, join(Selected))],
          [Classes, [<<"ah-listbox-remote">> || Search =/= []]],
          [[{id, Id}, {data_ah, <<"listbox">>}, {data_ah_value, join(Selected)},
            {tabindex, case Disabled of true -> <<"-1">>; false -> <<"0">> end},
            {role, listbox}, {aria_multiselectable, atom_to_binary(Multi)},
            {aria_disabled, Disabled andalso <<"true">>}],
           ?E:root_attrs(R, change)]).

selected(_, undefined) -> [];
selected(true, []) -> [];
selected(true, [V1 | _] = Vs) when not is_integer(V1) -> [text(V) || V <- Vs];
selected(_, V) -> [text(V)].

%% Grouped like sigil's group-by (in order of first appearance); the row
%% index (data-idx, the id suffix) is the position in `Items'.
listbox_rows(Items, Selected, Checkboxes, Id) ->
    Indexed = lists:zip(lists:seq(0, length(Items) - 1), Items),
    Groups = lists:foldl(fun({_, I}, Acc) ->
                                 G = maps:get(group, I, undefined),
                                 case lists:member(G, Acc) of
                                     true -> Acc;
                                     false -> Acc ++ [G]
                                 end
                         end, [], Indexed),
    [[[?H:el(li, G, [<<"ah-listbox-group">>], [{role, presentation}]) || G =/= undefined],
      [listbox_row(N, I, Selected, Checkboxes, Id)
       || {N, I} <- Indexed, maps:get(group, I, undefined) =:= G]]
     || G <- Groups].

listbox_row(N, #{value := V, label := L} = I, Selected, Checkboxes, Id) ->
    Sel = lists:member(V, Selected),
    Dis = maps:get(disabled, I, false),
    ?H:el(li,
          [[?H:el(span, [], [<<"ah-listbox-checkbox">>,
                             [<<"ah-listbox-checkbox-checked">> || Sel]], [])
            || Checkboxes],
           [?H:void(img, [<<"ah-listbox-icon">>], [{src, Src}, {alt, <<>>}])
            || #{icon := Src} <- [I]],
           ?H:el(span, L, [<<"ah-listbox-label">>], [])],
          [<<"ah-listbox-item">>, [<<"ah-listbox-item-selected">> || Sel],
           [<<"ah-listbox-item-disabled">> || Dis]],
          [{id, sub_id(Id, <<"o-", (integer_to_binary(N))/binary>>)},
           {role, option}, {aria_selected, atom_to_binary(Sel)},
           {aria_disabled, Dis andalso <<"true">>},
           {data_idx, N}, {data_value, V}]).

%% @doc Answer a listbox's `search' action: `listbox_items(Ctx, Event,
%% Items)'. Morphs the rendered rows into the list (`<root id>-list') and
%% calls the behaviour method `itemsLoaded', which marks the current
%% selection and shows the empty message when there are no rows.
-spec listbox_items(aihtml_action:ctx(), aihtml_action:event() | {id, iodata() | atom()},
                    [item()]) -> ok.
listbox_items(Ctx, #{data := Data}, Items) ->
    listbox_items(Ctx, {id, maps:get(<<"listbox">>, Data)}, Items,
                  #{checkboxes => maps:get(<<"checkboxes">>, Data, <<>>) =:= <<"true">>});
listbox_items(Ctx, Target, Items) ->
    listbox_items(Ctx, Target, Items, #{}).

%% @doc `listbox_items/3' with options: `checkboxes' (render check boxes,
%% default false) and `selected' (values to mark; the browser marks its
%% current selection anyway).
-spec listbox_items(aihtml_action:ctx(), {id, iodata() | atom()}, [item()],
                    #{checkboxes => boolean(), selected => [term()]}) -> ok.
listbox_items(Ctx, {id, Id0}, Items, Opts) ->
    Id = text(Id0),
    Html = listbox_rows([item(I) || I <- Items],
                        [text(V) || V <- maps:get(selected, Opts, [])],
                        maps:get(checkboxes, Opts, false), Id),
    aihtml_action:html(Ctx, {id, sub_id(Id, <<"list">>)}, Html, morph_inner),
    aihtml_action:call(Ctx, {id, Id}, itemsLoaded, []).

%%%===================================================================
%%% transfer
%%%===================================================================

%% @doc Sigil's transfer: the items not chosen on the left, the chosen
%% ones on the right. `Value' is the list of chosen values, in the order
%% of the right list; `data-ah-value' joins them with commas. Click rows
%% to select them (Space and arrows in a focused list), then move them
%% with the buttons, Enter or a double click.
%%
%% Css: `disabled', `no_filter' (no filter fields).
%% Options (in Attrs): `source_title' (default "Source"), `target_title'
%% (default "Target"), `filter_placeholder' (default "Search"),
%% `empty_text' (an empty list, default "No data").
-spec transfer([item()], [term()], aihtml_html:css(), aihtml_html:attrs()) -> #ah_transfer{}.
transfer(Items, Value, Css, Attrs) ->
    build(#ah_transfer{items = Items, value = Value}, Css, Attrs).

render_transfer(#ah_transfer{items = Items0, value = Value0, name = Name,
                             disabled = Disabled} = R0) ->
    {Id, R} = ensure_id(R0),
    Classes = classes(R),
    is_list(Value0) orelse error({aihtml, {bad_option, value, Value0}}),
    Items = lists:zip(lists:seq(0, length(Items0) - 1), [item(I) || I <- Items0]),
    Chosen = [text(V) || V <- Value0],
    Source = [I || {_, #{value := V}} = I <- Items, not lists:member(V, Chosen)],
    Target = lists:append([[I || {_, #{value := V1}} = I <- Items, V1 =:= V] || V <- Chosen]),
    Value = [V || {_, #{value := V}} <- Target],
    Filter = not R#ah_transfer.no_filter,
    Panel = fun(Side, Title, Rows) ->
                    SideB = atom_to_binary(Side),
                    ListId = sub_id(Id, SideB),
                    ?H:el('div',
                          [?H:el('div',
                                 [?H:el(span, Title, [<<"ah-transfer-panel-title">>],
                                        [{id, sub_id(ListId, <<"title">>)}]),
                                  ?H:el(span, integer_to_binary(length(Rows)),
                                        [<<"ah-transfer-panel-count">>], [])],
                                 [<<"ah-transfer-panel-header">>], []),
                           [?H:el('div',
                                  ?H:void(input, [<<"ah-transfer-filter-input">>],
                                          [{type, text}, {autocomplete, off},
                                           {placeholder, R#ah_transfer.filter_placeholder},
                                           {aria_label, R#ah_transfer.filter_placeholder},
                                           {disabled, Disabled}, {data_panel, SideB}]),
                                  [<<"ah-transfer-filter">>], [])
                            || Filter],
                           ?H:el('div',
                                 ?H:el(ul, [transfer_row(N, I, SideB, Id) || {N, I} <- Rows],
                                       [<<"ah-transfer-list">>],
                                       [{id, ListId}, {data_panel, SideB}, {role, listbox},
                                        {tabindex, case Disabled of
                                                       true -> <<"-1">>;
                                                       false -> <<"0">>
                                                   end},
                                        {aria_multiselectable, <<"true">>},
                                        {aria_labelledby, sub_id(ListId, <<"title">>)},
                                        {data_empty_text, R#ah_transfer.empty_text}]),
                                 [<<"ah-transfer-panel-content">>], [])],
                          [<<"ah-transfer-panel">>,
                           <<"ah-transfer-panel-", SideB/binary>>], [])
            end,
    Button = fun(Dir, Label) ->
                     ?H:el(button, <<"›"/utf8>>,
                           [<<"ah-transfer-btn">>, <<"ah-transfer-btn-", Dir/binary>>,
                            <<"ah-transfer-btn-disabled">>],
                           [{type, button}, {data_direction, Dir}, {title, Label},
                            {aria_label, Label}, {disabled, true}])
             end,
    ?H:el('div',
          [?H:el('div',
                 [Panel(source, R#ah_transfer.source_title, Source),
                  ?H:el('div', [Button(<<"to-target">>, <<"Move to target">>),
                                Button(<<"to-source">>, <<"Move to source">>)],
                        [<<"ah-transfer-buttons">>], []),
                  Panel(target, R#ah_transfer.target_title, Target)],
                 [<<"ah-transfer-panels">>], []),
           hidden(Name, join(Value))],
          Classes,
          [[{id, Id}, {data_ah, <<"transfer">>}, {data_ah_value, join(Value)},
            {aria_disabled, Disabled andalso <<"true">>}],
           ?E:root_attrs(R, change)]).

transfer_row(N, #{value := V, label := L} = I, Side, Id) ->
    Dis = maps:get(disabled, I, false),
    ?H:el(li,
          [[?H:el(span, Icon, [<<"ah-transfer-item-icon">>], [{aria_hidden, <<"true">>}])
            || #{icon := Icon} <- [I]],
           ?H:el(span, L, [<<"ah-transfer-item-label">>], [])],
          [<<"ah-transfer-item">>, [<<"ah-transfer-item-disabled">> || Dis]],
          [{id, sub_id(Id, <<"i-", (integer_to_binary(N))/binary>>)},
           {role, option}, {aria_selected, <<"false">>},
           {aria_disabled, Dis andalso <<"true">>},
           {data_value, V}, {data_idx, N}, {data_source, Side}]).

%%%===================================================================
%%% Shared
%%%===================================================================

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

%% @doc Functions besides the components that the aihtml facade re-exports.
-spec facade_extras() -> [{atom(), arity()}].
facade_extras() ->
    [{cascader_children, 3}, {cascader_children, 4}, {listbox_items, 3}, {listbox_items, 4}].

join(Vs) -> iolist_to_binary(lists:join(<<",">>, Vs)).

%%%===================================================================
%%% Records
%%%===================================================================

build(R, Css, Attrs) ->
    Tag = element(1, R),
    ?E:build(R, fields(Tag), entry(?E:component_name(Tag)), Css, Attrs).

%% @doc The field names of one of this group's records.
-spec fields(atom()) -> [atom()].
fields(ah_cascader) -> record_info(fields, ah_cascader);
fields(ah_listbox) -> record_info(fields, ah_listbox);
fields(ah_transfer) -> record_info(fields, ah_transfer).

-spec render(element()) -> aihtml_html:html().
render(#ah_cascader{} = R) -> render_cascader(R);
render(#ah_listbox{} = R) -> render_listbox(R);
render(#ah_transfer{} = R) -> render_transfer(R).

classes(R) ->
    Tag = element(1, R),
    ?E:classes(R, fields(Tag), entry(?E:component_name(Tag))).

entry(Name) -> aihtml_catalog:entry(?MODULE, Name).

%% The parts refer to each other by id (aria-controls, the actions' event
%% data), so a root without an id gets one. Returns the id and the record
%% holding it, for root_attrs/2.
ensure_id(R) ->
    Id = case element(3, R) of
             undefined -> <<"ah-l", (integer_to_binary(erlang:unique_integer([positive])))/binary>>;
             Id0 -> text(Id0)
         end,
    {Id, setelement(3, R, Id)}.

sub_id(Id, Part) -> <<Id/binary, "-", Part/binary>>.

hidden(undefined, _) -> [];
hidden(Name, Value) -> ?H:void(input, [], [{type, hidden}, {name, Name}, {value, Value}]).

text(undefined) -> <<>>;
text(B) when is_binary(B) -> B;
text(L) when is_list(L) -> unicode:characters_to_binary(L);
text(X) -> beamai_html_escape:to_binary(X, aihtml).

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => cascader, category => form,
       signature => <<"cascader(Items, Value, Css, Attrs)">>,
       root => <<"ah-cascader">>,
       groups => #{size => {[sm, lg], none}},
       flags => [disabled, filterable, change_on_select, no_arrow, no_clear],
       classes => #{change_on_select => [<<"ah-cascader-change-on-select">>],
                    no_arrow => [<<"ah-cascader-no-arrow">>],
                    no_clear => [<<"ah-cascader-no-clear">>]},
       options => [placeholder, separator, popup_height, empty_text, load],
       behavior => <<"cascader">>,
       events => [<<"change">>, <<"ah:open">>, <<"ah:close">>],
       doc => <<"A multi-level picker: one menu column per level in a popup, the value is "
                "the path to a leaf; keyboard navigation, search and lazily loaded levels.">>,
       option_docs =>
           #{disabled => <<"Not editable; the popup does not open.">>,
             filterable => <<"Typing searches every leaf path (labels joined by the separator).">>,
             change_on_select => <<"A branch can be the value too: picking it sets the value "
                                   "and keeps the popup open.">>,
             no_arrow => <<"Hide the dropdown arrow.">>,
             no_clear => <<"No clear button (it shows on hover when there is a value).">>,
             placeholder => <<"Text of the empty field (default \"Please select\").">>,
             separator => <<"Between the labels of the path in the field (default \" / \").">>,
             popup_height => <<"Maximum height of the menus in px (default 240).">>,
             empty_text => <<"Shown when a search finds nothing (default \"No results found\").">>,
             load => <<"Action ref {Module, Action, Args} run when a node with children = lazy "
                       "opens; Event.value is its path \"v1,v2\", the action answers with "
                       "cascader_children/3.">>},
       methods =>
           [#{name => setValue, args => <<"(\"v1,v2,v3\" | [V1, V2, V3])">>,
              doc => <<"Set the path without firing change.">>},
            #{name => getValue, args => <<"()">>, doc => <<"Return data-ah-value.">>},
            #{name => getLabels, args => <<"()">>,
              doc => <<"Return the labels of the selected path.">>},
            #{name => clear, args => <<"()">>, doc => <<"Empty the value and fire change.">>},
            #{name => open, args => <<"()">>, doc => <<"Open the popup.">>},
            #{name => close, args => <<"()">>, doc => <<"Close the popup.">>},
            #{name => childrenLoaded, args => <<"(Path)">>,
              doc => <<"Show a lazily loaded column; called by cascader_children/3.">>}]},
     #{name => listbox, category => form,
       signature => <<"listbox(Items, Value, Css, Attrs)">>,
       root => <<"ah-listbox">>,
       flags => [disabled, multiple, checkboxes, check_all, filterable],
       %% sigil's ah-listbox-check-all is the check-all row itself
       classes => #{check_all => []},
       options => [empty_text, filter_placeholder, check_all_label, search],
       behavior => <<"listbox">>,
       events => [<<"change">>],
       doc => <<"A focusable list with single or multiple selection, check boxes, groups, "
                "a filter and full keyboard navigation (arrows, Home/End, PageUp/PageDown, "
                "type-ahead).">>,
       option_docs =>
           #{disabled => <<"Not focusable, no selection.">>,
             multiple => <<"Several values: Ctrl+click toggles, Shift+click and Shift+arrows "
                           "select a range, Space toggles; value \"a,b,c\".">>,
             checkboxes => <<"Multiple, with a check box on every row; a click toggles it.">>,
             check_all => <<"With checkboxes: a row above the list that checks every visible "
                            "row.">>,
             filterable => <<"A filter field above the list (hides the rows that do not "
                             "contain the text).">>,
             empty_text => <<"Shown when there are no rows (default \"No data\").">>,
             filter_placeholder => <<"Placeholder of the filter (default \"Search\").">>,
             check_all_label => <<"Text of the check-all row (default \"Select all\").">>,
             search => <<"Action ref {Module, Action, Args} run (debounced) as the user types "
                         "in the filter (implies it); Event.value is the query, the action "
                         "answers with listbox_items/3.">>},
       methods =>
           [#{name => setValue, args => <<"(Value | [Value])">>,
              doc => <<"Set the selection without firing change.">>},
            #{name => getValue, args => <<"()">>, doc => <<"Return data-ah-value.">>},
            #{name => clear, args => <<"()">>, doc => <<"Empty the selection and fire change.">>},
            #{name => filter, args => <<"(Text)">>, doc => <<"Filter the rows by text.">>},
            #{name => itemsLoaded, args => <<"()">>,
              doc => <<"Re-read the rows after listbox_items/3 morphed them in; called by "
                       "listbox_items itself.">>}]},
     #{name => transfer, category => form,
       signature => <<"transfer(Items, Value, Css, Attrs)">>,
       root => <<"ah-transfer">>,
       flags => [disabled, no_filter],
       classes => #{no_filter => [<<"ah-transfer-no-filter">>]},
       options => [source_title, target_title, filter_placeholder, empty_text],
       behavior => <<"transfer">>,
       events => [<<"change">>],
       doc => <<"Two lists with move buttons: the value is the list of keys on the right, "
                "in order. Rows are selected by click or keyboard and moved by the buttons, "
                "Enter or a double click.">>,
       option_docs =>
           #{disabled => <<"Nothing can be selected or moved.">>,
             no_filter => <<"No filter fields above the lists.">>,
             source_title => <<"Title of the left list (default \"Source\").">>,
             target_title => <<"Title of the right list (default \"Target\").">>,
             filter_placeholder => <<"Placeholder of the filters (default \"Search\").">>,
             empty_text => <<"Shown in an empty list (default \"No data\").">>},
       methods =>
           [#{name => setValue, args => <<"([Key] | \"k1,k2\")">>,
              doc => <<"Put these keys on the right, in this order, without firing change.">>},
            #{name => getValue, args => <<"()">>, doc => <<"Return data-ah-value.">>},
            #{name => moveToTarget, args => <<"()">>,
              doc => <<"Move the selected rows of the left list; fires change.">>},
            #{name => moveToSource, args => <<"()">>,
              doc => <<"Move the selected rows of the right list; fires change.">>},
            #{name => selectAll, args => <<"(\"source\" | \"target\")">>,
              doc => <<"Select every visible enabled row of a list.">>},
            #{name => clearSelection, args => <<"(\"source\" | \"target\")">>,
              doc => <<"Unselect the rows of a list.">>}]}].
