%%%-------------------------------------------------------------------
%%% @doc sigil's cascader (form/cascader): a multi-level picker in a
%%% popup. See designs/04-components.md.
%%%
%%%   cascader(Items, Value, Css, Attrs)        the component
%%%   cascader_children(Ctx, Event, Children)   (in an action) a lazy level
%%%
%%% A value-bearing component: `Attrs' go to the root, which carries
%%% `data-ah-value' (the path joined with commas) and fires `change';
%%% `name' goes to a hidden input. The behaviour lives in
%%% assets/js/components/cascader.js.
%%%
%%% Everything is rendered here: the menu columns (all of them, the
%%% behaviour shows the ones on the open path) and the search panel. The
%%% browser builds no HTML besides single shell elements (the loading and
%%% empty messages).
%%%
%%% == Lazy levels ==
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
%%% The component function builds an #ah_cascader{} record (include/
%%% aihtml_cascader.hrl) and render/1 turns it into HTML
%%% (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_cascader).
-behaviour(aihtml_element).

-include("aihtml_cascader.hrl").

-export([cascader/4, cascader_children/3, cascader_children/4,
         render/1, fields/1, catalog/0, facade_extras/0]).

-export_type([cascader_node/0, children/0]).

-import(aihtml_lib_list, [ensure_id/1, sub_id/2, hidden/2, text/1, join/1]).

-define(H, aihtml_html).
-define(E, aihtml_element).

%% The children of a cascader node: a list, or `lazy' (loaded by the
%% cascader's `load' action when the node is opened).
-type children() :: [cascader_node()] | lazy.
%% A cascader node: a text that is both value and label, `{Value, Label}',
%% `{Value, Label, Children}', or a map with `value' and optionally
%% `label', `disabled' and `children'. A node without children is a leaf.
-type cascader_node() :: binary() | atom() | integer() | {term(), term()}
                       | {term(), term(), children()}
                       | #{value := term(), label => term(), disabled => boolean(),
                           children => children()}.

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
    ?E:build(?MODULE, #ah_cascader{items = Items, value = Value}, Css, Attrs).

-spec render(#ah_cascader{}) -> aihtml_html:html().
render(#ah_cascader{items = Items0, value = Value0, name = Name,
                    disabled = Disabled, separator = Sep0} = R0) ->
    {Id, R} = ensure_id(R0),
    Classes = ?E:classes(?MODULE, R),                       % checks the flag fields first
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

%% @doc Functions besides the component that the aihtml facade re-exports.
-spec facade_extras() -> [{atom(), arity()}].
facade_extras() -> [{cascader_children, 3}, {cascader_children, 4}].

%%%===================================================================
%%% Record and catalog
%%%===================================================================

%% @doc The field names of #ah_cascader{}.
-spec fields(atom()) -> [atom()].
fields(ah_cascader) -> record_info(fields, ah_cascader).

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
              doc => <<"Show a lazily loaded column; called by cascader_children/3.">>}]}].
