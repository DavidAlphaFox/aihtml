%%%-------------------------------------------------------------------
%%% @doc The ribbon and the tile layout, ported from sigil
%%% (layout/ribbon, layout/tile_layout). DOM and class names are the ones
%%% sigil renders, so the styles in priv/css/sigil apply unchanged.
%%%
%%%   ribbon(Tabs, Value, Css, Attrs)          an Office-style ribbon
%%%   tile_layout(Layout, Value, Css, Attrs)   resizable panes and tab groups
%%%
%%% == Ribbon ==
%%%
%%% Tabs over panels. A panel is any HTML or `{groups, Groups}': labelled
%%% groups of large and small buttons, toggles, dropdown menus, stacks and
%%% separators, in sigil's `.ah-ribbon-group' markup. `Value' is the active
%%% tab's key, kept in `data-ah-value' (plus a hidden input with `name');
%%% a user switch fires `change'. Every element with `data-command' inside
%%% the panels (the rendered buttons and menu items, or your own markup)
%%% fires `ah:command' on the root when clicked, with `data-command' (and
%%% `data-pressed' for toggles) copied onto the root first, so an action
%%% bound with postback or `on('ah:command', ...)' reads the command from
%%% `Event.data'.
%%%
%%% == Tile layout ==
%%%
%%% sigil's tile layout is an IDE-style tree, not a free dashboard grid:
%%% columns and rows of panes separated by splitbars, where a pane is a
%%% plain tile or a tab group. The user resizes panes with the splitbars
%%% (mouse, touch or arrow keys), drags tabs into another group or to an
%%% edge of any pane (which splits it), and closes tabs. Tile and tab
%%% contents are server-rendered HTML; the browser moves the live nodes.
%%%
%%% The arrangement is view state. data-ah-value holds it as JSON,
%%% `{"root": Node, "closed": [Id]}' with Node one of
%%% `{"type": "columns" | "rows", "id", "size", "items": [Node]}',
%%% `{"type": "tabs", "id", "size", "tabs": [Id], "active": Id}' or
%%% `{"type": "item", "id", "size"}'; every change fires `change'. Store
%%% `Event.value' and pass it back as `Value': the layout is then rendered
%%% in the stored arrangement, with the contents taken from `Layout' by id.
%%% Ids the layout no longer has are dropped; tiles and tabs of the layout
%%% that the stored value neither places nor lists as closed (added since)
%%% are appended. A value that does not parse is ignored.
%%%
%%% Each function builds an element record (#ah_ribbon{},
%%% #ah_tile_layout{}, include/aihtml_layout_tiles.hrl) and render/1 turns
%%% it into HTML (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_layout_tiles).
-behaviour(aihtml_element).

-include("aihtml_layout_tiles.hrl").

-export([ribbon/4, tile_layout/4, render/1, fields/1, catalog/0]).

-export_type([element/0]).

%% the last clauses reject items outside the declared types at run time
-dialyzer({no_match, [ribbon_tab/1, ribbon_group/1, ribbon_cmd/1, menu_item/1,
                      norm_node/2, norm_tab/1]}).

%% Shared templates (see aihtml_tpl): also compiled to AH.tpl.* for the browser.
-compile({parse_transform, beamai_mustache_transform}).
-mustache_template({tpl_tile_layout_tab, "../templates/tile_layout_tab.mustache"}).

-define(H, aihtml_html).
-define(E, aihtml_element).

-type html() :: aihtml_html:html().
-type css() :: aihtml_html:css().
-type attrs() :: aihtml_html:attrs().
-type element() :: #ah_ribbon{} | #ah_tile_layout{}.

%%%===================================================================
%%% Builders
%%%===================================================================

%% @doc An Office-style ribbon. `Tabs' are `{Key, Label, Content}',
%% `{Key, Label, Content, Opts}' (Opts: `icon', `disabled') or maps
%% `#{key, label, icon, disabled, content, groups}'; `Content' is HTML or
%% `{groups, [{Label, Commands}]}'. `Value' is the active tab's key (the
%% first enabled tab when undefined). Css: `top' (default), `bottom',
%% `left', `right'; `default', `collapsed', `popup'; a colour `primary',
%% `success', `warning', `danger'; `slide' or `fade'; `collapsible'.
-spec ribbon([ah_tiles_ribbon_tab()], term(), css(), attrs()) -> #ah_ribbon{}.
ribbon(Tabs, Value, Css, Attrs) ->
    build(#ah_ribbon{items = Tabs, value = Value}, Css, Attrs).

%% @doc Resizable panes and tab groups. `Layout' is the tree
%% (`{columns, Nodes}', `{rows, Nodes}', `{tabs, Tabs}', tiles
%% `#{id, content}', or maps with `id', `size', `min', ...; see
%% include/aihtml_layout_tiles.hrl). `Value' is a stored arrangement (the
%% JSON the component reported in `Event.value', or its decoded map) or
%% `undefined' for the layout as written.
-spec tile_layout(ah_tiles_node(), undefined | iodata() | map(), css(), attrs()) ->
          #ah_tile_layout{}.
tile_layout(Layout, Value, Css, Attrs) ->
    build(#ah_tile_layout{layout = Layout, value = Value}, Css, Attrs).

build(R, Css, Attrs) ->
    Tag = element(1, R),
    ?E:build(R, fields(Tag), entry(?E:component_name(Tag)), Css, Attrs).

%% @doc The field names of one of this group's records.
-spec fields(atom()) -> [atom()].
fields(ah_ribbon) -> record_info(fields, ah_ribbon);
fields(ah_tile_layout) -> record_info(fields, ah_tile_layout).

%%%===================================================================
%%% Rendering
%%%===================================================================

-spec render(element()) -> html().
render(#ah_ribbon{} = R) -> render_ribbon(R);
render(#ah_tile_layout{} = R) -> render_tile_layout(R).

%%% ribbon -------------------------------------------------------------

render_ribbon(#ah_ribbon{items = Tabs0, value = Value, name = Name,
                         disabled = Disabled} = R0) ->
    {Id, R} = ensure_id(R0),
    Classes = classes(R),                       % checks the modifier fields
    #ah_ribbon{position = Pos, selection_mode = SelMode, collapsible = Collapsible} = R,
    check(selection_mode, SelMode, [click, hover]),
    is_boolean(Disabled) orelse error({aihtml, {bad_option, disabled, Disabled}}),
    Tabs = [ribbon_tab(T) || T <- Tabs0],
    Cur = value_text(Value),
    Enabled = [K || #{key := K, disabled := false} <- Tabs],
    Active = case lists:member(Cur, Enabled) of
                 true -> Cur;
                 false -> case Enabled of [F | _] -> F; [] -> <<>> end
             end,
    Vertical = Pos =:= left orelse Pos =:= right,
    Indexed = lists:zip(lists:seq(0, length(Tabs) - 1), Tabs),
    TabButtons =
        [begin
             Sel = K =:= Active,
             Off = TOff orelse Disabled,
             ?H:el(button,
                   [[?H:el(span, Icon, [<<"ah-ribbon-tab-icon">>], [{aria_hidden, <<"true">>}])
                     || Icon =/= undefined],
                    ?H:el(span, Label, [<<"ah-ribbon-tab-text">>], [])],
                   [<<"ah-ribbon-tab">>, [<<"ah-ribbon-tab-selected">> || Sel],
                    [<<"ah-ribbon-tab-disabled">> || TOff]],
                   [{type, button}, {role, tab}, {id, sub_id(Id, <<"tab-", (idx(I))/binary>>)},
                    {data_index, I}, {data_key, K},
                    {aria_selected, atom_to_binary(Sel, utf8)},
                    {aria_controls, sub_id(Id, <<"panel-", (idx(I))/binary>>)},
                    {aria_disabled, Off andalso <<"true">>},
                    {tabindex, case Sel andalso not Off of true -> <<"0">>; false -> <<"-1">> end},
                    {disabled, Off}])
         end || {I, #{key := K, label := Label, icon := Icon, disabled := TOff}} <- Indexed],
    Panels =
        [?H:el('div', ribbon_content(sub_id(Id, <<"panel-", (idx(I))/binary>>), Content),
               [<<"ah-ribbon-tab-content">>, [<<"ah-ribbon-tab-content-active">> || K =:= Active]],
               [{id, sub_id(Id, <<"panel-", (idx(I))/binary>>)}, {role, tabpanel},
                {aria_labelledby, sub_id(Id, <<"tab-", (idx(I))/binary>>)},
                {data_index, I}, {data_key, K}])
         || {I, #{key := K, content := Content}} <- Indexed],
    {Back, Fwd} = case Vertical of
                      true -> {up, down};
                      false -> {left, right}
                  end,
    TabBar = ?H:el('div',
                   [scroll_btn(Back),
                    ?H:el('div',
                          [TabButtons,
                           ?H:el('div', [], [<<"ah-ribbon-selection-token">>],
                                 [{aria_hidden, <<"true">>}])],
                          [<<"ah-ribbon-tabs-inner">>],
                          [{role, tablist}, {aria_label, <<"Ribbon tabs">>},
                           {aria_orientation, case Vertical of
                                                  true -> vertical;
                                                  false -> horizontal
                                              end}]),
                    scroll_btn(Fwd),
                    [collapse_btn(R#ah_ribbon.mode =:= collapsed) || Collapsible]],
                   [<<"ah-ribbon-tabs">>], []),
    Style = [[<<"width:">>, css_size(W), $;] || W <- [R#ah_ribbon.width], W =/= undefined]
        ++ [[<<"height:">>, css_size(Hh), $;] || Hh <- [R#ah_ribbon.height], Hh =/= undefined],
    ?H:el('div',
          [hidden_input(Name, Active), TabBar,
           ?H:el('div', Panels, [<<"ah-ribbon-tabs-content">>], [])],
          [Classes, [<<"ah-ribbon-disabled">> || Disabled]],
          [[{id, Id},
            {style, case Style of [] -> undefined; _ -> iolist_to_binary(Style) end},
            {data_ah, <<"ribbon">>}, {data_ah_value, Active},
            {data_selection_mode, SelMode},
            {aria_disabled, Disabled andalso <<"true">>}],
           ?E:root_attrs(R, 'ah:command')]).

ribbon_tab(#{key := K, label := L} = M) ->
    Content = case M of
                  #{groups := Gs} -> {groups, Gs};
                  _ -> maps:get(content, M, [])
              end,
    #{key => text(K), label => L, icon => maps:get(icon, M, undefined),
      disabled => maps:get(disabled, M, false) =:= true, content => Content};
ribbon_tab({K, L, C}) ->
    ribbon_tab(#{key => K, label => L, content => C});
ribbon_tab({K, L, C, Opts}) ->
    O = flat(Opts),
    ribbon_tab(#{key => K, label => L, content => C,
                 icon => proplists:get_value(icon, O),
                 disabled => proplists:get_value(disabled, O, false)});
ribbon_tab(Other) -> error({aihtml, {bad_ribbon_tab, Other}}).

ribbon_content(PanelId, {groups, Groups}) when is_list(Groups) ->
    [ribbon_group_html(sub_id(PanelId, <<"g", (idx(I))/binary>>), ribbon_group(G))
     || {I, G} <- lists:zip(lists:seq(0, length(Groups) - 1), Groups)];
ribbon_content(_, Html) -> Html.

ribbon_group({Label, Items}) when is_list(Items) -> {Label, Items};
ribbon_group(#{label := Label, items := Items}) when is_list(Items) -> {Label, Items};
ribbon_group(Other) -> error({aihtml, {bad_ribbon_group, Other}}).

ribbon_group_html(GroupId, {Label, Items}) ->
    ?H:el('div',
          [?H:el('div', [ribbon_cmd(C) || C <- Items], [<<"ah-ribbon-group-content">>], []),
           ?H:el('div', Label, [<<"ah-ribbon-group-label">>], [{id, GroupId}])],
          [<<"ah-ribbon-group">>], [{role, group}, {aria_labelledby, GroupId}]).

ribbon_cmd(separator) ->
    ?H:el('div', [], [<<"ah-ribbon-separator">>],
          [{role, separator}, {aria_orientation, vertical}]);
ribbon_cmd({html, Html}) -> Html;
ribbon_cmd({stack, Cmds}) when is_list(Cmds) ->
    ?H:el('div', [ribbon_cmd(C) || C <- Cmds], [<<"ah-ribbon-stack">>], []);
ribbon_cmd({K, Icon, Label}) ->
    ribbon_cmd(#{key => K, icon => Icon, label => Label});
ribbon_cmd(#{key := K, label := Label} = M) ->
    Size = maps:get(size, M, small),
    check(size, Size, [small, large]),
    Icon = maps:get(icon, M, undefined),
    Menu = maps:get(items, M, undefined),
    Toggle = maps:get(toggle, M, false) =:= true,
    Pressed = Toggle andalso maps:get(pressed, M, false) =:= true,
    Off = maps:get(disabled, M, false) =:= true,
    Caret = [?H:el(span, <<"▾"/utf8>>, [<<"ah-ribbon-caret">>], [{aria_hidden, <<"true">>}])
             || Menu =/= undefined],
    Inner = case Size of
                large ->
                    [[?H:el(span, Icon, [<<"ah-ribbon-button-large-icon">>],
                            [{aria_hidden, <<"true">>}]) || Icon =/= undefined],
                     ?H:el(span, [Label, Caret], [<<"ah-ribbon-button-large-text">>], [])];
                small ->
                    [[?H:el(span, Icon, [<<"ah-ribbon-button-icon">>],
                            [{aria_hidden, <<"true">>}]) || Icon =/= undefined],
                     ?H:el(span, Label, [<<"ah-ribbon-button-text">>], []), Caret]
            end,
    Button = ?H:el(button, Inner,
                   [case Size of
                        large -> <<"ah-ribbon-button-large">>;
                        small -> <<"ah-ribbon-button">>
                    end,
                    [<<"ah-ribbon-button-pressed">> || Pressed],
                    [<<"ah-ribbon-dropdown-toggle">> || Menu =/= undefined]],
                   [{type, button},
                    {data_command, Menu =:= undefined andalso text(K)},
                    {data_menu, Menu =/= undefined andalso text(K)},
                    {data_toggle, Toggle},
                    {title, maps:get(title, M, undefined)},
                    {aria_pressed, Toggle andalso atom_to_binary(Pressed, utf8)},
                    {aria_haspopup, Menu =/= undefined andalso menu},
                    {aria_expanded, Menu =/= undefined andalso <<"false">>},
                    {disabled, Off}]),
    case Menu of
        undefined ->
            Button;
        Items when is_list(Items) ->
            ?H:el('div',
                  [Button,
                   ?H:el('div', [menu_item(I) || I <- Items],
                         [<<"ah-dropdown-btn-popup">>, <<"ah-ribbon-menu">>],
                         [{role, menu}, {hidden, true}])],
                  [<<"ah-ribbon-dropdown">>], []);
        Other ->
            error({aihtml, {bad_ribbon_command, Other}})
    end;
ribbon_cmd(Other) -> error({aihtml, {bad_ribbon_command, Other}}).

menu_item(divider) ->
    ?H:el('div', [], [<<"ah-dropdown-btn-divider">>], [{role, separator}]);
menu_item({K, L}) -> menu_item(#{key => K, label => L});
menu_item(#{key := K, label := L} = M) ->
    Icon = maps:get(icon, M, undefined),
    ?H:el(button,
          [[?H:el(span, Icon, [<<"ah-dropdown-btn-item-icon">>], [{aria_hidden, <<"true">>}])
            || Icon =/= undefined],
           ?H:el(span, L, [], [])],
          [<<"ah-dropdown-btn-item">>],
          [{type, button}, {role, menuitem}, {tabindex, <<"-1">>}, {data_command, text(K)},
           {disabled, maps:get(disabled, M, false) =:= true}]);
menu_item(Other) -> error({aihtml, {bad_ribbon_menu_item, Other}}).

scroll_btn(Dir) ->
    {Glyph, Label} = case Dir of
                         left -> {<<"◀"/utf8>>, <<"Scroll left">>};
                         right -> {<<"▶"/utf8>>, <<"Scroll right">>};
                         up -> {<<"▲"/utf8>>, <<"Scroll up">>};
                         down -> {<<"▼"/utf8>>, <<"Scroll down">>}
                     end,
    ?H:el(button, Glyph,
          [<<"ah-ribbon-scroll-btn">>, <<"ah-ribbon-scroll-", (atom_to_binary(Dir, utf8))/binary>>],
          [{type, button}, {data_scroll_direction, Dir}, {aria_label, Label},
           {tabindex, <<"-1">>}]).

collapse_btn(Collapsed) ->
    ?H:el(button,
          {safe, <<"<svg viewBox=\"0 0 16 16\" width=\"14\" height=\"14\" fill=\"none\" "
                   "stroke=\"currentColor\" stroke-width=\"1.8\" stroke-linecap=\"round\" "
                   "stroke-linejoin=\"round\" aria-hidden=\"true\"><path d=\"M4 10l4-4 4 4\"/>"
                   "</svg>">>},
          [<<"ah-ribbon-collapse-btn">>],
          [{type, button}, {aria_label, <<"Collapse the ribbon">>},
           {aria_expanded, atom_to_binary(not Collapsed, utf8)},
           {title, <<"Collapse the ribbon (Ctrl+F1)">>}]).

%%% tile layout --------------------------------------------------------

render_tile_layout(#ah_tile_layout{layout = Layout, value = Value, name = Name,
                                   splitbar_size = Bar, disabled = Disabled} = R0) ->
    {Id, R} = ensure_id(R0),
    Classes = classes(R),
    is_integer(Bar) andalso Bar > 0 orelse error({aihtml, {bad_option, splitbar_size, Bar}}),
    is_boolean(Disabled) orelse error({aihtml, {bad_option, disabled, Disabled}}),
    Tree0 = norm_node(Layout, []),
    _ = check_unique(Tree0),
    {Tree, Closed} = apply_value(Tree0, Value),
    Json = iolist_to_binary(json:encode(#{<<"root">> => node_value(Tree),
                                          <<"closed">> => Closed})),
    Style = [[<<"height:">>, css_size(Hh), $;]
             || Hh <- [R#ah_tile_layout.height], Hh =/= undefined],
    ?H:el('div',
          [hidden_input(Name, Json), render_node(Id, Bar, Tree)],
          [Classes, [<<"ah-tl-disabled">> || Disabled]],
          [[{id, Id},
            {style, case Style of [] -> undefined; _ -> iolist_to_binary(Style) end},
            {data_ah, <<"tile-layout">>}, {data_ah_value, Json},
            {data_splitbar_size, Bar},
            {aria_disabled, Disabled andalso <<"true">>}],
           ?E:root_attrs(R, change)]).

%% The layout tree as maps: #{kind := group | tabs | item, id, size, min,
%% resize, ...}. Groups and tab groups without an id get one from their
%% path ("n", "n0", "n0.1").
norm_node({Orient, Items}, Path) when Orient =:= columns; Orient =:= rows ->
    norm_node(#{Orient => Items}, Path);
norm_node({tabs, Tabs}, Path) ->
    norm_node(#{tabs => Tabs}, Path);
norm_node(#{} = M, Path) ->
    Common = #{id => case maps:find(id, M) of
                         {ok, I} -> text(I);
                         error -> path_id(Path)
                     end,
               size => size_text(maps:get(size, M, undefined)),
               min => min_px(maps:get(min, M, undefined)),
               resize => bool_opt(resize, maps:get(resize, M, true))},
    case M of
        #{columns := Items} when is_list(Items) -> group(Common, columns, Items, Path);
        #{rows := Items} when is_list(Items) -> group(Common, rows, Items, Path);
        #{tabs := Tabs0} when is_list(Tabs0) ->
            Tabs = [norm_tab(T) || T <- Tabs0],
            Pos = maps:get(position, M, top),
            check(position, Pos, [top, bottom, left, right]),
            Common#{kind => tabs, tabs => Tabs, position => Pos,
                    active => active_tab(value_text(maps:get(active, M, undefined)), Tabs)};
        #{id := _, content := Content} ->
            Common#{kind => item, content => Content,
                    label => text(maps:get(label, M, maps:get(id, Common)))};
        _ ->
            error({aihtml, {bad_tile_layout_node, M}})
    end;
norm_node(Other, _) -> error({aihtml, {bad_tile_layout_node, Other}}).

group(Common, Orient, Items, Path) ->
    Common#{kind => group, orient => Orient,
            items => [norm_node(N, Path ++ [I])
                      || {I, N} <- lists:zip(lists:seq(0, length(Items) - 1), Items)]}.

norm_tab({Id, Label, Content}) ->
    norm_tab(#{id => Id, label => Label, content => Content});
norm_tab(#{id := Id, label := Label} = M) ->
    #{id => text(Id), label => text(Label), content => maps:get(content, M, []),
      close => bool_opt(close, maps:get(close, M, true)),
      drag => bool_opt(drag, maps:get(drag, M, true))};
norm_tab(Other) -> error({aihtml, {bad_tile_layout_tab, Other}}).

path_id(Path) ->
    iolist_to_binary([$n, lists:join($., [integer_to_binary(I) || I <- Path])]).

active_tab(Want, Tabs) ->
    Ids = [I || #{id := I} <- Tabs],
    case lists:member(Want, Ids) of
        true -> Want;
        false -> case Ids of [F | _] -> F; [] -> <<>> end
    end.

%% Every id names one node: the browser and a stored value find nodes by id.
check_unique(Tree) ->
    lists:foldl(fun(I, Seen) ->
                        sets:is_element(I, Seen)
                            andalso error({aihtml, {duplicate_tile_id, I}}),
                        sets:add_element(I, Seen)
                end, sets:new([{version, 2}]), all_ids(Tree)).

all_ids(#{kind := group, id := Id, items := Items}) -> [Id | lists:append([all_ids(N) || N <- Items])];
all_ids(#{kind := tabs, id := Id, tabs := Tabs}) -> [Id | [T || #{id := T} <- Tabs]];
all_ids(#{kind := item, id := Id}) -> [Id].

render_node(RootId, Bar, #{kind := group, orient := Orient, items := Items} = N) ->
    Vert = Orient =:= columns,
    Splitbar = ?H:el('div', [],
                     [<<"ah-tl-splitbar">>,
                      case Vert of
                          true -> <<"ah-tl-splitbar-v">>;
                          false -> <<"ah-tl-splitbar-h">>
                      end],
                     [{role, separator},
                      {aria_orientation, case Vert of true -> vertical; false -> horizontal end},
                      {aria_label, <<"Resize">>}, {tabindex, <<"0">>}]),
    Prop = case Vert of
               true -> <<"grid-template-columns:">>;
               false -> <<"grid-template-rows:">>
           end,
    ?H:el('div', lists:join(Splitbar, [render_node(RootId, Bar, C) || C <- Items]),
          [<<"ah-tl-group">>, case Vert of
                                  true -> <<"ah-tl-vertical">>;
                                  false -> <<"ah-tl-horizontal">>
                              end],
          node_attrs(N, <<"layout-group">>)
          ++ [{data_orientation, case Vert of true -> vertical; false -> horizontal end},
              {style, iolist_to_binary([Prop, template(Items, Bar)])}]);
render_node(RootId, _Bar, #{kind := tabs, tabs := Tabs, active := Active, position := Pos} = N) ->
    Side = Pos =:= left orelse Pos =:= right,
    Strip = ?H:el('div', [tab_html(RootId, T, Active) || T <- Tabs], [<<"ah-tl-tab-strip">>],
                  [{role, tablist},
                   {aria_orientation, case Side of true -> vertical; false -> horizontal end}]),
    Panels = [?H:el('div', Content,
                    [<<"ah-tl-tab-content">>, [<<"ah-tl-tab-content-active">> || T =:= Active]],
                    [{id, dom_id(RootId, <<"panel">>, T)}, {data_id, T}, {role, tabpanel},
                     {aria_labelledby, dom_id(RootId, <<"tab">>, T)}])
              || #{id := T, content := Content} <- Tabs],
    ?H:el('div', [Strip | Panels],
          [<<"ah-tl-tab-group">>,
           [<<"ah-tl-tab-group-", (atom_to_binary(Pos, utf8))/binary>> || Pos =/= top]],
          node_attrs(N, <<"tab-group">>));
render_node(_RootId, _Bar, #{kind := item, content := Content, label := Label} = N) ->
    ?H:el('div', Content, [<<"ah-tl-item">>],
          node_attrs(N, <<"layout-item">>) ++ [{data_label, Label}]).

node_attrs(#{id := Id, size := Size, min := Min, resize := Resize}, Type) ->
    [{data_id, Id}, {data_type, Type}, {data_size, Size}, {data_min, Min},
     {data_resize, Resize =:= false andalso <<"false">>}].

tab_html(RootId, #{id := T, label := Label, close := Close, drag := Drag}, Active) ->
    Sel = T =:= Active,
    aihtml_tpl:safe(tpl_tile_layout_tab(
                      #{selected => Sel, dom_id => dom_id(RootId, <<"tab">>, T), id => T,
                        modifiers => iolist_to_binary(
                                       lists:join($,, [<<"drag">> || Drag] ++ [<<"close">> || Close])),
                        panel_id => dom_id(RootId, <<"panel">>, T),
                        aria_selected => atom_to_binary(Sel, utf8),
                        tabindex => case Sel of true -> <<"0">>; false -> <<"-1">> end,
                        label => Label, close => Close})).

dom_id(RootId, Part, TabId) -> <<RootId/binary, "-", Part/binary, "-", TabId/binary>>.

%% The grid tracks of a group's children with the splitbars between them.
%% As in sigil, percentages that add up to more than 100% all become 1fr.
template(Items, Bar) ->
    Sizes = [S || #{size := S} <- Items],
    Pct = lists:sum([pct(S) || S <- Sizes]),
    Tracks = [case S of
                  undefined -> <<"1fr">>;
                  _ when Pct > 100 -> <<"1fr">>;
                  _ -> S
              end || S <- Sizes],
    lists:join([$\s, integer_to_binary(Bar), <<"px ">>], Tracks).

pct(undefined) -> 0;
pct(S) ->
    case re:run(S, <<"^([0-9.]+)%$">>, [{capture, all_but_first, binary}]) of
        {match, [N]} -> num(N);
        nomatch -> 0
    end.

num(B) ->
    try binary_to_float(B)
    catch error:badarg ->
            try binary_to_integer(B) catch error:badarg -> 0 end
    end.

%%% the arrangement ----------------------------------------------------

node_value(#{kind := group, id := Id, orient := O, items := Items} = N) ->
    with_size(N, #{<<"type">> => atom_to_binary(O, utf8), <<"id">> => Id,
                   <<"items">> => [node_value(C) || C <- Items]});
node_value(#{kind := tabs, id := Id, tabs := Tabs, active := A} = N) ->
    with_size(N, #{<<"type">> => <<"tabs">>, <<"id">> => Id,
                   <<"tabs">> => [T || #{id := T} <- Tabs], <<"active">> => A});
node_value(#{kind := item, id := Id} = N) ->
    with_size(N, #{<<"type">> => <<"item">>, <<"id">> => Id}).

with_size(#{size := undefined}, M) -> M;
with_size(#{size := S}, M) -> M#{<<"size">> => S}.

apply_value(Tree, undefined) -> {Tree, []};
apply_value(Tree, Value) ->
    case decode(Value) of
        #{<<"root">> := Root} = V ->
            Known = index(Tree),
            Closed = [C || C <- lists:usort(listv(maps:get(<<"closed">>, V, []))),
                           is_binary(C), maps:is_key(C, Known)],
            case rebuild(Root, Known, sets:new([{version, 2}])) of
                {none, _} ->
                    {Tree, []};
                {New, Placed} ->
                    Missing = [L || {leaf, LId, _} = L <- leaves(Tree),
                                    not sets:is_element(LId, Placed),
                                    not lists:member(LId, Closed)],
                    {lists:foldl(fun add_missing/2, New, Missing), Closed}
            end;
        _ ->
            {Tree, []}
    end.

decode(M) when is_map(M) -> M;
decode(V) ->
    try json:decode(iolist_to_binary(V))
    catch _:_ -> invalid
    end.

listv(L) when is_list(L) -> L;
listv(_) -> [].

%% Id => the node (groups and tab groups without their children) or
%% {tab, Tab, GroupId}.
index(Tree) ->
    maps:from_list(index(Tree, undefined)).

index(#{kind := group, id := Id, items := Items} = N, _) ->
    [{Id, N#{items := []}} | lists:append([index(C, Id) || C <- Items])];
index(#{kind := tabs, id := Id, tabs := Tabs} = N, _) ->
    [{Id, N#{tabs := []}} | [{T, {tab, Tab, Id}} || #{id := T} = Tab <- Tabs]];
index(#{kind := item, id := Id} = N, _) ->
    [{Id, N}].

%% The tiles and tabs of the layout, in order, with where they were.
leaves(#{kind := group, items := Items}) -> lists:append([leaves(C) || C <- Items]);
leaves(#{kind := tabs, id := G, tabs := Tabs}) -> [{leaf, T, {tab, Tab, G}} || #{id := T} = Tab <- Tabs];
leaves(#{kind := item, id := Id} = N) -> [{leaf, Id, N}].

%% A stored node, rebuilt from the layout's own nodes. Returns
%% {Node | none, PlacedLeafIds}.
rebuild(#{<<"type">> := T, <<"items">> := Items} = J, Known, Placed0)
  when (T =:= <<"columns">> orelse T =:= <<"rows">>), is_list(Items) ->
    {Children, Placed} =
        lists:foldl(fun(C, {Acc, P0}) ->
                            case rebuild(C, Known, P0) of
                                {none, P} -> {Acc, P};
                                {Node, P} -> {[Node | Acc], P}
                            end
                    end, {[], Placed0}, Items),
    Size = stored_size(J),
    case lists:reverse(Children) of
        [] -> {none, Placed};
        [One] -> {case Size of undefined -> One; _ -> One#{size := Size} end, Placed};
        Many ->
            Id = stored_id(J),
            Base = case maps:find(Id, Known) of
                       {ok, #{kind := group} = G} -> G;
                       _ -> #{kind => group, id => Id, min => undefined, resize => true}
                   end,
            {Base#{orient => binary_to_atom(T, utf8), size => Size, items => Many}, Placed}
    end;
rebuild(#{<<"type">> := <<"tabs">>, <<"tabs">> := Ids} = J, Known, Placed0) when is_list(Ids) ->
    {Tabs, Placed} =
        lists:foldl(fun(TId, {Acc, P}) ->
                            case is_binary(TId) andalso not sets:is_element(TId, P)
                                andalso maps:find(TId, Known) of
                                {ok, {tab, Tab, _}} -> {[Tab | Acc], sets:add_element(TId, P)};
                                {ok, #{kind := item, label := L, content := C}} ->
                                    {[#{id => TId, label => L, content => C, close => true,
                                        drag => true} | Acc], sets:add_element(TId, P)};
                                _ -> {Acc, P}
                            end
                    end, {[], Placed0}, Ids),
    case lists:reverse(Tabs) of
        [] -> {none, Placed};
        Ts ->
            Id = stored_id(J),
            Base = case maps:find(Id, Known) of
                       {ok, #{kind := tabs} = G} -> G;
                       _ -> #{kind => tabs, id => Id, min => undefined, resize => true,
                              position => top}
                   end,
            Want = case maps:get(<<"active">>, J, undefined) of
                       A when is_binary(A) -> A;
                       _ -> <<>>
                   end,
            {Base#{size => stored_size(J), tabs => Ts, active => active_tab(Want, Ts)}, Placed}
    end;
rebuild(#{<<"type">> := <<"item">>, <<"id">> := Id} = J, Known, Placed) when is_binary(Id) ->
    case not sets:is_element(Id, Placed) andalso maps:find(Id, Known) of
        {ok, #{kind := item} = N} -> {N#{size := stored_size(J)}, sets:add_element(Id, Placed)};
        _ -> {none, Placed}
    end;
rebuild(_, _, Placed) ->
    {none, Placed}.

stored_id(#{<<"id">> := Id}) when is_binary(Id), Id =/= <<>> -> Id;
stored_id(_) -> <<"tl-", (integer_to_binary(erlang:unique_integer([positive])))/binary>>.

%% Sizes come back from the browser: only plain tracks go into the style.
stored_size(#{<<"size">> := S}) when is_binary(S) ->
    case re:run(S, <<"^[0-9]+(\\.[0-9]+)?(px|%|fr)$">>) of
        {match, _} -> S;
        nomatch -> undefined
    end;
stored_size(_) -> undefined.

%% A tab the stored value does not know joins the group it was written
%% in, else the first tab group, else a new group; a tile joins the root.
add_missing({leaf, _, {tab, Tab, G}}, Tree) ->
    case add_tab(Tree, G, Tab) of
        {true, T} -> T;
        false ->
            case add_tab(Tree, first_tabs(Tree), Tab) of
                {true, T} -> T;
                false -> append_root(Tree, #{kind => tabs, id => G, size => undefined,
                                             min => undefined, resize => true, position => top,
                                             tabs => [Tab], active => maps:get(id, Tab)})
            end
    end;
add_missing({leaf, _, Item}, Tree) ->
    append_root(Tree, Item).

add_tab(#{kind := tabs, id := G, tabs := Tabs} = N, G, Tab) ->
    {true, N#{tabs := Tabs ++ [Tab]}};
add_tab(#{kind := group, items := Items} = N, G, Tab) ->
    {Found, New} = lists:mapfoldl(fun(C, true) -> {C, true};
                                     (C, false) ->
                                          case add_tab(C, G, Tab) of
                                              {true, C2} -> {C2, true};
                                              false -> {C, false}
                                          end
                                  end, false, Items),
    case New of
        true -> {true, N#{items := Found}};
        false -> false
    end;
add_tab(_, _, _) -> false.

first_tabs(#{kind := tabs, id := Id}) -> Id;
first_tabs(#{kind := group, items := Items}) ->
    case [I || C <- Items, I <- [first_tabs(C)], I =/= undefined] of
        [I | _] -> I;
        [] -> undefined
    end;
first_tabs(_) -> undefined.

append_root(#{kind := group, items := Items} = Root, N) -> Root#{items := Items ++ [N]};
append_root(Root, N) ->
    #{kind => group, id => <<"n">>, orient => columns, size => undefined, min => undefined,
      resize => true, items => [Root#{size := undefined}, N]}.

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => ribbon, category => layout,
       signature => <<"ribbon(Tabs, Value, Css, Attrs)">>,
       root => <<"ah-ribbon">>,
       groups => #{position => {[top, bottom, left, right], top},
                   mode => {[default, collapsed, popup], default},
                   color => {[primary, success, warning, danger], none},
                   animation => {[slide, fade], none}},
       flags => [collapsible],
       classes => #{top => [<<"ah-ribbon-position-top">>],
                    bottom => [<<"ah-ribbon-position-bottom">>],
                    left => [<<"ah-ribbon-position-left">>],
                    right => [<<"ah-ribbon-position-right">>],
                    default => [<<"ah-ribbon-mode-default">>],
                    collapsed => [<<"ah-ribbon-mode-collapsed">>],
                    popup => [<<"ah-ribbon-mode-popup">>],
                    slide => [<<"ah-ribbon-animation-slide">>],
                    fade => [<<"ah-ribbon-animation-fade">>]},
       options => [selection_mode, width, height],
       behavior => <<"ribbon">>,
       events => [<<"ah:command">>, <<"change">>, <<"ah:collapse">>, <<"ah:expand">>],
       doc => <<"An Office-style ribbon: tabs over groups of large and small buttons, toggles "
                "and dropdown menus. The value is the active tab; commands fire ah:command "
                "with Event.data.command.">>,
       option_docs =>
           #{collapsible => <<"Show a collapse button; it, a double click on a tab or Ctrl+F1 "
                              "switches between the default and the collapsed mode.">>,
             selection_mode => <<"click (default) or hover: what switches tabs.">>,
             width => <<"Width: px as an integer or a CSS length.">>,
             height => <<"Height: px as an integer or a CSS length.">>},
       methods => [#{name => select, args => <<"(Key)">>,
                     doc => <<"Make a tab active without firing change.">>},
                   #{name => getValue, args => <<"()">>, doc => <<"Return the active tab's key.">>},
                   #{name => enableTab, args => <<"(Key)">>, doc => <<"Enable a tab.">>},
                   #{name => disableTab, args => <<"(Key)">>, doc => <<"Disable a tab.">>},
                   #{name => enableCommand, args => <<"(Command)">>,
                     doc => <<"Enable the buttons and menu items of a command.">>},
                   #{name => disableCommand, args => <<"(Command)">>,
                     doc => <<"Disable the buttons and menu items of a command.">>},
                   #{name => setPressed, args => <<"(Command, Pressed)">>,
                     doc => <<"Press or release a toggle command without firing ah:command.">>},
                   #{name => collapse, args => <<"()">>, doc => <<"Switch to the collapsed mode.">>},
                   #{name => expand, args => <<"()">>, doc => <<"Switch back to the default mode.">>},
                   #{name => close, args => <<"()">>,
                     doc => <<"Hide the panel of a collapsed or popup ribbon.">>}]},
     #{name => tile_layout, category => layout,
       signature => <<"tile_layout(Layout, Value, Css, Attrs)">>,
       root => <<"ah-tl">>,
       options => [splitbar_size, height],
       behavior => <<"tile-layout">>,
       events => [<<"change">>, <<"ah:tab-select">>, <<"ah:tab-close">>],
       doc => <<"Resizable panes and tab groups (an IDE-style layout): drag splitbars to "
                "resize, drag tabs between groups or to a pane's edge to split it. The value "
                "is the arrangement as JSON; pass it back to restore it.">>,
       option_docs =>
           #{splitbar_size => <<"Grid gap for the splitbars in px (default 4).">>,
             height => <<"Height: px as an integer or a CSS length (the layout fills its "
                         "parent by default).">>},
       methods => [#{name => getValue, args => <<"()">>,
                     doc => <<"Return the arrangement (an object; data-ah-value holds its JSON).">>},
                   #{name => select, args => <<"(TabId)">>,
                     doc => <<"Show a tab in its group without firing change.">>},
                   #{name => close, args => <<"(TabId)">>,
                     doc => <<"Close a tab (fires change).">>}]}].

%%%===================================================================
%%% Internal
%%%===================================================================

classes(R) ->
    Tag = element(1, R),
    ?E:classes(R, fields(Tag), entry(?E:component_name(Tag))).

entry(Name) -> aihtml_catalog:entry(?MODULE, Name).

check(Field, V, Allowed) ->
    lists:member(V, Allowed) orelse error({aihtml, {bad_option, Field, V}}).

bool_opt(_, B) when is_boolean(B) -> B;
bool_opt(F, V) -> error({aihtml, {bad_option, F, V}}).

min_px(undefined) -> undefined;
min_px(N) when is_integer(N), N >= 0 -> N;
min_px(V) -> error({aihtml, {bad_option, min, V}}).

size_text(undefined) -> undefined;
size_text(N) when is_integer(N) -> <<(integer_to_binary(N))/binary, "px">>;
size_text(S) when is_binary(S); is_list(S) -> iolist_to_binary(S);
size_text(V) -> error({aihtml, {bad_option, size, V}}).

%% The parts refer to each other by id, so a root without one gets one.
ensure_id(R) ->
    Id = case element(3, R) of
             undefined -> <<"ah-t", (integer_to_binary(erlang:unique_integer([positive])))/binary>>;
             Id0 -> text(Id0)
         end,
    {Id, setelement(3, R, Id)}.

sub_id(Id, Part) -> <<Id/binary, "-", Part/binary>>.

idx(I) -> integer_to_binary(I).

flat(M) when is_map(M) -> maps:to_list(M);
flat(L) when is_list(L) -> lists:flatten(L).

css_size(N) when is_integer(N) -> [integer_to_binary(N), <<"px">>];
css_size(S) -> S.

hidden_input(undefined, _) -> [];
hidden_input(Name, Value) ->
    ?H:void(input, [], [{type, hidden}, {name, Name}, {value, Value}]).

value_text(undefined) -> <<>>;
value_text(V) -> text(V).

text(undefined) -> <<>>;
text(B) when is_binary(B) -> B;
text(A) when is_atom(A) -> atom_to_binary(A, utf8);
text(I) when is_integer(I) -> integer_to_binary(I);
text(F) when is_float(F) -> float_to_binary(F, [short]);
text(L) when is_list(L) ->
    case unicode:characters_to_binary(L) of
        B when is_binary(B) -> B;
        _ -> error({aihtml, {bad_value, L}})
    end;
text(Other) -> error({aihtml, {bad_value, Other}}).
