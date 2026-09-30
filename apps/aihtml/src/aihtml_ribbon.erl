%%%-------------------------------------------------------------------
%%% @doc An Office-style ribbon, ported from sigil (layout/ribbon). DOM and
%%% class names are the ones sigil renders, so the styles in
%%% priv/css/sigil apply unchanged.
%%%
%%%   ah_ribbon(Tabs, Value, Css, Attrs)       an Office-style ribbon
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
%%% ah_ribbon/4 builds an #ah_ribbon{} (include/aihtml_ribbon.hrl) and
%%% render/1 turns it into HTML (designs/05-records.md). Behaviour:
%%% assets/js/components/ribbon.ts.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_ribbon).
-behaviour(aihtml_element).

-include("aihtml_ribbon.hrl").

-export([ah_ribbon/4, render/1, fields/1, catalog/0]).

-export_type([menu_item/0, cmd/0, group/0, content/0, tab/0, position/0, mode/0,
              color/0, animation/0]).

%% the last clauses reject items outside the declared types at run time
-dialyzer({no_match, [ribbon_tab/1, ribbon_group/1, ribbon_cmd/1, menu_item/1]}).

-define(H, aihtml_html).
-define(E, aihtml_element).
-define(L, aihtml_lib_tiles).

%% A dropdown menu entry: {Key, Label} | #{key, label, icon, disabled} | divider.
-type menu_item() :: {term(), aihtml_html:html()}
                   | #{key := term(), label := aihtml_html:html(),
                       icon => aihtml_html:html(), disabled => boolean()}
                   | divider.
%% A command in a ribbon group:
%%   {Key, Icon, Label}                        a small button
%%   #{key, label, icon, size => small | large, title, disabled,
%%     toggle, pressed, items => [menu_item()]}
%%                                             (items: a dropdown; toggle:
%%                                             a pressed / released button)
%%   {stack, [Command]}                        small buttons stacked in a column
%%   separator                                 a vertical rule
%%   {html, Html}                              any markup (a select, ...)
-type cmd() :: {term(), aihtml_html:html(), aihtml_html:html()}
             | #{key := term(), label := aihtml_html:html(),
                 icon => aihtml_html:html(), size => small | large,
                 title => iodata(), disabled => boolean(),
                 toggle => boolean(), pressed => boolean(),
                 items => [menu_item()]}
             | {stack, [cmd()]}
             | separator
             | {html, aihtml_html:html()}.
%% A labelled group of commands: {Label, [Command]} | #{label, items}.
-type group() :: {aihtml_html:html(), [cmd()]}
               | #{label := aihtml_html:html(), items := [cmd()]}.
%% A tab's panel: any HTML, or {groups, [Group]} for Office-style groups.
-type content() :: aihtml_html:html() | {groups, [group()]}.
%% {Key, Label, Content} | {Key, Label, Content, Opts} (Opts: icon, disabled)
%% | #{key, label, icon, disabled, content, groups}.
-type tab() :: {term(), aihtml_html:html(), content()}
             | {term(), aihtml_html:html(), content(), aihtml_html:attrs()}
             | #{key := term(), label := aihtml_html:html(),
                 icon => aihtml_html:html(), disabled => boolean(),
                 content => aihtml_html:html(), groups => [group()]}.
-type position() :: top | bottom | left | right.
-type mode() :: default | collapsed | popup.
-type color() :: primary | success | warning | danger.
-type animation() :: slide | fade.

%%%===================================================================
%%% Builder
%%%===================================================================

%% @doc An Office-style ribbon. `Tabs' are `{Key, Label, Content}',
%% `{Key, Label, Content, Opts}' (Opts: `icon', `disabled') or maps
%% `#{key, label, icon, disabled, content, groups}'; `Content' is HTML or
%% `{groups, [{Label, Commands}]}'. `Value' is the active tab's key (the
%% first enabled tab when undefined). Css: `top' (default), `bottom',
%% `left', `right'; `default', `collapsed', `popup'; a colour `primary',
%% `success', `warning', `danger'; `slide' or `fade'; `collapsible'.
-spec ah_ribbon([tab()], term(), aihtml_html:css(), aihtml_html:attrs()) -> #ah_ribbon{}.
ah_ribbon(Tabs, Value, Css, Attrs) ->
    ?E:build(?MODULE, #ah_ribbon{items = Tabs, value = Value}, Css, Attrs).

%% @doc The field names of #ah_ribbon{}.
-spec fields(atom()) -> [atom()].
fields(ah_ribbon) -> record_info(fields, ah_ribbon).

%%%===================================================================
%%% Rendering
%%%===================================================================

-spec render(#ah_ribbon{}) -> aihtml_html:html().
render(#ah_ribbon{} = R) -> render_ribbon(R).

render_ribbon(#ah_ribbon{items = Tabs0, value = Value, name = Name,
                         disabled = Disabled} = R0) ->
    {Id, R} = ?L:ensure_id(R0),
    Classes = ?E:classes(?MODULE, R),                       % checks the modifier fields
    #ah_ribbon{position = Pos, selection_mode = SelMode, collapsible = Collapsible} = R,
    ?L:check(selection_mode, SelMode, [click, hover]),
    is_boolean(Disabled) orelse error({aihtml, {bad_option, disabled, Disabled}}),
    Tabs = [ribbon_tab(T) || T <- Tabs0],
    Cur = ?L:value_text(Value),
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
                          [{role, tablist}, {aria_label, aihtml_i18n:text(ribbon, tabs)},
                           {aria_orientation, case Vertical of
                                                  true -> vertical;
                                                  false -> horizontal
                                              end}]),
                    scroll_btn(Fwd),
                    [collapse_btn(R#ah_ribbon.mode =:= collapsed) || Collapsible]],
                   [<<"ah-ribbon-tabs">>], []),
    Style = [[<<"width:">>, ?L:css_size(W), $;] || W <- [R#ah_ribbon.width], W =/= undefined]
        ++ [[<<"height:">>, ?L:css_size(Hh), $;] || Hh <- [R#ah_ribbon.height], Hh =/= undefined],
    ?H:el('div',
          [?L:hidden_input(Name, Active), TabBar,
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
    #{key => ?L:text(K), label => L, icon => maps:get(icon, M, undefined),
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
    ?L:check(size, Size, [small, large]),
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
                    {data_command, Menu =:= undefined andalso ?L:text(K)},
                    {data_menu, Menu =/= undefined andalso ?L:text(K)},
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
          [{type, button}, {role, menuitem}, {tabindex, <<"-1">>}, {data_command, ?L:text(K)},
           {disabled, maps:get(disabled, M, false) =:= true}]);
menu_item(Other) -> error({aihtml, {bad_ribbon_menu_item, Other}}).

scroll_btn(Dir) ->
    {Glyph, Label} = case Dir of
                         left -> {<<"◀"/utf8>>, aihtml_i18n:text(ribbon, scroll_left)};
                         right -> {<<"▶"/utf8>>, aihtml_i18n:text(ribbon, scroll_right)};
                         up -> {<<"▲"/utf8>>, aihtml_i18n:text(ribbon, scroll_up)};
                         down -> {<<"▼"/utf8>>, aihtml_i18n:text(ribbon, scroll_down)}
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
          [{type, button}, {aria_label, aihtml_i18n:text(ribbon, collapse)},
           {aria_expanded, atom_to_binary(not Collapsed, utf8)},
           {title, aihtml_i18n:text(ribbon, collapse_title)}]).

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => ribbon, category => layout,
       signature => <<"ah_ribbon(Tabs, Value, Css, Attrs)">>,
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
                     doc => <<"Hide the panel of a collapsed or popup ribbon.">>}]}].

%%%===================================================================
%%% Internal
%%%===================================================================

sub_id(Id, Part) -> <<Id/binary, "-", Part/binary>>.

idx(I) -> integer_to_binary(I).

flat(M) when is_map(M) -> maps:to_list(M);
flat(L) when is_list(L) -> lists:flatten(L).
