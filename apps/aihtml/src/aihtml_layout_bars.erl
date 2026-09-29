%%%-------------------------------------------------------------------
%%% @doc Bars and palettes, ported from sigil (layout/activity_bar,
%%% layout/navigationbar, overlay/command). DOM and class names are the
%%% ones sigil renders, so the styles in priv/css/sigil apply unchanged.
%%%
%%%   activity_bar(Items, Value, Css, Attrs)   a VS Code-style icon rail
%%%   navigationbar(Items, Value, Css, Attrs)  collapsible sections (accordion)
%%%   command(Items, Css, Attrs)               a command palette (⌘K)
%%%   set_command_items(Ctx, Target, Items)    (in an action) replace a
%%%                                            command palette's list
%%%
%%% activity_bar and navigationbar are value-bearing: the value is in
%%% `data-ah-value' on the root (the active item; the expanded indexes
%%% "0,2"), a `name' renders a hidden input, and user changes fire
%%% `change' on the root. command fires `ah:select' with the chosen
%%% command's value in `data-ah-value'.
%%%
%%% == Command filtering ==
%%%
%%% By default every command is rendered here and the behaviour hides the
%%% ones that do not match what the user types (a case-insensitive
%%% substring of the value, label or description, as in sigil). With
%%% `{search, {Mod, Action, Args}}' the text field instead posts the
%%% action (debounced) with `Event.value' = the query and `Event.data' =
%%% `#{<<"command">> => RootId}'; the action answers with
%%% `set_command_items(Ctx, Event, Items)', which renders the list here
%%% and morphs it into the palette. The browser builds no HTML.
%%%
%%% Each function builds an element record (#ah_activity_bar{},
%%% #ah_navigationbar{}, #ah_command{}, include/aihtml_layout_bars.hrl)
%%% and render/1 turns it into HTML (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_layout_bars).
-behaviour(aihtml_element).

-include("aihtml_layout_bars.hrl").

-export([activity_bar/4, navigationbar/4, command/3, set_command_items/3,
         render/1, fields/1, catalog/0, facade_extras/0]).

-export_type([element/0]).

%% the last clauses reject items outside the declared types at run time
-dialyzer({no_match, [activity_item/1, nav_item/1]}).

-define(H, aihtml_html).
-define(E, aihtml_element).

-type html() :: aihtml_html:html().
-type css() :: aihtml_html:css().
-type attrs() :: aihtml_html:attrs().
-type element() :: #ah_activity_bar{} | #ah_navigationbar{} | #ah_command{}.

%%%===================================================================
%%% Builders
%%%===================================================================

%% @doc A vertical rail of icon buttons (role tablist). `Items' are
%% `{Value, Icon, Label}', `{Value, Icon, Label, ItemAttrs}' or `divider';
%% `Icon' is HTML, `Label' the tooltip and aria-label, `ItemAttrs' HTML
%% attributes of the button (`{disabled, true}' disables it). `Value' is
%% the active item. Css: `left' (default) or `right' places the active
%% marker on that edge.
-spec activity_bar([ah_bars_activity_item()], term(), css(), attrs()) -> #ah_activity_bar{}.
activity_bar(Items, Value, Css, Attrs) ->
    build(#ah_activity_bar{items = Items, value = Value}, Css, Attrs).

%% @doc Collapsible sections with a clickable header each. `Items' are
%% `{Header, Content}', `{Header, Content, Opts}' (Opts: `disabled',
%% `actions') or maps `#{header, content, actions, disabled}'; a header
%% may be `#{title, subheader, extra}'. `Value' holds the expanded
%% indexes, 0-based: `N', `[N]', `<<"0,2">>' or `undefined'.
-spec navigationbar([ah_bars_nav_item()], ah_bars_nav_value(), css(), attrs()) ->
          #ah_navigationbar{}.
navigationbar(Items, Value, Css, Attrs) ->
    build(#ah_navigationbar{items = Items, value = Value}, Css, Attrs).

%% @doc A command palette: a search field over grouped commands. `Items'
%% are commands (`Label', `{Value, Label}' or `#{value, label,
%% description, icon, shortcut, href, disabled}') and groups
%% `#{heading, items}'. Css `palette' renders it in a hidden centred
%% overlay, opened with the `open' method or the `hotkey' option.
-spec command([ah_bars_cmd_entry()], css(), attrs()) -> #ah_command{}.
command(Items, Css, Attrs) ->
    build(#ah_command{items = Items}, Css, Attrs).

build(R, Css, Attrs) ->
    Tag = element(1, R),
    ?E:build(R, fields(Tag), entry(?E:component_name(Tag)), Css, Attrs).

%% @doc The field names of one of this group's records.
-spec fields(atom()) -> [atom()].
fields(ah_activity_bar) -> record_info(fields, ah_activity_bar);
fields(ah_navigationbar) -> record_info(fields, ah_navigationbar);
fields(ah_command) -> record_info(fields, ah_command).

%% @doc Functions besides the components that the aihtml facade re-exports.
-spec facade_extras() -> [{atom(), arity()}].
facade_extras() -> [{set_command_items, 3}].

%%%===================================================================
%%% Rendering
%%%===================================================================

-spec render(element()) -> html().
render(#ah_activity_bar{} = R) -> render_activity_bar(R);
render(#ah_navigationbar{} = R) -> render_navigationbar(R);
render(#ah_command{} = R) -> render_command(R).

%%% activity_bar -------------------------------------------------------

render_activity_bar(#ah_activity_bar{items = Items0, value = Value, name = Name,
                                     placement = Placement} = R) ->
    Classes = classes(R),                       % checks placement
    Cur = value_text(Value),
    Items = [activity_item(I) || I <- Items0],
    Enabled = [V || {V, _, _, IA} <- Items, not is_disabled(IA)],
    Focus = case lists:member(Cur, Enabled) of
                true -> Cur;
                false -> case Enabled of [F | _] -> F; [] -> undefined end
            end,
    Children =
        [case I of
             divider ->
                 ?H:el('div', [], [<<"ah-activity-bar__divider">>],
                       [{role, presentation}, {data_index, Idx}]);
             {V, Icon, Label, IA} ->
                 Active = V =:= Cur,
                 Off = is_disabled(IA),
                 ?H:el(button, ?H:el(span, Icon, [<<"ah-activity-bar__icon">>], []),
                       [<<"ah-activity-bar__item">>],
                       [[{type, button}, {role, tab}, {data_id, V},
                         {data_active, atom_to_binary(Active, utf8)},
                         {data_disabled, atom_to_binary(Off, utf8)},
                         {aria_selected, atom_to_binary(Active, utf8)},
                         {aria_label, Label}, {title, Label},
                         {tabindex, case V =:= Focus of true -> <<"0">>; false -> <<"-1">> end},
                         {disabled, Off}],
                        IA])
         end || {Idx, I} <- lists:zip(lists:seq(0, length(Items) - 1), Items)],
    ?H:el('div', [hidden_input(Name, Cur) | Children], Classes,
          [[{role, tablist}, {aria_orientation, vertical},
            {data_placement, Placement},
            {data_ah, <<"activity-bar">>}, {data_ah_value, Cur}],
           ?E:root_attrs(R, change)]).

activity_item(divider) -> divider;
activity_item({V, Icon, Label}) -> {text(V), Icon, text(Label), []};
activity_item({V, Icon, Label, IA}) -> {text(V), Icon, text(Label), IA};
activity_item(Other) -> error({aihtml, {bad_activity_bar_item, Other}}).

%%% navigationbar ------------------------------------------------------

render_navigationbar(#ah_navigationbar{items = Items0, value = Value, name = Name,
                                       disabled = Disabled} = R0) ->
    {Id, R} = ensure_id(R0),
    Classes = classes(R),
    #ah_navigationbar{expand_mode = Mode, animation = Anim, toggle_mode = Toggle,
                      arrow_position = ArrowPos} = R,
    check(expand_mode, Mode, [single, single_fit_height, multiple, toggle, none]),
    check(animation, Anim, [slide, fade, none]),
    check(toggle_mode, Toggle, [click, dblclick, none]),
    check(arrow_position, ArrowPos, [left, right]),
    [is_integer(D) andalso D >= 0 orelse error({aihtml, {bad_option, F, D}})
     || {F, D} <- [{expand_duration, R#ah_navigationbar.expand_duration},
                   {collapse_duration, R#ah_navigationbar.collapse_duration}]],
    Items = [nav_item(I) || I <- Items0],
    Expanded = indexes(Value),
    Arrow = fun(Open) -> nav_arrow(R, Open) end,
    Sections =
        [begin
             Open = lists:member(Idx, Expanded),
             HeaderId = <<Id/binary, "-item-", (integer_to_binary(Idx))/binary, "-header">>,
             BodyId = <<Id/binary, "-item-", (integer_to_binary(Idx))/binary, "-content">>,
             ?H:el('div',
                   [?H:el('div',
                          %% sigil puts the arrow first and moves a left one with
                          %% `order'; a right one goes after the text here
                          [?H:el(span, nav_header(Header),
                                 [<<"ah-navigationbar-header-text">>,
                                  [<<"ah-navigationbar-header-text-structured">>
                                   || is_map(Header)]], []),
                           Arrow(Open)],
                          [<<"ah-navigationbar-header">>,
                           [<<"ah-navigationbar-header-expanded">> || Open],
                           [<<"ah-navigationbar-disabled">> || Off],
                           [<<"ah-navigationbar-header-no-toggle">> || Toggle =:= none]],
                          [{id, HeaderId}, {role, button},
                           {tabindex, case Off orelse Disabled of
                                          true -> <<"-1">>;
                                          false -> <<"0">>
                                      end},
                           {aria_expanded, atom_to_binary(Open, utf8)},
                           {aria_controls, BodyId},
                           {aria_disabled, Off andalso <<"true">>}]),
                    ?H:el('div',
                          [?H:el('div', Content, [<<"ah-navigationbar-content">>], []),
                           case Actions of
                               undefined -> [];
                               _ -> ?H:el('div', Actions, [<<"ah-navigationbar-actions">>], [])
                           end],
                          [<<"ah-navigationbar-body">>],
                          [{id, BodyId}, {role, region}, {aria_labelledby, HeaderId},
                           {style, case Open of true -> undefined; false -> <<"display:none;">> end}])],
                   [<<"ah-navigationbar-item">>], [])
         end || {Idx, {Header, Content, Actions, Off}}
                    <- lists:zip(lists:seq(0, length(Items) - 1), Items)],
    Cur = join([integer_to_binary(I) || I <- Expanded]),
    Style = [[<<"width:">>, css_size(W), $;] || W <- [R#ah_navigationbar.width], W =/= undefined]
        ++ [[<<"height:">>, css_size(Hh), $;] || Hh <- [R#ah_navigationbar.height], Hh =/= undefined],
    ?H:el('div',
          %% the hidden input goes first: items rely on :last-child
          [hidden_input(Name, Cur) | Sections],
          [Classes, <<"ah-navigationbar-vertical">>,
           case Mode of
               single -> <<"ah-navigationbar-expand-single">>;
               multiple -> <<"ah-navigationbar-expand-multiple">>;
               _ -> []
           end,
           case Anim of
               slide -> <<"ah-navigationbar-animate-slide">>;
               fade -> <<"ah-navigationbar-animate-fade">>;
               none -> []
           end,
           [<<"ah-navigationbar-disabled">> || Disabled]],
          [[{id, Id}, {style, case Style of [] -> undefined; _ -> iolist_to_binary(Style) end},
            {data_ah, <<"navigationbar">>}, {data_ah_value, Cur},
            {data_expand_mode, Mode}, {data_animation, Anim}, {data_toggle_mode, Toggle},
            {data_expand_duration, R#ah_navigationbar.expand_duration},
            {data_collapse_duration, R#ah_navigationbar.collapse_duration},
            {data_fit, Mode =:= single_fit_height andalso R#ah_navigationbar.height =/= undefined},
            {aria_disabled, Disabled andalso <<"true">>}],
           ?E:root_attrs(R, change)]).

nav_item(#{header := H} = M) ->
    {H, maps:get(content, M, []), maps:get(actions, M, undefined),
     maps:get(disabled, M, false) =:= true};
nav_item({H, C}) -> {H, C, undefined, false};
nav_item({H, C, Opts}) ->
    O = flat(Opts),
    {H, C, proplists:get_value(actions, O), proplists:get_value(disabled, O, false) =:= true};
nav_item(Other) -> error({aihtml, {bad_navigationbar_item, Other}}).

nav_header(#{title := T} = M) ->
    [?H:el(span, T, [<<"ah-navigationbar-header-title">>], []),
     [?H:el(span, S, [<<"ah-navigationbar-header-subheader">>], [])
      || S <- [maps:get(subheader, M, undefined)], S =/= undefined],
     [?H:el(span, X, [<<"ah-navigationbar-header-extra">>], [])
      || X <- [maps:get(extra, M, undefined)], X =/= undefined]];
nav_header(H) when is_map(H) -> error({aihtml, {bad_navigationbar_header, H}});
nav_header(H) -> H.

%% sigil's render-arrow: one icon that turns, or two that swap.
nav_arrow(#ah_navigationbar{no_arrow = true}, _) -> [];
nav_arrow(#ah_navigationbar{arrow_position = Pos, expand_icon = Ex, collapse_icon = Co}, Open) ->
    Dual = Ex =/= undefined andalso Co =/= undefined,
    Cls = [<<"ah-navigationbar-arrow">>,
           [<<"ah-navigationbar-arrow-left">> || Pos =:= left],
           [<<"ah-navigationbar-arrow-up">> || Open],
           [<<"ah-navigationbar-arrow-dual">> || Dual]],
    Primary = case Ex of undefined -> <<"▼"/utf8>>; _ -> Ex end,
    case Dual of
        true ->
            ?H:el(span, [?H:el(span, Primary, [<<"ah-navigationbar-icon">>,
                                               <<"ah-navigationbar-icon-expand">>], []),
                         ?H:el(span, Co, [<<"ah-navigationbar-icon">>,
                                          <<"ah-navigationbar-icon-collapse">>], [])],
                  Cls, [{aria_hidden, <<"true">>}]);
        false ->
            ?H:el(span, Primary, Cls, [{aria_hidden, <<"true">>}])
    end.

indexes(undefined) -> [];
indexes(I) when is_integer(I), I >= 0 -> [I];
indexes(B) when is_binary(B) ->
    lists:usort([try binary_to_integer(string:trim(P))
                 catch error:badarg -> error({aihtml, {bad_value, B}})
                 end || P <- binary:split(B, <<",">>, [global, trim_all])]);
indexes(L) when is_list(L) ->
    [is_integer(I) andalso I >= 0 orelse error({aihtml, {bad_value, L}}) || I <- L],
    lists:usort(L);
indexes(Other) -> error({aihtml, {bad_value, Other}}).

css_size(N) when is_integer(N) -> [integer_to_binary(N), <<"px">>];
css_size(S) -> S.

%%% command ------------------------------------------------------------

render_command(#ah_command{palette = Palette} = R0) ->
    {Id, R} = ensure_id(R0),
    Classes = classes(R),
    #ah_command{placeholder = Placeholder, query = Query, search = Search,
                hotkey = Hotkey} = R,
    is_boolean(R#ah_command.close_on_select)
        orelse error({aihtml, {bad_option, close_on_select, R#ah_command.close_on_select}}),
    SearchAttrs = case Search of
                      undefined -> [];
                      {M, A, _} = Ref when is_atom(M), is_atom(A) ->
                          aihtml:on(input, Ref, #{debounce => 250});
                      Other -> error({aihtml, {bad_option, search, Other}})
                  end,
    ListId = sub_id(Id, <<"list">>),
    List = command_list(Id, R#ah_command.items, R#ah_command.empty_text),
    Input = ?H:void(input, [<<"ah-command__input">>],
                    [[{type, text}, {id, sub_id(Id, <<"input">>)},
                      {placeholder, Placeholder}, {value, Query},
                      {autocomplete, off}, {spellcheck, <<"false">>},
                      {aria_label, <<"command input">>},
                      {role, combobox}, {aria_expanded, <<"true">>},
                      {aria_autocomplete, list}, {aria_controls, ListId},
                      {data_command, Id}, {data_empty, R#ah_command.empty_text}],
                     SearchAttrs]),
    Root = ?H:el('div',
                 [?H:el('div', Input, [<<"ah-command__input-wrap">>], []),
                  ?H:el('div', List, [<<"ah-command__list">>],
                        [{id, ListId}, {role, listbox}])],
                 Classes,
                 [[{id, Id},
                   {role, Palette andalso dialog}, {aria_modal, Palette andalso <<"true">>},
                   {aria_label, Palette andalso <<"Command palette">>},
                   {data_ah, <<"command">>},
                   {data_ah_remote, Search =/= undefined},
                   {data_ah_query, Query},
                   {data_hotkey, Hotkey},
                   {data_auto_focus, R#ah_command.auto_focus},
                   {data_close_on_select, atom_to_binary(R#ah_command.close_on_select, utf8)}],
                  ?E:root_attrs(R, 'ah:select')]),
    case Palette of
        true -> ?H:el('div', Root, [<<"ah-command-overlay">>], [{hidden, true}]);
        false -> Root
    end.

%% The groups, items and the empty message, as the list's children. Items
%% are numbered across groups; the first one is active.
command_list(Id, Entries, EmptyText) ->
    Groups = command_groups(Entries),
    {Rendered, N} =
        lists:mapfoldl(
          fun({Heading, Items}, I0) ->
                  {Html, I1} = lists:mapfoldl(fun(It, I) -> {command_item(Id, It, I), I + 1} end,
                                              I0, Items),
                  {?H:el('div',
                         [[?H:el('div', Heading, [<<"ah-command__group-heading">>],
                                 [{role, presentation}]) || Heading =/= undefined],
                          Html],
                         [<<"ah-command__group">>], [{role, group}]), I1}
          end, 0, [G || {_, [_ | _]} = G <- Groups]),
    [Rendered,
     ?H:el('div', EmptyText, [<<"ah-command__empty">>], [{hidden, N > 0}])].

%% Consecutive bare commands form an unnamed group.
command_groups(Entries) ->
    [{case H of bare -> undefined; _ -> H end, Items}
     || {H, Items} <- fold_groups(Entries, [])].

fold_groups([], Acc) -> lists:reverse(Acc);
fold_groups([#{items := Items} = G | Rest], Acc) when is_list(Items) ->
    fold_groups(Rest, [{maps:get(heading, G, undefined), [cmd_item(I) || I <- Items]} | Acc]);
fold_groups([I | Rest], [{bare, Is} | Acc]) ->
    fold_groups(Rest, [{bare, Is ++ [cmd_item(I)]} | Acc]);
fold_groups([I | Rest], Acc) ->
    fold_groups(Rest, [{bare, [cmd_item(I)]} | Acc]).

cmd_item(#{value := V} = M) ->
    maps:merge(#{label => maps:get(label, M, text(V))},
               maps:put(value, text(V), maps:with([description, icon, shortcut, href,
                                                    disabled], M)));
cmd_item({V, L}) -> #{value => text(V), label => L};
cmd_item(V) when is_binary(V); is_atom(V); is_integer(V) ->
    T = text(V), #{value => T, label => T};
cmd_item(V) when is_list(V) ->
    case io_lib:printable_unicode_list(V) of
        true -> T = text(V), #{value => T, label => T};
        false -> error({aihtml, {bad_command_item, V}})
    end;
cmd_item(Other) -> error({aihtml, {bad_command_item, Other}}).

command_item(Id, #{value := V, label := L} = It, I) ->
    Idx = integer_to_binary(I),
    Active = I =:= 0,
    Off = maps:get(disabled, It, false) =:= true,
    Desc = maps:get(description, It, undefined),
    Icon = maps:get(icon, It, undefined),
    Shortcut = maps:get(shortcut, It, undefined),
    ?H:el('div',
          [[?H:el(span, Icon, [<<"ah-command__item-icon">>], [{aria_hidden, <<"true">>}])
            || Icon =/= undefined],
           ?H:el('div',
                 [?H:el(span, L, [<<"ah-command__item-label">>], []),
                  [?H:el(span, Desc, [<<"ah-command__item-desc">>], []) || Desc =/= undefined]],
                 [<<"ah-command__item-text">>], []),
           [?H:el(kbd, Shortcut, [<<"ah-kbd">>, <<"ah-command__item-shortcut">>], [])
            || Shortcut =/= undefined]],
          [<<"ah-command__item">>],
          [{id, sub_id(Id, <<"item-", Idx/binary>>)}, {role, option},
           {data_value, V}, {data_index, Idx},
           {data_active, atom_to_binary(Active, utf8)},
           {data_disabled, atom_to_binary(Off, utf8)},
           {data_href, maps:get(href, It, undefined)},
           {aria_selected, atom_to_binary(Active, utf8)},
           {aria_disabled, Off andalso <<"true">>}]).

%%%===================================================================
%%% Server-side search
%%%===================================================================

%% @doc Replace the commands of a palette from inside an action, typically
%% its `search' action: `set_command_items(Ctx, Event, Items)'. `Target'
%% is the search event (whose `data' names the palette and its empty
%% text) or `{id, RootId}'.
%% Items take the same forms as in `command/3'. The list is rendered
%% here and morphed into `<root id>-list' (morph_inner), so the text
%% field keeps its focus and caret; then the behaviour method
%% `itemsLoaded' marks the first command active.
-spec set_command_items(aihtml_action:ctx(), {id, iodata() | atom()} | aihtml_action:event(),
                        [ah_bars_cmd_entry()]) -> ok.
set_command_items(Ctx, #{data := #{<<"command">> := Id} = Data}, Items) ->
    set_items(Ctx, text(Id), Items, maps:get(<<"empty">>, Data, <<"No results found.">>));
set_command_items(Ctx, {id, Id}, Items) ->
    set_items(Ctx, text(Id), Items, <<"No results found.">>).

set_items(Ctx, Id, Items, EmptyText) ->
    aihtml_action:html(Ctx, {id, sub_id(Id, <<"list">>)},
                       command_list(Id, Items, EmptyText), morph_inner),
    aihtml_action:call(Ctx, {id, Id}, itemsLoaded, []).

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => activity_bar, category => layout,
       signature => <<"activity_bar(Items, Value, Css, Attrs)">>,
       root => <<"ah-activity-bar">>,
       groups => #{placement => {[left, right], left}},
       classes => #{left => [], right => []},
       behavior => <<"activity-bar">>,
       events => [<<"change">>, <<"ah:select">>],
       doc => <<"A narrow VS Code-style rail of icon buttons; the value is the active item.">>,
       option_docs => #{},
       methods => [#{name => setValue, args => <<"(Value)">>,
                     doc => <<"Make an item active without firing change.">>},
                   #{name => getValue, args => <<"()">>, doc => <<"Return the active item.">>}]},
     #{name => navigationbar, category => layout,
       signature => <<"navigationbar(Items, Value, Css, Attrs)">>,
       root => <<"ah-navigationbar">>,
       flags => [square, disable_gutters, no_arrow],
       classes => #{disable_gutters => [<<"ah-navigationbar-no-gutters">>], no_arrow => []},
       options => [expand_mode, animation, toggle_mode, arrow_position, expand_icon,
                   collapse_icon, expand_duration, collapse_duration, width, height],
       behavior => <<"navigationbar">>,
       events => [<<"change">>, <<"ah:expand">>, <<"ah:collapse">>],
       doc => <<"Collapsible sections under clickable headers (an accordion); "
                "the value is the expanded indexes, e.g. \"0,2\".">>,
       option_docs =>
           #{square => <<"No rounded corners.">>,
             disable_gutters => <<"Compact: no outer border or side padding, only dividers.">>,
             no_arrow => <<"Hide the expand arrow.">>,
             expand_mode => <<"single_fit_height (default; with a height the open section "
                              "fills it), single (one open, cannot be closed), toggle (at most "
                              "one open), multiple, or none (the user cannot toggle).">>,
             animation => <<"slide (default), fade or none.">>,
             toggle_mode => <<"What opens a section: click (default), dblclick or none; "
                              "Enter and Space always work unless none.">>,
             arrow_position => <<"right (default) or left of the header text.">>,
             expand_icon => <<"HTML of the arrow (default a small triangle that turns).">>,
             collapse_icon => <<"HTML shown while expanded; with expand_icon the two swap "
                                "(plus / minus style).">>,
             expand_duration => <<"Expand animation in ms (default 250).">>,
             collapse_duration => <<"Collapse animation in ms (default 250).">>,
             width => <<"Width: px as an integer or a CSS length.">>,
             height => <<"Height: px as an integer or a CSS length.">>},
       methods => [#{name => expand, args => <<"(Index)">>, doc => <<"Expand a section.">>},
                   #{name => collapse, args => <<"(Index)">>, doc => <<"Collapse a section.">>},
                   #{name => toggle, args => <<"(Index)">>, doc => <<"Expand or collapse a section.">>},
                   #{name => setValue, args => <<"(Indexes)">>,
                     doc => <<"Expand exactly these sections (a list or \"0,2\").">>},
                   #{name => getValue, args => <<"()">>,
                     doc => <<"Return the expanded indexes as an array.">>},
                   #{name => enable, args => <<"(Index)">>, doc => <<"Enable a section.">>},
                   #{name => disable, args => <<"(Index)">>, doc => <<"Disable a section.">>}]},
     #{name => command, category => overlay,
       signature => <<"command(Items, Css, Attrs)">>,
       root => <<"ah-command">>,
       flags => [palette, auto_focus],
       classes => #{palette => [<<"ah-command-panel">>], auto_focus => []},
       options => [placeholder, empty_text, query, search, hotkey, close_on_select],
       behavior => <<"command">>,
       events => [<<"ah:select">>, <<"ah:query">>, <<"ah:open">>, <<"ah:close">>],
       doc => <<"A command palette: a search field over grouped commands with keyboard "
                "navigation, inline or in a centred ⌘K overlay."/utf8>>,
       option_docs =>
           #{palette => <<"Render in a hidden centred overlay; open with the open method or hotkey.">>,
             auto_focus => <<"Focus the search field when the page loads (inline palettes).">>,
             placeholder => <<"Placeholder of the search field.">>,
             empty_text => <<"Shown when nothing matches (default \"No results found.\").">>,
             query => <<"Initial search text.">>,
             search => <<"Action ref {Module, Action, Args} run (debounced) as the user types; "
                         "Event.value is the query and the action answers with "
                         "set_command_items(Ctx, Event, Items). The browser does not filter.">>,
             hotkey => <<"A letter: Ctrl+letter or ⌘+letter toggles the palette, e.g. <<\"k\">>."/utf8>>,
             close_on_select => <<"Close the overlay after a command is chosen (default true).">>},
       methods => [#{name => open, args => <<"()">>, doc => <<"Open the overlay and focus the field.">>},
                   #{name => close, args => <<"()">>, doc => <<"Close the overlay.">>},
                   #{name => toggle, args => <<"()">>, doc => <<"Open or close the overlay.">>},
                   #{name => setQuery, args => <<"(Text)">>,
                     doc => <<"Set the search text and filter (or run the search action).">>},
                   #{name => focus, args => <<"()">>, doc => <<"Focus the search field.">>},
                   #{name => itemsLoaded, args => <<"()">>,
                     doc => <<"Re-read the list after the server replaced it (set_command_items does this).">>}]}].

%%%===================================================================
%%% Internal
%%%===================================================================

classes(R) ->
    Tag = element(1, R),
    ?E:classes(R, fields(Tag), entry(?E:component_name(Tag))).

entry(Name) -> aihtml_catalog:entry(?MODULE, Name).

check(Field, V, Allowed) ->
    lists:member(V, Allowed) orelse error({aihtml, {bad_option, Field, V}}).

%% The parts refer to each other by id, so a root without one gets one.
ensure_id(R) ->
    Id = case element(3, R) of
             undefined -> <<"ah-b", (integer_to_binary(erlang:unique_integer([positive])))/binary>>;
             Id0 -> text(Id0)
         end,
    {Id, setelement(3, R, Id)}.

sub_id(Id, Part) -> <<Id/binary, "-", Part/binary>>.

is_disabled(Attrs) ->
    case lists:keyfind(<<"disabled">>, 1, ?H:attrs(Attrs)) of
        {_, V} -> V =/= <<"false">>;
        false -> false
    end.

flat(M) when is_map(M) -> maps:to_list(M);
flat(L) when is_list(L) -> L.

hidden_input(undefined, _) -> [];
hidden_input(Name, Value) ->
    ?H:void(input, [], [{type, hidden}, {name, Name}, {value, Value}]).

value_text(undefined) -> <<>>;
value_text(V) -> text(V).

join(Vs) -> iolist_to_binary(lists:join(<<",">>, Vs)).

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
