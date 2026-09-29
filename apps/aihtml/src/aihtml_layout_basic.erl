%%%-------------------------------------------------------------------
%%% @doc Layout components ported from sigil: card, panel, expander,
%%% tabs, tab_bar, breadcrumbs, pagination, steps, skeleton, loader and
%%% empty (designs/04-components.md).
%%%
%%% The DOM and class names are sigil's, so the ported styles under
%%% priv/css/sigil/components apply unchanged; aihtml additions live in
%%% priv/css/extra/layout_basic.css and the behaviours in
%%% assets/js/components/layout_basic.js.
%%%
%%% Value-bearing components (tabs, tab_bar, pagination, steps, expander)
%%% keep their value in `data-ah-value' on the root and fire `change' there
%%% when the user changes it, so `on(change, Action)' on the root receives
%%% the new value as `Event.value'.
%%%
%%% Each function builds an element record (#ah_card{} ..., defined in
%%% include/aihtml_layout_basic.hrl) and render/1 turns it into HTML, so
%%% pages may also write the records directly (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_layout_basic).
-behaviour(aihtml_element).

-include("aihtml_layout_basic.hrl").

-export([card/3, panel/3, expander/3, tabs/4, tab_bar/4, breadcrumbs/3,
         pagination/4, steps/4, skeleton/2, loader/2, empty/3]).
-export([render/1, fields/1]).
-export([visible_pages/3, pagination_view/5]).
-export([catalog/0]).

%% Shared templates (see aihtml_tpl), also compiled to AH.tpl.* for the browser.
-compile({parse_transform, beamai_mustache_transform}).
-mustache_template({tpl_pagination_items, "../templates/pagination_items.mustache"}).
-mustache_template({tpl_steps_indicator, "../templates/steps_indicator.mustache"}).

-define(E, aihtml_element).

-type html() :: aihtml_html:html().
-type css() :: aihtml_html:css().
-type attrs() :: aihtml_html:attrs().
-type key() :: ah_lb_key().
-type element() :: #ah_card{} | #ah_panel{} | #ah_expander{} | #ah_tabs{} | #ah_tab_bar{}
                 | #ah_breadcrumbs{} | #ah_pagination{} | #ah_steps{} | #ah_skeleton{}
                 | #ah_loader{} | #ah_empty{}.

-export_type([key/0, element/0]).

%%%===================================================================
%%% Builders
%%%===================================================================

%% @doc A container with an optional media strip, header (title, subtitle,
%% extra), body (`Children') and footer.
%% Options: title, subtitle, extra, header, media, footer.
-spec card(html(), css(), attrs()) -> #ah_card{}.
card(Children, Css, Attrs) ->
    build(#ah_card{body = Children}, Css, Attrs).

%% @doc A scrollable content container (sigil's panel), optionally with a
%% header bar holding a title, actions and a collapse toggle.
%% Options: title, actions, collapsible, collapsed, height, max_height.
-spec panel(html(), css(), attrs()) -> #ah_panel{}.
panel(Children, Css, Attrs) ->
    build(#ah_panel{body = Children}, Css, Attrs).

%% @doc A collapsible section. `Children' is the content.
%% Options: header (html or #{title, subheader, extra}), actions,
%% expanded (default true), toggle_mode (click | dblclick | none),
%% animation (slide | fade | none), duration (ms), show_arrow,
%% arrow_position (right | left), expand_icon, collapse_icon,
%% accordion (a name: opening one closes the others with that name), name.
%% Value: "true" | "false".
-spec expander(html(), css(), attrs()) -> #ah_expander{}.
expander(Children, Css, Attrs) ->
    build(#ah_expander{body = Children}, Css, Attrs).

%% @doc Tabbed panels. `Tabs' is `[{Key, Label, Panel}]' or
%% `[{Key, Label, Panel, #{disabled => true}}]'; every panel is rendered,
%% the client switches between them. `Active' is a key (undefined: the
%% first enabled tab).
%% Options: animation (fade | none), selection_mode (click | hover),
%% scrollable, name. Value: the active key.
-spec tabs([{key(), html(), html()} | {key(), html(), html(), map()}],
           key() | undefined, css(), attrs()) -> #ah_tabs{}.
tabs(Tabs, Active, Css, Attrs) ->
    build(#ah_tabs{items = Tabs, value = Active}, Css, Attrs).

%% @doc An editor-style strip of closable tabs (no panels). `Items' is
%% `[{Id, Title}]' or `[{Id, Title, #{dirty => true, icon => Html}}]'.
%% Options: closable (default true), close_label, name.
%% Value: the active id. Closing a tab removes it (and fires `change' when
%% the active tab moves); the root also gets `ah:close' with the id.
-spec tab_bar([{key(), html()} | {key(), html(), map()}], key() | undefined,
              css(), attrs()) -> #ah_tab_bar{}.
tab_bar(Items, Active, Css, Attrs) ->
    build(#ah_tab_bar{items = Items, value = Active}, Css, Attrs).

%% @doc An ancestor path. `Items' are `Label', `{Label, Href}' or
%% `#{label, href, icon, attrs}'; the last item is the current page.
%% Options: separator (default "/"; none for dots), active_last, max_items.
-spec breadcrumbs([html() | {html(), binary() | undefined} | map()], css(), attrs()) ->
          #ah_breadcrumbs{}.
breadcrumbs(Items, Css, Attrs) ->
    build(#ah_breadcrumbs{items = Items}, Css, Attrs).

%% @doc Page navigation for `Total' items, `Page' being current (from 1).
%% Options: page_size (10), page_sizes ([10,20,50,100]),
%% show_size_selector (true), show_jumper, show_first_last, show_total,
%% max_visible (7 slots) or siblings (pages on each side of the current
%% one), href (a template with {page} and {size}: pages become links and
%% no script is needed), labels (#{prev, next, first, last, per_page,
%% total, goto, goto_suffix, goto_confirm, page_info, aria_label,
%% per_page_aria}), name. Value: the current page.
-spec pagination(non_neg_integer(), pos_integer(), css(), attrs()) -> #ah_pagination{}.
pagination(Total, Page, Css, Attrs) ->
    build(#ah_pagination{total = Total, value = Page}, Css, Attrs).

%% @doc A step indicator (wizard). `Steps' are `Title', `{Title, Description}'
%% or `#{title, description, content, status, disabled}' (status is one of
%% completed | active | error | disabled | pending; by default it follows
%% `Current', a 0-based index). When any step has content, the panels and
%% prev/next buttons are rendered too.
%% Options: clickable (default true), show_nav, prev_label, next_label, name.
%% Value: the current index.
-spec steps([html() | {html(), html()} | map()], non_neg_integer(), css(), attrs()) ->
          #ah_steps{}.
steps(Steps, Current, Css, Attrs) ->
    build(#ah_steps{items = Steps, value = Current}, Css, Attrs).

%% @doc A shimmering placeholder. Css: text (default) | circle | rect,
%% static (no shimmer), done (hidden).
%% Options: lines (text, default 3), width, height, radius, label.
-spec skeleton(css(), attrs()) -> #ah_skeleton{}.
skeleton(Css, Attrs) ->
    build(#ah_skeleton{}, Css, Attrs).

%% @doc A spinner. By default an overlay covering its positioned parent
%% (sigil's loader); `inline' puts it in the flow, `center' in a box fixed
%% at the middle of the viewport, `hidden' renders it hidden. Css also
%% picks the text position: bottom (default) | top | left | right.
%% Options: text (default "Loading..."; <<>> for none), modal (a page
%% scrim while shown; Esc hides it).
%% Methods: show([Left, Top]), hide, toggle, text(Text).
-spec loader(css(), attrs()) -> #ah_loader{}.
loader(Css, Attrs) ->
    build(#ah_loader{}, Css, Attrs).

%% @doc An empty-state placeholder: icon, title, description and
%% `Children' as the action area.
%% Options: icon (html, e.g. {safe, Svg}), title, description.
-spec empty(html(), css(), attrs()) -> #ah_empty{}.
empty(Children, Css, Attrs) ->
    build(#ah_empty{body = Children}, Css, Attrs).

build(R, Css, Attrs) ->
    Tag = element(1, R),
    ?E:build(R, fields(Tag), cat_entry(?E:component_name(Tag)), Css, Attrs).

%% @doc The field names of one of this group's records.
-spec fields(atom()) -> [atom()].
fields(ah_card) -> record_info(fields, ah_card);
fields(ah_panel) -> record_info(fields, ah_panel);
fields(ah_expander) -> record_info(fields, ah_expander);
fields(ah_tabs) -> record_info(fields, ah_tabs);
fields(ah_tab_bar) -> record_info(fields, ah_tab_bar);
fields(ah_breadcrumbs) -> record_info(fields, ah_breadcrumbs);
fields(ah_pagination) -> record_info(fields, ah_pagination);
fields(ah_steps) -> record_info(fields, ah_steps);
fields(ah_skeleton) -> record_info(fields, ah_skeleton);
fields(ah_loader) -> record_info(fields, ah_loader);
fields(ah_empty) -> record_info(fields, ah_empty).

%%%===================================================================
%%% Rendering
%%%===================================================================

-spec render(element()) -> html().
render(#ah_card{body = Children, header = H, title = T, subtitle = S, extra = X} = R) ->
    Header = case {H, T, S, X} of
                 {undefined, undefined, undefined, undefined} -> [];
                 {undefined, _, _, _} ->
                     el('div', [maybe_el(h3, T, <<"ah-card-title">>),
                                maybe_el(p, S, <<"ah-card-subtitle">>),
                                maybe_el('div', X, <<"ah-card-extra">>)],
                        [<<"ah-card-header">>], []);
                 _ -> el('div', H, [<<"ah-card-header">>], [])
             end,
    el('div', [maybe_el('div', R#ah_card.media, <<"ah-card-media">>),
               Header,
               el('div', Children, [<<"ah-card-body">>], []),
               maybe_el('div', R#ah_card.footer, <<"ah-card-footer">>)],
       classes(R), ?E:root_attrs(R, none));

render(#ah_panel{body = Children, title = Title, actions = Actions} = R0) ->
    Classes = classes(R0),
    {Id, R} = with_id(R0, <<"ah-panel">>),
    Collapsible = bool(collapsible, R#ah_panel.collapsible),
    Collapsed = bool(collapsed, R#ah_panel.collapsed) andalso Collapsible,
    BodyId = <<Id/binary, "-body">>,
    TitleId = <<Id/binary, "-title">>,
    Header = case Title =:= undefined andalso Actions =:= undefined
                 andalso not Collapsible of
                 true -> [];
                 false ->
                     el('div',
                        [maybe_el('div', Title, <<"ah-panel-title">>, [{id, TitleId}]),
                         maybe_el('div', Actions, <<"ah-panel-actions">>),
                         case Collapsible of
                             false -> [];
                             true ->
                                 el(button, {safe, chevron()}, [<<"ah-panel-toggle">>],
                                    [{type, button}, {aria_expanded, tf(not Collapsed)},
                                     {aria_controls, BodyId},
                                     {aria_label, R#ah_panel.toggle_label}])
                         end],
                        [<<"ah-panel-header">>], [])
             end,
    Style = style([{<<"height">>, len(R#ah_panel.height)},
                   {<<"max-height">>, len(R#ah_panel.max_height)},
                   {<<"display">>, Collapsed andalso <<"none">>}]),
    Wrapper = el('div', el('div', Children, [<<"ah-panel-content">>], []),
                 [<<"ah-panel-wrapper">>], [{id, BodyId}, {style, Style}]),
    el('div', [Header, Wrapper],
       [Classes, [<<"ah-panel-has-header">> || Header =/= []],
        [<<"ah-panel-collapsed">> || Collapsed]],
       [[{id, Id}, {data_ah, <<"panel">>},
         {aria_labelledby, Title =/= undefined andalso TitleId},
         {role, Title =/= undefined andalso region}], ?E:root_attrs(R, none)]);

render(#ah_expander{body = Children, disabled = Disabled, toggle_mode = Mode,
                    expand_icon = ExpIcon, collapse_icon = ColIcon} = R0) ->
    Classes = classes(R0),
    {Id, R} = with_id(R0, <<"ah-expander">>),
    HId = <<Id/binary, "-header">>,
    CId = <<Id/binary, "-content">>,
    Expanded = bool(expanded, R#ah_expander.expanded),
    one_of(toggle_mode, Mode, [click, dblclick, none]),
    one_of(animation, R#ah_expander.animation, [undefined, slide, fade, none]),
    ArrowPos = one_of(arrow_position, R#ah_expander.arrow_position, [right, left]),
    Dual = ExpIcon =/= undefined andalso ColIcon =/= undefined,
    ArrowCls = [<<"ah-expander-arrow">>,
                [<<"ah-expander-arrow-left">> || ArrowPos =:= left],
                [<<"ah-expander-arrow-expanded">> || Expanded],
                [<<"ah-expander-arrow-dual">> || Dual]],
    Primary = case ExpIcon of undefined -> <<"\x{25BE}"/utf8>>; I -> I end,
    Arrow = case {bool(show_arrow, R#ah_expander.show_arrow), Dual} of
                {false, _} -> [];
                {_, true} ->
                    el(span, [el(span, Primary, [<<"ah-expander-icon ah-expander-icon-expand">>], []),
                              el(span, ColIcon, [<<"ah-expander-icon ah-expander-icon-collapse">>], [])],
                       ArrowCls, [{aria_hidden, <<"true">>}]);
                {_, false} ->
                    el(span, Primary, ArrowCls, [{aria_hidden, <<"true">>}])
            end,
    Text = case R#ah_expander.header of
               #{} = M ->
                   el(span, [el(span, maps:get(title, M, <<>>), [<<"ah-expander-header-title">>], []),
                             maybe_el(span, maps:get(subheader, M, undefined),
                                      <<"ah-expander-header-subheader">>),
                             maybe_el(span, maps:get(extra, M, undefined),
                                      <<"ah-expander-header-extra">>)],
                      [<<"ah-expander-header-text ah-expander-header-text-structured">>], []);
               H -> el(span, H, [<<"ah-expander-header-text">>], [])
           end,
    Header = el('div', [Text, Arrow],
                [<<"ah-expander-header">>,
                 [<<"ah-expander-header-expanded">> || Expanded],
                 [<<"ah-expander-header-no-toggle">> || Mode =:= none]],
                [{id, HId}, {role, button},
                 {tabindex, case Disabled of true -> -1; false -> 0 end},
                 {aria_expanded, tf(Expanded)}, {aria_controls, CId},
                 {aria_disabled, Disabled andalso <<"true">>}]),
    Body = el('div', [el('div', Children, [<<"ah-expander-content">>], []),
                      maybe_el('div', R#ah_expander.actions, <<"ah-expander-actions">>)],
              [<<"ah-expander-body">>],
              [{id, CId}, {role, region}, {aria_labelledby, HId},
               {style, (not Expanded) andalso <<"display:none">>}]),
    Value = tf(Expanded),
    el('div', [Header, Body, hidden(R#ah_expander.name, Value)], Classes,
       [[{id, Id}, {data_ah, <<"expander">>}, {data_ah_value, Value},
         {data_toggle_mode, Mode =/= click andalso Mode},
         {data_animation, R#ah_expander.animation},
         {data_duration, R#ah_expander.duration},
         {data_accordion, R#ah_expander.accordion}], ?E:root_attrs(R, change)]);

render(#ah_tabs{items = Tabs, value = Active, position = Position} = R0) ->
    Classes = classes(R0),
    {Id, R} = with_id(R0, <<"ah-tabs">>),
    one_of(animation, R#ah_tabs.animation, [undefined, fade, none]),
    one_of(selection_mode, R#ah_tabs.selection_mode, [undefined, click, hover]),
    Norm = [norm_tab(T) || T <- Tabs],
    ActiveKey = active_key(Active, [{K, D} || {K, _, _, D} <- Norm]),
    Vertical = lists:member(Position, [left, right]),
    Indexed = lists:zip(lists:seq(0, length(Norm) - 1), Norm),
    TabId = fun(I) -> <<Id/binary, "-tab-", (integer_to_binary(I))/binary>> end,
    PanelId = fun(I) -> <<Id/binary, "-panel-", (integer_to_binary(I))/binary>> end,
    Items = [el(li, Label,
                [<<"ah-tabs-item">>, [<<"ah-tabs-item-selected">> || K =:= ActiveKey],
                 [<<"ah-tabs-item-disabled">> || Dis]],
                [{id, TabId(I)}, {role, tab}, {data_key, K},
                 {tabindex, case K =:= ActiveKey of true -> 0; false -> -1 end},
                 {aria_selected, tf(K =:= ActiveKey)}, {aria_controls, PanelId(I)},
                 {aria_disabled, Dis andalso <<"true">>}])
             || {I, {K, Label, _, Dis}} <- Indexed],
    Scrollable = bool(scrollable, R#ah_tabs.scrollable),
    Scroll = fun(Side, Glyph) ->
                     [el(li, Glyph, [<<"ah-tabs-scroll-btn ah-tabs-scroll-", Side/binary>>],
                         [{role, presentation}, {aria_hidden, <<"true">>}]) || Scrollable]
             end,
    Header = el(ul, [Scroll(<<"left">>, <<"\x{25C0}"/utf8>>), Items,
                     Scroll(<<"right">>, <<"\x{25B6}"/utf8>>)],
                [<<"ah-tabs-header">>],
                [{role, tablist},
                 {aria_orientation, case Vertical of true -> vertical; false -> horizontal end}]),
    Panels = [el('div', Panel,
                 [<<"ah-tabs-panel">>, [<<"ah-tabs-panel-active">> || K =:= ActiveKey]],
                 [{id, PanelId(I)}, {role, tabpanel}, {tabindex, 0},
                  {aria_labelledby, TabId(I)}, {aria_hidden, tf(K =/= ActiveKey)},
                  {style, K =/= ActiveKey andalso <<"display:none">>}])
              || {I, {K, _, Panel, _}} <- Indexed],
    el('div', [Header, el('div', Panels, [<<"ah-tabs-content">>], []),
               hidden(R#ah_tabs.name, ActiveKey)],
       [Classes, [<<"ah-tabs-scrollable">> || Scrollable]],
       [[{id, Id}, {data_ah, <<"tabs">>}, {data_ah_value, ActiveKey},
         {data_animation, R#ah_tabs.animation},
         {data_selection_mode, R#ah_tabs.selection_mode}], ?E:root_attrs(R, change)]);

render(#ah_tab_bar{items = Items, value = Active} = R) ->
    Norm = [case I of
                {K, T} -> {bin(K), T, #{}};
                {K, T, M} when is_map(M) -> {bin(K), T, M}
            end || I <- Items],
    ActiveKey = active_key(Active, [{K, false} || {K, _, _} <- Norm]),
    Closable = bool(closable, R#ah_tab_bar.closable),
    CloseLabel = R#ah_tab_bar.close_label,
    Tabs = [begin
                Act = K =:= ActiveKey,
                Dirty = maps:get(dirty, M, false) =:= true,
                el('div',
                   [maybe_el(span, maps:get(icon, M, undefined), <<"ah-tab-bar__icon">>),
                    el(span, T, [<<"ah-tab-bar__label">>], []),
                    [el(span, [], [<<"ah-tab-bar__dot">>], [{aria_hidden, <<"true">>}]) || Dirty],
                    [el(button, {safe, close_icon()}, [<<"ah-tab-bar__close">>],
                        [{type, button}, {tabindex, -1},
                         {aria_label, [CloseLabel, <<" ">>, text_of(T)]}]) || Closable]],
                   [<<"ah-tab-bar__tab">>],
                   [{role, tab}, {data_id, K}, {data_active, tf(Act)},
                    {data_dirty, tf(Dirty)}, {aria_selected, tf(Act)},
                    {tabindex, case Act of true -> 0; false -> -1 end},
                    {title, text_of(T)}])
            end || {K, T, M} <- Norm],
    el('div', [Tabs, hidden(R#ah_tab_bar.name, ActiveKey)], classes(R),
       [[{role, tablist}, {data_ah, <<"tab-bar">>}, {data_ah_value, ActiveKey}],
        ?E:root_attrs(R, change)]);

render(#ah_breadcrumbs{items = Items, separator = Sep} = R) ->
    HasSep = not lists:member(Sep, [none, undefined, <<>>, ""]),
    ActiveLast = bool(active_last, R#ah_breadcrumbs.active_last),
    Shown = collapse([norm_crumb(I) || I <- Items], R#ah_breadcrumbs.max_items),
    N = length(Shown),
    Lis = lists:append(
            [[crumb(It, I, I =:= N - 1, ActiveLast),
              [el(li, [Sep || HasSep], [<<"ah-breadcrumbs__separator">>],
                  [{role, presentation}, {aria_hidden, <<"true">>}]) || I < N - 1]]
             || {I, It} <- lists:zip(lists:seq(0, N - 1), Shown)]),
    el(nav, el(ol, Lis, [<<"ah-breadcrumbs__list">>], []), classes(R),
       [[{aria_label, R#ah_breadcrumbs.label}, {data_has_separator, tf(HasSep)}],
        ?E:root_attrs(R, none)]);

render(#ah_pagination{total = Total, value = Page, href = Href} = R) ->
    Classes = classes(R),
    Size = max(1, R#ah_pagination.page_size),
    Pages = max(1, (Total + Size - 1) div Size),
    Cur = min(max(1, Page), Pages),
    Max = case R#ah_pagination.siblings of
              undefined -> R#ah_pagination.max_visible;
              S -> 2 * S + 5
          end,
    L = maps:merge(labels(), R#ah_pagination.labels),
    %% What the page list depends on besides the state; layout_basic.js
    %% reads it to re-render the list with the same template.
    Cfg = #{labels => maps:map(fun(_, V) -> bin(V) end,
                              maps:with([prev, next, first, last, page_info], L)),
            first_last => bool(show_first_last, R#ah_pagination.show_first_last),
            simple => R#ah_pagination.simple,
            href => case Href of undefined -> null; _ -> bin(Href) end},
    List = el(ul, aihtml_tpl:safe(tpl_pagination_items(pagination_view(Cur, Pages, Max, Size, Cfg))),
              [<<"ah-pagination-pages">>],
              [{data_view, iolist_to_binary(json:encode(Cfg))}]),
    Total_ = [el(span, fmt(maps:get(total, L), [Total]), [<<"ah-pagination-total">>], [])
              || bool(show_total, R#ah_pagination.show_total)],
    SizeSel = [el('div',
                  el(select, [el(option, fmt(maps:get(per_page, L), [Sz]), [],
                                 [{value, Sz}, {selected, Sz =:= Size}])
                              || Sz <- R#ah_pagination.page_sizes],
                     [<<"ah-pagination-size-select">>],
                     [{aria_label, maps:get(per_page_aria, L)}]),
                  [<<"ah-pagination-size-selector">>], [])
               || bool(show_size_selector, R#ah_pagination.show_size_selector)],
    Jumper = [el('div', [el(span, maps:get(goto, L), [], []),
                         aihtml_html:void(input, [<<"ah-pagination-jumper-input">>],
                                          [{type, text}, {inputmode, numeric},
                                           {aria_label, maps:get(goto, L)}]),
                         el(span, fmt(maps:get(goto_suffix, L), [Pages]), [],
                            [{data_template, maps:get(goto_suffix, L)}]),
                         el(button, maps:get(goto_confirm, L), [<<"ah-pagination-jumper-btn">>],
                            [{type, button}])],
                 [<<"ah-pagination-jumper">>], [])
              || bool(show_jumper, R#ah_pagination.show_jumper)],
    el('div', [Total_, List, SizeSel, Jumper, hidden(R#ah_pagination.name, Cur)],
       [Classes, [<<"ah-pagination-links">> || Href =/= undefined]],
       [[{role, navigation}, {aria_label, maps:get(aria_label, L)},
         {data_ah, <<"pagination">>}, {data_ah_value, Cur},
         {data_total, Total}, {data_page_size, Size}, {data_max_visible, Max},
         {data_href, Href}], ?E:root_attrs(R, change)]);

render(#ah_steps{items = Steps, value = Current} = R) ->
    Classes = classes(R),
    Norm = [norm_step(S) || S <- Steps],
    N = length(Norm),
    Cur = min(max(0, Current), max(0, N - 1)),
    Clickable = bool(clickable, R#ah_steps.clickable),
    HasContent = lists:any(fun(M) -> maps:get(content, M, undefined) =/= undefined end, Norm),
    ShowNav = case R#ah_steps.show_nav of
                  undefined -> HasContent;
                  B -> bool(show_nav, B)
              end,
    Indexed = lists:zip(lists:seq(0, N - 1), Norm),
    Items = [step_item(I, M, Cur, Clickable) || {I, M} <- Indexed],
    Panels = [el('div', [el('div', maps:get(content, M, []),
                            [<<"ah-steps-panel">>, [<<"ah-steps-panel-active">> || I =:= Cur]],
                            [{data_index, I}]) || {I, M} <- Indexed],
                 [<<"ah-steps-panels">>], []) || HasContent],
    Nav = [el('div', [step_btn(prev, R#ah_steps.prev_label, Clickable andalso Cur > 0),
                      step_btn(next, R#ah_steps.next_label, Clickable andalso Cur < N - 1)],
              [<<"ah-steps-nav">>], []) || ShowNav],
    el('div', [el('div', Items, [<<"ah-steps-header">>], []), Panels, Nav,
               hidden(R#ah_steps.name, Cur)],
       Classes,
       [[{data_ah, <<"steps">>}, {data_ah_value, Cur},
         {data_clickable, (not Clickable) andalso <<"false">>}], ?E:root_attrs(R, change)]);

render(#ah_skeleton{width = W, height = H} = R) ->
    Classes = classes(R),
    Variant = case R#ah_skeleton.variant of undefined -> text; V -> V end,
    Body = case Variant of
               text ->
                   N = max(1, R#ah_skeleton.lines),
                   [el(span, [], [<<"ah-skeleton__line">>],
                       [{style, style([{<<"width">>, case I of N -> <<"62%">>; _ -> <<"100%">> end},
                                       {<<"height">>, len(H)}])}])
                    || I <- lists:seq(1, N)];
               circle ->
                   D = case W of undefined -> 40; _ -> W end,
                   el(span, [], [<<"ah-skeleton__shape ah-skeleton__shape-circle">>],
                      [{style, style([{<<"width">>, len(D)},
                                      {<<"height">>, len(case H of undefined -> D; _ -> H end)}])}]);
               rect ->
                   el(span, [], [<<"ah-skeleton__shape">>],
                      [{style, style([{<<"width">>, len(case W of undefined -> <<"100%">>; _ -> W end)},
                                      {<<"height">>, len(case H of undefined -> 120; _ -> H end)},
                                      {<<"border-radius">>, len(R#ah_skeleton.radius)}])}])
           end,
    el('div', Body, Classes,
       [[{data_variant, Variant}, {data_animated, tf(not R#ah_skeleton.static)},
         {role, status}, {aria_busy, <<"true">>}, {aria_live, polite},
         {aria_label, R#ah_skeleton.label}], ?E:root_attrs(R, none)]);

render(#ah_loader{text = Text} = R) ->
    Classes = classes(R),
    el('div', [el('div', [], [<<"ah-loader-icon">>], [{aria_hidden, <<"true">>}]),
               [el('div', Text, [<<"ah-loader-text">>], []) || Text =/= <<>>]],
       Classes,
       [[{role, status}, {aria_live, polite}, {aria_busy, tf(not R#ah_loader.hidden)},
         {aria_label, text_of(Text)}, {data_ah, <<"loader">>},
         {data_modal, bool(modal, R#ah_loader.modal) andalso <<"true">>}],
        ?E:root_attrs(R, none)]);

render(#ah_empty{body = Children} = R) ->
    el('div', [maybe_el('div', R#ah_empty.icon, <<"ah-empty__icon">>, [{aria_hidden, <<"true">>}]),
               maybe_el('div', R#ah_empty.title, <<"ah-empty__title">>),
               maybe_el('div', R#ah_empty.description, <<"ah-empty__description">>),
               case Children of
                   [] -> [];
                   undefined -> [];
                   _ -> el('div', Children, [<<"ah-empty__content">>], [])
               end],
       classes(R), ?E:root_attrs(R, none)).

%%%===================================================================
%%% Component helpers
%%%===================================================================

norm_tab({K, L, P}) -> {bin(K), L, P, false};
norm_tab({K, L, P, M}) when is_map(M) -> {bin(K), L, P, maps:get(disabled, M, false) =:= true}.

active_key(undefined, KDs) ->
    case [K || {K, false} <- KDs] of
        [K | _] -> K;
        [] -> <<>>
    end;
active_key(Active, _) -> bin(Active).

norm_crumb(#{} = M) -> M;
norm_crumb({L, H}) -> #{label => L, href => H};
norm_crumb(L) -> #{label => L}.

collapse(Items, Max) when is_integer(Max), length(Items) > Max, length(Items) > 3 ->
    [hd(Items), ellipsis | lists:nthtail(length(Items) - 2, Items)];
collapse(Items, _) -> Items.

crumb(ellipsis, _, _, _) ->
    el(li, <<"\x{2026}"/utf8>>, [<<"ah-breadcrumbs__item ah-breadcrumbs__ellipsis">>],
       [{aria_hidden, <<"true">>}]);
crumb(M, I, Last, ActiveLast) ->
    Current = Last andalso not ActiveLast,
    Href = maps:get(href, M, undefined),
    Inner = [maybe_el(span, maps:get(icon, M, undefined), <<"ah-breadcrumbs__icon">>),
             maps:get(label, M, <<>>)],
    Body = case Href =/= undefined andalso not Current of
               true -> el(a, Inner, [<<"ah-breadcrumbs__link">>],
                          [[{href, Href}], maps:get(attrs, M, [])]);
               false -> el(span, Inner, [<<"ah-breadcrumbs__text">>], [])
           end,
    el(li, Body, [<<"ah-breadcrumbs__item">>],
       [{data_index, I}, {aria_current, Current andalso page}]).

%%%===================================================================
%%% Pagination
%%%===================================================================

%% @doc The page numbers to show, `gap' standing for an ellipsis. At most
%% `Max' slots (at least 5): the first and last pages always, the current
%% page and its neighbours in the middle.
-spec visible_pages(pos_integer(), pos_integer(), pos_integer()) -> [pos_integer() | gap].
visible_pages(Cur, Total, Max0) ->
    Max = max(5, Max0),
    if
        Total =< Max -> lists:seq(1, Total);
        Cur =< Max - 3 -> lists:seq(1, Max - 2) ++ [gap, Total];
        Cur >= Total - (Max - 4) -> [1, gap | lists:seq(Total - (Max - 3), Total)];
        true ->
            H = (Max - 5) div 2,
            [1, gap | lists:seq(Cur - H, Cur + H)] ++ [gap, Total]
    end.

labels() ->
    #{prev => <<"Previous">>, next => <<"Next">>, first => <<"First">>, last => <<"Last">>,
      per_page => <<"{0} / page">>, total => <<"Total {0}">>, goto => <<"Go to">>,
      goto_suffix => <<" / {0} pages">>, goto_confirm => <<"Go">>,
      page_info => <<"Page {0} / {1}">>, aria_label => <<"Pagination">>,
      per_page_aria => <<"Items per page">>}.

%% @doc The view data of templates/pagination_items.mustache: the entries
%% of the page list (first/prev navs, pages and gaps or the simple-mode
%% info, next/last navs). layout_basic.js has the same function (pgView).
-spec pagination_view(pos_integer(), pos_integer(), pos_integer(), pos_integer(), map()) ->
          #{entries := [map()]}.
pagination_view(Cur, Pages, Max, Size, #{labels := L, first_last := FL, simple := Simple,
                                         href := Href}) ->
    Link = fun(P) -> href(Href, P, Size) end,
    Nav = fun(Type, Disabled, Target) ->
                  Url = case Disabled of true -> null; false -> Link(Target) end,
                  entry(#{nav => true, type => atom_to_binary(Type), label => maps:get(Type, L),
                          icon => nav_icon(Type), first_last => Type =:= first orelse Type =:= last,
                          disabled => Disabled, tabindex => tabindex(not Disabled),
                          link => Url =/= null, href => url(Url)})
          end,
    Middle = case Simple of
                 true ->
                     [entry(#{info => true, text => fmt(maps:get(page_info, L), [Cur, Pages])})];
                 false ->
                     [case P of
                          gap -> entry(#{gap => true});
                          _ -> Url = Link(P),
                               entry(#{item => true, number => P, active => P =:= Cur,
                                       tabindex => tabindex(P =/= Cur),
                                       link => Url =/= null, href => url(Url)})
                      end || P <- visible_pages(Cur, Pages, Max)]
             end,
    FirstLast = FL andalso not Simple,
    #{entries => [Nav(first, Cur =:= 1, 1) || FirstLast]
                 ++ [Nav(prev, Cur =:= 1, Cur - 1)] ++ Middle ++ [Nav(next, Cur =:= Pages, Cur + 1)]
                 ++ [Nav(last, Cur =:= Pages, Pages) || FirstLast]}.

%% Every entry carries every key, so a section never looks a key up in
%% the enclosing context.
entry(M) ->
    maps:merge(#{gap => false, info => false, item => false, nav => false, link => false,
                 active => false, disabled => false, first_last => false, href => <<>>,
                 number => 0, tabindex => 0, type => <<>>, label => <<>>, icon => <<>>,
                 text => <<>>}, M).

tabindex(true) -> 0;
tabindex(false) -> -1.

url(null) -> <<>>;
url(U) -> U.

nav_icon(prev) -> <<"\x{2039}"/utf8>>;
nav_icon(next) -> <<"\x{203A}"/utf8>>;
nav_icon(first) -> <<"\x{00AB}"/utf8>>;
nav_icon(last) -> <<"\x{00BB}"/utf8>>.

href(null, _, _) -> null;
href(undefined, _, _) -> null;
href(T, P, Size) ->
    B = bin(T),
    B1 = binary:replace(B, <<"{page}">>, integer_to_binary(P), [global]),
    binary:replace(B1, <<"{size}">>, integer_to_binary(Size), [global]).

fmt(T, Args) ->
    {Out, _} = lists:foldl(fun(A, {Acc, I}) ->
                                   {binary:replace(Acc, <<"{", (integer_to_binary(I))/binary, "}">>,
                                                   bin(A), [global]), I + 1}
                           end, {bin(T), 0}, Args),
    Out.

%%%===================================================================
%%% Steps
%%%===================================================================

norm_step(#{} = M) -> M;
norm_step({T, D}) -> #{title => T, description => D};
norm_step(T) -> #{title => T}.

step_status(I, M, Cur) ->
    case {maps:get(status, M, undefined), maps:get(disabled, M, false)} of
        {undefined, true} -> disabled;
        {undefined, _} when I < Cur -> completed;
        {undefined, _} when I =:= Cur -> active;
        {undefined, _} -> pending;
        {S, _} -> binary_to_existing_atom(bin(S), utf8)
    end.

step_item(I, M, Cur, Clickable) ->
    Status = step_status(I, M, Cur),
    Click = Clickable andalso Status =/= disabled,
    Indicator = aihtml_tpl:safe(tpl_steps_indicator(
                                  #{check => Status =:= completed, error => Status =:= error,
                                    plain => Status =/= completed andalso Status =/= error,
                                    number => I + 1})),
    el('div', [el('div', Indicator, [<<"ah-steps-indicator">>], [{aria_hidden, <<"true">>}]),
               el('div', [], [<<"ah-steps-connector">>,
                              [<<"ah-steps-connector-done">> || Status =:= completed]], []),
               el('div', [el('div', maps:get(title, M, <<>>), [<<"ah-steps-title">>], []),
                          maybe_el('div', maps:get(description, M, undefined),
                                   <<"ah-steps-description">>)],
                  [<<"ah-steps-content">>], [])],
       [<<"ah-steps-item">>, <<"ah-steps-item-", (atom_to_binary(Status, utf8))/binary>>,
        [<<"ah-steps-item-clickable">> || Click], [<<"ah-steps-item-selected">> || I =:= Cur]],
       [{data_index, I}, {role, Click andalso button},
        {tabindex, Click andalso case I =:= Cur of true -> 0; false -> -1 end},
        {aria_current, I =:= Cur andalso step},
        {aria_disabled, Status =:= disabled andalso <<"true">>}]).

step_btn(Action, Label, Enabled) ->
    el(button, Label, [<<"ah-steps-btn">>, [<<"ah-steps-btn-disabled">> || not Enabled]],
       [{type, button}, {data_action, Action}, {disabled, not Enabled}]).


%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => card, category => layout,
       option_docs => #{title => <<"Header title.">>, subtitle => <<"Muted line under the title.">>,
                        extra => <<"Html on the right of the header (actions).">>,
                        header => <<"Html replacing the whole header.">>,
                        media => <<"Html shown full-bleed above the header (image, banner).">>,
                        footer => <<"Html of the footer strip.">>,
                        hover => <<"Lift and deepen the shadow on hover.">>,
                        flush => <<"No padding around the body.">>},
       methods => [],
       signature => <<"card(Children, Css, Attrs)">>, root => <<"ah-card">>,
       flags => [hover, flush],
       options => [title, subtitle, extra, header, media, footer],
       doc => <<"A container with optional media, header (title, subtitle, extra) and footer.">>},
     #{name => panel, category => layout,
       option_docs => #{title => <<"Header title; the panel becomes a labelled region.">>,
                        actions => <<"Html on the right of the header.">>,
                        collapsible => <<"Show a toggle that collapses the scroll area.">>,
                        collapsed => <<"Start collapsed (with collapsible).">>,
                        height => <<"Height of the scroll area (integer px or CSS length).">>,
                        max_height => <<"Maximum height of the scroll area.">>,
                        toggle_label => <<"Accessible label of the toggle (default Toggle).">>,
                        bordered => <<"Frame the panel with a border and paper background.">>},
       methods => [#{name => scrollTo, args => <<"(X, Y)">>, doc => <<"Scroll the content to X, Y pixels.">>},
                   #{name => collapse, args => <<"()">>, doc => <<"Collapse the scroll area.">>},
                   #{name => expand, args => <<"()">>, doc => <<"Expand the scroll area.">>},
                   #{name => toggle, args => <<"()">>, doc => <<"Collapse or expand.">>}],
       signature => <<"panel(Children, Css, Attrs)">>, root => <<"ah-panel">>,
       flags => [bordered],
       options => [title, actions, collapsible, collapsed, height, max_height, toggle_label],
       behavior => <<"panel">>, events => [<<"ah:collapse">>, <<"ah:expand">>],
       doc => <<"A scrollable content container with an optional header, actions and "
                "collapse toggle. Methods: scrollTo(x, y), collapse, expand, toggle.">>},
     #{name => expander, category => layout,
       option_docs => #{header => <<"Header html, or #{title, subheader, extra} for a structured header.">>,
                        actions => <<"Html of an action strip under the content, collapsing with it.">>,
                        expanded => <<"Initial state (default true).">>,
                        toggle_mode => <<"click (default), dblclick or none.">>,
                        animation => <<"slide (default), fade or none.">>,
                        duration => <<"Animation time in ms (default 250).">>,
                        show_arrow => <<"Show the arrow (default true).">>,
                        arrow_position => <<"right (default) or left.">>,
                        expand_icon => <<"Arrow html; with collapse_icon the two icons swap.">>,
                        collapse_icon => <<"Icon shown when expanded (with expand_icon).">>,
                        accordion => <<"A name: opening one expander closes the others with that name.">>,
                        name => <<"Submit the state as a hidden input.">>,
                        top => <<"Header above the content (default).">>,
                        bottom => <<"Header below the content.">>,
                        square => <<"No rounded corners.">>,
                        no_gutters => <<"No frame or side padding.">>,
                        disabled => <<"Ignore clicks and keys.">>},
       methods => [#{name => open, args => <<"()">>, doc => <<"Expand without firing change.">>},
                   #{name => close, args => <<"()">>, doc => <<"Collapse without firing change.">>},
                   #{name => toggle, args => <<"()">>, doc => <<"Flip the state without firing change.">>}],
       signature => <<"expander(Children, Css, Attrs)">>, root => <<"ah-expander">>,
       groups => #{position => {[top, bottom], top}},
       flags => [square, no_gutters, disabled],
       classes => #{no_gutters => [<<"ah-expander-no-gutters">>]},
       options => [header, actions, expanded, toggle_mode, animation, duration,
                   show_arrow, arrow_position, expand_icon, collapse_icon, accordion, name],
       behavior => <<"expander">>,
       events => [<<"change">>, <<"ah:expanded">>, <<"ah:collapsed">>],
       doc => <<"A collapsible section; value \"true\" or \"false\". "
                "Methods: open, close, toggle.">>},
     #{name => tabs, category => layout,
       option_docs => #{animation => <<"fade (default) or none when switching panels.">>,
                        selection_mode => <<"click (default) or hover.">>,
                        scrollable => <<"Scroll buttons for a header wider than the tabs.">>,
                        name => <<"Submit the active key as a hidden input.">>,
                        top => <<"Tabs above the panels (default).">>,
                        bottom => <<"Tabs below the panels.">>,
                        left => <<"Tabs on the left, arrow keys up and down.">>,
                        right => <<"Tabs on the right.">>,
                        disabled => <<"Ignore clicks.">>},
       methods => [#{name => select, args => <<"(Key)">>, doc => <<"Show a tab without firing change.">>},
                   #{name => enable, args => <<"(Key)">>, doc => <<"Enable a tab.">>},
                   #{name => disable, args => <<"(Key)">>, doc => <<"Disable a tab.">>}],
       signature => <<"tabs(Tabs, Active, Css, Attrs)">>, root => <<"ah-tabs">>,
       groups => #{position => {[top, bottom, left, right], top}},
       flags => [disabled],
       options => [animation, selection_mode, scrollable, name],
       behavior => <<"tabs">>, events => [<<"change">>],
       doc => <<"Tabbed panels, all rendered by the server; value is the active key. "
                "Methods: select(key), enable(key), disable(key).">>},
     #{name => tab_bar, category => layout,
       option_docs => #{closable => <<"Close buttons on the tabs (default true); Delete closes the focused tab.">>,
                        close_label => <<"Accessible label prefix of the close buttons (default close).">>,
                        name => <<"Submit the active id as a hidden input.">>},
       methods => [#{name => select, args => <<"(Id)">>, doc => <<"Activate a tab without firing change.">>},
                   #{name => close, args => <<"(Id)">>, doc => <<"Remove a tab; the neighbour becomes active if it was.">>}],
       signature => <<"tab_bar(Items, Active, Css, Attrs)">>, root => <<"ah-tab-bar">>,
       options => [closable, close_label, name],
       behavior => <<"tab-bar">>, events => [<<"change">>, <<"ah:close">>],
       doc => <<"An editor-style strip of closable tabs; value is the active id. "
                "Methods: select(id), close(id).">>},
     #{name => breadcrumbs, category => layout,
       option_docs => #{separator => <<"Separator text (default /); none for dots.">>,
                        active_last => <<"Keep the last item a link.">>,
                        max_items => <<"Collapse the middle to an ellipsis beyond this many items.">>,
                        label => <<"aria-label of the nav (default breadcrumb).">>},
       methods => [],
       signature => <<"breadcrumbs(Items, Css, Attrs)">>, root => <<"ah-breadcrumbs">>,
       options => [separator, active_last, max_items, label],
       doc => <<"An ancestor path; the last item is the current page.">>},
     #{name => pagination, category => layout,
       option_docs => #{page_size => <<"Items per page (default 10).">>,
                        page_sizes => <<"Choices of the page-size select (default [10,20,50,100]).">>,
                        show_size_selector => <<"Show the page-size select (default true).">>,
                        show_jumper => <<"Show a go-to-page input.">>,
                        show_first_last => <<"Show first and last buttons.">>,
                        show_total => <<"Show the item count.">>,
                        max_visible => <<"Slots for page numbers and gaps (default 7).">>,
                        siblings => <<"Pages on each side of the current one (instead of max_visible).">>,
                        href => <<"Link template with {page} and {size}: pages become links, no script needed.">>,
                        labels => <<"Map overriding prev, next, first, last, per_page, total, goto, goto_suffix, goto_confirm, page_info, aria_label, per_page_aria.">>,
                        name => <<"Submit the page as a hidden input.">>,
                        simple => <<"Previous, Page x / y, Next only.">>,
                        disabled => <<"Dim and ignore clicks.">>},
       methods => [#{name => setPage, args => <<"(N)">>, doc => <<"Go to page N without firing change.">>},
                   #{name => next, args => <<"()">>, doc => <<"Next page.">>},
                   #{name => prev, args => <<"()">>, doc => <<"Previous page.">>},
                   #{name => first, args => <<"()">>, doc => <<"First page.">>},
                   #{name => last, args => <<"()">>, doc => <<"Last page.">>},
                   #{name => setTotal, args => <<"(Items)">>, doc => <<"Change the item count; the page is clamped.">>},
                   #{name => setPageSize, args => <<"(N)">>, doc => <<"Change the page size.">>}],
       signature => <<"pagination(Total, Page, Css, Attrs)">>, root => <<"ah-pagination">>,
       flags => [simple, disabled],
       options => [page_size, page_sizes, show_size_selector, show_jumper, show_first_last,
                   show_total, max_visible, siblings, href, labels, name],
       behavior => <<"pagination">>, events => [<<"change">>],
       doc => <<"Page navigation; value is the current page, data-page-size the page size. "
                "Methods: setPage(n), next, prev, first, last, setTotal(n), setPageSize(n).">>},
     #{name => steps, category => layout,
       option_docs => #{clickable => <<"Steps can be clicked and reached by arrow keys (default true).">>,
                        show_nav => <<"Previous and next buttons (default: when a step has content).">>,
                        prev_label => <<"Text of the previous button.">>,
                        next_label => <<"Text of the next button.">>,
                        name => <<"Submit the step index as a hidden input.">>,
                        horizontal => <<"Steps in a row (default).">>,
                        vertical => <<"Steps in a column, panels beside them.">>,
                        disabled => <<"Dim and ignore clicks.">>},
       methods => [#{name => select, args => <<"(Index)">>, doc => <<"Go to a step without firing change.">>},
                   #{name => next, args => <<"()">>, doc => <<"Next enabled step.">>},
                   #{name => prev, args => <<"()">>, doc => <<"Previous enabled step.">>},
                   #{name => first, args => <<"()">>, doc => <<"First step.">>},
                   #{name => last, args => <<"()">>, doc => <<"Last step.">>},
                   #{name => setStatus, args => <<"(Index, Status)">>, doc => <<"Set completed, active, error, disabled or pending.">>}],
       signature => <<"steps(Steps, Current, Css, Attrs)">>, root => <<"ah-steps">>,
       groups => #{orientation => {[horizontal, vertical], horizontal}},
       flags => [disabled],
       options => [clickable, show_nav, prev_label, next_label, name],
       behavior => <<"steps">>, events => [<<"change">>],
       doc => <<"A step indicator with optional panels; value is the 0-based step. "
                "Methods: select(i), next, prev, first, last, setStatus(i, status).">>},
     #{name => skeleton, category => layout,
       option_docs => #{lines => <<"Lines of the text variant (default 3, the last one shorter).">>,
                        width => <<"Width (circle: diameter), integer px or CSS length.">>,
                        height => <<"Height of lines or shape.">>,
                        radius => <<"Corner radius of the rect variant.">>,
                        label => <<"aria-label (default Loading).">>,
                        text => <<"Lines of text (default).">>,
                        circle => <<"A circle, e.g. for an avatar.">>,
                        rect => <<"A rectangle, e.g. for an image.">>,
                        static => <<"No shimmer.">>,
                        done => <<"Hidden: loading has finished.">>},
       methods => [],
       signature => <<"skeleton(Css, Attrs)">>, root => <<"ah-skeleton">>,
       groups => #{variant => {[text, circle, rect], none}},
       flags => [static, done],
       classes => #{text => [], circle => [], rect => [], static => [],
                    done => [<<"ah-skeleton--done">>]},
       options => [lines, width, height, radius, label],
       doc => <<"A shimmering placeholder while content loads.">>},
     #{name => loader, category => layout,
       option_docs => #{text => <<"Text next to the spinner (default Loading...; <<>> for none).">>,
                        modal => <<"Dim the page while shown; Esc hides it.">>,
                        bottom => <<"Text under the spinner (default).">>,
                        top => <<"Text above the spinner.">>,
                        left => <<"Text left of the spinner.">>,
                        right => <<"Text right of the spinner.">>,
                        hidden => <<"Render hidden; show it with the show method.">>,
                        inline => <<"In the flow instead of covering the positioned parent.">>,
                        center => <<"A box fixed in the middle of the viewport.">>,
                        disabled => <<"Dimmed.">>},
       methods => [#{name => show, args => <<"([Left, Top])">>, doc => <<"Show, optionally at a position in px.">>},
                   #{name => hide, args => <<"()">>, doc => <<"Hide.">>},
                   #{name => toggle, args => <<"()">>, doc => <<"Show or hide.">>},
                   #{name => text, args => <<"(Text)">>, doc => <<"Change the text.">>}],
       signature => <<"loader(Css, Attrs)">>, root => <<"ah-loader">>,
       groups => #{text_position => {[bottom, top, left, right], bottom}},
       flags => [hidden, inline, center, disabled],
       classes => #{bottom => [<<"ah-loader-text-bottom">>], top => [<<"ah-loader-text-top">>],
                    left => [<<"ah-loader-text-left">>], right => [<<"ah-loader-text-right">>]},
       options => [text, modal],
       behavior => <<"loader">>,
       doc => <<"A spinner, by default an overlay over its positioned parent. "
                "Methods: show([left, top]), hide, toggle, text(t).">>},
     #{name => empty, category => layout,
       option_docs => #{icon => <<"Icon html (an emoji or {safe, Svg}).">>,
                        title => <<"Title.">>,
                        description => <<"Explanation under the title.">>,
                        compact => <<"Less padding.">>},
       methods => [],
       signature => <<"empty(Children, Css, Attrs)">>, root => <<"ah-empty">>,
       flags => [compact],
       options => [icon, title, description],
       doc => <<"An empty-state placeholder: icon, title, description and actions.">>}].

%%%===================================================================
%%% Internal
%%%===================================================================

el(Tag, Children, Css, Attrs) -> aihtml_html:el(Tag, Children, Css, Attrs).

maybe_el(Tag, X, Class) -> maybe_el(Tag, X, Class, []).

maybe_el(_Tag, undefined, _Class, _Attrs) -> [];
maybe_el(Tag, X, Class, Attrs) -> el(Tag, X, [Class], Attrs).

cat_entry(Name) -> aihtml_catalog:entry(?MODULE, Name).

classes(R) ->
    Tag = element(1, R),
    ?E:classes(R, fields(Tag), cat_entry(?E:component_name(Tag))).

%% The root id as a binary and the record carrying it: the `id' field, an
%% id among the attrs, or a generated Prefix-N.
with_id(R, Prefix) ->
    Id = case element(3, R) of
             undefined ->
                 case lists:keyfind(<<"id">>, 1, aihtml_html:attrs(element(5, R))) of
                     {_, V} when is_binary(V) -> V;
                     _ -> <<Prefix/binary, "-",
                            (integer_to_binary(erlang:unique_integer([positive])))/binary>>
                 end;
             V -> bin(V)
         end,
    {Id, setelement(3, R, Id)}.

bool(Field, V) -> one_of(Field, V, [true, false]).

one_of(Field, V, Allowed) ->
    lists:member(V, Allowed) orelse error({aihtml, {bad_option, Field, V}}),
    V.

hidden(undefined, _) -> [];
hidden(Name, Value) ->
    aihtml_html:void(input, [], [{type, hidden}, {name, Name}, {value, Value}]).

tf(true) -> <<"true">>;
tf(false) -> <<"false">>.

bin(B) when is_binary(B) -> B;
bin(A) when is_atom(A) -> atom_to_binary(A, utf8);
bin(I) when is_integer(I) -> integer_to_binary(I);
bin(L) when is_list(L) -> unicode:characters_to_binary(L).

%% Plain text of a label, for title/aria attributes (<<>> for markup).
text_of(B) when is_binary(B) -> B;
text_of(A) when is_atom(A) -> atom_to_binary(A, utf8);
text_of(L) when is_list(L) ->
    case io_lib:printable_unicode_list(L) of
        true -> unicode:characters_to_binary(L);
        false -> iolist_to_binary([text_of(X) || X <- L])
    end;
text_of(I) when is_integer(I) -> integer_to_binary(I);
text_of(_) -> <<>>.

len(undefined) -> false;
len(N) when is_integer(N) -> <<(integer_to_binary(N))/binary, "px">>;
len(V) -> bin(V).

style(Decls) ->
    case [[K, $:, V, $;] || {K, V} <- Decls, V =/= false, V =/= undefined] of
        [] -> undefined;
        S -> iolist_to_binary(S)
    end.

chevron() ->
    <<"<svg viewBox=\"0 0 24 24\" width=\"16\" height=\"16\" fill=\"none\" stroke=\"currentColor\" "
      "stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\" aria-hidden=\"true\">"
      "<polyline points=\"6 9 12 15 18 9\"/></svg>">>.

close_icon() ->
    <<"<svg viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" "
      "stroke-linecap=\"round\" aria-hidden=\"true\"><line x1=\"18\" y1=\"6\" x2=\"6\" y2=\"18\"/>"
      "<line x1=\"6\" y1=\"6\" x2=\"18\" y2=\"18\"/></svg>">>.
