%%%-------------------------------------------------------------------
%%% @doc aihtml: write HTML pages as Erlang function calls.
%%%
%%% ```
%%% -include_lib("aihtml/include/aihtml.hrl").   % imports this module
%%%
%%% page() ->
%%%     ah_div([ah_checkbox(<<"Remember me">>, yes, [], [{name, remember}]),
%%%             ah_button(<<"Save">>, save, [primary, <<"mt-4">>], [{type, submit}])],
%%%            [<<"flex flex-col gap-2">>], [{id, login}]).
%%% '''
%%%
%%% Every element builder is named like its record with the `ah_' prefix:
%%% `ah_button/4' builds `#ah_button{}', `ah_div/3' and the other tags
%%% build `#ah_el{}' (designs/05-records.md).
%%%
%%% Generic tags take `(Children)' or `(Children, Css, Attrs)'. Void tags
%%% take `(Css, Attrs)'. Prefabs are listed in `aihtml_catalog:prefabs/0'.
%%% Everything returns an element; `render/1' turns it into iodata.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml).

%% The builders of migrated groups return element records; their specs
%% (generated below) name them.
-include("aihtml_records.hrl").

%% Tags with children, /1 and /3.
-export([ah_div/1, ah_div/3, ah_span/1, ah_span/3, ah_p/1, ah_p/3, ah_a/1, ah_a/3,
         ah_h1/1, ah_h1/3, ah_h2/1, ah_h2/3, ah_h3/1, ah_h3/3, ah_h4/1, ah_h4/3,
         ah_ul/1, ah_ul/3, ah_ol/1, ah_ol/3, ah_li/1, ah_li/3,
         ah_dl/1, ah_dl/3, ah_dt/1, ah_dt/3, ah_dd/1, ah_dd/3,
         ah_section/1, ah_section/3, ah_article/1, ah_article/3, ah_aside/1, ah_aside/3,
         ah_header/1, ah_header/3, ah_footer/1, ah_footer/3,
         ah_nav/1, ah_nav/3, ah_main/1, ah_main/3,
         ah_form/1, ah_form/3, ah_fieldset/1, ah_fieldset/3,
         ah_legend/1, ah_legend/3, ah_label/1, ah_label/3,
         ah_strong/1, ah_strong/3, ah_em/1, ah_em/3, ah_small/1, ah_small/3,
         ah_code/1, ah_code/3, ah_pre/1, ah_pre/3, ah_blockquote/1, ah_blockquote/3,
         ah_table/1, ah_table/3, ah_thead/1, ah_thead/3, ah_tbody/1, ah_tbody/3,
         ah_tr/1, ah_tr/3, ah_th/1, ah_th/3, ah_td/1, ah_td/3]).
%% Void tags.
-export([ah_br/0, ah_hr/2, ah_img/2]).
%% Escape hatches and rendering.
-export([ah_el/4, ah_void/3, text/1, safe/1, render/1, render_binary/1, page/2]).
%% Server round trips for the browser runtime.
-export([fetch/3, fetch/4]).
%% Browser events that call Erlang actions (see aihtml_action).
-export([on/2, on/3, on_client/2, preserve/0]).
%% Server push (see aihtml_push).
-export([subscribe/1, subscribe/2]).
%% Theme switcher; the component exports are generated below.
-export([ah_theme_switcher/2]).
%% BEGIN GENERATED EXPORTS
-export([ah_button/4,
         ah_link_button/4,
         ah_toggle_button/4,
         ah_button_group/4,
         ah_segmented_control/4,
         ah_dropdown_button/4,
         ah_split_button/4,
         ah_checkbox/4,
         ah_radiobutton/4,
         ah_switch_button/4,
         ah_checkbox_group/4,
         ah_radiobutton_group/4,
         ah_radio_cards/4,
         ah_rating_group/4,
         ah_input/3,
         ah_textarea/3,
         ah_password_input/3,
         ah_number_input/3,
         ah_input_otp/4,
         ah_tag_input/3,
         ah_markdown_editor/3,
         ah_markdown_view/3,
         ah_dropdownlist/4,
         ah_select/4,
         ah_slider/4,
         ah_field/4,
         validate/1,
         ah_form_layout/4,
         ah_datepicker/3,
         ah_combobox/4,
         set_items/3,
         set_items/4,
         ah_timepicker/3,
         ah_colorpicker/3,
         ah_calendar/3,
         set_events/3,
         add_event/3,
         ah_datetime_input/3,
         ah_cascader/4,
         cascader_children/3,
         cascader_children/4,
         ah_listbox/4,
         listbox_items/3,
         listbox_items/4,
         ah_transfer/4,
         ah_masked_input/3,
         ah_formatted_input/3,
         ah_range_selector/4,
         ah_repeat_button/4,
         ah_upload/3,
         uploaded_files/1,
         ah_card/3,
         ah_panel/3,
         ah_expander/3,
         ah_tabs/4,
         ah_tab_bar/4,
         ah_breadcrumbs/3,
         ah_pagination/4,
         ah_steps/4,
         ah_skeleton/2,
         ah_loader/2,
         ah_empty/3,
         ah_menu/4,
         ah_navbar/4,
         ah_sidenav/4,
         ah_toolbar/3,
         ah_splitter/3,
         ah_listmenu/4,
         ah_status_bar/3,
         ah_scrollview/3,
         ah_scrollbar/3,
         ah_responsive_panel/3,
         ah_activity_bar/4,
         ah_navigationbar/4,
         ah_command/3,
         set_command_items/3,
         ah_sortable/4,
         ah_dragdrop/3,
         draggable_attrs/2,
         drop_zone_attrs/2,
         ah_docking/3,
         docking_add_window/4,
         ah_dock_layout/3,
         dock_layout_open/3,
         dock_layout_open/4,
         ah_ribbon/4,
         ah_tile_layout/4,
         ah_tooltip/4,
         tooltip_attrs/2,
         ah_popover/3,
         ah_drawer/3,
         ah_sheet/3,
         shows_toast/2,
         ah_toast/3,
         ah_notification/3,
         ah_window/3,
         ah_avatar/3,
         ah_badge/3,
         ah_chip/3,
         ah_aspect_ratio/3,
         ah_kbd/3,
         ah_time_ago/3,
         ah_expandable_text/3,
         ah_alert/3,
         ah_progressbar/3,
         ah_progress_circle/3,
         ah_meter/3,
         ah_statistic/3,
         ah_kpi_card/3,
         ah_timeline/3,
         ah_ranking_list/3,
         ah_tag_cloud/3,
         ah_tree/4,
         set_children/3,
         ah_nav_tree/4,
         ah_diff/4,
         ah_heatmap_calendar/3,
         ah_datagrid/4,
         datagrid_query/1,
         datagrid_rows/4,
         datagrid_row/3,
         datagrid_select/2,
         ah_pivotgrid/4,
         pivotgrid_rows/3,
         pivotgrid_view/1,
         pivotgrid_cell/1,
         ah_treegrid/4,
         treegrid_children/3,
         ah_datatable/4,
         datatable_query/1,
         datatable_rows/3,
         datatable_row/4,
         ah_gantt/3,
         gantt_update/3,
         ah_scheduler/4,
         scheduler_range/1,
         scheduler_update/3,
         ah_swimlane/3,
         swimlane_update/3,
         ah_chart/3,
         chart_option/1,
         chart_update/3,
         ah_area_chart/3,
         ah_bar_chart/3,
         ah_donut_chart/3,
         ah_radar_chart/3,
         ah_relation_graph/3,
         ah_node_graph/3,
         node_graph_layout/1,
         set_node_graph/3,
         opens/1,
         toggles/1,
         closes/0,
         closes/1,
         closes/2]).
%% END GENERATED EXPORTS

-export_type([html/0, element/0, css/0, attrs/0]).

-type html() :: aihtml_html:html().
-type element() :: aihtml_html:element().
-type css() :: aihtml_html:css().
-type attrs() :: aihtml_html:attrs().

%% One /1 and one /3 function per tag (a macro cannot define functions).
-spec ah_div(html()) -> element().
ah_div(C) -> aihtml_html:el('div', C, [], []).
-spec ah_div(html(), css(), attrs()) -> element().
ah_div(C, Css, A) -> aihtml_html:el('div', C, Css, A).

-spec ah_span(html()) -> element().
ah_span(C) -> aihtml_html:el(span, C, [], []).
-spec ah_span(html(), css(), attrs()) -> element().
ah_span(C, Css, A) -> aihtml_html:el(span, C, Css, A).

-spec ah_p(html()) -> element().
ah_p(C) -> aihtml_html:el(p, C, [], []).
-spec ah_p(html(), css(), attrs()) -> element().
ah_p(C, Css, A) -> aihtml_html:el(p, C, Css, A).

-spec ah_a(html()) -> element().
ah_a(C) -> aihtml_html:el(a, C, [], []).
-spec ah_a(html(), css(), attrs()) -> element().
ah_a(C, Css, A) -> aihtml_html:el(a, C, Css, A).

-spec ah_h1(html()) -> element().
ah_h1(C) -> aihtml_html:el(h1, C, [], []).
-spec ah_h1(html(), css(), attrs()) -> element().
ah_h1(C, Css, A) -> aihtml_html:el(h1, C, Css, A).

-spec ah_h2(html()) -> element().
ah_h2(C) -> aihtml_html:el(h2, C, [], []).
-spec ah_h2(html(), css(), attrs()) -> element().
ah_h2(C, Css, A) -> aihtml_html:el(h2, C, Css, A).

-spec ah_h3(html()) -> element().
ah_h3(C) -> aihtml_html:el(h3, C, [], []).
-spec ah_h3(html(), css(), attrs()) -> element().
ah_h3(C, Css, A) -> aihtml_html:el(h3, C, Css, A).

-spec ah_h4(html()) -> element().
ah_h4(C) -> aihtml_html:el(h4, C, [], []).
-spec ah_h4(html(), css(), attrs()) -> element().
ah_h4(C, Css, A) -> aihtml_html:el(h4, C, Css, A).

-spec ah_ul(html()) -> element().
ah_ul(C) -> aihtml_html:el(ul, C, [], []).
-spec ah_ul(html(), css(), attrs()) -> element().
ah_ul(C, Css, A) -> aihtml_html:el(ul, C, Css, A).

-spec ah_ol(html()) -> element().
ah_ol(C) -> aihtml_html:el(ol, C, [], []).
-spec ah_ol(html(), css(), attrs()) -> element().
ah_ol(C, Css, A) -> aihtml_html:el(ol, C, Css, A).

-spec ah_li(html()) -> element().
ah_li(C) -> aihtml_html:el(li, C, [], []).
-spec ah_li(html(), css(), attrs()) -> element().
ah_li(C, Css, A) -> aihtml_html:el(li, C, Css, A).

-spec ah_dl(html()) -> element().
ah_dl(C) -> aihtml_html:el(dl, C, [], []).
-spec ah_dl(html(), css(), attrs()) -> element().
ah_dl(C, Css, A) -> aihtml_html:el(dl, C, Css, A).

-spec ah_dt(html()) -> element().
ah_dt(C) -> aihtml_html:el(dt, C, [], []).
-spec ah_dt(html(), css(), attrs()) -> element().
ah_dt(C, Css, A) -> aihtml_html:el(dt, C, Css, A).

-spec ah_dd(html()) -> element().
ah_dd(C) -> aihtml_html:el(dd, C, [], []).
-spec ah_dd(html(), css(), attrs()) -> element().
ah_dd(C, Css, A) -> aihtml_html:el(dd, C, Css, A).

-spec ah_section(html()) -> element().
ah_section(C) -> aihtml_html:el(section, C, [], []).
-spec ah_section(html(), css(), attrs()) -> element().
ah_section(C, Css, A) -> aihtml_html:el(section, C, Css, A).

-spec ah_article(html()) -> element().
ah_article(C) -> aihtml_html:el(article, C, [], []).
-spec ah_article(html(), css(), attrs()) -> element().
ah_article(C, Css, A) -> aihtml_html:el(article, C, Css, A).

-spec ah_aside(html()) -> element().
ah_aside(C) -> aihtml_html:el(aside, C, [], []).
-spec ah_aside(html(), css(), attrs()) -> element().
ah_aside(C, Css, A) -> aihtml_html:el(aside, C, Css, A).

-spec ah_header(html()) -> element().
ah_header(C) -> aihtml_html:el(header, C, [], []).
-spec ah_header(html(), css(), attrs()) -> element().
ah_header(C, Css, A) -> aihtml_html:el(header, C, Css, A).

-spec ah_footer(html()) -> element().
ah_footer(C) -> aihtml_html:el(footer, C, [], []).
-spec ah_footer(html(), css(), attrs()) -> element().
ah_footer(C, Css, A) -> aihtml_html:el(footer, C, Css, A).

-spec ah_nav(html()) -> element().
ah_nav(C) -> aihtml_html:el(nav, C, [], []).
-spec ah_nav(html(), css(), attrs()) -> element().
ah_nav(C, Css, A) -> aihtml_html:el(nav, C, Css, A).

-spec ah_main(html()) -> element().
ah_main(C) -> aihtml_html:el(main, C, [], []).
-spec ah_main(html(), css(), attrs()) -> element().
ah_main(C, Css, A) -> aihtml_html:el(main, C, Css, A).

-spec ah_form(html()) -> element().
ah_form(C) -> aihtml_html:el(form, C, [], []).
-spec ah_form(html(), css(), attrs()) -> element().
ah_form(C, Css, A) -> aihtml_html:el(form, C, Css, A).

-spec ah_fieldset(html()) -> element().
ah_fieldset(C) -> aihtml_html:el(fieldset, C, [], []).
-spec ah_fieldset(html(), css(), attrs()) -> element().
ah_fieldset(C, Css, A) -> aihtml_html:el(fieldset, C, Css, A).

-spec ah_legend(html()) -> element().
ah_legend(C) -> aihtml_html:el(legend, C, [], []).
-spec ah_legend(html(), css(), attrs()) -> element().
ah_legend(C, Css, A) -> aihtml_html:el(legend, C, Css, A).

-spec ah_label(html()) -> element().
ah_label(C) -> aihtml_html:el(label, C, [], []).
-spec ah_label(html(), css(), attrs()) -> element().
ah_label(C, Css, A) -> aihtml_html:el(label, C, Css, A).

-spec ah_strong(html()) -> element().
ah_strong(C) -> aihtml_html:el(strong, C, [], []).
-spec ah_strong(html(), css(), attrs()) -> element().
ah_strong(C, Css, A) -> aihtml_html:el(strong, C, Css, A).

-spec ah_em(html()) -> element().
ah_em(C) -> aihtml_html:el(em, C, [], []).
-spec ah_em(html(), css(), attrs()) -> element().
ah_em(C, Css, A) -> aihtml_html:el(em, C, Css, A).

-spec ah_small(html()) -> element().
ah_small(C) -> aihtml_html:el(small, C, [], []).
-spec ah_small(html(), css(), attrs()) -> element().
ah_small(C, Css, A) -> aihtml_html:el(small, C, Css, A).

-spec ah_code(html()) -> element().
ah_code(C) -> aihtml_html:el(code, C, [], []).
-spec ah_code(html(), css(), attrs()) -> element().
ah_code(C, Css, A) -> aihtml_html:el(code, C, Css, A).

-spec ah_pre(html()) -> element().
ah_pre(C) -> aihtml_html:el(pre, C, [], []).
-spec ah_pre(html(), css(), attrs()) -> element().
ah_pre(C, Css, A) -> aihtml_html:el(pre, C, Css, A).

-spec ah_blockquote(html()) -> element().
ah_blockquote(C) -> aihtml_html:el(blockquote, C, [], []).
-spec ah_blockquote(html(), css(), attrs()) -> element().
ah_blockquote(C, Css, A) -> aihtml_html:el(blockquote, C, Css, A).

-spec ah_table(html()) -> element().
ah_table(C) -> aihtml_html:el(table, C, [], []).
-spec ah_table(html(), css(), attrs()) -> element().
ah_table(C, Css, A) -> aihtml_html:el(table, C, Css, A).

-spec ah_thead(html()) -> element().
ah_thead(C) -> aihtml_html:el(thead, C, [], []).
-spec ah_thead(html(), css(), attrs()) -> element().
ah_thead(C, Css, A) -> aihtml_html:el(thead, C, Css, A).

-spec ah_tbody(html()) -> element().
ah_tbody(C) -> aihtml_html:el(tbody, C, [], []).
-spec ah_tbody(html(), css(), attrs()) -> element().
ah_tbody(C, Css, A) -> aihtml_html:el(tbody, C, Css, A).

-spec ah_tr(html()) -> element().
ah_tr(C) -> aihtml_html:el(tr, C, [], []).
-spec ah_tr(html(), css(), attrs()) -> element().
ah_tr(C, Css, A) -> aihtml_html:el(tr, C, Css, A).

-spec ah_th(html()) -> element().
ah_th(C) -> aihtml_html:el(th, C, [], []).
-spec ah_th(html(), css(), attrs()) -> element().
ah_th(C, Css, A) -> aihtml_html:el(th, C, Css, A).

-spec ah_td(html()) -> element().
ah_td(C) -> aihtml_html:el(td, C, [], []).
-spec ah_td(html(), css(), attrs()) -> element().
ah_td(C, Css, A) -> aihtml_html:el(td, C, Css, A).

-spec ah_br() -> element().
ah_br() -> aihtml_html:void(br, [], []).

-spec ah_hr(css(), attrs()) -> element().
ah_hr(Css, A) -> aihtml_html:void(hr, Css, A).

-spec ah_img(css(), attrs()) -> element().
ah_img(Css, A) -> aihtml_html:void(img, Css, A).

%% @doc Any element, for tags without a helper.
-spec ah_el(atom() | binary(), html(), css(), attrs()) -> element().
ah_el(Tag, Children, Css, Attrs) -> aihtml_html:el(Tag, Children, Css, Attrs).

-spec ah_void(atom() | binary(), css(), attrs()) -> element().
ah_void(Tag, Css, Attrs) -> aihtml_html:void(Tag, Css, Attrs).

%% @doc Text content. Plain binaries are text already; this exists for
%% values that would otherwise be read as children, such as a list of
%% integers.
-spec text(term()) -> binary().
text(V) when is_list(V) -> unicode:characters_to_binary(V);
text(V) -> beamai_html_escape:to_binary(V, aihtml).

%% @doc Trusted HTML, written without escaping. Never pass user input.
-spec safe(iodata()) -> {safe, iodata()}.
safe(IoData) -> {safe, IoData}.

-spec render(html()) -> iodata().
render(Html) -> aihtml_html:render(Html).

-spec render_binary(html()) -> binary().
render_binary(Html) -> aihtml_html:render_binary(Html).

%% @doc A whole document, see `aihtml_page'.
-spec page(html(), aihtml_page:opts()) -> iodata().
page(Body, Opts) -> aihtml_page:render(Body, Opts).

%% @doc Attributes that make an element fetch HTML from the server and swap
%% it into `Target' (a CSS selector, or `this'). Splice the result into any
%% Attrs list: `ah_button(<<"More">>, more, [], [fetch(get, <<"/more">>, <<"#list">>)])'.
-spec fetch(get | post | put | patch | delete, iodata(), iodata() | this) -> attrs().
fetch(Method, Url, Target) -> fetch(Method, Url, Target, #{}).

%% @doc Options: `swap' (inner | outer | append | prepend | none | morph |
%% morph_inner, default inner; see aihtml_action:html/4), `trigger' (a DOM event name; default submit for forms, change
%% for inputs, click otherwise), `confirm' (a question asked first),
%% `indicator' and `disable' (as for on/3).
-spec fetch(get | post | put | patch | delete, iodata(), iodata() | this,
            #{swap => inner | outer | append | prepend | none | morph | morph_inner,
              trigger => atom() | binary(), confirm => iodata(),
              indicator => iodata() | this, disable => iodata() | this}) -> attrs().
fetch(Method, Url, Target, Opts) ->
    lists:member(Method, [get, post, put, patch, delete])
        orelse error({aihtml, {bad_fetch_method, Method}}),
    Swap = maps:get(swap, Opts, inner),
    lists:member(Swap, [inner, outer, append, prepend, none, morph, morph_inner])
        orelse error({aihtml, {bad_fetch_swap, Swap}}),
    [{data_ah_fetch, Method},
     {data_ah_url, iolist_to_binary(Url)},
     {data_ah_target, target(Target)},
     {data_ah_swap, Swap},
     {data_ah_trigger, maps:get(trigger, Opts, undefined)},
     {data_ah_confirm, maps:get(confirm, Opts, undefined)},
     request_attrs(Opts)].

%% @doc Bind an event to an action, spliced into Attrs like `fetch/3':
%% `ah_button(<<"Save">>, save, [], [on(click, {?MODULE, save, #{id => 7}})])'.
%% When the browser reports `Event' (click, change, input, submit, keydown,
%% ..., or a component event such as 'ah:close'), it POSTs the signed action
%% and the event; `Module:action/4' runs
%% on whichever node receives the request. See `aihtml_action'.
-spec on(atom() | binary(), aihtml_action:ref()) -> attrs().
on(Event, Action) -> on(Event, Action, #{}).

%% @doc Options:
%% `debounce' (ms) waits for the events to pause and sends only the last
%% one, for `input' and `keyup';
%% `include' is a list of selectors (or `{id, Id}') whose controls' values
%% are sent along in the event's `values';
%% `confirm' asks the user first;
%% `sync' decides what happens when a request of the same element (or
%% scope) is still running: drop the new one (default for click, submit),
%% replace the running one (default for input, change, key events) or
%% queue the new one until the running one ends;
%% `sync_scope' is a selector of an ancestor whose elements share one
%% queue (e.g. <<"form">>);
%% `indicator' is a selector (or `this', or <<"closest ...">>) whose
%% elements get the class ah-request while the request runs (style
%% .ah-indicator elements appear then);
%% `disable' names elements disabled while the request runs.
%% sync, sync_scope, indicator and disable are attributes of the element,
%% so they apply to every action bound on it.
-spec on(atom() | binary(), aihtml_action:ref(),
         #{debounce => pos_integer(), include => [iodata() | {id, iodata() | atom()}],
           confirm => iodata(), sync => drop | replace | queue, sync_scope => iodata(),
           indicator => iodata() | this, disable => iodata() | this}) -> attrs().
on(Event, Action, Opts) when is_map(Opts) ->
    E = event_name(Event),
    is_function(Action) andalso error({aihtml, {action_must_be_mfa, Action}}),
    lists:member(maps:get(sync, Opts, drop), [drop, replace, queue])
        orelse error({aihtml, {bad_sync, maps:get(sync, Opts)}}),
    [{<<"data-ah-on">>, {actions, [{E, aihtml_action:token(Action), maps:with([debounce], Opts)}]}},
     [{<<"data-ah-include">>, {selectors, [include_sel(S) || S <- Sels]}}
      || #{include := Sels} <- [Opts], Sels =/= []],
     {data_ah_confirm, maps:get(confirm, Opts, undefined)},
     {data_ah_sync, maps:get(sync, Opts, undefined)},
     {data_ah_sync_scope, maps:get(sync_scope, Opts, undefined)},
     request_attrs(Opts)].

%% @doc Apply DOM operations in the browser when `Event' fires, without a
%% request: the client-side counterpart of `on/2'. `Fun' gets a context
%% and calls the same operation functions an action does (aihtml_action:
%% call/4, add_class/3, attr/4, set_value/3, trigger/4, ...); they are
%% recorded when the page is rendered and written into the element:
%%
%% ```
%% ah_button(<<"Expand all">>, expand, [], [on_client(click, fun(C) ->
%%     aihtml_action:call(C, <<".faq [data-ah=expander]">>, open, [])
%% end)])
%% '''
%%
%% Use it for what needs no server: open, close or switch a component,
%% toggle a class, copy a value into a field. An element may carry both:
%% its `on_client' operations run first, then its `on' action is sent. The
%% operations are part of the page, so the user can read them; data they
%% must not see belongs in an action.
-spec on_client(atom() | binary(), fun((aihtml_action:ctx()) -> any())) -> attrs().
on_client(Event, Fun) when is_function(Fun, 1) ->
    [{<<"data-ah-on-client">>, {local, [{event_name(Event), aihtml_action:render_ops(Fun)}]}}].

%% DOM events (click, change, ...) or component events (ah:close, ...)
event_name(Event) ->
    E = beamai_html_escape:to_binary(Event, aihtml),
    re:run(E, <<"^(ah:)?[a-z][a-z-]*$">>) =/= nomatch
        orelse error({aihtml, {bad_event_name, Event}}),
    E.

%% indicator and disable, shared with fetch/4
request_attrs(Opts) ->
    [{data_ah_indicator, sel_opt(maps:get(indicator, Opts, undefined))},
     {data_ah_disable, sel_opt(maps:get(disable, Opts, undefined))}].

sel_opt(undefined) -> undefined;
sel_opt(this) -> <<"this">>;
sel_opt(S) -> text(S).

%% @doc Keep this element (it needs an id) as it is when new content from
%% a swap brings an element with the same id: the existing node is moved
%% into place, with its state (a playing video, typed text, a mounted
%% component). Splice into Attrs: `ah_div(Player, [], [{id, player}, preserve()])'.
-spec preserve() -> attrs().
preserve() -> [{data_ah_preserve, true}].

%% @doc Follow a push topic, spliced into the Attrs of the element whose
%% content the topic updates: `ah_ul(Items, [], [{id, list}, subscribe(todos)])'.
%% The page opens one event stream for all its topics. One subscription
%% per element.
-spec subscribe(aihtml_push:topic()) -> attrs().
subscribe(Topic) -> subscribe(Topic, #{}).

%% @doc Options: `refresh' is an action run after every reconnect of the
%% stream (not the first connect), to reload what may have been missed.
-spec subscribe(aihtml_push:topic(), #{refresh => aihtml_action:ref()}) -> attrs().
subscribe(Topic, Opts) when is_map(Opts) ->
    [{data_ah_subscribe, aihtml_push:token(Topic)},
     [{data_ah_refresh, aihtml_action:token(Ref)} || #{refresh := Ref} <- [Opts]]].

include_sel({id, Id}) -> <<"#", (text(Id))/binary>>;
include_sel(Sel) -> text(Sel).

target(this) -> <<"this">>;
target(T) -> iolist_to_binary(T).

%%%===================================================================
%%% Components: the generated section below re-exports every component
%%% group module (scripts/gen-facade.escript).
%%%===================================================================

-spec ah_theme_switcher(css(), attrs()) -> element().
ah_theme_switcher(Css, Attrs) -> aihtml_theme:switcher(Css, Attrs).

%% BEGIN GENERATED COMPONENTS
%% aihtml_button
-spec ah_button(aihtml_html:html(),
                term(),
                aihtml_html:css(),
                aihtml_html:attrs()) ->
                   #ah_button{}.
ah_button(A1, A2, A3, A4) -> aihtml_button:ah_button(A1, A2, A3, A4).

%% aihtml_link_button
-spec ah_link_button(aihtml_html:html(),
                     iodata() | undefined,
                     aihtml_html:css(),
                     aihtml_html:attrs()) ->
                        #ah_link_button{}.
ah_link_button(A1, A2, A3, A4) -> aihtml_link_button:ah_link_button(A1, A2, A3, A4).

%% aihtml_toggle_button
-spec ah_toggle_button(aihtml_html:html(),
                       boolean(),
                       aihtml_html:css(),
                       aihtml_html:attrs()) ->
                          #ah_toggle_button{}.
ah_toggle_button(A1, A2, A3, A4) -> aihtml_toggle_button:ah_toggle_button(A1, A2, A3, A4).

%% aihtml_button_group
-spec ah_button_group([aihtml_lib_button:item()],
                      term(),
                      aihtml_html:css(),
                      aihtml_html:attrs()) ->
                         #ah_button_group{}.
ah_button_group(A1, A2, A3, A4) -> aihtml_button_group:ah_button_group(A1, A2, A3, A4).

%% aihtml_segmented_control
-spec ah_segmented_control([aihtml_lib_button:item()],
                           term(),
                           aihtml_html:css(),
                           aihtml_html:attrs()) ->
                              #ah_segmented_control{}.
ah_segmented_control(A1, A2, A3, A4) -> aihtml_segmented_control:ah_segmented_control(A1, A2, A3, A4).

%% aihtml_dropdown_button
-spec ah_dropdown_button(aihtml_html:html(),
                         [aihtml_lib_button:item()],
                         aihtml_html:css(),
                         aihtml_html:attrs()) ->
                            #ah_dropdown_button{}.
ah_dropdown_button(A1, A2, A3, A4) -> aihtml_dropdown_button:ah_dropdown_button(A1, A2, A3, A4).

%% aihtml_split_button
-spec ah_split_button(aihtml_html:html(),
                      [aihtml_lib_button:item()],
                      aihtml_html:css(),
                      aihtml_html:attrs()) ->
                         #ah_split_button{}.
ah_split_button(A1, A2, A3, A4) -> aihtml_split_button:ah_split_button(A1, A2, A3, A4).

%% aihtml_checkbox
-spec ah_checkbox(aihtml_html:html(),
                  aihtml_lib_choice:value() | undefined,
                  aihtml_html:css(),
                  aihtml_html:attrs()) ->
                     #ah_checkbox{}.
ah_checkbox(A1, A2, A3, A4) -> aihtml_checkbox:ah_checkbox(A1, A2, A3, A4).

%% aihtml_radiobutton
-spec ah_radiobutton(aihtml_html:html(),
                     aihtml_lib_choice:value() | undefined,
                     aihtml_html:css(),
                     aihtml_html:attrs()) ->
                        #ah_radiobutton{}.
ah_radiobutton(A1, A2, A3, A4) -> aihtml_radiobutton:ah_radiobutton(A1, A2, A3, A4).

%% aihtml_switch_button
-spec ah_switch_button(aihtml_html:html(),
                       aihtml_lib_choice:value() | undefined,
                       aihtml_html:css(),
                       aihtml_html:attrs()) ->
                          #ah_switch_button{}.
ah_switch_button(A1, A2, A3, A4) -> aihtml_switch_button:ah_switch_button(A1, A2, A3, A4).

%% aihtml_checkbox_group
-spec ah_checkbox_group([aihtml_lib_choice:item()],
                        [aihtml_lib_choice:value()],
                        aihtml_html:css(),
                        aihtml_html:attrs()) ->
                           #ah_checkbox_group{}.
ah_checkbox_group(A1, A2, A3, A4) -> aihtml_checkbox_group:ah_checkbox_group(A1, A2, A3, A4).

%% aihtml_radiobutton_group
-spec ah_radiobutton_group([aihtml_lib_choice:item()],
                           aihtml_lib_choice:value() | undefined,
                           aihtml_html:css(),
                           aihtml_html:attrs()) ->
                              #ah_radiobutton_group{}.
ah_radiobutton_group(A1, A2, A3, A4) -> aihtml_radiobutton_group:ah_radiobutton_group(A1, A2, A3, A4).

%% aihtml_radio_cards
-spec ah_radio_cards([aihtml_lib_choice:item()],
                     aihtml_lib_choice:value() | undefined,
                     aihtml_html:css(),
                     aihtml_html:attrs()) ->
                        #ah_radio_cards{}.
ah_radio_cards(A1, A2, A3, A4) -> aihtml_radio_cards:ah_radio_cards(A1, A2, A3, A4).

%% aihtml_rating_group
-spec ah_rating_group(pos_integer(),
                      number() | undefined,
                      aihtml_html:css(),
                      aihtml_html:attrs()) ->
                         #ah_rating_group{}.
ah_rating_group(A1, A2, A3, A4) -> aihtml_rating_group:ah_rating_group(A1, A2, A3, A4).

%% aihtml_input
-spec ah_input(binary() | undefined,
               aihtml_html:css(),
               aihtml_html:attrs()) ->
                  #ah_input{}.
ah_input(A1, A2, A3) -> aihtml_input:ah_input(A1, A2, A3).

%% aihtml_textarea
-spec ah_textarea(binary() | undefined,
                  aihtml_html:css(),
                  aihtml_html:attrs()) ->
                     #ah_textarea{}.
ah_textarea(A1, A2, A3) -> aihtml_textarea:ah_textarea(A1, A2, A3).

%% aihtml_password_input
-spec ah_password_input(binary() | undefined,
                        aihtml_html:css(),
                        aihtml_html:attrs()) ->
                           #ah_password_input{}.
ah_password_input(A1, A2, A3) -> aihtml_password_input:ah_password_input(A1, A2, A3).

%% aihtml_number_input
-spec ah_number_input(number() | binary() | undefined,
                      aihtml_html:css(),
                      aihtml_html:attrs()) ->
                         #ah_number_input{}.
ah_number_input(A1, A2, A3) -> aihtml_number_input:ah_number_input(A1, A2, A3).

%% aihtml_input_otp
-spec ah_input_otp(pos_integer(),
                   binary() | undefined,
                   aihtml_html:css(),
                   aihtml_html:attrs()) ->
                      #ah_input_otp{}.
ah_input_otp(A1, A2, A3, A4) -> aihtml_input_otp:ah_input_otp(A1, A2, A3, A4).

%% aihtml_tag_input
-spec ah_tag_input([binary()], aihtml_html:css(), aihtml_html:attrs()) ->
                      #ah_tag_input{}.
ah_tag_input(A1, A2, A3) -> aihtml_tag_input:ah_tag_input(A1, A2, A3).

%% aihtml_markdown_editor
-spec ah_markdown_editor(undefined | unicode:chardata(),
                         aihtml_html:css(),
                         aihtml_html:attrs()) ->
                            #ah_markdown_editor{}.
ah_markdown_editor(A1, A2, A3) -> aihtml_markdown_editor:ah_markdown_editor(A1, A2, A3).

%% aihtml_markdown_view
-spec ah_markdown_view(undefined | unicode:chardata(),
                       aihtml_html:css(),
                       aihtml_html:attrs()) ->
                          aihtml_markdown_view:element().
ah_markdown_view(A1, A2, A3) -> aihtml_markdown_view:ah_markdown_view(A1, A2, A3).

%% aihtml_dropdownlist
-spec ah_dropdownlist([aihtml_lib_select:item()],
                      aihtml_lib_select:value() | undefined,
                      aihtml_html:css(),
                      aihtml_html:attrs()) ->
                         #ah_dropdownlist{}.
ah_dropdownlist(A1, A2, A3, A4) -> aihtml_dropdownlist:ah_dropdownlist(A1, A2, A3, A4).

%% aihtml_select
-spec ah_select([aihtml_lib_select:item()],
                aihtml_lib_select:value() |
                [aihtml_lib_select:value()] |
                undefined,
                aihtml_html:css(),
                aihtml_html:attrs()) ->
                   #ah_select{}.
ah_select(A1, A2, A3, A4) -> aihtml_select:ah_select(A1, A2, A3, A4).

%% aihtml_slider
-spec ah_slider(aihtml_slider:range(),
                number() | {number(), number()} | undefined,
                aihtml_html:css(),
                aihtml_html:attrs()) ->
                   #ah_slider{}.
ah_slider(A1, A2, A3, A4) -> aihtml_slider:ah_slider(A1, A2, A3, A4).

%% aihtml_field
-spec ah_field(aihtml_html:html(),
               aihtml_html:html(),
               aihtml_html:css(),
               aihtml_html:attrs()) ->
                  #ah_field{}.
ah_field(A1, A2, A3, A4) -> aihtml_field:ah_field(A1, A2, A3, A4).
-spec validate([aihtml_field:rule() | {hint | position | on, term()}]) ->
                  aihtml_html:attrs().
validate(A1) -> aihtml_field:validate(A1).

%% aihtml_form_layout
-spec ah_form_layout([aihtml_form_layout:field_spec()],
                     #{term() => term()},
                     aihtml_html:css(),
                     aihtml_html:attrs()) ->
                        #ah_form_layout{}.
ah_form_layout(A1, A2, A3, A4) -> aihtml_form_layout:ah_form_layout(A1, A2, A3, A4).

%% aihtml_datepicker
-spec ah_datepicker(aihtml_datepicker:date_value(),
                    aihtml_html:css(),
                    aihtml_html:attrs()) ->
                       #ah_datepicker{}.
ah_datepicker(A1, A2, A3) -> aihtml_datepicker:ah_datepicker(A1, A2, A3).

%% aihtml_combobox
-spec ah_combobox([aihtml_combobox:item()],
                  term() | [term()] | undefined,
                  aihtml_html:css(),
                  aihtml_html:attrs()) ->
                     #ah_combobox{}.
ah_combobox(A1, A2, A3, A4) -> aihtml_combobox:ah_combobox(A1, A2, A3, A4).
-spec set_items(aihtml_action:ctx(),
                {id, iodata() | atom()} | aihtml_action:event(),
                [aihtml_combobox:item()]) ->
                   ok.
set_items(A1, A2, A3) -> aihtml_combobox:set_items(A1, A2, A3).
-spec set_items(aihtml_action:ctx(),
                {id, iodata() | atom()},
                [aihtml_combobox:item()],
                #{checkboxes => boolean(), selected => [term()]}) ->
                   ok.
set_items(A1, A2, A3, A4) -> aihtml_combobox:set_items(A1, A2, A3, A4).

%% aihtml_timepicker
-spec ah_timepicker(binary() | string() | tuple() | undefined,
                    aihtml_html:css(),
                    aihtml_html:attrs()) ->
                       #ah_timepicker{}.
ah_timepicker(A1, A2, A3) -> aihtml_timepicker:ah_timepicker(A1, A2, A3).

%% aihtml_colorpicker
-spec ah_colorpicker(binary() | string() | tuple() | undefined,
                     aihtml_html:css(),
                     aihtml_html:attrs()) ->
                        #ah_colorpicker{}.
ah_colorpicker(A1, A2, A3) -> aihtml_colorpicker:ah_colorpicker(A1, A2, A3).

%% aihtml_calendar
-spec ah_calendar(aihtml_lib_date:date(),
                  aihtml_html:css(),
                  aihtml_html:attrs()) ->
                     #ah_calendar{}.
ah_calendar(A1, A2, A3) -> aihtml_calendar:ah_calendar(A1, A2, A3).
-spec set_events(aihtml_action:ctx(),
                 {id, iodata() | atom()} | aihtml_action:event(),
                 [aihtml_calendar:event()]) ->
                    ok.
set_events(A1, A2, A3) -> aihtml_calendar:set_events(A1, A2, A3).
-spec add_event(aihtml_action:ctx(),
                {id, iodata() | atom()} | aihtml_action:event(),
                aihtml_calendar:event()) ->
                   ok.
add_event(A1, A2, A3) -> aihtml_calendar:add_event(A1, A2, A3).

%% aihtml_datetime_input
-spec ah_datetime_input(aihtml_datetime_input:value(),
                        aihtml_html:css(),
                        aihtml_html:attrs()) ->
                           #ah_datetime_input{}.
ah_datetime_input(A1, A2, A3) -> aihtml_datetime_input:ah_datetime_input(A1, A2, A3).

%% aihtml_cascader
-spec ah_cascader([aihtml_cascader:cascader_node()],
                  [term()] | undefined,
                  aihtml_html:css(),
                  aihtml_html:attrs()) ->
                     #ah_cascader{}.
ah_cascader(A1, A2, A3, A4) -> aihtml_cascader:ah_cascader(A1, A2, A3, A4).
-spec cascader_children(aihtml_action:ctx(),
                        aihtml_action:event(),
                        [aihtml_cascader:cascader_node()]) ->
                           ok.
cascader_children(A1, A2, A3) -> aihtml_cascader:cascader_children(A1, A2, A3).
-spec cascader_children(aihtml_action:ctx(),
                        {id, iodata() | atom()},
                        [term()],
                        [aihtml_cascader:cascader_node()]) ->
                           ok.
cascader_children(A1, A2, A3, A4) -> aihtml_cascader:cascader_children(A1, A2, A3, A4).

%% aihtml_listbox
-spec ah_listbox([aihtml_lib_list:item()],
                 term() | [term()] | undefined,
                 aihtml_html:css(),
                 aihtml_html:attrs()) ->
                    #ah_listbox{}.
ah_listbox(A1, A2, A3, A4) -> aihtml_listbox:ah_listbox(A1, A2, A3, A4).
-spec listbox_items(aihtml_action:ctx(),
                    aihtml_action:event() | {id, iodata() | atom()},
                    [aihtml_lib_list:item()]) ->
                       ok.
listbox_items(A1, A2, A3) -> aihtml_listbox:listbox_items(A1, A2, A3).
-spec listbox_items(aihtml_action:ctx(),
                    {id, iodata() | atom()},
                    [aihtml_lib_list:item()],
                    #{checkboxes => boolean(), selected => [term()]}) ->
                       ok.
listbox_items(A1, A2, A3, A4) -> aihtml_listbox:listbox_items(A1, A2, A3, A4).

%% aihtml_transfer
-spec ah_transfer([aihtml_lib_list:item()],
                  [term()],
                  aihtml_html:css(),
                  aihtml_html:attrs()) ->
                     #ah_transfer{}.
ah_transfer(A1, A2, A3, A4) -> aihtml_transfer:ah_transfer(A1, A2, A3, A4).

%% aihtml_masked_input
-spec ah_masked_input(unicode:chardata() | undefined,
                      aihtml_html:css(),
                      aihtml_html:attrs()) ->
                         #ah_masked_input{}.
ah_masked_input(A1, A2, A3) -> aihtml_masked_input:ah_masked_input(A1, A2, A3).

%% aihtml_formatted_input
-spec ah_formatted_input(aihtml_formatted_input:integer_value(),
                         aihtml_html:css(),
                         aihtml_html:attrs()) ->
                            #ah_formatted_input{}.
ah_formatted_input(A1, A2, A3) -> aihtml_formatted_input:ah_formatted_input(A1, A2, A3).

%% aihtml_range_selector
-spec ah_range_selector({number(), number()} |
                        {number(), number(), number()},
                        {number(), number()} | undefined,
                        aihtml_html:css(),
                        aihtml_html:attrs()) ->
                           #ah_range_selector{}.
ah_range_selector(A1, A2, A3, A4) -> aihtml_range_selector:ah_range_selector(A1, A2, A3, A4).

%% aihtml_repeat_button
-spec ah_repeat_button(aihtml_html:html(),
                       term(),
                       aihtml_html:css(),
                       aihtml_html:attrs()) ->
                          #ah_repeat_button{}.
ah_repeat_button(A1, A2, A3, A4) -> aihtml_repeat_button:ah_repeat_button(A1, A2, A3, A4).

%% aihtml_upload
-spec ah_upload([aihtml_upload:file()],
                aihtml_html:css(),
                aihtml_html:attrs()) ->
                   #ah_upload{}.
ah_upload(A1, A2, A3) -> aihtml_upload:ah_upload(A1, A2, A3).
-spec uploaded_files(aihtml_action:event() | binary()) -> [term()].
uploaded_files(A1) -> aihtml_upload:uploaded_files(A1).

%% aihtml_card
-spec ah_card(aihtml_html:html(),
              aihtml_html:css(),
              aihtml_html:attrs()) ->
                 #ah_card{}.
ah_card(A1, A2, A3) -> aihtml_card:ah_card(A1, A2, A3).

%% aihtml_panel
-spec ah_panel(aihtml_html:html(),
               aihtml_html:css(),
               aihtml_html:attrs()) ->
                  #ah_panel{}.
ah_panel(A1, A2, A3) -> aihtml_panel:ah_panel(A1, A2, A3).

%% aihtml_expander
-spec ah_expander(aihtml_html:html(),
                  aihtml_html:css(),
                  aihtml_html:attrs()) ->
                     #ah_expander{}.
ah_expander(A1, A2, A3) -> aihtml_expander:ah_expander(A1, A2, A3).

%% aihtml_tabs
-spec ah_tabs([{aihtml_lib_layout:key(),
                aihtml_html:html(),
                aihtml_html:html()} |
               {aihtml_lib_layout:key(),
                aihtml_html:html(),
                aihtml_html:html(),
                map()}],
              aihtml_lib_layout:key() | undefined,
              aihtml_html:css(),
              aihtml_html:attrs()) ->
                 #ah_tabs{}.
ah_tabs(A1, A2, A3, A4) -> aihtml_tabs:ah_tabs(A1, A2, A3, A4).

%% aihtml_tab_bar
-spec ah_tab_bar([{aihtml_lib_layout:key(), aihtml_html:html()} |
                  {aihtml_lib_layout:key(), aihtml_html:html(), map()}],
                 aihtml_lib_layout:key() | undefined,
                 aihtml_html:css(),
                 aihtml_html:attrs()) ->
                    #ah_tab_bar{}.
ah_tab_bar(A1, A2, A3, A4) -> aihtml_tab_bar:ah_tab_bar(A1, A2, A3, A4).

%% aihtml_breadcrumbs
-spec ah_breadcrumbs([aihtml_html:html() |
                      {aihtml_html:html(), binary() | undefined} |
                      map()],
                     aihtml_html:css(),
                     aihtml_html:attrs()) ->
                        #ah_breadcrumbs{}.
ah_breadcrumbs(A1, A2, A3) -> aihtml_breadcrumbs:ah_breadcrumbs(A1, A2, A3).

%% aihtml_pagination
-spec ah_pagination(non_neg_integer(),
                    pos_integer(),
                    aihtml_html:css(),
                    aihtml_html:attrs()) ->
                       #ah_pagination{}.
ah_pagination(A1, A2, A3, A4) -> aihtml_pagination:ah_pagination(A1, A2, A3, A4).

%% aihtml_steps
-spec ah_steps([aihtml_html:html() |
                {aihtml_html:html(), aihtml_html:html()} |
                map()],
               non_neg_integer(),
               aihtml_html:css(),
               aihtml_html:attrs()) ->
                  #ah_steps{}.
ah_steps(A1, A2, A3, A4) -> aihtml_steps:ah_steps(A1, A2, A3, A4).

%% aihtml_skeleton
-spec ah_skeleton(aihtml_html:css(), aihtml_html:attrs()) ->
                     #ah_skeleton{}.
ah_skeleton(A1, A2) -> aihtml_skeleton:ah_skeleton(A1, A2).

%% aihtml_loader
-spec ah_loader(aihtml_html:css(), aihtml_html:attrs()) -> #ah_loader{}.
ah_loader(A1, A2) -> aihtml_loader:ah_loader(A1, A2).

%% aihtml_empty
-spec ah_empty(aihtml_html:html(),
               aihtml_html:css(),
               aihtml_html:attrs()) ->
                  #ah_empty{}.
ah_empty(A1, A2, A3) -> aihtml_empty:ah_empty(A1, A2, A3).

%% aihtml_menu
-spec ah_menu([aihtml_lib_nav:item()],
              aihtml_lib_nav:key() | undefined,
              aihtml_html:css(),
              aihtml_html:attrs()) ->
                 #ah_menu{}.
ah_menu(A1, A2, A3, A4) -> aihtml_menu:ah_menu(A1, A2, A3, A4).

%% aihtml_navbar
-spec ah_navbar([aihtml_lib_nav:item()],
                aihtml_lib_nav:key() | undefined,
                aihtml_html:css(),
                aihtml_html:attrs()) ->
                   #ah_navbar{}.
ah_navbar(A1, A2, A3, A4) -> aihtml_navbar:ah_navbar(A1, A2, A3, A4).

%% aihtml_sidenav
-spec ah_sidenav([aihtml_sidenav:group()] | [aihtml_lib_nav:item()],
                 aihtml_lib_nav:key() | undefined,
                 aihtml_html:css(),
                 aihtml_html:attrs()) ->
                    #ah_sidenav{}.
ah_sidenav(A1, A2, A3, A4) -> aihtml_sidenav:ah_sidenav(A1, A2, A3, A4).

%% aihtml_toolbar
-spec ah_toolbar([aihtml_toolbar:tool()],
                 aihtml_html:css(),
                 aihtml_html:attrs()) ->
                    #ah_toolbar{}.
ah_toolbar(A1, A2, A3) -> aihtml_toolbar:ah_toolbar(A1, A2, A3).

%% aihtml_splitter
-spec ah_splitter([aihtml_splitter:pane()],
                  aihtml_html:css(),
                  aihtml_html:attrs()) ->
                     #ah_splitter{}.
ah_splitter(A1, A2, A3) -> aihtml_splitter:ah_splitter(A1, A2, A3).

%% aihtml_listmenu
-spec ah_listmenu([aihtml_lib_nav:item()],
                  aihtml_lib_nav:key() | undefined,
                  aihtml_html:css(),
                  aihtml_html:attrs()) ->
                     #ah_listmenu{}.
ah_listmenu(A1, A2, A3, A4) -> aihtml_listmenu:ah_listmenu(A1, A2, A3, A4).

%% aihtml_status_bar
-spec ah_status_bar([aihtml_status_bar:segment()],
                    aihtml_html:css(),
                    aihtml_html:attrs()) ->
                       #ah_status_bar{}.
ah_status_bar(A1, A2, A3) -> aihtml_status_bar:ah_status_bar(A1, A2, A3).

%% aihtml_scrollview
-spec ah_scrollview([aihtml_html:html()],
                    aihtml_html:css(),
                    aihtml_html:attrs()) ->
                       #ah_scrollview{}.
ah_scrollview(A1, A2, A3) -> aihtml_scrollview:ah_scrollview(A1, A2, A3).

%% aihtml_scrollbar
-spec ah_scrollbar(aihtml_html:html(),
                   aihtml_html:css(),
                   aihtml_html:attrs()) ->
                      #ah_scrollbar{}.
ah_scrollbar(A1, A2, A3) -> aihtml_scrollbar:ah_scrollbar(A1, A2, A3).

%% aihtml_responsive_panel
-spec ah_responsive_panel(aihtml_html:html(),
                          aihtml_html:css(),
                          aihtml_html:attrs()) ->
                             #ah_responsive_panel{}.
ah_responsive_panel(A1, A2, A3) -> aihtml_responsive_panel:ah_responsive_panel(A1, A2, A3).

%% aihtml_activity_bar
-spec ah_activity_bar([aihtml_activity_bar:item()],
                      term(),
                      aihtml_html:css(),
                      aihtml_html:attrs()) ->
                         #ah_activity_bar{}.
ah_activity_bar(A1, A2, A3, A4) -> aihtml_activity_bar:ah_activity_bar(A1, A2, A3, A4).

%% aihtml_navigationbar
-spec ah_navigationbar([aihtml_navigationbar:item()],
                       aihtml_navigationbar:value(),
                       aihtml_html:css(),
                       aihtml_html:attrs()) ->
                          #ah_navigationbar{}.
ah_navigationbar(A1, A2, A3, A4) -> aihtml_navigationbar:ah_navigationbar(A1, A2, A3, A4).

%% aihtml_command
-spec ah_command([aihtml_command:entry()],
                 aihtml_html:css(),
                 aihtml_html:attrs()) ->
                    #ah_command{}.
ah_command(A1, A2, A3) -> aihtml_command:ah_command(A1, A2, A3).
-spec set_command_items(aihtml_action:ctx(),
                        {id, iodata() | atom()} | aihtml_action:event(),
                        [aihtml_command:entry()]) ->
                           ok.
set_command_items(A1, A2, A3) -> aihtml_command:set_command_items(A1, A2, A3).

%% aihtml_sortable
-spec ah_sortable([aihtml_sortable:item()],
                  undefined | [term()] | iodata(),
                  aihtml_html:css(),
                  aihtml_html:attrs()) ->
                     #ah_sortable{}.
ah_sortable(A1, A2, A3, A4) -> aihtml_sortable:ah_sortable(A1, A2, A3, A4).

%% aihtml_dragdrop
-spec ah_dragdrop(aihtml_html:html(),
                  aihtml_html:css(),
                  aihtml_html:attrs()) ->
                     #ah_dragdrop{}.
ah_dragdrop(A1, A2, A3) -> aihtml_dragdrop:ah_dragdrop(A1, A2, A3).
-spec draggable_attrs(term(), map()) -> aihtml_html:attrs().
draggable_attrs(A1, A2) -> aihtml_dragdrop:draggable_attrs(A1, A2).
-spec drop_zone_attrs(term(), map()) -> aihtml_html:attrs().
drop_zone_attrs(A1, A2) -> aihtml_dragdrop:drop_zone_attrs(A1, A2).

%% aihtml_docking
-spec ah_docking([aihtml_docking:panel()],
                 aihtml_html:css(),
                 aihtml_html:attrs()) ->
                    #ah_docking{}.
ah_docking(A1, A2, A3) -> aihtml_docking:ah_docking(A1, A2, A3).
-spec docking_add_window(aihtml_action:ctx(),
                         {id, iodata() | atom()},
                         aihtml_lib_dock:id(),
                         aihtml_docking:window()) ->
                            ok.
docking_add_window(A1, A2, A3, A4) -> aihtml_docking:docking_add_window(A1, A2, A3, A4).

%% aihtml_dock_layout
-spec ah_dock_layout(aihtml_dock_layout:layout(),
                     aihtml_html:css(),
                     aihtml_html:attrs()) ->
                        #ah_dock_layout{}.
ah_dock_layout(A1, A2, A3) -> aihtml_dock_layout:ah_dock_layout(A1, A2, A3).
-spec dock_layout_open(aihtml_action:ctx(),
                       {id, iodata() | atom()},
                       aihtml_dock_layout:panel()) ->
                          ok.
dock_layout_open(A1, A2, A3) -> aihtml_dock_layout:dock_layout_open(A1, A2, A3).
-spec dock_layout_open(aihtml_action:ctx(),
                       {id, iodata() | atom()},
                       aihtml_dock_layout:panel(),
                       #{in => aihtml_lib_dock:id(),
                         edge => left | right | top | bottom,
                         float => boolean() | {number(), number()},
                         labels => aihtml_dock_layout:labels()}) ->
                          ok.
dock_layout_open(A1, A2, A3, A4) -> aihtml_dock_layout:dock_layout_open(A1, A2, A3, A4).

%% aihtml_ribbon
-spec ah_ribbon([aihtml_ribbon:tab()],
                term(),
                aihtml_html:css(),
                aihtml_html:attrs()) ->
                   #ah_ribbon{}.
ah_ribbon(A1, A2, A3, A4) -> aihtml_ribbon:ah_ribbon(A1, A2, A3, A4).

%% aihtml_tile_layout
-spec ah_tile_layout(aihtml_tile_layout:layout_node(),
                     undefined | iodata() | map(),
                     aihtml_html:css(),
                     aihtml_html:attrs()) ->
                        #ah_tile_layout{}.
ah_tile_layout(A1, A2, A3, A4) -> aihtml_tile_layout:ah_tile_layout(A1, A2, A3, A4).

%% aihtml_tooltip
-spec ah_tooltip(aihtml_html:html(),
                 aihtml_html:html(),
                 aihtml_html:css(),
                 aihtml_html:attrs()) ->
                    #ah_tooltip{}.
ah_tooltip(A1, A2, A3, A4) -> aihtml_tooltip:ah_tooltip(A1, A2, A3, A4).
-spec tooltip_attrs(iodata(), map()) -> aihtml_html:attrs().
tooltip_attrs(A1, A2) -> aihtml_tooltip:tooltip_attrs(A1, A2).

%% aihtml_popover
-spec ah_popover(aihtml_html:html(),
                 aihtml_html:css(),
                 aihtml_html:attrs()) ->
                    #ah_popover{}.
ah_popover(A1, A2, A3) -> aihtml_popover:ah_popover(A1, A2, A3).

%% aihtml_drawer
-spec ah_drawer(aihtml_html:html(),
                aihtml_html:css(),
                aihtml_html:attrs()) ->
                   #ah_drawer{}.
ah_drawer(A1, A2, A3) -> aihtml_drawer:ah_drawer(A1, A2, A3).

%% aihtml_sheet
-spec ah_sheet(aihtml_html:html(),
               aihtml_html:css(),
               aihtml_html:attrs()) ->
                  #ah_sheet{}.
ah_sheet(A1, A2, A3) -> aihtml_sheet:ah_sheet(A1, A2, A3).

%% aihtml_toast
-spec shows_toast(iodata(), map()) -> aihtml_html:attrs().
shows_toast(A1, A2) -> aihtml_toast:shows_toast(A1, A2).
-spec ah_toast(aihtml_action:ctx(), iodata(), map()) -> ok.
ah_toast(A1, A2, A3) -> aihtml_toast:ah_toast(A1, A2, A3).

%% aihtml_notification
-spec ah_notification(aihtml_html:html(),
                      aihtml_html:css(),
                      aihtml_html:attrs()) ->
                         #ah_notification{}.
ah_notification(A1, A2, A3) -> aihtml_notification:ah_notification(A1, A2, A3).

%% aihtml_window
-spec ah_window(aihtml_html:html(),
                aihtml_html:css(),
                aihtml_html:attrs()) ->
                   #ah_window{}.
ah_window(A1, A2, A3) -> aihtml_window:ah_window(A1, A2, A3).

%% aihtml_avatar
-spec ah_avatar(aihtml_html:html(),
                aihtml_html:css(),
                aihtml_html:attrs()) ->
                   #ah_avatar{}.
ah_avatar(A1, A2, A3) -> aihtml_avatar:ah_avatar(A1, A2, A3).

%% aihtml_badge
-spec ah_badge(aihtml_html:html(),
               aihtml_html:css(),
               aihtml_html:attrs()) ->
                  #ah_badge{}.
ah_badge(A1, A2, A3) -> aihtml_badge:ah_badge(A1, A2, A3).

%% aihtml_chip
-spec ah_chip(aihtml_html:html(),
              aihtml_html:css(),
              aihtml_html:attrs()) ->
                 #ah_chip{}.
ah_chip(A1, A2, A3) -> aihtml_chip:ah_chip(A1, A2, A3).

%% aihtml_aspect_ratio
-spec ah_aspect_ratio(aihtml_html:html(),
                      aihtml_html:css(),
                      aihtml_html:attrs()) ->
                         #ah_aspect_ratio{}.
ah_aspect_ratio(A1, A2, A3) -> aihtml_aspect_ratio:ah_aspect_ratio(A1, A2, A3).

%% aihtml_kbd
-spec ah_kbd(aihtml_html:html() | [aihtml_html:html()],
             aihtml_html:css(),
             aihtml_html:attrs()) ->
                #ah_kbd{}.
ah_kbd(A1, A2, A3) -> aihtml_kbd:ah_kbd(A1, A2, A3).

%% aihtml_time_ago
-spec ah_time_ago(integer() | calendar:datetime() | binary(),
                  aihtml_html:css(),
                  aihtml_html:attrs()) ->
                     #ah_time_ago{}.
ah_time_ago(A1, A2, A3) -> aihtml_time_ago:ah_time_ago(A1, A2, A3).

%% aihtml_expandable_text
-spec ah_expandable_text(unicode:chardata(),
                         aihtml_html:css(),
                         aihtml_html:attrs()) ->
                            #ah_expandable_text{}.
ah_expandable_text(A1, A2, A3) -> aihtml_expandable_text:ah_expandable_text(A1, A2, A3).

%% aihtml_alert
-spec ah_alert(aihtml_html:html(),
               aihtml_html:css(),
               aihtml_html:attrs()) ->
                  #ah_alert{}.
ah_alert(A1, A2, A3) -> aihtml_alert:ah_alert(A1, A2, A3).

%% aihtml_progressbar
-spec ah_progressbar(number() | undefined,
                     aihtml_html:css(),
                     aihtml_html:attrs()) ->
                        #ah_progressbar{}.
ah_progressbar(A1, A2, A3) -> aihtml_progressbar:ah_progressbar(A1, A2, A3).

%% aihtml_progress_circle
-spec ah_progress_circle(number() | undefined,
                         aihtml_html:css(),
                         aihtml_html:attrs()) ->
                            #ah_progress_circle{}.
ah_progress_circle(A1, A2, A3) -> aihtml_progress_circle:ah_progress_circle(A1, A2, A3).

%% aihtml_meter
-spec ah_meter(number(), aihtml_html:css(), aihtml_html:attrs()) ->
                  #ah_meter{}.
ah_meter(A1, A2, A3) -> aihtml_meter:ah_meter(A1, A2, A3).

%% aihtml_statistic
-spec ah_statistic(number() | aihtml_html:html(),
                   aihtml_html:css(),
                   aihtml_html:attrs()) ->
                      #ah_statistic{}.
ah_statistic(A1, A2, A3) -> aihtml_statistic:ah_statistic(A1, A2, A3).

%% aihtml_kpi_card
-spec ah_kpi_card(aihtml_html:html(),
                  aihtml_html:css(),
                  aihtml_html:attrs()) ->
                     #ah_kpi_card{}.
ah_kpi_card(A1, A2, A3) -> aihtml_kpi_card:ah_kpi_card(A1, A2, A3).

%% aihtml_timeline
-spec ah_timeline([map()], aihtml_html:css(), aihtml_html:attrs()) ->
                     #ah_timeline{}.
ah_timeline(A1, A2, A3) -> aihtml_timeline:ah_timeline(A1, A2, A3).

%% aihtml_ranking_list
-spec ah_ranking_list([map()], aihtml_html:css(), aihtml_html:attrs()) ->
                         #ah_ranking_list{}.
ah_ranking_list(A1, A2, A3) -> aihtml_ranking_list:ah_ranking_list(A1, A2, A3).

%% aihtml_tag_cloud
-spec ah_tag_cloud([map() | {aihtml_html:html(), number()}],
                   aihtml_html:css(),
                   aihtml_html:attrs()) ->
                      #ah_tag_cloud{}.
ah_tag_cloud(A1, A2, A3) -> aihtml_tag_cloud:ah_tag_cloud(A1, A2, A3).

%% aihtml_tree
-spec ah_tree([aihtml_tree:item()],
              term(),
              aihtml_html:css(),
              aihtml_html:attrs()) ->
                 #ah_tree{}.
ah_tree(A1, A2, A3, A4) -> aihtml_tree:ah_tree(A1, A2, A3, A4).
-spec set_children(aihtml_action:ctx(),
                   aihtml_action:event(),
                   [aihtml_tree:item()]) ->
                      ok.
set_children(A1, A2, A3) -> aihtml_tree:set_children(A1, A2, A3).

%% aihtml_nav_tree
-spec ah_nav_tree([aihtml_nav_tree:item()],
                  iodata() | atom() | undefined,
                  aihtml_html:css(),
                  aihtml_html:attrs()) ->
                     #ah_nav_tree{}.
ah_nav_tree(A1, A2, A3, A4) -> aihtml_nav_tree:ah_nav_tree(A1, A2, A3, A4).

%% aihtml_diff
-spec ah_diff(unicode:chardata(),
              unicode:chardata(),
              aihtml_html:css(),
              aihtml_html:attrs()) ->
                 #ah_diff{}.
ah_diff(A1, A2, A3, A4) -> aihtml_diff:ah_diff(A1, A2, A3, A4).

%% aihtml_heatmap_calendar
-spec ah_heatmap_calendar(aihtml_heatmap_calendar:data(),
                          aihtml_html:css(),
                          aihtml_html:attrs()) ->
                             #ah_heatmap_calendar{}.
ah_heatmap_calendar(A1, A2, A3) -> aihtml_heatmap_calendar:ah_heatmap_calendar(A1, A2, A3).

%% aihtml_datagrid
-spec ah_datagrid([aihtml_datagrid:column()],
                  [aihtml_datagrid:row()],
                  aihtml_html:css(),
                  aihtml_html:attrs()) ->
                     #ah_datagrid{}.
ah_datagrid(A1, A2, A3, A4) -> aihtml_datagrid:ah_datagrid(A1, A2, A3, A4).
-spec datagrid_query(aihtml_action:event()) -> aihtml_datagrid:query().
datagrid_query(A1) -> aihtml_datagrid:datagrid_query(A1).
-spec datagrid_rows(aihtml_action:ctx(),
                    aihtml_action:event(),
                    [aihtml_datagrid:row()],
                    non_neg_integer()) ->
                       ok.
datagrid_rows(A1, A2, A3, A4) -> aihtml_datagrid:datagrid_rows(A1, A2, A3, A4).
-spec datagrid_row(aihtml_action:ctx(),
                   aihtml_action:event(),
                   aihtml_datagrid:row()) ->
                      ok.
datagrid_row(A1, A2, A3) -> aihtml_datagrid:datagrid_row(A1, A2, A3).
-spec datagrid_select(aihtml_datagrid:query(), [aihtml_datagrid:row()]) ->
                         {[aihtml_datagrid:row()], non_neg_integer()}.
datagrid_select(A1, A2) -> aihtml_datagrid:datagrid_select(A1, A2).

%% aihtml_pivotgrid
-spec ah_pivotgrid([aihtml_pivotgrid:row()],
                   aihtml_pivotgrid:layout(),
                   aihtml_html:css(),
                   aihtml_html:attrs()) ->
                      #ah_pivotgrid{}.
ah_pivotgrid(A1, A2, A3, A4) -> aihtml_pivotgrid:ah_pivotgrid(A1, A2, A3, A4).
-spec pivotgrid_rows(aihtml_action:ctx(),
                     aihtml_action:event(),
                     [aihtml_pivotgrid:row()]) ->
                        ok.
pivotgrid_rows(A1, A2, A3) -> aihtml_pivotgrid:pivotgrid_rows(A1, A2, A3).
-spec pivotgrid_view(aihtml_action:event()) ->
                        #{layout := map(), view := map()}.
pivotgrid_view(A1) -> aihtml_pivotgrid:pivotgrid_view(A1).
-spec pivotgrid_cell(aihtml_action:event()) ->
                        #{row := list(),
                          col := list(),
                          filter := map(),
                          field := binary() | null,
                          agg := atom(),
                          value := number() | null,
                          text := binary()}.
pivotgrid_cell(A1) -> aihtml_pivotgrid:pivotgrid_cell(A1).

%% aihtml_treegrid
-spec ah_treegrid([aihtml_treegrid:column()],
                  [aihtml_treegrid:row()],
                  aihtml_html:css(),
                  aihtml_html:attrs()) ->
                     #ah_treegrid{}.
ah_treegrid(A1, A2, A3, A4) -> aihtml_treegrid:ah_treegrid(A1, A2, A3, A4).
-spec treegrid_children(aihtml_action:ctx(),
                        aihtml_action:event(),
                        #ah_treegrid{}) ->
                           ok.
treegrid_children(A1, A2, A3) -> aihtml_treegrid:treegrid_children(A1, A2, A3).

%% aihtml_datatable
-spec ah_datatable([aihtml_datatable:column()],
                   [aihtml_datatable:row()],
                   aihtml_html:css(),
                   aihtml_html:attrs()) ->
                      #ah_datatable{}.
ah_datatable(A1, A2, A3, A4) -> aihtml_datatable:ah_datatable(A1, A2, A3, A4).
-spec datatable_query(aihtml_action:event()) -> aihtml_datatable:query().
datatable_query(A1) -> aihtml_datatable:datatable_query(A1).
-spec datatable_rows(aihtml_action:ctx(),
                     aihtml_action:event(),
                     #ah_datatable{}) ->
                        ok.
datatable_rows(A1, A2, A3) -> aihtml_datatable:datatable_rows(A1, A2, A3).
-spec datatable_row(aihtml_action:ctx(),
                    aihtml_action:event(),
                    #ah_datatable{},
                    aihtml_datatable:row()) ->
                       ok.
datatable_row(A1, A2, A3, A4) -> aihtml_datatable:datatable_row(A1, A2, A3, A4).

%% aihtml_gantt
-spec ah_gantt([aihtml_gantt:task()],
               aihtml_html:css(),
               aihtml_html:attrs()) ->
                  #ah_gantt{}.
ah_gantt(A1, A2, A3) -> aihtml_gantt:ah_gantt(A1, A2, A3).
-spec gantt_update(aihtml_action:ctx(),
                   aihtml_action:event(),
                   #ah_gantt{}) ->
                      ok.
gantt_update(A1, A2, A3) -> aihtml_gantt:gantt_update(A1, A2, A3).

%% aihtml_scheduler
-spec ah_scheduler([aihtml_scheduler:event()],
                   aihtml_lib_date:date(),
                   aihtml_html:css(),
                   aihtml_html:attrs()) ->
                      #ah_scheduler{}.
ah_scheduler(A1, A2, A3, A4) -> aihtml_scheduler:ah_scheduler(A1, A2, A3, A4).
-spec scheduler_range(aihtml_action:event()) ->
                         #{view := aihtml_scheduler:view(),
                           date := binary(),
                           start := binary(),
                           'end' := binary()}.
scheduler_range(A1) -> aihtml_scheduler:scheduler_range(A1).
-spec scheduler_update(aihtml_action:ctx(),
                       aihtml_action:event(),
                       #ah_scheduler{}) ->
                          ok.
scheduler_update(A1, A2, A3) -> aihtml_scheduler:scheduler_update(A1, A2, A3).

%% aihtml_swimlane
-spec ah_swimlane([aihtml_swimlane:item()],
                  aihtml_html:css(),
                  aihtml_html:attrs()) ->
                     #ah_swimlane{}.
ah_swimlane(A1, A2, A3) -> aihtml_swimlane:ah_swimlane(A1, A2, A3).
-spec swimlane_update(aihtml_action:ctx(),
                      aihtml_action:event(),
                      #ah_swimlane{}) ->
                         ok.
swimlane_update(A1, A2, A3) -> aihtml_swimlane:swimlane_update(A1, A2, A3).

%% aihtml_chart
-spec ah_chart(aihtml_chart:option(),
               aihtml_html:css(),
               aihtml_html:attrs()) ->
                  #ah_chart{}.
ah_chart(A1, A2, A3) -> aihtml_chart:ah_chart(A1, A2, A3).
-spec chart_option(aihtml_chart:chart_record()) -> aihtml_chart:option().
chart_option(A1) -> aihtml_chart:chart_option(A1).
-spec chart_update(aihtml_action:ctx(),
                   aihtml_action:target(),
                   aihtml_chart:chart()) ->
                      ok.
chart_update(A1, A2, A3) -> aihtml_chart:chart_update(A1, A2, A3).

%% aihtml_area_chart
-spec ah_area_chart([aihtml_lib_chart:series()],
                    aihtml_html:css(),
                    aihtml_html:attrs()) ->
                       #ah_area_chart{}.
ah_area_chart(A1, A2, A3) -> aihtml_area_chart:ah_area_chart(A1, A2, A3).

%% aihtml_bar_chart
-spec ah_bar_chart([aihtml_lib_chart:series()],
                   aihtml_html:css(),
                   aihtml_html:attrs()) ->
                      #ah_bar_chart{}.
ah_bar_chart(A1, A2, A3) -> aihtml_bar_chart:ah_bar_chart(A1, A2, A3).

%% aihtml_donut_chart
-spec ah_donut_chart([aihtml_donut_chart:item()],
                     aihtml_html:css(),
                     aihtml_html:attrs()) ->
                        #ah_donut_chart{}.
ah_donut_chart(A1, A2, A3) -> aihtml_donut_chart:ah_donut_chart(A1, A2, A3).

%% aihtml_radar_chart
-spec ah_radar_chart([aihtml_lib_chart:series()],
                     aihtml_html:css(),
                     aihtml_html:attrs()) ->
                        #ah_radar_chart{}.
ah_radar_chart(A1, A2, A3) -> aihtml_radar_chart:ah_radar_chart(A1, A2, A3).

%% aihtml_relation_graph
-spec ah_relation_graph(aihtml_relation_graph:graph(),
                        aihtml_html:css(),
                        aihtml_html:attrs()) ->
                           #ah_relation_graph{}.
ah_relation_graph(A1, A2, A3) -> aihtml_relation_graph:ah_relation_graph(A1, A2, A3).

%% aihtml_node_graph
-spec ah_node_graph(aihtml_node_graph:graph(),
                    aihtml_html:css(),
                    aihtml_html:attrs()) ->
                       #ah_node_graph{}.
ah_node_graph(A1, A2, A3) -> aihtml_node_graph:ah_node_graph(A1, A2, A3).
-spec node_graph_layout(aihtml_node_graph:graph()) ->
                           aihtml_node_graph:graph().
node_graph_layout(A1) -> aihtml_node_graph:node_graph_layout(A1).
-spec set_node_graph(aihtml_action:ctx(),
                     aihtml_action:target() | aihtml_action:event(),
                     aihtml_node_graph:graph()) ->
                        ok.
set_node_graph(A1, A2, A3) -> aihtml_node_graph:set_node_graph(A1, A2, A3).

%% aihtml_lib_overlay
-spec opens(aihtml_lib_overlay:target()) -> aihtml_html:attrs().
opens(A1) -> aihtml_lib_overlay:opens(A1).
-spec toggles(aihtml_lib_overlay:target()) -> aihtml_html:attrs().
toggles(A1) -> aihtml_lib_overlay:toggles(A1).
-spec closes() -> aihtml_html:attrs().
closes() -> aihtml_lib_overlay:closes().
-spec closes(aihtml_lib_overlay:target()) -> aihtml_html:attrs().
closes(A1) -> aihtml_lib_overlay:closes(A1).
-spec closes(aihtml_lib_overlay:target() | closest, atom() | iodata()) ->
                aihtml_html:attrs().
closes(A1, A2) -> aihtml_lib_overlay:closes(A1, A2).

%% END GENERATED COMPONENTS
