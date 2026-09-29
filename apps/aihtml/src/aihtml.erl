%%%-------------------------------------------------------------------
%%% @doc aihtml: write HTML pages as Erlang function calls.
%%%
%%% ```
%%% -include_lib("aihtml/include/aihtml.hrl").   % imports this module
%%%
%%% page() ->
%%%     'div'([checkbox(<<"Remember me">>, yes, [], [{name, remember}]),
%%%            button(<<"Save">>, save, [primary, <<"mt-4">>], [{type, submit}])],
%%%           [<<"flex flex-col gap-2">>], [{id, login}]).
%%% '''
%%%
%%% `div' is an Erlang operator, so that one tag is written `'div''.
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
-export(['div'/1, 'div'/3, span/1, span/3, p/1, p/3, a/1, a/3,
         h1/1, h1/3, h2/1, h2/3, h3/1, h3/3, h4/1, h4/3,
         ul/1, ul/3, ol/1, ol/3, li/1, li/3, dl/1, dl/3, dt/1, dt/3, dd/1, dd/3,
         section/1, section/3, article/1, article/3, aside/1, aside/3,
         header/1, header/3, footer/1, footer/3, nav/1, nav/3, main/1, main/3,
         form/1, form/3, fieldset/1, fieldset/3, legend/1, legend/3, label/1, label/3,
         strong/1, strong/3, em/1, em/3, small/1, small/3, code/1, code/3,
         pre/1, pre/3, blockquote/1, blockquote/3,
         table/1, table/3, thead/1, thead/3, tbody/1, tbody/3,
         tr/1, tr/3, th/1, th/3, td/1, td/3]).
%% Void tags.
-export([br/0, hr/2, img/2]).
%% Escape hatches and rendering.
-export([el/4, void/3, text/1, safe/1, render/1, render_binary/1, page/2]).
%% Server round trips for the jQuery runtime.
-export([fetch/3, fetch/4]).
%% Browser events that call Erlang actions (see aihtml_action).
-export([on/2, on/3, preserve/0]).
%% Server push (see aihtml_push).
-export([subscribe/1, subscribe/2]).
%% Theme switcher; the component exports are generated below.
-export([theme_switcher/2]).
%% BEGIN GENERATED EXPORTS
-export([button/4,
         link_button/4,
         toggle_button/4,
         button_group/4,
         segmented_control/4,
         dropdown_button/4,
         split_button/4,
         checkbox/4,
         radiobutton/4,
         switch_button/4,
         checkbox_group/4,
         radiobutton_group/4,
         radio_cards/4,
         rating_group/4,
         input/3,
         textarea/3,
         password_input/3,
         number_input/3,
         input_otp/4,
         tag_input/3,
         dropdownlist/4,
         select/4,
         slider/4,
         field/4,
         form_layout/4,
         validate/1,
         datepicker/3,
         combobox/4,
         set_items/3,
         set_items/4,
         timepicker/3,
         colorpicker/3,
         calendar/3,
         set_events/3,
         add_event/3,
         datetime_input/3,
         cascader/4,
         cascader_children/3,
         cascader_children/4,
         listbox/4,
         listbox_items/3,
         listbox_items/4,
         transfer/4,
         masked_input/3,
         formatted_input/3,
         range_selector/4,
         repeat_button/4,
         upload/3,
         uploaded_files/1,
         card/3,
         panel/3,
         expander/3,
         tabs/4,
         tab_bar/4,
         breadcrumbs/3,
         pagination/4,
         steps/4,
         skeleton/2,
         loader/2,
         empty/3,
         menu/4,
         navbar/4,
         sidenav/4,
         toolbar/3,
         splitter/3,
         listmenu/4,
         status_bar/3,
         scrollview/3,
         scrollbar/3,
         responsive_panel/3,
         activity_bar/4,
         navigationbar/4,
         command/3,
         set_command_items/3,
         sortable/4,
         dragdrop/3,
         draggable_attrs/2,
         drop_zone_attrs/2,
         docking/3,
         dock_layout/3,
         docking_add_window/4,
         dock_layout_open/3,
         dock_layout_open/4,
         ribbon/4,
         tile_layout/4,
         tooltip/4,
         tooltip_attrs/2,
         popover/3,
         drawer/3,
         sheet/3,
         window/3,
         notification/3,
         shows_toast/2,
         opens/1,
         toggles/1,
         closes/0,
         closes/1,
         closes/2,
         toast/3,
         avatar/3,
         badge/3,
         chip/3,
         aspect_ratio/3,
         kbd/3,
         time_ago/3,
         expandable_text/3,
         alert/3,
         progressbar/3,
         progress_circle/3,
         meter/3,
         statistic/3,
         kpi_card/3,
         timeline/3,
         ranking_list/3,
         tag_cloud/3,
         tree/4,
         nav_tree/4,
         diff/4,
         heatmap_calendar/3,
         set_children/3,
         datagrid/4,
         datagrid_query/1,
         datagrid_rows/4,
         datagrid_row/3,
         datagrid_select/2,
         pivotgrid/4,
         pivotgrid_rows/3,
         pivotgrid_view/1,
         pivotgrid_cell/1,
         treegrid/4,
         datatable/4,
         treegrid_children/3,
         datatable_query/1,
         datatable_rows/3,
         datatable_row/4,
         gantt/3,
         gantt_update/3,
         scheduler/4,
         scheduler_range/1,
         scheduler_update/3,
         swimlane/3,
         swimlane_update/3,
         chart/3,
         area_chart/3,
         bar_chart/3,
         donut_chart/3,
         radar_chart/3,
         relation_graph/3,
         chart_option/1,
         chart_update/3,
         node_graph/3,
         node_graph_layout/1,
         set_node_graph/3]).
%% END GENERATED EXPORTS

-export_type([html/0, element/0, css/0, attrs/0]).

-type html() :: aihtml_html:html().
-type element() :: aihtml_html:element().
-type css() :: aihtml_html:css().
-type attrs() :: aihtml_html:attrs().

%% One /1 and one /3 function per tag (a macro cannot define functions).
-spec 'div'(html()) -> element().
'div'(C) -> aihtml_html:el('div', C, [], []).
-spec 'div'(html(), css(), attrs()) -> element().
'div'(C, Css, A) -> aihtml_html:el('div', C, Css, A).

-spec span(html()) -> element().
span(C) -> aihtml_html:el(span, C, [], []).
-spec span(html(), css(), attrs()) -> element().
span(C, Css, A) -> aihtml_html:el(span, C, Css, A).

-spec p(html()) -> element().
p(C) -> aihtml_html:el(p, C, [], []).
-spec p(html(), css(), attrs()) -> element().
p(C, Css, A) -> aihtml_html:el(p, C, Css, A).

-spec a(html()) -> element().
a(C) -> aihtml_html:el(a, C, [], []).
-spec a(html(), css(), attrs()) -> element().
a(C, Css, A) -> aihtml_html:el(a, C, Css, A).

-spec h1(html()) -> element().
h1(C) -> aihtml_html:el(h1, C, [], []).
-spec h1(html(), css(), attrs()) -> element().
h1(C, Css, A) -> aihtml_html:el(h1, C, Css, A).

-spec h2(html()) -> element().
h2(C) -> aihtml_html:el(h2, C, [], []).
-spec h2(html(), css(), attrs()) -> element().
h2(C, Css, A) -> aihtml_html:el(h2, C, Css, A).

-spec h3(html()) -> element().
h3(C) -> aihtml_html:el(h3, C, [], []).
-spec h3(html(), css(), attrs()) -> element().
h3(C, Css, A) -> aihtml_html:el(h3, C, Css, A).

-spec h4(html()) -> element().
h4(C) -> aihtml_html:el(h4, C, [], []).
-spec h4(html(), css(), attrs()) -> element().
h4(C, Css, A) -> aihtml_html:el(h4, C, Css, A).

-spec ul(html()) -> element().
ul(C) -> aihtml_html:el(ul, C, [], []).
-spec ul(html(), css(), attrs()) -> element().
ul(C, Css, A) -> aihtml_html:el(ul, C, Css, A).

-spec ol(html()) -> element().
ol(C) -> aihtml_html:el(ol, C, [], []).
-spec ol(html(), css(), attrs()) -> element().
ol(C, Css, A) -> aihtml_html:el(ol, C, Css, A).

-spec li(html()) -> element().
li(C) -> aihtml_html:el(li, C, [], []).
-spec li(html(), css(), attrs()) -> element().
li(C, Css, A) -> aihtml_html:el(li, C, Css, A).

-spec dl(html()) -> element().
dl(C) -> aihtml_html:el(dl, C, [], []).
-spec dl(html(), css(), attrs()) -> element().
dl(C, Css, A) -> aihtml_html:el(dl, C, Css, A).

-spec dt(html()) -> element().
dt(C) -> aihtml_html:el(dt, C, [], []).
-spec dt(html(), css(), attrs()) -> element().
dt(C, Css, A) -> aihtml_html:el(dt, C, Css, A).

-spec dd(html()) -> element().
dd(C) -> aihtml_html:el(dd, C, [], []).
-spec dd(html(), css(), attrs()) -> element().
dd(C, Css, A) -> aihtml_html:el(dd, C, Css, A).

-spec section(html()) -> element().
section(C) -> aihtml_html:el(section, C, [], []).
-spec section(html(), css(), attrs()) -> element().
section(C, Css, A) -> aihtml_html:el(section, C, Css, A).

-spec article(html()) -> element().
article(C) -> aihtml_html:el(article, C, [], []).
-spec article(html(), css(), attrs()) -> element().
article(C, Css, A) -> aihtml_html:el(article, C, Css, A).

-spec aside(html()) -> element().
aside(C) -> aihtml_html:el(aside, C, [], []).
-spec aside(html(), css(), attrs()) -> element().
aside(C, Css, A) -> aihtml_html:el(aside, C, Css, A).

-spec header(html()) -> element().
header(C) -> aihtml_html:el(header, C, [], []).
-spec header(html(), css(), attrs()) -> element().
header(C, Css, A) -> aihtml_html:el(header, C, Css, A).

-spec footer(html()) -> element().
footer(C) -> aihtml_html:el(footer, C, [], []).
-spec footer(html(), css(), attrs()) -> element().
footer(C, Css, A) -> aihtml_html:el(footer, C, Css, A).

-spec nav(html()) -> element().
nav(C) -> aihtml_html:el(nav, C, [], []).
-spec nav(html(), css(), attrs()) -> element().
nav(C, Css, A) -> aihtml_html:el(nav, C, Css, A).

-spec main(html()) -> element().
main(C) -> aihtml_html:el(main, C, [], []).
-spec main(html(), css(), attrs()) -> element().
main(C, Css, A) -> aihtml_html:el(main, C, Css, A).

-spec form(html()) -> element().
form(C) -> aihtml_html:el(form, C, [], []).
-spec form(html(), css(), attrs()) -> element().
form(C, Css, A) -> aihtml_html:el(form, C, Css, A).

-spec fieldset(html()) -> element().
fieldset(C) -> aihtml_html:el(fieldset, C, [], []).
-spec fieldset(html(), css(), attrs()) -> element().
fieldset(C, Css, A) -> aihtml_html:el(fieldset, C, Css, A).

-spec legend(html()) -> element().
legend(C) -> aihtml_html:el(legend, C, [], []).
-spec legend(html(), css(), attrs()) -> element().
legend(C, Css, A) -> aihtml_html:el(legend, C, Css, A).

-spec label(html()) -> element().
label(C) -> aihtml_html:el(label, C, [], []).
-spec label(html(), css(), attrs()) -> element().
label(C, Css, A) -> aihtml_html:el(label, C, Css, A).

-spec strong(html()) -> element().
strong(C) -> aihtml_html:el(strong, C, [], []).
-spec strong(html(), css(), attrs()) -> element().
strong(C, Css, A) -> aihtml_html:el(strong, C, Css, A).

-spec em(html()) -> element().
em(C) -> aihtml_html:el(em, C, [], []).
-spec em(html(), css(), attrs()) -> element().
em(C, Css, A) -> aihtml_html:el(em, C, Css, A).

-spec small(html()) -> element().
small(C) -> aihtml_html:el(small, C, [], []).
-spec small(html(), css(), attrs()) -> element().
small(C, Css, A) -> aihtml_html:el(small, C, Css, A).

-spec code(html()) -> element().
code(C) -> aihtml_html:el(code, C, [], []).
-spec code(html(), css(), attrs()) -> element().
code(C, Css, A) -> aihtml_html:el(code, C, Css, A).

-spec pre(html()) -> element().
pre(C) -> aihtml_html:el(pre, C, [], []).
-spec pre(html(), css(), attrs()) -> element().
pre(C, Css, A) -> aihtml_html:el(pre, C, Css, A).

-spec blockquote(html()) -> element().
blockquote(C) -> aihtml_html:el(blockquote, C, [], []).
-spec blockquote(html(), css(), attrs()) -> element().
blockquote(C, Css, A) -> aihtml_html:el(blockquote, C, Css, A).

-spec table(html()) -> element().
table(C) -> aihtml_html:el(table, C, [], []).
-spec table(html(), css(), attrs()) -> element().
table(C, Css, A) -> aihtml_html:el(table, C, Css, A).

-spec thead(html()) -> element().
thead(C) -> aihtml_html:el(thead, C, [], []).
-spec thead(html(), css(), attrs()) -> element().
thead(C, Css, A) -> aihtml_html:el(thead, C, Css, A).

-spec tbody(html()) -> element().
tbody(C) -> aihtml_html:el(tbody, C, [], []).
-spec tbody(html(), css(), attrs()) -> element().
tbody(C, Css, A) -> aihtml_html:el(tbody, C, Css, A).

-spec tr(html()) -> element().
tr(C) -> aihtml_html:el(tr, C, [], []).
-spec tr(html(), css(), attrs()) -> element().
tr(C, Css, A) -> aihtml_html:el(tr, C, Css, A).

-spec th(html()) -> element().
th(C) -> aihtml_html:el(th, C, [], []).
-spec th(html(), css(), attrs()) -> element().
th(C, Css, A) -> aihtml_html:el(th, C, Css, A).

-spec td(html()) -> element().
td(C) -> aihtml_html:el(td, C, [], []).
-spec td(html(), css(), attrs()) -> element().
td(C, Css, A) -> aihtml_html:el(td, C, Css, A).

-spec br() -> element().
br() -> aihtml_html:void(br, [], []).

-spec hr(css(), attrs()) -> element().
hr(Css, A) -> aihtml_html:void(hr, Css, A).

-spec img(css(), attrs()) -> element().
img(Css, A) -> aihtml_html:void(img, Css, A).

%% @doc Any element, for tags without a helper.
-spec el(atom() | binary(), html(), css(), attrs()) -> element().
el(Tag, Children, Css, Attrs) -> aihtml_html:el(Tag, Children, Css, Attrs).

-spec void(atom() | binary(), css(), attrs()) -> element().
void(Tag, Css, Attrs) -> aihtml_html:void(Tag, Css, Attrs).

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
%% Attrs list: `button(<<"More">>, more, [], [fetch(get, <<"/more">>, <<"#list">>)])'.
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
%% `button(<<"Save">>, save, [], [on(click, {?MODULE, save, #{id => 7}})])'.
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
    E = beamai_html_escape:to_binary(Event, aihtml),
    %% DOM events (click, change, ...) or component events (ah:close, ...)
    re:run(E, <<"^(ah:)?[a-z][a-z-]*$">>) =/= nomatch
        orelse error({aihtml, {bad_event_name, Event}}),
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
%% component). Splice into Attrs: `'div'(Player, [], [{id, player}, preserve()])'.
-spec preserve() -> attrs().
preserve() -> [{data_ah_preserve, true}].

%% @doc Follow a push topic, spliced into the Attrs of the element whose
%% content the topic updates: `ul(Items, [], [{id, list}, subscribe(todos)])'.
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

-spec theme_switcher(css(), attrs()) -> element().
theme_switcher(Css, Attrs) -> aihtml_theme:switcher(Css, Attrs).

%% BEGIN GENERATED COMPONENTS
%% aihtml_form_buttons
-spec button(aihtml_html:html(),
             term(),
             aihtml_html:css(),
             aihtml_html:attrs()) ->
                #ah_button{}.
button(A1, A2, A3, A4) -> aihtml_form_buttons:button(A1, A2, A3, A4).
-spec link_button(aihtml_html:html(),
                  iodata() | undefined,
                  aihtml_html:css(),
                  aihtml_html:attrs()) ->
                     #ah_link_button{}.
link_button(A1, A2, A3, A4) -> aihtml_form_buttons:link_button(A1, A2, A3, A4).
-spec toggle_button(aihtml_html:html(),
                    boolean(),
                    aihtml_html:css(),
                    aihtml_html:attrs()) ->
                       #ah_toggle_button{}.
toggle_button(A1, A2, A3, A4) -> aihtml_form_buttons:toggle_button(A1, A2, A3, A4).
-spec button_group([aihtml_form_buttons:item()],
                   term(),
                   aihtml_html:css(),
                   aihtml_html:attrs()) ->
                      #ah_button_group{}.
button_group(A1, A2, A3, A4) -> aihtml_form_buttons:button_group(A1, A2, A3, A4).
-spec segmented_control([aihtml_form_buttons:item()],
                        term(),
                        aihtml_html:css(),
                        aihtml_html:attrs()) ->
                           #ah_segmented_control{}.
segmented_control(A1, A2, A3, A4) -> aihtml_form_buttons:segmented_control(A1, A2, A3, A4).
-spec dropdown_button(aihtml_html:html(),
                      [aihtml_form_buttons:item()],
                      aihtml_html:css(),
                      aihtml_html:attrs()) ->
                         #ah_dropdown_button{}.
dropdown_button(A1, A2, A3, A4) -> aihtml_form_buttons:dropdown_button(A1, A2, A3, A4).
-spec split_button(aihtml_html:html(),
                   [aihtml_form_buttons:item()],
                   aihtml_html:css(),
                   aihtml_html:attrs()) ->
                      #ah_split_button{}.
split_button(A1, A2, A3, A4) -> aihtml_form_buttons:split_button(A1, A2, A3, A4).

%% aihtml_form_choice
-spec checkbox(aihtml_html:html(),
               aihtml_form_choice:value() | undefined,
               aihtml_html:css(),
               aihtml_html:attrs()) ->
                  #ah_checkbox{}.
checkbox(A1, A2, A3, A4) -> aihtml_form_choice:checkbox(A1, A2, A3, A4).
-spec radiobutton(aihtml_html:html(),
                  aihtml_form_choice:value() | undefined,
                  aihtml_html:css(),
                  aihtml_html:attrs()) ->
                     #ah_radiobutton{}.
radiobutton(A1, A2, A3, A4) -> aihtml_form_choice:radiobutton(A1, A2, A3, A4).
-spec switch_button(aihtml_html:html(),
                    aihtml_form_choice:value() | undefined,
                    aihtml_html:css(),
                    aihtml_html:attrs()) ->
                       #ah_switch_button{}.
switch_button(A1, A2, A3, A4) -> aihtml_form_choice:switch_button(A1, A2, A3, A4).
-spec checkbox_group([aihtml_form_choice:item()],
                     [aihtml_form_choice:value()],
                     aihtml_html:css(),
                     aihtml_html:attrs()) ->
                        #ah_checkbox_group{}.
checkbox_group(A1, A2, A3, A4) -> aihtml_form_choice:checkbox_group(A1, A2, A3, A4).
-spec radiobutton_group([aihtml_form_choice:item()],
                        aihtml_form_choice:value() | undefined,
                        aihtml_html:css(),
                        aihtml_html:attrs()) ->
                           #ah_radiobutton_group{}.
radiobutton_group(A1, A2, A3, A4) -> aihtml_form_choice:radiobutton_group(A1, A2, A3, A4).
-spec radio_cards([aihtml_form_choice:item()],
                  aihtml_form_choice:value() | undefined,
                  aihtml_html:css(),
                  aihtml_html:attrs()) ->
                     #ah_radio_cards{}.
radio_cards(A1, A2, A3, A4) -> aihtml_form_choice:radio_cards(A1, A2, A3, A4).
-spec rating_group(pos_integer(),
                   number() | undefined,
                   aihtml_html:css(),
                   aihtml_html:attrs()) ->
                      #ah_rating_group{}.
rating_group(A1, A2, A3, A4) -> aihtml_form_choice:rating_group(A1, A2, A3, A4).

%% aihtml_form_text
-spec input(binary() | undefined,
            aihtml_html:css(),
            aihtml_html:attrs()) ->
               #ah_input{}.
input(A1, A2, A3) -> aihtml_form_text:input(A1, A2, A3).
-spec textarea(binary() | undefined,
               aihtml_html:css(),
               aihtml_html:attrs()) ->
                  #ah_textarea{}.
textarea(A1, A2, A3) -> aihtml_form_text:textarea(A1, A2, A3).
-spec password_input(binary() | undefined,
                     aihtml_html:css(),
                     aihtml_html:attrs()) ->
                        #ah_password_input{}.
password_input(A1, A2, A3) -> aihtml_form_text:password_input(A1, A2, A3).
-spec number_input(number() | binary() | undefined,
                   aihtml_html:css(),
                   aihtml_html:attrs()) ->
                      #ah_number_input{}.
number_input(A1, A2, A3) -> aihtml_form_text:number_input(A1, A2, A3).
-spec input_otp(pos_integer(),
                binary() | undefined,
                aihtml_html:css(),
                aihtml_html:attrs()) ->
                   #ah_input_otp{}.
input_otp(A1, A2, A3, A4) -> aihtml_form_text:input_otp(A1, A2, A3, A4).
-spec tag_input([binary()], aihtml_html:css(), aihtml_html:attrs()) ->
                   #ah_tag_input{}.
tag_input(A1, A2, A3) -> aihtml_form_text:tag_input(A1, A2, A3).

%% aihtml_form_select
-spec dropdownlist([aihtml_form_select:item()],
                   binary() | atom() | number() | undefined,
                   aihtml_html:css(),
                   aihtml_html:attrs()) ->
                      #ah_dropdownlist{}.
dropdownlist(A1, A2, A3, A4) -> aihtml_form_select:dropdownlist(A1, A2, A3, A4).
-spec select([aihtml_form_select:item()],
             binary() | atom() | number() |
             [binary() | atom() | number()] |
             undefined,
             aihtml_html:css(),
             aihtml_html:attrs()) ->
                #ah_select{}.
select(A1, A2, A3, A4) -> aihtml_form_select:select(A1, A2, A3, A4).
-spec slider({number(), number()} | {number(), number(), number()},
             number() | {number(), number()} | undefined,
             aihtml_html:css(),
             aihtml_html:attrs()) ->
                #ah_slider{}.
slider(A1, A2, A3, A4) -> aihtml_form_select:slider(A1, A2, A3, A4).
-spec field(aihtml_html:html(),
            aihtml_html:html(),
            aihtml_html:css(),
            aihtml_html:attrs()) ->
               #ah_field{}.
field(A1, A2, A3, A4) -> aihtml_form_select:field(A1, A2, A3, A4).
-spec form_layout([aihtml_form_select:field_spec()],
                  #{term() => term()},
                  aihtml_html:css(),
                  aihtml_html:attrs()) ->
                     #ah_form_layout{}.
form_layout(A1, A2, A3, A4) -> aihtml_form_select:form_layout(A1, A2, A3, A4).
-spec validate([aihtml_form_select:rule() |
                {hint | position | on, term()}]) ->
                  aihtml_html:attrs().
validate(A1) -> aihtml_form_select:validate(A1).

%% aihtml_form_pickers
-spec datepicker(aihtml_form_pickers:date_value(),
                 aihtml_html:css(),
                 aihtml_html:attrs()) ->
                    #ah_datepicker{}.
datepicker(A1, A2, A3) -> aihtml_form_pickers:datepicker(A1, A2, A3).
-spec combobox([aihtml_form_pickers:item()],
               term() | [term()] | undefined,
               aihtml_html:css(),
               aihtml_html:attrs()) ->
                  #ah_combobox{}.
combobox(A1, A2, A3, A4) -> aihtml_form_pickers:combobox(A1, A2, A3, A4).
-spec set_items(aihtml_action:ctx(),
                {id, iodata() | atom()} | aihtml_action:event(),
                [aihtml_form_pickers:item()]) ->
                   ok.
set_items(A1, A2, A3) -> aihtml_form_pickers:set_items(A1, A2, A3).
-spec set_items(aihtml_action:ctx(),
                {id, iodata() | atom()},
                [aihtml_form_pickers:item()],
                #{checkboxes => boolean(), selected => [term()]}) ->
                   ok.
set_items(A1, A2, A3, A4) -> aihtml_form_pickers:set_items(A1, A2, A3, A4).

%% aihtml_form_time_color
-spec timepicker(binary() | string() | tuple() | undefined,
                 aihtml_html:css(),
                 aihtml_html:attrs()) ->
                    #ah_timepicker{}.
timepicker(A1, A2, A3) -> aihtml_form_time_color:timepicker(A1, A2, A3).
-spec colorpicker(binary() | string() | tuple() | undefined,
                  aihtml_html:css(),
                  aihtml_html:attrs()) ->
                     #ah_colorpicker{}.
colorpicker(A1, A2, A3) -> aihtml_form_time_color:colorpicker(A1, A2, A3).

%% aihtml_form_calendar
-spec calendar(binary() | string() | calendar:date() | undefined,
               aihtml_html:css(),
               aihtml_html:attrs()) ->
                  #ah_calendar{}.
calendar(A1, A2, A3) -> aihtml_form_calendar:calendar(A1, A2, A3).
-spec set_events(aihtml_action:ctx(),
                 {id, iodata() | atom()} | aihtml_action:event(),
                 [aihtml_form_calendar:event()]) ->
                    ok.
set_events(A1, A2, A3) -> aihtml_form_calendar:set_events(A1, A2, A3).
-spec add_event(aihtml_action:ctx(),
                {id, iodata() | atom()} | aihtml_action:event(),
                aihtml_form_calendar:event()) ->
                   ok.
add_event(A1, A2, A3) -> aihtml_form_calendar:add_event(A1, A2, A3).
-spec datetime_input(binary() |
                     string() |
                     calendar:date() |
                     calendar:datetime() |
                     undefined,
                     aihtml_html:css(),
                     aihtml_html:attrs()) ->
                        #ah_datetime_input{}.
datetime_input(A1, A2, A3) -> aihtml_form_calendar:datetime_input(A1, A2, A3).

%% aihtml_form_lists
-spec cascader([aihtml_form_lists:cascader_node()],
               [term()] | undefined,
               aihtml_html:css(),
               aihtml_html:attrs()) ->
                  #ah_cascader{}.
cascader(A1, A2, A3, A4) -> aihtml_form_lists:cascader(A1, A2, A3, A4).
-spec cascader_children(aihtml_action:ctx(),
                        aihtml_action:event(),
                        [aihtml_form_lists:cascader_node()]) ->
                           ok.
cascader_children(A1, A2, A3) -> aihtml_form_lists:cascader_children(A1, A2, A3).
-spec cascader_children(aihtml_action:ctx(),
                        {id, iodata() | atom()},
                        [term()],
                        [aihtml_form_lists:cascader_node()]) ->
                           ok.
cascader_children(A1, A2, A3, A4) -> aihtml_form_lists:cascader_children(A1, A2, A3, A4).
-spec listbox([aihtml_form_lists:item()],
              term() | [term()] | undefined,
              aihtml_html:css(),
              aihtml_html:attrs()) ->
                 #ah_listbox{}.
listbox(A1, A2, A3, A4) -> aihtml_form_lists:listbox(A1, A2, A3, A4).
-spec listbox_items(aihtml_action:ctx(),
                    aihtml_action:event() | {id, iodata() | atom()},
                    [aihtml_form_lists:item()]) ->
                       ok.
listbox_items(A1, A2, A3) -> aihtml_form_lists:listbox_items(A1, A2, A3).
-spec listbox_items(aihtml_action:ctx(),
                    {id, iodata() | atom()},
                    [aihtml_form_lists:item()],
                    #{checkboxes => boolean(), selected => [term()]}) ->
                       ok.
listbox_items(A1, A2, A3, A4) -> aihtml_form_lists:listbox_items(A1, A2, A3, A4).
-spec transfer([aihtml_form_lists:item()],
               [term()],
               aihtml_html:css(),
               aihtml_html:attrs()) ->
                  #ah_transfer{}.
transfer(A1, A2, A3, A4) -> aihtml_form_lists:transfer(A1, A2, A3, A4).

%% aihtml_form_entry
-spec masked_input(unicode:chardata() | undefined,
                   aihtml_html:css(),
                   aihtml_html:attrs()) ->
                      #ah_masked_input{}.
masked_input(A1, A2, A3) -> aihtml_form_entry:masked_input(A1, A2, A3).
-spec formatted_input(integer() | binary() | string(),
                      aihtml_html:css(),
                      aihtml_html:attrs()) ->
                         #ah_formatted_input{}.
formatted_input(A1, A2, A3) -> aihtml_form_entry:formatted_input(A1, A2, A3).
-spec range_selector({number(), number()} |
                     {number(), number(), number()},
                     {number(), number()} | undefined,
                     aihtml_html:css(),
                     aihtml_html:attrs()) ->
                        #ah_range_selector{}.
range_selector(A1, A2, A3, A4) -> aihtml_form_entry:range_selector(A1, A2, A3, A4).
-spec repeat_button(aihtml_html:html(),
                    term(),
                    aihtml_html:css(),
                    aihtml_html:attrs()) ->
                       #ah_repeat_button{}.
repeat_button(A1, A2, A3, A4) -> aihtml_form_entry:repeat_button(A1, A2, A3, A4).

%% aihtml_form_upload
-spec upload([aihtml_form_upload:file()],
             aihtml_html:css(),
             aihtml_html:attrs()) ->
                #ah_upload{}.
upload(A1, A2, A3) -> aihtml_form_upload:upload(A1, A2, A3).
-spec uploaded_files(aihtml_action:event() | binary()) -> [term()].
uploaded_files(A1) -> aihtml_form_upload:uploaded_files(A1).

%% aihtml_layout_basic
-spec card(aihtml_html:html(), aihtml_html:css(), aihtml_html:attrs()) ->
              #ah_card{}.
card(A1, A2, A3) -> aihtml_layout_basic:card(A1, A2, A3).
-spec panel(aihtml_html:html(), aihtml_html:css(), aihtml_html:attrs()) ->
               #ah_panel{}.
panel(A1, A2, A3) -> aihtml_layout_basic:panel(A1, A2, A3).
-spec expander(aihtml_html:html(),
               aihtml_html:css(),
               aihtml_html:attrs()) ->
                  #ah_expander{}.
expander(A1, A2, A3) -> aihtml_layout_basic:expander(A1, A2, A3).
-spec tabs([{aihtml_layout_basic:key(),
             aihtml_html:html(),
             aihtml_html:html()} |
            {aihtml_layout_basic:key(),
             aihtml_html:html(),
             aihtml_html:html(),
             map()}],
           aihtml_layout_basic:key() | undefined,
           aihtml_html:css(),
           aihtml_html:attrs()) ->
              #ah_tabs{}.
tabs(A1, A2, A3, A4) -> aihtml_layout_basic:tabs(A1, A2, A3, A4).
-spec tab_bar([{aihtml_layout_basic:key(), aihtml_html:html()} |
               {aihtml_layout_basic:key(), aihtml_html:html(), map()}],
              aihtml_layout_basic:key() | undefined,
              aihtml_html:css(),
              aihtml_html:attrs()) ->
                 #ah_tab_bar{}.
tab_bar(A1, A2, A3, A4) -> aihtml_layout_basic:tab_bar(A1, A2, A3, A4).
-spec breadcrumbs([aihtml_html:html() |
                   {aihtml_html:html(), binary() | undefined} |
                   map()],
                  aihtml_html:css(),
                  aihtml_html:attrs()) ->
                     #ah_breadcrumbs{}.
breadcrumbs(A1, A2, A3) -> aihtml_layout_basic:breadcrumbs(A1, A2, A3).
-spec pagination(non_neg_integer(),
                 pos_integer(),
                 aihtml_html:css(),
                 aihtml_html:attrs()) ->
                    #ah_pagination{}.
pagination(A1, A2, A3, A4) -> aihtml_layout_basic:pagination(A1, A2, A3, A4).
-spec steps([aihtml_html:html() |
             {aihtml_html:html(), aihtml_html:html()} |
             map()],
            non_neg_integer(),
            aihtml_html:css(),
            aihtml_html:attrs()) ->
               #ah_steps{}.
steps(A1, A2, A3, A4) -> aihtml_layout_basic:steps(A1, A2, A3, A4).
-spec skeleton(aihtml_html:css(), aihtml_html:attrs()) -> #ah_skeleton{}.
skeleton(A1, A2) -> aihtml_layout_basic:skeleton(A1, A2).
-spec loader(aihtml_html:css(), aihtml_html:attrs()) -> #ah_loader{}.
loader(A1, A2) -> aihtml_layout_basic:loader(A1, A2).
-spec empty(aihtml_html:html(), aihtml_html:css(), aihtml_html:attrs()) ->
               #ah_empty{}.
empty(A1, A2, A3) -> aihtml_layout_basic:empty(A1, A2, A3).

%% aihtml_layout_nav
-spec menu([aihtml_layout_nav:item()],
           aihtml_layout_nav:key() | undefined,
           aihtml_html:css(),
           aihtml_html:attrs()) ->
              #ah_menu{}.
menu(A1, A2, A3, A4) -> aihtml_layout_nav:menu(A1, A2, A3, A4).
-spec navbar([aihtml_layout_nav:item()],
             aihtml_layout_nav:key() | undefined,
             aihtml_html:css(),
             aihtml_html:attrs()) ->
                #ah_navbar{}.
navbar(A1, A2, A3, A4) -> aihtml_layout_nav:navbar(A1, A2, A3, A4).
-spec sidenav([#{label => aihtml_html:html(),
                 items := [aihtml_layout_nav:item()]}] |
              [aihtml_layout_nav:item()],
              aihtml_layout_nav:key() | undefined,
              aihtml_html:css(),
              aihtml_html:attrs()) ->
                 #ah_sidenav{}.
sidenav(A1, A2, A3, A4) -> aihtml_layout_nav:sidenav(A1, A2, A3, A4).
-spec toolbar([aihtml_layout_nav:tool()],
              aihtml_html:css(),
              aihtml_html:attrs()) ->
                 #ah_toolbar{}.
toolbar(A1, A2, A3) -> aihtml_layout_nav:toolbar(A1, A2, A3).
-spec splitter([aihtml_layout_nav:pane()],
               aihtml_html:css(),
               aihtml_html:attrs()) ->
                  #ah_splitter{}.
splitter(A1, A2, A3) -> aihtml_layout_nav:splitter(A1, A2, A3).
-spec listmenu([aihtml_layout_nav:item()],
               aihtml_layout_nav:key() | undefined,
               aihtml_html:css(),
               aihtml_html:attrs()) ->
                  #ah_listmenu{}.
listmenu(A1, A2, A3, A4) -> aihtml_layout_nav:listmenu(A1, A2, A3, A4).
-spec status_bar([aihtml_layout_nav:segment()],
                 aihtml_html:css(),
                 aihtml_html:attrs()) ->
                    #ah_status_bar{}.
status_bar(A1, A2, A3) -> aihtml_layout_nav:status_bar(A1, A2, A3).

%% aihtml_layout_scroll
-spec scrollview([aihtml_html:html()],
                 aihtml_html:css(),
                 aihtml_html:attrs()) ->
                    #ah_scrollview{}.
scrollview(A1, A2, A3) -> aihtml_layout_scroll:scrollview(A1, A2, A3).
-spec scrollbar(aihtml_html:html(),
                aihtml_html:css(),
                aihtml_html:attrs()) ->
                   #ah_scrollbar{}.
scrollbar(A1, A2, A3) -> aihtml_layout_scroll:scrollbar(A1, A2, A3).
-spec responsive_panel(aihtml_html:html(),
                       aihtml_html:css(),
                       aihtml_html:attrs()) ->
                          #ah_responsive_panel{}.
responsive_panel(A1, A2, A3) -> aihtml_layout_scroll:responsive_panel(A1, A2, A3).

%% aihtml_layout_bars
-spec activity_bar([{term(), aihtml_html:html(), iodata()} |
                    {term(),
                     aihtml_html:html(),
                     iodata(),
                     aihtml_html:attrs()} |
                    divider],
                   term(),
                   aihtml_html:css(),
                   aihtml_html:attrs()) ->
                      #ah_activity_bar{}.
activity_bar(A1, A2, A3, A4) -> aihtml_layout_bars:activity_bar(A1, A2, A3, A4).
-spec navigationbar([{aihtml_html:html() |
                      #{title := aihtml_html:html(),
                        subheader => aihtml_html:html(),
                        extra => aihtml_html:html()},
                      aihtml_html:html()} |
                     {aihtml_html:html() |
                      #{title := aihtml_html:html(),
                        subheader => aihtml_html:html(),
                        extra => aihtml_html:html()},
                      aihtml_html:html(),
                      aihtml_html:attrs()} |
                     #{header :=
                           aihtml_html:html() |
                           #{title := aihtml_html:html(),
                             subheader => aihtml_html:html(),
                             extra => aihtml_html:html()},
                       content => aihtml_html:html(),
                       actions => aihtml_html:html(),
                       disabled => boolean()}],
                    undefined |
                    non_neg_integer() |
                    [non_neg_integer()] |
                    binary(),
                    aihtml_html:css(),
                    aihtml_html:attrs()) ->
                       #ah_navigationbar{}.
navigationbar(A1, A2, A3, A4) -> aihtml_layout_bars:navigationbar(A1, A2, A3, A4).
-spec command([iodata() |
               atom() |
               integer() |
               {term(), aihtml_html:html()} |
               #{value := term(),
                 label => aihtml_html:html(),
                 description => aihtml_html:html(),
                 icon => aihtml_html:html(),
                 shortcut => aihtml_html:html(),
                 href => iodata(),
                 disabled => boolean()} |
               #{heading => aihtml_html:html(),
                 items :=
                     [iodata() |
                      atom() |
                      integer() |
                      {term(), aihtml_html:html()} |
                      #{value := term(),
                        label => aihtml_html:html(),
                        description => aihtml_html:html(),
                        icon => aihtml_html:html(),
                        shortcut => aihtml_html:html(),
                        href => iodata(),
                        disabled => boolean()}]}],
              aihtml_html:css(),
              aihtml_html:attrs()) ->
                 #ah_command{}.
command(A1, A2, A3) -> aihtml_layout_bars:command(A1, A2, A3).
-spec set_command_items(aihtml_action:ctx(),
                        {id, iodata() | atom()} | aihtml_action:event(),
                        [iodata() |
                         atom() |
                         integer() |
                         {term(), aihtml_html:html()} |
                         #{value := term(),
                           label => aihtml_html:html(),
                           description => aihtml_html:html(),
                           icon => aihtml_html:html(),
                           shortcut => aihtml_html:html(),
                           href => iodata(),
                           disabled => boolean()} |
                         #{heading => aihtml_html:html(),
                           items :=
                               [iodata() |
                                atom() |
                                integer() |
                                {term(), aihtml_html:html()} |
                                #{value := term(),
                                  label => aihtml_html:html(),
                                  description => aihtml_html:html(),
                                  icon => aihtml_html:html(),
                                  shortcut => aihtml_html:html(),
                                  href => iodata(),
                                  disabled => boolean()}]}]) ->
                           ok.
set_command_items(A1, A2, A3) -> aihtml_layout_bars:set_command_items(A1, A2, A3).

%% aihtml_layout_dnd
-spec sortable([aihtml_layout_dnd:item()],
               undefined | [term()] | iodata(),
               aihtml_html:css(),
               aihtml_html:attrs()) ->
                  #ah_sortable{}.
sortable(A1, A2, A3, A4) -> aihtml_layout_dnd:sortable(A1, A2, A3, A4).
-spec dragdrop(aihtml_html:html(),
               aihtml_html:css(),
               aihtml_html:attrs()) ->
                  #ah_dragdrop{}.
dragdrop(A1, A2, A3) -> aihtml_layout_dnd:dragdrop(A1, A2, A3).
-spec draggable_attrs(term(), map()) -> aihtml_html:attrs().
draggable_attrs(A1, A2) -> aihtml_layout_dnd:draggable_attrs(A1, A2).
-spec drop_zone_attrs(term(), map()) -> aihtml_html:attrs().
drop_zone_attrs(A1, A2) -> aihtml_layout_dnd:drop_zone_attrs(A1, A2).

%% aihtml_layout_dock
-spec docking([aihtml_layout_dock:panel()],
              aihtml_html:css(),
              aihtml_html:attrs()) ->
                 #ah_docking{}.
docking(A1, A2, A3) -> aihtml_layout_dock:docking(A1, A2, A3).
-spec dock_layout(aihtml_layout_dock:layout(),
                  aihtml_html:css(),
                  aihtml_html:attrs()) ->
                     #ah_dock_layout{}.
dock_layout(A1, A2, A3) -> aihtml_layout_dock:dock_layout(A1, A2, A3).
-spec docking_add_window(aihtml_action:ctx(),
                         {id, iodata() | atom()},
                         atom() | binary() | integer(),
                         aihtml_layout_dock:window()) ->
                            ok.
docking_add_window(A1, A2, A3, A4) -> aihtml_layout_dock:docking_add_window(A1, A2, A3, A4).
-spec dock_layout_open(aihtml_action:ctx(),
                       {id, iodata() | atom()},
                       aihtml_layout_dock:dl_panel()) ->
                          ok.
dock_layout_open(A1, A2, A3) -> aihtml_layout_dock:dock_layout_open(A1, A2, A3).
-spec dock_layout_open(aihtml_action:ctx(),
                       {id, iodata() | atom()},
                       aihtml_layout_dock:dl_panel(),
                       #{in => atom() | binary() | integer(),
                         edge => left | right | top | bottom,
                         float => boolean() | {number(), number()},
                         labels =>
                             #{auto_hide | float | dock | close =>
                                   unicode:chardata()}}) ->
                          ok.
dock_layout_open(A1, A2, A3, A4) -> aihtml_layout_dock:dock_layout_open(A1, A2, A3, A4).

%% aihtml_layout_tiles
-spec ribbon([{term(),
               aihtml_html:html(),
               aihtml_html:html() |
               {groups,
                [{aihtml_html:html(),
                  [{term(), aihtml_html:html(), aihtml_html:html()} |
                   #{key := term(),
                     label := aihtml_html:html(),
                     icon => aihtml_html:html(),
                     size => small | large,
                     title => iodata(),
                     disabled => boolean(),
                     toggle => boolean(),
                     pressed => boolean(),
                     items =>
                         [{term(), aihtml_html:html()} |
                          #{key := term(),
                            label := aihtml_html:html(),
                            icon => aihtml_html:html(),
                            disabled => boolean()} |
                          divider]} |
                   {stack, [term()]} |
                   separator |
                   {html, aihtml_html:html()}]} |
                 #{label := aihtml_html:html(),
                   items :=
                       [{term(), aihtml_html:html(), aihtml_html:html()} |
                        #{key := term(),
                          label := aihtml_html:html(),
                          icon => aihtml_html:html(),
                          size => small | large,
                          title => iodata(),
                          disabled => boolean(),
                          toggle => boolean(),
                          pressed => boolean(),
                          items =>
                              [{term(), aihtml_html:html()} |
                               #{key := term(),
                                 label := aihtml_html:html(),
                                 icon => aihtml_html:html(),
                                 disabled => boolean()} |
                               divider]} |
                        {stack, [term()]} |
                        separator |
                        {html, aihtml_html:html()}]}]}} |
              {term(),
               aihtml_html:html(),
               aihtml_html:html() |
               {groups,
                [{aihtml_html:html(),
                  [{term(), aihtml_html:html(), aihtml_html:html()} |
                   #{key := term(),
                     label := aihtml_html:html(),
                     icon => aihtml_html:html(),
                     size => small | large,
                     title => iodata(),
                     disabled => boolean(),
                     toggle => boolean(),
                     pressed => boolean(),
                     items =>
                         [{term(), aihtml_html:html()} |
                          #{key := term(),
                            label := aihtml_html:html(),
                            icon => aihtml_html:html(),
                            disabled => boolean()} |
                          divider]} |
                   {stack, [term()]} |
                   separator |
                   {html, aihtml_html:html()}]} |
                 #{label := aihtml_html:html(),
                   items :=
                       [{term(), aihtml_html:html(), aihtml_html:html()} |
                        #{key := term(),
                          label := aihtml_html:html(),
                          icon => aihtml_html:html(),
                          size => small | large,
                          title => iodata(),
                          disabled => boolean(),
                          toggle => boolean(),
                          pressed => boolean(),
                          items =>
                              [{term(), aihtml_html:html()} |
                               #{key := term(),
                                 label := aihtml_html:html(),
                                 icon => aihtml_html:html(),
                                 disabled => boolean()} |
                               divider]} |
                        {stack, [term()]} |
                        separator |
                        {html, aihtml_html:html()}]}]},
               aihtml_html:attrs()} |
              #{key := term(),
                label := aihtml_html:html(),
                icon => aihtml_html:html(),
                disabled => boolean(),
                content => aihtml_html:html(),
                groups =>
                    [{aihtml_html:html(),
                      [{term(), aihtml_html:html(), aihtml_html:html()} |
                       #{key := term(),
                         label := aihtml_html:html(),
                         icon => aihtml_html:html(),
                         size => small | large,
                         title => iodata(),
                         disabled => boolean(),
                         toggle => boolean(),
                         pressed => boolean(),
                         items =>
                             [{term(), aihtml_html:html()} |
                              #{key := term(),
                                label := aihtml_html:html(),
                                icon => aihtml_html:html(),
                                disabled => boolean()} |
                              divider]} |
                       {stack, [term()]} |
                       separator |
                       {html, aihtml_html:html()}]} |
                     #{label := aihtml_html:html(),
                       items :=
                           [{term(),
                             aihtml_html:html(),
                             aihtml_html:html()} |
                            #{key := term(),
                              label := aihtml_html:html(),
                              icon => aihtml_html:html(),
                              size => small | large,
                              title => iodata(),
                              disabled => boolean(),
                              toggle => boolean(),
                              pressed => boolean(),
                              items =>
                                  [{term(), aihtml_html:html()} |
                                   #{key := term(),
                                     label := aihtml_html:html(),
                                     icon => aihtml_html:html(),
                                     disabled => boolean()} |
                                   divider]} |
                            {stack, [term()]} |
                            separator |
                            {html, aihtml_html:html()}]}]}],
             term(),
             aihtml_html:css(),
             aihtml_html:attrs()) ->
                #ah_ribbon{}.
ribbon(A1, A2, A3, A4) -> aihtml_layout_tiles:ribbon(A1, A2, A3, A4).
-spec tile_layout({columns | rows, [term()]} |
                  {tabs,
                   [{term(), iodata(), aihtml_html:html()} |
                    #{id := term(),
                      label := iodata(),
                      content => aihtml_html:html(),
                      close => boolean(),
                      drag => boolean()}]} |
                  #{columns => [term()],
                    rows => [term()],
                    tabs =>
                        [{term(), iodata(), aihtml_html:html()} |
                         #{id := term(),
                           label := iodata(),
                           content => aihtml_html:html(),
                           close => boolean(),
                           drag => boolean()}],
                    id => term(),
                    content => aihtml_html:html(),
                    label => iodata(),
                    size => undefined | integer() | iodata(),
                    min => non_neg_integer(),
                    resize => boolean(),
                    position => top | bottom | left | right,
                    active => term()},
                  undefined | iodata() | map(),
                  aihtml_html:css(),
                  aihtml_html:attrs()) ->
                     #ah_tile_layout{}.
tile_layout(A1, A2, A3, A4) -> aihtml_layout_tiles:tile_layout(A1, A2, A3, A4).

%% aihtml_overlay
-spec tooltip(aihtml_html:html(),
              aihtml_html:html(),
              aihtml_html:css(),
              aihtml_html:attrs()) ->
                 #ah_tooltip{}.
tooltip(A1, A2, A3, A4) -> aihtml_overlay:tooltip(A1, A2, A3, A4).
-spec tooltip_attrs(iodata(), map()) -> aihtml_html:attrs().
tooltip_attrs(A1, A2) -> aihtml_overlay:tooltip_attrs(A1, A2).
-spec popover(aihtml_html:html(),
              aihtml_html:css(),
              aihtml_html:attrs()) ->
                 #ah_popover{}.
popover(A1, A2, A3) -> aihtml_overlay:popover(A1, A2, A3).
-spec drawer(aihtml_html:html(), aihtml_html:css(), aihtml_html:attrs()) ->
                #ah_drawer{}.
drawer(A1, A2, A3) -> aihtml_overlay:drawer(A1, A2, A3).
-spec sheet(aihtml_html:html(), aihtml_html:css(), aihtml_html:attrs()) ->
               #ah_sheet{}.
sheet(A1, A2, A3) -> aihtml_overlay:sheet(A1, A2, A3).
-spec window(aihtml_html:html(), aihtml_html:css(), aihtml_html:attrs()) ->
                #ah_window{}.
window(A1, A2, A3) -> aihtml_overlay:window(A1, A2, A3).
-spec notification(aihtml_html:html(),
                   aihtml_html:css(),
                   aihtml_html:attrs()) ->
                      #ah_notification{}.
notification(A1, A2, A3) -> aihtml_overlay:notification(A1, A2, A3).
-spec shows_toast(iodata(), map()) -> aihtml_html:attrs().
shows_toast(A1, A2) -> aihtml_overlay:shows_toast(A1, A2).
-spec opens(aihtml_overlay:target()) -> aihtml_html:attrs().
opens(A1) -> aihtml_overlay:opens(A1).
-spec toggles(aihtml_overlay:target()) -> aihtml_html:attrs().
toggles(A1) -> aihtml_overlay:toggles(A1).
-spec closes() -> aihtml_html:attrs().
closes() -> aihtml_overlay:closes().
-spec closes(aihtml_overlay:target()) -> aihtml_html:attrs().
closes(A1) -> aihtml_overlay:closes(A1).
-spec closes(aihtml_overlay:target() | closest, atom() | iodata()) ->
                aihtml_html:attrs().
closes(A1, A2) -> aihtml_overlay:closes(A1, A2).
-spec toast(aihtml_action:ctx(), iodata(), map()) -> ok.
toast(A1, A2, A3) -> aihtml_overlay:toast(A1, A2, A3).

%% aihtml_display
-spec avatar(aihtml_html:html(), aihtml_html:css(), aihtml_html:attrs()) ->
                #ah_avatar{}.
avatar(A1, A2, A3) -> aihtml_display:avatar(A1, A2, A3).
-spec badge(aihtml_html:html(), aihtml_html:css(), aihtml_html:attrs()) ->
               #ah_badge{}.
badge(A1, A2, A3) -> aihtml_display:badge(A1, A2, A3).
-spec chip(aihtml_html:html(), aihtml_html:css(), aihtml_html:attrs()) ->
              #ah_chip{}.
chip(A1, A2, A3) -> aihtml_display:chip(A1, A2, A3).
-spec aspect_ratio(aihtml_html:html(),
                   aihtml_html:css(),
                   aihtml_html:attrs()) ->
                      #ah_aspect_ratio{}.
aspect_ratio(A1, A2, A3) -> aihtml_display:aspect_ratio(A1, A2, A3).
-spec kbd(aihtml_html:html() | [aihtml_html:html()],
          aihtml_html:css(),
          aihtml_html:attrs()) ->
             #ah_kbd{}.
kbd(A1, A2, A3) -> aihtml_display:kbd(A1, A2, A3).
-spec time_ago(integer() | calendar:datetime() | binary(),
               aihtml_html:css(),
               aihtml_html:attrs()) ->
                  #ah_time_ago{}.
time_ago(A1, A2, A3) -> aihtml_display:time_ago(A1, A2, A3).
-spec expandable_text(unicode:chardata(),
                      aihtml_html:css(),
                      aihtml_html:attrs()) ->
                         #ah_expandable_text{}.
expandable_text(A1, A2, A3) -> aihtml_display:expandable_text(A1, A2, A3).
-spec alert(aihtml_html:html(), aihtml_html:css(), aihtml_html:attrs()) ->
               #ah_alert{}.
alert(A1, A2, A3) -> aihtml_display:alert(A1, A2, A3).
-spec progressbar(number() | undefined,
                  aihtml_html:css(),
                  aihtml_html:attrs()) ->
                     #ah_progressbar{}.
progressbar(A1, A2, A3) -> aihtml_display:progressbar(A1, A2, A3).
-spec progress_circle(number() | undefined,
                      aihtml_html:css(),
                      aihtml_html:attrs()) ->
                         #ah_progress_circle{}.
progress_circle(A1, A2, A3) -> aihtml_display:progress_circle(A1, A2, A3).
-spec meter(number(), aihtml_html:css(), aihtml_html:attrs()) ->
               #ah_meter{}.
meter(A1, A2, A3) -> aihtml_display:meter(A1, A2, A3).
-spec statistic(number() | aihtml_html:html(),
                aihtml_html:css(),
                aihtml_html:attrs()) ->
                   #ah_statistic{}.
statistic(A1, A2, A3) -> aihtml_display:statistic(A1, A2, A3).
-spec kpi_card(aihtml_html:html(),
               aihtml_html:css(),
               aihtml_html:attrs()) ->
                  #ah_kpi_card{}.
kpi_card(A1, A2, A3) -> aihtml_display:kpi_card(A1, A2, A3).
-spec timeline([map()], aihtml_html:css(), aihtml_html:attrs()) ->
                  #ah_timeline{}.
timeline(A1, A2, A3) -> aihtml_display:timeline(A1, A2, A3).
-spec ranking_list([map()], aihtml_html:css(), aihtml_html:attrs()) ->
                      #ah_ranking_list{}.
ranking_list(A1, A2, A3) -> aihtml_display:ranking_list(A1, A2, A3).
-spec tag_cloud([map() | {aihtml_html:html(), number()}],
                aihtml_html:css(),
                aihtml_html:attrs()) ->
                   #ah_tag_cloud{}.
tag_cloud(A1, A2, A3) -> aihtml_display:tag_cloud(A1, A2, A3).

%% aihtml_data_tree
-spec tree([aihtml_data_tree:tree_item()],
           term(),
           aihtml_html:css(),
           aihtml_html:attrs()) ->
              #ah_tree{}.
tree(A1, A2, A3, A4) -> aihtml_data_tree:tree(A1, A2, A3, A4).
-spec nav_tree([aihtml_data_tree:nav_item()],
               iodata() | atom() | undefined,
               aihtml_html:css(),
               aihtml_html:attrs()) ->
                  #ah_nav_tree{}.
nav_tree(A1, A2, A3, A4) -> aihtml_data_tree:nav_tree(A1, A2, A3, A4).
-spec diff(unicode:chardata(),
           unicode:chardata(),
           aihtml_html:css(),
           aihtml_html:attrs()) ->
              #ah_diff{}.
diff(A1, A2, A3, A4) -> aihtml_data_tree:diff(A1, A2, A3, A4).
-spec heatmap_calendar(#{binary() | string() | calendar:date() =>
                             number()} |
                       [{binary() | string() | calendar:date(),
                         number()}],
                       aihtml_html:css(),
                       aihtml_html:attrs()) ->
                          #ah_heatmap_calendar{}.
heatmap_calendar(A1, A2, A3) -> aihtml_data_tree:heatmap_calendar(A1, A2, A3).
-spec set_children(aihtml_action:ctx(),
                   aihtml_action:event(),
                   [aihtml_data_tree:tree_item()]) ->
                      ok.
set_children(A1, A2, A3) -> aihtml_data_tree:set_children(A1, A2, A3).

%% aihtml_data_grid
-spec datagrid([aihtml_data_grid:column()],
               [aihtml_data_grid:row()],
               aihtml_html:css(),
               aihtml_html:attrs()) ->
                  #ah_datagrid{}.
datagrid(A1, A2, A3, A4) -> aihtml_data_grid:datagrid(A1, A2, A3, A4).
-spec datagrid_query(aihtml_action:event()) -> aihtml_data_grid:query().
datagrid_query(A1) -> aihtml_data_grid:datagrid_query(A1).
-spec datagrid_rows(aihtml_action:ctx(),
                    aihtml_action:event(),
                    [aihtml_data_grid:row()],
                    non_neg_integer()) ->
                       ok.
datagrid_rows(A1, A2, A3, A4) -> aihtml_data_grid:datagrid_rows(A1, A2, A3, A4).
-spec datagrid_row(aihtml_action:ctx(),
                   aihtml_action:event(),
                   aihtml_data_grid:row()) ->
                      ok.
datagrid_row(A1, A2, A3) -> aihtml_data_grid:datagrid_row(A1, A2, A3).
-spec datagrid_select(aihtml_data_grid:query(),
                      [aihtml_data_grid:row()]) ->
                         {[aihtml_data_grid:row()], non_neg_integer()}.
datagrid_select(A1, A2) -> aihtml_data_grid:datagrid_select(A1, A2).

%% aihtml_data_pivot
-spec pivotgrid([aihtml_data_pivot:row()],
                aihtml_data_pivot:layout(),
                aihtml_html:css(),
                aihtml_html:attrs()) ->
                   #ah_pivotgrid{}.
pivotgrid(A1, A2, A3, A4) -> aihtml_data_pivot:pivotgrid(A1, A2, A3, A4).
-spec pivotgrid_rows(aihtml_action:ctx(),
                     aihtml_action:event(),
                     [aihtml_data_pivot:row()]) ->
                        ok.
pivotgrid_rows(A1, A2, A3) -> aihtml_data_pivot:pivotgrid_rows(A1, A2, A3).
-spec pivotgrid_view(aihtml_action:event()) ->
                        #{layout := map(), view := map()}.
pivotgrid_view(A1) -> aihtml_data_pivot:pivotgrid_view(A1).
-spec pivotgrid_cell(aihtml_action:event()) ->
                        #{row := list(),
                          col := list(),
                          filter := map(),
                          field := binary() | null,
                          agg := atom(),
                          value := number() | null,
                          text := binary()}.
pivotgrid_cell(A1) -> aihtml_data_pivot:pivotgrid_cell(A1).

%% aihtml_data_tables
-spec treegrid([aihtml_data_tables:column()],
               [aihtml_data_tables:row()],
               aihtml_html:css(),
               aihtml_html:attrs()) ->
                  #ah_treegrid{}.
treegrid(A1, A2, A3, A4) -> aihtml_data_tables:treegrid(A1, A2, A3, A4).
-spec datatable([aihtml_data_tables:column()],
                [aihtml_data_tables:row()],
                aihtml_html:css(),
                aihtml_html:attrs()) ->
                   #ah_datatable{}.
datatable(A1, A2, A3, A4) -> aihtml_data_tables:datatable(A1, A2, A3, A4).
-spec treegrid_children(aihtml_action:ctx(),
                        aihtml_action:event(),
                        #ah_treegrid{}) ->
                           ok.
treegrid_children(A1, A2, A3) -> aihtml_data_tables:treegrid_children(A1, A2, A3).
-spec datatable_query(aihtml_action:event()) ->
                         aihtml_data_tables:query().
datatable_query(A1) -> aihtml_data_tables:datatable_query(A1).
-spec datatable_rows(aihtml_action:ctx(),
                     aihtml_action:event(),
                     #ah_datatable{}) ->
                        ok.
datatable_rows(A1, A2, A3) -> aihtml_data_tables:datatable_rows(A1, A2, A3).
-spec datatable_row(aihtml_action:ctx(),
                    aihtml_action:event(),
                    #ah_datatable{},
                    aihtml_data_tables:row()) ->
                       ok.
datatable_row(A1, A2, A3, A4) -> aihtml_data_tables:datatable_row(A1, A2, A3, A4).

%% aihtml_data_schedule
-spec gantt([aihtml_data_schedule:gantt_task()],
            aihtml_html:css(),
            aihtml_html:attrs()) ->
               #ah_gantt{}.
gantt(A1, A2, A3) -> aihtml_data_schedule:gantt(A1, A2, A3).
-spec gantt_update(aihtml_action:ctx(),
                   aihtml_action:event(),
                   #ah_gantt{}) ->
                      ok.
gantt_update(A1, A2, A3) -> aihtml_data_schedule:gantt_update(A1, A2, A3).
-spec scheduler([aihtml_data_schedule:scheduler_event()],
                binary() | string() | calendar:date() | undefined,
                aihtml_html:css(),
                aihtml_html:attrs()) ->
                   #ah_scheduler{}.
scheduler(A1, A2, A3, A4) -> aihtml_data_schedule:scheduler(A1, A2, A3, A4).
-spec scheduler_range(aihtml_action:event()) ->
                         #{view :=
                               day | week | month | agenda |
                               timeline_day | timeline_week |
                               timeline_month,
                           date := binary(),
                           start := binary(),
                           'end' := binary()}.
scheduler_range(A1) -> aihtml_data_schedule:scheduler_range(A1).
-spec scheduler_update(aihtml_action:ctx(),
                       aihtml_action:event(),
                       #ah_scheduler{}) ->
                          ok.
scheduler_update(A1, A2, A3) -> aihtml_data_schedule:scheduler_update(A1, A2, A3).
-spec swimlane([aihtml_data_schedule:swimlane_node()],
               aihtml_html:css(),
               aihtml_html:attrs()) ->
                  #ah_swimlane{}.
swimlane(A1, A2, A3) -> aihtml_data_schedule:swimlane(A1, A2, A3).
-spec swimlane_update(aihtml_action:ctx(),
                      aihtml_action:event(),
                      #ah_swimlane{}) ->
                         ok.
swimlane_update(A1, A2, A3) -> aihtml_data_schedule:swimlane_update(A1, A2, A3).

%% aihtml_data_charts
-spec chart(aihtml_data_charts:option(),
            aihtml_html:css(),
            aihtml_html:attrs()) ->
               #ah_chart{}.
chart(A1, A2, A3) -> aihtml_data_charts:chart(A1, A2, A3).
-spec area_chart([aihtml_data_charts:series()],
                 aihtml_html:css(),
                 aihtml_html:attrs()) ->
                    #ah_area_chart{}.
area_chart(A1, A2, A3) -> aihtml_data_charts:area_chart(A1, A2, A3).
-spec bar_chart([aihtml_data_charts:series()],
                aihtml_html:css(),
                aihtml_html:attrs()) ->
                   #ah_bar_chart{}.
bar_chart(A1, A2, A3) -> aihtml_data_charts:bar_chart(A1, A2, A3).
-spec donut_chart([aihtml_data_charts:item()],
                  aihtml_html:css(),
                  aihtml_html:attrs()) ->
                     #ah_donut_chart{}.
donut_chart(A1, A2, A3) -> aihtml_data_charts:donut_chart(A1, A2, A3).
-spec radar_chart([aihtml_data_charts:series()],
                  aihtml_html:css(),
                  aihtml_html:attrs()) ->
                     #ah_radar_chart{}.
radar_chart(A1, A2, A3) -> aihtml_data_charts:radar_chart(A1, A2, A3).
-spec relation_graph(aihtml_data_charts:graph(),
                     aihtml_html:css(),
                     aihtml_html:attrs()) ->
                        #ah_relation_graph{}.
relation_graph(A1, A2, A3) -> aihtml_data_charts:relation_graph(A1, A2, A3).
-spec chart_option(aihtml_data_charts:element()) ->
                      aihtml_data_charts:option().
chart_option(A1) -> aihtml_data_charts:chart_option(A1).
-spec chart_update(aihtml_action:ctx(),
                   aihtml_action:target(),
                   aihtml_data_charts:chart()) ->
                      ok.
chart_update(A1, A2, A3) -> aihtml_data_charts:chart_update(A1, A2, A3).

%% aihtml_data_graph
-spec node_graph(aihtml_data_graph:graph(),
                 aihtml_html:css(),
                 aihtml_html:attrs()) ->
                    #ah_node_graph{}.
node_graph(A1, A2, A3) -> aihtml_data_graph:node_graph(A1, A2, A3).
-spec node_graph_layout(aihtml_data_graph:graph()) ->
                           aihtml_data_graph:graph().
node_graph_layout(A1) -> aihtml_data_graph:node_graph_layout(A1).
-spec set_node_graph(aihtml_action:ctx(),
                     aihtml_action:target() | aihtml_action:event(),
                     aihtml_data_graph:graph()) ->
                        ok.
set_node_graph(A1, A2, A3) -> aihtml_data_graph:set_node_graph(A1, A2, A3).

%% END GENERATED COMPONENTS
