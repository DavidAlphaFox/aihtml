%%%-------------------------------------------------------------------
%%% @doc A resource scheduler, ported from sigil (data/scheduler). See
%%% designs/04-components.md.
%%%
%%%   ah_scheduler(Events, Value, Css, Attrs) resource scheduler (day, week,
%%%                                        month, agenda, timeline views)
%%%   scheduler_update(Ctx, Event, S)      (in an action) render another range
%%%   scheduler_range(Event)               the view, date and range an event asks for
%%%
%%% Everything is rendered here, on the server: the time grid, the month
%%% segments, the agenda and the timelines, recurring appointments
%%% expanded (aihtml_lib_rrule). The behaviour
%%% (assets/js/components/scheduler.ts) handles scrolling, keyboard, drag
%%% and drop and the context menu; after a local change it moves the
%%% existing elements, but never builds HTML.
%%%
%%% == Edits ==
%%%
%%% Dragging (with the `editable' modifier) moves an appointment in place
%%% and fires `ah:event-change' on the root after writing its details
%%% (event, source, from, to, resource, kind) to data attributes, so a
%%% postback gets them in `Event.data'. The action stores the change and
%%% may answer with scheduler_update/3, rendering the scheduler again from
%%% the stored data; the morph keeps the scroll position, and a refused
%%% change is undone the same way.
%%%
%%% == Navigation (remote) ==
%%%
%%% The scheduler's value is the date it shows. The toolbar (prev, today,
%%% next, the view buttons) updates `data-ah-value', `data-view',
%%% `data-start' and `data-end' (the visible range, end exclusive) and fires
%%% `change'; the `source' option binds an action to it, which loads that
%%% range from the database and answers with
%%% `scheduler_update(Ctx, Event, ah_scheduler(Events, undefined, Css, Attrs))':
%%% the date and view come from the event, the new view is rendered here
%%% and morphed into the page. The browser never renders a range itself,
%%% so the page holds only what is visible. Without `source' (or a change
%%% postback, or `href') the toolbar shows only the title.
%%%
%%% == Links (href) ==
%%%
%%% With `{href, <<"/agenda?date={date}&view={view}">>}' the toolbar's
%%% prev, today, next and view buttons are links (`<a href>') to the date
%%% and view they lead to, so every state has a URL the server can render
%%% by itself: crawlers and pages opened without script follow them, and
%%% the page reads `date' and `view' from its query to render that state.
%%% When the scheduler is bound (a `source' or a change postback), the
%%% behaviour intercepts a plain click: it navigates in the page as the
%%% buttons do (change, the action morphs the new range in) and pushes the
%%% link's URL, so back, forward and bookmarks work (going back reloads
%%% that URL). Modified clicks (new tab) and unbound schedulers follow the
%%% link. The behaviour keeps the links pointing at the neighbours of the
%%% shown date.
%%%
%%% ah_scheduler/4 builds an #ah_scheduler{} (include/aihtml_scheduler.hrl)
%%% and render/1 turns it into HTML, so pages may also write the record
%%% directly (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_scheduler).
-behaviour(aihtml_element).

-include("aihtml_scheduler.hrl").

-export([ah_scheduler/4, scheduler_update/3, scheduler_range/1,
         render/1, fields/1, catalog/0, facade_extras/0]).

-export_type([element/0, event/0, status/0, resource/0, view/0, label_key/0, labels/0]).

-define(H, aihtml_html).
-define(E, aihtml_element).
-define(D, aihtml_lib_date).

%% The record field types promise well-formed data, but builders pass
%% whatever the page gives: these clauses turn it into {aihtml, _} errors.
-dialyzer({no_match, [resource/1, labels/3]}).

-define(DAY, 1440).
-define(VIEWS, [day, week, month, agenda, timeline_day, timeline_week, timeline_month]).
-define(PRIMARY, <<"var(--ah-color-primary)">>).

-type status() :: free | busy | tentative | out_of_office.
%% A scheduler appointment. `start' is required; `end' defaults to one day
%% (all day) or one hour later; a start and end at midnight, or
%% `all_day', make an all day appointment. `resource' is a resource id.
%% `rrule' is an iCalendar RRULE subset (FREQ, INTERVAL, COUNT, UNTIL,
%% BYDAY, BYMONTHDAY, BYMONTH), `exdates' the days left out of the series.
-type event() :: #{id => term(), title => unicode:chardata(),
                   start := aihtml_lib_date:time(), 'end' => aihtml_lib_date:time(),
                   all_day => boolean(), resource => term(),
                   status => status(), color => unicode:chardata(),
                   rrule => unicode:chardata(), exdates => [aihtml_lib_date:date()]}.
%% A scheduler resource (room, person): a column in the day and week
%% views, a row in the timeline views.
-type resource() :: #{id := term(), name => unicode:chardata(),
                      color => unicode:chardata()}.
-type view() :: day | week | month | agenda
              | timeline_day | timeline_week | timeline_month.
-type label_key() :: today | prev | next | day | week | month | agenda
                   | timeline_day | timeline_week | timeline_month
                   | all_day | all_day_short | more | no_events | hint_navigate
                   | edit | delete | copy | new | am | pm
                   | months | months_short | weekdays | weekdays_short
                   | title_day | title_month | range_start | range_end
                   | agenda_date | popover_date.
%% Texts of the scheduler. `months', `months_short' (12), `weekdays' and
%% `weekdays_short' (7, from Sunday) are lists; `more' holds {n}; the
%% title_*, range_* and agenda_date and popover_date keys are display
%% formats (yyyy MMMM MMM MM M dd d EEEE EEE).
-type labels() :: #{label_key() => unicode:chardata() | [unicode:chardata()]}.
-type element() :: #ah_scheduler{}.

%% @doc A resource scheduler (sigil's scheduler). `Events' is a list of
%% appointment maps (see `event()'); `Value' is the date shown (an
%% ISO date or `calendar:date()'; `undefined' is today).
%%
%% Css: `editable' (drag appointments to other times, days and resources,
%% resize them, drag over free time to select a range, context menu),
%% `no_all_day' (no all day row in the day and week views).
%% Options (in Attrs): `view' (day, week (default), month, agenda,
%% timeline_day, timeline_week, timeline_month), `views' (the view
%% buttons, default [day, week, month, agenda]), `resources' (a list of
%% `#{id, name, color}'), `first_day' (0 = Sunday .. 6, default 1),
%% `slot_duration' (minutes, a divisor of 60, default 30), `slot_height'
%% (px, 20), `day_start', `day_end' (hours shown, 0 and 24), `height' (px,
%% 600), `agenda_days' (30), `day_max_events' (month rows before "+n
%% more", 3), `hour_format' (12 or 24), `today' (the date highlighted,
%% default the server's date), `toolbar' (default true), `source' (the
%% action that loads another range, see the module doc), `labels',
%% `href' (a URL template with {date} and {view}: the toolbar becomes
%% links, see the module doc), `name' (a hidden input with the shown date).
-spec ah_scheduler([event()], aihtml_lib_date:date(), aihtml_html:css(), aihtml_html:attrs()) ->
          #ah_scheduler{}.
ah_scheduler(Events, Value, Css, Attrs) ->
    ?E:build(?MODULE, #ah_scheduler{items = Events, value = Value}, Css, Attrs).

%% @doc The field names of #ah_scheduler{}.
-spec fields(atom()) -> [atom()].
fields(ah_scheduler) -> record_info(fields, ah_scheduler).

%% The texts and date names of the current language (aihtml_i18n).
sch_label_defaults() ->
    maps:merge(aihtml_i18n:formats([months, months_short, weekdays, weekdays_short, am, pm]),
               aihtml_i18n:texts(scheduler)).

-spec render(element()) -> aihtml_html:html().
render(#ah_scheduler{view = View, views = Views, editable = Editable} = R0) ->
    {Id, R} = ensure_id(R0),
    Classes = ?E:classes(?MODULE, R),
    lists:member(View, ?VIEWS) orelse error({aihtml, {bad_option, view, View}}),
    (is_list(Views) andalso Views =/= [] andalso lists:all(fun(V) -> lists:member(V, ?VIEWS) end,
                                                           Views))
        orelse error({aihtml, {bad_option, views, Views}}),
    First = in_range(first_day, R#ah_scheduler.first_day, 0, 6),
    SD = pos_int(slot_duration, R#ah_scheduler.slot_duration),
    60 rem SD =:= 0 orelse error({aihtml, {bad_option, slot_duration, SD}}),
    SH = pos_int(slot_height, R#ah_scheduler.slot_height),
    DS = in_range(day_start, R#ah_scheduler.day_start, 0, 23),
    DE = in_range(day_end, R#ah_scheduler.day_end, DS + 1, 24),
    Agenda = pos_int(agenda_days, R#ah_scheduler.agenda_days),
    Max = pos_int(day_max_events, R#ah_scheduler.day_max_events),
    HF = case R#ah_scheduler.hour_format of
             F when F =:= 12; F =:= 24 -> F;
             F -> error({aihtml, {bad_option, hour_format, F}})
         end,
    is_boolean(R#ah_scheduler.toolbar)
        orelse error({aihtml, {bad_option, toolbar, R#ah_scheduler.toolbar}}),
    L = labels(sch_label_defaults(), R#ah_scheduler.labels, scheduler),
    {Today, _} = ?D:today(R#ah_scheduler.today),
    Cur = case R#ah_scheduler.value of
              undefined -> Today;
              V -> ?D:day_of(V)
          end,
    {RS, RE} = profile(View, Cur, First, Agenda),
    Title = sch_title(View, Cur, RS, RE, L),
    Resources = [resource(X) || X <- R#ah_scheduler.resources],
    Items = R#ah_scheduler.items,
    is_list(Items) orelse error({aihtml, {bad_scheduler_events, Items}}),
    Events = [sch_event(E, N) || {N, E} <- lists:zip(lists:seq(1, length(Items)), Items)],
    Insts = instances(Events, RS * ?DAY, RE * ?DAY),
    Cfg = #{id => Id, today => Today, cur => Cur, rs => RS, re => RE, sd => SD, sh => SH,
            ds => DS, de => DE, hf => HF, labels => L, resources => Resources,
            editable => Editable, max => Max, first => First, view => View,
            all_day_row => not R#ah_scheduler.no_all_day},
    Body = case View of
               day -> timegrid(Insts, Cfg);
               week -> timegrid(Insts, Cfg);
               month -> month_view(Insts, Cfg);
               agenda -> agenda_view(Insts, Cfg);
               _ -> timeline_view(Insts, Cfg)
           end,
    #{postback := Postback} = ?E:base(R),
    Source = R#ah_scheduler.source,
    Href = R#ah_scheduler.href,
    Navigable = Source =/= undefined orelse Postback =/= undefined orelse Href =/= undefined,
    Iso = ?D:iso_date(Cur),
    Nav = #{href => Href, cur => Cur, today => Today, view => View, agenda => Agenda},
    ?H:el('div',
          [[sch_toolbar(Title, Navigable, Views, Nav, L) || R#ah_scheduler.toolbar],
           hidden(R#ah_scheduler.name, Iso),
           ?H:el('div', Body, [<<"ah-scheduler-view-container">>], []),
           [sch_menus(L) || Editable],
           ?H:el('div', [], [<<"ah-scheduler-live">>],
                 [{aria_live, polite}, {style, <<"position:absolute;width:1px;height:1px;"
                                                 "overflow:hidden;clip:rect(0 0 0 0);">>}])],
          Classes,
          [[{id, Id}, {data_ah, <<"scheduler">>},
            {style, height_style(R#ah_scheduler.height)},
            {data_ah_value, Iso}, {data_view, View},
            {data_start, ?D:iso_date(RS)}, {data_end, ?D:iso_date(RE)},
            {data_first_day, First}, {data_agenda_days, Agenda},
            {data_slot_duration, SD}, {data_slot_height, SH},
            {data_day_start, DS}, {data_day_end, DE},
            {data_hour_format, HF}, {data_am, maps:get(am, L)}, {data_pm, maps:get(pm, L)},
            {data_editable, Editable},
            {data_href, case Href of undefined -> undefined; _ -> text(Href) end},
            {role, region}, {aria_label, Title}],
           case Source of
               undefined -> [];
               _ -> aihtml:on(change, Source, #{sync => queue})
           end,
           ?E:root_attrs(R, change)]).

%% The visible days [Start, End) of a view (sigil's compute-date-profile).
profile(View, D, _, _) when View =:= day; View =:= timeline_day -> {D, D + 1};
profile(View, D, First, _) when View =:= week; View =:= timeline_week ->
    S = ?D:start_of_week(D, First), {S, S + 7};
profile(month, D, First, _) ->
    {?D:start_of_week(?D:first_of_month(D), First), ?D:start_of_week(?D:last_of_month(D), First) + 7};
profile(timeline_month, D, _, _) -> {?D:first_of_month(D), ?D:last_of_month(D) + 1};
profile(agenda, D, _, N) -> {D, D + N}.

sch_title(View, D, _, _, L) when View =:= day; View =:= timeline_day ->
    fmt(D * ?DAY, maps:get(title_day, L), L);
sch_title(View, D, _, _, L) when View =:= month; View =:= timeline_month ->
    fmt(D * ?DAY, maps:get(title_month, L), L);
sch_title(_, _, S, E, L) ->
    [fmt(S * ?DAY, maps:get(range_start, L), L), <<" – "/utf8>>,
     fmt((E - 1) * ?DAY, maps:get(range_end, L), L)].

sch_toolbar(Title, Navigable, Views, #{view := View} = Nav, L) ->
    ?H:el('div',
          ?H:el('div',
                [?H:el('div',
                       [[nav_btn(<<"‹"/utf8>>, <<"ah-scheduler-btn ah-scheduler-btn-prev">>,
                                 [{aria_label, maps:get(prev, L)}], nav_url(Nav, prev, View)),
                         nav_btn(maps:get(today, L), <<"ah-scheduler-btn ah-scheduler-btn-today">>,
                                 [], nav_url(Nav, today, View)),
                         nav_btn(<<"›"/utf8>>, <<"ah-scheduler-btn ah-scheduler-btn-next">>,
                                 [{aria_label, maps:get(next, L)}], nav_url(Nav, next, View))]
                        || Navigable],
                       [<<"ah-scheduler-toolbar-left">>], []),
                 ?H:el('div', ?H:el(h2, Title, [<<"ah-scheduler-title">>], [{aria_live, polite}]),
                       [<<"ah-scheduler-toolbar-center">>], []),
                 ?H:el('div',
                       [[view_btn(V, View, maps:get(V, L), nav_url(Nav, current, V))
                         || V <- Views] || Navigable],
                       [<<"ah-scheduler-toolbar-right">>], [{role, group}])],
                [<<"ah-scheduler-toolbar-inner">>], []),
          [<<"ah-scheduler-toolbar">>], [{role, toolbar}]).

%% A toolbar button, or with the `href' option a link to the same state
%% (the behaviour follows it in the page when the scheduler is bound to
%% an action; without script the server renders the linked page).
nav_btn(Content, Class, Attrs, undefined) ->
    ?H:el(button, Content, [Class], [{type, button} | Attrs]);
nav_btn(Content, Class, Attrs, Url) ->
    ?H:el(a, Content, [Class], [{href, Url} | Attrs]).

view_btn(V, View, Label, undefined) ->
    ?H:el(button, Label,
          [<<"ah-scheduler-view-btn">>, [<<" ah-scheduler-view-btn-active">> || V =:= View]],
          [{type, button}, {data_view, V}, {aria_pressed, atom_to_binary(V =:= View)}]);
view_btn(V, View, Label, Url) ->
    ?H:el(a, Label,
          [<<"ah-scheduler-view-btn">>, [<<" ah-scheduler-view-btn-active">> || V =:= View]],
          [{href, Url}, {data_view, V}, {aria_current, V =:= View andalso <<"true">>}]).

%% The link of a toolbar entry: the date prev / next / today moves to in
%% view `V', or the current date (a view button), put in the template.
nav_url(#{href := undefined}, _, _) -> undefined;
nav_url(#{href := Href, cur := Cur, today := Today, view := View, agenda := Agenda}, Which, V) ->
    D = case Which of
            prev -> step(View, Cur, -1, Agenda);
            next -> step(View, Cur, 1, Agenda);
            today -> Today;
            current -> Cur
        end,
    fill_href(Href, D, V).

-spec fill_href(unicode:chardata(), aihtml_lib_date:days(), view()) -> binary().
fill_href(Href, D, V) ->
    B = binary:replace(text(Href), <<"{date}">>, ?D:iso_date(D), [global]),
    binary:replace(B, <<"{view}">>, atom_to_binary(V), [global]).

%% The day prev (-1) or next (1) shows (the twin of step() in scheduler.ts).
step(View, D, Dir, _) when View =:= day; View =:= timeline_day -> D + Dir;
step(View, D, Dir, _) when View =:= week; View =:= timeline_week -> D + 7 * Dir;
step(View, D, Dir, _) when View =:= month; View =:= timeline_month -> ?D:add_months(D, Dir);
step(agenda, D, Dir, Agenda) -> D + Agenda * Dir.

%% Context menus as inert templates, cloned by the behaviour.
sch_menus(L) ->
    Item = fun(Action, Key) ->
                   ?H:el('div', maps:get(Key, L), [<<"ah-scheduler-contextmenu-item">>],
                         [{data_action, Action}, {role, menuitem}, {tabindex, -1}])
           end,
    [?H:el(template, [Item(edit, edit), Item(delete, delete), Item(copy, copy)],
           [<<"ah-scheduler-menu-event">>], []),
     ?H:el(template, Item(create, new), [<<"ah-scheduler-menu-cell">>], [])].

resource(#{id := Id} = Res) ->
    #{id => text(Id), name => text(maps:get(name, Res, Id)),
      color => color(maps:get(color, Res, ?PRIMARY))};
resource(Other) -> error({aihtml, {bad_scheduler_resource, Other}}).

sch_event(#{start := S0} = E, N) ->
    {S, DateOnly} = ?D:parse_time(S0),
    Flag = maps:get(all_day, E, false) =:= true,
    End0 = case maps:get('end', E, undefined) of
               undefined when Flag; DateOnly -> S + ?DAY;
               undefined -> S + 60;
               E0 -> element(1, ?D:parse_time(E0))
           end,
    End = max(S, End0),
    AllDay = Flag orelse (S rem ?DAY =:= 0 andalso End rem ?DAY =:= 0 andalso S =/= End),
    #{id => case maps:get(id, E, undefined) of
                undefined -> <<"appt", (integer_to_binary(N))/binary>>;
                I -> text(I)
            end,
      title => text(maps:get(title, E, <<>>)),
      s => S, e => End, all_day => AllDay,
      resource => case maps:get(resource, E, undefined) of
                      undefined -> undefined;
                      Rs -> text(Rs)
                  end,
      status => status(maps:get(status, E, busy)),
      color => color(maps:get(color, E, ?PRIMARY)),
      rrule => case maps:get(rrule, E, undefined) of
                   undefined -> undefined;
                   Rule -> parse_rrule(text(Rule))
               end,
      exdates => [?D:iso_date(?D:day_of(D)) || D <- maps:get(exdates, E, [])]};
sch_event(Other, _) ->
    error({aihtml, {bad_scheduler_event, Other}}).

status(S) when S =:= free; S =:= busy; S =:= tentative; S =:= out_of_office -> S;
status(B) when is_binary(B); is_list(B) ->
    case string:replace(string:lowercase(text(B)), <<"-">>, <<"_">>, all) of
        X -> status_atom(iolist_to_binary(X), B)
    end;
status(Other) -> error({aihtml, {bad_scheduler_status, Other}}).

status_atom(<<"free">>, _) -> free;
status_atom(<<"busy">>, _) -> busy;
status_atom(<<"tentative">>, _) -> tentative;
status_atom(<<"out_of_office">>, _) -> out_of_office;
status_atom(_, B) -> error({aihtml, {bad_scheduler_status, B}}).

status_class(out_of_office) -> <<"ah-scheduler-status-out-of-office">>;
status_class(S) -> <<"ah-scheduler-status-", (atom_to_binary(S))/binary>>.

status_color(free) -> <<"var(--ah-color-success)">>;
status_color(busy) -> <<"var(--ah-color-error)">>;
status_color(tentative) -> <<"var(--ah-color-warning)">>;
status_color(out_of_office) -> <<"var(--ah-color-info, #6366f1)">>.

%% Instances overlapping [RS, RE) (minutes), recurring series expanded:
%% #{id, ev, s, e}; an occurrence's id is <series id>_<yyyyMMddTHHmmss>.
instances(Events, RS, RE) ->
    lists:append(
      [case Rule of
           undefined when S < RE, E > RS; S =:= E, S >= RS, S < RE -> [#{id => Id, ev => Ev, s => S, e => E}];
           undefined -> [];
           _ -> [#{id => <<Id/binary, "_", (aihtml_lib_rrule:stamp(Cs))/binary>>, ev => Ev, s => Cs, e => Ce}
                 || {Cs, Ce} <- aihtml_lib_rrule:expand(S, E, Rule, RS, RE, Ex,
                                                              #{instants => true})]
       end || #{id := Id, s := S, e := E, rrule := Rule, exdates := Ex} = Ev <- Events]).

%% The attributes every appointment element carries.
ev_attrs(#{id := IId, s := S, e := E, ev := #{id := Src, all_day := AD, resource := Res,
                                                title := T}}, Cfg) ->
    [{data_eventid, IId}, {data_source, Src}, {data_resourceid, Res},
     {data_start, ?D:iso_time(S, AD)}, {data_end, ?D:iso_time(E, AD)}, {data_all_day, AD},
     {tabindex, 0}, {role, button},
     {aria_label, [T, <<", ">>, time_range(S, E, AD, Cfg)]}].

time_range(_, _, true, #{labels := L}) -> maps:get(all_day, L);
time_range(S, E, false, Cfg) -> [clock(S, Cfg), <<" – "/utf8>>, clock(E, Cfg)].

clock(Min, #{hf := 24}) ->
    <<(?D:pad(Min rem ?DAY div 60))/binary, ":", (?D:pad(Min rem 60))/binary>>;
clock(Min, #{labels := L}) ->
    H = Min rem ?DAY div 60,
    H12 = case H rem 12 of 0 -> 12; X -> X end,
    <<(integer_to_binary(H12))/binary, ":", (?D:pad(Min rem 60))/binary, " ",
      (maps:get(case H < 12 of true -> am; false -> pm end, L))/binary>>.

ev_style(#{ev := #{color := C, status := St}}) ->
    [<<"background:">>, C, <<";border-left:3px solid ">>, status_color(St), <<";">>].

ev_classes(Base, #{ev := #{status := St}}) ->
    [Base, <<" ">>, status_class(St)].

ev_title(#{ev := #{title := T, rrule := Rule}}, Tag, Class) ->
    ?H:el(Tag, [[?H:el(span, <<"↻ "/utf8>>, [<<"ah-scheduler-event-recurring-icon">>],
                       [{aria_hidden, <<"true">>}]) || Rule =/= undefined], T],
          [Class], []).

overlaps(#{s := S, e := E}, From, To) -> S < To andalso (E > From orelse (S =:= E andalso S >= From)).

res_matches(_, undefined) -> true;
res_matches(#{ev := #{resource := R}}, #{id := R}) -> true;
res_matches(_, _) -> false.

%%%-------------------------------------------------------------------
%%% Day and week views: a time grid, optionally a column per resource
%%%-------------------------------------------------------------------

timegrid(Insts, #{rs := RS, re := RE, resources := Res, ds := DS, de := DE,
                  sd := SD, sh := SH, today := Today, labels := L} = Cfg) ->
    Days = lists:seq(RS, RE - 1),
    HasRes = Res =/= [],
    Cols = case HasRes of true -> length(Days) * length(Res); false -> length(Days) end,
    MinW = [<<"min-width:">>, num(Cols * 80), <<"px;">>],
    PerDay = case HasRes of true -> Res; false -> [undefined] end,
    HourPx = SH * 60 div SD,
    Header = [case HasRes of
                  false -> day_header_cell(D, Today, L);
                  true ->
                      ?H:el('div',
                            [day_header_cell(D, Today, L),
                             ?H:el('div',
                                   [?H:el('div', N, [<<"ah-scheduler-dayview-resource-label">>],
                                          [{style, [<<"border-bottom:2px solid ">>, C, <<";">>]}])
                                    || #{name := N, color := C} <- Res],
                                   [<<"ah-scheduler-dayview-resource-header">>], [])],
                            [<<"ah-scheduler-dayview-header-day-group">>], [])
              end || D <- Days],
    AllDay = case maps:get(all_day_row, Cfg) of
                 false -> [];
                 true ->
                     ?H:el('div',
                           [?H:el('div', maps:get(all_day_short, L),
                                  [<<"ah-scheduler-dayview-gutter">>], []),
                            ?H:el('div',
                                  [?H:el('div',
                                         [allday_event(I, Cfg)
                                          || #{ev := #{all_day := true}} = I <- Insts,
                                             overlaps(I, D * ?DAY, (D + 1) * ?DAY),
                                             res_matches(I, Rr)],
                                         [<<"ah-scheduler-dayview-allday-cell">>],
                                         [{data_date, ?D:iso_date(D)}, {data_resourceid, res_id(Rr)}])
                                   || D <- Days, Rr <- PerDay],
                                  [<<"ah-scheduler-dayview-allday">>], [{style, MinW}])],
                           [<<"ah-scheduler-dayview-allday-row">>], [])
             end,
    Slots = [?H:el('div',
                   [?H:el('div', hour_label(H, Cfg), [<<"ah-scheduler-timegrid-slot-label">>], []),
                    ?H:el('div', [], [<<"ah-scheduler-timegrid-slot-line">>], [])],
                   [<<"ah-scheduler-timegrid-slot">>],
                   [{style, [<<"height:">>, num(HourPx), <<"px;">>]}])
             || H <- lists:seq(DS, DE - 1)],
    TotalH = (DE - DS) * HourPx,
    Columns = [day_column(D, Rr, Insts, TotalH, Cfg) || D <- Days, Rr <- PerDay],
    ?H:el('div',
          ?H:el('div',
                [?H:el('div',
                       [?H:el('div', [], [<<"ah-scheduler-dayview-gutter-header">>], []),
                        ?H:el('div', Header, [<<"ah-scheduler-dayview-header-cols">>],
                              [{style, MinW}])],
                       [<<"ah-scheduler-dayview-header">>], []),
                 AllDay,
                 ?H:el('div',
                       ?H:el('div',
                             [?H:el('div', Slots, [<<"ah-scheduler-dayview-gutter">>],
                                    [{aria_hidden, <<"true">>}]),
                              ?H:el('div', Columns, [<<"ah-scheduler-dayview-cols">>],
                                    [{style, MinW}]),
                              ?H:el('div', [], [<<"ah-scheduler-dayview-now-indicator">>],
                                    [{style, <<"display:none;">>}])],
                             [<<"ah-scheduler-dayview-body">>], []),
                       [<<"ah-scheduler-dayview-scroll">>], [])],
                [<<"ah-scheduler-dayview-hscroll">>], []),
          [<<"ah-scheduler-dayview">>], []).

res_id(undefined) -> undefined;
res_id(#{id := Id}) -> Id.

day_header_cell(D, Today, L) ->
    ?H:el('div',
          [?H:el(span, fmt(D * ?DAY, <<"EEE">>, L), [<<"ah-scheduler-dayview-header-day">>], []),
           ?H:el(span, fmt(D * ?DAY, <<"d">>, L), [<<"ah-scheduler-dayview-header-date">>], [])],
          [<<"ah-scheduler-dayview-header-cell">>,
           [<<" ah-scheduler-dayview-header-today">> || D =:= Today]], []).

hour_label(H, #{hf := 24}) -> <<(?D:pad(H))/binary, ":00">>;
hour_label(H, #{labels := L}) ->
    H12 = case H rem 12 of 0 -> 12; X -> X end,
    <<(integer_to_binary(H12))/binary, " ",
      (maps:get(case H < 12 of true -> am; false -> pm end, L))/binary>>.

allday_event(I, Cfg) ->
    ?H:el('div', ev_title(I, span, <<"ah-scheduler-event-title">>),
          ev_classes(<<"ah-scheduler-event ah-scheduler-allday-event">>, I),
          [ev_attrs(I, Cfg), {style, ev_style(I)}]).

day_column(D, Res, Insts, TotalH, #{ds := DS, de := DE, sd := SD, sh := SH,
                                     today := Today, editable := Editable} = Cfg) ->
    W0 = D * ?DAY + DS * 60,
    W1 = D * ?DAY + DE * 60,
    Timed = [I#{cs => max(S, W0), ce => max(min(E, W1), max(S, W0) + SD)}
             || #{s := S, e := E, ev := #{all_day := false}} = I <- Insts,
                S < W1, E > W0 orelse (S =:= E andalso S >= W0),
                res_matches(I, Res)],
    Placed = assign_columns(Timed),
    Events = [?H:el('div',
                    [ev_title(I, 'div', <<"ah-scheduler-event-title">>),
                     ?H:el('div', time_range(S, E, false, Cfg), [<<"ah-scheduler-event-time">>], []),
                     [?H:el('div', [], [<<"ah-scheduler-timegrid-resize-handle">>], []) || Editable]],
                    ev_classes(<<"ah-scheduler-event ah-scheduler-timegrid-event">>, I),
                    [ev_attrs(I, Cfg),
                     {style, [<<"position:absolute;top:">>, num((Cs - W0) / SD * SH),
                              <<"px;height:">>, num((Ce - Cs) / SD * SH),
                              <<"px;left:">>, num(Col * 100 / Total), <<"%;width:">>,
                              num(100 / Total), <<"%;">>, ev_style(I)]}])
              || {#{s := S, e := E, cs := Cs, ce := Ce} = I, Col, Total} <- Placed],
    case Res of
        undefined ->
            ?H:el('div', Events,
                  [<<"ah-scheduler-dayview-col">>, [<<" ah-scheduler-dayview-col-today">> || D =:= Today]],
                  [{data_date, ?D:iso_date(D)},
                   {style, [<<"position:relative;height:">>, num(TotalH), <<"px;">>]}]);
        #{id := RId} ->
            ?H:el('div', Events, [<<"ah-scheduler-dayview-res-col">>],
                  [{data_date, ?D:iso_date(D)}, {data_resourceid, RId},
                   {style, [<<"position:relative;height:">>, num(TotalH),
                            <<"px;border-left:1px solid var(--ah-color-grey-100);">>]}])
    end.

%% sigil's assign-columns: by start, each item into the first column
%% where it does not overlap. Returns [{Item, Column, Columns}].
assign_columns([]) -> [];
assign_columns(Items) ->
    Sorted = lists:sort(fun(#{cs := A}, #{cs := B}) -> A =< B end, Items),
    {Cols, Placed} =
        lists:foldl(
          fun(#{cs := S, ce := E} = I, {Cs, Acc}) ->
                  Free = fun(Col) -> not lists:any(fun({S2, E2}) -> S < E2 andalso E > S2 end,
                                                   Col)
                         end,
                  case index_of(Free, Cs) of
                      0 -> {Cs ++ [[{S, E}]], [{I, length(Cs)} | Acc]};
                      N -> {setnth(N, Cs, [{S, E} | lists:nth(N, Cs)]), [{I, N - 1} | Acc]}
                  end
          end, {[], []}, Sorted),
    Total = length(Cols),
    [{I, C, Total} || {I, C} <- lists:reverse(Placed)].

index_of(F, L) -> index_of(F, L, 1).
index_of(_, [], _) -> 0;
index_of(F, [X | Xs], N) -> case F(X) of true -> N; false -> index_of(F, Xs, N + 1) end.

setnth(1, [_ | T], X) -> [X | T];
setnth(N, [H | T], X) -> [H | setnth(N - 1, T, X)].

%%%-------------------------------------------------------------------
%%% Month view
%%%-------------------------------------------------------------------

month_view(Insts, #{rs := RS, re := RE, cur := Cur, labels := L} = Cfg) ->
    Names = [fmt((RS + K) * ?DAY, <<"EEE">>, L) || K <- lists:seq(0, 6)],
    {_, CurMonth, _} = calendar:gregorian_days_to_date(Cur),
    Weeks = [lists:seq(W, W + 6) || W <- lists:seq(RS, RE - 1, 7)],
    ?H:el('div',
          [?H:el('div', [?H:el('div', N, [<<"ah-scheduler-monthview-header-cell">>], [])
                         || N <- Names],
                 [<<"ah-scheduler-monthview-header">>], [{aria_hidden, <<"true">>}]),
           ?H:el('div', [week_row(W, Insts, CurMonth, Cfg) || W <- Weeks],
                 [<<"ah-scheduler-monthview-body">>], [])],
          [<<"ah-scheduler-monthview">>], []).

week_row([WS | _] = Days, Insts, CurMonth, #{today := Today, max := Max, labels := L,
                                              resources := Res} = Cfg) ->
    Segs = week_segments(WS, Insts),
    Visible = [S || #{row := Rw} = S <- Segs, Rw < Max],
    Overflow = lists:foldl(fun(#{row := Rw, sc := Sc, ec := Ec}, Acc) when Rw >= Max ->
                                   lists:foldl(fun(C, A) -> maps:update_with(C, fun(X) -> X + 1 end,
                                                                             1, A)
                                               end, Acc, lists:seq(Sc, Ec - 1));
                              (_, Acc) -> Acc
                           end, #{}, Segs),
    Other = fun(D) -> element(2, calendar:gregorian_days_to_date(D)) =/= CurMonth end,
    ?H:el('div',
          [?H:el('div',
                 [?H:el('div', [], [<<"ah-scheduler-day">>,
                                    [<<" ah-scheduler-day-today">> || D =:= Today],
                                    [<<" ah-scheduler-day-other">> || Other(D)]],
                        [{data_date, ?D:iso_date(D)}]) || D <- Days],
                 [<<"ah-scheduler-week-bg">>], []),
           ?H:el('div',
                 [[?H:el('div', fmt(D * ?DAY, <<"d">>, L),
                         [<<"ah-scheduler-day-num">>,
                          [<<" ah-scheduler-day-num-today">> || D =:= Today],
                          [<<" ah-scheduler-day-num-other">> || Other(D)]],
                         [{style, [<<"grid-column:">>, integer_to_binary(K), <<";grid-row:1;">>]}])
                   || {K, D} <- lists:zip(lists:seq(1, 7), Days)],
                  [month_event(S, Res, Cfg) || S <- Visible],
                  [more_link(Col, lists:nth(Col, Days), N, Insts, Cfg)
                   || {Col, N} <- lists:sort(maps:to_list(Overflow))]],
                 [<<"ah-scheduler-week-content">>], [])],
          [<<"ah-scheduler-week-row">>], []).

%% sigil's compute-week-segments: every instance overlapping the week as
%% a bar from column sc to ec (1-based, ec exclusive) in the first free row.
week_segments(WS, Insts) ->
    W0 = WS * ?DAY, W1 = (WS + 7) * ?DAY,
    Segs = lists:filtermap(
             fun(#{s := S, e := E, ev := #{all_day := AD}} = I) ->
                     VS = S div ?DAY * ?DAY,
                     VE = case AD of true -> max(E, VS + ?DAY); false -> VS + ?DAY end,
                     case overlaps(I, W0, W1) of
                         false -> false;
                         true ->
                             Cs = max(VS, W0), Ce = min(VE, W1),
                             Sc = Cs div ?DAY - WS + 1,
                             Ec = (Ce + ?DAY - 1) div ?DAY - WS + 1,
                             Span = Ec - Sc,
                             Span > 0 andalso
                                 {true, #{inst => I, sc => Sc, ec => Ec, span => Span,
                                          multi => (VE - VS) > ?DAY,
                                          continuation => VS < W0, continues => VE > W1}}
                     end
             end, Insts),
    Sorted = lists:sort(fun(A, B) -> seg_key(A) =< seg_key(B) end, Segs),
    {_, Out} = lists:foldl(
                 fun(#{sc := Sc, ec := Ec} = Seg, {Rows, Acc}) ->
                         Free = fun(Rg) -> not lists:any(fun({A, B}) -> Sc < B andalso Ec > A end, Rg) end,
                         case index_of(Free, Rows) of
                             0 -> {Rows ++ [[{Sc, Ec}]], [Seg#{row => length(Rows)} | Acc]};
                             N -> {setnth(N, Rows, [{Sc, Ec} | lists:nth(N, Rows)]),
                                   [Seg#{row => N - 1} | Acc]}
                         end
                 end, {[], []}, Sorted),
    lists:reverse(Out).

seg_key(#{multi := M, span := Span, inst := #{s := S}}) ->
    {case M of true -> 0; false -> 1 end, -Span, S rem ?DAY}.

month_event(#{inst := #{s := S, ev := #{all_day := AD, resource := RId, title := T}} = I,
              sc := Sc, ec := Ec, row := Rw, multi := Multi,
              continuation := Cont, continues := Conts}, Res, Cfg) ->
    Dot = [?H:el(span, [], [<<"ah-scheduler-month-event-resource-dot">>],
                 [{style, [<<"background:">>, C, <<";">>]}])
           || #{id := Rid, color := C} <- Res, Rid =:= RId],
    ?H:el('div',
          [Dot,
           [?H:el(span, clock(S, Cfg), [<<"ah-scheduler-event-time">>], []) || not AD, not Multi],
           ev_title(I, span, <<"ah-scheduler-event-title">>)],
          [ev_classes(<<"ah-scheduler-event ah-scheduler-month-event">>, I),
           [<<" ah-scheduler-month-event-multi">> || Multi],
           [<<" ah-scheduler-month-event-start">> || Cont],
           [<<" ah-scheduler-month-event-end">> || Conts]],
          [ev_attrs(I, Cfg), {title, T},
           {style, [<<"grid-column:">>, integer_to_binary(Sc), <<"/">>, integer_to_binary(Ec),
                    <<";grid-row:">>, integer_to_binary(Rw + 2), <<";">>, ev_style(I)]}]).

%% "+n more" and, as an inert template, the day's popover.
more_link(Col, D, N, Insts, #{max := Max, labels := L} = Cfg) ->
    Day = lists:sort(fun(#{s := A}, #{s := B}) -> A =< B end,
                     [I || I <- Insts, overlaps(I, D * ?DAY, (D + 1) * ?DAY)]),
    Popover = [?H:el('div',
                     [?H:el(span, fmt(D * ?DAY, maps:get(popover_date, L), L), [], []),
                      ?H:el(span, <<"×"/utf8>>, [<<"ah-scheduler-more-popover-close">>],
                            [{role, button}, {tabindex, 0}, {aria_label, <<"Close">>}])],
                     [<<"ah-scheduler-more-popover-header">>], []),
               ?H:el('div',
                     [?H:el('div',
                            [?H:el(span, time_range(S, E, AD, Cfg), [<<"ah-scheduler-event-time">>], []),
                             ev_title(I, span, <<"ah-scheduler-event-title">>)],
                            ev_classes(<<"ah-scheduler-more-popover-item ah-scheduler-event">>, I),
                            [ev_attrs(I, Cfg), {style, ev_style(I)}])
                      || #{s := S, e := E, ev := #{all_day := AD}} = I <- Day],
                     [<<"ah-scheduler-more-popover-body">>], [])],
    ?H:el('div',
          [replace(maps:get(more, L), <<"{n}">>, integer_to_binary(N)),
           ?H:el(template, Popover, [], [])],
          [<<"ah-scheduler-day-more">>],
          [{data_date, ?D:iso_date(D)}, {role, button}, {tabindex, 0}, {aria_haspopup, dialog},
           {style, [<<"grid-column:">>, integer_to_binary(Col),
                    <<";grid-row:">>, integer_to_binary(Max + 2), <<";">>]}]).

%%%-------------------------------------------------------------------
%%% Agenda view
%%%-------------------------------------------------------------------

agenda_view(Insts, #{rs := RS, re := RE, labels := L, resources := Res} = Cfg) ->
    Groups = [{D, lists:sort(fun(#{s := A}, #{s := B}) -> A =< B end,
                             [I || I <- Insts, overlaps(I, D * ?DAY, (D + 1) * ?DAY)])}
              || D <- lists:seq(RS, RE - 1)],
    Body = case [G || {_, [_ | _]} = G <- Groups] of
               [] ->
                   ?H:el('div',
                   [?H:el('div', <<"📅"/utf8>>, [<<"ah-scheduler-agenda-empty-icon">>],
                          [{aria_hidden, <<"true">>}]),
                    ?H:el('div', maps:get(no_events, L), [], []),
                    ?H:el('div', maps:get(hint_navigate, L), [<<"ah-scheduler-agenda-empty-hint">>], [])],
                   [<<"ah-scheduler-agenda-empty">>], []);
               Gs ->
                   [?H:el('div',
                          [?H:el('div',
                                 [?H:el(span, fmt(D * ?DAY, <<"EEEE">>, L),
                                        [<<"ah-scheduler-agenda-day-name">>], []),
                                  ?H:el(span, fmt(D * ?DAY, maps:get(agenda_date, L), L),
                                        [<<"ah-scheduler-agenda-day-date">>], [])],
                                 [<<"ah-scheduler-agenda-day-header">>], []),
                           ?H:el('div', [agenda_row(I, Res, Cfg) || I <- Is],
                                 [<<"ah-scheduler-agenda-day-events">>], [])],
                          [<<"ah-scheduler-agenda-day-group">>], [])
                    || {D, Is} <- Gs]
           end,
    ?H:el('div', Body, [<<"ah-scheduler-agenda">>], []).

agenda_row(#{s := S, e := E, ev := #{all_day := AD, status := St, resource := RId}} = I, Res, Cfg) ->
    R = [X || #{id := Rid} = X <- Res, Rid =:= RId],
    ?H:el('div',
          [?H:el('div', [], [<<"ah-scheduler-agenda-event-status">>],
                 [{style, [<<"background:">>, status_color(St), <<";">>]}]),
           [?H:el('div', [], [<<"ah-scheduler-agenda-event-resource-dot">>],
                  [{style, [<<"background:">>, C, <<";">>]}]) || #{color := C} <- R],
           ?H:el('div', time_range(S, E, AD, Cfg), [<<"ah-scheduler-agenda-event-time">>], []),
           ev_title(I, 'div', <<"ah-scheduler-agenda-event-title">>),
           [?H:el('div', N, [<<"ah-scheduler-agenda-event-resource-name">>], []) || #{name := N} <- R]],
          [<<"ah-scheduler-agenda-event">>],
          ev_attrs(I, Cfg)).

%%%-------------------------------------------------------------------
%%% Timeline views: a row per resource on a horizontal time axis
%%%-------------------------------------------------------------------

timeline_view(Insts, #{view := View, cur := Cur, rs := RS, re := RE, ds := DS, de := DE,
                       today := Today, resources := Res, labels := L,
                       editable := Editable} = Cfg) ->
    {TS, TE, Slots} =
        case View of
            timeline_day ->
                {Cur * ?DAY + DS * 60, Cur * ?DAY + DE * 60,
                 [{Cur * ?DAY + H * 60, fmt(Cur * ?DAY + H * 60, <<"HH:mm">>, L), false}
                  || H <- lists:seq(DS, DE - 1)]};
            timeline_week ->
                {RS * ?DAY, RE * ?DAY,
                 [{D * ?DAY, fmt(D * ?DAY, <<"EEE d">>, L), D =:= Today}
                  || D <- lists:seq(RS, RE - 1)]};
            timeline_month ->
                {RS * ?DAY, RE * ?DAY,
                 [{D * ?DAY, fmt(D * ?DAY, <<"d">>, L), D =:= Today}
                  || D <- lists:seq(RS, RE - 1)]}
        end,
    Total = TE - TS,
    SlotW = case View of timeline_day -> 60; timeline_week -> 100; timeline_month -> 40 end,
    Width = max(100, length(Slots) * SlotW),
    Pct = 100 / length(Slots),
    Lines = [?H:el('div', [], [<<"ah-scheduler-timeline-grid-line">>],
                   [{style, [<<"left:">>, num((S - TS) * 100 / Total), <<"%;">>]}])
             || {S, _, _} <- Slots],
    Bar = fun(#{s := S, e := E} = I) ->
                  Left = max(0, S - TS) * 100 / Total,
                  Right = min(Total, E - TS) * 100 / Total,
                  ?H:el('div',
                        [ev_title(I, 'div', <<"ah-scheduler-timeline-event-title">>),
                         [[?H:el('div', [], [<<"ah-scheduler-timeline-resize-handle ah-scheduler-timeline-resize-left">>], []),
                           ?H:el('div', [], [<<"ah-scheduler-timeline-resize-handle ah-scheduler-timeline-resize-right">>], [])]
                          || Editable]],
                        ev_classes(<<"ah-scheduler-timeline-event">>, I),
                        [ev_attrs(I, Cfg),
                         {style, [<<"position:absolute;left:">>, num(Left), <<"%;width:">>,
                                  num(max(0.5, Right - Left)), <<"%;">>, ev_style(I)]}])
          end,
    InRange = [I || #{ev := #{all_day := AD}} = I <- Insts, overlaps(I, TS, TE),
                    not (AD andalso View =:= timeline_day)],
    Row = fun(R) ->
                  ?H:el('div',
                        ?H:el('div', [Lines, [Bar(I) || I <- InRange, res_matches(I, R)]],
                              [<<"ah-scheduler-timeline-row-events">>],
                              [{style, <<"position:relative;height:100%;">>}]),
                        [<<"ah-scheduler-timeline-row">>], [{data_resourceid, res_id(R)}])
          end,
    Panel = case Res of
                [] -> ?H:el('div', ?H:el('div', <<"—"/utf8>>, [<<"ah-scheduler-timeline-resource-name">>], []),
                            [<<"ah-scheduler-timeline-resource-cell">>], []);
                _ -> [?H:el('div',
                            [?H:el('div', [], [<<"ah-scheduler-timeline-resource-dot">>],
                                   [{style, [<<"background:">>, C, <<";">>]}]),
                             ?H:el('div', N, [<<"ah-scheduler-timeline-resource-name">>], [])],
                            [<<"ah-scheduler-timeline-resource-cell">>], [{data_resourceid, Id}])
                      || #{id := Id, name := N, color := C} <- Res]
            end,
    ?H:el('div',
          [?H:el('div',
                 [?H:el('div', [], [<<"ah-scheduler-timeline-resource-header">>], []),
                  ?H:el('div',
                        [?H:el('div', Label,
                               [<<"ah-scheduler-timeline-slot-header">>,
                                [<<" ah-scheduler-timeline-slot-today">> || IsToday]],
                               [{style, [<<"width:">>, num(Pct), <<"%;">>]}])
                         || {_, Label, IsToday} <- Slots],
                        [<<"ah-scheduler-timeline-slots-header">>],
                        [{style, [<<"min-width:">>, num(Width), <<"px;">>]}])],
                 [<<"ah-scheduler-timeline-header">>], [{aria_hidden, <<"true">>}]),
           ?H:el('div',
                 [?H:el('div', Panel, [<<"ah-scheduler-timeline-resource-panel">>], []),
                  ?H:el('div',
                        ?H:el('div', case Res of
                                         [] -> Row(undefined);
                                         _ -> [Row(R) || R <- Res]
                                     end,
                              [<<"ah-scheduler-timeline-grid">>],
                              [{data_from, ?D:iso_time(TS, false)}, {data_to, ?D:iso_time(TE, false)},
                               {style, [<<"min-width:">>, num(Width), <<"px;">>]}]),
                        [<<"ah-scheduler-timeline-scroll">>], [])],
                 [<<"ah-scheduler-timeline-body">>], [])],
          [<<"ah-scheduler-timeline">>], []).

%%%-------------------------------------------------------------------
%%% Navigation round trip
%%%-------------------------------------------------------------------

%% @doc The range a scheduler's navigation event asks for: its view, the
%% date shown and the visible days `start' .. `end' (exclusive), as ISO
%% dates, computed from `Event.value' and the view, first day and agenda
%% length in `Event.data'.
-spec scheduler_range(aihtml_action:event()) ->
          #{view := view(), date := binary(), start := binary(), 'end' := binary()}.
scheduler_range(#{data := Data} = Event) ->
    View = event_view(Data),
    D = event_date(Event),
    First = event_int(<<"firstDay">>, Data, 1, 0, 6),
    Agenda = event_int(<<"agendaDays">>, Data, 30, 1, 3660),
    {S, E} = profile(View, D, First, Agenda),
    #{view => View, date => ?D:iso_date(D), start => ?D:iso_date(S), 'end' => ?D:iso_date(E)}.

%% @doc Answer a scheduler's navigation (or an edit): render `Scheduler'
%% (built from the events of the requested range; its value and view are
%% taken from `Event') in place of the scheduler that fired `Event'.
%% Sends one morph operation, so the scroll position is kept.
-spec scheduler_update(aihtml_action:ctx(), aihtml_action:event(), #ah_scheduler{}) -> ok.
scheduler_update(Ctx, #{id := Id, data := Data} = Event, #ah_scheduler{} = S) ->
    S1 = S#ah_scheduler{id = Id, value = ?D:iso_date(event_date(Event)), view = event_view(Data)},
    aihtml_action:html(Ctx, {id, Id}, S1, morph).

event_view(Data) ->
    V = maps:get(<<"view">>, Data, <<"week">>),
    case [X || X <- ?VIEWS, atom_to_binary(X) =:= V] of
        [View] -> View;
        [] -> error({aihtml, {bad_scheduler_view, V}})
    end.

event_date(#{value := V}) when is_binary(V), V =/= <<>> -> ?D:day_of(V);
event_date(#{data := #{<<"date">> := V}}) -> ?D:day_of(V);
event_date(_) -> element(1, ?D:today(undefined)).

event_int(K, Data, Default, Lo, Hi) ->
    try binary_to_integer(maps:get(K, Data)) of
        N when N >= Lo, N =< Hi -> N;
        _ -> Default
    catch _:_ -> Default
    end.

%%%-------------------------------------------------------------------
%%% Dates
%%%-------------------------------------------------------------------

%% A display format: yyyy MMMM MMM MM M dd d EEEE EEE HH mm; other
%% characters are copied.
fmt(Min, Format, L) -> iolist_to_binary(fmt_tokens(Format, Min, L)).

fmt_tokens(<<>>, _, _) -> [];
fmt_tokens(<<"yyyy", R/binary>>, T, L) -> [integer_to_binary(y(T)) | fmt_tokens(R, T, L)];
fmt_tokens(<<"MMMM", R/binary>>, T, L) -> [lists:nth(m(T), maps:get(months, L)) | fmt_tokens(R, T, L)];
fmt_tokens(<<"MMM", R/binary>>, T, L) -> [lists:nth(m(T), maps:get(months_short, L)) | fmt_tokens(R, T, L)];
fmt_tokens(<<"MM", R/binary>>, T, L) -> [?D:pad(m(T)) | fmt_tokens(R, T, L)];
fmt_tokens(<<"M", R/binary>>, T, L) -> [integer_to_binary(m(T)) | fmt_tokens(R, T, L)];
fmt_tokens(<<"dd", R/binary>>, T, L) -> [?D:pad(d(T)) | fmt_tokens(R, T, L)];
fmt_tokens(<<"d", R/binary>>, T, L) -> [integer_to_binary(d(T)) | fmt_tokens(R, T, L)];
fmt_tokens(<<"EEEE", R/binary>>, T, L) -> [lists:nth(?D:dow(T div ?DAY) + 1, maps:get(weekdays, L)) | fmt_tokens(R, T, L)];
fmt_tokens(<<"EEE", R/binary>>, T, L) -> [lists:nth(?D:dow(T div ?DAY) + 1, maps:get(weekdays_short, L)) | fmt_tokens(R, T, L)];
fmt_tokens(<<"HH", R/binary>>, T, L) -> [?D:pad(T rem ?DAY div 60) | fmt_tokens(R, T, L)];
fmt_tokens(<<"mm", R/binary>>, T, L) -> [?D:pad(T rem 60) | fmt_tokens(R, T, L)];
fmt_tokens(<<C/utf8, R/binary>>, T, L) -> [<<C/utf8>> | fmt_tokens(R, T, L)].

y(T) -> element(1, calendar:gregorian_days_to_date(T div ?DAY)).
m(T) -> element(2, calendar:gregorian_days_to_date(T div ?DAY)).
d(T) -> element(3, calendar:gregorian_days_to_date(T div ?DAY)).

%% An appointment's rule, which must have a FREQ.
parse_rrule(R) -> aihtml_lib_rrule:parse(R, #{require_freq => true}).

%% @doc Functions besides the component that the aihtml facade re-exports.
-spec facade_extras() -> [{atom(), arity()}].
facade_extras() -> [{scheduler_update, 3}, {scheduler_range, 1}].

%%%===================================================================
%%% Internal
%%%===================================================================

%% A root without an id gets one: the behaviour and the update functions
%% refer to it. Returns the id and the record holding it, for root_attrs/2.
ensure_id(R) ->
    Id = case R#ah_scheduler.id of
             undefined -> <<"ah-s", (integer_to_binary(erlang:unique_integer([positive])))/binary>>;
             Id0 -> text(Id0)
         end,
    {Id, R#ah_scheduler{id = Id}}.

hidden(undefined, _) -> [];
hidden(Name, Value) -> ?H:void(input, [], [{type, hidden}, {name, Name}, {value, Value}]).

height_style(undefined) -> undefined;
height_style(H) when is_integer(H), H > 0 -> [<<"height:">>, integer_to_binary(H), <<"px;">>];
height_style(H) -> error({aihtml, {bad_option, height, H}}).

pos_int(_, N) when is_integer(N), N > 0 -> N;
pos_int(K, V) -> error({aihtml, {bad_option, K, V}}).

in_range(_, N, Lo, Hi) when is_integer(N), N >= Lo, N =< Hi -> N;
in_range(K, V, _, _) -> error({aihtml, {bad_option, K, V}}).

%% Merge custom texts over the defaults: unknown keys fail, lists of
%% months (12) and weekdays (7) are checked.
labels(Defaults, Custom, Comp) when is_map(Custom) ->
    maps:foreach(fun(K, _) -> maps:is_key(K, Defaults)
                                  orelse error({aihtml, {bad_label, Comp, K}})
                 end, Custom),
    M = maps:map(fun(_, V) when is_list(V), V =/= [], not is_integer(hd(V)) -> [text(X) || X <- V];
                    (_, V) -> text(V)
                 end, maps:merge(Defaults, Custom)),
    [length(maps:get(K, M)) =:= N orelse error({aihtml, {bad_label, Comp, K}})
     || {K, N} <- [{months, 12}, {months_short, 12}, {weekdays, 7}, {weekdays_short, 7}],
        maps:is_key(K, M)],
    M;
labels(_, Other, Comp) -> error({aihtml, {bad_label, Comp, Other}}).

replace(B, Pat, With) -> iolist_to_binary(string:replace(B, Pat, With, all)).

%% A colour for a style attribute: nothing that could end the declaration.
color(C) ->
    B = text(C),
    case re:run(B, <<"^[#a-zA-Z0-9(),.%\\s-]+$">>) of
        {match, _} -> B;
        nomatch -> error({aihtml, {bad_color, C}})
    end.

%% A CSS number: integers as such, others with up to 3 decimals.
num(N) when is_integer(N) -> integer_to_binary(N);
num(F) when is_float(F) ->
    R = round(F),
    case abs(F - R) < 0.0005 of
        true -> integer_to_binary(R);
        false -> float_to_binary(F, [{decimals, 3}, compact])
    end.

text(undefined) -> undefined;
text(B) when is_binary(B) -> B;
text(A) when is_atom(A) -> atom_to_binary(A);
text(I) when is_integer(I) -> integer_to_binary(I);
text(L) when is_list(L) -> unicode:characters_to_binary(L);
text(X) -> beamai_html_escape:to_binary(X, aihtml).

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => scheduler, category => data,
       signature => <<"ah_scheduler(Events, Value, Css, Attrs)">>,
       root => <<"ah-scheduler">>,
       flags => [editable, no_all_day],
       classes => #{editable => [], no_all_day => []},
       options => [view, views, resources, first_day, slot_duration, slot_height,
                   day_start, day_end, height, agenda_days, day_max_events, hour_format,
                   today, toolbar, source, href, labels],
       behavior => <<"scheduler">>,
       events => [<<"change">>, <<"ah:event-click">>, <<"ah:event-change">>,
                  <<"ah:event-edit">>, <<"ah:event-delete">>, <<"ah:event-copy">>,
                  <<"ah:select">>, <<"ah:more-click">>],
       doc => <<"A resource scheduler: day and week time grids with a column per resource, "
                "month grid, agenda list and timeline rows, all rendered on the server; "
                "navigation loads the new range through an action.">>,
       option_docs =>
           #{editable => <<"Drag appointments (time, day, resource), resize them, drag over free "
                           "time to select a range (ah:select); right click or Shift+F10 opens a "
                           "menu (edit, delete, copy, new). Arrow keys move a focused "
                           "appointment, Shift+Up/Down resizes it.">>,
             no_all_day => <<"No all day row in the day and week views.">>,
             view => <<"day, week (default), month, agenda, timeline_day, timeline_week or "
                       "timeline_month.">>,
             views => <<"View buttons in the toolbar (default [day, week, month, agenda]).">>,
             resources => <<"Resources #{id, name, color}: columns of the day and week views, "
                            "rows of the timelines; appointments name theirs with resource.">>,
             first_day => <<"First day of the week, 0 = Sunday .. 6 (default 1).">>,
             slot_duration => <<"Minutes per time slot, a divisor of 60 (default 30).">>,
             slot_height => <<"Height of a slot in px (default 20).">>,
             day_start => <<"First hour shown in the time grid and timeline day (default 0).">>,
             day_end => <<"Hour the time grid ends (default 24).">>,
             height => <<"Height in px (default 600).">>,
             agenda_days => <<"Days in the agenda view (default 30).">>,
             day_max_events => <<"Rows of a month week before \"+n more\" (default 3).">>,
             hour_format => <<"12 (default) or 24.">>,
             today => <<"The date highlighted as today (default the server's date).">>,
             toolbar => <<"Show the toolbar (default true). Its navigation buttons appear only "
                          "with a source, a change postback or href.">>,
             href => <<"URL template with {date} (ISO) and {view}: prev, today, next and the "
                       "view buttons become links to that state, which the server renders "
                       "when a link is opened directly (crawlers, new tabs, bookmarks). With a "
                       "source or change postback a plain click navigates in the page and "
                       "pushes the link's URL; without one it follows the link.">>,
             source => <<"Action ref {Module, Action, Args} bound to change: it loads the range "
                         "the event names (scheduler_range/1) and answers with "
                         "scheduler_update/3.">>,
             labels => <<"Map of toolbar, view and menu texts, am, pm, months, months_short, "
                         "weekdays, weekdays_short (from Sunday) and the formats title_day, "
                         "title_month, range_start, range_end, agenda_date, popover_date.">>},
       methods =>
           [#{name => navigate, args => <<"(\"prev\" | \"next\" | \"today\")">>,
              doc => <<"Navigate as the toolbar buttons do (fires change).">>},
            #{name => setView, args => <<"(View)">>,
              doc => <<"Switch the view (fires change).">>},
            #{name => gotoDate, args => <<"(Iso)">>,
              doc => <<"Show another date (fires change).">>},
            #{name => getValue, args => <<"()">>, doc => <<"Return the shown date.">>}]}].
