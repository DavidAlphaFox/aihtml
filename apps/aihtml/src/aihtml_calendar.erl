%%%-------------------------------------------------------------------
%%% @doc An event calendar, ported from sigil (form/calendar). See
%%% designs/04-components.md.
%%%
%%%   ah_calendar(Value, Css, Attrs)       an event calendar: month, week, day, agenda
%%%   set_events(Ctx, Target, Events)      (in an action) replace a calendar's events
%%%   add_event(Ctx, Target, Event)        (in an action) add one event
%%%
%%% A value-bearing component: `Attrs' go to the root, which carries
%%% `data-ah-value' and fires `change'; `name' goes to a hidden input. The
%%% behaviour is `calendar' (assets/js/components/calendar.ts).
%%%
%%% The value is the date the view shows. The server renders the toolbar
%%% and the first view; navigating (prev, next, today, the view buttons)
%%% re-renders in the browser from the same shared templates
%%% (templates/calendar_month, calendar_timegrid, calendar_list), with the
%%% same layout code on both sides (recurring events, multi-day bars,
%%% overlapping columns). Every navigation fires `change'; the root keeps
%%% `data-view', `data-start' and `data-end' (the visible range, end
%%% exclusive), so a postback receives them in `Event.data' and may answer
%%% with `set_events(Ctx, Event, Events)' to load the range lazily.
%%%
%%% With `{href, <<"/events?date={date}&view={view}">>}' the toolbar's
%%% prev, next, today and view buttons are links (`<a href>') to the date
%%% and view they lead to, so every state has a URL the server can render
%%% by itself (crawlers and pages opened without script follow them; the
%%% page reads `date' and `view' from its query). A plain click is still
%%% handled in the browser, which also pushes the link's URL, so back,
%%% forward and bookmarks work (going back reloads that URL); modified
%%% clicks (new tab) follow the link. The links are kept pointing at the
%%% neighbours of the shown date.
%%%
%%% Interactions fire component events on the root, after writing their
%%% details to data attributes (so that `Event.data' carries them):
%%%
%%%   ah:event-click    event (the event's id)
%%%   ah:event-drop     event, from, to (new start and end), days, allDay
%%%   ah:event-resize   event, from, to
%%%   ah:select         from, to (end exclusive), allDay
%%%   ah:more-click     date (then the day view opens, unless prevented)
%%%
%%% ah_calendar/3 builds an #ah_calendar{} (include/aihtml_calendar.hrl) and
%%% render/1 turns it into HTML, so pages may also write the record
%%% directly (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_calendar).
-behaviour(aihtml_element).

-include("aihtml_calendar.hrl").

-export([ah_calendar/3, set_events/3, add_event/3,
         render/1, fields/1, catalog/0, facade_extras/0]).

-export_type([element/0, event/0, status/0, view/0, label_key/0, labels/0]).

-define(H, aihtml_html).
-define(E, aihtml_element).
-define(D, aihtml_lib_date).

%% Shared templates (see aihtml_tpl): also compiled to AH.tpl.* for the browser.
-compile({parse_transform, beamai_mustache_transform}).
-mustache_template({tpl_calendar_month, "../templates/calendar_month.mustache"}).
-mustache_template({tpl_calendar_timegrid, "../templates/calendar_timegrid.mustache"}).
-mustache_template({tpl_calendar_list, "../templates/calendar_list.mustache"}).

-type status() :: confirmed | tentative | cancelled | binary().
%% A calendar event. `start' is required; `end' defaults to one day (all
%% day events) or one hour later; a start without a time, or `all_day',
%% makes an all day event. `rrule' is an iCalendar RRULE subset (FREQ,
%% INTERVAL, COUNT, UNTIL, BYDAY, BYMONTHDAY, BYMONTH), `exdates' the days
%% left out of the series, `status' colours the agenda row.
-type event() :: #{id => term(), title => unicode:chardata(),
                   start := aihtml_lib_date:time(), 'end' => aihtml_lib_date:time(),
                   all_day => boolean(), color => unicode:chardata(),
                   rrule => unicode:chardata(), exdates => [aihtml_lib_date:date()],
                   status => status()}.
-type view() :: month | week | day | list.
-type label_key() :: today | prev | next | month | week | day | list
                   | all_day | all_day_short | more | no_events | no_events_hint
                   | am | pm | months | months_short | weekdays | weekdays_short
                   | title_month | title_day | range_start | range_end | list_date.
%% Texts of the calendar. `months', `months_short' (12), `weekdays' and
%% `weekdays_short' (7, from Sunday) are lists; `more' holds {n}; the
%% title_*, range_* and list_date keys are display formats (yyyy MMMM
%% MMM MM M dd d EEEE EEE).
-type labels() :: #{label_key() => unicode:chardata() | [unicode:chardata()]}.
-type element() :: #ah_calendar{}.

-define(VIEWS, [month, week, day, list]).
-define(DAY, 1440).
-define(DEFAULT_COLOR, <<"var(--ah-color-primary)">>).

%% @doc An event calendar (sigil's calendar). `Value' is the date the view
%% shows (an ISO date or `calendar:date()'; `undefined' is today).
%%
%% Css: `editable' (drag events to other days and times, resize timed
%% events), `selectable' (drag over days or times to select a range).
%% Options (in Attrs): `events' (a list of event maps, see `event()'),
%% `view' (month (default), week, day, list), `views' (the view buttons
%% shown, default all four), `first_day' (0 = Sunday .. 6), `agenda_days'
%% (days of the list view, default 30), `day_max_events' (event rows per
%% month cell, default 3), `slot_duration' (minutes, default 30),
%% `slot_height' (px, default 20), `height' (px, default 600; `undefined'
%% lets the content decide), `hour_format' (12 or 24), `href' (a URL
%% template with {date} and {view}: the toolbar becomes links, see the
%% module doc), `labels' (see `labels()').
-spec ah_calendar(aihtml_lib_date:date(), aihtml_html:css(), aihtml_html:attrs()) -> #ah_calendar{}.
ah_calendar(Value, Css, Attrs) ->
    ?E:build(?MODULE, #ah_calendar{value = Value}, Css, Attrs).

%% @doc The field names of #ah_calendar{}.
-spec fields(atom()) -> [atom()].
fields(ah_calendar) -> record_info(fields, ah_calendar).

-spec render(element()) -> aihtml_html:html().
render(#ah_calendar{view = View, views = Views, first_day = First,
                    height = Height, name = Name} = R0) ->
    {Id, R} = ensure_id(R0),
    Classes = ?E:classes(?MODULE, R),           % checks the flag fields first
    lists:member(View, ?VIEWS) orelse error({aihtml, {bad_calendar_view, View}}),
    (is_list(Views) andalso lists:all(fun(V) -> lists:member(V, ?VIEWS) end, Views))
        orelse error({aihtml, {bad_calendar_views, Views}}),
    check_first_day(First),
    [check_pos(K, V) || {K, V} <- [{agenda_days, R#ah_calendar.agenda_days},
                                   {day_max_events, R#ah_calendar.day_max_events},
                                   {slot_duration, R#ah_calendar.slot_duration},
                                   {slot_height, R#ah_calendar.slot_height}]],
    Height =:= undefined orelse check_pos(height, Height),
    lists:member(R#ah_calendar.hour_format, [12, 24])
        orelse error({aihtml, {bad_hour_format, R#ah_calendar.hour_format}}),
    L = cal_labels(R#ah_calendar.labels),
    Cur = case R#ah_calendar.value of
              undefined -> calendar:date_to_gregorian_days(date());
              V -> ?D:days(V)
          end,
    Events = normalize_events(R#ah_calendar.events),
    Opts = #{view => View, first_day => First, cur => Cur,
             agenda_days => R#ah_calendar.agenda_days,
             day_max_events => R#ah_calendar.day_max_events,
             slot_duration => R#ah_calendar.slot_duration,
             slot_height => R#ah_calendar.slot_height,
             hour_format => R#ah_calendar.hour_format,
             editable => R#ah_calendar.editable,
             today => calendar:date_to_gregorian_days(date()),
             labels => L},
    {RS, RE, Title} = profile(View, Cur, Opts),
    Value = ?D:iso_date(Cur),
    Href = R#ah_calendar.href,
    Nav = #{href => Href, cur => Cur, today => maps:get(today, Opts), view => View,
            agenda => R#ah_calendar.agenda_days},
    Toolbar =
        ?H:el('div',
              [?H:el('div',
                     [nav_btn(<<"‹"/utf8>>, <<"ah-calendar-btn ah-calendar-btn-prev">>,
                              [{aria_label, lbl(prev, L)}], nav_url(Nav, prev, View)),
                      nav_btn(<<"›"/utf8>>, <<"ah-calendar-btn ah-calendar-btn-next">>,
                              [{aria_label, lbl(next, L)}], nav_url(Nav, next, View)),
                      nav_btn(lbl(today, L), <<"ah-calendar-btn ah-calendar-btn-today">>,
                              [], nav_url(Nav, today, View))],
                     [<<"ah-calendar-toolbar-left">>], []),
               ?H:el('div', ?H:el(h2, Title, [<<"ah-calendar-title">>],
                                  [{id, sub_id(Id, <<"title">>)}, {aria_live, polite}]),
                     [<<"ah-calendar-toolbar-center">>], []),
               ?H:el('div',
                     [view_btn(V, View, lbl(V, L), nav_url(Nav, current, V)) || V <- Views],
                     [<<"ah-calendar-toolbar-right">>], [])],
              [<<"ah-calendar-toolbar">>], []),
    Body = aihtml_tpl:safe(view_html(View, Id, Cur, RS, RE, Events, Opts)),
    ?H:el('div',
          [Toolbar,
           hidden(Name, Value),
           ?H:el('div', Body, [<<"ah-calendar-view-container">>],
                 [{role, region}, {aria_labelledby, sub_id(Id, <<"title">>)}])],
          Classes,
          [[{id, Id}, {data_ah, <<"calendar">>}, {data_ah_value, Value},
            {data_ah_view, View},
            {data_ah_events, iolist_to_binary(aihtml_json:encode(Events))},
            {data_ah_first_day, First},
            {data_ah_agenda_days, R#ah_calendar.agenda_days},
            {data_ah_day_max_events, R#ah_calendar.day_max_events},
            {data_ah_slot_duration, R#ah_calendar.slot_duration},
            {data_ah_slot_height, R#ah_calendar.slot_height},
            {data_ah_hour_format, R#ah_calendar.hour_format},
            {data_ah_labels, case R#ah_calendar.labels of
                                 M when map_size(M) =:= 0 -> undefined;
                                 _ -> iolist_to_binary(aihtml_json:encode(L))
                             end},
            {data_view, View}, {data_start, ?D:iso_date(RS)}, {data_end, ?D:iso_date(RE)},
            {data_ah_href, case Href of undefined -> undefined; _ -> text(Href) end},
            {style, [[<<"height:">>, integer_to_binary(Height), <<"px">>]
                     || is_integer(Height)]}],
           ?E:root_attrs(R, change)]).

%% A toolbar button, or with the `href' option a link to the same state
%% (the behaviour follows it in the page; without script the server
%% renders the linked page).
nav_btn(Content, Class, Attrs, undefined) ->
    ?H:el(button, Content, [Class], [{type, button} | Attrs]);
nav_btn(Content, Class, Attrs, Url) ->
    ?H:el(a, Content, [Class], [{href, Url} | Attrs]).

view_btn(V, View, Label, undefined) ->
    ?H:el(button, Label, [<<"ah-calendar-view-btn">>, [<<"ah-calendar-view-btn-active">> || V =:= View]],
          [{type, button}, {data_view, V}, {aria_pressed, atom_to_binary(V =:= View)}]);
view_btn(V, View, Label, Url) ->
    ?H:el(a, Label, [<<"ah-calendar-view-btn">>, [<<"ah-calendar-view-btn-active">> || V =:= View]],
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
    B = binary:replace(text(Href), <<"{date}">>, ?D:iso_date(D), [global]),
    binary:replace(B, <<"{view}">>, atom_to_binary(V), [global]).

%% The day prev (-1) or next (1) shows (the twin of calStep in calendar.ts).
step(month, D, Dir, _) -> ?D:add_months(D, Dir);
step(week, D, Dir, _) -> D + 7 * Dir;
step(day, D, Dir, _) -> D + Dir;
step(list, D, Dir, Agenda) -> D + Agenda * Dir.

check_first_day(F) ->
    (is_integer(F) andalso F >= 0 andalso F =< 6) orelse error({aihtml, {bad_first_day, F}}).

check_pos(_, V) when is_integer(V), V > 0 -> ok;
check_pos(K, V) -> error({aihtml, {bad_option, K, V}}).

%%%-------------------------------------------------------------------
%%% Labels
%%%-------------------------------------------------------------------

%% The texts and date names of the current language (aihtml_i18n).
cal_label_defaults() ->
    maps:merge(aihtml_i18n:formats([months, months_short, weekdays, weekdays_short, am, pm]),
               aihtml_i18n:texts(calendar)).

cal_labels(Custom) ->
    aihtml_lib_calendar:labels(Custom, cal_label_defaults(), bad_calendar_label,
                               [{months, 12}, {months_short, 12}, {weekdays, 7},
                                {weekdays_short, 7}]).

lbl(K, L) -> maps:get(atom_to_binary(K), L).

%%%-------------------------------------------------------------------
%%% Events
%%%-------------------------------------------------------------------

%% Event maps as the browser gets them (data-ah-events, set_events/3):
%% binary keys, ISO times, explicit end and allDay. The JS normalizes the
%% same way (calNormalize) for events added in the browser.
normalize_events(Events) when is_list(Events) ->
    [normalize_event(E, N) || {N, E} <- lists:zip(lists:seq(1, length(Events)), Events)];
normalize_events(Other) ->
    error({aihtml, {bad_calendar_events, Other}}).

normalize_event(#{start := S0} = E, N) ->
    {S, DateOnly} = parse_time(S0),
    AllDayFlag = maps:get(all_day, E, false) =:= true orelse DateOnly,
    End = case maps:get('end', E, undefined) of
              undefined when AllDayFlag -> S + ?DAY;
              undefined -> S + 60;
              E0 -> element(1, parse_time(E0))
          end,
    AllDay = AllDayFlag orelse (S rem ?DAY =:= 0 andalso End rem ?DAY =:= 0 andalso S =/= End),
    Opt = fun(K, F) -> case maps:get(K, E, undefined) of
                           undefined -> [];
                           V -> [{atom_to_binary(K), F(V)}]
                       end
          end,
    maps:from_list(
      [{<<"id">>, case maps:get(id, E, undefined) of
                      undefined -> <<"ev", (integer_to_binary(N))/binary>>;
                      I -> text(I)
                  end},
       {<<"title">>, text(maps:get(title, E, <<>>))},
       {<<"start">>, ?D:iso_time(S, AllDay)},
       {<<"end">>, ?D:iso_time(End, AllDay)},
       {<<"allDay">>, AllDay}]
      ++ Opt(color, fun color/1)
      ++ Opt(rrule, fun(R) -> _ = aihtml_lib_rrule:parse(text(R)), text(R) end)
      ++ Opt(exdates, fun(Ds) -> [?D:iso_date(?D:days(D)) || D <- Ds] end)
      ++ Opt(status, fun text/1));
normalize_event(Other, _) ->
    error({aihtml, {bad_calendar_event, Other}}).

%% A colour for a style attribute: nothing that could end the declaration.
color(C) ->
    B = text(C),
    case re:run(B, <<"^[#a-zA-Z0-9(),.%\\s-]+$">>) of
        {match, _} -> B;
        nomatch -> error({aihtml, {bad_calendar_color, C}})
    end.

%% ISO date or date-time -> {minutes since gregorian day 0, date only?}.
%% The hours and minutes of a calendar:datetime() are not range checked.
parse_time({{_, _, _} = D, {H, Mi, _}}) when is_integer(H), is_integer(Mi) ->
    {?D:days(D) * ?DAY + H * 60 + Mi, false};
parse_time(T) -> ?D:parse_time(T).

%%%-------------------------------------------------------------------
%%% Recurrence (aihtml_lib_rrule)
%%%-------------------------------------------------------------------

%% Instances overlapping [RS, RE), in event order: #{id, src, s, e, all_day}
instances(Events, RS, RE) ->
    lists:append(
      [begin
           {S, _} = parse_time(maps:get(<<"start">>, Ev)),
           {E, _} = parse_time(maps:get(<<"end">>, Ev)),
           Id = maps:get(<<"id">>, Ev),
           AllDay = maps:get(<<"allDay">>, Ev),
           case maps:get(<<"rrule">>, Ev, undefined) of
               undefined when S < RE, E > RS ->
                   [#{id => Id, src => Ev, s => S, e => E, all_day => AllDay}];
               undefined -> [];
               Rule ->
                   Ex = maps:get(<<"exdates">>, Ev, []),
                   [#{id => <<Id/binary, "_", (aihtml_lib_rrule:stamp(Cs))/binary>>, src => Ev,
                      s => Cs, e => Ce, all_day => AllDay}
                    || {Cs, Ce} <- aihtml_lib_rrule:expand(S, E, aihtml_lib_rrule:parse(Rule),
                                                           RS, RE, Ex)]
           end
       end || Ev <- Events]).

in_range(Insts, From, To) ->
    [I || #{s := S, e := E} = I <- Insts, S < To, E > From].

%%%-------------------------------------------------------------------
%%% Views (the Erlang twin of calView in calendar.ts)
%%%-------------------------------------------------------------------

%% The visible range [RS, RE) in days and the title of a view.
profile(month, Cur, #{first_day := F, labels := L}) ->
    {Y, M, _} = calendar:gregorian_days_to_date(Cur),
    MS = calendar:date_to_gregorian_days(Y, M, 1),
    ME = calendar:date_to_gregorian_days(Y, M, calendar:last_day_of_the_month(Y, M)),
    {?D:start_of_week(MS, F), ?D:start_of_week(ME, F) + 7, fmt(Cur, lbl(title_month, L), L)};
profile(week, Cur, #{first_day := F, labels := L}) ->
    WS = ?D:start_of_week(Cur, F),
    {WS, WS + 7, range_title(WS, WS + 6, L)};
profile(day, Cur, #{labels := L}) ->
    {Cur, Cur + 1, fmt(Cur, lbl(title_day, L), L)};
profile(list, Cur, #{agenda_days := N, labels := L}) ->
    {Cur, Cur + N, range_title(Cur, Cur + N - 1, L)}.

range_title(A, B, L) ->
    <<(fmt(A, lbl(range_start, L), L))/binary, " – "/utf8,
      (fmt(B, lbl(range_end, L), L))/binary>>.

view_html(View, _Id, _Cur, RS, RE, Events, Opts) ->
    Insts = instances(Events, RS * ?DAY, RE * ?DAY),
    case View of
        month -> tpl_calendar_month(month_view(RS, RE, Insts, Opts));
        list -> tpl_calendar_list(list_view(RS, RE, Insts, Opts));
        _ -> tpl_calendar_timegrid(timegrid_view(RS, RE, Insts, Opts))
    end.

month_view(RS, RE, Insts, #{first_day := F, labels := L, today := Today, cur := Cur,
                            day_max_events := Max} = O) ->
    {_, CurM, _} = calendar:gregorian_days_to_date(Cur),
    Week = fun(W) ->
                   Segs = week_segments(W, Insts),
                   Over = lists:foldl(
                            fun(#{row := Row, sc := Sc, ec := Ec}, Acc) when Row >= Max ->
                                    lists:foldl(fun(C, A) -> A#{C => maps:get(C, A, 0) + 1} end,
                                                Acc, lists:seq(Sc, Ec - 1));
                               (_, Acc) -> Acc
                            end, #{}, Segs),
                   #{days => [begin
                                  D = W + I,
                                  {_, Dm, Dd} = calendar:gregorian_days_to_date(D),
                                  Other = Dm =/= CurM,
                                  #{bg_cls => cls([<<"ah-calendar-day">>,
                                                   {D =:= Today, <<"ah-calendar-day-today">>},
                                                   {Other, <<"ah-calendar-day-other">>}]),
                                    num_cls => cls([<<"ah-calendar-day-num">>,
                                                    {D =:= Today, <<"ah-calendar-day-num-today">>},
                                                    {Other, <<"ah-calendar-day-num-other">>}]),
                                    date => ?D:iso_date(D), col => integer_to_binary(I + 1),
                                    num => integer_to_binary(Dd)}
                              end || I <- lists:seq(0, 6)],
                     events => [seg_view(S, O) || #{row := Row} = S <- Segs, Row < Max],
                     more => [#{date => ?D:iso_date(W + C - 1), col => integer_to_binary(C),
                                row => integer_to_binary(Max + 2),
                                label => more_label(N, L)}
                              || {C, N} <- lists:sort(maps:to_list(Over)), N > 0]}
           end,
    #{headers => [#{label => lists:nth((F + I) rem 7 + 1, lbl(weekdays_short, L))}
                  || I <- lists:seq(0, 6)],
      weeks => [Week(W) || W <- lists:seq(RS, RE - 1, 7)]}.

more_label(N, L) ->
    iolist_to_binary(string:replace(lbl(more, L), <<"{n}">>, integer_to_binary(N), all)).

seg_view(#{inst := #{id := Id, src := Src, s := S, all_day := AllDay}, sc := Sc, ec := Ec,
           row := Row, multi := Multi, cont := Cont, conts := Conts}, #{} = O) ->
    #{cls => cls([<<"ah-calendar-event ah-calendar-daygrid-event">>,
                  {Multi, <<"ah-calendar-daygrid-event-multi">>},
                  {Cont, <<"ah-calendar-daygrid-event-start">>},
                  {Conts, <<"ah-calendar-daygrid-event-end">>}]),
      id => Id, sc => integer_to_binary(Sc), ec => integer_to_binary(Ec),
      row => integer_to_binary(Row + 2), color => src_color(Src),
      title => maps:get(<<"title">>, Src),
      has_time => not AllDay andalso not Multi,
      time => fmt_time(S, O)}.

src_color(Src) -> maps:get(<<"color">>, Src, ?DEFAULT_COLOR).

%% sigil's compute-week-segments: the bars of one week row, placed in the
%% first row where they fit; multi-day bars first, then longer, then
%% earlier (event order breaks ties).
week_segments(W, Insts) ->
    WS = W * ?DAY, WE = (W + 7) * ?DAY,
    Segs0 = [begin
                 Vs = S div ?DAY,
                 Ve = case AllDay of
                          true -> max(Vs + 1, (E + ?DAY - 1) div ?DAY);
                          false -> Vs + 1
                      end,
                 Cs = max(Vs, W), Ce = min(Ve, W + 7),
                 #{inst => I, sc => Cs - W + 1, ec => Ce - W + 1, span => Ce - Cs,
                   multi => Ve - Vs > 1, cont => Vs < W, conts => Ve > W + 7, n => N}
             end || {N, #{s := S, e := E, all_day := AllDay} = I}
                        <- lists:enumerate(in_range(Insts, WS, WE))],
    Segs1 = [Sg || #{span := Sp} = Sg <- Segs0, Sp > 0],
    Key = fun(#{multi := M, span := Sp, inst := #{s := S}, n := N}) ->
                  {case M of true -> 0; false -> 1 end, -Sp, S rem ?DAY, N}
          end,
    Sorted = lists:sort(fun(A, B) -> Key(A) =< Key(B) end, Segs1),
    {Placed, _} =
        lists:foldl(
          fun(#{sc := Sc, ec := Ec} = Sg, {Acc, Rows}) ->
                  Free = fun(Ranges) -> not lists:any(fun({Rs, Re}) -> Sc < Re andalso Ec > Rs end,
                                                      Ranges) end,
                  case first_index(Free, Rows) of
                      none -> {[Sg#{row => length(Rows)} | Acc], Rows ++ [[{Sc, Ec}]]};
                      R -> {[Sg#{row => R} | Acc],
                            setnth(R + 1, Rows, [{Sc, Ec} | lists:nth(R + 1, Rows)])}
                  end
          end, {[], []}, Sorted),
    lists:reverse(Placed).

first_index(F, L) -> first_index(F, L, 0).
first_index(_, [], _) -> none;
first_index(F, [H | T], I) ->
    case F(H) of true -> I; false -> first_index(F, T, I + 1) end.

setnth(1, [_ | T], X) -> [X | T];
setnth(N, [H | T], X) -> [H | setnth(N - 1, T, X)].

timegrid_view(RS, RE, Insts, #{labels := L, today := Today, slot_duration := Dur,
                               slot_height := SH, editable := Editable} = O) ->
    Total = round(?DAY * SH / Dur),
    #{all_day => lbl(all_day_short, L),
      slots => [#{slot_height => integer_to_binary(round(60 * SH / Dur)),
                  label => slot_label(H, O)} || H <- lists:seq(0, 23)],
      days => [begin
                   {_, _, Dd} = calendar:gregorian_days_to_date(D),
                   DayI = in_range(Insts, D * ?DAY, (D + 1) * ?DAY),
                   #{date => ?D:iso_date(D),
                     dow => lists:nth(?D:dow(D) + 1, lbl(weekdays_short, L)),
                     num => integer_to_binary(Dd),
                     head_cls => cls([<<"ah-calendar-timegrid-header-cell">>,
                                      {D =:= Today, <<"ah-calendar-timegrid-header-today">>}]),
                     col_cls => cls([<<"ah-calendar-timegrid-day-col">>,
                                     {D =:= Today, <<"ah-calendar-timegrid-day-today">>}]),
                     col_height => integer_to_binary(Total),
                     allday => [#{id => Id, color => src_color(Src),
                                  title => maps:get(<<"title">>, Src)}
                                || #{id := Id, src := Src, all_day := true} <- DayI],
                     timed => [#{id => Id, color => src_color(Src),
                                 title => maps:get(<<"title">>, Src),
                                 top => integer_to_binary(round(Ts * SH / Dur)),
                                 height => integer_to_binary(round((Te - Ts) * SH / Dur)),
                                 left => <<"calc(100% * ", (integer_to_binary(Col))/binary,
                                           " / ", (integer_to_binary(Cols))/binary, ")">>,
                                 width => <<"calc(100% / ", (integer_to_binary(Cols))/binary,
                                            ")">>,
                                 time => <<(fmt_time(S, O))/binary, " – "/utf8,
                                           (fmt_time(E, O))/binary>>,
                                 resizable => Editable}
                               || {#{id := Id, src := Src, s := S, e := E}, Ts, Te, Col, Cols}
                                      <- columns(D, [I || #{all_day := false} = I <- DayI])]}
               end || D <- lists:seq(RS, RE - 1)]}.

%% sigil's assign-columns: timed events of day D sorted by start, each in
%% the first column it does not overlap; all share the day's column count.
%% Returns {Inst, Top, Bottom, Col, Cols}, minutes of the day.
columns(D, Insts) ->
    Clip = [{I, max(S - D * ?DAY, 0), min(E - D * ?DAY, ?DAY), N}
            || {N, #{s := S, e := E} = I} <- lists:enumerate(Insts)],
    Fixed = [{I, Ts, case Te =< Ts of true -> Ts + 30; false -> Te end, N}
             || {I, Ts, Te, N} <- Clip],
    Sorted = lists:sort(fun({_, A, _, Na}, {_, B, _, Nb}) -> {A, Na} =< {B, Nb} end, Fixed),
    {Placed, Cols} =
        lists:foldl(
          fun({I, Ts, Te, _}, {Acc, Cs}) ->
                  Free = fun(C) -> not lists:any(fun({Os, Oe}) -> Ts < Oe andalso Te > Os end, C)
                         end,
                  case first_index(Free, Cs) of
                      none -> {[{I, Ts, Te, length(Cs)} | Acc], Cs ++ [[{Ts, Te}]]};
                      K -> {[{I, Ts, Te, K} | Acc],
                            setnth(K + 1, Cs, [{Ts, Te} | lists:nth(K + 1, Cs)])}
                  end
          end, {[], []}, Sorted),
    [{I, Ts, Te, K, length(Cols)} || {I, Ts, Te, K} <- lists:reverse(Placed)].

list_view(RS, RE, Insts, #{labels := L} = O) ->
    Groups = [begin
                  Evs = lists:sort(fun({A, _}, {B, _}) -> A =< B end,
                                   [{{S, N}, I} || {N, #{s := S} = I}
                                                       <- lists:enumerate(
                                                            in_range(Insts, D * ?DAY,
                                                                     (D + 1) * ?DAY))]),
                  {D, [I || {_, I} <- Evs]}
              end || D <- lists:seq(RS, RE - 1)],
    Status = fun(Src) -> maps:get(<<"status">>, Src, undefined) end,
    #{empty => lists:all(fun({_, Es}) -> Es =:= [] end, Groups),
      no_events => lbl(no_events, L), no_events_hint => lbl(no_events_hint, L),
      groups => [#{name => lists:nth(?D:dow(D) + 1, lbl(weekdays, L)),
                   date => fmt(D, lbl(list_date, L), L),
                   events => [#{id => Id, color => src_color(Src),
                                title => maps:get(<<"title">>, Src),
                                time => case AllDay of
                                            true -> lbl(all_day, L);
                                            false -> <<(fmt_time(S, O))/binary, " – "/utf8,
                                                       (fmt_time(E, O))/binary>>
                                        end,
                                recurring => maps:is_key(<<"rrule">>, Src),
                                has_status => Status(Src) =/= undefined,
                                status_color => status_color(Status(Src))}
                              || #{id := Id, src := Src, s := S, e := E,
                                   all_day := AllDay} <- Es]}
                 || {D, Es} <- Groups, Es =/= []]}.

status_color(<<"confirmed">>) -> <<"var(--ah-color-success)">>;
status_color(<<"tentative">>) -> <<"var(--ah-color-warning)">>;
status_color(<<"cancelled">>) -> <<"var(--ah-color-error)">>;
status_color(_) -> <<"var(--ah-color-grey-300)">>.

%% "h:mm a" (12) or "HH:mm" (24)
fmt_time(Min, #{hour_format := F, labels := L}) ->
    H = Min rem ?DAY div 60, M = ?D:pad(Min rem 60),
    case F of
        24 -> <<(?D:pad(H))/binary, ":", M/binary>>;
        12 -> <<(integer_to_binary(h12(H)))/binary, ":", M/binary, " ",
                (ampm(H, L))/binary>>
    end.

slot_label(H, #{hour_format := 24}) -> <<(?D:pad(H))/binary, ":00">>;
slot_label(H, #{labels := L}) -> <<(integer_to_binary(h12(H)))/binary, " ", (ampm(H, L))/binary>>.

h12(0) -> 12;
h12(H) when H > 12 -> H - 12;
h12(H) -> H.

ampm(H, L) when H < 12 -> lbl(am, L);
ampm(_, L) -> lbl(pm, L).

%% A class list from a base and {Cond, Class} pairs.
cls([Base | Opt]) ->
    iolist_to_binary([Base | [[<<" ">>, C] || {true, C} <- Opt]]).

%% Display formats: yyyy yy MMMM MMM MM M dd d EEEE EEE (the JS twin is fmtDate).
fmt(Days, Format, L) ->
    {Y, M, D} = calendar:gregorian_days_to_date(Days),
    iolist_to_binary(fmt_tokens(Format, #{y => Y, m => M, d => D, w => ?D:dow(Days)}, L)).

fmt_tokens(<<"yyyy", R/binary>>, V, L) -> [integer_to_binary(maps:get(y, V)) | fmt_tokens(R, V, L)];
fmt_tokens(<<"yy", R/binary>>, V, L) -> [?D:pad(maps:get(y, V) rem 100) | fmt_tokens(R, V, L)];
fmt_tokens(<<"MMMM", R/binary>>, V, L) ->
    [lists:nth(maps:get(m, V), lbl(months, L)) | fmt_tokens(R, V, L)];
fmt_tokens(<<"MMM", R/binary>>, V, L) ->
    [lists:nth(maps:get(m, V), lbl(months_short, L)) | fmt_tokens(R, V, L)];
fmt_tokens(<<"MM", R/binary>>, V, L) -> [?D:pad(maps:get(m, V)) | fmt_tokens(R, V, L)];
fmt_tokens(<<"M", R/binary>>, V, L) -> [integer_to_binary(maps:get(m, V)) | fmt_tokens(R, V, L)];
fmt_tokens(<<"dd", R/binary>>, V, L) -> [?D:pad(maps:get(d, V)) | fmt_tokens(R, V, L)];
fmt_tokens(<<"d", R/binary>>, V, L) -> [integer_to_binary(maps:get(d, V)) | fmt_tokens(R, V, L)];
fmt_tokens(<<"EEEE", R/binary>>, V, L) ->
    [lists:nth(maps:get(w, V) + 1, lbl(weekdays, L)) | fmt_tokens(R, V, L)];
fmt_tokens(<<"EEE", R/binary>>, V, L) ->
    [lists:nth(maps:get(w, V) + 1, lbl(weekdays_short, L)) | fmt_tokens(R, V, L)];
fmt_tokens(<<C/utf8, R/binary>>, V, L) -> [<<C/utf8>> | fmt_tokens(R, V, L)];
fmt_tokens(<<>>, _, _) -> [].

%%%-------------------------------------------------------------------
%%% Server-driven events
%%%-------------------------------------------------------------------

%% @doc Replace the events of a calendar from inside an action, typically
%% its `change' postback (whose `Event.data' holds the new visible range:
%% `view', `start', `end'). `Target' is that event (its `id' is the
%% calendar) or `{id, Id}'. Events take the same form as the `events'
%% option; the view re-renders without firing `change'.
-spec set_events(aihtml_action:ctx(), {id, iodata() | atom()} | aihtml_action:event(),
                 [event()]) -> ok.
set_events(Ctx, Target, Events) ->
    aihtml_action:call(Ctx, target(Target), setEvents, [normalize_events(Events)]).

%% @doc Add one event to a calendar from inside an action (for instance
%% after `ah:select'). `Target' as in `set_events/3'.
-spec add_event(aihtml_action:ctx(), {id, iodata() | atom()} | aihtml_action:event(),
                event()) -> ok.
add_event(Ctx, Target, Event) ->
    [E] = normalize_events([Event]),
    aihtml_action:call(Ctx, target(Target), addEvent, [E]).

target(#{id := Id}) -> {id, Id};
target({id, _} = T) -> T.

%% @doc Functions besides the component that the aihtml facade re-exports.
-spec facade_extras() -> [{atom(), arity()}].
facade_extras() -> [{set_events, 3}, {add_event, 3}].

%%%-------------------------------------------------------------------
%%% Internal
%%%-------------------------------------------------------------------

%% A root without an id gets one: the parts refer to each other by id.
%% Returns the id and the record holding it, for root_attrs/2.
ensure_id(R) ->
    Id = case R#ah_calendar.id of
             undefined -> <<"ah-cal", (integer_to_binary(erlang:unique_integer([positive])))/binary>>;
             Id0 -> text(Id0)
         end,
    {Id, R#ah_calendar{id = Id}}.

sub_id(Id, Part) -> <<Id/binary, "-", Part/binary>>.

hidden(undefined, _) -> [];
hidden(Name, Value) -> ?H:void(input, [], [{type, hidden}, {name, Name}, {value, Value}]).

text(undefined) -> <<>>;
text(B) when is_binary(B) -> B;
text(L) when is_list(L) -> unicode:characters_to_binary(L);
text(X) -> beamai_html_escape:to_binary(X, aihtml).

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => calendar, category => form,
       signature => <<"ah_calendar(Value, Css, Attrs)">>,
       root => <<"ah-calendar">>,
       flags => [editable, selectable],
       options => [events, view, views, first_day, agenda_days, day_max_events,
                   slot_duration, slot_height, height, hour_format, href, labels],
       behavior => <<"calendar">>,
       events => [<<"change">>, <<"ah:event-click">>, <<"ah:event-drop">>,
                  <<"ah:event-resize">>, <<"ah:select">>, <<"ah:more-click">>],
       doc => <<"An event calendar with month, week, day and agenda views, recurring and "
                "multi-day events; value in data-ah-value is the date shown, change fires "
                "on navigation.">>,
       option_docs =>
           #{editable => <<"Drag events to other days or times and resize timed events "
                           "(ah:event-drop, ah:event-resize).">>,
             selectable => <<"Drag over days (month) or times (week, day) to select a range "
                             "(ah:select).">>,
             events => <<"List of maps: id, title, start, end, all_day, color, rrule "
                         "(FREQ, INTERVAL, COUNT, UNTIL, BYDAY, BYMONTHDAY, BYMONTH), exdates, "
                         "status (confirmed, tentative, cancelled).">>,
             view => <<"The first view: month (default), week, day or list.">>,
             views => <<"The view buttons in the toolbar (default [month, week, day, list]).">>,
             first_day => <<"First day of the week, 0 = Sunday (default) .. 6.">>,
             agenda_days => <<"Days shown by the list view (default 30).">>,
             day_max_events => <<"Event rows per month cell before \"+n more\" (default 3).">>,
             slot_duration => <<"Minutes per time slot in week and day views (default 30).">>,
             slot_height => <<"Pixel height of a time slot (default 20).">>,
             height => <<"Height in px (default 600); undefined lets the content decide.">>,
             hour_format => <<"12 (default, 9:00 AM) or 24 (09:00).">>,
             href => <<"URL template with {date} (ISO) and {view}: prev, next, today and the "
                       "view buttons become links to that state, which the server renders when "
                       "a link is opened directly (crawlers, new tabs, bookmarks). A plain "
                       "click still navigates in the page and pushes the link's URL.">>,
             labels => <<"Map of texts and formats: today, prev, next, month, week, day, "
                         "list, all_day, all_day_short, more (\"+{n} more\"), no_events, "
                         "no_events_hint, am, pm, months, months_short, weekdays, "
                         "weekdays_short, title_month, title_day, range_start, range_end, "
                         "list_date.">>},
       methods =>
           [#{name => prev, args => <<"()">>, doc => <<"Go to the previous period (fires change).">>},
            #{name => next, args => <<"()">>, doc => <<"Go to the next period (fires change).">>},
            #{name => today, args => <<"()">>, doc => <<"Go to today (fires change).">>},
            #{name => changeView, args => <<"(\"month\" | \"week\" | \"day\" | \"list\")">>,
              doc => <<"Switch the view (fires change).">>},
            #{name => setValue, args => <<"(Iso)">>,
              doc => <<"Show the period holding a date, without firing change.">>},
            #{name => getValue, args => <<"()">>, doc => <<"Return data-ah-value.">>},
            #{name => setEvents, args => <<"([Event])">>,
              doc => <<"Replace all events (set_events/3 sends this).">>},
            #{name => addEvent, args => <<"(Event)">>,
              doc => <<"Add one event (add_event/3 sends this).">>},
            #{name => updateEvent, args => <<"(Id, Changes)">>,
              doc => <<"Merge changes (title, start, end, color, ...) into an event.">>},
            #{name => removeEvent, args => <<"(Id)">>, doc => <<"Remove an event.">>},
            #{name => getEvents, args => <<"()">>, doc => <<"Return the events.">>}]}].
