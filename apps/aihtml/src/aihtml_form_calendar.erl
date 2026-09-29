%%%-------------------------------------------------------------------
%%% @doc Calendar components, ported from sigil (form/calendar and
%%% form/datetime_input). See designs/04-components.md.
%%%
%%%   calendar(Value, Css, Attrs)          an event calendar: month, week, day, agenda
%%%   datetime_input(Value, Css, Attrs)    a segmented date/time field
%%%   set_events(Ctx, Target, Events)      (in an action) replace a calendar's events
%%%   add_event(Ctx, Target, Event)        (in an action) add one event
%%%
%%% Both are value-bearing components: `Attrs' go to the root, which
%%% carries `data-ah-value' and fires `change'; `name' goes to a hidden
%%% input. The behaviours are `calendar' and `datetime_input'
%%% (assets/js/components/form_calendar.js).
%%%
%%% == Calendar ==
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
%%% Interactions fire component events on the root, after writing their
%%% details to data attributes (so that `Event.data' carries them):
%%%
%%%   ah:event-click    event (the event's id)
%%%   ah:event-drop     event, from, to (new start and end), days, allDay
%%%   ah:event-resize   event, from, to
%%%   ah:select         from, to (end exclusive), allDay
%%%   ah:more-click     date (then the day view opens, unless prevented)
%%%
%%% Each component function builds an element record (#ah_calendar{},
%%% #ah_datetime_input{}, defined in include/aihtml_form_calendar.hrl) and
%%% render/1 turns it into HTML, so pages may also write the records
%%% directly (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_form_calendar).
-behaviour(aihtml_element).

-include("aihtml_form_calendar.hrl").

-export([calendar/3, datetime_input/3, set_events/3, add_event/3,
         render/1, fields/1, catalog/0, facade_extras/0]).

-export_type([element/0, event/0]).

-define(H, aihtml_html).
-define(E, aihtml_element).

%% Shared templates (see aihtml_tpl): also compiled to AH.tpl.* for the browser.
-compile({parse_transform, beamai_mustache_transform}).
-mustache_template({tpl_calendar_month, "../templates/calendar_month.mustache"}).
-mustache_template({tpl_calendar_timegrid, "../templates/calendar_timegrid.mustache"}).
-mustache_template({tpl_calendar_list, "../templates/calendar_list.mustache"}).
-mustache_template({tpl_datetime_input_calendar, "../templates/datetime_input_calendar.mustache"}).

-type element() :: #ah_calendar{} | #ah_datetime_input{}.
-type event() :: ah_cal_event().

-define(VIEWS, [month, week, day, list]).
-define(DAY, 1440).
-define(MAX_ITERS, 5000).
-define(DEFAULT_COLOR, <<"var(--ah-color-primary)">>).

%%%===================================================================
%%% calendar
%%%===================================================================

%% @doc An event calendar (sigil's calendar). `Value' is the date the view
%% shows (an ISO date or `calendar:date()'; `undefined' is today).
%%
%% Css: `editable' (drag events to other days and times, resize timed
%% events), `selectable' (drag over days or times to select a range).
%% Options (in Attrs): `events' (a list of event maps, see
%% `ah_cal_event()'), `view' (month (default), week, day, list), `views'
%% (the view buttons shown, default all four), `first_day' (0 = Sunday ..
%% 6), `agenda_days' (days of the list view, default 30), `day_max_events'
%% (event rows per month cell, default 3), `slot_duration' (minutes,
%% default 30), `slot_height' (px, default 20), `height' (px, default
%% 600; `undefined' lets the content decide), `hour_format' (12 or 24),
%% `labels' (see `ah_cal_labels()').
-spec calendar(ah_cal_date(), aihtml_html:css(), aihtml_html:attrs()) -> #ah_calendar{}.
calendar(Value, Css, Attrs) ->
    build(#ah_calendar{value = Value}, Css, Attrs).

render_calendar(#ah_calendar{view = View, views = Views, first_day = First,
                             height = Height, name = Name} = R0) ->
    {Id, R} = ensure_id(R0),
    Classes = classes(R),                       % checks the flag fields first
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
              V -> days_of(V)
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
    Value = iso_date(Cur),
    Toolbar =
        ?H:el('div',
              [?H:el('div',
                     [?H:el(button, <<"‹"/utf8>>,
                            [<<"ah-calendar-btn ah-calendar-btn-prev">>],
                            [{type, button}, {aria_label, lbl(prev, L)}]),
                      ?H:el(button, <<"›"/utf8>>,
                            [<<"ah-calendar-btn ah-calendar-btn-next">>],
                            [{type, button}, {aria_label, lbl(next, L)}]),
                      ?H:el(button, lbl(today, L), [<<"ah-calendar-btn ah-calendar-btn-today">>],
                            [{type, button}])],
                     [<<"ah-calendar-toolbar-left">>], []),
               ?H:el('div', ?H:el(h2, Title, [<<"ah-calendar-title">>],
                                  [{id, sub_id(Id, <<"title">>)}, {aria_live, polite}]),
                     [<<"ah-calendar-toolbar-center">>], []),
               ?H:el('div',
                     [?H:el(button, lbl(V, L),
                            [<<"ah-calendar-view-btn">>,
                             [<<"ah-calendar-view-btn-active">> || V =:= View]],
                            [{type, button}, {data_view, V},
                             {aria_pressed, atom_to_binary(V =:= View)}])
                      || V <- Views],
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
            {data_ah_events, iolist_to_binary(json:encode(Events))},
            {data_ah_first_day, First},
            {data_ah_agenda_days, R#ah_calendar.agenda_days},
            {data_ah_day_max_events, R#ah_calendar.day_max_events},
            {data_ah_slot_duration, R#ah_calendar.slot_duration},
            {data_ah_slot_height, R#ah_calendar.slot_height},
            {data_ah_hour_format, R#ah_calendar.hour_format},
            {data_ah_labels, case R#ah_calendar.labels of
                                 M when map_size(M) =:= 0 -> undefined;
                                 _ -> iolist_to_binary(json:encode(L))
                             end},
            {data_view, View}, {data_start, iso_date(RS)}, {data_end, iso_date(RE)},
            {style, [[<<"height:">>, integer_to_binary(Height), <<"px">>]
                     || is_integer(Height)]}],
           ?E:root_attrs(R, change)]).

check_first_day(F) ->
    (is_integer(F) andalso F >= 0 andalso F =< 6) orelse error({aihtml, {bad_first_day, F}}).

check_pos(_, V) when is_integer(V), V > 0 -> ok;
check_pos(K, V) -> error({aihtml, {bad_option, K, V}}).

%%%-------------------------------------------------------------------
%%% Labels
%%%-------------------------------------------------------------------

cal_label_defaults() ->
    #{today => <<"Today">>, prev => <<"Previous">>, next => <<"Next">>,
      month => <<"Month">>, week => <<"Week">>, day => <<"Day">>, list => <<"Agenda">>,
      all_day => <<"All day">>, all_day_short => <<"all-day">>, more => <<"+{n} more">>,
      no_events => <<"No events in this period">>,
      no_events_hint => <<"Try navigating to a different date range">>,
      am => <<"AM">>, pm => <<"PM">>,
      months => [<<"January">>, <<"February">>, <<"March">>, <<"April">>, <<"May">>,
                 <<"June">>, <<"July">>, <<"August">>, <<"September">>, <<"October">>,
                 <<"November">>, <<"December">>],
      months_short => [<<"Jan">>, <<"Feb">>, <<"Mar">>, <<"Apr">>, <<"May">>, <<"Jun">>,
                       <<"Jul">>, <<"Aug">>, <<"Sep">>, <<"Oct">>, <<"Nov">>, <<"Dec">>],
      weekdays => [<<"Sunday">>, <<"Monday">>, <<"Tuesday">>, <<"Wednesday">>,
                   <<"Thursday">>, <<"Friday">>, <<"Saturday">>],
      weekdays_short => [<<"Sun">>, <<"Mon">>, <<"Tue">>, <<"Wed">>, <<"Thu">>,
                         <<"Fri">>, <<"Sat">>],
      title_month => <<"MMMM yyyy">>, title_day => <<"EEEE, MMMM d, yyyy">>,
      range_start => <<"MMM d">>, range_end => <<"MMM d, yyyy">>,
      list_date => <<"MMMM d, yyyy">>}.

cal_labels(Custom) ->
    labels(Custom, cal_label_defaults(), bad_calendar_label,
           [{months, 12}, {months_short, 12}, {weekdays, 7}, {weekdays_short, 7}]).

%% Defaults merged with the custom texts, checked; binary keys (the JSON
%% the browser gets, and what lbl/2 reads).
labels(Custom, Defaults, Err, Lens) ->
    is_map(Custom) orelse error({aihtml, {Err, Custom}}),
    maps:foreach(fun(K, _) -> maps:is_key(K, Defaults) orelse error({aihtml, {Err, K}}) end,
                 Custom),
    M = maps:merge(Defaults, Custom),
    [begin
         V = maps:get(K, M),
         (is_list(V) andalso length(V) =:= N andalso not is_integer(hd(V)))
             orelse error({aihtml, {Err, K}})
     end || {K, N} <- Lens],
    maps:fold(fun(K, V, Acc) ->
                      case lists:keymember(K, 1, Lens) of
                          true -> Acc#{atom_to_binary(K) => [text(X) || X <- V]};
                          false -> Acc#{atom_to_binary(K) => text(V)}
                      end
              end, #{}, M).

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
       {<<"start">>, iso_time(S, AllDay)},
       {<<"end">>, iso_time(End, AllDay)},
       {<<"allDay">>, AllDay}]
      ++ Opt(color, fun color/1)
      ++ Opt(rrule, fun(R) -> _ = parse_rrule(text(R)), text(R) end)
      ++ Opt(exdates, fun(Ds) -> [iso_date(days_of(D)) || D <- Ds] end)
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

%% ISO date or date-time -> {minutes since gregorian day 0, date only?}
parse_time({{_, _, _} = D, {H, Mi, _}}) when is_integer(H), is_integer(Mi) ->
    {days_of(D) * ?DAY + H * 60 + Mi, false};
parse_time({Y, M, D} = Date) when is_integer(Y), is_integer(M), is_integer(D) ->
    {days_of(Date) * ?DAY, true};
parse_time(L) when is_list(L) -> parse_time(unicode:characters_to_binary(L));
parse_time(<<Date:10/binary>>) -> {days_of(Date) * ?DAY, true};
parse_time(<<Date:10/binary, Sep, H:2/binary, ":", Mi:2/binary, _/binary>> = B)
  when Sep =:= $T; Sep =:= $\s ->
    try {binary_to_integer(H), binary_to_integer(Mi)} of
        {Hi, Mii} when Hi >= 0, Hi < 24, Mii >= 0, Mii < 60 ->
            {days_of(Date) * ?DAY + Hi * 60 + Mii, false};
        _ -> error({aihtml, {bad_date, B}})
    catch
        _:_ -> error({aihtml, {bad_date, B}})
    end;
parse_time(Other) -> error({aihtml, {bad_date, Other}}).

%% Gregorian days of an ISO date or calendar:date(), checked.
days_of({Y, M, D} = Date) when is_integer(Y), is_integer(M), is_integer(D) ->
    calendar:valid_date(Date) orelse error({aihtml, {bad_date, Date}}),
    calendar:date_to_gregorian_days(Date);
days_of(<<Y:4/binary, "-", M:2/binary, "-", D:2/binary>> = B) ->
    try {binary_to_integer(Y), binary_to_integer(M), binary_to_integer(D)} of
        Date -> calendar:valid_date(Date) orelse error({aihtml, {bad_date, B}}),
                calendar:date_to_gregorian_days(Date)
    catch
        _:_ -> error({aihtml, {bad_date, B}})
    end;
days_of(L) when is_list(L) -> days_of(unicode:characters_to_binary(L));
days_of(Other) -> error({aihtml, {bad_date, Other}}).

iso_date(Days) ->
    {Y, M, D} = calendar:gregorian_days_to_date(Days),
    <<(pad4(Y))/binary, "-", (pad(M))/binary, "-", (pad(D))/binary>>.

iso_time(Min, true) when Min rem ?DAY =:= 0 -> iso_date(Min div ?DAY);
iso_time(Min, _) ->
    <<(iso_date(Min div ?DAY))/binary, "T", (pad(Min rem ?DAY div 60))/binary, ":",
      (pad(Min rem 60))/binary>>.

%%%-------------------------------------------------------------------
%%% Recurrence (sigil's calendar/recurrence.cljs)
%%%-------------------------------------------------------------------

%% "FREQ=WEEKLY;BYDAY=MO,WE;COUNT=10" -> #{freq => weekly, byday => [1, 3], count => 10}
parse_rrule(<<"RRULE:", R/binary>>) -> parse_rrule(R);
parse_rrule(R) ->
    lists:foldl(
      fun(<<>>, Acc) -> Acc;
         (Part, Acc) ->
              case binary:split(Part, <<"=">>) of
                  [K, V] -> rrule_part(string:uppercase(K), V, Acc);
                  _ -> error({aihtml, {bad_rrule, R}})
              end
      end, #{}, binary:split(R, <<";">>, [global])).

rrule_part(<<"FREQ">>, V, Acc) ->
    case string:lowercase(V) of
        F when F =:= <<"daily">>; F =:= <<"weekly">>; F =:= <<"monthly">>; F =:= <<"yearly">> ->
            Acc#{freq => binary_to_atom(F)};
        _ -> error({aihtml, {bad_rrule, V}})
    end;
rrule_part(<<"INTERVAL">>, V, Acc) -> Acc#{interval => rrule_int(V)};
rrule_part(<<"COUNT">>, V, Acc) -> Acc#{count => rrule_int(V)};
rrule_part(<<"UNTIL">>, V, Acc) -> Acc#{until => rrule_until(V)};
rrule_part(<<"BYDAY">>, V, Acc) ->
    Days = [<<"SU">>, <<"MO">>, <<"TU">>, <<"WE">>, <<"TH">>, <<"FR">>, <<"SA">>],
    Index = maps:from_list(lists:zip(Days, lists:seq(0, 6))),
    Acc#{byday => [case maps:find(string:uppercase(D), Index) of
                       error -> error({aihtml, {bad_rrule, V}});
                       {ok, I} -> I
                   end || D <- binary:split(V, <<",">>, [global])]};
rrule_part(<<"BYMONTHDAY">>, V, Acc) ->
    Acc#{bymonthday => [rrule_int(X) || X <- binary:split(V, <<",">>, [global])]};
rrule_part(<<"BYMONTH">>, V, Acc) ->
    Acc#{bymonth => [rrule_int(X) || X <- binary:split(V, <<",">>, [global])]};
rrule_part(_, _, Acc) -> Acc.

rrule_int(V) ->
    try binary_to_integer(V) of
        N when N > 0 -> N;
        _ -> error({aihtml, {bad_rrule, V}})
    catch _:_ -> error({aihtml, {bad_rrule, V}})
    end.

%% UNTIL: YYYYMMDD or YYYYMMDDTHHMMSS[Z], as local time
rrule_until(<<Y:4/binary, M:2/binary, D:2/binary, Rest/binary>> = V) ->
    Day = days_of(<<Y/binary, "-", M/binary, "-", D/binary>>),
    case Rest of
        <<"T", H:2/binary, Mi:2/binary, _/binary>> ->
            Day * ?DAY + rrule_num(H, V) * 60 + rrule_num(Mi, V);
        _ -> Day * ?DAY
    end;
rrule_until(V) -> error({aihtml, {bad_rrule, V}}).

rrule_num(B, V) ->
    try binary_to_integer(B) catch _:_ -> error({aihtml, {bad_rrule, V}}) end.

%% The occurrences {Start, End} of a series overlapping [RS, RE), minutes.
expand(S, E, Rule, RS, RE, Ex) ->
    case maps:is_key(freq, Rule) of
        false -> [];
        true ->
            Ctx = #{s => S, dur => E - S, rs => RS, re => RE, ex => Ex,
                    interval => maps:get(interval, Rule, 1), rule => Rule,
                    until => maps:get(until, Rule, undefined),
                    max => maps:get(count, Rule, undefined)},
            case {maps:get(freq, Rule), maps:get(byday, Rule, [])} of
                {weekly, [_ | _] = ByDay} ->
                    Week = start_of_week(S div ?DAY, 1),
                    lists:reverse(weekly(Week, lists:sort(ByDay), Ctx, 0, 0, []));
                _ ->
                    lists:reverse(generic(S, Ctx, 0, 0, []))
            end
    end.

count_ok(_, #{max := undefined}) -> true;
count_ok(C, #{max := Max}) -> C < Max.

until_ok(_, #{until := undefined}) -> true;
until_ok(T, #{until := U}) -> T =< U.

occurrence(C, #{dur := Dur, rs := RS, re := RE, ex := Ex}, Acc) ->
    CE = C + Dur,
    case not lists:member(iso_date(C div ?DAY), Ex) andalso C < RE andalso CE > RS of
        true -> [{C, CE} | Acc];
        false -> Acc
    end.

weekly(Week, ByDay, #{re := RE, interval := I} = Ctx, Iter, Count, Acc) ->
    case Iter < ?MAX_ITERS andalso Week * ?DAY < RE andalso until_ok(Week * ?DAY, Ctx)
        andalso count_ok(Count, Ctx) of
        false -> Acc;
        true ->
            Offset = maps:get(s, Ctx) rem ?DAY,
            Wd = dow(Week),
            Cands = lists:sort([(Week + (D - Wd + 7) rem 7) * ?DAY + Offset || D <- ByDay]),
            {Iter1, Count1, Acc1} =
                lists:foldl(
                  fun(C, {It, Co, A}) ->
                          case count_ok(Co, Ctx) andalso It < ?MAX_ITERS
                              andalso C >= maps:get(s, Ctx) andalso until_ok(C, Ctx)
                              andalso C < RE of
                              true -> {It + 1, Co + 1, occurrence(C, Ctx, A)};
                              false -> {It, Co, A}
                          end
                  end, {Iter, Count, Acc}, Cands),
            weekly(Week + 7 * I, ByDay, Ctx, Iter1, Count1, Acc1)
    end.

generic(C, #{re := RE, rule := Rule, interval := I} = Ctx, Iter, Count, Acc) ->
    case Iter < ?MAX_ITERS andalso C < RE andalso until_ok(C, Ctx) andalso count_ok(Count, Ctx) of
        false -> Acc;
        true ->
            {Count1, Acc1} = case matches(C, Rule) of
                                 true -> {Count + 1, occurrence(C, Ctx, Acc)};
                                 false -> {Count, Acc}
                             end,
            generic(advance(C, maps:get(freq, Rule), I), Ctx, Iter + 1, Count1, Acc1)
    end.

matches(C, Rule) ->
    {_, M, D} = calendar:gregorian_days_to_date(C div ?DAY),
    lists:member(dow(C div ?DAY), maps:get(byday, Rule, [dow(C div ?DAY)]))
        andalso lists:member(D, maps:get(bymonthday, Rule, [D]))
        andalso lists:member(M, maps:get(bymonth, Rule, [M])).

advance(C, daily, I) -> C + I * ?DAY;
advance(C, weekly, I) -> C + 7 * I * ?DAY;
advance(C, monthly, I) -> add_months(C div ?DAY, I) * ?DAY + C rem ?DAY;
advance(C, yearly, I) -> add_months(C div ?DAY, 12 * I) * ?DAY + C rem ?DAY.

%% date-fns addMonths: the day clamped to the target month's length
add_months(Days, N) ->
    {Y, M, D} = calendar:gregorian_days_to_date(Days),
    T = Y * 12 + (M - 1) + N,
    Ty = T div 12, Tm = T rem 12 + 1,
    calendar:date_to_gregorian_days(Ty, Tm, min(D, calendar:last_day_of_the_month(Ty, Tm))).

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
                   [#{id => <<Id/binary, "_", (stamp(Cs))/binary>>, src => Ev,
                      s => Cs, e => Ce, all_day => AllDay}
                    || {Cs, Ce} <- expand(S, E, parse_rrule(Rule), RS, RE, Ex)]
           end
       end || Ev <- Events]).

%% yyyyMMdd'T'HHmmss
stamp(Min) ->
    {Y, M, D} = calendar:gregorian_days_to_date(Min div ?DAY),
    <<(pad4(Y))/binary, (pad(M))/binary, (pad(D))/binary, "T",
      (pad(Min rem ?DAY div 60))/binary, (pad(Min rem 60))/binary, "00">>.

in_range(Insts, From, To) ->
    [I || #{s := S, e := E} = I <- Insts, S < To, E > From].

%%%-------------------------------------------------------------------
%%% Views (the Erlang twin of calView in form_calendar.js)
%%%-------------------------------------------------------------------

%% The visible range [RS, RE) in days and the title of a view.
profile(month, Cur, #{first_day := F, labels := L}) ->
    {Y, M, _} = calendar:gregorian_days_to_date(Cur),
    MS = calendar:date_to_gregorian_days(Y, M, 1),
    ME = calendar:date_to_gregorian_days(Y, M, calendar:last_day_of_the_month(Y, M)),
    {start_of_week(MS, F), start_of_week(ME, F) + 7, fmt(Cur, lbl(title_month, L), L)};
profile(week, Cur, #{first_day := F, labels := L}) ->
    WS = start_of_week(Cur, F),
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
                                    date => iso_date(D), col => integer_to_binary(I + 1),
                                    num => integer_to_binary(Dd)}
                              end || I <- lists:seq(0, 6)],
                     events => [seg_view(S, O) || #{row := Row} = S <- Segs, Row < Max],
                     more => [#{date => iso_date(W + C - 1), col => integer_to_binary(C),
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
                   #{date => iso_date(D),
                     dow => lists:nth(dow(D) + 1, lbl(weekdays_short, L)),
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
      groups => [#{name => lists:nth(dow(D) + 1, lbl(weekdays, L)),
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
    H = Min rem ?DAY div 60, M = pad(Min rem 60),
    case F of
        24 -> <<(pad(H))/binary, ":", M/binary>>;
        12 -> <<(integer_to_binary(h12(H)))/binary, ":", M/binary, " ",
                (ampm(H, L))/binary>>
    end.

slot_label(H, #{hour_format := 24}) -> <<(pad(H))/binary, ":00">>;
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
    iolist_to_binary(fmt_tokens(Format, #{y => Y, m => M, d => D, w => dow(Days)}, L)).

fmt_tokens(<<"yyyy", R/binary>>, V, L) -> [integer_to_binary(maps:get(y, V)) | fmt_tokens(R, V, L)];
fmt_tokens(<<"yy", R/binary>>, V, L) -> [pad(maps:get(y, V) rem 100) | fmt_tokens(R, V, L)];
fmt_tokens(<<"MMMM", R/binary>>, V, L) ->
    [lists:nth(maps:get(m, V), lbl(months, L)) | fmt_tokens(R, V, L)];
fmt_tokens(<<"MMM", R/binary>>, V, L) ->
    [lists:nth(maps:get(m, V), lbl(months_short, L)) | fmt_tokens(R, V, L)];
fmt_tokens(<<"MM", R/binary>>, V, L) -> [pad(maps:get(m, V)) | fmt_tokens(R, V, L)];
fmt_tokens(<<"M", R/binary>>, V, L) -> [integer_to_binary(maps:get(m, V)) | fmt_tokens(R, V, L)];
fmt_tokens(<<"dd", R/binary>>, V, L) -> [pad(maps:get(d, V)) | fmt_tokens(R, V, L)];
fmt_tokens(<<"d", R/binary>>, V, L) -> [integer_to_binary(maps:get(d, V)) | fmt_tokens(R, V, L)];
fmt_tokens(<<"EEEE", R/binary>>, V, L) ->
    [lists:nth(maps:get(w, V) + 1, lbl(weekdays, L)) | fmt_tokens(R, V, L)];
fmt_tokens(<<"EEE", R/binary>>, V, L) ->
    [lists:nth(maps:get(w, V) + 1, lbl(weekdays_short, L)) | fmt_tokens(R, V, L)];
fmt_tokens(<<C/utf8, R/binary>>, V, L) -> [<<C/utf8>> | fmt_tokens(R, V, L)];
fmt_tokens(<<>>, _, _) -> [].

%% 0 = Sunday .. 6 = Saturday, as JS getDay
dow(Days) -> calendar:day_of_the_week(calendar:gregorian_days_to_date(Days)) rem 7.

start_of_week(D, First) -> D - (dow(D) - First + 7) rem 7.

pad(N) when N < 10 -> <<"0", (integer_to_binary(N))/binary>>;
pad(N) -> integer_to_binary(N).

pad4(N) -> iolist_to_binary(io_lib:format("~4..0B", [N])).

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

%% @doc Functions besides the components that the aihtml facade re-exports.
-spec facade_extras() -> [{atom(), arity()}].
facade_extras() -> [{set_events, 3}, {add_event, 3}].

%%%===================================================================
%%% datetime_input
%%%===================================================================

%% @doc A segmented date/time field (sigil's datetime_input): the text is
%% split into the parts of `format', edited one at a time with digits,
%% the arrow keys, PageUp/PageDown (±10), Home/End and Backspace. `Value'
%% is an ISO date, date-time or time, a `calendar:date()' or
%% `calendar:datetime()'; `data-ah-value' has the shape the format implies
%% (yyyy-MM-dd, yyyy-MM-ddTHH:mm[:ss] or HH:mm[:ss]).
%%
%% Css: `disabled', `readonly', `spinner' (up/down buttons), `no_calendar'
%% (no drop-down calendar), `show_time' (hour and minute fields in the
%% drop-down), `floating_label' (the placeholder is a label that floats
%% above the value), `no_rounded' (square corners).
%% Options (in Attrs): `placeholder', `format' (tokens yyyy yy MM M dd d
%% HH H hh h mm m ss s a; single letters show two digits too; default
%% "yyyy-MM-dd"), `min', `max', `first_day' (0 = Sunday .. 6), `labels'
%% (a map with `months', `weekdays' (7, from Sunday), `title' (a format),
%% `time', `prev_month', `next_month').
-spec datetime_input(ah_dti_value(), aihtml_html:css(), aihtml_html:attrs()) ->
          #ah_datetime_input{}.
datetime_input(Value, Css, Attrs) ->
    build(#ah_datetime_input{value = Value}, Css, Attrs).

render_datetime_input(#ah_datetime_input{value = Value0, format = Format0, name = Name,
                                         disabled = Disabled, placeholder = Placeholder,
                                         first_day = First} = R0) ->
    {Id, R} = ensure_id(R0),
    Classes = classes(R),                       % checks the flag fields first
    check_first_day(First),
    Format = text(Format0),
    Segs = segments(Format),
    [] =/= [T || {tok, T, _} <- Segs] orelse error({aihtml, {bad_datetime_format, Format0}}),
    Kind = kind(Segs),
    Labels = dti_labels(R#ah_datetime_input.labels),
    Value = dti_value(Value0),
    Iso = dti_iso(Value, Kind),
    Display = case Value of
                  undefined -> <<>>;
                  _ -> iolist_to_binary([seg_text(S, Value) || S <- Segs])
              end,
    Float = R#ah_datetime_input.floating_label,
    %% a time alone has no calendar
    Cal = not R#ah_datetime_input.no_calendar andalso Kind =/= {time, true}
        andalso Kind =/= {time, false},
    InputId = sub_id(Id, <<"input">>),
    Svg = fun(D) -> {safe, [<<"<svg width=\"9\" height=\"9\" viewBox=\"0 0 24 24\" fill=\"none\" "
                              "stroke=\"currentColor\" stroke-width=\"3\" stroke-linecap=\"round\" "
                              "stroke-linejoin=\"round\" aria-hidden=\"true\"><path d=\"">>, D,
                            <<"\"/></svg>">>]}
          end,
    Input = ?H:void(input, [<<"ah-dti-input">>],
                    [{type, text}, {id, InputId}, {readonly, true},
                     {placeholder, case Float of true -> undefined; false -> Placeholder end},
                     {autocomplete, off}, {spellcheck, <<"false">>}, {value, Display},
                     {disabled, Disabled},
                     {aria_label, case Float of true -> undefined; false -> nonempty(Placeholder) end},
                     {aria_haspopup, Cal andalso dialog},
                     {aria_expanded, Cal andalso <<"false">>},
                     {aria_controls, Cal andalso sub_id(Id, <<"dropdown">>)},
                     {aria_description, <<"Arrow keys change the selected part">>}]),
    ?H:el('div',
          [?H:el('div',
                 [Input,
                  [?H:el('div', <<"📅"/utf8>>, [<<"ah-dti-cal-btn">>],
                         [{data_action, <<"toggle-dropdown">>}, {aria_hidden, <<"true">>}])
                   || Cal],
                  [?H:el('div',
                         [?H:el(button, Svg(<<"m6 15 6-6 6 6">>), [<<"ah-dti-spin ah-dti-spin-up">>],
                                [{type, button}, {tabindex, <<"-1">>}, {aria_label, <<"Increment">>}]),
                          ?H:el(button, Svg(<<"m6 9 6 6 6-6">>),
                                [<<"ah-dti-spin ah-dti-spin-down">>],
                                [{type, button}, {tabindex, <<"-1">>}, {aria_label, <<"Decrement">>}])],
                         [<<"ah-dti-spinner">>], [{aria_hidden, <<"true">>}])
                   || R#ah_datetime_input.spinner]],
                 [<<"ah-dti-row">>], []),
           ?H:el(span, [], [<<"ah-dti-live">>], [{aria_live, polite}, {aria_atomic, <<"true">>}]),
           [?H:el(label, Placeholder,
                  [<<"ah-dti-label">>, [<<"ah-dti-label-float">> || Value =/= undefined]],
                  [{for, InputId}])
            || Float],
           hidden(Name, Iso),
           [?H:el('div', [], [<<"ah-dti-dropdown">>],
                  [{id, sub_id(Id, <<"dropdown">>)}, {role, dialog},
                   {aria_label, <<"Choose date">>}, {hidden, true}])
            || Cal]],
          Classes,
          [[{id, Id}, {data_ah, <<"datetime_input">>}, {data_ah_value, Iso},
            {data_ah_format, Format},
            {data_ah_min, opt_iso(R#ah_datetime_input.min, Kind)},
            {data_ah_max, opt_iso(R#ah_datetime_input.max, Kind)},
            {data_ah_first_day, First},
            {data_ah_show_time, R#ah_datetime_input.show_time},
            {data_ah_labels, case R#ah_datetime_input.labels of
                                 M when map_size(M) =:= 0 -> undefined;
                                 _ -> iolist_to_binary(json:encode(Labels))
                             end},
            {aria_disabled, Disabled andalso <<"true">>}],
           ?E:root_attrs(R, change)]).

nonempty(undefined) -> undefined;
nonempty(T) -> case text(T) of <<>> -> undefined; B -> B end.

dti_labels(Custom) ->
    Defaults = #{months => maps:get(months, cal_label_defaults()),
                 weekdays => [<<"Su">>, <<"Mo">>, <<"Tu">>, <<"We">>, <<"Th">>, <<"Fr">>,
                              <<"Sa">>],
                 title => <<"MMMM yyyy">>, time => <<"Time">>,
                 prev_month => <<"Previous month">>, next_month => <<"Next month">>},
    labels(Custom, Defaults, bad_datetime_label, [{months, 12}, {weekdays, 7}]).

%% The parts of a format: {tok, Type, Pattern} or {lit, Text}. Single
%% letter tokens are two digits wide, like their doubled forms, so every
%% part keeps its place in the text.
segments(F) -> segments(F, []).

segments(<<>>, Acc) -> lists:reverse(Acc);
segments(B, Acc) ->
    Toks = [{<<"yyyy">>, year}, {<<"yy">>, year2}, {<<"MM">>, month}, {<<"M">>, month},
            {<<"dd">>, day}, {<<"d">>, day}, {<<"HH">>, hour}, {<<"H">>, hour},
            {<<"hh">>, hour12}, {<<"h">>, hour12}, {<<"mm">>, minute}, {<<"m">>, minute},
            {<<"ss">>, second}, {<<"s">>, second}, {<<"aa">>, ampm}, {<<"a">>, ampm}],
    case [{P, T} || {P, T} <- Toks, binary:longest_common_prefix([P, B]) =:= byte_size(P)] of
        [{P, T} | _] ->
            segments(binary:part(B, byte_size(P), byte_size(B) - byte_size(P)),
                     [{tok, T, P} | Acc]);
        [] ->
            <<C/utf8, Rest/binary>> = B,
            case Acc of
                [{lit, L} | Acc1] -> segments(Rest, [{lit, <<L/binary, C/utf8>>} | Acc1]);
                _ -> segments(Rest, [{lit, <<C/utf8>>} | Acc])
            end
    end.

kind(Segs) ->
    Types = [T || {tok, T, _} <- Segs],
    Date = lists:any(fun(T) -> lists:member(T, [year, year2, month, day]) end, Types),
    Time = lists:any(fun(T) -> lists:member(T, [hour, hour12, minute, second, ampm]) end, Types),
    Sec = lists:member(second, Types),
    case {Date, Time} of
        {true, false} -> date;
        {false, true} -> {time, Sec};
        _ -> {datetime, Sec}
    end.

seg_text({lit, L}, _) -> L;
seg_text({tok, T, _}, {{Y, Mo, D}, {H, Mi, S}}) ->
    case T of
        year -> pad4(Y);
        year2 -> pad(Y rem 100);
        month -> pad(Mo);
        day -> pad(D);
        hour -> pad(H);
        hour12 -> pad(h12(H));
        minute -> pad(Mi);
        second -> pad(S);
        ampm when H < 12 -> <<"AM">>;
        ampm -> <<"PM">>
    end.

%% A value as {{Y, M, D}, {H, Mi, S}}; a time alone takes today's date.
dti_value(undefined) -> undefined;
dti_value(<<>>) -> undefined;
dti_value({{_, _, _} = D, {H, Mi, S}} = V) when is_integer(H), is_integer(Mi), is_integer(S) ->
    _ = days_of(D),
    (H >= 0 andalso H < 24 andalso Mi >= 0 andalso Mi < 60 andalso S >= 0 andalso S < 60)
        orelse error({aihtml, {bad_date, V}}),
    V;
dti_value({Y, M, D} = Date) when is_integer(Y), is_integer(M), is_integer(D) ->
    _ = days_of(Date),
    {Date, {0, 0, 0}};
dti_value(L) when is_list(L) -> dti_value(unicode:characters_to_binary(L));
dti_value(<<Date:10/binary>>) -> dti_value(calendar:gregorian_days_to_date(days_of(Date)));
dti_value(<<Date:10/binary, Sep, Time/binary>> = B) when Sep =:= $T; Sep =:= $\s ->
    {{H, Mi, S}, _} = hms(Time, B),
    dti_value({calendar:gregorian_days_to_date(days_of(Date)), {H, Mi, S}});
dti_value(<<_:2/binary, ":", _/binary>> = B) ->
    {T, _} = hms(B, B),
    dti_value({date(), T});
dti_value(Other) -> error({aihtml, {bad_date, Other}}).

hms(<<H:2/binary, ":", Mi:2/binary, Rest/binary>>, B) ->
    S = case Rest of
            <<":", S0:2/binary, _/binary>> -> S0;
            _ -> <<"00">>
        end,
    try {{binary_to_integer(H), binary_to_integer(Mi), binary_to_integer(S)}, ok}
    catch _:_ -> error({aihtml, {bad_date, B}})
    end;
hms(_, B) -> error({aihtml, {bad_date, B}}).

dti_iso(undefined, _) -> <<>>;
dti_iso({{Y, M, D}, _}, date) ->
    <<(pad4(Y))/binary, "-", (pad(M))/binary, "-", (pad(D))/binary>>;
dti_iso({_, {H, Mi, S}}, {time, Sec}) ->
    <<(pad(H))/binary, ":", (pad(Mi))/binary, (secs(S, Sec))/binary>>;
dti_iso({Date, _} = V, {datetime, Sec}) ->
    <<(dti_iso({Date, {0, 0, 0}}, date))/binary, "T", (dti_iso(V, {time, Sec}))/binary>>.

secs(S, true) -> <<":", (pad(S))/binary>>;
secs(_, false) -> <<>>.

opt_iso(undefined, _) -> undefined;
opt_iso(V, Kind) -> dti_iso(dti_value(V), Kind).

%%%===================================================================
%%% Records
%%%===================================================================

build(R, Css, Attrs) ->
    Tag = element(1, R),
    ?E:build(R, fields(Tag), entry(?E:component_name(Tag)), Css, Attrs).

%% @doc The field names of one of this group's records.
-spec fields(atom()) -> [atom()].
fields(ah_calendar) -> record_info(fields, ah_calendar);
fields(ah_datetime_input) -> record_info(fields, ah_datetime_input).

-spec render(element()) -> aihtml_html:html().
render(#ah_calendar{} = R) -> render_calendar(R);
render(#ah_datetime_input{} = R) -> render_datetime_input(R).

classes(R) ->
    Tag = element(1, R),
    ?E:classes(R, fields(Tag), entry(?E:component_name(Tag))).

entry(Name) -> aihtml_catalog:entry(?MODULE, Name).

%% A root without an id gets one: the parts refer to each other by id.
%% Returns the id and the record holding it, for root_attrs/2.
ensure_id(R) ->
    Id = case element(3, R) of
             undefined -> <<"ah-cal", (integer_to_binary(erlang:unique_integer([positive])))/binary>>;
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
    [#{name => calendar, category => form,
       signature => <<"calendar(Value, Css, Attrs)">>,
       root => <<"ah-calendar">>,
       flags => [editable, selectable],
       options => [events, view, views, first_day, agenda_days, day_max_events,
                   slot_duration, slot_height, height, hour_format, labels],
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
            #{name => getEvents, args => <<"()">>, doc => <<"Return the events.">>}]},
     #{name => datetime_input, category => form,
       signature => <<"datetime_input(Value, Css, Attrs)">>,
       root => <<"ah-dti-group">>,
       flags => [disabled, readonly, spinner, no_calendar, show_time, floating_label,
                 no_rounded],
       classes => #{disabled => [<<"ah-dti-disabled">>],
                    readonly => [<<"ah-dti-readonly">>],
                    spinner => [], no_calendar => [], show_time => [],
                    floating_label => [],
                    no_rounded => [<<"ah-dti-no-rounded">>]},
       options => [placeholder, format, min, max, first_day, labels],
       behavior => <<"datetime_input">>,
       events => [<<"change">>, <<"input">>, <<"ah:open">>, <<"ah:close">>],
       doc => <<"A segmented date/time field edited part by part with digits and arrow "
                "keys, with a drop-down calendar; value in data-ah-value as ISO.">>,
       option_docs =>
           #{disabled => <<"Not editable, not focusable.">>,
             readonly => <<"Shows the value; keys and the calendar do nothing.">>,
             spinner => <<"Up/down buttons that step the selected part.">>,
             no_calendar => <<"No calendar button and drop-down.">>,
             show_time => <<"Hour and minute fields under the drop-down calendar.">>,
             floating_label => <<"The placeholder becomes a label floating above the value.">>,
             no_rounded => <<"Square corners.">>,
             placeholder => <<"Text of the empty field.">>,
             format => <<"Parts: yyyy yy MM M dd d HH H hh h mm m ss s a (default "
                         "yyyy-MM-dd); the value is yyyy-MM-dd, yyyy-MM-ddTHH:mm[:ss] or "
                         "HH:mm[:ss] accordingly.">>,
             min => <<"Earliest value; later edits are clamped on blur.">>,
             max => <<"Latest value.">>,
             first_day => <<"First day of the calendar week, 0 = Sunday (default) .. 6.">>,
             labels => <<"Map of months, weekdays (from Sunday), title (a format), time, "
                         "prev_month, next_month.">>},
       methods =>
           [#{name => setValue, args => <<"(Iso | null)">>,
              doc => <<"Set the value without firing change.">>},
            #{name => getValue, args => <<"()">>, doc => <<"Return data-ah-value.">>},
            #{name => clear, args => <<"()">>, doc => <<"Empty the value and fire change.">>},
            #{name => open, args => <<"()">>, doc => <<"Open the calendar.">>},
            #{name => close, args => <<"()">>, doc => <<"Close the calendar.">>}]}].
