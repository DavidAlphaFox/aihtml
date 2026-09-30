%%%-------------------------------------------------------------------
%%% @doc Date helpers shared by the date components (internal).
%%%
%%% Days are gregorian day numbers (calendar:date_to_gregorian_days/1),
%%% times are minutes since gregorian day 0: local wall times without a
%%% zone, so there is no DST shifting. Used by aihtml_calendar,
%%% aihtml_datetime_input, aihtml_gantt, aihtml_scheduler (and
%%% aihtml_lib_rrule); aihtml_datepicker uses the week helpers. The
%%% browser twin is AH.lib.date (assets/js/components/_lib_date.ts).
%%%
%%% Invalid input raises `error({aihtml, {bad_date, Value}})'.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_lib_date).

-export([days/1, day_of/1, today/1, parse_time/1, iso_date/1, iso_time/2, dow/1,
         start_of_week/2, first_of_month/1, last_of_month/1, add_months/2, pad/1, pad4/1,
         first_day/1, time_12h/2]).

-export_type([days/0, minutes/0, date/0, time/0]).

-define(DAY, 1440).

%% A gregorian day number.
-type days() :: integer().
%% Minutes since gregorian day 0.
-type minutes() :: integer().
%% A day: an ISO date (<<"2026-09-29">> or "2026-09-29"), a
%% calendar:date() or undefined.
-type date() :: binary() | string() | calendar:date() | undefined.
%% A point in time, local and without a zone: an ISO date (a whole day),
%% an ISO date-time (<<"2026-09-29T14:30">>, seconds are dropped), a
%% calendar:date() or a calendar:datetime().
-type time() :: binary() | string() | calendar:date() | calendar:datetime().

%% @doc The day number of an ISO date or `calendar:date()', checked.
-spec days(term()) -> days().
days({Y, M, D} = Date) when is_integer(Y), is_integer(M), is_integer(D) ->
    calendar:valid_date(Date) orelse error({aihtml, {bad_date, Date}}),
    calendar:date_to_gregorian_days(Date);
days(<<Y:4/binary, "-", M:2/binary, "-", D:2/binary>> = B) ->
    try {binary_to_integer(Y), binary_to_integer(M), binary_to_integer(D)} of
        Date -> calendar:valid_date(Date) orelse error({aihtml, {bad_date, B}}),
                calendar:date_to_gregorian_days(Date)
    catch
        _:_ -> error({aihtml, {bad_date, B}})
    end;
days(L) when is_list(L) -> days(unicode:characters_to_binary(L));
days(Other) -> error({aihtml, {bad_date, Other}}).

%% @doc `days/1', also accepting an ISO date-time (its time is ignored).
-spec day_of(term()) -> days().
day_of(<<Date:10/binary, Sep, _/binary>>) when Sep =:= $T; Sep =:= $\s -> days(Date);
day_of(L) when is_list(L) -> day_of(unicode:characters_to_binary(L));
day_of(D) -> days(D).

%% @doc `{Day, Minute}' of "today": a given date (see `day_of/1') at noon,
%% or, for `undefined', the server's local clock.
-spec today(term()) -> {days(), minutes()}.
today(undefined) ->
    {Date, {H, M, _}} = calendar:local_time(),
    D = calendar:date_to_gregorian_days(Date),
    {D, D * ?DAY + H * 60 + M};
today(Date) ->
    D = day_of(Date),
    {D, D * ?DAY + 720}.

%% @doc An ISO date or date-time, `calendar:date()' or
%% `calendar:datetime()' as `{Minutes, DateOnly}'.
-spec parse_time(term()) -> {minutes(), boolean()}.
parse_time({{_, _, _} = D, {H, Mi, _}}) when is_integer(H), is_integer(Mi), H >= 0, H < 24,
                                             Mi >= 0, Mi < 60 ->
    {days(D) * ?DAY + H * 60 + Mi, false};
parse_time({Y, M, D} = Date) when is_integer(Y), is_integer(M), is_integer(D) ->
    {days(Date) * ?DAY, true};
parse_time(L) when is_list(L) -> parse_time(unicode:characters_to_binary(L));
parse_time(<<Date:10/binary>>) -> {days(Date) * ?DAY, true};
parse_time(<<Date:10/binary, Sep, H:2/binary, ":", Mi:2/binary, _/binary>> = B)
  when Sep =:= $T; Sep =:= $\s ->
    try {binary_to_integer(H), binary_to_integer(Mi)} of
        {Hi, Mii} when Hi >= 0, Hi < 24, Mii >= 0, Mii < 60 ->
            {days(Date) * ?DAY + Hi * 60 + Mii, false};
        _ -> error({aihtml, {bad_date, B}})
    catch
        _:_ -> error({aihtml, {bad_date, B}})
    end;
parse_time(Other) -> error({aihtml, {bad_date, Other}}).

%% @doc A day number as <<"yyyy-mm-dd">>.
-spec iso_date(days()) -> binary().
iso_date(Days) ->
    {Y, M, D} = calendar:gregorian_days_to_date(Days),
    <<(pad4(Y))/binary, "-", (pad(M))/binary, "-", (pad(D))/binary>>.

%% @doc Minutes as <<"yyyy-mm-ddTHH:MM">>, or as a date when `DateOnly'
%% and the time is midnight.
-spec iso_time(minutes(), boolean()) -> binary().
iso_time(Min, true) when Min rem ?DAY =:= 0 -> iso_date(Min div ?DAY);
iso_time(Min, _) ->
    <<(iso_date(Min div ?DAY))/binary, "T", (pad(Min rem ?DAY div 60))/binary, ":",
      (pad(Min rem 60))/binary>>.

%% @doc The weekday of a day number: 0 = Sunday .. 6 = Saturday, as JS getDay.
-spec dow(days()) -> 0..6.
dow(Days) -> calendar:day_of_the_week(calendar:gregorian_days_to_date(Days)) rem 7.

%% @doc The first day of the week holding `D', weeks starting on `First'
%% (0 = Sunday .. 6).
-spec start_of_week(days(), 0..6) -> days().
start_of_week(D, First) -> D - (dow(D) - First + 7) rem 7.

%% @doc The first day of the month holding `Days'.
-spec first_of_month(days()) -> days().
first_of_month(Days) ->
    {Y, M, _} = calendar:gregorian_days_to_date(Days),
    calendar:date_to_gregorian_days(Y, M, 1).

%% @doc The last day of the month holding `Days'.
-spec last_of_month(days()) -> days().
last_of_month(Days) ->
    {Y, M, _} = calendar:gregorian_days_to_date(Days),
    calendar:date_to_gregorian_days(Y, M, calendar:last_day_of_the_month(Y, M)).

%% @doc date-fns addMonths: the day clamped to the target month's length.
-spec add_months(days(), integer()) -> days().
add_months(Days, N) ->
    {Y, M, D} = calendar:gregorian_days_to_date(Days),
    T = Y * 12 + (M - 1) + N,
    Ty = T div 12, Tm = T rem 12 + 1,
    calendar:date_to_gregorian_days(Ty, Tm, min(D, calendar:last_day_of_the_month(Ty, Tm))).

%% @doc Two digits.
-spec pad(non_neg_integer()) -> binary().
pad(N) when N < 10 -> <<"0", (integer_to_binary(N))/binary>>;
pad(N) -> integer_to_binary(N).

%% @doc Four digits (years).
-spec pad4(integer()) -> binary().
pad4(N) -> iolist_to_binary(io_lib:format("~4..0B", [N])).

%% @doc A component's `first_day' option: the day given (0 = Sunday .. 6),
%% or, when it is undefined, the current language's (aihtml_i18n format
%% first_day: 0 in English, 1 in Chinese).
-spec first_day(term()) -> term().
first_day(undefined) -> aihtml_i18n:format(first_day);
first_day(Day) -> Day.

%% @doc A 12-hour time ("10:00", "10") with its AM / PM text, in the order
%% of the current language (aihtml_i18n format time_12h: "{time} {ampm}"
%% in English, "{ampm}{time}" in Chinese). The browser twin is time12 in
%% _lib_date.ts.
-spec time_12h(binary(), binary()) -> binary().
time_12h(Time, AmPm) ->
    P = binary:replace(aihtml_i18n:format(time_12h), <<"{time}">>, Time, [global]),
    binary:replace(P, <<"{ampm}">>, AmPm, [global]).
