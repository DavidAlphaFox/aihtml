%%%-------------------------------------------------------------------
%%% @doc Recurrence rules shared by aihtml_calendar and aihtml_scheduler
%%% (internal): sigil's calendar/recurrence.cljs.
%%%
%%% An iCalendar RRULE subset: FREQ (daily, weekly, monthly, yearly),
%%% INTERVAL, COUNT, UNTIL, BYDAY, BYMONTHDAY, BYMONTH; other parts are
%%% ignored. Times are minutes since gregorian day 0 (aihtml_lib_date).
%%% The browser twin is AH.lib.rrule (assets/js/components/_lib_rrule.js).
%%%
%%% A malformed rule raises `error({aihtml, {bad_rrule, Part}})'.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_lib_rrule).

-export([parse/1, parse/2, expand/6, expand/7, stamp/1]).

-export_type([rule/0]).

-define(DAY, 1440).
-define(MAX_ITERS, 5000).

%% "FREQ=WEEKLY;BYDAY=MO,WE;COUNT=10" -> #{freq => weekly, byday => [1, 3], count => 10}
-type rule() :: #{freq => daily | weekly | monthly | yearly, interval => pos_integer(),
                  count => pos_integer(), until => aihtml_lib_date:minutes(),
                  byday => [0..6], bymonthday => [pos_integer()], bymonth => [pos_integer()]}.

%% @doc Parse a rule (with or without the "RRULE:" prefix). A rule
%% without FREQ is accepted and expands to nothing.
-spec parse(binary()) -> rule().
parse(R) -> parse(R, #{}).

%% @doc `parse/1' with options: `require_freq' (default false) makes a
%% rule without FREQ an error.
-spec parse(binary(), #{require_freq => boolean()}) -> rule().
parse(<<"RRULE:", R/binary>>, Opts) -> parse(R, Opts);
parse(R, Opts) ->
    Rule = lists:foldl(
             fun(<<>>, Acc) -> Acc;
                (Part, Acc) ->
                     case binary:split(Part, <<"=">>) of
                         [K, V] -> rrule_part(string:uppercase(K), V, Acc);
                         _ -> error({aihtml, {bad_rrule, R}})
                     end
             end, #{}, binary:split(R, <<";">>, [global])),
    (not maps:get(require_freq, Opts, false) orelse maps:is_key(freq, Rule))
        orelse error({aihtml, {bad_rrule, R}}),
    Rule.

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
    Day = aihtml_lib_date:days(<<Y/binary, "-", M/binary, "-", D/binary>>),
    case Rest of
        <<"T", H:2/binary, Mi:2/binary, _/binary>> ->
            Day * ?DAY + rrule_num(H, V) * 60 + rrule_num(Mi, V);
        _ -> Day * ?DAY
    end;
rrule_until(V) -> error({aihtml, {bad_rrule, V}}).

rrule_num(B, V) ->
    try binary_to_integer(B) catch _:_ -> error({aihtml, {bad_rrule, V}}) end.

%% @doc The occurrences `{Start, End}' of a series from `S' to `E' (the
%% first occurrence) overlapping `[RS, RE)', minutes, leaving out the days
%% in `Ex' (ISO dates). A rule without FREQ has none.
-spec expand(aihtml_lib_date:minutes(), aihtml_lib_date:minutes(), rule(),
             aihtml_lib_date:minutes(), aihtml_lib_date:minutes(), [binary()]) ->
          [{aihtml_lib_date:minutes(), aihtml_lib_date:minutes()}].
expand(S, E, Rule, RS, RE, Ex) -> expand(S, E, Rule, RS, RE, Ex, #{}).

%% @doc `expand/6' with options: `instants' (default false) also keeps
%% zero-length occurrences starting at `RS'.
-spec expand(aihtml_lib_date:minutes(), aihtml_lib_date:minutes(), rule(),
             aihtml_lib_date:minutes(), aihtml_lib_date:minutes(), [binary()],
             #{instants => boolean()}) ->
          [{aihtml_lib_date:minutes(), aihtml_lib_date:minutes()}].
expand(S, E, Rule, RS, RE, Ex, Opts) ->
    case maps:is_key(freq, Rule) of
        false -> [];
        true ->
            Ctx = #{s => S, dur => E - S, rs => RS, re => RE, ex => Ex,
                    interval => maps:get(interval, Rule, 1), rule => Rule,
                    until => maps:get(until, Rule, undefined),
                    max => maps:get(count, Rule, undefined),
                    instants => maps:get(instants, Opts, false)},
            case {maps:get(freq, Rule), maps:get(byday, Rule, [])} of
                {weekly, [_ | _] = ByDay} ->
                    Week = aihtml_lib_date:start_of_week(S div ?DAY, 1),
                    lists:reverse(weekly(Week, lists:sort(ByDay), Ctx, 0, 0, []));
                _ ->
                    lists:reverse(generic(S, Ctx, 0, 0, []))
            end
    end.

count_ok(_, #{max := undefined}) -> true;
count_ok(C, #{max := Max}) -> C < Max.

until_ok(_, #{until := undefined}) -> true;
until_ok(T, #{until := U}) -> T =< U.

occurrence(C, #{dur := Dur, rs := RS, re := RE, ex := Ex, instants := Instants}, Acc) ->
    CE = C + Dur,
    case not lists:member(aihtml_lib_date:iso_date(C div ?DAY), Ex) andalso C < RE
        andalso (CE > RS orelse (Instants andalso Dur =:= 0 andalso C >= RS)) of
        true -> [{C, CE} | Acc];
        false -> Acc
    end.

weekly(Week, ByDay, #{re := RE, interval := I} = Ctx, Iter, Count, Acc) ->
    case Iter < ?MAX_ITERS andalso Week * ?DAY < RE andalso until_ok(Week * ?DAY, Ctx)
        andalso count_ok(Count, Ctx) of
        false -> Acc;
        true ->
            Offset = maps:get(s, Ctx) rem ?DAY,
            Wd = aihtml_lib_date:dow(Week),
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
    Dow = aihtml_lib_date:dow(C div ?DAY),
    {_, M, D} = calendar:gregorian_days_to_date(C div ?DAY),
    lists:member(Dow, maps:get(byday, Rule, [Dow]))
        andalso lists:member(D, maps:get(bymonthday, Rule, [D]))
        andalso lists:member(M, maps:get(bymonth, Rule, [M])).

advance(C, daily, I) -> C + I * ?DAY;
advance(C, weekly, I) -> C + 7 * I * ?DAY;
advance(C, monthly, I) -> aihtml_lib_date:add_months(C div ?DAY, I) * ?DAY + C rem ?DAY;
advance(C, yearly, I) -> aihtml_lib_date:add_months(C div ?DAY, 12 * I) * ?DAY + C rem ?DAY.

%% @doc The occurrence suffix of a start time, yyyyMMdd'T'HHmmss: an
%% occurrence's id is <series id>_<stamp>.
-spec stamp(aihtml_lib_date:minutes()) -> binary().
stamp(Min) ->
    P = fun aihtml_lib_date:pad/1,
    {Y, M, D} = calendar:gregorian_days_to_date(Min div ?DAY),
    <<(aihtml_lib_date:pad4(Y))/binary, (P(M))/binary, (P(D))/binary, "T",
      (P(Min rem ?DAY div 60))/binary, (P(Min rem 60))/binary, "00">>.
