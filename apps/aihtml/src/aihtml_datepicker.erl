%%%-------------------------------------------------------------------
%%% @doc A date field with a month grid popup, ported from sigil
%%% (form/datepicker). See designs/04-components.md.
%%%
%%%   ah_datepicker(Value, Css, Attrs)     a text field with a month grid popup
%%%
%%% A value-bearing component: `Attrs' go to the root, which carries
%%% `data-ah-value' and fires `change'; `name' goes to a hidden input. The
%%% popup is rendered inside the root and driven by the `datepicker'
%%% behaviour (assets/js/components/datepicker.ts). The browser builds the
%%% month grid from the shared template templates/datepicker_month.mustache.
%%%
%%% ah_datepicker/3 builds an #ah_datepicker{} (include/aihtml_datepicker.hrl)
%%% and render/1 turns it into HTML, so pages may also write the record
%%% directly (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_datepicker).
-behaviour(aihtml_element).

-include("aihtml_datepicker.hrl").

-export([ah_datepicker/3, render/1, fields/1, catalog/0]).

-export_type([date/0, date_value/0, label_key/0, labels/0, element/0]).

-define(H, aihtml_html).
-define(E, aihtml_element).
-define(D, aihtml_lib_date).

%% Shared template (see aihtml_tpl): also compiled to AH.tpl.* for the browser.
-compile({parse_transform, beamai_mustache_transform}).
-mustache_template({tpl_datepicker_month, "../templates/datepicker_month.mustache"}).

%% An ISO date (<<"2026-09-29">> or "2026-09-29"), a calendar:date() or
%% undefined.
-type date() :: binary() | string() | calendar:date() | undefined.
%% One date, or a range {From, To}.
-type date_value() :: date() | {date(), date()}.
-type label_key() :: months | months_short | weekdays | title | today | clear
                   | prev_month | next_month | prev_year | next_year.
%% Texts of the calendar; `months', `months_short' (12) and `weekdays'
%% (7, from Sunday) are lists, `title' is a display format.
-type labels() :: #{label_key() => unicode:chardata() | [unicode:chardata()]}.
-type element() :: #ah_datepicker{}.

%% @doc A read-only text field with sigil's calendar popup. `Value' is an
%% ISO date (`<<"2026-09-29">>'), a `calendar:date()' or `undefined'; a
%% pair `{From, To}' (or the `range' modifier) selects a range, whose
%% `data-ah-value' is `"from,to"'.
%%
%% Css: `disabled', `readonly', `range', `clearable', `inline' (the month
%% grid is always shown under the field, rendered on the server with the
%% template the browser uses).
%% Options (in Attrs): `placeholder' (default "Select date..."), `format'
%% (display format: yyyy yy MMMM MMM MM M dd d, default "yyyy-MM-dd"),
%% `min', `max' (dates), `disabled_dates' (a list of dates),
%% `first_day' (0 = Sunday .. 6, default the language's), `week_numbers' (default
%% false), `other_month_days' (default true), `weekends' (weekend days
%% in the error colour, sigil's enable-weekend; default false), `labels' (a map with
%% `months', `months_short', `weekdays' (7, from Sunday), `title' (a
%% format, default "MMMM yyyy"), `today', `clear', `prev_month',
%% `next_month', `prev_year', `next_year').
-spec ah_datepicker(date_value(), aihtml_html:css(), aihtml_html:attrs()) -> #ah_datepicker{}.
ah_datepicker(Value, Css, Attrs) ->
    ?E:build(?MODULE, #ah_datepicker{value = Value}, Css, Attrs).

%% @doc The field names of #ah_datepicker{}.
-spec fields(atom()) -> [atom()].
fields(ah_datepicker) -> record_info(fields, ah_datepicker).

-spec render(element()) -> aihtml_html:html().
render(#ah_datepicker{value = Value0, name = Name, disabled = Disabled,
                      readonly = Readonly, inline = Inline,
                      first_day = FirstDay0, min = Min0, max = Max0} = R0) ->
    {Id, R} = ensure_id(R0),
    Classes = ?E:classes(?MODULE, R),           % checks the flag fields first
    Range = R#ah_datepicker.range orelse is_range(Value0),
    Value = case Range of
                true -> range_value(Value0);
                false -> iso(Value0)
            end,
    Labels = labels(R#ah_datepicker.labels),
    Format = text(R#ah_datepicker.format),
    FirstDay = aihtml_lib_date:first_day(FirstDay0),
    (is_integer(FirstDay) andalso FirstDay >= 0 andalso FirstDay =< 6)
        orelse error({aihtml, {bad_first_day, FirstDay}}),
    Min = iso_opt(Min0),
    Max = iso_opt(Max0),
    Off = [iso(D) || D <- R#ah_datepicker.disabled_dates],
    {Iso, Display} = case Value of
                         undefined -> {<<>>, <<>>};
                         {F, T} -> {<<(nz(F))/binary, ",", (nz(T))/binary>>,
                                    range_display(F, T, Format, Labels)};
                         D -> {D, format(D, Format, Labels)}
                     end,
    Clear = [?H:el(button, {safe, <<"&times;">>}, [<<"ah-datepicker-clear">>],
                   [{type, button}, {tabindex, <<"-1">>},
                    {aria_label, maps:get(<<"clear">>, Labels)}])
             || R#ah_datepicker.clearable, not Disabled, not Readonly],
    Input = ?H:void(input, [<<"ah-datepicker-input">>],
                    [{type, text}, {id, sub_id(Id, <<"input">>)},
                     {autocomplete, off}, {spellcheck, <<"false">>}, {readonly, true},
                     {placeholder, R#ah_datepicker.placeholder},
                     {value, Display}, {disabled, Disabled},
                     {role, combobox}, {aria_haspopup, dialog},
                     {aria_expanded, <<"false">>}]),
    ?H:el('div',
          [?H:el('div',
                 [Input, Clear,
                  ?H:el(span, ?H:el(span, <<"📅"/utf8>>, [<<"ah-datepicker-icon">>], []),
                        [<<"ah-datepicker-trigger">>], [{aria_hidden, <<"true">>}])],
                 [<<"ah-datepicker-input-area">>], []),
           hidden(Name, Iso),
           ?H:el('div',
                 case Inline of
                     false -> [];
                     true ->
                         aihtml_tpl:safe(tpl_datepicker_month(
                           month_view(#{id => Id, value => Value, first_day => FirstDay,
                                        week_numbers => R#ah_datepicker.week_numbers,
                                        weekends => R#ah_datepicker.weekends,
                                        other_month => R#ah_datepicker.other_month_days,
                                        min => Min, max => Max,
                                        off => Off, labels => Labels})))
                 end,
                 [<<"ah-datepicker-popup">>],
                 [{role, case Inline of true -> group; false -> dialog end},
                  {aria_label, aihtml_i18n:text(common, choose_date)}])],
          [Classes,
           [<<"ah-datepicker-range">> || Range, not R#ah_datepicker.range]],
          %% the id comes first, as before; root_attrs repeats it in place
          [[{id, Id}, {data_ah, <<"datepicker">>}, {data_ah_value, Iso},
            {data_ah_range, Range},
            {data_ah_format, Format},
            {data_ah_min, Min},
            {data_ah_max, Max},
            {data_ah_disabled_dates,
             case Off of
                 [] -> undefined;
                 Ds -> iolist_to_binary(lists:join(<<",">>, Ds))
             end},
            {data_ah_first_day, FirstDay},
            {data_ah_week_numbers, R#ah_datepicker.week_numbers},
            {data_ah_weekends, R#ah_datepicker.weekends},
            {data_ah_other_month_days,
             case R#ah_datepicker.other_month_days of
                 true -> undefined;
                 false -> <<"false">>
             end},
            {data_ah_labels, iolist_to_binary(aihtml_json:encode(Labels))},
            {aria_disabled, Disabled andalso <<"true">>}],
           ?E:root_attrs(R, change)]).

is_range({A, B}) when not is_integer(A); not is_integer(B) -> true;
is_range({undefined, undefined}) -> true;
is_range(_) -> false.

range_value(undefined) -> undefined;
range_value({undefined, undefined}) -> undefined;
range_value({F, T}) ->
    case {iso(F), iso(T)} of
        {A, B} when A =/= undefined, B =/= undefined, A > B -> {B, A};
        Pair -> Pair
    end;
range_value(Single) -> {iso(Single), undefined}.

range_display(F, T, Format, Labels) ->
    iolist_to_binary([fmt_opt(F, Format, Labels),
                      [[<<" - ">>, format(T, Format, Labels)] || T =/= undefined]]).

fmt_opt(undefined, _, _) -> <<>>;
fmt_opt(D, Format, Labels) -> format(D, Format, Labels).

nz(undefined) -> <<>>;
nz(B) -> B.

iso_opt(undefined) -> undefined;
iso_opt(D) -> iso(D).

%% A date as <<"yyyy-mm-dd">>, checked.
iso(undefined) -> undefined;
iso(<<>>) -> undefined;
iso({Y, M, D} = Date) when is_integer(Y), is_integer(M), is_integer(D) ->
    calendar:valid_date(Date) orelse error({aihtml, {bad_date, Date}}),
    iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0B", [Y, M, D]));
iso(B) when is_binary(B) ->
    try
        <<Y:4/binary, "-", M:2/binary, "-", D:2/binary>> = B,
        iso({binary_to_integer(Y), binary_to_integer(M), binary_to_integer(D)})
    catch
        _:_ -> error({aihtml, {bad_date, B}})
    end;
iso(L) when is_list(L) -> iso(unicode:characters_to_binary(L));
iso(Other) -> error({aihtml, {bad_date, Other}}).

%% The defaults are the current language's (aihtml_i18n): its month names,
%% two-letter weekdays and the datepicker texts.
labels(Custom) when is_map(Custom) ->
    Defaults = (aihtml_i18n:texts(datepicker))#{
                 months => aihtml_i18n:format(months),
                 months_short => aihtml_i18n:format(months_short),
                 weekdays => aihtml_i18n:format(weekdays_min)},
    maps:foreach(fun(K, _) -> maps:is_key(K, Defaults)
                                  orelse error({aihtml, {bad_datepicker_label, K}})
                 end, Custom),
    M = maps:merge(Defaults, Custom),
    check_len(months, M, 12), check_len(months_short, M, 12), check_len(weekdays, M, 7),
    maps:fold(fun(K, V, Acc) when is_list(V), V =/= [], not is_integer(hd(V)) ->
                      Acc#{atom_to_binary(K) => [text(X) || X <- V]};
                 (K, V, Acc) -> Acc#{atom_to_binary(K) => text(V)}
              end, #{}, M).

check_len(K, M, N) ->
    length(maps:get(K, M)) =:= N orelse error({aihtml, {bad_datepicker_label, K}}).

%% date-fns style display format, the tokens the JS formatter knows too.
format(Iso, Format, Labels) ->
    <<Y:4/binary, "-", M:2/binary, "-", D:2/binary>> = Iso,
    Mi = binary_to_integer(M),
    Di = binary_to_integer(D),
    iolist_to_binary(fmt(Format, #{y => Y, m => Mi, d => Di}, Labels)).

fmt(<<"yyyy", R/binary>>, V, L) -> [maps:get(y, V) | fmt(R, V, L)];
fmt(<<"yy", R/binary>>, V, L) -> [binary:part(maps:get(y, V), 2, 2) | fmt(R, V, L)];
fmt(<<"MMMM", R/binary>>, V, L) ->
    [lists:nth(maps:get(m, V), maps:get(<<"months">>, L)) | fmt(R, V, L)];
fmt(<<"MMM", R/binary>>, V, L) ->
    [lists:nth(maps:get(m, V), maps:get(<<"months_short">>, L)) | fmt(R, V, L)];
fmt(<<"MM", R/binary>>, V, L) -> [?D:pad(maps:get(m, V)) | fmt(R, V, L)];
fmt(<<"M", R/binary>>, V, L) -> [integer_to_binary(maps:get(m, V)) | fmt(R, V, L)];
fmt(<<"dd", R/binary>>, V, L) -> [?D:pad(maps:get(d, V)) | fmt(R, V, L)];
fmt(<<"d", R/binary>>, V, L) -> [integer_to_binary(maps:get(d, V)) | fmt(R, V, L)];
fmt(<<C/utf8, R/binary>>, V, L) -> [<<C/utf8>> | fmt(R, V, L)];
fmt(<<>>, _, _) -> [].

%% The view of templates/datepicker_month.mustache for the month holding
%% the focused day (the value, or today), as the browser's dpView builds
%% it; used for the `inline' first render.
month_view(#{id := Id, value := Value, first_day := First, labels := L} = O) ->
    {Sel, From, To} = case Value of
                          {F, T} -> {undefined, days(F), days(T)};
                          V -> {days(V), undefined, undefined}
                      end,
    Today = calendar:date_to_gregorian_days(date()),
    Focus = case {Sel, From} of
                {undefined, undefined} -> Today;
                {undefined, _} -> From;
                _ -> Sel
            end,
    {Y, M, _} = calendar:gregorian_days_to_date(Focus),
    MonthStart = calendar:date_to_gregorian_days(Y, M, 1),
    MonthEnd = calendar:date_to_gregorian_days(Y, M, calendar:last_day_of_the_month(Y, M)),
    GridStart = ?D:start_of_week(MonthStart, First),
    GridEnd = ?D:start_of_week(MonthEnd, First) + 7,
    Off = [days(D) || D <- maps:get(off, O)],
    Min = days(maps:get(min, O)),
    Max = days(maps:get(max, O)),
    Day = fun(D) ->
              {Dy, Dm, Dd} = calendar:gregorian_days_to_date(D),
              Other = Dm =/= M,
              case Other andalso not maps:get(other_month, O) of
                  true -> #{empty => true};
                  false ->
                      Dis = (Min =/= undefined andalso D < Min)
                          orelse (Max =/= undefined andalso D > Max)
                          orelse lists:member(D, Off),
                      IsStart = D =:= From, IsEnd = D =:= To,
                      Selected = case Value of {_, _} -> IsStart orelse IsEnd; _ -> D =:= Sel end,
                      InRange = From =/= undefined andalso To =/= undefined
                          andalso D >= From andalso D =< To,
                      Dow = ?D:dow(D),
                      Iso = iso({Dy, Dm, Dd}),
                      Cls = [<<"ah-datepicker-day">>,
                             [<<" ah-datepicker-day-other-month">> || Other],
                             [<<" ah-datepicker-day-today">> || D =:= Today],
                             [<<" ah-datepicker-day-weekend">>
                              || maps:get(weekends, O), Dow =:= 0 orelse Dow =:= 6],
                             [<<" ah-datepicker-day-disabled">> || Dis],
                             [<<" ah-datepicker-day-selected">> || Selected],
                             [<<" ah-datepicker-day-in-range">> || InRange],
                             [<<" ah-datepicker-day-range-start">> || IsStart],
                             [<<" ah-datepicker-day-range-end">> || IsEnd],
                             [<<" ah-datepicker-day-focused">> || D =:= Focus]],
                      #{empty => false, cls => iolist_to_binary(Cls),
                        id => <<Id/binary, "-d", Iso/binary>>, date => Iso,
                        selected => atom_to_binary(Selected), disabled => atom_to_binary(Dis),
                        label => format(Iso, <<"d MMMM yyyy">>, L),
                        day => integer_to_binary(Dd)}
              end
          end,
    #{title_id => <<Id/binary, "-title">>,
      title => format(iso({Y, M, 1}), maps:get(<<"title">>, L), L),
      prev_year => maps:get(<<"prev_year">>, L), prev_month => maps:get(<<"prev_month">>, L),
      next_month => maps:get(<<"next_month">>, L), next_year => maps:get(<<"next_year">>, L),
      today => maps:get(<<"today">>, L),
      week_numbers => maps:get(week_numbers, O),
      txt_week => aihtml_i18n:text(common, week_short),
      weekdays => [#{label => lists:nth((First + I) rem 7 + 1, maps:get(<<"weekdays">>, L))}
                   || I <- lists:seq(0, 6)],
      weeks => [#{num => integer_to_binary(week_number(W, First)),
                  days => [Day(W + K) || K <- lists:seq(0, 6)]}
                || W <- lists:seq(GridStart, GridEnd - 1, 7)]}.

%% The day number of a checked ISO date (see iso/1).
days(undefined) -> undefined;
days(Iso) ->
    <<Y:4/binary, "-", M:2/binary, "-", D:2/binary>> = Iso,
    calendar:date_to_gregorian_days(binary_to_integer(Y), binary_to_integer(M),
                                    binary_to_integer(D)).

%% date-fns getWeek (weekStartsOn = First, firstWeekContainsDate = 1),
%% the same algorithm as weekNumber in datepicker.ts.
week_number(D, First) ->
    {Y, _, _} = calendar:gregorian_days_to_date(D),
    Jan1 = fun(Yr) -> ?D:start_of_week(calendar:date_to_gregorian_days(Yr, 1, 1), First) end,
    Next = Jan1(Y + 1),
    This = Jan1(Y),
    WY = if D >= Next -> Y + 1;
            D >= This -> Y;
            true -> Y - 1
         end,
    (?D:start_of_week(D, First) - Jan1(WY)) div 7 + 1.

%% `name' goes to the hidden input, `id' stays on the root (and derives the
%% ids of the parts). A root without an id gets one: the parts refer to
%% each other by id. Returns the id and the record holding it, for
%% root_attrs/2.
ensure_id(R) ->
    Id = case R#ah_datepicker.id of
             undefined -> <<"ah-p", (integer_to_binary(erlang:unique_integer([positive])))/binary>>;
             Id0 -> text(Id0)
         end,
    {Id, R#ah_datepicker{id = Id}}.

sub_id(Id, Part) -> <<Id/binary, "-", Part/binary>>.

hidden(undefined, _) -> [];
hidden(Name, Value) -> ?H:void(input, [], [{type, hidden}, {name, Name}, {value, Value}]).

text(undefined) -> <<>>;
text(B) when is_binary(B) -> B;
text(L) when is_list(L) -> unicode:characters_to_binary(L);
text(X) -> beamai_html_escape:to_binary(X, aihtml).

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => datepicker, category => form,
       signature => <<"ah_datepicker(Value, Css, Attrs)">>,
       root => <<"ah-datepicker">>,
       flags => [disabled, readonly, range, clearable, inline],
       classes => #{clearable => [<<"ah-datepicker-clearable">>]},
       options => [placeholder, format, min, max, disabled_dates, first_day,
                   week_numbers, other_month_days, weekends, labels],
       behavior => <<"datepicker">>,
       events => [<<"change">>, <<"ah:open">>, <<"ah:close">>],
       doc => <<"A date field with a month grid popup and full keyboard support; "
                "single dates or ranges, value in data-ah-value as yyyy-mm-dd.">>,
       option_docs =>
           #{disabled => <<"Not editable; the popup does not open.">>,
             readonly => <<"Shows the value; the popup does not open.">>,
             range => <<"Select a range: value \"from,to\" (implied by a {From, To} value).">>,
             clearable => <<"A clear button (and Backspace/Delete) empties the value.">>,
             inline => <<"The month grid is always shown under the field, rendered on the server.">>,
             placeholder => <<"Text of the empty field (default \"Select date...\").">>,
             format => <<"Display format: yyyy yy MMMM MMM MM M dd d (default yyyy-MM-dd).">>,
             min => <<"Earliest selectable date (ISO binary or calendar:date()).">>,
             max => <<"Latest selectable date.">>,
             disabled_dates => <<"List of dates that cannot be picked.">>,
             first_day => <<"First day of the week, 0 = Sunday .. 6 (default: the page language's, Sunday in English, Monday in Chinese).">>,
             week_numbers => <<"Show a week number column.">>,
             other_month_days => <<"Show days of the neighbouring months (default true).">>,
             weekends => <<"Colour Saturdays and Sundays.">>,
             labels => <<"Map of months, months_short, weekdays (from Sunday), title (a format), "
                         "today, clear, prev_month, next_month, prev_year, next_year.">>},
       methods =>
           [#{name => setValue, args => <<"(Iso | \"from,to\" | [From, To])">>,
              doc => <<"Set the value without firing change.">>},
            #{name => getValue, args => <<"()">>, doc => <<"Return data-ah-value.">>},
            #{name => clear, args => <<"()">>, doc => <<"Empty the value and fire change.">>},
            #{name => open, args => <<"()">>, doc => <<"Open the calendar.">>},
            #{name => close, args => <<"()">>, doc => <<"Close the calendar.">>}]}].
