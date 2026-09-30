%%%-------------------------------------------------------------------
%%% @doc The contribution heatmap, ported from sigil
%%% (data/heatmap_calendar). DOM and class names are sigil's, so the styles
%%% in priv/css/sigil apply unchanged.
%%%
%%%   ah_heatmap_calendar(Data, Css, Attrs)   GitHub-style contribution grid
%%%
%%% heatmap_calendar fires 'ah:select' with the clicked date as
%%% `data-ah-value'.
%%%
%%% ah_heatmap_calendar/3 builds an element record (#ah_heatmap_calendar{},
%%% defined in include/aihtml_heatmap_calendar.hrl) and render/1 turns it
%%% into HTML, so pages may also write the record directly
%%% (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_heatmap_calendar).
-behaviour(aihtml_element).

-include("aihtml_heatmap_calendar.hrl").

-export([ah_heatmap_calendar/3, render/1, fields/1, catalog/0]).

-export_type([element/0, day/0, data/0]).

-define(H, aihtml_html).
-define(E, aihtml_element).

-type html() :: aihtml_html:html().
-type css() :: aihtml_html:css().
-type attrs() :: aihtml_html:attrs().
%% An ISO date (<<"2026-09-29">> or "2026-09-29") or a calendar:date().
-type day() :: binary() | string() | calendar:date().
%% Values per day, as a map or a list of pairs.
-type data() :: #{day() => number()} | [{day(), number()}].
-type element() :: #ah_heatmap_calendar{}.

%%%===================================================================
%%% Builder
%%%===================================================================

%% @doc A contribution heatmap. `Data' maps days (ISO dates or
%% `{Y, M, D}') to numbers. Options: `months' (how far back, default 12),
%% `end_date' (the last day, default today), `thresholds' (ascending,
%% default [0, 1, 3, 6]: N thresholds give N + 1 colour levels),
%% `weekday_labels' (7, from Sunday), `month_labels' (12), `legend'
%% (`{Less, More}' texts, or false), `tooltip' (text with {date} and
%% {value}).
-spec ah_heatmap_calendar(data(), css(), attrs()) -> #ah_heatmap_calendar{}.
ah_heatmap_calendar(Data, Css, Attrs) ->
    ?E:build(?MODULE, #ah_heatmap_calendar{data = Data}, Css, Attrs).

%% @doc The field names of this component's record.
-spec fields(atom()) -> [atom()].
fields(ah_heatmap_calendar) -> record_info(fields, ah_heatmap_calendar).

%%%===================================================================
%%% Rendering
%%%===================================================================

-spec render(element()) -> html().
render(#ah_heatmap_calendar{} = R) -> render_heatmap(R).

-define(CELL_PX, 15).
-define(MONTHS, [<<"Jan">>, <<"Feb">>, <<"Mar">>, <<"Apr">>, <<"May">>, <<"Jun">>,
                 <<"Jul">>, <<"Aug">>, <<"Sep">>, <<"Oct">>, <<"Nov">>, <<"Dec">>]).
-define(WEEKDAYS, [<<>>, <<"Mon">>, <<>>, <<"Wed">>, <<>>, <<"Fri">>, <<>>]).

render_heatmap(#ah_heatmap_calendar{data = Data0, months = Months, end_date = End0,
                                    thresholds = Thr, weekday_labels = WL0,
                                    month_labels = ML0, legend = Legend,
                                    tooltip = Tip} = R) ->
    is_integer(Months) andalso Months > 0
        orelse error({aihtml, {bad_option, months, Months}}),
    is_list(Thr) andalso lists:all(fun is_number/1, Thr) andalso lists:sort(Thr) =:= Thr
        orelse error({aihtml, {bad_option, thresholds, Thr}}),
    WL = labels(weekday_labels, WL0, ?WEEKDAYS, 7),
    ML = labels(month_labels, ML0, ?MONTHS, 12),
    Legend =:= false orelse (is_tuple(Legend) andalso tuple_size(Legend) =:= 2)
        orelse error({aihtml, {bad_option, legend, Legend}}),
    Data = heat_data(Data0),
    End = case End0 of
              undefined -> date();
              _ -> to_date(End0)
          end,
    Weeks = build_weeks(Data, End, Months),
    Spans = month_spans(Weeks),
    ?H:el('div',
          [?H:el('div',
                 [?H:el(span, lists:nth(Mo, ML), [<<"ah-heatmap-calendar__month">>],
                        [{style, [<<"width:">>, integer_to_binary(Span * ?CELL_PX), <<"px;">>]}])
                  || {{_, Mo}, Span} <- Spans],
                 [<<"ah-heatmap-calendar__months">>], [{aria_hidden, <<"true">>}]),
           ?H:el('div',
                 [?H:el('div', [?H:el(span, L, [<<"ah-heatmap-calendar__weekday">>], [])
                                || L <- WL],
                        [<<"ah-heatmap-calendar__weekdays">>], [{aria_hidden, <<"true">>}]),
                  ?H:el('div',
                        [?H:el('div',
                               [?H:el('div', [], [<<"ah-heatmap-calendar__cell">>],
                                      [{data_level, level(V, Thr)}, {data_date, iso(D)},
                                       {data_value, num(V)}])
                                || {D, V} <- Week],
                               [<<"ah-heatmap-calendar__week">>], [])
                         || Week <- Weeks],
                        [<<"ah-heatmap-calendar__grid">>], [])],
                 [<<"ah-heatmap-calendar__body">>], []),
           case Legend of
               false -> [];
               {Less, More} ->
                   ?H:el('div',
                         [?H:el(span, Less, [], []),
                          [?H:el('div', [], [<<"ah-heatmap-calendar__legend-cell">>],
                                 [{data_level, L}])
                           || L <- lists:seq(0, length(Thr))],
                          ?H:el(span, More, [], [])],
                         [<<"ah-heatmap-calendar__legend">>], [{aria_hidden, <<"true">>}])
           end,
           ?H:el('div', [], [<<"ah-heatmap-calendar__tooltip">>],
                 [{data_visible, <<"false">>}, {role, tooltip}])],
          ?E:classes(?MODULE, R),
          [[{data_ah, <<"heatmap-calendar">>}, {data_tip, text(Tip)}],
           ?E:root_attrs(R, 'ah:select')]).

labels(_, undefined, Default, _) -> Default;
labels(Key, L, _, N) ->
    is_list(L) andalso length(L) =:= N orelse error({aihtml, {bad_option, Key, L}}),
    L.

heat_data(M) when is_map(M) -> heat_data(maps:to_list(M));
heat_data(L) ->
    is_list(L) orelse error({aihtml, {bad_heatmap_data, L}}),
    maps:from_list([{to_date(D), case is_number(V) of
                                     true -> V;
                                     false -> error({aihtml, {bad_heatmap_value, D, V}})
                                 end} || {D, V} <- L]).

to_date({Y, M, D} = Date) when is_integer(Y), is_integer(M), is_integer(D) ->
    calendar:valid_date(Date) orelse error({aihtml, {bad_date, Date}}),
    Date;
to_date(S) when is_list(S) -> to_date(text(S));
to_date(<<Y:4/binary, "-", M:2/binary, "-", D:2/binary>> = B) ->
    Date = try {binary_to_integer(Y), binary_to_integer(M), binary_to_integer(D)}
           catch error:badarg -> error({aihtml, {bad_date, B}})
           end,
    calendar:valid_date(Date) orelse error({aihtml, {bad_date, B}}),
    Date;
to_date(Other) -> error({aihtml, {bad_date, Other}}).

%% Weeks from Sunday, starting in the week of the day `Months' months
%% before `End' and ending in the week of `End'; each week has 7 days.
build_weeks(Data, End, Months) ->
    {Y, M, D} = End,
    Mi = Y * 12 + (M - 1) - Months,
    {Y0, M0} = {Mi div 12, Mi rem 12 + 1},
    Start0 = {Y0, M0, min(D, calendar:last_day_of_the_month(Y0, M0))},
    S0 = calendar:date_to_gregorian_days(Start0),
    S = S0 - calendar:day_of_the_week(Start0) rem 7,       % back to Sunday
    E = calendar:date_to_gregorian_days(End),
    NWeeks = (E - S + 1 + 6) div 7,
    [[begin
          Day = calendar:gregorian_days_to_date(S + W * 7 + I),
          {Day, maps:get(Day, Data, 0)}
      end || I <- lists:seq(0, 6)]
     || W <- lists:seq(0, NWeeks - 1)].

%% Month label spans: weeks grouped by the month of their Wednesday.
month_spans(Weeks) ->
    lists:reverse(
      lists:foldl(fun(Week, Acc) ->
                          {{Y, M, _}, _} = lists:nth(4, Week),
                          case Acc of
                              [{{Y, M}, N} | Rest] -> [{{Y, M}, N + 1} | Rest];
                              _ -> [{{Y, M}, 1} | Acc]
                          end
                  end, [], Weeks)).

level(V, Thr) -> level(V, Thr, 0).

level(_, [], I) -> I;
level(V, [T | _], I) when V =< T -> I;
level(V, [_ | Ts], I) -> level(V, Ts, I + 1).

iso({Y, M, D}) ->
    iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0B", [Y, M, D])).

num(V) when is_integer(V) -> integer_to_binary(V);
num(V) when is_float(V) ->
    case V == trunc(V) of
        true -> integer_to_binary(trunc(V));
        false -> float_to_binary(V, [short])
    end.

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => heatmap_calendar, category => data,
       signature => <<"ah_heatmap_calendar(Data, Css, Attrs)">>,
       root => <<"ah-heatmap-calendar">>,
       options => [months, end_date, thresholds, weekday_labels, month_labels, legend, tooltip],
       behavior => <<"heatmap-calendar">>, events => [<<"ah:select">>],
       doc => <<"A contribution heatmap: one column per week, days coloured by value, "
                "a tooltip on hover and a select event on click.">>,
       option_docs =>
           #{months => <<"How many months back from end_date (default 12).">>,
             end_date => <<"The last day shown (default today).">>,
             thresholds => <<"Ascending limits of the colour levels (default [0, 1, 3, 6]); "
                             "a value =< the i-th limit gets level i.">>,
             weekday_labels => <<"7 labels from Sunday (default only Mon, Wed, Fri).">>,
             month_labels => <<"12 month names (default Jan ... Dec).">>,
             legend => <<"{Less, More} texts of the legend, or false to hide it.">>,
             tooltip => <<"Tooltip text; {date} and {value} are replaced "
                          "(default \"{value} · {date}\").">>},
       methods => []}].

%%%===================================================================
%%% Internal
%%%===================================================================

text(B) when is_binary(B) -> B;
text(L) when is_list(L) ->
    case unicode:characters_to_binary(L) of
        B when is_binary(B) -> B;
        _ -> error({aihtml, {bad_text, L}})
    end;
text(X) -> beamai_html_escape:to_binary(X, aihtml).
