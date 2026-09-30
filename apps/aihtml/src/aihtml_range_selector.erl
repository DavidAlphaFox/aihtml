%%%-------------------------------------------------------------------
%%% @doc A {Lo, Hi} range on a ticked track, ported from sigil
%%% (form/range_selector). See designs/04-components.md.
%%%
%%% A value-bearing component: `Attrs' go to the root, which carries
%%% `data-ah-value' and fires `change' (and `input' while the value is
%%% being edited); `name' goes to a hidden input. The server renders the
%%% complete first state (the track with its ticks, labels and markers),
%%% so nothing needs to be laid out in the browser before it is shown. The
%%% behaviour lives in assets/js/components/range_selector.ts.
%%%
%%% ah_range_selector/4 builds an #ah_range_selector{}
%%% (include/aihtml_range_selector.hrl) and render/1 turns it into HTML, so
%%% pages may also write the record directly (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_range_selector).
-behaviour(aihtml_element).

-include("aihtml_range_selector.hrl").

-export([ah_range_selector/4, render/1, fields/1, catalog/0]).

-export_type([format/0]).

-define(H, aihtml_html).
-define(E, aihtml_element).

%% How a range_selector writes a value: `number' (integers as is, others
%% with 2 decimals), `{fixed, Decimals}', `currency' ($1,234), `date'
%% (M/D/YYYY), `month' (Jan), `time' (4:00 PM) - the last three read the
%% value as a UTC timestamp in milliseconds - or `{Prefix, Format, Suffix}'.
-type format() :: number | {fixed, 0..20} | currency | date | month | time
                | {unicode:chardata(), format(), unicode:chardata()}.

%% @doc A range picked on a track: the selected span is a bar between two
%% round markers, each with its value above it; ticks and labels sit under
%% the track. `Range' is `{Min, Max}' or `{Min, Max, Step}' (default step
%% 1); values snap to `Min + k * Step'. `Value' is `{Lo, Hi}' or
%% `undefined' (the whole range); `data-ah-value' is "lo,hi". Drag a
%% marker to move one end, the bar to move both; the markers are ARIA
%% sliders (arrow keys: one step, PageUp/PageDown: a major tick,
%% Home/End).
%%
%% Css: `disabled'. Options: `major_ticks' (interval, default 10),
%% `minor_ticks' (interval, default 1), `tick_values' (explicit major tick
%% values), `show_major_ticks' (default true), `show_minor_ticks' (default
%% false), `show_labels' (default true), `show_markers' (default true),
%% `labels_format', `markers_format' (see format(); markers default to the
%% labels' format), `min_span' (smallest Hi - Lo, default 0).
-spec ah_range_selector({number(), number()} | {number(), number(), number()},
                        {number(), number()} | undefined,
                        aihtml_html:css(), aihtml_html:attrs()) -> #ah_range_selector{}.
ah_range_selector(Range, Value, Css, Attrs) ->
    ?E:build(?MODULE, #ah_range_selector{range = Range, value = Value}, Css, Attrs).

%% @doc The field names of #ah_range_selector{}.
-spec fields(atom()) -> [atom()].
fields(ah_range_selector) -> record_info(fields, ah_range_selector).

-spec render(#ah_range_selector{}) -> aihtml_html:html().
render(#ah_range_selector{range = {Min, Max}} = R) ->
    render(R#ah_range_selector{range = {Min, Max, 1}});
render(#ah_range_selector{range = {Min, Max, Step}, value = Value, name = Name,
                                disabled = Disabled, min_span = Span} = R)
  when is_number(Min), is_number(Max), is_number(Step), Max > Min, Step > 0 ->
    Classes = ?E:classes(?MODULE, R),
    (is_number(Span) andalso Span >= 0 andalso Span =< Max - Min)
        orelse error({aihtml, {bad_option, min_span, Span}}),
    LFmt = check_format(labels_format, R#ah_range_selector.labels_format),
    MFmt = case R#ah_range_selector.markers_format of
               undefined -> LFmt;
               F -> check_format(markers_format, F)
           end,
    Snap = fun(V) -> tidy(Min + round((V - Min) / Step) * Step) end,
    {Lo, Hi} = case Value of
                   undefined -> {Min, Max};
                   {A, B} when is_number(A), is_number(B) ->
                       {max(Min, Snap(min(A, B))), min(Max, Snap(max(A, B)))};
                   Other -> error({aihtml, {bad_range_value, Other}})
               end,
    Pct = fun(V) -> (V - Min) / (Max - Min) * 100 end,
    Major = R#ah_range_selector.major_ticks,
    Minor = R#ah_range_selector.minor_ticks,
    [error({aihtml, {bad_option, K, V}})
     || {K, V} <- [{major_ticks, Major}, {minor_ticks, Minor}], not is_number(V) orelse V < 0],
    MajorValues = case R#ah_range_selector.tick_values of
                      undefined -> steps(Min, Max, Major);
                      Vs -> [V || V <- Vs, is_number(V) orelse
                                               error({aihtml, {bad_option, tick_values, Vs}})]
                  end,
    Tick = fun(Kind, V) ->
                   ?H:el('div', [], [<<"ah-range-selector-tick">>,
                                     <<"ah-range-selector-tick-", Kind/binary>>],
                         [{style, [<<"left:">>, pct(Pct(V))]}])
           end,
    Ticks = [[Tick(<<"major">>, V) || R#ah_range_selector.show_major_ticks, V <- MajorValues],
             [Tick(<<"minor">>, V) || R#ah_range_selector.show_minor_ticks,
                                      V <- steps(Min, Max, Minor)],
             [?H:el('div', format(V, LFmt), [<<"ah-range-selector-label">>],
                    [{style, [<<"left:">>, pct(Pct(V))]}])
              || R#ah_range_selector.show_labels, V <- MajorValues]],
    Aria = [{aria_valuemin, num(Min)}, {aria_valuemax, num(Max)},
            {aria_disabled, Disabled andalso <<"true">>}],
    Marker = fun(Side, Label, V) ->
                     Txt = format(V, MFmt),
                     ?H:el('div', ?H:el(span, Txt, [<<"ah-range-selector-marker-value">>], []),
                           [<<"ah-range-selector-marker">>,
                            <<"ah-range-selector-marker-", Side/binary>>],
                           [{style, [<<"left:">>, pct(Pct(V)),
                                     [<<";display:none">> || not R#ah_range_selector.show_markers]]},
                            {role, slider}, {tabindex, case Disabled of true -> <<"-1">>;
                                                                          false -> <<"0">> end},
                            {aria_label, Label}, {aria_valuenow, num(V)},
                            {aria_valuetext, Txt} | Aria])
             end,
    Val = <<(num(Lo))/binary, ",", (num(Hi))/binary>>,
    ?H:el('div',
          [?H:el('div',
                [?H:el('div', Ticks, [<<"ah-range-selector-ticks">>], [{aria_hidden, <<"true">>}]),
                 ?H:el('div', [], [<<"ah-range-selector-shutter-left">>],
                       [{style, [<<"left:0;width:">>, pct(Pct(Lo))]}]),
                 ?H:el('div', ?H:el('div', [], [<<"ah-range-selector-slider-inner">>], []),
                       [<<"ah-range-selector-slider">>],
                       [{style, [<<"left:">>, pct(Pct(Lo)), <<";width:">>, pct(Pct(Hi) - Pct(Lo))]}]),
                 ?H:el('div', [], [<<"ah-range-selector-shutter-right">>],
                       [{style, [<<"left:">>, pct(Pct(Hi)), <<";width:">>, pct(100 - Pct(Hi))]}]),
                 Marker(<<"left">>, aihtml_i18n:text(common, minimum), Lo),
                 Marker(<<"right">>, aihtml_i18n:text(common, maximum), Hi)],
                [<<"ah-range-selector-track">>], []),
           hidden(Name, Val)],
          Classes,
          [[{role, group}, {data_ah, <<"range-selector">>}, {data_ah_value, Val},
            {data_ah_min, num(Min)}, {data_ah_max, num(Max)}, {data_ah_step, num(Step)},
            {data_ah_page, num(case Major > 0 of true -> Major; false -> Step * 10 end)},
            {data_ah_min_span, num(Span)},
            {data_ah_format, iolist_to_binary(aihtml_json:encode(format_json(MFmt)))},
            {aria_disabled, Disabled andalso <<"true">>}],
           ?E:root_attrs(R, change)]);
render(#ah_range_selector{range = Range}) ->
    error({aihtml, {bad_range, Range}}).

%% Min, Min + I, ... up to Max (none when I is 0).
steps(_, _, I) when I =< 0 -> [];
steps(Min, Max, I) ->
    [tidy(Min + K * I) || K <- lists:seq(0, floor((Max - Min) / I + 1.0e-9))].

tidy(V) when is_float(V) ->
    case round(V) of
        I when abs(V - I) < 1.0e-9 -> I;
        _ -> round(V * 1.0e9) / 1.0e9
    end;
tidy(V) -> V.

pct(P) ->
    [float_to_binary(float(tidy(P)), [{decimals, 4}, compact]), <<"%">>].

num(I) when is_integer(I) -> integer_to_binary(I);
num(F) when is_float(F) ->
    case tidy(F) of
        I when is_integer(I) -> integer_to_binary(I);
        G -> float_to_binary(G, [short])
    end.

check_format(K, F) ->
    case valid_format(F) of
        true -> F;
        false -> error({aihtml, {bad_option, K, F}})
    end.

valid_format(F) when F =:= number; F =:= currency; F =:= date; F =:= month; F =:= time -> true;
valid_format({fixed, N}) -> is_integer(N) andalso N >= 0 andalso N =< 20;
valid_format({_, F, _}) -> valid_format(F);
valid_format(_) -> false.

%% The format for the browser, which formats the markers while dragging.
format_json({P, F, S}) ->
    (format_json(F))#{<<"p">> => text(P), <<"s">> => text(S)};
format_json({fixed, N}) -> #{<<"f">> => <<"fixed">>, <<"n">> => N};
format_json(F) -> #{<<"f">> => atom_to_binary(F)}.

%% @doc The text of a range_selector value in a format (the browser's
%% formatter does the same).
format(V, {P, F, S}) -> iolist_to_binary([text(P), format(V, F), text(S)]);
format(V, number) ->
    case abs(V - round(V)) < 0.001 of
        true -> integer_to_binary(round(V));
        false -> fixed(V, 2)
    end;
format(V, {fixed, N}) -> fixed(V, N);
format(V, currency) ->
    I = round(V),
    Digits = integer_to_binary(abs(I)),
    <<"$", (case I < 0 of true -> <<"-">>; false -> <<>> end)/binary,
      (group3(Digits))/binary>>;
format(V, date) ->
    {{Y, M, D}, _} = utc(V),
    iolist_to_binary([integer_to_binary(M), $/, integer_to_binary(D), $/, integer_to_binary(Y)]);
format(V, month) ->
    {{_, M, _}, _} = utc(V),
    lists:nth(M, aihtml_i18n:format(months_short));
format(V, time) ->
    {_, {H, Mi, _}} = utc(V),
    H12 = case H rem 12 of 0 -> 12; X -> X end,
    iolist_to_binary([integer_to_binary(H12), $:, pad2(Mi), $\s,
                      case H >= 12 of true -> <<"PM">>; false -> <<"AM">> end]).

fixed(V, N) -> float_to_binary(float(V), [{decimals, N}]).

group3(Digits) ->
    case byte_size(Digits) of
        L when L =< 3 -> Digits;
        L -> <<(group3(binary:part(Digits, 0, L - 3)))/binary, ",",
               (binary:part(Digits, L - 3, 3))/binary>>
    end.

utc(Ms) ->
    calendar:system_time_to_universal_time(floor(Ms), millisecond).

pad2(N) when N < 10 -> <<"0", (integer_to_binary(N))/binary>>;
pad2(N) -> integer_to_binary(N).

hidden(undefined, _) -> [];
hidden(Name, Value) -> ?H:void(input, [], [{type, hidden}, {name, Name}, {value, Value}]).

text(undefined) -> <<>>;
text(B) when is_binary(B) -> B;
text(L) when is_list(L) -> unicode:characters_to_binary(L);
text(X) -> beamai_html_escape:to_binary(X, aihtml).

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => range_selector, category => form,
       signature => <<"ah_range_selector(Range, Value, Css, Attrs)">>,
       root => <<"ah-range-selector">>,
       flags => [disabled],
       options => [major_ticks, minor_ticks, tick_values, show_major_ticks, show_minor_ticks,
                   show_labels, show_markers, labels_format, markers_format, min_span],
       behavior => <<"range-selector">>,
       events => [<<"input">>, <<"change">>],
       doc => <<"A {Lo, Hi} range on a ticked track: drag either marker or the bar between "
                "them; numbers, money, dates or times; value \"lo,hi\".">>,
       option_docs =>
           #{disabled => <<"Not editable.">>,
             major_ticks => <<"Interval of the major ticks and labels (default 10).">>,
             minor_ticks => <<"Interval of the minor ticks (default 1).">>,
             tick_values => <<"Explicit major tick values, instead of the interval.">>,
             show_major_ticks => <<"Draw the major ticks (default true).">>,
             show_minor_ticks => <<"Draw the minor ticks (default false).">>,
             show_labels => <<"Label the major ticks (default true).">>,
             show_markers => <<"Show the two markers (default true).">>,
             labels_format => <<"number (default), {fixed, N}, currency, date, month, time "
                                "(timestamps in ms, UTC) or {Prefix, Format, Suffix}.">>,
             markers_format => <<"Format of the marker values (default: labels_format).">>,
             min_span => <<"Smallest Hi - Lo (default 0).">>},
       methods =>
           [#{name => setValue, args => <<"([Lo, Hi] | \"lo,hi\")">>,
              doc => <<"Move the markers without firing change.">>},
            #{name => getValue, args => <<"()">>, doc => <<"Return [Lo, Hi].">>}]}].
