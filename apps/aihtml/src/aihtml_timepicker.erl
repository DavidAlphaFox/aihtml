%%%-------------------------------------------------------------------
%%% @doc A clock-face time picker, ported from sigil (form/timepicker).
%%% See designs/04-components.md.
%%%
%%%   ah_timepicker(Value, Css, Attrs)    a clock face (SVG) with hour and
%%%                                       minute modes, 12h or 24h
%%%
%%% Value-bearing: the root carries `data-ah-value' (the canonical value,
%%% "" when empty), a hidden input carries it under the `name' taken from
%%% Attrs, and the root fires `change' when a value is committed.
%%%
%%% By default it renders a field that opens the sigil panel in a popup;
%%% the `inline' flag renders the panel alone, as sigil does. The
%%% behaviour is `timepicker' (assets/js/components/timepicker.ts).
%%%
%%% ah_timepicker/3 builds an #ah_timepicker{} (include/aihtml_timepicker.hrl)
%%% and render/1 turns it into HTML, so pages may also write the record
%%% directly (designs/05-records.md).
%%%
%%% Values are validated when rendering: a malformed value raises
%%% `error({aihtml, {bad_value, timepicker, Value}})'; `undefined' and
%%% `<<>>' mean empty.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_timepicker).
-behaviour(aihtml_element).

-include("aihtml_timepicker.hrl").

-export([ah_timepicker/3, normalize_time/1, render/1, fields/1, catalog/0]).

-export_type([value/0, format/0, element/0]).

-define(H, aihtml_html).
-define(E, aihtml_element).

%% Shared templates (see aihtml_tpl): also compiled to AH.tpl.* for the
%% browser, which redraws the clock header and numbers on mode changes.
-compile({parse_transform, beamai_mustache_transform}).
-mustache_template({tpl_timepicker_header, "../templates/timepicker_header.mustache"}).
-mustache_template({tpl_timepicker_numbers, "../templates/timepicker_numbers.mustache"}).

%% Clock geometry, in the SVG's 260x260 viewBox (sigil timepicker/math).
-define(CX, 130).
-define(CY, 130).
-define(FACE_R, 120).
-define(OUTER_R, 105).
-define(INNER_R, 70).
-define(SELECT_R, 18).

%% "HH:MM", "HH:MM:SS", {H, M}, {H, M, S}; undefined or <<>> is empty.
-type value() :: undefined | binary() | string()
               | {integer(), integer()} | {integer(), integer(), integer()}.
-type format() :: '12h' | '24h' | 12 | 24 | binary().
-type time() :: {0..23, 0..59}.
-type element() :: #ah_timepicker{}.

%% @doc A time picker. `Value' is `<<"HH:MM">>' (24h), `<<"HH:MM:SS">>'
%% (seconds are dropped: sigil picks minutes), `{H, M}', `{H, M, S}',
%% `undefined' or `<<>>'. The value is always written as "HH:MM".
%%
%% Options (taken from Attrs): `format' (`'12h'' default, or `'24h''),
%% `minute_step' (default 5, sigil's minute-interval), `auto_switch'
%% (default true: go to minutes after an hour is picked), `min', `max'
%% (times, same forms as Value; out-of-range numbers are disabled),
%% `placeholder', `footer' (html under the clock).
-spec ah_timepicker(binary() | string() | tuple() | undefined,
                    aihtml_html:css(), aihtml_html:attrs()) -> #ah_timepicker{}.
ah_timepicker(Value, Css, Attrs) ->
    ?E:build(?MODULE, #ah_timepicker{value = Value}, Css, Attrs).

%% @doc The field names of #ah_timepicker{}.
-spec fields(atom()) -> [atom()].
fields(ah_timepicker) -> record_info(fields, ah_timepicker).

-spec render(element()) -> aihtml_html:html().
render(#ah_timepicker{value = Value0, name = Name, inline = Inline, disabled = Disabled,
                      clearable = Clearable} = T) ->
    Classes = ?E:classes(?MODULE, T),
    Landscape = T#ah_timepicker.view =:= landscape,
    Value = normalize_time(Value0),
    Format = time_format(T#ah_timepicker.format),
    Step = case T#ah_timepicker.minute_step of
               S when is_integer(S), S >= 1, S =< 30 -> S;
               S -> error({aihtml, {bad_option, timepicker, minute_step, S}})
           end,
    Min = range_opt(min, T#ah_timepicker.min),
    Max = range_opt(max, T#ah_timepicker.max),
    AutoSwitch = T#ah_timepicker.auto_switch =/= false,
    {H, M} = case Value of
                 undefined -> {12, 0};
                 _ -> Value
             end,
    ValueBin = time_bin(Value),
    Panel = time_panel(H, M, Format, Step, Min, Max, Landscape, Disabled,
                       T#ah_timepicker.footer, Inline),
    Placeholder = case T#ah_timepicker.placeholder of
                      undefined when Format =:= '12h' -> <<"--:-- --">>;
                      undefined -> <<"--:--">>;
                      P -> P
                  end,
    Body = case Inline of
               true -> Panel;
               false ->
                   [?H:el('div',
                          [?H:void(input, [<<"ah-timepicker-input">>],
                                   [{type, text}, {value, display_time(Value, Format)},
                                    {placeholder, Placeholder}, {autocomplete, off},
                                    {aria_haspopup, dialog}, {aria_expanded, <<"false">>},
                                    {disabled, Disabled}]),
                           [clear_button(<<"ah-timepicker-clear">>, Value =:= undefined)
                            || Clearable],
                           ?H:el(span, clock_icon(), [<<"ah-timepicker-trigger">>],
                                 [{aria_hidden, <<"true">>}])],
                          [<<"ah-timepicker-input-area">>], []),
                    ?H:el('div', Panel, [<<"ah-timepicker-popup">>],
                          [{role, dialog}, {aria_label, <<"Choose time">>}, {hidden, true}])]
           end,
    ?H:el('div', [Body, hidden_input(Name, ValueBin, Disabled)],
          Classes,
          [[{data_ah, <<"timepicker">>}, {data_ah_value, ValueBin},
            {data_format, Format}, {data_step, Step},
            {data_min, time_bin(Min)}, {data_max, time_bin(Max)},
            {data_auto_switch, not AutoSwitch andalso <<"false">>},
            {aria_disabled, Disabled andalso <<"true">>}],
           ?E:root_attrs(T, change)]).

%% @doc Normalise a time value to `{H, M}' or `undefined'.
-spec normalize_time(term()) -> time() | undefined.
normalize_time(undefined) -> undefined;
normalize_time(<<>>) -> undefined;
normalize_time({H, M}) when is_integer(H), H >= 0, H =< 23,
                            is_integer(M), M >= 0, M =< 59 -> {H, M};
normalize_time({H, M, S}) when is_integer(S), S >= 0, S =< 59 ->
    case normalize_time({H, M}) of
        undefined -> bad_time({H, M, S});
        T -> T
    end;
normalize_time(B) when is_binary(B) ->
    case re:run(B, <<"^\\s*([01]?[0-9]|2[0-3]):([0-5][0-9])(?::[0-5][0-9])?\\s*$">>,
                [{capture, all_but_first, binary}]) of
        {match, [Hb, Mb]} -> {binary_to_integer(Hb), binary_to_integer(Mb)};
        nomatch -> bad_time(B)
    end;
normalize_time(L) when is_list(L) ->
    case io_lib:printable_unicode_list(L) of
        true -> normalize_time(unicode:characters_to_binary(L));
        false -> bad_time(L)
    end;
normalize_time(Other) -> bad_time(Other).

-spec bad_time(term()) -> no_return().
bad_time(V) -> error({aihtml, {bad_value, timepicker, V}}).

time_format(F) when F =:= '12h'; F =:= <<"12h">>; F =:= 12 -> '12h';
time_format(F) when F =:= '24h'; F =:= <<"24h">>; F =:= 24 -> '24h';
time_format(F) -> error({aihtml, {bad_option, timepicker, format, F}}).

range_opt(_K, undefined) -> undefined;
range_opt(K, V) ->
    try normalize_time(V)
    catch error:{aihtml, {bad_value, _, _}} ->
            error({aihtml, {bad_option, timepicker, K, V}})
    end.

time_bin(undefined) -> <<>>;
time_bin({H, M}) -> <<(pad2(H))/binary, ":", (pad2(M))/binary>>.

display_time(undefined, _) -> <<>>;
display_time({H, M}, '24h') -> time_bin({H, M});
display_time({H, M}, '12h') ->
    {H12, P} = to12(H),
    <<(integer_to_binary(H12))/binary, ":", (pad2(M))/binary, " ",
      (period_label(P))/binary>>.

period_label(am) -> <<"AM">>;
period_label(pm) -> <<"PM">>.

pad2(N) when N < 10 -> <<"0", (integer_to_binary(N))/binary>>;
pad2(N) -> integer_to_binary(N).

to12(0) -> {12, am};
to12(H) when H < 12 -> {H, am};
to12(12) -> {12, pm};
to12(H) -> {H - 12, pm}.

to24(12, am) -> 0;
to24(H, am) -> H;
to24(12, pm) -> 12;
to24(H, pm) -> H + 12.

%% The sigil panel: header, SVG clock (hours mode), footer.
time_panel(H, M, Format, Step, Min, Max, Landscape, Disabled, Footer, Inline) ->
    {_, Period} = to12(H),
    Header = time_header(H, M, Period, Format, Disabled),
    Svg = clock_svg(H, Format, Period, Step, Min, Max),
    ?H:el('div',
          [?H:el('div', Header, [<<"ah-timepicker-header">>], []),
           ?H:el('div', ?H:el('div', Svg, [<<"ah-timepicker-clock-wrap">>], []),
                 [<<"ah-timepicker-body">>], []),
           ?H:el('div', case Footer of undefined -> []; _ -> Footer end,
                 [<<"ah-timepicker-footer">>], [])],
          [<<"ah-timepicker">>,
           [<<"ah-timepicker-landscape">> || Landscape],
           [<<"ah-timepicker-disabled">> || Disabled andalso Inline]],
          []).

%% View data as built by timepicker.ts tpHeader (hours mode).
time_header(H, M, Period, Format, Disabled) ->
    HourText = case Format of
                   '24h' -> pad2(H);
                   '12h' -> integer_to_binary(element(1, to12(H)))
               end,
    aihtml_tpl:safe(tpl_timepicker_header(
                      #{hours => HourText, minutes => pad2(M),
                        hours_active => true, minutes_active => false,
                        twelve => Format =:= '12h',
                        am => Period =:= am, pm => Period =:= pm,
                        disabled => Disabled,
                        tabindex => case Disabled of true -> -1; false -> 0 end})).

%% The clock in hours mode; the browser redraws the numbers on mode changes.
%% Drawing order differs from sigil: the selection circle goes under the
%% numbers so the selected number stays readable.
clock_svg(H, Format, Period, Step, Min, Max) ->
    {Display, Selected} = case Format of
                              '24h' -> {H rem 12, H};
                              '12h' -> {H12, _} = to12(H), {H12 rem 12, H12}
                          end,
    Angle = value_angle(Display, 12),
    R = case Format =:= '24h' andalso (H =:= 0 orelse H >= 13) of
            true -> ?INNER_R;
            false -> ?OUTER_R
        end,
    {X, Y} = angle_xy(Angle, R),
    Hours = case Format of
                '12h' -> [{I, integer_to_binary(I), ?OUTER_R, to24(I, Period), false}
                          || I <- lists:seq(1, 12)];
                '24h' -> [{I, integer_to_binary(I), ?OUTER_R, I, false}
                          || I <- lists:seq(1, 12)] ++
                             [{V, pad2(V), ?INNER_R, V, true}
                              || V <- [0 | lists:seq(13, 23)]]
            end,
    Numbers = [begin
                   {Nx, Ny} = angle_xy(value_angle(Val rem 12, 12), Rad),
                   #{label => Label, val => Val, x => num(Nx), y => num(Ny),
                     inner => Inner, selected => Val =:= Selected,
                     disabled => not hour_allowed(H24, Step, Min, Max)}
               end || {Val, Label, Rad, H24, Inner} <- Hours],
    Svg = fun(Tag, Cls, As) -> ?H:el(Tag, [], Cls, As) end,
    ?H:el(svg,
          [Svg(circle, [<<"ah-timepicker-face">>],
               [{cx, ?CX}, {cy, ?CY}, {r, ?FACE_R}]),
           Svg(line, [<<"ah-timepicker-hand">>],
               [{x1, ?CX}, {y1, ?CY}, {x2, num(X)}, {y2, num(Y)}]),
           Svg(circle, [<<"ah-timepicker-selection">>],
               [{cx, num(X)}, {cy, num(Y)}, {r, ?SELECT_R}]),
           Svg(circle, [<<"ah-timepicker-center">>], [{cx, ?CX}, {cy, ?CY}, {r, 4}]),
           ?H:el(g, aihtml_tpl:safe(tpl_timepicker_numbers(#{numbers => Numbers})),
                 [<<"ah-timepicker-numbers">>], [])],
          [<<"ah-timepicker-svg">>],
          [{viewBox, <<"0 0 260 260">>}, {xmlns, <<"http://www.w3.org/2000/svg">>},
           {role, slider}, {tabindex, 0}, {aria_label, <<"Hours">>},
           {aria_valuemin, 0}, {aria_valuemax, 23}, {aria_valuenow, H},
           {aria_valuetext, integer_to_binary(Selected)}]).

hour_allowed(_H, _Step, undefined, undefined) -> true;
hour_allowed(H, Step, Min, Max) ->
    Lo = minutes(Min, 0),
    Hi = minutes(Max, 24 * 60 - 1),
    lists:any(fun(M) -> T = H * 60 + M, T >= Lo andalso T =< Hi end,
              lists:seq(0, 59, Step)).

minutes(undefined, Default) -> Default;
minutes({H, M}, _) -> H * 60 + M.

value_angle(V, Max) -> V / Max * 2 * math:pi().

angle_xy(A, R) -> {?CX + R * math:sin(A), ?CY - R * math:cos(A)}.

num(F) ->
    R = round(F * 100) / 100,
    case R == trunc(R) of
        true -> integer_to_binary(trunc(R));
        false -> float_to_binary(R, [{decimals, 2}, compact])
    end.

clock_icon() ->
    {safe, <<"<svg width=\"16\" height=\"16\" viewBox=\"0 0 24 24\" fill=\"none\" "
             "stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\">"
             "<circle cx=\"12\" cy=\"12\" r=\"9\"/><path d=\"M12 7v5l3 2\"/></svg>">>}.

hidden_input(Name, Value, Disabled) ->
    ?H:void(input, [], [{type, hidden}, {name, Name}, {value, Value},
                        {disabled, Disabled}]).

clear_button(Cls, Hidden) ->
    ?H:el(button, {safe, <<"&times;">>}, [Cls],
          [{type, button}, {tabindex, <<"-1">>}, {aria_label, <<"Clear">>},
           {hidden, Hidden}]).

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => timepicker, category => form,
       signature => <<"ah_timepicker(Value, Css, Attrs)">>,
       root => <<"ah-timepicker-field">>,
       groups => #{view => {[portrait, landscape], none}},
       flags => [inline, disabled, clearable],
       classes => #{portrait => []},
       options => [format, minute_step, auto_switch, min, max, placeholder, footer],
       behavior => <<"timepicker">>,
       events => [<<"change">>],
       option_docs =>
           #{portrait => <<"Header above the clock (sigil's default view).">>,
             landscape => <<"Header beside the clock.">>,
             inline => <<"Render the clock panel in place instead of a field with a popup.">>,
             disabled => <<"No interaction; the hidden input is disabled too.">>,
             clearable => <<"A clear button in the field that empties the value.">>,
             format => <<"'12h' (default, with AM/PM) or '24h' (two rings).">>,
             minute_step => <<"Minute granularity, 1..30 (default 5).">>,
             auto_switch => <<"Go to the minutes after an hour is picked (default true).">>,
             min => <<"Earliest selectable time, same forms as Value.">>,
             max => <<"Latest selectable time, same forms as Value.">>,
             placeholder => <<"Field text when empty.">>,
             footer => <<"HTML under the clock.">>},
       methods =>
           [#{name => getValue, args => <<"()">>, doc => <<"Return \"HH:MM\" or \"\".">>},
            #{name => setValue, args => <<"(Time)">>,
              doc => <<"Set \"HH:MM\" (or \"\" to clear) without firing change.">>},
            #{name => clear, args => <<"()">>, doc => <<"Empty the value and fire change.">>},
            #{name => open, args => <<"()">>, doc => <<"Open the popup and focus the clock.">>},
            #{name => close, args => <<"()">>, doc => <<"Close the popup.">>},
            #{name => setMode, args => <<"(hours | minutes)">>,
              doc => <<"Show the hour or the minute face.">>}],
       doc => <<"Clock-face time picker (12h/24h, minute step, min/max) in a popup "
                "field, or inline. Value \"HH:MM\".">>}].
