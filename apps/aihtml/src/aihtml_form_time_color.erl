%%%-------------------------------------------------------------------
%%% @doc Time and colour pickers, ported from sigil (form/timepicker,
%%% form/colorpicker). See designs/04-components.md.
%%%
%%%   timepicker(Value, Css, Attrs)    a clock face (SVG) with hour and
%%%                                    minute modes, 12h or 24h
%%%   colorpicker(Value, Css, Attrs)   saturation/value area, hue bar,
%%%                                    optional alpha bar, hex and RGB
%%%                                    inputs, swatches
%%%
%%% Both are value-bearing: the root carries `data-ah-value' (the
%%% canonical value, "" when empty), a hidden input carries it under the
%%% `name' taken from Attrs, and the root fires `change' when a value is
%%% committed (the colour picker also fires `input' while dragging).
%%%
%%% By default each renders a field that opens the sigil panel in a popup;
%%% the `inline' flag renders the panel alone, as sigil does.
%%%
%%% Each function builds an element record (#ah_timepicker{},
%%% #ah_colorpicker{}, defined in include/aihtml_form_time_color.hrl) and
%%% render/1 turns it into HTML, so pages may also write the records
%%% directly (designs/05-records.md).
%%%
%%% Values are validated when rendering: a malformed value raises
%%% `error({aihtml, {bad_value, Component, Value}})'; `undefined' and
%%% `<<>>' mean empty.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_form_time_color).
-behaviour(aihtml_element).

-include("aihtml_form_time_color.hrl").

-export([timepicker/3, colorpicker/3,
         normalize_time/1, normalize_color/2,
         render/1, fields/1, catalog/0]).

-export_type([element/0]).

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

-type time() :: {0..23, 0..59}.
-type element() :: #ah_timepicker{} | #ah_colorpicker{}.

%%%===================================================================
%%% Builders
%%%===================================================================

%% @doc A time picker. `Value' is `<<"HH:MM">>' (24h), `<<"HH:MM:SS">>'
%% (seconds are dropped: sigil picks minutes), `{H, M}', `{H, M, S}',
%% `undefined' or `<<>>'. The value is always written as "HH:MM".
%%
%% Options (taken from Attrs): `format' (`'12h'' default, or `'24h''),
%% `minute_step' (default 5, sigil's minute-interval), `auto_switch'
%% (default true: go to minutes after an hour is picked), `min', `max'
%% (times, same forms as Value; out-of-range numbers are disabled),
%% `placeholder', `footer' (html under the clock).
-spec timepicker(binary() | string() | tuple() | undefined,
                 aihtml_html:css(), aihtml_html:attrs()) -> #ah_timepicker{}.
timepicker(Value, Css, Attrs) ->
    build(#ah_timepicker{value = Value}, Css, Attrs).

%% @doc A colour picker. `Value' is `<<"#RRGGBB">>' (also without `#' or
%% in the 3-digit form), `{R, G, B}', `undefined' or `<<>>'; with the
%% `alpha' flag also `<<"#RRGGBBAA">>', `#RGBA' or `{R, G, B, A}' (0..255).
%% The value is written lowercase as "#rrggbb", or "#rrggbbaa" when alpha
%% is below ff.
%%
%% Options: `swatches' (a list of colours shown under the inputs),
%% `placeholder' (trigger text when empty), `width', `height' (sigil's
%% sizes, pixels or a CSS length), `clear_label' (default "Clear").
-spec colorpicker(binary() | string() | tuple() | undefined,
                  aihtml_html:css(), aihtml_html:attrs()) -> #ah_colorpicker{}.
colorpicker(Value, Css, Attrs) ->
    build(#ah_colorpicker{value = Value}, Css, Attrs).

build(R, Css, Attrs) ->
    Tag = element(1, R),
    ?E:build(R, fields(Tag), entry(?E:component_name(Tag)), Css, Attrs).

%% @doc The field names of one of this group's records.
-spec fields(atom()) -> [atom()].
fields(ah_timepicker) -> record_info(fields, ah_timepicker);
fields(ah_colorpicker) -> record_info(fields, ah_colorpicker).

%%%===================================================================
%%% Rendering
%%%===================================================================

-spec render(element()) -> aihtml_html:html().
render(#ah_timepicker{value = Value0, name = Name, inline = Inline, disabled = Disabled,
                      clearable = Clearable} = T) ->
    Classes = classes(T),
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
           ?E:root_attrs(T, change)]);

render(#ah_colorpicker{value = Value0, name = Name, inline = Inline, disabled = Disabled,
                       alpha = Alpha} = C) ->
    Classes = classes(C),
    Value = normalize_color(Value0, Alpha),
    ValueBin = color_bin(Value),
    {R, G, B, A} = case Value of undefined -> {255, 0, 0, 255}; _ -> Value end,
    Swatches = [normalize_color(S, Alpha) || S <- C#ah_colorpicker.swatches],
    [error({aihtml, {bad_option, colorpicker, swatches, S}}) || S <- Swatches, S =:= undefined],
    Panel = color_panel({R, G, B, A}, Value, Swatches, C),
    Body = case Inline of
               true -> Panel;
               false ->
                   Placeholder = C#ah_colorpicker.placeholder,
                   [?H:el(button,
                          [?H:el(span, [], [<<"ah-colorpicker-trigger-swatch">>,
                                            [<<"ah-colorpicker-trigger-empty">>
                                             || Value =:= undefined]],
                                 [{style, swatch_style(Value)}]),
                           ?H:el(span, case Value of
                                           undefined -> Placeholder;
                                           _ -> ValueBin
                                       end,
                                 [<<"ah-colorpicker-trigger-text">>], [])],
                          [<<"ah-colorpicker-trigger">>],
                          [{type, button}, {aria_haspopup, dialog},
                           {aria_expanded, <<"false">>}, {disabled, Disabled},
                           {data_placeholder, Placeholder}]),
                    ?H:el('div', Panel, [<<"ah-colorpicker-popup">>],
                          [{role, dialog}, {aria_label, <<"Choose color">>}, {hidden, true}])]
           end,
    ?H:el('div', [Body, hidden_input(Name, ValueBin, Disabled)],
          Classes,
          [[{data_ah, <<"colorpicker">>}, {data_ah_value, ValueBin},
            {data_alpha, Alpha andalso <<"true">>},
            {aria_disabled, Disabled andalso <<"true">>}],
           ?E:root_attrs(C, change)]).

classes(R) ->
    Tag = element(1, R),
    ?E:classes(R, fields(Tag), entry(?E:component_name(Tag))).

%%%===================================================================
%%% timepicker
%%%===================================================================

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

%% View data as built by form_time_color.js tpHeader (hours mode).
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

%% The clock in hours mode; aihtml.js redraws the numbers on mode changes.
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

%%%===================================================================
%%% colorpicker
%%%===================================================================

%% @doc Normalise a colour to `{R, G, B, A}' or `undefined'. `Alpha' says
%% whether an alpha channel is accepted.
-spec normalize_color(term(), boolean()) ->
          {0..255, 0..255, 0..255, 0..255} | undefined.
normalize_color(undefined, _) -> undefined;
normalize_color(<<>>, _) -> undefined;
normalize_color({R, G, B} = C, Alpha) ->
    try normalize_color({R, G, B, 255}, Alpha)
    catch error:{aihtml, {bad_value, colorpicker, _}} -> bad_color(C)
    end;
normalize_color({R, G, B, A} = C, Alpha) ->
    case lists:all(fun(X) -> is_integer(X) andalso X >= 0 andalso X =< 255 end,
                   [R, G, B, A]) andalso (Alpha orelse A =:= 255) of
        true -> C;
        false -> bad_color(C)
    end;
normalize_color(L, Alpha) when is_list(L) ->
    case io_lib:printable_unicode_list(L) of
        true -> normalize_color(unicode:characters_to_binary(L), Alpha);
        false -> bad_color(L)
    end;
normalize_color(Bin, Alpha) when is_binary(Bin) ->
    Hex = case string:trim(Bin) of
              <<"#", Rest/binary>> -> Rest;
              Rest -> Rest
          end,
    Ok = re:run(Hex, <<"^[0-9a-fA-F]+$">>, [{capture, none}]) =:= match,
    case {Ok, byte_size(Hex)} of
        {true, 3} -> expand(Hex, Bin, Alpha);
        {true, 4} when Alpha -> expand(Hex, Bin, Alpha);
        {true, 6} -> hex_channels(Hex);
        {true, 8} when Alpha -> hex_channels(Hex);
        _ -> bad_color(Bin)
    end;
normalize_color(Other, _) -> bad_color(Other).

expand(Hex, _Orig, _Alpha) ->
    hex_channels(<< <<C, C>> || <<C>> <= Hex >>).

hex_channels(<<R:2/binary, G:2/binary, B:2/binary>>) ->
    {hex(R), hex(G), hex(B), 255};
hex_channels(<<R:2/binary, G:2/binary, B:2/binary, A:2/binary>>) ->
    {hex(R), hex(G), hex(B), hex(A)}.

hex(B) -> binary_to_integer(B, 16).

-spec bad_color(term()) -> no_return().
bad_color(V) -> error({aihtml, {bad_value, colorpicker, V}}).

color_bin(undefined) -> <<>>;
color_bin({R, G, B, 255}) -> <<"#", (hex2(R))/binary, (hex2(G))/binary, (hex2(B))/binary>>;
color_bin({R, G, B, A}) -> <<(color_bin({R, G, B, 255}))/binary, (hex2(A))/binary>>.

hex2(N) ->
    string:lowercase(iolist_to_binary(io_lib:format("~2.16.0B", [N]))).

swatch_style(undefined) -> undefined;
swatch_style({R, G, B, A}) ->
    <<"--ah-cp-swatch: rgba(", (integer_to_binary(R))/binary, ",",
      (integer_to_binary(G))/binary, ",", (integer_to_binary(B))/binary, ",",
      (num(A / 255))/binary, ")">>.

%% sigil colorpicker/color rgb->hsv: {H 0..360, S 0..100, V 0..100}, rounded.
rgb_to_hsv(R0, G0, B0) ->
    [R, G, B] = [X / 255 || X <- [R0, G0, B0]],
    Max = lists:max([R, G, B]),
    Min = lists:min([R, G, B]),
    D = Max - Min,
    H0 = if D == 0 -> 0.0;
            Max == R -> 60 * fmod((G - B) / D, 6);
            Max == G -> 60 * ((B - R) / D + 2);
            true -> 60 * ((R - G) / D + 4)
         end,
    H = case H0 < 0 of true -> H0 + 360; false -> H0 end,
    S = case Max == 0 of true -> 0.0; false -> D / Max * 100 end,
    {round(H) rem 360, round(S), round(Max * 100)}.

fmod(X, Y) ->
    M = math:fmod(X, Y),
    case M < 0 of true -> M + Y; false -> M end.

%% The sigil panel plus aihtml's alpha bar and swatches.
color_panel({R, G, B, A}, Value, Swatches,
            #ah_colorpicker{alpha = Alpha, disabled = Disabled} = C) ->
    {H, S, V} = rgb_to_hsv(R, G, B),
    Bright = 0.299 * R + 0.587 * G + 0.114 * B > 150,
    Size = fun(undefined) -> undefined;
              (N) when is_integer(N) -> <<(integer_to_binary(N))/binary, "px">>;
              (L) -> iolist_to_binary(L)
           end,
    Height = Size(C#ah_colorpicker.height),
    HeightStyle = case Height of undefined -> undefined;
                      _ -> <<"height: ", Height/binary>>
                  end,
    Style = iolist_to_binary(lists:join(<<"; ">>,
              [<<"width: ", W/binary>> || W <- [Size(C#ah_colorpicker.width)], W =/= undefined])),
    Hex6 = color_bin({R, G, B, 255}),
    Map = ?H:el('div',
                [?H:el('div', [], [<<"ah-colorpicker-map-overlay">>], []),
                 ?H:el('div', [], [<<"ah-colorpicker-map-pointer">>,
                                   case Bright of
                                       true -> <<"ah-colorpicker-map-pointer-dark">>;
                                       false -> <<"ah-colorpicker-map-pointer-light">>
                                   end],
                       [{style, <<"left: ", (integer_to_binary(S))/binary, "%; top: ",
                                  (integer_to_binary(100 - V))/binary, "%">>}])],
                [<<"ah-colorpicker-map">>],
                [{style, iolist_to_binary(
                           [<<"background-color: hsl(">>, integer_to_binary(H),
                            <<", 100%, 50%)">>,
                            [[<<"; ">>, HeightStyle] || HeightStyle =/= undefined]])},
                 {role, slider}, {tabindex, 0},
                 {aria_label, <<"Saturation and brightness">>},
                 {aria_valuemin, 0}, {aria_valuemax, 100}, {aria_valuenow, S},
                 {aria_valuetext, sv_text(S, V)}]),
    Bar = ?H:el('div', ?H:el('div', [], [<<"ah-colorpicker-bar-pointer">>],
                             [{style, <<"top: ", (num(H / 360 * 100))/binary, "%">>}]),
                [<<"ah-colorpicker-bar">>],
                [{style, HeightStyle}, {role, slider}, {tabindex, 0},
                 {aria_label, <<"Hue">>}, {aria_orientation, vertical},
                 {aria_valuemin, 0}, {aria_valuemax, 360}, {aria_valuenow, H}]),
    AlphaPct = round(A / 255 * 100),
    AlphaBar = [?H:el('div', ?H:el('div', [], [<<"ah-colorpicker-bar-pointer">>],
                                   [{style, <<"top: ", (integer_to_binary(100 - AlphaPct))/binary,
                                              "%">>}]),
                      [<<"ah-colorpicker-bar">>, <<"ah-colorpicker-alpha">>],
                      [{style, iolist_to_binary(
                                 [<<"--ah-cp-rgb: ">>, Hex6,
                                  [[<<"; ">>, HeightStyle] || HeightStyle =/= undefined]])},
                       {role, slider}, {tabindex, 0}, {aria_label, <<"Alpha">>},
                       {aria_orientation, vertical},
                       {aria_valuemin, 0}, {aria_valuemax, 100}, {aria_valuenow, AlphaPct},
                       {aria_valuetext, <<(integer_to_binary(AlphaPct))/binary, "%">>}])
                || Alpha],
    Field = fun(Cls, Val, Label, Extra) ->
                    ?H:void(input, [<<"ah-colorpicker-field">>, Cls],
                            [[{value, Val}, {aria_label, Label}, {disabled, Disabled}],
                             Extra])
            end,
    Rgb = fun(L, Cls, Val, Label) ->
                  ?H:el(label, [L, Field(Cls, integer_to_binary(Val), Label,
                                         [{type, number}, {min, 0}, {max, 255}])],
                        [<<"ah-colorpicker-label">>], [])
          end,
    Inputs = case C#ah_colorpicker.no_inputs of
                 true -> [];
                 false ->
                     HexText = case Alpha andalso A < 255 of
                                   true -> <<(binary:part(Hex6, 1, 6))/binary, (hex2(A))/binary>>;
                                   false -> binary:part(Hex6, 1, 6)
                               end,
                     ?H:el('div',
                           [?H:el('div',
                                  [[?H:el('div', [], [<<"ah-colorpicker-preview">>],
                                          [{style, <<"background-color: ",
                                                     (color_bin({R, G, B, A}))/binary>>}])
                                    || not C#ah_colorpicker.no_preview],
                                   ?H:el(span, <<"#">>, [<<"ah-colorpicker-hash">>], []),
                                   Field(<<"ah-colorpicker-hex-input">>, HexText, <<"Hex">>,
                                         [{type, text}, {spellcheck, <<"false">>},
                                          {maxlength, case Alpha of true -> 8; false -> 6 end}])],
                                  [<<"ah-colorpicker-hex">>], []),
                            ?H:el('div',
                                  [Rgb(<<"R">>, <<"ah-colorpicker-r-input">>, R, <<"Red">>),
                                   Rgb(<<"G">>, <<"ah-colorpicker-g-input">>, G, <<"Green">>),
                                   Rgb(<<"B">>, <<"ah-colorpicker-b-input">>, B, <<"Blue">>),
                                   [?H:el(label, [<<"A">>,
                                                  Field(<<"ah-colorpicker-a-input">>,
                                                        integer_to_binary(AlphaPct), <<"Alpha">>,
                                                        [{type, number}, {min, 0}, {max, 100}])],
                                          [<<"ah-colorpicker-label">>], []) || Alpha]],
                                  [<<"ah-colorpicker-rgb">>], [])],
                           [<<"ah-colorpicker-inputs">>], [])
             end,
    Current = color_bin(case Value of undefined -> undefined; _ -> {R, G, B, A} end),
    SwatchRow = case Swatches of
                    [] -> [];
                    _ -> ?H:el('div',
                               [?H:el(button, [], [<<"ah-colorpicker-swatch">>],
                                      [{type, button}, {style, swatch_style(Sw)},
                                       {data_color, color_bin(Sw)},
                                       {aria_label, color_bin(Sw)}, {title, color_bin(Sw)},
                                       {aria_pressed, atom_to_binary(color_bin(Sw) =:= Current,
                                                                     utf8)},
                                       {disabled, Disabled}])
                                || Sw <- Swatches],
                               [<<"ah-colorpicker-swatches">>],
                               [{role, group}, {aria_label, <<"Swatches">>}])
                end,
    Clear = [?H:el('div', ?H:el(a, C#ah_colorpicker.clear_label, [],
                                [{href, <<"#">>}, {role, button}]),
                   [<<"ah-colorpicker-transparent">>], [])
             || C#ah_colorpicker.clearable],
    ?H:el('div',
          [?H:el('div', [Map, Bar, AlphaBar], [<<"ah-colorpicker-body">>], []),
           Inputs, SwatchRow, Clear],
          [<<"ah-colorpicker">>, [<<"ah-colorpicker-disabled">> || Disabled]],
          [{role, application}, {aria_label, <<"Color Picker">>},
           {aria_disabled, Disabled andalso <<"true">>},
           {style, case Style of <<>> -> undefined; _ -> Style end}]).

sv_text(S, V) ->
    <<"Saturation ", (integer_to_binary(S))/binary, "%, brightness ",
      (integer_to_binary(V))/binary, "%">>.

%%%===================================================================
%%% Shared
%%%===================================================================

hidden_input(Name, Value, Disabled) ->
    ?H:void(input, [], [{type, hidden}, {name, Name}, {value, Value},
                        {disabled, Disabled}]).

clear_button(Cls, Hidden) ->
    ?H:el(button, {safe, <<"&times;">>}, [Cls],
          [{type, button}, {tabindex, <<"-1">>}, {aria_label, <<"Clear">>},
           {hidden, Hidden}]).

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => timepicker, category => form,
       signature => <<"timepicker(Value, Css, Attrs)">>,
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
                "field, or inline. Value \"HH:MM\".">>},
     #{name => colorpicker, category => form,
       signature => <<"colorpicker(Value, Css, Attrs)">>,
       root => <<"ah-colorpicker-field">>,
       flags => [inline, disabled, clearable, alpha, no_inputs, no_preview],
       classes => #{alpha => [], no_inputs => [], no_preview => []},
       options => [swatches, placeholder, width, height, clear_label],
       behavior => <<"colorpicker">>,
       events => [<<"input">>, <<"change">>],
       option_docs =>
           #{inline => <<"Render the panel in place instead of a trigger with a popup.">>,
             disabled => <<"No interaction; the hidden input is disabled too.">>,
             clearable => <<"A link under the panel that empties the value.">>,
             alpha => <<"Alpha bar and input; the value may be #rrggbbaa.">>,
             no_inputs => <<"Hide the hex and RGB inputs.">>,
             no_preview => <<"Hide the preview square beside the hex input.">>,
             swatches => <<"List of preset colours shown under the inputs.">>,
             placeholder => <<"Trigger text when empty (default \"No color\").">>,
             width => <<"Panel width: pixels or a CSS length.">>,
             height => <<"Height of the colour area and bars: pixels or a CSS length.">>,
             clear_label => <<"Text of the clear link (default \"Clear\").">>},
       methods =>
           [#{name => getValue, args => <<"()">>, doc => <<"Return the hex value or \"\".">>},
            #{name => setValue, args => <<"(Hex)">>,
              doc => <<"Set a colour (or \"\" to clear) without firing events.">>},
            #{name => clear, args => <<"()">>, doc => <<"Empty the value and fire change.">>},
            #{name => open, args => <<"()">>, doc => <<"Open the popup.">>},
            #{name => close, args => <<"()">>, doc => <<"Close the popup.">>}],
       doc => <<"HSV colour picker: saturation/value area, hue and alpha bars, hex and "
                "RGB inputs, swatches; in a popup field or inline. Value \"#rrggbb[aa]\".">>}].

%%%===================================================================
%%% Internal
%%%===================================================================

entry(Name) -> aihtml_catalog:entry(?MODULE, Name).
