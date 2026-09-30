%%%-------------------------------------------------------------------
%%% @doc An HSV colour picker, ported from sigil (form/colorpicker). See
%%% designs/04-components.md.
%%%
%%%   ah_colorpicker(Value, Css, Attrs)   saturation/value area, hue bar,
%%%                                       optional alpha bar, hex and RGB
%%%                                       inputs, swatches
%%%
%%% Value-bearing: the root carries `data-ah-value' (the canonical value,
%%% "" when empty), a hidden input carries it under the `name' taken from
%%% Attrs, and the root fires `change' when a value is committed (and
%%% `input' while dragging).
%%%
%%% By default it renders a trigger that opens the sigil panel in a popup;
%%% the `inline' flag renders the panel alone, as sigil does. The
%%% behaviour is `colorpicker' (assets/js/components/colorpicker.ts).
%%%
%%% ah_colorpicker/3 builds an #ah_colorpicker{} (include/aihtml_colorpicker.hrl)
%%% and render/1 turns it into HTML, so pages may also write the record
%%% directly (designs/05-records.md).
%%%
%%% Values are validated when rendering: a malformed value raises
%%% `error({aihtml, {bad_value, colorpicker, Value}})'; `undefined' and
%%% `<<>>' mean empty.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_colorpicker).
-behaviour(aihtml_element).

-include("aihtml_colorpicker.hrl").

-export([ah_colorpicker/3, normalize_color/2, render/1, fields/1, catalog/0]).

-export_type([color/0, css_length/0, element/0]).

-define(H, aihtml_html).
-define(E, aihtml_element).

%% "#rrggbb[aa]", "#rgb[a]" (the # is optional), {R, G, B}, {R, G, B, A};
%% undefined or <<>> is empty.
-type color() :: undefined | binary() | string()
               | {integer(), integer(), integer()}
               | {integer(), integer(), integer(), integer()}.
%% Pixels or a CSS length.
-type css_length() :: undefined | non_neg_integer() | iodata().
-type element() :: #ah_colorpicker{}.

%% @doc A colour picker. `Value' is `<<"#RRGGBB">>' (also without `#' or
%% in the 3-digit form), `{R, G, B}', `undefined' or `<<>>'; with the
%% `alpha' flag also `<<"#RRGGBBAA">>', `#RGBA' or `{R, G, B, A}' (0..255).
%% The value is written lowercase as "#rrggbb", or "#rrggbbaa" when alpha
%% is below ff.
%%
%% Options: `swatches' (a list of colours shown under the inputs),
%% `placeholder' (trigger text when empty), `width', `height' (sigil's
%% sizes, pixels or a CSS length), `clear_label' (default "Clear").
-spec ah_colorpicker(binary() | string() | tuple() | undefined,
                     aihtml_html:css(), aihtml_html:attrs()) -> #ah_colorpicker{}.
ah_colorpicker(Value, Css, Attrs) ->
    ?E:build(?MODULE, #ah_colorpicker{value = Value}, Css, Attrs).

%% @doc The field names of #ah_colorpicker{}.
-spec fields(atom()) -> [atom()].
fields(ah_colorpicker) -> record_info(fields, ah_colorpicker).

-spec render(element()) -> aihtml_html:html().
render(#ah_colorpicker{value = Value0, name = Name, inline = Inline, disabled = Disabled,
                       alpha = Alpha} = C) ->
    Classes = ?E:classes(?MODULE, C),
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
                          [{role, dialog}, {aria_label, aihtml_i18n:text(colorpicker, dialog)}, {hidden, true}])]
           end,
    ?H:el('div', [Body, hidden_input(Name, ValueBin, Disabled)],
          Classes,
          [[{data_ah, <<"colorpicker">>}, {data_ah_value, ValueBin},
            {data_alpha, Alpha andalso <<"true">>},
            {aria_disabled, Disabled andalso <<"true">>}],
           ?E:root_attrs(C, change)]).

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
                 {aria_label, aihtml_i18n:text(colorpicker, saturation)},
                 {aria_valuemin, 0}, {aria_valuemax, 100}, {aria_valuenow, S},
                 {aria_valuetext, sv_text(S, V)}]),
    Bar = ?H:el('div', ?H:el('div', [], [<<"ah-colorpicker-bar-pointer">>],
                             [{style, <<"top: ", (num(H / 360 * 100))/binary, "%">>}]),
                [<<"ah-colorpicker-bar">>],
                [{style, HeightStyle}, {role, slider}, {tabindex, 0},
                 {aria_label, aihtml_i18n:text(colorpicker, hue)}, {aria_orientation, vertical},
                 {aria_valuemin, 0}, {aria_valuemax, 360}, {aria_valuenow, H}]),
    AlphaPct = round(A / 255 * 100),
    AlphaBar = [?H:el('div', ?H:el('div', [], [<<"ah-colorpicker-bar-pointer">>],
                                   [{style, <<"top: ", (integer_to_binary(100 - AlphaPct))/binary,
                                              "%">>}]),
                      [<<"ah-colorpicker-bar">>, <<"ah-colorpicker-alpha">>],
                      [{style, iolist_to_binary(
                                 [<<"--ah-cp-rgb: ">>, Hex6,
                                  [[<<"; ">>, HeightStyle] || HeightStyle =/= undefined]])},
                       {role, slider}, {tabindex, 0}, {aria_label, aihtml_i18n:text(colorpicker, alpha)},
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
                                   Field(<<"ah-colorpicker-hex-input">>, HexText, aihtml_i18n:text(colorpicker, hex),
                                         [{type, text}, {spellcheck, <<"false">>},
                                          {maxlength, case Alpha of true -> 8; false -> 6 end}])],
                                  [<<"ah-colorpicker-hex">>], []),
                            ?H:el('div',
                                  [Rgb(<<"R">>, <<"ah-colorpicker-r-input">>, R, aihtml_i18n:text(colorpicker, red)),
                                   Rgb(<<"G">>, <<"ah-colorpicker-g-input">>, G, aihtml_i18n:text(colorpicker, green)),
                                   Rgb(<<"B">>, <<"ah-colorpicker-b-input">>, B, aihtml_i18n:text(colorpicker, blue)),
                                   [?H:el(label, [<<"A">>,
                                                  Field(<<"ah-colorpicker-a-input">>,
                                                        integer_to_binary(AlphaPct), aihtml_i18n:text(colorpicker, alpha),
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
                               [{role, group}, {aria_label, aihtml_i18n:text(colorpicker, swatches)}])
                end,
    Clear = [?H:el('div', ?H:el(a, C#ah_colorpicker.clear_label, [],
                                [{href, <<"#">>}, {role, button}]),
                   [<<"ah-colorpicker-transparent">>], [])
             || C#ah_colorpicker.clearable],
    ?H:el('div',
          [?H:el('div', [Map, Bar, AlphaBar], [<<"ah-colorpicker-body">>], []),
           Inputs, SwatchRow, Clear],
          [<<"ah-colorpicker">>, [<<"ah-colorpicker-disabled">> || Disabled]],
          [{role, application}, {aria_label, aihtml_i18n:text(colorpicker, picker)},
           {aria_disabled, Disabled andalso <<"true">>},
           {style, case Style of <<>> -> undefined; _ -> Style end}]).

sv_text(S, V) ->
    <<"Saturation ", (integer_to_binary(S))/binary, "%, brightness ",
      (integer_to_binary(V))/binary, "%">>.

num(F) ->
    R = round(F * 100) / 100,
    case R == trunc(R) of
        true -> integer_to_binary(trunc(R));
        false -> float_to_binary(R, [{decimals, 2}, compact])
    end.

hidden_input(Name, Value, Disabled) ->
    ?H:void(input, [], [{type, hidden}, {name, Name}, {value, Value},
                        {disabled, Disabled}]).

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => colorpicker, category => form,
       signature => <<"ah_colorpicker(Value, Css, Attrs)">>,
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
