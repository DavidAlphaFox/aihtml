%%%-------------------------------------------------------------------
%%% @doc Entry components, ported from sigil (form/masked_input,
%%% form/formatted_input, form/range_selector, form/repeat_button). See
%%% designs/04-components.md.
%%%
%%%   masked_input(Value, Css, Attrs)            a text field with an input mask
%%%   formatted_input(Value, Css, Attrs)         an integer in radix 2, 8, 10 or 16
%%%   range_selector(Range, Value, Css, Attrs)   a {Lo, Hi} range on a ticked track
%%%   repeat_button(Content, Value, Css, Attrs)  a button that repeats its click
%%%
%%% The first three are value-bearing components: `Attrs' go to the root,
%%% which carries `data-ah-value' and fires `change' (and `input' while
%%% the value is being edited); `name' goes to a hidden input. The server
%%% renders the complete first state (the masked text, the number in its
%%% radix, the track with its ticks, labels and markers), so nothing needs
%%% to be laid out in the browser before it is shown. The behaviours live
%%% in assets/js/components/form_entry.js.
%%%
%%% repeat_button renders the markup of `aihtml_form_buttons:button/4'
%%% and fires `click' on press and then repeatedly while held, so
%%% `on(click, ...)' or a postback runs once per repetition.
%%%
%%% Each component function builds an element record (#ah_masked_input{}
%%% ..., defined in include/aihtml_form_entry.hrl) and render/1 turns it
%%% into HTML, so pages may also write the records directly
%%% (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_form_entry).
-behaviour(aihtml_element).

-include("aihtml_form_entry.hrl").
-include("aihtml_form_buttons.hrl").

-export([masked_input/3, formatted_input/3, range_selector/4, repeat_button/4,
         render/1, fields/1, catalog/0]).

-export_type([element/0, format/0]).

-define(H, aihtml_html).
-define(E, aihtml_element).

-type element() :: #ah_masked_input{} | #ah_formatted_input{} | #ah_range_selector{}
                 | #ah_repeat_button{}.
-type format() :: ah_entry_format().

-define(MONTHS, [<<"Jan">>, <<"Feb">>, <<"Mar">>, <<"Apr">>, <<"May">>, <<"Jun">>,
                 <<"Jul">>, <<"Aug">>, <<"Sep">>, <<"Oct">>, <<"Nov">>, <<"Dec">>]).
-define(RADIXES, [{2, <<"BIN">>, <<"Binary">>}, {8, <<"OCT">>, <<"Octal">>},
                  {10, <<"DEC">>, <<"Decimal">>}, {16, <<"HEX">>, <<"Hexadecimal">>}]).

%%%===================================================================
%%% masked_input
%%%===================================================================

%% @doc A text field with an input mask. The mask characters are
%% `9' / `0' (a digit), `#' (a digit, + or -), `A' / `a' (a letter or
%% digit), `L' / `l' (a letter), `c' / `C' (any character) and `[...]' (a
%% regular expression class such as `[0-9A-F]'); anything else is a literal.
%% `Value' is the characters to type into the editable positions, in
%% order (literals in it are skipped).
%%
%% Css: `disabled', `readonly', `square' (no rounded corners),
%% `floating_label' (the placeholder floats above the field).
%% Options: `mask' (default "99999"), `prompt_char' (default "_"),
%% `placeholder', `include_literals' (the value keeps the literals).
-spec masked_input(unicode:chardata() | undefined, aihtml_html:css(), aihtml_html:attrs()) ->
          #ah_masked_input{}.
masked_input(Value, Css, Attrs) ->
    build(#ah_masked_input{value = Value}, Css, Attrs).

render_masked(#ah_masked_input{value = Value0, name = Name, disabled = Disabled,
                               readonly = Readonly, floating_label = Floating,
                               mask = Mask0, prompt_char = Prompt0,
                               placeholder = Placeholder} = R) ->
    Classes = classes(R),
    Mask = text(Mask0),
    Prompt = case text(Prompt0) of
                 <<P/utf8>> -> <<P/utf8>>;
                 Bad -> error({aihtml, {bad_option, prompt_char, Bad}})
             end,
    Items = fill(parse_mask(Mask), chars(text(Value0))),
    Display = display(Items, Prompt),
    Raw = edit_value(Items),
    Value = case R#ah_masked_input.include_literals of
                true when Raw =/= <<>> -> Display;
                true -> <<>>;
                false -> Raw
            end,
    Numeric = re:run(Mask, <<"^[9#0\\[\\]\\-\\(\\)\\s]+$">>, [unicode]) =/= nomatch,
    ?H:el('div',
          [?H:void(input, [<<"ah-masked-input">>],
                   [{type, text},
                    {placeholder, case Floating of true -> <<>>; false -> text(Placeholder) end},
                    {autocomplete, off}, {spellcheck, <<"false">>},
                    {autocorrect, off}, {autocapitalize, off},
                    {inputmode, Numeric andalso numeric},
                    %% an empty floating-label field shows the label, not the mask
                    {value, case Floating andalso Raw =:= <<>> of
                                true -> <<>>;
                                false -> Display
                            end},
                    {disabled, Disabled}, {readonly, Readonly},
                    {aria_label, text(Placeholder) =/= <<>> andalso not Floating
                                     andalso text(Placeholder)}]),
           [?H:el(label, text(Placeholder),
                  [<<"ah-masked-input-label">>,
                   [<<"ah-masked-input-label-float">> || Raw =/= <<>>]], [])
            || Floating],
           hidden(Name, Value)],
          Classes,
          [[{data_ah, <<"masked-input">>}, {data_ah_value, Value},
            {data_ah_mask, Mask}, {data_ah_prompt, Prompt},
            {data_ah_literals, R#ah_masked_input.include_literals},
            {aria_disabled, Disabled andalso <<"true">>}],
           ?E:root_attrs(R, change)]).

%% [{edit, Regex, Char | undefined} | {lit, Char}], as the JS parses it.
parse_mask(<<"[", Rest/binary>>) ->
    case binary:split(Rest, <<"]">>) of
        [Class, More] -> [{edit, <<"([", Class/binary, "])">>, undefined} | parse_mask(More)];
        [_] -> error({aihtml, {bad_option, mask, <<"[", Rest/binary>>}})
    end;
parse_mask(<<C/utf8, Rest/binary>>) ->
    Item = case C of
               $9 -> {edit, <<"\\d">>, undefined};
               $0 -> {edit, <<"\\d">>, undefined};
               $# -> {edit, <<"[\\d|+|-]">>, undefined};
               $A -> {edit, <<"\\w">>, undefined};
               $a -> {edit, <<"\\w">>, undefined};
               $L -> {edit, <<"[a-zA-Z]">>, undefined};
               $l -> {edit, <<"[a-zA-Z]">>, undefined};
               $c -> {edit, <<".">>, undefined};
               $C -> {edit, <<".">>, undefined};
               _ -> {lit, <<C/utf8>>}
           end,
    [Item | parse_mask(Rest)];
parse_mask(<<>>) -> [].

%% Fill the editable positions in order: value characters that do not fit
%% a position are skipped, and a value character equal to the literal at
%% the current position is taken as that literal.
fill([{lit, L} | Items], [L | Cs]) -> [{lit, L} | fill(Items, Cs)];
fill([{lit, _} = I | Items], Cs) -> [I | fill(Items, Cs)];
fill([{edit, Re, _} | Items], Cs) ->
    case lists:dropwhile(fun(C) -> not matches(Re, C) end, Cs) of
        [C | Rest] -> [{edit, Re, C} | fill(Items, Rest)];
        [] -> [{edit, Re, undefined} | Items]
    end;
fill([], _) -> [].

matches(Re, C) ->
    re:run(C, <<"^(?:", Re/binary, ")$">>, [unicode, caseless]) =/= nomatch.

chars(B) -> [<<C/utf8>> || <<C/utf8>> <= B].

display(Items, Prompt) ->
    iolist_to_binary([case I of
                          {lit, L} -> L;
                          {edit, _, undefined} -> Prompt;
                          {edit, _, C} -> C
                      end || I <- Items]).

edit_value(Items) ->
    iolist_to_binary([C || {edit, _, C} <- Items, C =/= undefined]).

%%%===================================================================
%%% formatted_input
%%%===================================================================

%% @doc An integer field in binary, octal, decimal or hexadecimal, as in
%% sigil (values of any size: BigInt in the browser). `Value' is the
%% decimal value, an integer or its text; `data-ah-value' is decimal
%% whatever the radix shown. Arrow keys and the spin buttons add or
%% subtract `spin_step'; the arrow button opens a menu of radixes.
%%
%% Css: `disabled'. Options: `radix' (2, 8, 10 (default) or 16), `min',
%% `max', `upper_case' (hex digits), `spin_buttons' (default true),
%% `spin_step' (default 1), `drop_down' (the radix menu, default true),
%% `drop_down_width' (px), `notation' (default | exponential, decimal
%% only), `placeholder'.
-spec formatted_input(ah_entry_integer(), aihtml_html:css(), aihtml_html:attrs()) ->
          #ah_formatted_input{}.
formatted_input(Value, Css, Attrs) ->
    build(#ah_formatted_input{value = Value}, Css, Attrs).

render_formatted(#ah_formatted_input{value = Value0, name = Name, disabled = Disabled,
                                     radix = Radix, upper_case = Upper,
                                     notation = Notation} = R0) ->
    {Id, R} = ensure_id(R0),
    Classes = classes(R),
    lists:keymember(Radix, 1, ?RADIXES) orelse error({aihtml, {bad_option, radix, Radix}}),
    lists:member(Notation, [default, exponential])
        orelse error({aihtml, {bad_option, notation, Notation}}),
    Min = int_opt(min, R#ah_formatted_input.min),
    Max = int_opt(max, R#ah_formatted_input.max),
    Step = int_opt(spin_step, R#ah_formatted_input.spin_step),
    Value = clamp(int_opt(value, Value0), Min, Max),
    Dec = integer_to_binary(Value),
    Display = radix_text(Value, Radix, Upper, Notation),
    Spin = R#ah_formatted_input.spin_buttons,
    Drop = R#ah_formatted_input.drop_down,
    ListId = <<Id/binary, "-radix">>,
    Popup = ?H:el('div',
                  [?H:el('div',
                         [?H:el(span, Label, [<<"ah-fmt-popup-item-label">>], []),
                          ?H:el(span, Desc, [<<"ah-fmt-popup-item-desc">>], [])],
                         [<<"ah-fmt-popup-item">>, [<<"ah-fmt-popup-item-active">> || X =:= Radix]],
                         [{role, option}, {id, <<ListId/binary, "-", (integer_to_binary(X))/binary>>},
                          {aria_selected, atom_to_binary(X =:= Radix)},
                          {data_radix, integer_to_binary(X)}])
                   || {X, Label, Desc} <- ?RADIXES],
                  [<<"ah-fmt-popup">>],
                  [{id, ListId}, {role, listbox}, {aria_label, <<"Radix">>},
                   {style, case R#ah_formatted_input.drop_down_width of
                               W when is_integer(W) ->
                                   [<<"width:">>, integer_to_binary(W), <<"px">>];
                               undefined -> undefined
                           end}]),
    ?H:el('div',
          [?H:el('div',
                 [?H:void(input, [<<"ah-fmt-input">>],
                          [{type, text}, {id, <<Id/binary, "-input">>},
                           {autocomplete, off}, {spellcheck, <<"false">>},
                           {placeholder, text(R#ah_formatted_input.placeholder)},
                           {value, Display}, {disabled, Disabled},
                           {role, spinbutton}, {aria_valuenow, Dec},
                           {aria_valuetext, Display},
                           {aria_valuemin, Min =/= undefined andalso integer_to_binary(Min)},
                           {aria_valuemax, Max =/= undefined andalso integer_to_binary(Max)}]),
                  [?H:el('div',
                         [?H:el(span, <<"▲"/utf8>>, [<<"ah-fmt-spin-up">>], []),
                          ?H:el(span, <<"▼"/utf8>>, [<<"ah-fmt-spin-down">>], [])],
                         [<<"ah-fmt-spin-buttons">>], [{aria_hidden, <<"true">>}]) || Spin],
                  [?H:el(span, <<"▼"/utf8>>, [<<"ah-fmt-dropdown-btn">>],
                         [{role, button}, {aria_label, <<"Radix">>},
                          {aria_haspopup, listbox}, {aria_expanded, <<"false">>},
                          {aria_controls, ListId}]) || Drop]],
                 [<<"ah-fmt-input-row">>], []),
           [Popup || Drop],
           hidden(Name, Dec)],
          Classes,
          [[{id, Id}, {data_ah, <<"formatted-input">>}, {data_ah_value, Dec},
            {data_ah_radix, integer_to_binary(Radix)},
            {data_ah_min, Min =/= undefined andalso integer_to_binary(Min)},
            {data_ah_max, Max =/= undefined andalso integer_to_binary(Max)},
            {data_ah_step, integer_to_binary(Step)},
            {data_ah_upper, Upper},
            {data_ah_notation, Notation =:= exponential andalso <<"exponential">>},
            {aria_disabled, Disabled andalso <<"true">>}],
           ?E:root_attrs(R, change)]).

int_opt(_, undefined) -> undefined;
int_opt(_, I) when is_integer(I) -> I;
int_opt(K, B) when is_binary(B); is_list(B) ->
    try binary_to_integer(string:trim(text(B)))
    catch _:_ -> error({aihtml, {bad_option, K, B}})
    end;
int_opt(K, V) -> error({aihtml, {bad_option, K, V}}).

clamp(V, Min, _) when Min =/= undefined, V < Min -> Min;
clamp(V, _, Max) when Max =/= undefined, V > Max -> Max;
clamp(V, _, _) -> V.

%% BigInt.toString(radix), optionally upper case or (decimal) exponential.
radix_text(V, Radix, Upper, Notation) ->
    S = string:lowercase(integer_to_binary(V, Radix)),
    S1 = case Upper of true -> string:uppercase(S); false -> S end,
    case {Notation, Radix} of
        {exponential, 10} ->
            {Sign, Abs} = case S1 of
                              <<"-", A/binary>> -> {<<"-">>, A};
                              A -> {<<>>, A}
                          end,
            case Abs of
                <<D, Rest/binary>> when Rest =/= <<>> ->
                    <<Sign/binary, D, ".", Rest/binary, "e+",
                      (integer_to_binary(byte_size(Rest)))/binary>>;
                _ -> S1
            end;
        _ -> S1
    end.

%%%===================================================================
%%% range_selector
%%%===================================================================

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
-spec range_selector({number(), number()} | {number(), number(), number()},
                     {number(), number()} | undefined,
                     aihtml_html:css(), aihtml_html:attrs()) -> #ah_range_selector{}.
range_selector(Range, Value, Css, Attrs) ->
    build(#ah_range_selector{range = Range, value = Value}, Css, Attrs).

render_range(#ah_range_selector{range = {Min, Max}} = R) ->
    render_range(R#ah_range_selector{range = {Min, Max, 1}});
render_range(#ah_range_selector{range = {Min, Max, Step}, value = Value, name = Name,
                                disabled = Disabled, min_span = Span} = R)
  when is_number(Min), is_number(Max), is_number(Step), Max > Min, Step > 0 ->
    Classes = classes(R),
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
                 Marker(<<"left">>, <<"Minimum">>, Lo),
                 Marker(<<"right">>, <<"Maximum">>, Hi)],
                [<<"ah-range-selector-track">>], []),
           hidden(Name, Val)],
          Classes,
          [[{role, group}, {data_ah, <<"range-selector">>}, {data_ah_value, Val},
            {data_ah_min, num(Min)}, {data_ah_max, num(Max)}, {data_ah_step, num(Step)},
            {data_ah_page, num(case Major > 0 of true -> Major; false -> Step * 10 end)},
            {data_ah_min_span, num(Span)},
            {data_ah_format, iolist_to_binary(json:encode(format_json(MFmt)))},
            {aria_disabled, Disabled andalso <<"true">>}],
           ?E:root_attrs(R, change)]);
render_range(#ah_range_selector{range = Range}) ->
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
    lists:nth(M, ?MONTHS);
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

%%%===================================================================
%%% repeat_button
%%%===================================================================

%% @doc A button that repeats its click while held, as sigil's
%% repeat-button: pressing it (mouse, touch, Enter or Space) fires `click'
%% at once, then every `interval' ms once `delay' ms have passed, until it
%% is released. The browser's own click on release is swallowed, so each
%% repetition is one `click' (one action with `on(click, ...)'). Renders
%% as `aihtml_form_buttons:button/4' with the same modifiers (variant,
%% size, round) and options (icon, img, icon_position).
%% Options: `delay' (ms before repeating, default 300), `interval' (ms
%% between clicks, default 50).
-spec repeat_button(aihtml_html:html(), term(), aihtml_html:css(), aihtml_html:attrs()) ->
          #ah_repeat_button{}.
repeat_button(Content, Value, Css, Attrs) ->
    build(#ah_repeat_button{body = Content, value = Value}, Css, Attrs).

render_repeat(#ah_repeat_button{delay = Delay, interval = Interval} = R) ->
    _ = classes(R),                             % checks the modifier fields
    (is_integer(Delay) andalso Delay >= 0) orelse error({aihtml, {bad_option, delay, Delay}}),
    (is_integer(Interval) andalso Interval > 0)
        orelse error({aihtml, {bad_option, interval, Interval}}),
    #ah_button{id = R#ah_repeat_button.id, css = R#ah_repeat_button.css,
               attrs = [{data_ah, <<"repeat-button">>},
                        {data_ah_delay, integer_to_binary(Delay)},
                        {data_ah_interval, integer_to_binary(Interval)},
                        R#ah_repeat_button.attrs],
               postback = R#ah_repeat_button.postback,
               delegate = R#ah_repeat_button.delegate,
               body = R#ah_repeat_button.body, value = R#ah_repeat_button.value,
               variant = R#ah_repeat_button.variant, size = R#ah_repeat_button.size,
               round = R#ah_repeat_button.round, disabled = R#ah_repeat_button.disabled,
               icon = R#ah_repeat_button.icon, img = R#ah_repeat_button.img,
               icon_position = R#ah_repeat_button.icon_position}.

%%%===================================================================
%%% Records
%%%===================================================================

build(R, Css, Attrs) ->
    Tag = element(1, R),
    ?E:build(R, fields(Tag), entry(?E:component_name(Tag)), Css, Attrs).

%% @doc The field names of one of this group's records.
-spec fields(atom()) -> [atom()].
fields(ah_masked_input) -> record_info(fields, ah_masked_input);
fields(ah_formatted_input) -> record_info(fields, ah_formatted_input);
fields(ah_range_selector) -> record_info(fields, ah_range_selector);
fields(ah_repeat_button) -> record_info(fields, ah_repeat_button).

-spec render(element()) -> aihtml_html:html().
render(#ah_masked_input{} = R) -> render_masked(R);
render(#ah_formatted_input{} = R) -> render_formatted(R);
render(#ah_range_selector{} = R) -> render_range(R);
render(#ah_repeat_button{} = R) -> render_repeat(R).

classes(R) ->
    Tag = element(1, R),
    ?E:classes(R, fields(Tag), entry(?E:component_name(Tag))).

entry(Name) -> aihtml_catalog:entry(?MODULE, Name).

%% The parts of a formatted_input refer to each other by id
%% (aria-controls), so a root without one gets one.
ensure_id(R) ->
    Id = case element(3, R) of
             undefined -> <<"ah-e", (integer_to_binary(erlang:unique_integer([positive])))/binary>>;
             Id0 -> text(Id0)
         end,
    {Id, setelement(3, R, Id)}.

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
    [#{name => masked_input, category => form,
       signature => <<"masked_input(Value, Css, Attrs)">>,
       root => <<"ah-masked-input-group">>,
       flags => [disabled, readonly, square, floating_label],
       classes => #{disabled => [<<"ah-masked-input-disabled">>],
                    readonly => [<<"ah-masked-input-readonly">>],
                    square => [<<"ah-masked-input-no-rounded">>],
                    floating_label => []},
       options => [mask, prompt_char, placeholder, include_literals],
       behavior => <<"masked-input">>,
       events => [<<"input">>, <<"change">>],
       doc => <<"A text field with an input mask (phone numbers, dates, codes); "
                "only characters that fit the mask can be typed or pasted.">>,
       option_docs =>
           #{disabled => <<"Not editable.">>,
             readonly => <<"Shows the value; not editable.">>,
             square => <<"No rounded corners.">>,
             floating_label => <<"The placeholder is a label that floats above the field.">>,
             mask => <<"9 or 0 a digit, # a digit or sign, A/a a letter or digit, L/l a letter, "
                       "c/C any character, [..] a character class; anything else is literal "
                       "(default \"99999\").">>,
             prompt_char => <<"Shown in empty positions (default \"_\").">>,
             placeholder => <<"The field's label (aria-label, or the floating label).">>,
             include_literals => <<"The value keeps the mask's literals, "
                                   "e.g. \"(555) 123-4567\" instead of \"5551234567\".">>},
       methods =>
           [#{name => setValue, args => <<"(Text)">>,
              doc => <<"Fill the mask from Text without firing change.">>},
            #{name => getValue, args => <<"()">>, doc => <<"Return data-ah-value.">>},
            #{name => getMaskedValue, args => <<"()">>,
              doc => <<"Return the text shown, literals and prompt characters included.">>},
            #{name => isComplete, args => <<"()">>,
              doc => <<"Whether every editable position is filled.">>},
            #{name => clear, args => <<"()">>, doc => <<"Empty the field and fire change.">>},
            #{name => setMask, args => <<"(Mask)">>,
              doc => <<"Change the mask, keeping the typed characters that fit.">>},
            #{name => focus, args => <<"()">>, doc => <<"Focus the field.">>}]},
     #{name => formatted_input, category => form,
       signature => <<"formatted_input(Value, Css, Attrs)">>,
       root => <<"ah-fmt-input-group">>,
       flags => [disabled],
       classes => #{disabled => [<<"ah-fmt-input-disabled">>]},
       options => [radix, min, max, upper_case, spin_buttons, spin_step, drop_down,
                   drop_down_width, notation, placeholder],
       behavior => <<"formatted-input">>,
       events => [<<"input">>, <<"change">>, <<"ah:radix-change">>, <<"ah:open">>,
                  <<"ah:close">>],
       doc => <<"An integer field in binary, octal, decimal or hexadecimal, with spin "
                "buttons and a radix menu; the value stays decimal, of any size.">>,
       option_docs =>
           #{disabled => <<"Not editable.">>,
             radix => <<"2, 8, 10 (default) or 16: the radix the number is shown and typed in.">>,
             min => <<"Smallest value (an integer or its decimal text).">>,
             max => <<"Largest value.">>,
             upper_case => <<"Hexadecimal digits in upper case.">>,
             spin_buttons => <<"Show the up / down buttons (default true).">>,
             spin_step => <<"What the buttons and arrow keys add (default 1).">>,
             drop_down => <<"Show the radix menu (default true).">>,
             drop_down_width => <<"Width of the radix menu in px.">>,
             notation => <<"default or exponential (decimal only: 1.2345e+4).">>,
             placeholder => <<"Text of the empty field.">>},
       methods =>
           [#{name => setValue, args => <<"(Decimal)">>,
              doc => <<"Set the value (a number or decimal text) without firing change.">>},
            #{name => getValue, args => <<"()">>, doc => <<"Return data-ah-value (decimal text).">>},
            #{name => setRadix, args => <<"(Radix)">>, doc => <<"Show the value in radix 2, 8, 10 or 16.">>},
            #{name => getRadix, args => <<"()">>, doc => <<"Return the radix shown.">>},
            #{name => open, args => <<"()">>, doc => <<"Open the radix menu.">>},
            #{name => close, args => <<"()">>, doc => <<"Close the radix menu.">>}]},
     #{name => range_selector, category => form,
       signature => <<"range_selector(Range, Value, Css, Attrs)">>,
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
            #{name => getValue, args => <<"()">>, doc => <<"Return [Lo, Hi].">>}]},
     #{name => repeat_button, category => form,
       signature => <<"repeat_button(Content, Value, Css, Attrs)">>,
       root => <<"ah-btn">>,
       groups => #{variant => {[primary, secondary, outlined, success, warning, error,
                                info, default, borderless], primary},
                   size => {[sm, md, lg], md}},
       flags => [round],
       classes => #{md => []},
       options => [icon, img, icon_position, delay, interval],
       behavior => <<"repeat-button">>,
       events => [<<"click">>],
       doc => <<"A button that fires click again and again while it is held, "
                "e.g. to step a value.">>,
       option_docs =>
           #{round => <<"Pill-shaped corners.">>,
             icon => <<"HTML shown beside the text, e.g. a glyph or an SVG.">>,
             img => <<"URL of a 16px image shown beside the text.">>,
             icon_position => <<"Where the icon goes: left (default), right, top or bottom.">>,
             delay => <<"Milliseconds held before the clicks repeat (default 300).">>,
             interval => <<"Milliseconds between repeated clicks (default 50).">>},
       methods =>
           [#{name => stop, args => <<"()">>, doc => <<"Stop repeating (as if released).">>}]}].
