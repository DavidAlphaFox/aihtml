%%%-------------------------------------------------------------------
%%% @doc sigil's Slider (sigil.components.form.slider): a single value or
%%% a {Lo, Hi} range.
%%%
%%%   ah_slider(Range, Value, Css, Attrs)
%%%
%%% The markup and classes are sigil's, so the ported stylesheets under
%%% priv/css/sigil/components apply; behaviour is in
%%% assets/js/components/slider.ts.
%%%
%%% The component function builds an #ah_slider{} record (include/
%%% aihtml_slider.hrl) and render/1 turns it into HTML, so pages may also
%%% write the record directly (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_slider).
-behaviour(aihtml_element).

-include("aihtml_slider.hrl").

-export([ah_slider/4, render/1, fields/1, catalog/0]).

-export_type([range/0]).

-define(E, aihtml_element).

%% {Min, Max} or {Min, Max, Step}
-type range() :: {number(), number()} | {number(), number(), number()}.

-define(THUMB, 18).

%% @doc sigil's Slider. Range is {Min, Max} or {Min, Max, Step}; Value a
%% number, or {Lo, Hi} for a two-thumb range slider. data-ah-value holds
%% "V" or "Lo,Hi"; `input' fires while dragging, `change' on release and
%% on each keyboard or button step.
-spec ah_slider(range(),
                number() | {number(), number()} | undefined,
                aihtml_html:css(), aihtml_html:attrs()) -> #ah_slider{}.
ah_slider({Min, Max}, Value, Css, Attrs) ->
    ah_slider({Min, Max, 1}, Value, Css, Attrs);
ah_slider({Min, Max, Step} = Range, Value, Css, Attrs) when Max > Min, Step > 0 ->
    ?E:build(?MODULE, #ah_slider{range = Range, value = Value}, Css, Attrs);
ah_slider(Range, _Value, _Css, _Attrs) ->
    error({aihtml, {bad_slider_range, Range}}).

-spec render(#ah_slider{}) -> aihtml_html:html().
render(#ah_slider{range = {Min, Max}} = S) ->
    render(S#ah_slider{range = {Min, Max, 1}});
render(#ah_slider{range = {Min, Max, Step}, value = Value, disabled = Disabled,
                  buttons = Buttons, ticks = Ticks, ticks_position = TicksPos} = S)
  when is_number(Min), is_number(Max), is_number(Step), Max > Min, Step > 0 ->
    Classes = ?E:classes(?MODULE, S),
    Vertical = S#ah_slider.orientation =:= vertical,
    lists:member(TicksPos, [top, bottom, both])
        orelse error({aihtml, {bad_option, ticks_position, TicksPos}}),
    Clamp = fun(V) -> max(Min, min(Max, V)) end,
    Ratio = fun(V) -> (V - Min) / (Max - Min) end,
    {Range, Values} = case Value of
                          {Lo, Hi} -> {true, [Clamp(min(Lo, Hi)), Clamp(max(Lo, Hi))]};
                          undefined -> {false, [Min]};
                          V -> {false, [Clamp(V)]}
                      end,
    Val = iolist_to_binary(lists:join(<<",">>, [num(V) || V <- Values])),
    Pos = fun(R) -> pos_style(Vertical, R) end,
    Base = [{aria_valuemin, num(Min)}, {aria_valuemax, num(Max)},
            {aria_orientation, orientation(Vertical)}],
    Thumb = fun(Which, V, Extra) ->
                    el('div', [], [<<"ah-slider-thumb">>, <<"ah-slider-thumb-", Which/binary>>],
                       [{style, Pos(Ratio(V))} | Extra])
            end,
    {Thumbs, RangeStyle, RootAria} =
        case {Range, Values} of
            {true, [A, B]} ->
                {[Thumb(<<"start">>, A,
                        [{role, slider}, {tabindex, tab(Disabled)}, {aria_label, aihtml_i18n:text(common, minimum)},
                         {aria_valuenow, num(A)}, {aria_valuetext, num(A)} | Base]),
                  Thumb(<<"end">>, B,
                        [{role, slider}, {tabindex, tab(Disabled)}, {aria_label, aihtml_i18n:text(common, maximum)},
                         {aria_valuenow, num(B)}, {aria_valuetext, num(B)} | Base])],
                 range_style(Vertical, Ratio(A), Ratio(B)),
                 [{role, group}, {aria_orientation, orientation(Vertical)}]};
            {false, [A]} ->
                {[Thumb(<<"end">>, A, [{aria_hidden, <<"true">>}])],
                 fill_style(Vertical, Ratio(A)),
                 [{role, slider}, {tabindex, tab(Disabled)},
                  {aria_valuenow, num(A)}, {aria_valuetext, num(A)} | Base]}
        end,
    TickHtml = fun(Where) ->
                       case Ticks =/= false andalso (TicksPos =:= Where orelse TicksPos =:= both) of
                           true -> ticks(Where, {Min, Max}, Ticks, S, Vertical);
                           false -> []
                       end
               end,
    Btn = fun(Which, Icon) ->
                  el(button, el(span, Icon, [<<"ah-slider-button-icon">>], []),
                     [<<"ah-slider-button">>, <<"ah-slider-button-", Which/binary>>],
                     [{type, button}, {tabindex, -1},
                      {aria_label, case Which of <<"prev">> -> aihtml_i18n:text(common, decrease);
                                                 _ -> aihtml_i18n:text(common, increase) end}])
          end,
    ButtonsHtml = case {Buttons, Vertical} of
                      {false, _} -> [];
                      {true, false} -> [Btn(<<"prev">>, <<"◀"/utf8>>), Btn(<<"next">>, <<"▶"/utf8>>)];
                      {true, true} -> [Btn(<<"prev">>, <<"▲"/utf8>>), Btn(<<"next">>, <<"▼"/utf8>>)]
                  end,
    Tooltip = case S#ah_slider.tooltip of
                  true -> el('div', [], [<<"ah-slider-tooltip">>], [{aria_hidden, <<"true">>}]);
                  false -> []
              end,
    Extra = [[<<"ah-slider-buttons-hidden">> || not Buttons],
             [<<"ah-slider-ticks-hidden">> || Ticks =:= false],
             [<<"ah-slider-range-slider">> || Range],
             [<<"ah-slider-ticks-", (atom_to_binary(TicksPos))/binary>> || Ticks =/= false]],
    el('div',
       [ButtonsHtml,
        el('div', [TickHtml(top),
                   el('div', [el('div', [], [<<"ah-slider-range">>],
                                 [{aria_hidden, <<"true">>}, {style, RangeStyle}]),
                              Thumbs],
                      [<<"ah-slider-track">>], []),
                   TickHtml(bottom)],
           [<<"ah-slider-content">>], []),
        Tooltip,
        hidden(S#ah_slider.name, Val)],
       [Classes, Extra],
       [RootAria,
        [{aria_disabled, aria(Disabled)},
         {data_ah, <<"slider">>}, {data_ah_value, Val},
         {data_ah_min, num(Min)}, {data_ah_max, num(Max)}, {data_ah_step, num(Step)},
         {data_ah_min_range, case S#ah_slider.min_range of
                                 undefined -> undefined;
                                 MR -> num(MR)
                             end}],
        ?E:root_attrs(S, change)]);
render(#ah_slider{range = Range}) ->
    error({aihtml, {bad_slider_range, Range}}).

tab(true) -> -1;
tab(false) -> 0.

orientation(true) -> <<"vertical">>;
orientation(false) -> <<"horizontal">>.

%% Positions are fractions of the track less one thumb, so the server can
%% lay the slider out without measuring anything.
frac(R) -> [<<"calc((100% - ">>, integer_to_binary(?THUMB), <<"px) * ">>, ratio(R), <<")">>].
frac_center(R) ->
    [<<"calc((100% - ">>, integer_to_binary(?THUMB), <<"px) * ">>, ratio(R),
     <<" + ">>, integer_to_binary(?THUMB div 2), <<"px)">>].

pos_style(false, R) -> iolist_to_binary([<<"left:">>, frac(R)]);
pos_style(true, R) -> iolist_to_binary([<<"top:">>, frac(1 - R)]).

fill_style(false, R) -> iolist_to_binary([<<"left:0;width:">>, frac_center(R)]);
fill_style(true, R) -> iolist_to_binary([<<"bottom:0;height:">>, frac_center(R)]).

range_style(false, A, B) ->
    iolist_to_binary([<<"left:">>, frac_center(A), <<";width:">>, frac(B - A)]);
range_style(true, A, B) ->
    iolist_to_binary([<<"bottom:">>, frac_center(A), <<";height:">>, frac(B - A)]).

ratio(R) -> float_to_binary(float(R), [{decimals, 4}, compact]).

ticks(Where, {Min, Max}, Interval, S, Vertical) ->
    Major = tick_values(Min, Max, Interval),
    Minor = case S#ah_slider.minor_ticks of
                false -> [];
                MI -> tick_values(Min, Max, MI) -- Major
            end,
    Labels = S#ah_slider.labels,
    Dir = orientation(Vertical),
    Prop = case Vertical of true -> <<"top:">>; false -> <<"left:">> end,
    At = fun(V) ->
                 R = (V - Min) / (Max - Min),
                 iolist_to_binary([Prop, frac_center(case Vertical of true -> 1 - R;
                                                                      false -> R end)])
         end,
    Tick = fun(V, Kind) ->
                   el('div', [], [<<"ah-slider-tick">>, <<"ah-slider-tick-", Kind/binary>>,
                                  <<"ah-slider-tick-", Dir/binary>>], [{style, At(V)}])
           end,
    el('div',
       [[Tick(V, <<"major">>) || V <- Major],
        [Tick(V, <<"minor">>) || V <- Minor],
        [el('div', num(V), [<<"ah-slider-tick-label">>], [{style, At(V)}]) || Labels, V <- Major]],
       [<<"ah-slider-ticks">>, <<"ah-slider-ticks-", (atom_to_binary(Where))/binary>>],
       [{aria_hidden, <<"true">>}]).

tick_values(Min, Max, Interval) when Interval > 0 ->
    N = trunc((Max - Min) / Interval + 1.0e-9),
    [tidy(Min + I * Interval) || I <- lists:seq(0, N)].

%%%===================================================================
%%% Record and catalog
%%%===================================================================

%% @doc The field names of #ah_slider{}.
-spec fields(atom()) -> [atom()].
fields(ah_slider) -> record_info(fields, ah_slider).

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => slider, category => form,
       signature => <<"ah_slider({Min, Max} | {Min, Max, Step}, Value | {Lo, Hi}, Css, Attrs)">>,
       root => <<"ah-slider">>,
       groups => #{orientation => {[horizontal, vertical], horizontal},
                   template => {[primary, success, warning, danger, info, secondary], none}},
       flags => [disabled, buttons, tooltip],
       classes => #{buttons => [], tooltip => []},
       options => [name, ticks, minor_ticks, labels, ticks_position, min_range],
       behavior => <<"slider">>,
       events => [<<"input">>, <<"change">>],
       option_docs => #{horizontal => <<"Left to right (default).">>,
                        vertical => <<"Bottom to top; give it a height (default 160px).">>,
                        primary => <<"Primary colour (the default look).">>,
                        success => <<"Success colour.">>,
                        warning => <<"Warning colour.">>,
                        danger => <<"Danger colour.">>,
                        info => <<"Info colour.">>,
                        secondary => <<"Secondary colour.">>,
                        disabled => <<"Greyed out and inert.">>,
                        buttons => <<"Decrease / increase buttons at both ends.">>,
                        tooltip => <<"Value bubble over the thumb while dragging or focused.">>,
                        name => <<"Name of the hidden input (\"V\" or \"Lo,Hi\").">>,
                        ticks => <<"Interval between major ticks; off by default.">>,
                        minor_ticks => <<"Interval between minor ticks.">>,
                        labels => <<"false hides the major tick labels.">>,
                        ticks_position => <<"top | bottom (default) | both.">>,
                        min_range => <<"Smallest gap between the two thumbs of a range slider.">>},
       methods => [#{name => getValue, args => <<"()">>, doc => <<"Return data-ah-value (\"V\" or \"Lo,Hi\").">>},
                   #{name => setValue, args => <<"(Value, Silent)">>, doc => <<"Set a number, [Lo, Hi] or \"Lo,Hi\" (snapped and clamped); fires change unless Silent is true.">>}],
       doc => <<"Pointer and keyboard slider; {Lo, Hi} gives two thumbs. Options: ticks "
                "(major interval), minor_ticks, labels (default true), ticks_position "
                "(top | bottom | both), min_range. Flags: buttons (step buttons), tooltip "
                "(value bubble while dragging).">>}].

%%%===================================================================
%%% Internal
%%%===================================================================

el(Tag, Children, Css, Attrs) -> aihtml_html:el(Tag, Children, Css, Attrs).

hidden(undefined, _Val) -> [];
hidden(Name, Val) -> aihtml_html:void(input, [], [{type, hidden}, {name, Name}, {value, Val}]).

aria(true) -> <<"true">>;
aria(false) -> undefined.

num(N) -> aihtml_lib_form:num(N).

tidy(V) -> aihtml_lib_form:tidy(V).
