%%%-------------------------------------------------------------------
%%% @doc Selection and form layout components, ported from sigil
%%% (sigil.components.form.{dropdownlist, slider, form, validator}).
%%%
%%%   dropdownlist(Items, Value, Css, Attrs)   custom popup list
%%%   select(Options, Value, Css, Attrs)       native select, same look
%%%   slider(Range, Value, Css, Attrs)         single value or {Lo, Hi}
%%%   field(Label, Control, Css, Attrs)        one labelled form row
%%%   form_layout(Fields, Values, Css, Attrs)  sigil's declarative form
%%%   validate(Rules)                          client validation attrs
%%%
%%% The markup and classes are sigil's, so the ported stylesheets under
%%% priv/css/sigil/components apply; behaviour is in
%%% assets/js/components/form_select.js.
%%%
%%% == Items and options ==
%%%
%%% dropdownlist/4 and select/4 take a list of
%%%
%%%   Value                           value and label are the same
%%%   {Value, Label}
%%%   {Value, Label, #{disabled => true}}
%%%   {group, Label, [Item]}          a group heading (select: optgroup)
%%%
%%% Values are binaries, atoms or numbers and are compared as text.
%%%
%%% == form_layout/4 fields ==
%%%
%%% Fields is a list of rows, each one of
%%%
%%%   {Label, Control}                a labelled row
%%%   #{label => Label, control => Control, key => Key,
%%%     for => Id, help => Text, error => Text, required => true,
%%%     info => Text, label_position => top, label_width => 120,
%%%     hidden => true}
%%%   {columns, [Field]}              several fields on one row
%%%   {text, Text}                    a line of static text
%%%   blank | {blank, HeightPx}       vertical space
%%%
%%% Control is any html(), or a fun((Value) -> html()) that receives
%%% `maps:get(Key, Values, undefined)', so one Values map fills the form.
%%%
%%% == validate/1 rules ==
%%%
%%%   required | email | number | integer | phone | zip_code | ssn
%%%   | not_number | starts_with_letter
%%%   | {min_length, N} | {max_length, N} | {length, Min, Max}
%%%   | {min, N} | {max, N} | {range, Min, Max}
%%%   | {pattern, Regex}        whole value must match (like HTML pattern)
%%%   | {same_as, Selector}     equal to another field (confirm password)
%%%   | {Rule, Message}         any of the above with its own message
%%%
%%% and these options in the same list:
%%%   {hint, auto | tooltip | label}   auto: label inside field/4 rows,
%%%                                    sigil's tooltip bubble elsewhere
%%%   {position, right | left | top | bottom}   tooltip side (right)
%%%   {on, blur | input | change | [Event]}     when to check (blur)
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_form_select).

-export([dropdownlist/4, select/4, slider/4, field/4, form_layout/4,
         validate/1, catalog/0, facade_extras/0]).

-export_type([item/0, field_spec/0, rule/0]).

-type value() :: binary() | atom() | number().
-type item() :: value() | {value(), aihtml_html:html()}
              | {value(), aihtml_html:html(), #{disabled => boolean()}}
              | {group, aihtml_html:html(), [item()]}.
-type control() :: aihtml_html:html() | fun((term()) -> aihtml_html:html()).
-type field_spec() :: {aihtml_html:html(), control()}
                    | #{label => aihtml_html:html(), control := control(),
                        atom() => term()}
                    | {columns, [field_spec()]}
                    | {text, aihtml_html:html()}
                    | blank | {blank, pos_integer()}.
-type base_rule() :: atom() | {atom(), term()} | {atom(), term(), term()}.
-type rule() :: base_rule() | {base_rule(), binary()}.

-define(SIMPLE_RULES, [required, email, number, integer, phone, zip_code, ssn,
                       not_number, starts_with_letter]).
-define(ARG_RULES, [min_length, max_length, min, max, pattern, same_as]).

%%%===================================================================
%%% dropdownlist
%%%===================================================================

%% @doc sigil's DropDownList: a combobox root showing the current label and
%% a popup listbox. Value-bearing: data-ah-value on the root, a hidden
%% input when the `name' option is given, `change' on the root.
-spec dropdownlist([item()], value() | undefined, aihtml_html:css(),
                   aihtml_html:attrs()) -> aihtml_html:element().
dropdownlist(Items, Value, Css, Attrs) ->
    E = entry(dropdownlist),
    {Opts, Rest} = aihtml_catalog:split_options(E, Attrs),
    Flags = aihtml_catalog:flags(E, Css),
    Disabled = lists:member(disabled, Flags),
    Norm = norm_items(Items),
    Val = opt_bin(Value),
    Placeholder = maps:get(placeholder, Opts, <<"Select…"/utf8>>),
    Content = case find_label(Val, Norm) of
                  none -> el(span, Placeholder, [<<"ah-dropdownlist-content">>,
                                                 <<"ah-dropdownlist-content-placeholder">>], []);
                  {ok, L} -> el(span, L, [<<"ah-dropdownlist-content">>], [])
              end,
    Arrow = case lists:member(simple, Flags) of
                true -> [];
                false -> el(span, el(span, <<"▼"/utf8>>, [<<"ah-dropdownlist-arrow-icon">>], []),
                            [<<"ah-dropdownlist-arrow">>], [{aria_hidden, <<"true">>}])
            end,
    Filter = case maps:get(filterable, Opts, false) of
                 true ->
                     el('div', aihtml_html:void(input, [<<"ah-listbox-filter-input">>],
                                                [{type, text}, {autocomplete, off},
                                                 {aria_label, <<"Filter">>},
                                                 {placeholder, maps:get(filter_placeholder, Opts,
                                                                        <<"Search…"/utf8>>)}]),
                        [<<"ah-listbox-filter">>], []);
                 false -> []
             end,
    Height = maps:get(dropdown_height, Opts, 200),
    List = case Norm of
               [] -> el('div', <<"No data">>, [<<"ah-listbox-empty">>], []);
               _ -> el(ul, list_items(Norm, Val), [<<"ah-listbox-list">>], [{role, listbox}])
           end,
    Popup = el('div',
               el('div', [Filter,
                          el('div', List, [<<"ah-listbox-content">>],
                             [{style, [<<"max-height:">>, css_size(Height)]}])],
                  [<<"ah-listbox">>], []),
               [<<"ah-dropdownlist-popup">>], []),
    Hidden = hidden(Opts, Val),
    el('div', [el('div', [Content, Arrow], [<<"ah-dropdownlist-input-area">>], []),
               Popup, Hidden],
       cls(E, Css),
       [[{role, combobox}, {tabindex, case Disabled of true -> -1; false -> 0 end},
         {aria_haspopup, listbox}, {aria_expanded, <<"false">>},
         {aria_disabled, aria(Disabled)},
         {data_ah, <<"dropdownlist">>}, {data_ah_value, Val},
         {data_ah_placeholder, Placeholder}],
        Rest]).

list_items(Norm, Val) ->
    {Html, _} = lists:mapfoldl(
                  fun({group, Label, Sub}, Idx) ->
                          {Lis, Idx1} = lists:mapfoldl(fun(I, N) -> {list_item(I, N, Val), N + 1} end,
                                                       Idx, Sub),
                          {[el(li, Label, [<<"ah-listbox-group">>], [{role, presentation}]) | Lis], Idx1};
                     (Item, Idx) ->
                          {list_item(Item, Idx, Val), Idx + 1}
                  end, 0, Norm),
    Html.

list_item({item, V, L, Dis}, Idx, Val) ->
    Sel = V =:= Val,
    el(li, el(span, L, [<<"ah-listbox-label">>], []),
       [<<"ah-listbox-item">>,
        [<<"ah-listbox-item-selected">> || Sel],
        [<<"ah-listbox-item-disabled">> || Dis]],
       [{role, option}, {data_idx, Idx}, {data_value, V},
        {aria_selected, atom_to_binary(Sel)}, {aria_disabled, aria(Dis)}]).

find_label(<<>>, _) -> none;
find_label(Val, Norm) ->
    case [L || {item, V, L, _} <- flat_items(Norm), V =:= Val] of
        [L | _] -> {ok, L};
        [] -> none
    end.

flat_items(Norm) ->
    lists:flatmap(fun({group, _, Sub}) -> Sub; (I) -> [I] end, Norm).

norm_items(Items) ->
    [norm_item(I) || I <- Items].

norm_item({group, Label, Sub}) when is_list(Sub) ->
    {group, Label, [norm_item(I) || I <- Sub]};
norm_item({V, L}) -> {item, bin(V), L, false};
norm_item({V, L, #{} = O}) -> {item, bin(V), L, maps:get(disabled, O, false) =:= true};
norm_item(V) when is_binary(V); is_atom(V); is_number(V) -> {item, bin(V), bin(V), false};
norm_item(Other) -> error({aihtml, {bad_item, Other}}).

%%%===================================================================
%%% select
%%%===================================================================

%% @doc A native select wrapped to look like the dropdownlist. Css styles
%% the wrapper; Attrs (name, id, multiple, disabled, on/2 ...) go to the
%% select itself. Value may be a list when the select is `multiple'.
-spec select([item()], value() | [value()] | undefined, aihtml_html:css(),
             aihtml_html:attrs()) -> aihtml_html:element().
select(Options, Value, Css, Attrs) ->
    E = entry(select),
    {Opts, Rest} = aihtml_catalog:split_options(E, Attrs),
    Vals = case Value of
               undefined -> [];
               L when is_list(L), not is_integer(hd(L)) -> [bin(V) || V <- L];
               V -> [bin(V)]
           end,
    Placeholder = case maps:find(placeholder, Opts) of
                      {ok, P} -> el(option, P, [], [{value, <<>>}, {selected, Vals =:= []}]);
                      error -> []
                  end,
    OptionEls = [select_option(I, Vals) || I <- norm_items(Options)],
    el(span, [el(select, [Placeholder, OptionEls], [<<"ah-select-control">>], Rest),
              el(span, el(span, <<"▼"/utf8>>, [<<"ah-dropdownlist-arrow-icon">>], []),
                 [<<"ah-select-arrow">>], [{aria_hidden, <<"true">>}])],
       cls(E, Css), []).

select_option({group, Label, Sub}, Vals) ->
    el(optgroup, [select_option(I, Vals) || I <- Sub], [], [{label, text(Label)}]);
select_option({item, V, L, Dis}, Vals) ->
    el(option, L, [], [{value, V}, {selected, lists:member(V, Vals)}, {disabled, Dis}]).

%%%===================================================================
%%% slider
%%%===================================================================

-define(THUMB, 18).

%% @doc sigil's Slider. Range is {Min, Max} or {Min, Max, Step}; Value a
%% number, or {Lo, Hi} for a two-thumb range slider. data-ah-value holds
%% "V" or "Lo,Hi"; `input' fires while dragging, `change' on release and
%% on each keyboard or button step.
-spec slider({number(), number()} | {number(), number(), number()},
             number() | {number(), number()} | undefined,
             aihtml_html:css(), aihtml_html:attrs()) -> aihtml_html:element().
slider({Min, Max}, Value, Css, Attrs) ->
    slider({Min, Max, 1}, Value, Css, Attrs);
slider({Min, Max, Step}, Value, Css, Attrs) when Max > Min, Step > 0 ->
    E = entry(slider),
    {Opts, Rest} = aihtml_catalog:split_options(E, Attrs),
    Flags = aihtml_catalog:flags(E, Css),
    Vertical = lists:member(vertical, lists:flatten([Css])),
    Disabled = lists:member(disabled, Flags),
    Buttons = lists:member(buttons, Flags),
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
                        [{role, slider}, {tabindex, tab(Disabled)}, {aria_label, <<"Minimum">>},
                         {aria_valuenow, num(A)}, {aria_valuetext, num(A)} | Base]),
                  Thumb(<<"end">>, B,
                        [{role, slider}, {tabindex, tab(Disabled)}, {aria_label, <<"Maximum">>},
                         {aria_valuenow, num(B)}, {aria_valuetext, num(B)} | Base])],
                 range_style(Vertical, Ratio(A), Ratio(B)),
                 [{role, group}, {aria_orientation, orientation(Vertical)}]};
            {false, [A]} ->
                {[Thumb(<<"end">>, A, [{aria_hidden, <<"true">>}])],
                 fill_style(Vertical, Ratio(A)),
                 [{role, slider}, {tabindex, tab(Disabled)},
                  {aria_valuenow, num(A)}, {aria_valuetext, num(A)} | Base]}
        end,
    Ticks = maps:get(ticks, Opts, false),
    TicksPos = maps:get(ticks_position, Opts, bottom),
    TickHtml = fun(Where) ->
                       case Ticks =/= false andalso (TicksPos =:= Where orelse TicksPos =:= both) of
                           true -> ticks(Where, {Min, Max}, Ticks, Opts, Vertical);
                           false -> []
                       end
               end,
    Btn = fun(Which, Icon) ->
                  el(button, el(span, Icon, [<<"ah-slider-button-icon">>], []),
                     [<<"ah-slider-button">>, <<"ah-slider-button-", Which/binary>>],
                     [{type, button}, {tabindex, -1},
                      {aria_label, case Which of <<"prev">> -> <<"Decrease">>;
                                                 _ -> <<"Increase">> end}])
          end,
    ButtonsHtml = case {Buttons, Vertical} of
                      {false, _} -> [];
                      {true, false} -> [Btn(<<"prev">>, <<"◀"/utf8>>), Btn(<<"next">>, <<"▶"/utf8>>)];
                      {true, true} -> [Btn(<<"prev">>, <<"▲"/utf8>>), Btn(<<"next">>, <<"▼"/utf8>>)]
                  end,
    Tooltip = case lists:member(tooltip, Flags) of
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
        hidden(Opts, Val)],
       [cls(E, Css), Extra],
       [RootAria,
        [{aria_disabled, aria(Disabled)},
         {data_ah, <<"slider">>}, {data_ah_value, Val},
         {data_ah_min, num(Min)}, {data_ah_max, num(Max)}, {data_ah_step, num(Step)},
         {data_ah_min_range, case maps:find(min_range, Opts) of
                                 {ok, MR} -> num(MR);
                                 error -> undefined
                             end}],
        Rest]);
slider(Range, _Value, _Css, _Attrs) ->
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

ticks(Where, {Min, Max}, Interval, Opts, Vertical) ->
    Major = tick_values(Min, Max, Interval),
    Minor = case maps:get(minor_ticks, Opts, false) of
                false -> [];
                MI -> tick_values(Min, Max, MI) -- Major
            end,
    Labels = maps:get(labels, Opts, true),
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

tidy(V) when is_float(V) ->
    R = round(V),
    case abs(V - R) < 1.0e-9 of
        true -> R;
        false -> round(V * 1.0e6) / 1.0e6
    end;
tidy(V) -> V.

%%%===================================================================
%%% field
%%%===================================================================

%% @doc One form row in sigil's form markup: a label, the control, and an
%% optional help or error line under the control.
-spec field(aihtml_html:html(), aihtml_html:html(), aihtml_html:css(),
            aihtml_html:attrs()) -> aihtml_html:element().
field(Label, Control, Css, Attrs) ->
    E = entry(field),
    {Opts, Rest} = aihtml_catalog:split_options(E, Attrs),
    el('div', row_body(Label, Control, Opts),
       [cls(E, Css), [<<"ah-form-row-invalid">> || maps:is_key(error, Opts)]],
       Rest).

row_body(Label, Control, Opts) ->
    [label(Label, Opts),
     el('div',
        [el('div', [el('div', Control, [], []),
                    case maps:find(info, Opts) of
                        {ok, Info} -> el('div', <<"ⓘ"/utf8>>, [<<"ah-form-info">>],
                                         [{title, text(Info)}, {aria_label, text(Info)}]);
                        error -> []
                    end],
            [<<"ah-form-field">>], []),
         case maps:find(help, Opts) of
             {ok, Help} -> el('div', Help, [<<"ah-form-help">>], []);
             error -> []
         end,
         case maps:find(error, Opts) of
             {ok, Err} -> el('div', Err, [<<"ah-form-error">>, <<"ah-validator-error-label">>],
                             [{role, alert}]);
             error -> []
         end],
        [<<"ah-form-body">>], [])].

label(undefined, _Opts) -> [];
label(Label, Opts) ->
    Style = case maps:find(label_width, Opts) of
                {ok, W} -> S = css_size(W), [<<"width:">>, S, <<";min-width:">>, S];
                error -> undefined
            end,
    el(label, [el(span, Label, [], []),
               case maps:get(required, Opts, false) of
                   true -> el(span, <<"*">>, [<<"ah-form-required">>], [{aria_hidden, <<"true">>}]);
                   false -> []
               end],
       [<<"ah-form-label">>],
       [{for, maps:get(for, Opts, undefined)},
        {style, case Style of undefined -> undefined; _ -> iolist_to_binary(Style) end}]).

%%%===================================================================
%%% form_layout
%%%===================================================================

%% @doc sigil's declarative Form rendered on the server: rows of labelled
%% controls (see the module doc for the field shapes). The root is a
%% `<form>' (option `tag => div' for a plain container); Attrs go to it.
-spec form_layout([field_spec()], #{term() => term()}, aihtml_html:css(),
                  aihtml_html:attrs()) -> aihtml_html:element().
form_layout(Fields, Values, Css, Attrs) ->
    E = entry(form_layout),
    {Opts, Rest} = aihtml_catalog:split_options(E, Attrs),
    Global = maps:with([label_position, label_width], Opts),
    Rows = [form_row(F, Values, Global) || F <- Fields],
    Pad = case maps:get(padding, Opts, 10) of
              {T, R, B, L} -> [px(T), $\s, px(R), $\s, px(B), $\s, px(L)];
              P -> px(P)
          end,
    el(maps:get(tag, Opts, form), Rows, cls(E, Css),
       [[{style, iolist_to_binary([<<"padding:">>, Pad])}], Rest]).

form_row(blank, _Values, _G) -> form_row({blank, 16}, _Values, _G);
form_row({blank, H}, _Values, _G) ->
    el('div', [], [<<"ah-form-row">>, <<"ah-form-row-blank">>],
       [{style, iolist_to_binary([<<"height:">>, px(H)])}, {aria_hidden, <<"true">>}]);
form_row({text, Text}, _Values, _G) ->
    el('div', el('div', Text, [<<"ah-form-label-text">>], []),
       [<<"ah-form-row">>, <<"ah-form-row-label">>], []);
form_row({columns, Cols}, Values, G) ->
    el('div', [form_cell(<<"ah-form-col">>, C, Values, G) || C <- Cols],
       [<<"ah-form-row">>, <<"ah-form-columns">>], []);
form_row(F, Values, G) ->
    form_cell(<<"ah-form-row">>, F, Values, G).

form_cell(Class, {Label, Control}, Values, G) ->
    form_cell(Class, #{label => Label, control => Control}, Values, G);
form_cell(Class, #{control := Control0} = F, Values, G) ->
    Opts = maps:merge(G, F),
    Control = case Control0 of
                  Fun when is_function(Fun, 1) ->
                      Fun(maps:get(maps:get(key, F, undefined), Values, undefined));
                  Html -> Html
              end,
    Pos = case maps:get(label_position, Opts, left) of
              left -> [];
              P when P =:= top; P =:= right; P =:= bottom ->
                  [<<"ah-form-row-", (atom_to_binary(P))/binary>>];
              P -> error({aihtml, {bad_label_position, P}})
          end,
    el('div', row_body(maps:get(label, F, undefined), Control, Opts),
       [Class, Pos, [<<"ah-form-row-invalid">> || maps:is_key(error, F)]],
       [{data_ah_key, case maps:find(key, F) of {ok, K} -> bin(K); error -> undefined end},
        {hidden, maps:get(hidden, F, false)}]);
form_cell(_Class, Other, _Values, _G) ->
    error({aihtml, {bad_form_field, Other}}).

%%%===================================================================
%%% validate
%%%===================================================================

%% @doc Attributes that make a control validate on the client (sigil's
%% Validator). Put them in the control's Attrs, e.g.
%% `input(..., [{name, email}, validate([required, email])])'. A form
%% holding such controls is checked on submit; if anything fails the
%% submit is stopped before actions (on/2) or data-ah-fetch see it.
-spec validate([rule() | {hint | position | on, term()}]) -> aihtml_html:attrs().
validate(Rules) ->
    {Options, RuleList} = lists:partition(
                            fun({K, _}) -> lists:member(K, [hint, position, on]);
                               (_) -> false
                            end, Rules),
    Json = [rule_json(R) || R <- RuleList],
    Opts = maps:from_list(Options),
    On = case maps:get(on, Opts, blur) of
             L when is_list(L) -> iolist_to_binary(lists:join(<<" ">>, [bin(X) || X <- L]));
             X -> bin(X)
         end,
    [{data_ah_validate, iolist_to_binary(json:encode(Json))},
     {data_ah_validate_on, On},
     {data_ah_validate_hint, atom_opt(hint, Opts, [auto, tooltip, label])},
     {data_ah_validate_position, atom_opt(position, Opts, [right, left, top, bottom])},
     {aria_required, aria(lists:any(fun(#{rule := R}) -> R =:= required end, Json))}].

atom_opt(K, Opts, Allowed) ->
    case maps:find(K, Opts) of
        error -> undefined;
        {ok, V} ->
            lists:member(V, Allowed) orelse error({aihtml, {bad_validate_option, K, V}}),
            atom_to_binary(V)
    end.

rule_json({R, Msg}) when is_binary(Msg), is_tuple(R) ->
    (rule_json(R))#{msg => Msg};
rule_json({R, Msg}) when is_binary(Msg), is_atom(R) ->
    case lists:member(R, ?SIMPLE_RULES) of
        true -> #{rule => R, msg => Msg};
        false -> arg_rule(R, [Msg])            % {pattern, Regex}, {same_as, Sel}
    end;
rule_json(R) when is_atom(R) ->
    lists:member(R, ?SIMPLE_RULES) orelse error({aihtml, {unknown_rule, R}}),
    #{rule => R};
rule_json({length, Min, Max}) when is_integer(Min), is_integer(Max) ->
    #{rule => length, args => [Min, Max]};
rule_json({range, Min, Max}) when is_number(Min), is_number(Max) ->
    #{rule => range, args => [Min, Max]};
rule_json({R, Arg}) when is_atom(R) ->
    arg_rule(R, [Arg]);
rule_json(Other) ->
    error({aihtml, {unknown_rule, Other}}).

arg_rule(R, [Arg]) ->
    lists:member(R, ?ARG_RULES) orelse error({aihtml, {unknown_rule, R}}),
    A = case R of
            pattern -> bin(Arg);
            same_as -> bin(Arg);
            _ when is_number(Arg) -> Arg;
            _ -> error({aihtml, {bad_rule_argument, R, Arg}})
        end,
    #{rule => R, args => [A]}.

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    Templates = [primary, success, warning, danger],
    [#{name => dropdownlist, category => form,
       signature => <<"dropdownlist(Items, Value, Css, Attrs)">>,
       root => <<"ah-dropdownlist">>,
       groups => #{template => {Templates, none}},
       flags => [simple, disabled, block],
       options => [name, placeholder, filterable, filter_placeholder, dropdown_height],
       behavior => <<"dropdownlist">>,
       events => [<<"change">>, <<"ah:open">>, <<"ah:close">>],
       option_docs => #{primary => <<"Primary-coloured border.">>,
                        success => <<"Success-coloured border.">>,
                        warning => <<"Warning-coloured border.">>,
                        danger => <<"Danger-coloured border.">>,
                        simple => <<"No arrow segment.">>,
                        disabled => <<"Greyed out, not focusable, does not open.">>,
                        block => <<"Fill the container width.">>,
                        name => <<"Name of the hidden input that submits the value.">>,
                        placeholder => <<"Text shown while nothing is selected (default \"Select…\").">>,
                        filterable => <<"true: a filter box at the top of the popup.">>,
                        filter_placeholder => <<"Placeholder of the filter box.">>,
                        dropdown_height => <<"Maximum list height, px or CSS length (default 200).">>},
       methods => [#{name => open, args => <<"()">>, doc => <<"Open the popup.">>},
                   #{name => close, args => <<"()">>, doc => <<"Close the popup.">>},
                   #{name => getValue, args => <<"()">>, doc => <<"Return data-ah-value.">>},
                   #{name => setValue, args => <<"(Value, Silent)">>, doc => <<"Select the item with this value (\"\" clears); fires change unless Silent is true.">>},
                   #{name => disable, args => <<"()">>, doc => <<"Disable the control.">>},
                   #{name => enable, args => <<"()">>, doc => <<"Enable the control.">>}],
       doc => <<"Single-choice popup list with keyboard navigation, type-ahead and optional "
                "filtering. Items: Value | {Value, Label} | {Value, Label, #{disabled => true}} "
                "| {group, Label, Items}. `simple' hides the arrow; `block' fills the width.">>},
     #{name => select, category => form,
       signature => <<"select(Options, Value, Css, Attrs)">>,
       root => <<"ah-select">>,
       groups => #{size => {[sm, lg], none}, template => {Templates, none}},
       flags => [block],
       options => [placeholder],
       events => [<<"change">>],
       option_docs => #{sm => <<"Compact height (28px).">>,
                        lg => <<"Large height (42px).">>,
                        primary => <<"Primary-coloured border.">>,
                        success => <<"Success-coloured border.">>,
                        warning => <<"Warning-coloured border.">>,
                        danger => <<"Danger-coloured border.">>,
                        block => <<"Fill the container width.">>,
                        placeholder => <<"An empty first option with this label, selected when Value is undefined.">>},
       methods => [],
       doc => <<"Native <select> styled like the dropdownlist; Attrs go to the select. "
                "Options as for dropdownlist, groups become optgroups; Value may be a list "
                "with `multiple'. Option `placeholder' adds an empty first option.">>},
     #{name => slider, category => form,
       signature => <<"slider({Min, Max} | {Min, Max, Step}, Value | {Lo, Hi}, Css, Attrs)">>,
       root => <<"ah-slider">>,
       groups => #{orientation => {[horizontal, vertical], horizontal},
                   template => {Templates ++ [info, secondary], none}},
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
                "(value bubble while dragging).">>},
     #{name => field, category => form,
       signature => <<"field(Label, Control, Css, Attrs)">>,
       root => <<"ah-form-row">>,
       groups => #{label_position => {[left, top, right, bottom], left}},
       classes => #{left => []},
       options => [for, help, error, required, info, label_width],
       option_docs => #{left => <<"Label left of the control (default).">>,
                        top => <<"Label above the control.">>,
                        right => <<"Label right of the control (checkboxes).">>,
                        bottom => <<"Label under the control.">>,
                        for => <<"Id of the control the label points to.">>,
                        help => <<"Help line under the control.">>,
                        error => <<"Error line under the control; marks the row invalid.">>,
                        required => <<"true: a red asterisk after the label.">>,
                        info => <<"Tooltip text of an info icon after the control.">>,
                        label_width => <<"Label width, px or CSS length.">>},
       methods => [],
       doc => <<"A labelled form row: label (with `for', `required', `label_width'), the "
                "control, and a `help' or `error' line under it; `info' adds a hint icon. "
                "Controls given validate(Rules) show their errors in this row; page "
                "functions AH.fn validate(Target) and clearValidation(Target).">>},
     #{name => form_layout, category => form,
       signature => <<"form_layout(Fields, Values, Css, Attrs)">>,
       root => <<"ah-form">>,
       flags => [bordered, bg, disabled],
       options => [label_position, label_width, padding, tag],
       option_docs => #{bordered => <<"A border round the form.">>,
                        bg => <<"Paper background.">>,
                        disabled => <<"Dim the form and ignore the pointer.">>,
                        label_position => <<"Default label position of every field: left | top | right | bottom.">>,
                        label_width => <<"Default label width, px or CSS length.">>,
                        padding => <<"Inner padding in px, or {Top, Right, Bottom, Left} (default 10).">>,
                        tag => <<"form (default) or div.">>},
       methods => [],
       doc => <<"sigil's declarative form, rendered on the server. Fields: {Label, Control} "
                "| #{label, control, key, for, help, error, required, info, label_position, "
                "label_width, hidden} | {columns, [Field]} | {text, Text} | blank | "
                "{blank, Px}. A control may be fun(Value) -> html(), called with "
                "maps:get(Key, Values, undefined). Options: label_position (left | top | "
                "right | bottom), label_width, padding (px or {T,R,B,L}), tag (form | div).">>}].

%%%===================================================================
%%% Internal
%%%===================================================================

entry(Name) -> aihtml_catalog:entry(?MODULE, Name).

cls(Entry, Css) -> aihtml_catalog:classes(Entry, Css).

el(Tag, Children, Css, Attrs) -> aihtml_html:el(Tag, Children, Css, Attrs).

hidden(Opts, Val) ->
    case maps:find(name, Opts) of
        {ok, Name} -> aihtml_html:void(input, [], [{type, hidden}, {name, Name}, {value, Val}]);
        error -> []
    end.

aria(true) -> <<"true">>;
aria(false) -> undefined.

opt_bin(undefined) -> <<>>;
opt_bin(V) -> bin(V).

bin(B) when is_binary(B) -> B;
bin(A) when is_atom(A) -> atom_to_binary(A, utf8);
bin(N) when is_number(N) -> num(N);
bin(L) when is_list(L) -> unicode:characters_to_binary(L).

%% Label text for an attribute (title, label); non-text labels give "".
text(B) when is_binary(B) -> B;
text(A) when is_atom(A); is_number(A) -> bin(A);
text(L) when is_list(L) ->
    case io_lib:printable_unicode_list(L) of
        true -> unicode:characters_to_binary(L);
        false -> <<>>
    end;
text(_) -> <<>>.

num(I) when is_integer(I) -> integer_to_binary(I);
num(F) when is_float(F) ->
    case tidy(F) of
        I when is_integer(I) -> integer_to_binary(I);
        G -> float_to_binary(G, [short])
    end.

px(N) when is_integer(N) -> [integer_to_binary(N), <<"px">>];
px(B) when is_binary(B) -> B.

css_size(N) when is_integer(N) -> iolist_to_binary(px(N));
css_size(B) when is_binary(B) -> B.

%% @doc validate/1 builds Attrs, so the aihtml facade re-exports it.
-spec facade_extras() -> [{atom(), arity()}].
facade_extras() -> [{validate, 1}].
