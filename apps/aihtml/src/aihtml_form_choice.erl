%%%-------------------------------------------------------------------
%%% @doc Choice controls ported from sigil: checkbox, radio button,
%%% switch, their groups, radio cards and the star rating.
%%%
%%% The single controls (checkbox/4, radiobutton/4, switch_button/4) wrap a
%%% real, visually hidden `<input>' in a `<label>' that carries sigil's
%%% markup (box, check mark, track, thumb). `Attrs' go to that input, so
%%% `name', `checked', `disabled', `id' and `aihtml:on(change, ...)' behave
%%% natively and the control takes part in form submission.
%%%
%%% The groups (checkbox_group/4, radiobutton_group/4, radio_cards/4) render
%%% one native input per item. Their `Attrs' are split: `name', `disabled',
%%% `required' and `form' go to every input, everything else (id, class,
%%% data, aria, `aihtml:on/2') goes to the root. The root carries
%%% `data-ah-value' (comma separated for the checkbox group); the behaviour
%%% keeps it in sync, stops the inputs' own `change' at the root and fires
%%% one `change' on the root instead, so `on(change, Action)' in the
%%% group's Attrs sees `Event.value' = the group's value.
%%%
%%% rating_group/4 is a value-bearing custom control (designs/04): the value
%%% is in `data-ah-value', a hidden input carries it when Attrs has a
%%% `name', and `change' fires on the root.
%%%
%%% Each function builds an element record (#ah_checkbox{} ..., defined in
%%% include/aihtml_form_choice.hrl) and render/1 turns it into HTML, so
%%% pages may also write the records directly (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_form_choice).
-behaviour(aihtml_element).

-include("aihtml_form_choice.hrl").

-export([checkbox/4, radiobutton/4, switch_button/4,
         checkbox_group/4, radiobutton_group/4, radio_cards/4,
         rating_group/4,
         render/1, fields/1, catalog/0]).

-export_type([item/0, value/0, element/0]).

-define(H, aihtml_html).
-define(E, aihtml_element).

-type html() :: aihtml_html:html().
-type css() :: aihtml_html:css().
-type attrs() :: aihtml_html:attrs().
-type value() :: ah_choice_value().
%% A group item. Opts (a map or proplist): disabled, class; radio_cards
%% also description and icon (html).
-type item() :: ah_choice_item().
-type element() :: #ah_checkbox{} | #ah_radiobutton{} | #ah_switch_button{}
                 | #ah_checkbox_group{} | #ah_radiobutton_group{}
                 | #ah_radio_cards{} | #ah_rating_group{}.

-define(INPUT_CLASS, <<"ah-choice-input">>).
%% Attributes of a group that go to every native input instead of the root.
-define(INPUT_ATTRS, [<<"name">>, <<"disabled">>, <<"required">>, <<"form">>]).

-define(STAR_SVG,
        {safe, <<"<svg viewBox=\"0 0 24 24\" fill=\"currentColor\" aria-hidden=\"true\">"
                 "<path d=\"M12 2l3.09 6.26L22 9.27l-5 4.87 1.18 6.88L12 17.77l-6.18 "
                 "3.25L7 14.14 2 9.27l6.91-1.01L12 2z\"/></svg>">>}).

%%%===================================================================
%%% Builders
%%%===================================================================

%% @doc A checkbox. `Value' is the input's form value (`undefined' keeps
%% the browser default "on"). Options in Attrs: `indeterminate',
%% `three_states' (click cycles checked -> mixed -> unchecked), `locked'
%% (focusable but cannot be toggled), `box_size' (px).
-spec checkbox(html(), value() | undefined, css(), attrs()) -> #ah_checkbox{}.
checkbox(Content, Value, Css, Attrs) ->
    build(#ah_checkbox{body = Content, value = Value}, Css, Attrs).

%% @doc A radio button. Radio buttons with the same `name' are exclusive
%% (natively), and the behaviour restyles the one that was unchecked.
%% Options: `locked', `box_size'.
-spec radiobutton(html(), value() | undefined, css(), attrs()) -> #ah_radiobutton{}.
radiobutton(Content, Value, Css, Attrs) ->
    build(#ah_radiobutton{body = Content, value = Value}, Css, Attrs).

%% @doc A switch: a native checkbox with `role="switch"'. Options:
%% `on_label', `off_label' (text inside the track), `locked', and sigil's
%% `width', `height', `thumb_size' (px) for a custom size.
-spec switch_button(html(), value() | undefined, css(), attrs()) -> #ah_switch_button{}.
switch_button(Content, Value, Css, Attrs) ->
    build(#ah_switch_button{body = Content, value = Value}, Css, Attrs).

%% @doc Several checkboxes; `Values' lists the checked ones.
-spec checkbox_group([item()], [value()], css(), attrs()) -> #ah_checkbox_group{}.
checkbox_group(Items, Values, Css, Attrs) ->
    build(#ah_checkbox_group{items = Items, value = Values}, Css, Attrs).

%% @doc Mutually exclusive radio buttons; `Value' is the selected one (or
%% `undefined'). Arrow keys move the selection, as in sigil. Give the
%% group a `name' so it is exclusive without JS too.
-spec radiobutton_group([item()], value() | undefined, css(), attrs()) ->
          #ah_radiobutton_group{}.
radiobutton_group(Items, Value, Css, Attrs) ->
    build(#ah_radiobutton_group{items = Items, value = Value}, Css, Attrs).

%% @doc Selectable cards with a title, an optional description and icon
%% (item Opts `description', `icon'). Options in Attrs: `columns'
%% (1 | 2 | 3 | auto) and `align' (center | start).
-spec radio_cards([item()], value() | undefined, css(), attrs()) -> #ah_radio_cards{}.
radio_cards(Items, Value, Css, Attrs) ->
    build(#ah_radio_cards{items = Items, value = Value}, Css, Attrs).

%% @doc Star rating from 0 to `Max'. Options in Attrs: `name' (hidden
%% input), `precision' (1 | 0.5), `allow_clear' (clicking the current
%% value clears it, default true), `readonly', `disabled'.
-spec rating_group(pos_integer(), number() | undefined, css(), attrs()) ->
          #ah_rating_group{}.
rating_group(Max, Value, Css, Attrs) when is_integer(Max), Max >= 1 ->
    build(#ah_rating_group{max = Max, value = Value}, Css, Attrs);
rating_group(Max, _Value, _Css, _Attrs) ->
    error({aihtml, {bad_max, rating_group, Max}}).

build(R, Css, Attrs) ->
    Tag = element(1, R),
    ?E:build(R, fields(Tag), entry(?E:component_name(Tag)), Css, Attrs).

%% @doc The field names of one of this group's records.
-spec fields(atom()) -> [atom()].
fields(ah_checkbox) -> record_info(fields, ah_checkbox);
fields(ah_radiobutton) -> record_info(fields, ah_radiobutton);
fields(ah_switch_button) -> record_info(fields, ah_switch_button);
fields(ah_checkbox_group) -> record_info(fields, ah_checkbox_group);
fields(ah_radiobutton_group) -> record_info(fields, ah_radiobutton_group);
fields(ah_radio_cards) -> record_info(fields, ah_radio_cards);
fields(ah_rating_group) -> record_info(fields, ah_rating_group).

%%%===================================================================
%%% Rendering
%%%===================================================================

%% The single controls put `id', the postback and `attrs' on the native
%% input, after its value and before `checked' / `disabled'.
-spec render(element()) -> html().
render(#ah_checkbox{body = Content, value = Value, checked = C, disabled = D,
                    indeterminate = Indet0, box_size = BoxSize} = R) ->
    {Checked, Disabled, InputAttrs} =
        single_input(R#ah_checkbox.attrs, C, D, R#ah_checkbox{attrs = []}),
    Indet = truthy(Indet0),
    ?H:el(label,
        [input(checkbox, Value, InputAttrs, []), check_box(Checked, Indet, BoxSize),
         label_span(<<"ah-checkbox-label">>, Content)],
        [classes(R),
         state(Disabled, <<"ah-checkbox-disabled">>),
         state(Checked andalso not Indet, <<"ah-checkbox-checked">>),
         state(Indet, <<"ah-checkbox-indeterminate">>)],
        [{data_ah, <<"checkbox">>},
         {data_ah_three_states, truthy(R#ah_checkbox.three_states)},
         {data_ah_locked, truthy(R#ah_checkbox.locked)}]);

render(#ah_radiobutton{body = Content, value = Value, checked = C, disabled = D,
                       box_size = BoxSize} = R) ->
    {Checked, Disabled, InputAttrs} =
        single_input(R#ah_radiobutton.attrs, C, D, R#ah_radiobutton{attrs = []}),
    ?H:el(label,
        [input(radio, Value, InputAttrs, []), radio_box(Checked, BoxSize),
         label_span(<<"ah-radiobutton-label">>, Content)],
        [classes(R),
         state(Disabled, <<"ah-radiobutton-disabled">>),
         state(Checked, <<"ah-radiobutton-checked">>)],
        [{data_ah, <<"radiobutton">>},
         {data_ah_locked, truthy(R#ah_radiobutton.locked)}]);

render(#ah_switch_button{body = Content, value = Value, checked = C, disabled = D} = R) ->
    {Checked, Disabled, InputAttrs} =
        single_input(R#ah_switch_button.attrs, C, D, R#ah_switch_button{attrs = []}),
    {TrackStyle, ThumbStyle} = switch_style(R#ah_switch_button.width,
                                            R#ah_switch_button.height,
                                            R#ah_switch_button.thumb_size),
    Track = ?H:el(span,
        [label_span(<<"ah-switch-label ah-switch-label-on">>, R#ah_switch_button.on_label),
         label_span(<<"ah-switch-label ah-switch-label-off">>, R#ah_switch_button.off_label),
         ?H:el(span, [], [<<"ah-switch-thumb">>], [{style, ThumbStyle}])],
        [<<"ah-switch-track">>], [{style, TrackStyle}, {aria_hidden, <<"true">>}]),
    ?H:el(label,
        [input(checkbox, Value, InputAttrs, [{role, switch}]), Track,
         label_span(<<"ah-switch-text">>, Content)],
        [classes(R),
         state(Checked, <<"ah-switch-on">>),
         state(Disabled, <<"ah-switch-disabled">>)],
        [{data_ah, <<"switch-button">>},
         {data_ah_locked, truthy(R#ah_switch_button.locked)}]);

render(#ah_checkbox_group{items = Items, value = Values} = R) ->
    Selected = [to_bin(V) || V <- Values],
    #ah_checkbox_group{name = N, disabled = D, required = Rq, form = F} = R,
    {InputAttrs, Root} = group_attrs(R#ah_checkbox_group.attrs, N, D, Rq, F),
    group(checkbox, <<"ah-checkbox-group">>, <<"group">>, <<"checkbox-group">>,
          Items, fun(V) -> lists:member(V, Selected) end,
          join_values([V || {V, _, _} <- norm_items(Items), lists:member(V, Selected)]),
          R#ah_checkbox_group.size, R#ah_checkbox_group.label_before, classes(R),
          InputAttrs, ?E:root_attrs(R#ah_checkbox_group{attrs = Root}, change));

render(#ah_radiobutton_group{items = Items, value = Value} = R) ->
    Sel = opt_bin(Value),
    #ah_radiobutton_group{name = N, disabled = D, required = Rq, form = F} = R,
    {InputAttrs, Root} = group_attrs(R#ah_radiobutton_group.attrs, N, D, Rq, F),
    group(radio, <<"ah-radiobutton-group">>, <<"radiogroup">>, <<"radiobutton-group">>,
          Items, fun(V) -> V =:= Sel end, selected_value(Sel, Items),
          R#ah_radiobutton_group.size, R#ah_radiobutton_group.label_before, classes(R),
          InputAttrs, ?E:root_attrs(R#ah_radiobutton_group{attrs = Root}, change));

render(#ah_radio_cards{items = Items, value = Value} = R) ->
    #ah_radio_cards{name = N, disabled = D, required = Rq, form = F} = R,
    {InputAttrs, Root} = group_attrs(R#ah_radio_cards.attrs, N, D, Rq, F),
    GroupDisabled = truthy(attr(<<"disabled">>, InputAttrs)),
    Sel = opt_bin(Value),
    Columns = check_opt(radio_cards, columns, to_bin(R#ah_radio_cards.columns),
                        [<<"1">>, <<"2">>, <<"3">>, <<"auto">>]),
    Align = check_opt(radio_cards, align, to_bin(R#ah_radio_cards.align),
                      [<<"center">>, <<"start">>]),
    Cards = [begin
                 Off = GroupDisabled orelse truthy(opt(disabled, IOpts)),
                 On = V =:= Sel,
                 ?H:el(label,
                     [input(radio, V, InputAttrs, [{checked, On}, {disabled, Off}]),
                      icon_span(opt(icon, IOpts)),
                      ?H:el(span,
                          [?H:el(span, Label, [<<"ah-radio-cards__label">>], []),
                           desc_span(opt(description, IOpts))],
                          [<<"ah-radio-cards__text">>], [])],
                     [<<"ah-radio-cards__card">>, opt_class(IOpts)],
                     [{data_value, V}, {data_index, I - 1},
                      {data_selected, bool(On)}, {data_disabled, bool(Off)},
                      {data_ah_item_disabled, truthy(opt(disabled, IOpts))}])
             end || {I, {V, Label, IOpts}} <- enumerate(norm_items(Items))],
    ?H:el('div', Cards, classes(R),
        [{data_ah, <<"radio-cards">>}, {role, radiogroup},
         {data_ah_value, selected_value(Sel, Items)},
         {data_columns, Columns}, {data_align, Align},
         {data_disabled, bool(GroupDisabled)},
         {aria_disabled, GroupDisabled andalso <<"true">>},
         ?E:root_attrs(R#ah_radio_cards{attrs = Root}, change)]);

render(#ah_rating_group{max = Max, value = Value, size = Size, color = Color} = R) ->
    is_integer(Max) andalso Max >= 1
        orelse error({aihtml, {bad_max, rating_group, Max}}),
    V = case Value of
            undefined -> 0;
            _ when is_number(Value) -> Value;
            _ -> error({aihtml, {bad_value, rating_group, Value}})
        end,
    Precision = case R#ah_rating_group.precision of
                    P when P == 1 -> <<"1">>;
                    P when P == 0.5 -> <<"0.5">>;
                    P -> error({aihtml, {bad_option, rating_group, precision, P}})
                end,
    Readonly = truthy(R#ah_rating_group.readonly),
    Disabled = truthy(R#ah_rating_group.disabled),
    Static = Readonly orelse Disabled,
    Stars = [?H:el(button,
                 [?H:el(span, ?STAR_SVG, [<<"ah-rating__empty">>], []),
                  ?H:el(span, ?STAR_SVG, [<<"ah-rating__filled">>],
                        [{style, [<<"width:">>, pct(fill_ratio(I, V)), <<"%;">>]}])],
                 [<<"ah-rating__star">>],
                 [{type, button}, {data_index, I}, {role, radio},
                  {aria_checked, bool(V >= I + 1)},
                  {aria_label, [integer_to_binary(I + 1), <<" / ">>, integer_to_binary(Max)]},
                  {tabindex, case Static of true -> -1; false -> 0 end},
                  {disabled, Disabled}])
             || I <- lists:seq(0, Max - 1)],
    Hidden = case R#ah_rating_group.name of
                 undefined -> [];
                 Name -> ?H:void(input, [], [{type, hidden}, {name, Name}, {value, num(V)}])
             end,
    ?H:el('div', [Stars, Hidden], classes(R),
        [{data_ah, <<"rating">>}, {role, radiogroup},
         {data_ah_value, num(V)}, {data_ah_max, Max},
         {data_size, Size}, {data_color, Color},
         {data_precision, Precision},
         {data_readonly, bool(Readonly)}, {data_disabled, bool(Disabled)},
         {data_allow_clear, bool(truthy(R#ah_rating_group.allow_clear))},
         {aria_readonly, Readonly andalso <<"true">>},
         {aria_disabled, Disabled andalso <<"true">>},
         ?E:root_attrs(R, change)]).

classes(R) ->
    Tag = element(1, R),
    ?E:classes(R, fields(Tag), entry(?E:component_name(Tag))).

%% The native input's attributes of a single control: the `attrs' field,
%% then disabled / checked, then id and postback (`Bare' is the record
%% without its attrs). A binary "checked" / "disabled" key in attrs still
%% counts, as it did before these were fields.
single_input(Attrs0, Checked, Disabled, Bare) ->
    Attrs = flat(Attrs0),
    {truthy(attr(<<"checked">>, [{checked, Checked} | Attrs])),
     truthy(attr(<<"disabled">>, [{disabled, Disabled} | Attrs])),
     [Attrs, {disabled, Disabled}, {checked, Checked}, ?E:root_attrs(Bare, change)]}.

%% A group's attributes for every input (the name / disabled / required /
%% form fields, then such keys left in attrs) and for the root (the rest).
group_attrs(Attrs, Name, Disabled, Required, Form) ->
    {Input, Root} = split_attrs(?INPUT_ATTRS, Attrs),
    {[{name, Name}, {disabled, Disabled}, {required, Required}, {form, Form} | Input], Root}.

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [maps:merge(E, api(N)) || #{name := N} = E <- entries()].

entries() ->
    Size = {[sm, md, lg], none},
    [#{name => checkbox, category => form,
       signature => <<"checkbox(Content, Value, Css, Attrs)">>,
       root => <<"ah-checkbox">>, groups => #{size => Size},
       options => [indeterminate, three_states, locked, box_size],
       behavior => <<"checkbox">>, events => [<<"change">>, <<"input">>],
       doc => <<"Checkbox around a native input. Attrs (name, checked, disabled, id, "
                "on/2) go to the <input>; Value is its form value. Options: "
                "indeterminate, three_states, locked, box_size.">>},
     #{name => radiobutton, category => form,
       signature => <<"radiobutton(Content, Value, Css, Attrs)">>,
       root => <<"ah-radiobutton">>, groups => #{size => Size},
       options => [locked, box_size],
       behavior => <<"radiobutton">>, events => [<<"change">>, <<"input">>],
       doc => <<"Radio button around a native input; Attrs go to the <input>, "
                "radios sharing a name are exclusive. Options: locked, box_size.">>},
     #{name => switch_button, category => form,
       signature => <<"switch_button(Content, Value, Css, Attrs)">>,
       root => <<"ah-switch">>, groups => #{size => Size},
       options => [on_label, off_label, locked, width, height, thumb_size],
       behavior => <<"switch-button">>, events => [<<"change">>, <<"input">>],
       doc => <<"On/off switch: a native checkbox with role=switch; Attrs go to the "
                "<input>. Options: on_label, off_label, locked, width, height, "
                "thumb_size.">>},
     #{name => checkbox_group, category => form,
       signature => <<"checkbox_group(Items, Values, Css, Attrs)">>,
       root => <<"ah-checkbox-group">>,
       groups => #{layout => {[vertical, horizontal], vertical}, size => Size},
       flags => [label_before],
       classes => #{sm => [], md => [], lg => [], label_before => []},
       behavior => <<"checkbox-group">>, events => [<<"change">>],
       doc => <<"Checkboxes from Items [{Value, Label} | {Value, Label, Opts}] "
                "(Opts: disabled, class). name, disabled, required, form go to every "
                "input; other Attrs (id, on/2, ...) to the root, whose data-ah-value "
                "is the comma separated checked values and which fires one change.">>},
     #{name => radiobutton_group, category => form,
       signature => <<"radiobutton_group(Items, Value, Css, Attrs)">>,
       root => <<"ah-radiobutton-group">>,
       groups => #{layout => {[vertical, horizontal], vertical}, size => Size},
       flags => [label_before],
       classes => #{sm => [], md => [], lg => [], label_before => []},
       behavior => <<"radiobutton-group">>, events => [<<"change">>],
       doc => <<"Radio buttons from Items; arrow keys move the selection. name, "
                "disabled, required, form go to every input, other Attrs to the root "
                "(data-ah-value, one change event).">>},
     #{name => radio_cards, category => form,
       signature => <<"radio_cards(Items, Value, Css, Attrs)">>,
       root => <<"ah-radio-cards">>, options => [columns, align],
       behavior => <<"radio-cards">>, events => [<<"change">>],
       doc => <<"Card radio group; item Opts: description, icon, disabled, class. "
                "Options: columns (1|2|3|auto), align (center|start). Attrs split as "
                "in radiobutton_group.">>},
     #{name => rating_group, category => form,
       signature => <<"rating_group(Max, Value, Css, Attrs)">>,
       root => <<"ah-rating">>,
       groups => #{size => {[sm, md, lg], md},
                   color => {[warning, primary, success, error], warning}},
       classes => #{sm => [], md => [], lg => [], warning => [], primary => [],
                    success => [], error => []},
       options => [name, precision, allow_clear, readonly, disabled],
       behavior => <<"rating">>, events => [<<"change">>, <<"ah:hover">>],
       doc => <<"Star rating with hover preview, half stars (precision 0.5) and "
                "arrow keys. Value in data-ah-value, hidden input when name is given, "
                "change on the root.">>}].


%% API docs (option_docs, methods) for the docs page, per component.
api(checkbox) ->
    #{option_docs =>
          #{sm => <<"Small box (14px) and text.">>,
            md => <<"Default box (16px).">>,
            lg => <<"Large box (20px) and text.">>,
            indeterminate => <<"true: start in the mixed state (input.indeterminate).">>,
            three_states => <<"true: a click cycles checked, mixed, unchecked.">>,
            locked => <<"true: focusable but the user cannot toggle it.">>,
            box_size => <<"Box size in px, overrides the size modifier.">>},
      methods => [set_checked(<<"true | false | \"mixed\"">>), get_value(<<"true | false | \"mixed\"">>),
                  set_disabled()]};
api(radiobutton) ->
    #{option_docs =>
          #{sm => <<"Small circle and text.">>, md => <<"Default size.">>,
            lg => <<"Large circle and text.">>,
            locked => <<"true: focusable but the user cannot select it.">>,
            box_size => <<"Circle size in px.">>},
      methods => [set_checked(<<"true | false">>), get_value(<<"true | false">>), set_disabled()]};
api(switch_button) ->
    #{option_docs =>
          #{sm => <<"36 x 20 track.">>, md => <<"50 x 24 track (default).">>,
            lg => <<"60 x 30 track.">>,
            on_label => <<"Text in the track while on.">>,
            off_label => <<"Text in the track while off.">>,
            locked => <<"true: focusable but the user cannot toggle it.">>,
            width => <<"Track width in px (default 50).">>,
            height => <<"Track height in px (default 24).">>,
            thumb_size => <<"Thumb size in px (default height - 4).">>},
      methods => [set_checked(<<"true | false">>), get_value(<<"true | false">>), set_disabled()]};
api(Group) when Group =:= checkbox_group; Group =:= radiobutton_group ->
    Multi = Group =:= checkbox_group,
    #{option_docs =>
          #{vertical => <<"One item per line (default).">>,
            horizontal => <<"Items in a wrapping row.">>,
            sm => <<"Small controls.">>, md => <<"Default size.">>, lg => <<"Large controls.">>,
            label_before => <<"Put each label before its control.">>},
      methods => [set_value(Multi), group_get_value(Multi), group_set_disabled()]};
api(radio_cards) ->
    #{option_docs =>
          #{columns => <<"1 | 2 | 3 | auto (default: as many 200px columns as fit).">>,
            align => <<"center (default) | start: top-align cards with long descriptions.">>},
      methods => [set_value(false), group_get_value(false), group_set_disabled()]};
api(rating_group) ->
    #{option_docs =>
          #{sm => <<"16px stars.">>, md => <<"22px stars (default).">>, lg => <<"30px stars.">>,
            warning => <<"Gold stars (default).">>, primary => <<"Stars in the primary colour.">>,
            success => <<"Green stars.">>, error => <<"Red stars.">>,
            name => <<"Name of the hidden input that submits the value.">>,
            precision => <<"1 (default) or 0.5 for half stars.">>,
            allow_clear => <<"Clicking the current value resets to 0 (default true).">>,
            readonly => <<"true: shows the value, no interaction.">>,
            disabled => <<"true: dimmed, no interaction.">>},
      methods => [#{name => setValue, args => <<"(value)">>,
                    doc => <<"Set the rating (snapped to the precision); no change event.">>},
                  #{name => getValue, args => <<"()">>, doc => <<"The current rating.">>}]}.

set_checked(Args) ->
    #{name => setChecked, args => <<"(", Args/binary, ")">>,
      doc => <<"Set the state; no change event.">>}.
get_value(Ret) ->
    #{name => getValue, args => <<"()">>, doc => <<"The current state: ", Ret/binary, ".">>}.
set_disabled() ->
    #{name => setDisabled, args => <<"(bool)">>, doc => <<"Disable or enable the input.">>}.
set_value(true) ->
    #{name => setValue, args => <<"(values | \"a,b\")">>,
      doc => <<"Check exactly these values; no change event.">>};
set_value(false) ->
    #{name => setValue, args => <<"(value)">>,
      doc => <<"Select this value (\"\" for none); no change event.">>}.
group_get_value(true) ->
    #{name => getValue, args => <<"()">>, doc => <<"The checked values, an array.">>};
group_get_value(false) ->
    #{name => getValue, args => <<"()">>, doc => <<"The selected value, \"\" if none.">>}.
group_set_disabled() ->
    #{name => setDisabled, args => <<"(bool)">>,
      doc => <<"Disable or enable the whole group; items disabled on their own stay disabled.">>}.

%%%===================================================================
%%% Internal: shared markup
%%%===================================================================

entry(Name) -> aihtml_catalog:entry(?MODULE, Name).

%% The native input. Attrs from the caller come after type/value so that
%% name, id, checked, on/2 ... apply; Forced wins over both.
input(Type, Value, Attrs, Forced) ->
    aihtml_html:void(input, [?INPUT_CLASS],
                     [{type, Type}, {value, opt_bin(Value)}, Attrs, Forced,
                      {type, Type}]).

check_box(Checked, Indet, BoxSize) ->
    Check = [<<"ah-checkbox-check">>,
             state(Checked andalso not Indet, <<"ah-checkbox-check-checked">>),
             state(Indet, <<"ah-checkbox-check-indeterminate">>)],
    aihtml_html:el(span, aihtml_html:el(span, [], Check, []),
                   [<<"ah-checkbox-box">>], [{style, box_style(BoxSize)}, {aria_hidden, <<"true">>}]).

radio_box(Checked, BoxSize) ->
    Check = [<<"ah-radiobutton-check">>, state(Checked, <<"ah-radiobutton-check-checked">>)],
    aihtml_html:el(span, aihtml_html:el(span, [], Check, []),
                   [<<"ah-radiobutton-box">>], [{style, box_style(BoxSize)}, {aria_hidden, <<"true">>}]).

box_style(undefined) -> undefined;
box_style(N) when is_integer(N), N > 0 ->
    Px = [integer_to_binary(N), <<"px">>],
    iolist_to_binary([<<"width:">>, Px, <<";height:">>, Px, <<";">>]);
box_style(N) -> error({aihtml, {bad_option, box_size, N}}).

%% sigil: thumb = height - 4, travel = thumb - width + 4.
switch_style(Width, Height, ThumbSize) ->
    case {Width, Height, ThumbSize} of
        {undefined, undefined, undefined} -> {undefined, undefined};
        {W0, H0, T0} ->
            W = default_int(W0, 50), H = default_int(H0, 24),
            T = default_int(T0, H - 4),
            {iolist_to_binary(io_lib:format("width:~bpx;height:~bpx;--sw-travel:~bpx;",
                                            [W, H, T - W + 4])),
             iolist_to_binary(io_lib:format("width:~bpx;height:~bpx;", [T, T]))}
    end.

default_int(undefined, D) -> D;
default_int(N, _) when is_integer(N), N > 0 -> N;
default_int(N, _) -> error({aihtml, {bad_option, switch_button, N}}).

label_span(_Class, undefined) -> [];
label_span(_Class, <<>>) -> [];
label_span(_Class, []) -> [];
label_span(Class, Content) -> aihtml_html:el(span, Content, [Class], []).

icon_span(undefined) -> [];
icon_span(Icon) -> aihtml_html:el(span, Icon, [<<"ah-radio-cards__icon">>], [{aria_hidden, <<"true">>}]).

desc_span(undefined) -> [];
desc_span(D) -> aihtml_html:el(span, D, [<<"ah-radio-cards__description">>], []).

%% checkbox_group and radiobutton_group: sigil's group markup, one item per
%% option, each wrapping the inner control (without its own label) and the
%% item label. `Classes' are the root's catalog classes, `RootAttrs' its
%% id, postback and attrs.
group(Kind, Prefix, Role, Behavior, Items, IsOn, DataValue, Size, Before0, Classes,
      InputAttrs, RootAttrs) ->
    GroupDisabled = truthy(attr(<<"disabled">>, InputAttrs)),
    Before = Before0 =:= true,
    Inner = case Kind of
                checkbox -> <<"ah-checkbox">>;
                radio -> <<"ah-radiobutton">>
            end,
    ItemEls =
        [begin
             D = GroupDisabled orelse truthy(opt(disabled, IOpts)),
             On = IsOn(V),
             Box = case Kind of checkbox -> check_box(On, false, undefined);
                                radio -> radio_box(On, undefined)
                   end,
             Control = aihtml_html:el(span,
                 [input(Kind, V, InputAttrs, [{checked, On}, {disabled, D}]), Box],
                 [Inner, size_class(Inner, Size), state(D, <<Inner/binary, "-disabled">>),
                  state(On, <<Inner/binary, "-checked">>)], []),
             Lbl = aihtml_html:el(span, Label, [<<Prefix/binary, "-label">>], []),
             aihtml_html:el(label,
                 case Before of true -> [Lbl, Control]; false -> [Control, Lbl] end,
                 [<<Prefix/binary, "-item">>, opt_class(IOpts),
                  state(D, <<Prefix/binary, "-item-disabled">>)],
                 [{data_value, V}, {data_index, I - 1},
                  {data_ah_item_disabled, truthy(opt(disabled, IOpts))}])
         end || {I, {V, Label, IOpts}} <- enumerate(norm_items(Items))],
    aihtml_html:el('div', ItemEls,
        [Classes, state(GroupDisabled, <<Prefix/binary, "-disabled">>)],
        [{data_ah, Behavior}, {role, Role}, {data_ah_value, DataValue},
         {data_label_position, case Before of true -> before; false -> 'after' end},
         {aria_disabled, GroupDisabled andalso <<"true">>},
         RootAttrs]).

size_class(_Inner, undefined) -> [];
size_class(Inner, S) ->
    <<Inner/binary, "-", (atom_to_binary(S, utf8))/binary>>.

%%%===================================================================
%%% Internal: values and attributes
%%%===================================================================

norm_items(Items) -> [norm_item(I) || I <- Items].

norm_item({V, L}) -> {to_bin(V), L, #{}};
norm_item({V, L, O}) when is_map(O) -> {to_bin(V), L, O};
norm_item({V, L, O}) when is_list(O) -> {to_bin(V), L, maps:from_list(O)};
norm_item(V) when is_binary(V); is_atom(V); is_number(V) -> {to_bin(V), to_bin(V), #{}};
norm_item(V) when is_list(V) -> {to_bin(V), to_bin(V), #{}};
norm_item(Other) -> error({aihtml, {bad_item, Other}}).

enumerate(L) -> lists:zip(lists:seq(1, length(L)), L).

opt(K, Opts) -> maps:get(K, Opts, undefined).

opt_class(Opts) ->
    case opt(class, Opts) of undefined -> []; C -> C end.

selected_value(undefined, _Items) -> <<>>;
selected_value(Sel, Items) ->
    case lists:keymember(Sel, 1, norm_items(Items)) of
        true -> Sel;
        false -> <<>>
    end.

join_values(Vs) -> iolist_to_binary(lists:join(<<",">>, Vs)).

check_opt(Name, Key, V, Allowed) ->
    case lists:member(V, Allowed) of
        true -> V;
        false -> error({aihtml, {bad_option, Name, Key, V}})
    end.

to_bin(B) when is_binary(B) -> B;
to_bin(A) when is_atom(A) -> atom_to_binary(A, utf8);
to_bin(I) when is_integer(I) -> integer_to_binary(I);
to_bin(F) when is_float(F) -> num(F);
to_bin(L) when is_list(L) -> unicode:characters_to_binary(L).

opt_bin(undefined) -> undefined;
opt_bin(V) -> to_bin(V).

num(I) when is_integer(I) -> integer_to_binary(I);
num(F) when is_float(F), F == trunc(F) -> integer_to_binary(trunc(F));
num(F) when is_float(F) -> float_to_binary(F, [short]).

bool(true) -> <<"true">>;
bool(false) -> <<"false">>.

state(true, Class) -> Class;
state(false, _Class) -> [].

truthy(V) -> not (V =:= false orelse V =:= undefined orelse V =:= null
                  orelse V =:= <<"false">>).

fill_ratio(I, V) ->
    R = V - I,
    if R >= 1 -> 1; R =< 0 -> 0; true -> R end.

pct(R) -> num(R * 100).

%% Attribute lookup and splitting with aihtml_html's key rules ("_" is "-").
attr(Name, Attrs) ->
    case [V || {K, V} <- flat(Attrs), key(K) =:= Name] of
        [] -> undefined;
        Vs -> lists:last(Vs)
    end.

split_attrs(Names, Attrs) ->
    lists:partition(fun({K, _}) -> lists:member(key(K), Names); (_) -> false end,
                    flat(Attrs)).

key(A) when is_atom(A) -> key(atom_to_binary(A, utf8));
key(B) when is_binary(B) -> binary:replace(B, <<"_">>, <<"-">>, [global]);
key(Other) -> Other.

flat(M) when is_map(M) -> lists:sort(maps:to_list(M));
flat(L) when is_list(L) ->
    lists:flatmap(fun(X) when is_list(X); is_map(X) -> flat(X); (X) -> [X] end, L).
