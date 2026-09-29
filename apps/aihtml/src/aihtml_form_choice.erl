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
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_form_choice).

-export([checkbox/4, radiobutton/4, switch_button/4,
         checkbox_group/4, radiobutton_group/4, radio_cards/4,
         rating_group/4,
         catalog/0, examples/0]).

-export_type([item/0, value/0]).

-type value() :: binary() | atom() | integer() | float() | string().
%% A group item. Opts (a map or proplist): disabled, class; radio_cards
%% also description and icon (html).
-type item() :: value() | {value(), aihtml_html:html()}
              | {value(), aihtml_html:html(), map() | list()}.

-define(INPUT_CLASS, <<"ah-choice-input">>).
%% Attributes of a group that go to every native input instead of the root.
-define(INPUT_ATTRS, [<<"name">>, <<"disabled">>, <<"required">>, <<"form">>]).

-define(STAR_SVG,
        {safe, <<"<svg viewBox=\"0 0 24 24\" fill=\"currentColor\" aria-hidden=\"true\">"
                 "<path d=\"M12 2l3.09 6.26L22 9.27l-5 4.87 1.18 6.88L12 17.77l-6.18 "
                 "3.25L7 14.14 2 9.27l6.91-1.01L12 2z\"/></svg>">>}).

%%%===================================================================
%%% Single controls
%%%===================================================================

%% @doc A checkbox. `Value' is the input's form value (`undefined' keeps
%% the browser default "on"). Options in Attrs: `indeterminate',
%% `three_states' (click cycles checked -> mixed -> unchecked), `locked'
%% (focusable but cannot be toggled), `box_size' (px).
-spec checkbox(aihtml_html:html(), value() | undefined, aihtml_html:css(),
               aihtml_html:attrs()) -> aihtml_html:element().
checkbox(Content, Value, Css, Attrs) ->
    Entry = entry(checkbox),
    {Opts, Rest} = aihtml_catalog:split_options(Entry, Attrs),
    Checked = truthy(attr(<<"checked">>, Rest)),
    Disabled = truthy(attr(<<"disabled">>, Rest)),
    Indet = truthy(maps:get(indeterminate, Opts, false)),
    Input = input(checkbox, Value, Rest, []),
    aihtml_html:el(label,
        [Input, check_box(Checked, Indet, maps:get(box_size, Opts, undefined)),
         label_span(<<"ah-checkbox-label">>, Content)],
        [aihtml_catalog:classes(Entry, Css),
         state(Disabled, <<"ah-checkbox-disabled">>),
         state(Checked andalso not Indet, <<"ah-checkbox-checked">>),
         state(Indet, <<"ah-checkbox-indeterminate">>)],
        [{data_ah, <<"checkbox">>},
         {data_ah_three_states, truthy(maps:get(three_states, Opts, false))},
         {data_ah_locked, truthy(maps:get(locked, Opts, false))}]).

%% @doc A radio button. Radio buttons with the same `name' are exclusive
%% (natively), and the behaviour restyles the one that was unchecked.
%% Options: `locked', `box_size'.
-spec radiobutton(aihtml_html:html(), value() | undefined, aihtml_html:css(),
                  aihtml_html:attrs()) -> aihtml_html:element().
radiobutton(Content, Value, Css, Attrs) ->
    Entry = entry(radiobutton),
    {Opts, Rest} = aihtml_catalog:split_options(Entry, Attrs),
    Checked = truthy(attr(<<"checked">>, Rest)),
    Disabled = truthy(attr(<<"disabled">>, Rest)),
    aihtml_html:el(label,
        [input(radio, Value, Rest, []),
         radio_box(Checked, maps:get(box_size, Opts, undefined)),
         label_span(<<"ah-radiobutton-label">>, Content)],
        [aihtml_catalog:classes(Entry, Css),
         state(Disabled, <<"ah-radiobutton-disabled">>),
         state(Checked, <<"ah-radiobutton-checked">>)],
        [{data_ah, <<"radiobutton">>},
         {data_ah_locked, truthy(maps:get(locked, Opts, false))}]).

%% @doc A switch: a native checkbox with `role="switch"'. Options:
%% `on_label', `off_label' (text inside the track), `locked', and sigil's
%% `width', `height', `thumb_size' (px) for a custom size.
-spec switch_button(aihtml_html:html(), value() | undefined, aihtml_html:css(),
                    aihtml_html:attrs()) -> aihtml_html:element().
switch_button(Content, Value, Css, Attrs) ->
    Entry = entry(switch_button),
    {Opts, Rest} = aihtml_catalog:split_options(Entry, Attrs),
    Checked = truthy(attr(<<"checked">>, Rest)),
    Disabled = truthy(attr(<<"disabled">>, Rest)),
    {TrackStyle, ThumbStyle} = switch_style(Opts),
    Track = aihtml_html:el(span,
        [label_span(<<"ah-switch-label ah-switch-label-on">>,
                    maps:get(on_label, Opts, undefined)),
         label_span(<<"ah-switch-label ah-switch-label-off">>,
                    maps:get(off_label, Opts, undefined)),
         aihtml_html:el(span, [], [<<"ah-switch-thumb">>], [{style, ThumbStyle}])],
        [<<"ah-switch-track">>], [{style, TrackStyle}, {aria_hidden, <<"true">>}]),
    aihtml_html:el(label,
        [input(checkbox, Value, Rest, [{role, switch}]), Track,
         label_span(<<"ah-switch-text">>, Content)],
        [aihtml_catalog:classes(Entry, Css),
         state(Checked, <<"ah-switch-on">>),
         state(Disabled, <<"ah-switch-disabled">>)],
        [{data_ah, <<"switch-button">>},
         {data_ah_locked, truthy(maps:get(locked, Opts, false))}]).

%%%===================================================================
%%% Groups
%%%===================================================================

%% @doc Several checkboxes; `Values' lists the checked ones.
-spec checkbox_group([item()], [value()], aihtml_html:css(), aihtml_html:attrs()) ->
          aihtml_html:element().
checkbox_group(Items, Values, Css, Attrs) ->
    Selected = [to_bin(V) || V <- Values],
    group(checkbox_group, <<"ah-checkbox-group">>, <<"group">>, <<"checkbox-group">>,
          Items, fun(V) -> lists:member(V, Selected) end,
          join_values([V || {V, _, _} <- norm_items(Items), lists:member(V, Selected)]),
          Css, Attrs).

%% @doc Mutually exclusive radio buttons; `Value' is the selected one (or
%% `undefined'). Arrow keys move the selection, as in sigil. Give the
%% group a `name' so it is exclusive without JS too.
-spec radiobutton_group([item()], value() | undefined, aihtml_html:css(),
                        aihtml_html:attrs()) -> aihtml_html:element().
radiobutton_group(Items, Value, Css, Attrs) ->
    Sel = opt_bin(Value),
    group(radiobutton_group, <<"ah-radiobutton-group">>, <<"radiogroup">>,
          <<"radiobutton-group">>, Items, fun(V) -> V =:= Sel end,
          selected_value(Sel, Items), Css, Attrs).

%% @doc Selectable cards with a title, an optional description and icon
%% (item Opts `description', `icon'). Options in Attrs: `columns'
%% (1 | 2 | 3 | auto) and `align' (center | start).
-spec radio_cards([item()], value() | undefined, aihtml_html:css(),
                  aihtml_html:attrs()) -> aihtml_html:element().
radio_cards(Items, Value, Css, Attrs) ->
    Entry = entry(radio_cards),
    {Opts, Rest} = aihtml_catalog:split_options(Entry, Attrs),
    {InputAttrs, RootAttrs} = split_attrs(?INPUT_ATTRS, Rest),
    GroupDisabled = truthy(attr(<<"disabled">>, InputAttrs)),
    Sel = opt_bin(Value),
    Columns = check_opt(radio_cards, columns, to_bin(maps:get(columns, Opts, auto)),
                        [<<"1">>, <<"2">>, <<"3">>, <<"auto">>]),
    Align = check_opt(radio_cards, align, to_bin(maps:get(align, Opts, center)),
                      [<<"center">>, <<"start">>]),
    Cards = [begin
                 D = GroupDisabled orelse truthy(opt(disabled, IOpts)),
                 On = V =:= Sel,
                 aihtml_html:el(label,
                     [input(radio, V, InputAttrs, [{checked, On}, {disabled, D}]),
                      icon_span(opt(icon, IOpts)),
                      aihtml_html:el(span,
                          [aihtml_html:el(span, Label, [<<"ah-radio-cards__label">>], []),
                           desc_span(opt(description, IOpts))],
                          [<<"ah-radio-cards__text">>], [])],
                     [<<"ah-radio-cards__card">>, opt_class(IOpts)],
                     [{data_value, V}, {data_index, I - 1},
                      {data_selected, bool(On)}, {data_disabled, bool(D)},
                      {data_ah_item_disabled, truthy(opt(disabled, IOpts))}])
             end || {I, {V, Label, IOpts}} <- enumerate(norm_items(Items))],
    aihtml_html:el('div', Cards, aihtml_catalog:classes(Entry, Css),
        [{data_ah, <<"radio-cards">>}, {role, radiogroup},
         {data_ah_value, selected_value(Sel, Items)},
         {data_columns, Columns}, {data_align, Align},
         {data_disabled, bool(GroupDisabled)},
         {aria_disabled, GroupDisabled andalso <<"true">>},
         RootAttrs]).

%%%===================================================================
%%% Rating
%%%===================================================================

%% @doc Star rating from 0 to `Max'. Options in Attrs: `name' (hidden
%% input), `precision' (1 | 0.5), `allow_clear' (clicking the current
%% value clears it, default true), `readonly', `disabled'.
-spec rating_group(pos_integer(), number() | undefined, aihtml_html:css(),
                   aihtml_html:attrs()) -> aihtml_html:element().
rating_group(Max, Value, Css, Attrs) when is_integer(Max), Max >= 1 ->
    Entry = entry(rating_group),
    {Opts, Rest} = aihtml_catalog:split_options(Entry, Attrs),
    V = case Value of undefined -> 0; _ when is_number(Value) -> Value end,
    Precision = case maps:get(precision, Opts, 1) of
                    P when P == 1 -> <<"1">>;
                    P when P == 0.5 -> <<"0.5">>;
                    P -> error({aihtml, {bad_option, rating_group, precision, P}})
                end,
    Readonly = truthy(maps:get(readonly, Opts, false)),
    Disabled = truthy(maps:get(disabled, Opts, false)),
    Static = Readonly orelse Disabled,
    Mods = css_atoms(Css),
    Size = pick(Mods, [sm, md, lg], md),
    Color = pick(Mods, [warning, primary, success, error], warning),
    Stars = [aihtml_html:el(button,
                 [aihtml_html:el(span, ?STAR_SVG, [<<"ah-rating__empty">>], []),
                  aihtml_html:el(span, ?STAR_SVG, [<<"ah-rating__filled">>],
                                 [{style, [<<"width:">>, pct(fill_ratio(I, V)), <<"%;">>]}])],
                 [<<"ah-rating__star">>],
                 [{type, button}, {data_index, I}, {role, radio},
                  {aria_checked, bool(V >= I + 1)},
                  {aria_label, [integer_to_binary(I + 1), <<" / ">>, integer_to_binary(Max)]},
                  {tabindex, case Static of true -> -1; false -> 0 end},
                  {disabled, Disabled}])
             || I <- lists:seq(0, Max - 1)],
    Hidden = case maps:get(name, Opts, undefined) of
                 undefined -> [];
                 Name -> aihtml_html:void(input, [], [{type, hidden}, {name, Name},
                                                      {value, num(V)}])
             end,
    aihtml_html:el('div', [Stars, Hidden], aihtml_catalog:classes(Entry, Css),
        [{data_ah, <<"rating">>}, {role, radiogroup},
         {data_ah_value, num(V)}, {data_ah_max, Max},
         {data_size, Size}, {data_color, Color},
         {data_precision, Precision},
         {data_readonly, bool(Readonly)}, {data_disabled, bool(Disabled)},
         {data_allow_clear, bool(truthy(maps:get(allow_clear, Opts, true)))},
         {aria_readonly, Readonly andalso <<"true">>},
         {aria_disabled, Disabled andalso <<"true">>},
         Rest]);
rating_group(Max, _Value, _Css, _Attrs) ->
    error({aihtml, {bad_max, rating_group, Max}}).

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
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

%%%===================================================================
%%% Examples
%%%===================================================================

-spec examples() -> [{atom(), binary(), aihtml_html:html()}].
examples() ->
    Row = fun(Children) -> aihtml_html:el('div', Children, [<<"flex flex-wrap items-center gap-6">>], []) end,
    Col = fun(Children) -> aihtml_html:el('div', Children, [<<"flex flex-col gap-4">>], []) end,
    Fruits = [{apple, <<"Apple">>}, {pear, <<"Pear">>},
              {plum, <<"Plum <b>">>}, {fig, <<"Fig">>, #{disabled => true}}],
    [{checkbox, <<"States and sizes">>,
      Col([Row([checkbox(<<"Unchecked">>, undefined, [], [{name, cb1}]),
                checkbox(<<"Checked">>, undefined, [], [{name, cb2}, {checked, true}]),
                checkbox(<<"Indeterminate">>, undefined, [], [{indeterminate, true}]),
                checkbox(<<"Three states">>, undefined, [], [{three_states, true}, {checked, true}]),
                checkbox(<<"Locked">>, undefined, [], [{locked, true}, {checked, true}]),
                checkbox(<<"Disabled">>, undefined, [], [{disabled, true}]),
                checkbox(<<"Disabled on">>, undefined, [], [{disabled, true}, {checked, true}])]),
           Row([checkbox(<<"Small">>, undefined, [sm], [{checked, true}]),
                checkbox(<<"Medium">>, undefined, [md], [{checked, true}]),
                checkbox(<<"Large">>, undefined, [lg], [{checked, true}]),
                checkbox(<<"box_size 24">>, undefined, [], [{box_size, 24}, {checked, true}])])])},
     {radiobutton, <<"Shared name, sizes, disabled">>,
      Col([Row([radiobutton(<<"Email">>, email, [], [{name, contact}, {checked, true}]),
                radiobutton(<<"Phone">>, phone, [], [{name, contact}]),
                radiobutton(<<"Post">>, post, [], [{name, contact}]),
                radiobutton(<<"Locked">>, x, [], [{name, other}, {locked, true}]),
                radiobutton(<<"Disabled">>, y, [], [{disabled, true}, {checked, true}])]),
           Row([radiobutton(<<"Small">>, s, [sm], [{name, size}, {checked, true}]),
                radiobutton(<<"Medium">>, m, [md], [{name, size}]),
                radiobutton(<<"Large">>, l, [lg], [{name, size}])])])},
     {switch_button, <<"Labels, sizes, disabled">>,
      Col([Row([switch_button(<<"Wi-Fi">>, undefined, [], [{name, wifi}, {checked, true}]),
                switch_button(<<"Bluetooth">>, undefined, [], [{name, bt}]),
                switch_button(<<"On/Off labels">>, undefined, [],
                              [{on_label, <<"On">>}, {off_label, <<"Off">>}, {checked, true}]),
                switch_button(<<"Locked">>, undefined, [], [{locked, true}])]),
           Row([switch_button(<<"Small">>, undefined, [sm], [{checked, true}]),
                switch_button(<<"Medium">>, undefined, [md], [{checked, true}]),
                switch_button(<<"Large">>, undefined, [lg], [{checked, true}]),
                switch_button(<<"80 x 32">>, undefined, [],
                              [{width, 80}, {height, 32}, {on_label, <<"是"/utf8>>},
                               {off_label, <<"否"/utf8>>}]),
                switch_button(<<"Disabled">>, undefined, [], [{disabled, true}]),
                switch_button(<<"Disabled on">>, undefined, [], [{disabled, true}, {checked, true}])])])},
     {checkbox_group, <<"Vertical, horizontal, label before, disabled">>,
      Row([checkbox_group(Fruits, [apple, plum], [], [{name, fruit}, {id, <<"cbg1">>}]),
           checkbox_group(Fruits, [pear], [horizontal, sm], [{name, fruit2}]),
           checkbox_group(Fruits, [], [label_before, lg], [{name, fruit3}]),
           checkbox_group(Fruits, [apple], [], [{name, fruit4}, {disabled, true}])])},
     {radiobutton_group, <<"Vertical, horizontal, label before, disabled">>,
      Row([radiobutton_group(Fruits, pear, [], [{name, pick}, {id, <<"rbg1">>}]),
           radiobutton_group(Fruits, undefined, [horizontal, sm], [{name, pick2}]),
           radiobutton_group(Fruits, apple, [label_before, lg], []),
           radiobutton_group(Fruits, apple, [], [{name, pick4}, {disabled, true}])])},
     {radio_cards, <<"Plans, columns and alignment">>,
      Col([radio_cards([{free, <<"Free">>, #{description => <<"Personal trial, limited features">>}},
                        {pro, <<"Pro">>, #{description => <<"All features + priority support">>}},
                        {team, <<"Team">>, #{description => <<"Collaboration and permissions">>}},
                        {ent, <<"Enterprise">>, #{description => <<"Contact sales">>, disabled => true}}],
                       pro, [], [{name, plan}, {id, <<"rc1">>}, {columns, 2}, {align, start}]),
           radio_cards([{s, <<"Small">>, #{icon => <<"S"/utf8>>}},
                        {m, <<"Medium">>, #{icon => <<"M"/utf8>>}},
                        {l, <<"Large">>, #{icon => <<"L"/utf8>>}}],
                       undefined, [], [{name, tshirt}, {columns, 3}]),
           radio_cards([{a, <<"Disabled group">>}, {b, <<"B">>}], a, [],
                       [{columns, 1}, {disabled, true}])])},
     {rating_group, <<"Sizes, colours, half stars, read-only">>,
      Col([Row([rating_group(5, 3, [], [{name, stars}, {id, <<"rt1">>}]),
                rating_group(5, 2.5, [], [{precision, 0.5}, {id, <<"rt2">>}]),
                rating_group(10, 7, [sm, primary], [])]),
           Row([rating_group(5, 4, [sm], []),
                rating_group(5, 4, [md, success], []),
                rating_group(5, 4, [lg, error], []),
                rating_group(5, 3.5, [], [{readonly, true}, {precision, 0.5}]),
                rating_group(5, 2, [], [{disabled, true}])])])}].

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
switch_style(Opts) ->
    case {maps:get(width, Opts, undefined), maps:get(height, Opts, undefined),
          maps:get(thumb_size, Opts, undefined)} of
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
%% item label.
group(Name, Prefix, Role, Behavior, Items, IsOn, DataValue, Css, Attrs) ->
    Entry = entry(Name),
    {_, Rest} = aihtml_catalog:split_options(Entry, Attrs),
    {InputAttrs, RootAttrs} = split_attrs(?INPUT_ATTRS, Rest),
    GroupDisabled = truthy(attr(<<"disabled">>, InputAttrs)),
    Mods = css_atoms(Css),
    Before = lists:member(label_before, aihtml_catalog:flags(Entry, Css)),
    Size = pick(Mods, [sm, md, lg], none),
    {Kind, Inner} = case Name of
                        checkbox_group -> {checkbox, <<"ah-checkbox">>};
                        radiobutton_group -> {radio, <<"ah-radiobutton">>}
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
        [aihtml_catalog:classes(Entry, Css), state(GroupDisabled, <<Prefix/binary, "-disabled">>)],
        [{data_ah, Behavior}, {role, Role}, {data_ah_value, DataValue},
         {data_label_position, case Before of true -> before; false -> 'after' end},
         {aria_disabled, GroupDisabled andalso <<"true">>},
         RootAttrs]).

size_class(_Inner, none) -> [];
size_class(Inner, S) -> <<Inner/binary, "-", (atom_to_binary(S, utf8))/binary>>.

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

css_atoms(Css) -> [A || A <- lists:flatten([Css]), is_atom(A)].

pick(Mods, Allowed, Default) ->
    case [M || M <- Mods, lists:member(M, Allowed)] of
        [M | _] -> M;
        [] -> Default
    end.

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
