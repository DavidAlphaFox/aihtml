%%%-------------------------------------------------------------------
%%% @doc Text entry components, ported from sigil (form/input,
%%% form/password_input, form/number_input, form/input_otp,
%%% form/tag_input). See designs/04-components.md.
%%%
%%%   input(Value, Css, Attrs)             one line of text
%%%   textarea(Value, Css, Attrs)          several lines, styled like input
%%%   password_input(Value, Css, Attrs)    with a show/hide toggle
%%%   number_input(Value, Css, Attrs)      spin buttons, min/max/step
%%%   input_otp(Length, Value, Css, Attrs) one box per character
%%%   tag_input(Tags, Css, Attrs)          chips plus a text field
%%%
%%% The first four keep a native control: `Attrs' (name, placeholder,
%%% on(...), ...) go to the `<input>'/`<textarea>', `Css' to the wrapper.
%%% input_otp and tag_input are value-bearing components: `Attrs' go to the
%%% root, which carries `data-ah-value' and fires `change'; `name' goes to
%%% a hidden input.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_form_text).

-export([input/3, textarea/3, password_input/3, number_input/3,
         input_otp/4, tag_input/3,
         catalog/0, examples/0]).

-define(H, aihtml_html).

%% Shared templates (see aihtml_tpl): also compiled to AH.tpl.* for the browser.
-compile({parse_transform, beamai_mustache_transform}).
-mustache_template({tpl_tag_input_chip, "../templates/tag_input_chip.mustache"}).

%%%===================================================================
%%% input
%%%===================================================================

%% @doc A text input in sigil's `.ah-input-group' shell. Options:
%% `prefix', `suffix' (any html: text or an icon), `label' (a floating
%% label; replaces the placeholder).
-spec input(binary() | undefined, aihtml_html:css(), aihtml_html:attrs()) ->
          aihtml_html:element().
input(Value, Css, Attrs0) ->
    Entry = aihtml_catalog:entry(?MODULE, input),
    {Opts, Attrs} = aihtml_catalog:split_options(Entry, Attrs0),
    Flags = aihtml_catalog:flags(Entry, Css),
    Mods = modifiers(Css),
    Disabled = lists:member(disabled, Flags),
    Clearable = lists:member(clearable, Flags),
    Label = maps:get(label, Opts, undefined),
    Id = input_id(Attrs, Label),
    Prefix = maps:get(prefix, Opts, undefined),
    Suffix = maps:get(suffix, Opts, undefined),
    Native = ?H:void(input, [<<"ah-input">>],
                     [[{type, text}, {value, Value}, {id, Id},
                       {aria_invalid, lists:member(invalid, Mods) andalso <<"true">>},
                       {disabled, Disabled}],
                      no_placeholder(Label, Attrs)]),
    Clear = [clear_button() || Clearable],
    Body = case Prefix =/= undefined orelse Suffix =/= undefined orelse Clearable of
               false -> Native;
               true ->
                   ?H:el('div',
                         [addon(<<"prefix">>, Prefix), Native, Clear,
                          addon(<<"suffix">>, Suffix)],
                         [<<"ah-input-row">>], [])
           end,
    ?H:el('div', [Body, float_label(<<"ah-input">>, Label, Id, Value)],
          [aihtml_catalog:classes(Entry, Css),
           [<<"ah-input-has-value">> || has_value(Value)]],
          [{data_ah, <<"input">>}]).

addon(_Side, undefined) -> [];
addon(Side, Html) ->
    ?H:el(span, Html, [<<"ah-input-addon">>, <<"ah-input-addon-", Side/binary>>], []).

clear_button() ->
    ?H:el(button, {safe, <<"&times;">>}, [<<"ah-input-clear">>],
          [{type, button}, {tabindex, <<"-1">>}, {aria_label, <<"Clear">>}]).

%%%===================================================================
%%% textarea
%%%===================================================================

%% @doc A multi-line input, styled like `input' (sigil has no textarea
%% widget; this follows its a2ui textarea). Option: `label' (floating).
-spec textarea(binary() | undefined, aihtml_html:css(), aihtml_html:attrs()) ->
          aihtml_html:element().
textarea(Value, Css, Attrs0) ->
    Entry = aihtml_catalog:entry(?MODULE, textarea),
    {Opts, Attrs} = aihtml_catalog:split_options(Entry, Attrs0),
    Flags = aihtml_catalog:flags(Entry, Css),
    Label = maps:get(label, Opts, undefined),
    Id = input_id(Attrs, Label),
    Native = ?H:el(textarea, text(Value), [<<"ah-input">>, <<"ah-textarea">>],
                   [[{rows, 3}, {id, Id},
                     {aria_invalid, lists:member(invalid, modifiers(Css)) andalso <<"true">>},
                     {disabled, lists:member(disabled, Flags)}],
                    no_placeholder(Label, Attrs)]),
    ?H:el('div', [Native, float_label(<<"ah-input">>, Label, Id, Value)],
          [<<"ah-input-group">>, aihtml_catalog:classes(Entry, Css)],
          [{data_ah, <<"input">>}]).

%%%===================================================================
%%% password_input
%%%===================================================================

%% @doc A password field with sigil's eye toggle. Options: `toggle'
%% (default true), `strength' (show the strength meter, default false),
%% `label' (floating).
-spec password_input(binary() | undefined, aihtml_html:css(), aihtml_html:attrs()) ->
          aihtml_html:element().
password_input(Value, Css, Attrs0) ->
    Entry = aihtml_catalog:entry(?MODULE, password_input),
    {Opts, Attrs} = aihtml_catalog:split_options(Entry, Attrs0),
    Flags = aihtml_catalog:flags(Entry, Css),
    Label = maps:get(label, Opts, undefined),
    Id = input_id(Attrs, Label),
    Native = ?H:void(input, [<<"ah-pwd">>],
                     [[{type, password}, {value, Value}, {id, Id},
                       {autocomplete, <<"current-password">>},
                       {spellcheck, <<"false">>},
                       {aria_invalid, lists:member(invalid, modifiers(Css)) andalso <<"true">>},
                       {disabled, lists:member(disabled, Flags)}],
                      no_placeholder(Label, Attrs)]),
    Toggle = case maps:get(toggle, Opts, true) of
                 false -> [];
                 _ -> ?H:el(button, [eye_open(), eye_closed()], [<<"ah-pwd-toggle">>],
                            [{type, button}, {tabindex, <<"-1">>},
                             {aria_label, <<"Show password">>},
                             {aria_pressed, <<"false">>},
                             {aria_controls, Id}])
             end,
    Strength = case maps:get(strength, Opts, false) of
                   true ->
                       ?H:el('div',
                             [?H:el('div', ?H:el('div', [], [<<"ah-pwd-strength-fill">>], []),
                                    [<<"ah-pwd-strength-bar">>], []),
                              ?H:el(span, [], [<<"ah-pwd-strength-text">>],
                                    [{aria_live, <<"polite">>}])],
                             [<<"ah-pwd-strength">>], []);
                   _ -> []
               end,
    ?H:el('div',
          [?H:el('div', [Native, Toggle], [<<"ah-pwd-wrapper">>], []),
           float_label(<<"ah-pwd">>, Label, Id, Value),
           Strength],
          aihtml_catalog:classes(Entry, Css),
          [{data_ah, <<"password-input">>}]).

svg(Class, Children) ->
    ?H:el(svg, Children, [Class],
          [{xmlns, <<"http://www.w3.org/2000/svg">>}, {width, 18}, {height, 18},
           {viewBox, <<"0 0 24 24">>}, {fill, none}, {stroke, <<"currentColor">>},
           {stroke_width, 2}, {stroke_linecap, round}, {stroke_linejoin, round},
           {aria_hidden, <<"true">>}]).

eye_open() ->
    svg(<<"ah-pwd-icon-show">>,
        [?H:el(path, [], [], [{d, <<"M1 12s4-8 11-8 11 8 11 8-4 8-11 8-11-8-11-8z">>}]),
         ?H:el(circle, [], [], [{cx, 12}, {cy, 12}, {r, 3}])]).

eye_closed() ->
    svg(<<"ah-pwd-icon-hide">>,
        [?H:el(path, [], [],
               [{d, <<"M17.94 17.94A10.07 10.07 0 0 1 12 20c-7 0-11-8-11-8a18.45 18.45 0 0 1 "
                      "5.06-5.94M9.9 4.24A9.12 9.12 0 0 1 12 4c7 0 11 8 11 8a18.5 18.5 0 0 1"
                      "-2.16 3.19m-6.72-1.07a3 3 0 1 1-4.24-4.24">>}]),
         ?H:el(line, [], [], [{x1, 1}, {y1, 1}, {x2, 23}, {y2, 23}])]).

%%%===================================================================
%%% number_input
%%%===================================================================

%% @doc A numeric field with sigil's spin buttons. The native input keeps
%% the plain number (no digit grouping) so forms submit it as is. Options:
%% `min', `max', `step' (default 1), `decimals' (default: those of step),
%% `spin' (default true), `symbol', `symbol_position' (left | right,
%% default left), `allow_null' (default true: blank stays blank),
%% `label' (floating).
-spec number_input(number() | binary() | undefined, aihtml_html:css(),
                   aihtml_html:attrs()) -> aihtml_html:element().
number_input(Value0, Css, Attrs0) ->
    Entry = aihtml_catalog:entry(?MODULE, number_input),
    {Opts, Attrs} = aihtml_catalog:split_options(Entry, Attrs0),
    Flags = aihtml_catalog:flags(Entry, Css),
    Min = num_opt(min, Opts),
    Max = num_opt(max, Opts),
    Step = case num_opt(step, Opts) of
               undefined -> 1;
               S when S > 0 -> S;
               S -> error({aihtml, {bad_option, number_input, step, S}})
           end,
    Decimals = case maps:get(decimals, Opts, undefined) of
                   undefined -> decimals_of(Step);
                   D when is_integer(D), D >= 0 -> D;
                   D -> error({aihtml, {bad_option, number_input, decimals, D}})
               end,
    AllowNull = maps:get(allow_null, Opts, true),
    Value = case parse_number(Value0) of
                undefined when AllowNull =:= false -> clamp(0, Min, Max);
                undefined -> undefined;
                N -> clamp(N, Min, Max)
            end,
    Text = format_number(Value, Decimals),
    Label = maps:get(label, Opts, undefined),
    Id = input_id(Attrs, Label),
    ReadOnly = lists:member(readonly, Flags),
    Native = ?H:void(input, [<<"ah-numinput-input">>],
                     [[{type, text}, {inputmode, decimal}, {role, spinbutton},
                       {autocomplete, off}, {spellcheck, <<"false">>},
                       {value, Text}, {id, Id},
                       {aria_valuemin, Min}, {aria_valuemax, Max},
                       {aria_valuenow, Value},
                       {aria_invalid, lists:member(invalid, modifiers(Css)) andalso <<"true">>},
                       {disabled, lists:member(disabled, Flags)},
                       {readonly, ReadOnly}],
                      no_placeholder(Label, Attrs)]),
    Symbol = maps:get(symbol, Opts, undefined),
    Left = maps:get(symbol_position, Opts, left) =:= left,
    Spin = case maps:get(spin, Opts, true) of
               false -> [];
               _ -> ?H:el('div',
                          [?H:el(span, {safe, <<"&#9650;">>}, [<<"ah-numinput-spin-up">>],
                                 [{aria_hidden, <<"true">>}]),
                           ?H:el(span, {safe, <<"&#9660;">>}, [<<"ah-numinput-spin-down">>],
                                 [{aria_hidden, <<"true">>}])],
                          [<<"ah-numinput-spin">>], [])
           end,
    Row = ?H:el('div',
                [[?H:el(span, Symbol, [<<"ah-numinput-prefix">>], [])
                  || Symbol =/= undefined, Left],
                 Native, Spin,
                 [?H:el(span, Symbol, [<<"ah-numinput-suffix">>], [])
                  || Symbol =/= undefined, not Left]],
                [<<"ah-numinput-row">>], []),
    ?H:el('div', [Row, float_label(<<"ah-numinput">>, Label, Id, Text)],
          aihtml_catalog:classes(Entry, Css),
          [{data_ah, <<"number-input">>},
           {data_min, Min}, {data_max, Max}, {data_step, Step},
           {data_decimals, Decimals}, {data_allow_null, atom_to_binary(AllowNull =/= false)}]).

num_opt(K, Opts) ->
    case maps:get(K, Opts, undefined) of
        undefined -> undefined;
        N when is_number(N) -> N;
        B when is_binary(B) ->
            case parse_number(B) of
                undefined -> error({aihtml, {bad_option, number_input, K, B}});
                N -> N
            end;
        X -> error({aihtml, {bad_option, number_input, K, X}})
    end.

parse_number(undefined) -> undefined;
parse_number(N) when is_number(N) -> N;
parse_number(B) when is_binary(B) ->
    S = string:trim(binary_to_list(B)),
    case {string:to_float(S), string:to_integer(S)} of
        {{F, []}, _} -> F;
        {_, {I, []}} when is_integer(I) -> I;
        _ when S =:= [] -> undefined;
        _ -> error({aihtml, {bad_number, B}})
    end;
parse_number(X) -> error({aihtml, {bad_number, X}}).

clamp(N, Min, Max) ->
    N1 = case Min of undefined -> N; _ -> max(N, Min) end,
    case Max of undefined -> N1; _ -> min(N1, Max) end.

decimals_of(Step) when is_integer(Step) -> 0;
decimals_of(Step) ->
    case binary:split(float_to_binary(Step, [short]), <<".">>) of
        [_, <<"0">>] -> 0;
        [_, Frac] -> byte_size(Frac);
        [_] -> 0
    end.

format_number(undefined, _) -> <<>>;
format_number(N, 0) -> integer_to_binary(round(N));
format_number(N, D) -> float_to_binary(float(N), [{decimals, D}]).

%%%===================================================================
%%% input_otp
%%%===================================================================

%% @doc One box per character (sigil input_otp). Typing moves forward,
%% Backspace clears and moves back, arrows move, a paste fills the boxes.
%% Options: `pattern' (digit | alphanumeric, default digit),
%% `separator_at' (a dash after that many boxes). Fires `change' on the
%% root, and `ah:complete' when every box is filled.
-spec input_otp(pos_integer(), binary() | undefined, aihtml_html:css(),
                aihtml_html:attrs()) -> aihtml_html:element().
input_otp(Length, Value0, Css, Attrs0) when is_integer(Length), Length > 0 ->
    Entry = aihtml_catalog:entry(?MODULE, input_otp),
    {Opts, Attrs1} = aihtml_catalog:split_options(Entry, Attrs0),
    {Name, Attrs} = take_name(Attrs1),
    Disabled = lists:member(disabled, aihtml_catalog:flags(Entry, Css)),
    Pattern = case maps:get(pattern, Opts, digit) of
                  P when P =:= digit; P =:= alphanumeric -> P;
                  P -> error({aihtml, {bad_option, input_otp, pattern, P}})
              end,
    Sep = maps:get(separator_at, Opts, undefined),
    Value = sanitize_otp(Value0, Pattern, Length),
    Chars = [binary:part(Value, I, 1) || I <- lists:seq(0, byte_size(Value) - 1)],
    Slots = [begin
                 Ch = case I < length(Chars) of
                          true -> lists:nth(I + 1, Chars);
                          false -> <<>>
                      end,
                 [?H:void(input, [<<"ah-input-otp__slot">>],
                          [{type, text},
                           {inputmode, case Pattern of digit -> numeric; _ -> text end},
                           {autocomplete, I =:= 0 andalso <<"one-time-code">>},
                           {maxlength, 1}, {data_index, I},
                           {data_filled, atom_to_binary(Ch =/= <<>>)},
                           {value, Ch}, {disabled, Disabled},
                           {aria_label, <<"Character ", (integer_to_binary(I + 1))/binary,
                                          " of ", (integer_to_binary(Length))/binary>>}]),
                  [?H:el(span, <<"-">>, [<<"ah-input-otp__separator">>],
                         [{aria_hidden, <<"true">>}])
                   || Sep =:= I + 1, I + 1 < Length]]
             end || I <- lists:seq(0, Length - 1)],
    ?H:el('div', [Slots, hidden(Name, Value, Disabled)],
          aihtml_catalog:classes(Entry, Css),
          [[{data_ah, <<"input-otp">>}, {role, group},
            {data_ah_value, Value}, {data_length, Length}, {data_pattern, Pattern},
            {data_disabled, atom_to_binary(Disabled)},
            {data_complete, atom_to_binary(byte_size(Value) =:= Length)}],
           Attrs]);
input_otp(Length, _, _, _) ->
    error({aihtml, {bad_length, input_otp, Length}}).

sanitize_otp(undefined, _, _) -> <<>>;
sanitize_otp(V, Pattern, Length) ->
    B = unicode:characters_to_binary(V),
    Ok = << <<C>> || <<C>> <= B, otp_char(C, Pattern) >>,
    binary:part(Ok, 0, min(Length, byte_size(Ok))).

otp_char(C, _) when C >= $0, C =< $9 -> true;
otp_char(C, alphanumeric) -> (C >= $a andalso C =< $z) orelse (C >= $A andalso C =< $Z);
otp_char(_, _) -> false.

%%%===================================================================
%%% tag_input
%%%===================================================================

%% @doc Chips plus a text field (sigil tag_input). Enter or comma adds the
%% typed tag, Backspace in the empty field removes the last one, leaving
%% the field adds what was typed, a pasted list is split on commas and
%% new lines; duplicates are dropped. Options: `placeholder' (default
%% "Add tag…"), `max_tags', `allow_duplicates' (default false),
%% `chip_color' (default primary), `chip_variant' (default soft).
%% `data-ah-value' is the tags joined with commas.
-spec tag_input([binary()], aihtml_html:css(), aihtml_html:attrs()) ->
          aihtml_html:element().
tag_input(Tags0, Css, Attrs0) when is_list(Tags0) ->
    Entry = aihtml_catalog:entry(?MODULE, tag_input),
    {Opts, Attrs1} = aihtml_catalog:split_options(Entry, Attrs0),
    {Name, Attrs} = take_name(Attrs1),
    Disabled = lists:member(disabled, aihtml_catalog:flags(Entry, Css)),
    Color = maps:get(chip_color, Opts, primary),
    Variant = maps:get(chip_variant, Opts, soft),
    Tags = [unicode:characters_to_binary(T) || T <- Tags0],
    Chips = [aihtml_tpl:safe(tpl_tag_input_chip(#{variant => Variant, color => Color,
                                                  index => I - 1, label => T,
                                                  disabled => Disabled}))
             || {I, T} <- lists:zip(lists:seq(1, length(Tags)), Tags)],
    Field = ?H:void(input, [<<"ah-tag-input__field">>],
                    [{type, text},
                     {placeholder, maps:get(placeholder, Opts, <<"Add tag…"/utf8>>)},
                     {aria_label, maps:get(placeholder, Opts, <<"Add tag">>)},
                     {disabled, Disabled}]),
    Value = iolist_to_binary(lists:join(<<",">>, Tags)),
    ?H:el('div', [Chips, Field, hidden(Name, Value, Disabled)],
          aihtml_catalog:classes(Entry, Css),
          [[{data_ah, <<"tag-input">>}, {role, group},
            {data_ah_value, Value},
            {data_disabled, atom_to_binary(Disabled)},
            {data_chip_color, Color}, {data_chip_variant, Variant},
            {data_max_tags, maps:get(max_tags, Opts, undefined)},
            {data_allow_duplicates, maps:get(allow_duplicates, Opts, false) =:= true}],
           Attrs]).

%%%===================================================================
%%% Shared helpers
%%%===================================================================

%% The Css atoms (modifiers) only.
modifiers(Css) ->
    [A || A <- lists:flatten([Css]), is_atom(A)].

has_value(undefined) -> false;
has_value(V) -> iolist_size(text(V)) > 0.

text(undefined) -> <<>>;
text(V) when is_binary(V) -> V;
text(V) when is_list(V) -> unicode:characters_to_binary(V);
text(V) when is_number(V); is_atom(V) -> beamai_html_escape:to_binary(V, aihtml).

%% The native control's id: the one in Attrs, or a generated one when a
%% label must point at it.
input_id(Attrs, Label) ->
    case lists:keyfind(<<"id">>, 1, ?H:attrs(Attrs)) of
        {_, Id} -> Id;
        false when Label =:= undefined -> undefined;
        false -> <<"ah-in-", (integer_to_binary(erlang:unique_integer([positive])))/binary>>
    end.

float_label(_Prefix, undefined, _Id, _Value) -> [];
float_label(Prefix, Label, Id, Value) ->
    ?H:el(label, Label,
          [<<Prefix/binary, "-label">>,
           [<<Prefix/binary, "-label-float">> || has_value(Value)]],
          [{for, Id}]).

%% A floating label takes the placeholder's place (sigil does the same).
no_placeholder(undefined, Attrs) -> Attrs;
no_placeholder(_Label, Attrs) ->
    lists:keydelete(<<"placeholder">>, 1, ?H:attrs(Attrs)).

take_name(Attrs) ->
    Norm = ?H:attrs(Attrs),
    case lists:keytake(<<"name">>, 1, Norm) of
        {value, {_, Name}, Rest} -> {Name, Rest};
        false -> {undefined, Norm}
    end.

hidden(undefined, _Value, _Disabled) -> [];
hidden(Name, Value, Disabled) ->
    ?H:void(input, [], [{type, hidden}, {name, Name}, {value, Value},
                        {disabled, Disabled}]).

%% Modifier classes of the sigil input family: `<prefix>-<modifier>',
%% underscores written as dashes (no_rounded -> ah-input-no-rounded).
family(Prefix, Mods) ->
    maps:from_list(
      [{M, [<<Prefix/binary, "-",
              (binary:replace(atom_to_binary(M), <<"_">>, <<"-">>, [global]))/binary>>]}
       || M <- Mods]).

-define(SIZES, {[sm, lg], none}).
-define(STATES, {[invalid, valid], none}).

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => input, category => form,
       signature => <<"input(Value, Css, Attrs)">>,
       root => <<"ah-input-group">>,
       groups => #{size => ?SIZES, state => ?STATES},
       flags => [disabled, no_rounded, clearable],
       classes => family(<<"ah-input">>, [sm, lg, invalid, valid, disabled,
                                          no_rounded, clearable]),
       options => [prefix, suffix, label],
       behavior => <<"input">>,
       events => [<<"input">>, <<"change">>],
       doc => <<"Text input with sizes, valid/invalid states, prefix/suffix "
                "addons, a clear button and a floating label.">>},
     #{name => textarea, category => form,
       signature => <<"textarea(Value, Css, Attrs)">>,
       root => <<"ah-textarea-group">>,
       groups => #{size => ?SIZES, state => ?STATES},
       flags => [disabled, no_rounded],
       classes => family(<<"ah-input">>, [sm, lg, invalid, valid, disabled, no_rounded]),
       options => [label],
       behavior => <<"input">>,
       events => [<<"input">>, <<"change">>],
       doc => <<"Multi-line text input styled like input.">>},
     #{name => password_input, category => form,
       signature => <<"password_input(Value, Css, Attrs)">>,
       root => <<"ah-pwd-group">>,
       groups => #{size => ?SIZES, state => ?STATES},
       flags => [disabled],
       classes => family(<<"ah-pwd">>, [sm, lg, invalid, valid, disabled]),
       options => [toggle, strength, label],
       behavior => <<"password-input">>,
       events => [<<"input">>, <<"change">>],
       doc => <<"Password field with a show/hide toggle and an optional "
                "strength meter.">>},
     #{name => number_input, category => form,
       signature => <<"number_input(Value, Css, Attrs)">>,
       root => <<"ah-numinput-group">>,
       groups => #{size => ?SIZES, state => ?STATES},
       flags => [disabled, readonly],
       classes => family(<<"ah-numinput">>, [sm, lg, invalid, valid, disabled, readonly]),
       options => [min, max, step, decimals, spin, symbol, symbol_position,
                   allow_null, label],
       behavior => <<"number-input">>,
       events => [<<"input">>, <<"change">>],
       doc => <<"Numeric field with spin buttons, arrow keys and the mouse "
                "wheel, clamped to min/max.">>},
     #{name => input_otp, category => form,
       signature => <<"input_otp(Length, Value, Css, Attrs)">>,
       root => <<"ah-input-otp">>,
       flags => [disabled],
       classes => #{disabled => []},
       options => [pattern, separator_at],
       behavior => <<"input-otp">>,
       events => [<<"change">>, <<"ah:complete">>],
       doc => <<"One-time code entry, one box per character; paste fills "
                "every box.">>},
     #{name => tag_input, category => form,
       signature => <<"tag_input(Tags, Css, Attrs)">>,
       root => <<"ah-tag-input">>,
       flags => [disabled],
       classes => #{disabled => []},
       options => [placeholder, max_tags, allow_duplicates, chip_color, chip_variant],
       behavior => <<"tag-input">>,
       events => [<<"change">>],
       doc => <<"Chips plus a text field: Enter or comma adds a tag, "
                "Backspace removes the last.">>}].

%%%===================================================================
%%% Examples
%%%===================================================================

-spec examples() -> [{atom(), binary(), aihtml_html:html()}].
examples() ->
    Row = fun(Items) -> ?H:el('div', Items, [<<"flex flex-wrap items-start gap-4">>], []) end,
    Col = fun(Items) -> ?H:el('div', Items, [<<"flex flex-col gap-3">>], []) end,
    Search = {safe, <<"<svg width=\"14\" height=\"14\" viewBox=\"0 0 24 24\" fill=\"none\" "
                      "stroke=\"currentColor\" stroke-width=\"2\" aria-hidden=\"true\">"
                      "<circle cx=\"11\" cy=\"11\" r=\"7\"/><path d=\"m20 20-3.5-3.5\"/></svg>">>},
    [{input, <<"Sizes, states and addons">>,
      Col([Row([input(undefined, [sm, <<"w-56">>], [{placeholder, <<"Small">>}]),
                input(undefined, [<<"w-56">>], [{placeholder, <<"Medium">>}, {name, q}]),
                input(undefined, [lg, <<"w-56">>], [{placeholder, <<"Large">>}])]),
           Row([input(<<"not-an-email">>, [invalid, <<"w-56">>], []),
                input(<<"ada@example.com">>, [valid, <<"w-56">>], []),
                input(<<"Disabled">>, [disabled, <<"w-56">>], [])]),
           Row([input(undefined, [<<"w-56">>], [{prefix, <<"https://">>},
                                                {placeholder, <<"example.com">>}]),
                input(<<"42">>, [<<"w-40">>], [{suffix, <<"kg">>}]),
                input(undefined, [clearable, <<"w-56">>], [{prefix, Search},
                                                          {placeholder, <<"Search">>}]),
                input(<<"Clear me">>, [clearable, <<"w-48">>], [{id, <<"ex-clear">>}])]),
           Row([input(undefined, [<<"w-56">>], [{label, <<"Floating label">>}]),
                input(<<"Filled <b>&</b>">>, [<<"w-56">>], [{label, <<"With value">>}]),
                input(undefined, [no_rounded, <<"w-56">>], [{placeholder, <<"No rounding">>}])])])},
     {textarea, <<"Multi-line">>,
      Row([textarea(undefined, [<<"w-72">>], [{placeholder, <<"Write something...">>}]),
           textarea(<<"Line one\nLine </textarea> two">>, [invalid, <<"w-72">>], [{rows, 4}]),
           textarea(undefined, [sm, <<"w-60">>], [{label, <<"Notes">>}]),
           textarea(<<"Read only">>, [disabled, <<"w-60">>], [])])},
     {password_input, <<"Reveal toggle and strength">>,
      Col([Row([password_input(<<"secret123">>, [<<"w-60">>], [{name, password}]),
                password_input(undefined, [<<"w-60">>],
                               [{strength, true}, {placeholder, <<"New password">>}]),
                password_input(undefined, [<<"w-60">>], [{label, <<"Password">>}])]),
           Row([password_input(<<"x">>, [sm, invalid, <<"w-60">>], []),
                password_input(undefined, [lg, <<"w-60">>], [{toggle, false},
                                                            {placeholder, <<"No toggle">>}]),
                password_input(<<"hunter2">>, [disabled, <<"w-60">>], [])])])},
     {number_input, <<"Spin buttons, min/max/step, symbols">>,
      Col([Row([number_input(5, [<<"w-40">>], [{min, 0}, {max, 10}, {name, qty},
                                               {id, <<"ex-num">>}]),
                number_input(<<"19.5">>, [<<"w-44">>], [{step, 0.5}, {symbol, <<"$">>}]),
                number_input(75, [<<"w-40">>], [{min, 0}, {max, 100},
                                                {symbol, <<"%">>}, {symbol_position, right}]),
                number_input(undefined, [<<"w-40">>], [{spin, false},
                                                       {placeholder, <<"No spin">>}])]),
           Row([number_input(1, [sm, <<"w-32">>], []),
                number_input(2, [lg, <<"w-40">>], []),
                number_input(200, [invalid, <<"w-40">>], [{max, 100}]),
                number_input(3, [readonly, <<"w-32">>], []),
                number_input(4, [disabled, <<"w-32">>], []),
                number_input(undefined, [<<"w-40">>], [{label, <<"Amount">>}])])])},
     {input_otp, <<"Digits, separator, alphanumeric">>,
      Col([input_otp(6, undefined, [], [{name, code}, {id, <<"ex-otp">>}]),
           input_otp(6, <<"123456">>, [], [{separator_at, 3}]),
           input_otp(4, <<"A1">>, [], [{pattern, alphanumeric}]),
           input_otp(4, <<"12">>, [disabled], [])])},
     {tag_input, <<"Add with Enter or comma, Backspace removes">>,
      Col([tag_input([<<"erlang">>, <<"jquery">>, <<"<tailwind>">>], [<<"w-96">>],
                     [{name, tags}, {id, <<"ex-tags">>}]),
           tag_input([<<"red">>], [<<"w-96">>], [{chip_color, error}, {chip_variant, filled},
                                                 {max_tags, 3},
                                                 {placeholder, <<"Up to 3 tags">>}]),
           tag_input([<<"locked">>], [disabled, <<"w-96">>], [])])}].
