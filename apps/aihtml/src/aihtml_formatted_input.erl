%%%-------------------------------------------------------------------
%%% @doc An integer field in radix 2, 8, 10 or 16, ported from sigil
%%% (form/formatted_input). See designs/04-components.md.
%%%
%%% A value-bearing component: `Attrs' go to the root, which carries
%%% `data-ah-value' and fires `change' (and `input' while the value is
%%% being edited); `name' goes to a hidden input. The server renders the
%%% complete first state (the number in its radix), so nothing needs to be
%%% laid out in the browser before it is shown. The behaviour lives in
%%% assets/js/components/formatted_input.ts.
%%%
%%% formatted_input/3 builds an #ah_formatted_input{}
%%% (include/aihtml_formatted_input.hrl) and render/1 turns it into HTML,
%%% so pages may also write the record directly (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_formatted_input).
-behaviour(aihtml_element).

-include("aihtml_formatted_input.hrl").

-export([formatted_input/3, render/1, fields/1, catalog/0]).

-export_type([integer_value/0, radix/0]).

-define(H, aihtml_html).
-define(E, aihtml_element).
-define(RADIXES, [{2, <<"BIN">>, <<"Binary">>}, {8, <<"OCT">>, <<"Octal">>},
                  {10, <<"DEC">>, <<"Decimal">>}, {16, <<"HEX">>, <<"Hexadecimal">>}]).

%% Radix of a formatted_input: binary, octal, decimal, hexadecimal.
-type radix() :: 2 | 8 | 10 | 16.
%% An integer, or its decimal text (<<"-42">>, "12345678901234567890").
-type integer_value() :: integer() | binary() | string().

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
-spec formatted_input(integer_value(), aihtml_html:css(), aihtml_html:attrs()) ->
          #ah_formatted_input{}.
formatted_input(Value, Css, Attrs) ->
    ?E:build(?MODULE, #ah_formatted_input{value = Value}, Css, Attrs).

%% @doc The field names of #ah_formatted_input{}.
-spec fields(atom()) -> [atom()].
fields(ah_formatted_input) -> record_info(fields, ah_formatted_input).

-spec render(#ah_formatted_input{}) -> aihtml_html:html().
render(#ah_formatted_input{value = Value0, name = Name, disabled = Disabled,
                                     radix = Radix, upper_case = Upper,
                                     notation = Notation} = R0) ->
    {Id, R} = ensure_id(R0),
    Classes = ?E:classes(?MODULE, R),
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

%% The parts of a formatted_input refer to each other by id
%% (aria-controls), so a root without one gets one.
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

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => formatted_input, category => form,
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
            #{name => close, args => <<"()">>, doc => <<"Close the radix menu.">>}]}].
