%%%-------------------------------------------------------------------
%%% @doc A numeric field with spin buttons, ported from sigil
%%% (form/number_input). See designs/04-components.md.
%%%
%%% The native control is kept: `Attrs' (name, placeholder, on(...), ...)
%%% go to the `<input>', `Css' to the wrapper.
%%%
%%% ah_number_input/3 builds an #ah_number_input{}
%%% (include/aihtml_number_input.hrl) and render/1 turns it into HTML, so
%%% pages may also write the record directly (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_number_input).
-behaviour(aihtml_element).

-include("aihtml_number_input.hrl").

-export([ah_number_input/3, render/1, fields/1, catalog/0]).

-export_type([numeric/0]).

-define(H, aihtml_html).
-define(L, aihtml_lib_input).
-define(M(Name, Args, Doc), #{name => Name, args => Args, doc => Doc}).

%% A number, or its text.
-type numeric() :: number() | binary().

%% Records may be written by hand, so render/1 still checks the numbers
%% that the field types already rule out.
-dialyzer({no_match, [num_opt/2, parse_number/1]}).

%% @doc A numeric field with sigil's spin buttons. The native input keeps
%% the plain number (no digit grouping) so forms submit it as is. Options:
%% `min', `max', `step' (default 1), `decimals' (default: those of step),
%% `spin' (default true), `symbol', `symbol_position' (left | right,
%% default left), `allow_null' (default true: blank stays blank),
%% `label' (floating).
-spec ah_number_input(number() | binary() | undefined, aihtml_html:css(), aihtml_html:attrs()) ->
          #ah_number_input{}.
ah_number_input(Value, Css, Attrs) ->
    aihtml_element:build(?MODULE, #ah_number_input{value = Value}, Css, Attrs).

%% @doc The field names of #ah_number_input{}.
-spec fields(atom()) -> [atom()].
fields(ah_number_input) -> record_info(fields, ah_number_input).

-spec render(#ah_number_input{}) -> aihtml_html:html().
render(#ah_number_input{value = Value0, label = Label, allow_null = AllowNull,
                        symbol = Symbol} = R) ->
    Classes = ?L:classes(?MODULE, R),
    Min = num_opt(min, R#ah_number_input.min),
    Max = num_opt(max, R#ah_number_input.max),
    Step = case num_opt(step, R#ah_number_input.step) of
               undefined -> 1;
               S when S > 0 -> S;
               S -> error({aihtml, {bad_option, number_input, step, S}})
           end,
    Decimals = case R#ah_number_input.decimals of
                   undefined -> decimals_of(Step);
                   D when is_integer(D), D >= 0 -> D;
                   D -> error({aihtml, {bad_option, number_input, decimals, D}})
               end,
    Value = case parse_number(Value0) of
                undefined when AllowNull =:= false -> clamp(0, Min, Max);
                undefined -> undefined;
                N -> clamp(N, Min, Max)
            end,
    Text = format_number(Value, Decimals),
    Id = ?L:input_id(R, Label),
    Native = ?H:void(input, [<<"ah-numinput-input">>],
                     [[{type, text}, {inputmode, decimal}, {role, spinbutton},
                       {autocomplete, off}, {spellcheck, <<"false">>},
                       {value, Text}, {id, Id},
                       {aria_valuemin, Min}, {aria_valuemax, Max},
                       {aria_valuenow, Value},
                       {aria_invalid, R#ah_number_input.state =:= invalid andalso <<"true">>},
                       {disabled, R#ah_number_input.disabled},
                       {readonly, R#ah_number_input.readonly}],
                      ?L:native_attrs(R, Label)]),
    Left = R#ah_number_input.symbol_position =:= left,
    Spin = case R#ah_number_input.spin of
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
    ?H:el('div', [Row, ?L:float_label(<<"ah-numinput">>, Label, Id, Text)],
          Classes,
          [{data_ah, <<"number-input">>},
           {data_min, Min}, {data_max, Max}, {data_step, Step},
           {data_decimals, Decimals}, {data_allow_null, atom_to_binary(AllowNull =/= false)}]).

num_opt(_K, undefined) -> undefined;
num_opt(_K, N) when is_number(N) -> N;
num_opt(K, B) when is_binary(B) ->
    case parse_number(B) of
        undefined -> error({aihtml, {bad_option, number_input, K, B}});
        N -> N
    end;
num_opt(K, X) -> error({aihtml, {bad_option, number_input, K, X}}).

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

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => number_input, category => form,
       signature => <<"ah_number_input(Value, Css, Attrs)">>,
       root => <<"ah-numinput-group">>,
       groups => #{size => ?L:sizes(), state => ?L:states()},
       flags => [disabled, readonly],
       classes => ?L:family(<<"ah-numinput">>, [sm, lg, invalid, valid, disabled, readonly]),
       options => [min, max, step, decimals, spin, symbol, symbol_position,
                   allow_null, label],
       behavior => <<"number-input">>,
       events => [<<"input">>, <<"change">>],
       doc => <<"Numeric field with spin buttons, arrow keys and the mouse "
                "wheel, clamped to min/max.">>,
       option_docs => maps:merge(?L:field_docs(),
                                 #{readonly => <<"Read only; hides the spin buttons.">>,
                                   min => <<"Lowest value; input is clamped to it.">>,
                                   max => <<"Highest value; input is clamped to it.">>,
                                   step => <<"Amount per spin, arrow key or wheel notch (default 1); "
                                             "PageUp/PageDown step ten times.">>,
                                   decimals => <<"Digits after the point (default: those of step).">>,
                                   spin => <<"Show the spin buttons (default true).">>,
                                   symbol => <<"Text shown beside the field, e.g. $ or %.">>,
                                   symbol_position => <<"left (default) or right.">>,
                                   allow_null => <<"Keep a blank field blank (default true); "
                                                   "false turns it into 0.">>}),
       methods => [?M(getValue, <<"()">>, <<"Return the number, or null when blank.">>),
                   ?M(setValue, <<"(Number)">>, <<"Set, clamp and format; fires change if it differs.">>),
                   ?M(stepUp, <<"()">>, <<"Add one step, firing input and change.">>),
                   ?M(stepDown, <<"()">>, <<"Subtract one step, firing input and change.">>),
                   ?M(clear, <<"()">>, <<"Blank the field (or 0 without allow_null).">>),
                   ?M(focus, <<"()">>, <<"Focus the input.">>)]}].
