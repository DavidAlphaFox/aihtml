%%%-------------------------------------------------------------------
%%% @doc A text field with an input mask, ported from sigil
%%% (form/masked_input). See designs/04-components.md.
%%%
%%% A value-bearing component: `Attrs' go to the root, which carries
%%% `data-ah-value' and fires `change' (and `input' while the value is
%%% being edited); `name' goes to a hidden input. The server renders the
%%% complete first state (the masked text), so nothing needs to be laid out
%%% in the browser before it is shown. The behaviour lives in
%%% assets/js/components/masked_input.ts.
%%%
%%% masked_input/3 builds an #ah_masked_input{}
%%% (include/aihtml_masked_input.hrl) and render/1 turns it into HTML, so
%%% pages may also write the record directly (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_masked_input).
-behaviour(aihtml_element).

-include("aihtml_masked_input.hrl").

-export([masked_input/3, render/1, fields/1, catalog/0]).

-define(H, aihtml_html).
-define(E, aihtml_element).

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
    ?E:build(?MODULE, #ah_masked_input{value = Value}, Css, Attrs).

%% @doc The field names of #ah_masked_input{}.
-spec fields(atom()) -> [atom()].
fields(ah_masked_input) -> record_info(fields, ah_masked_input).

-spec render(#ah_masked_input{}) -> aihtml_html:html().
render(#ah_masked_input{value = Value0, name = Name, disabled = Disabled,
                               readonly = Readonly, floating_label = Floating,
                               mask = Mask0, prompt_char = Prompt0,
                               placeholder = Placeholder} = R) ->
    Classes = ?E:classes(?MODULE, R),
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

hidden(undefined, _) -> [];
hidden(Name, Value) -> ?H:void(input, [], [{type, hidden}, {name, Name}, {value, Value}]).

text(undefined) -> <<>>;
text(B) when is_binary(B) -> B;
text(L) when is_list(L) -> unicode:characters_to_binary(L);
text(X) -> beamai_html_escape:to_binary(X, aihtml).

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
            #{name => focus, args => <<"()">>, doc => <<"Focus the field.">>}]}].
