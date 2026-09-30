%%%-------------------------------------------------------------------
%%% @doc One box per character, ported from sigil (form/input_otp). See
%%% designs/04-components.md.
%%%
%%% A value-bearing component: `Attrs' go to the root, which carries
%%% `data-ah-value' and fires `change'; `name' goes to a hidden input.
%%%
%%% ah_input_otp/4 builds an #ah_input_otp{} (include/aihtml_input_otp.hrl)
%%% and render/1 turns it into HTML, so pages may also write the record
%%% directly (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_input_otp).
-behaviour(aihtml_element).

-include("aihtml_input_otp.hrl").

-export([ah_input_otp/4, render/1, fields/1, catalog/0]).

-define(H, aihtml_html).
-define(L, aihtml_lib_input).
-define(M(Name, Args, Doc), #{name => Name, args => Args, doc => Doc}).

%% @doc One box per character (sigil input_otp). Typing moves forward,
%% Backspace clears and moves back, arrows move, a paste fills the boxes.
%% Options: `pattern' (digit | alphanumeric, default digit),
%% `separator_at' (a dash after that many boxes). Fires `change' on the
%% root, and `ah:complete' when every box is filled.
-spec ah_input_otp(pos_integer(), binary() | undefined, aihtml_html:css(), aihtml_html:attrs()) ->
          #ah_input_otp{}.
ah_input_otp(Length, Value, Css, Attrs) when is_integer(Length), Length > 0 ->
    aihtml_element:build(?MODULE, #ah_input_otp{length = Length, value = Value}, Css, Attrs);
ah_input_otp(Length, _, _, _) ->
    error({aihtml, {bad_length, input_otp, Length}}).

%% @doc The field names of #ah_input_otp{}.
-spec fields(atom()) -> [atom()].
fields(ah_input_otp) -> record_info(fields, ah_input_otp).

-spec render(#ah_input_otp{}) -> aihtml_html:html().
render(#ah_input_otp{length = Length, value = Value0, name = Name, disabled = Disabled,
                     pattern = Pattern, separator_at = Sep} = R) ->
    Classes = ?L:classes(?MODULE, R),
    is_integer(Length) andalso Length > 0
        orelse error({aihtml, {bad_length, input_otp, Length}}),
    lists:member(Pattern, [digit, alphanumeric])
        orelse error({aihtml, {bad_option, input_otp, pattern, Pattern}}),
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
                           {aria_label, aihtml_i18n:text(input_otp, character, [I + 1, Length])}]),
                  [?H:el(span, <<"-">>, [<<"ah-input-otp__separator">>],
                         [{aria_hidden, <<"true">>}])
                   || Sep =:= I + 1, I + 1 < Length]]
             end || I <- lists:seq(0, Length - 1)],
    ?H:el('div', [Slots, ?L:hidden(Name, Value, Disabled)],
          Classes,
          [[{data_ah, <<"input-otp">>}, {role, group},
            {data_ah_value, Value}, {data_length, Length}, {data_pattern, Pattern},
            {data_disabled, atom_to_binary(Disabled)},
            {data_complete, atom_to_binary(byte_size(Value) =:= Length)}],
           aihtml_element:root_attrs(R, change)]).

sanitize_otp(undefined, _, _) -> <<>>;
sanitize_otp(V, Pattern, Length) ->
    B = unicode:characters_to_binary(V),
    Ok = << <<C>> || <<C>> <= B, otp_char(C, Pattern) >>,
    binary:part(Ok, 0, min(Length, byte_size(Ok))).

otp_char(C, _) when C >= $0, C =< $9 -> true;
otp_char(C, alphanumeric) -> (C >= $a andalso C =< $z) orelse (C >= $A andalso C =< $Z);
otp_char(_, _) -> false.

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => input_otp, category => form,
       signature => <<"ah_input_otp(Length, Value, Css, Attrs)">>,
       root => <<"ah-input-otp">>,
       flags => [disabled],
       classes => #{disabled => []},
       options => [pattern, separator_at],
       behavior => <<"input-otp">>,
       events => [<<"change">>, <<"ah:complete">>],
       doc => <<"One-time code entry, one box per character; paste fills "
                "every box.">>,
       option_docs => #{disabled => <<"Disable every box and the hidden input.">>,
                        pattern => <<"digit (default) or alphanumeric.">>,
                        separator_at => <<"Put a dash after this many boxes.">>,
                        name => <<"Name of the hidden input that submits the code.">>},
       methods => [?M(getValue, <<"()">>, <<"Return the code typed so far.">>),
                   ?M(setValue, <<"(Code)">>, <<"Fill the boxes; fires change (and ah:complete "
                                                 "when full) if it differs.">>),
                   ?M(clear, <<"()">>, <<"Empty every box, firing change.">>),
                   ?M(focus, <<"()">>, <<"Focus the first empty box.">>),
                   ?M(invalid, <<"([On])">>, <<"Mark the code wrong (red boxes) until the "
                                                "next edit; false clears it.">>)]}].
