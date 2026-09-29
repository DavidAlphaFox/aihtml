%%%-------------------------------------------------------------------
%%% @doc Several lines of text, styled like input (sigil has no textarea
%%% widget; this follows its a2ui textarea). See designs/04-components.md.
%%%
%%% The native control is kept: `Attrs' (name, placeholder, rows, on(...),
%%% ...) go to the `<textarea>', `Css' to the wrapper. It shares the
%%% `input' behaviour.
%%%
%%% textarea/3 builds an #ah_textarea{} (include/aihtml_textarea.hrl) and
%%% render/1 turns it into HTML, so pages may also write the record
%%% directly (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_textarea).
-behaviour(aihtml_element).

-include("aihtml_textarea.hrl").

-export([textarea/3, render/1, fields/1, catalog/0]).

-define(H, aihtml_html).
-define(L, aihtml_lib_input).
-define(M(Name, Args, Doc), #{name => Name, args => Args, doc => Doc}).

%% @doc A multi-line input, styled like `input' (sigil has no textarea
%% widget; this follows its a2ui textarea). Option: `label' (floating).
-spec textarea(binary() | undefined, aihtml_html:css(), aihtml_html:attrs()) -> #ah_textarea{}.
textarea(Value, Css, Attrs) ->
    aihtml_element:build(?MODULE, #ah_textarea{value = Value}, Css, Attrs).

%% @doc The field names of #ah_textarea{}.
-spec fields(atom()) -> [atom()].
fields(ah_textarea) -> record_info(fields, ah_textarea).

-spec render(#ah_textarea{}) -> aihtml_html:html().
render(#ah_textarea{value = Value, label = Label} = R) ->
    Classes = ?L:classes(?MODULE, R),
    Id = ?L:input_id(R, Label),
    Native = ?H:el(textarea, ?L:text(Value), [<<"ah-input">>, <<"ah-textarea">>],
                   [[{rows, 3}, {id, Id},
                     {aria_invalid, R#ah_textarea.state =:= invalid andalso <<"true">>},
                     {disabled, R#ah_textarea.disabled}],
                    ?L:native_attrs(R, Label)]),
    ?H:el('div', [Native, ?L:float_label(<<"ah-input">>, Label, Id, Value)],
          [<<"ah-input-group">>, Classes],
          [{data_ah, <<"input">>}]).

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => textarea, category => form,
       signature => <<"textarea(Value, Css, Attrs)">>,
       root => <<"ah-textarea-group">>,
       groups => #{size => ?L:sizes(), state => ?L:states()},
       flags => [disabled, no_rounded],
       classes => ?L:family(<<"ah-input">>, [sm, lg, invalid, valid, disabled, no_rounded]),
       options => [label],
       behavior => <<"input">>,
       events => [<<"input">>, <<"change">>],
       doc => <<"Multi-line text input styled like input.">>,
       option_docs => maps:merge(?L:field_docs(), #{no_rounded => <<"Square corners.">>}),
       methods => [?M(getValue, <<"()">>, <<"Return the value.">>),
                   ?M(setValue, <<"(Value)">>, <<"Set the value without firing events.">>),
                   ?M(clear, <<"()">>, <<"Empty the field, firing input and change.">>),
                   ?M(focus, <<"()">>, <<"Focus the textarea.">>)]}].
