%%%-------------------------------------------------------------------
%%% @doc One line of text, ported from sigil (form/input). See
%%% designs/04-components.md.
%%%
%%% The native control is kept: `Attrs' (name, placeholder, on(...), ...)
%%% go to the `<input>', `Css' to the wrapper.
%%%
%%% input/3 builds an #ah_input{} (include/aihtml_input.hrl) and render/1
%%% turns it into HTML, so pages may also write the record directly
%%% (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_input).
-behaviour(aihtml_element).

-include("aihtml_input.hrl").

-export([input/3, render/1, fields/1, catalog/0]).

-define(H, aihtml_html).
-define(L, aihtml_lib_input).
-define(M(Name, Args, Doc), #{name => Name, args => Args, doc => Doc}).

%% @doc A text input in sigil's `.ah-input-group' shell. Options:
%% `prefix', `suffix' (any html: text or an icon), `label' (a floating
%% label; replaces the placeholder).
-spec input(binary() | undefined, aihtml_html:css(), aihtml_html:attrs()) -> #ah_input{}.
input(Value, Css, Attrs) ->
    aihtml_element:build(?MODULE, #ah_input{value = Value}, Css, Attrs).

%% @doc The field names of #ah_input{}.
-spec fields(atom()) -> [atom()].
fields(ah_input) -> record_info(fields, ah_input).

-spec render(#ah_input{}) -> aihtml_html:html().
render(#ah_input{value = Value, disabled = Disabled, clearable = Clearable,
                 prefix = Prefix, suffix = Suffix, label = Label} = R) ->
    Classes = ?L:classes(?MODULE, R),
    Id = ?L:input_id(R, Label),
    Native = ?H:void(input, [<<"ah-input">>],
                     [[{type, text}, {value, Value}, {id, Id},
                       {aria_invalid, R#ah_input.state =:= invalid andalso <<"true">>},
                       {disabled, Disabled}],
                      ?L:native_attrs(R, Label)]),
    Clear = [clear_button() || Clearable],
    Body = case Prefix =/= undefined orelse Suffix =/= undefined orelse Clearable of
               false -> Native;
               true ->
                   ?H:el('div',
                         [addon(<<"prefix">>, Prefix), Native, Clear,
                          addon(<<"suffix">>, Suffix)],
                         [<<"ah-input-row">>], [])
           end,
    ?H:el('div', [Body, ?L:float_label(<<"ah-input">>, Label, Id, Value)],
          [Classes, [<<"ah-input-has-value">> || ?L:has_value(Value)]],
          [{data_ah, <<"input">>}]).

addon(_Side, undefined) -> [];
addon(Side, Html) ->
    ?H:el(span, Html, [<<"ah-input-addon">>, <<"ah-input-addon-", Side/binary>>], []).

clear_button() ->
    ?H:el(button, {safe, <<"&times;">>}, [<<"ah-input-clear">>],
          [{type, button}, {tabindex, <<"-1">>}, {aria_label, <<"Clear">>}]).

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => input, category => form,
       signature => <<"input(Value, Css, Attrs)">>,
       root => <<"ah-input-group">>,
       groups => #{size => ?L:sizes(), state => ?L:states()},
       flags => [disabled, no_rounded, clearable],
       classes => ?L:family(<<"ah-input">>, [sm, lg, invalid, valid, disabled,
                                             no_rounded, clearable]),
       options => [prefix, suffix, label],
       behavior => <<"input">>,
       events => [<<"input">>, <<"change">>],
       doc => <<"Text input with sizes, valid/invalid states, prefix/suffix "
                "addons, a clear button and a floating label.">>,
       option_docs => maps:merge(?L:field_docs(),
                                 #{no_rounded => <<"Square corners.">>,
                                   clearable => <<"A clear button (and Escape) empties the field, "
                                                  "firing input and change.">>,
                                   prefix => <<"HTML before the input: text or an icon.">>,
                                   suffix => <<"HTML after the input.">>}),
       methods => [?M(getValue, <<"()">>, <<"Return the value.">>),
                   ?M(setValue, <<"(Value)">>, <<"Set the value without firing events.">>),
                   ?M(clear, <<"()">>, <<"Empty the field, firing input and change.">>),
                   ?M(focus, <<"()">>, <<"Focus the input.">>),
                   ?M(selectAll, <<"()">>, <<"Focus the input and select its text.">>)]}].
