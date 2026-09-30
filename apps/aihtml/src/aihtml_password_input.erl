%%%-------------------------------------------------------------------
%%% @doc A password field with a show/hide toggle, ported from sigil
%%% (form/password_input). See designs/04-components.md.
%%%
%%% The native control is kept: `Attrs' (name, placeholder, on(...), ...)
%%% go to the `<input>', `Css' to the wrapper.
%%%
%%% ah_password_input/3 builds an #ah_password_input{}
%%% (include/aihtml_password_input.hrl) and render/1 turns it into HTML,
%%% so pages may also write the record directly (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_password_input).
-behaviour(aihtml_element).

-include("aihtml_password_input.hrl").

-export([ah_password_input/3, render/1, fields/1, catalog/0]).

-define(H, aihtml_html).
-define(L, aihtml_lib_input).
-define(M(Name, Args, Doc), #{name => Name, args => Args, doc => Doc}).

%% @doc A password field with sigil's eye toggle. Options: `toggle'
%% (default true), `strength' (show the strength meter, default false),
%% `label' (floating).
-spec ah_password_input(binary() | undefined, aihtml_html:css(), aihtml_html:attrs()) ->
          #ah_password_input{}.
ah_password_input(Value, Css, Attrs) ->
    aihtml_element:build(?MODULE, #ah_password_input{value = Value}, Css, Attrs).

%% @doc The field names of #ah_password_input{}.
-spec fields(atom()) -> [atom()].
fields(ah_password_input) -> record_info(fields, ah_password_input).

-spec render(#ah_password_input{}) -> aihtml_html:html().
render(#ah_password_input{value = Value, label = Label} = R) ->
    Classes = ?L:classes(?MODULE, R),
    Id = ?L:input_id(R, Label),
    Native = ?H:void(input, [<<"ah-pwd">>],
                     [[{type, password}, {value, Value}, {id, Id},
                       {autocomplete, <<"current-password">>},
                       {spellcheck, <<"false">>},
                       {aria_invalid, R#ah_password_input.state =:= invalid
                            andalso <<"true">>},
                       {disabled, R#ah_password_input.disabled}],
                      ?L:native_attrs(R, Label)]),
    Toggle = case R#ah_password_input.toggle of
                 false -> [];
                 _ -> ?H:el(button, [eye_open(), eye_closed()], [<<"ah-pwd-toggle">>],
                            [{type, button}, {tabindex, <<"-1">>},
                             {aria_label, <<"Show password">>},
                             {aria_pressed, <<"false">>},
                             {aria_controls, Id}])
             end,
    Strength = case R#ah_password_input.strength of
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
           ?L:float_label(<<"ah-pwd">>, Label, Id, Value),
           Strength],
          Classes,
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

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => password_input, category => form,
       signature => <<"ah_password_input(Value, Css, Attrs)">>,
       root => <<"ah-pwd-group">>,
       groups => #{size => ?L:sizes(), state => ?L:states()},
       flags => [disabled],
       classes => ?L:family(<<"ah-pwd">>, [sm, lg, invalid, valid, disabled]),
       options => [toggle, strength, label],
       behavior => <<"password-input">>,
       events => [<<"input">>, <<"change">>],
       doc => <<"Password field with a show/hide toggle and an optional "
                "strength meter.">>,
       option_docs => maps:merge(?L:field_docs(),
                                 #{toggle => <<"Show the eye button that reveals the password "
                                               "(default true).">>,
                                   strength => <<"Show a strength bar and label under the field "
                                                 "(default false).">>}),
       methods => [?M(getValue, <<"()">>, <<"Return the value.">>),
                   ?M(setValue, <<"(Value)">>, <<"Set the value and update the strength meter.">>),
                   ?M(toggle, <<"([Show])">>, <<"Reveal (true) or hide (false) the password; "
                                               "flips it without an argument.">>),
                   ?M(focus, <<"()">>, <<"Focus the input.">>)]}].
