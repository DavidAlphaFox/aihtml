%%%-------------------------------------------------------------------
%%% @doc A radio button ported from sigil. It wraps a real, visually
%%% hidden `<input type="radio">' in a `<label>' that carries sigil's
%%% markup; `Attrs' go to that input, so radios sharing a `name' are
%%% exclusive natively.
%%%
%%% ah_radiobutton/4 builds an #ah_radiobutton{} (include/aihtml_radiobutton.hrl)
%%% and render/1 turns it into HTML, so pages may also write the record
%%% directly (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_radiobutton).
-behaviour(aihtml_element).

-include("aihtml_radiobutton.hrl").

-export([ah_radiobutton/4, render/1, fields/1, catalog/0]).

-define(H, aihtml_html).
-define(E, aihtml_element).
-define(L, aihtml_lib_choice).

%% @doc A radio button. Radio buttons with the same `name' are exclusive
%% (natively), and the behaviour restyles the one that was unchecked.
%% Options: `locked', `box_size'.
-spec ah_radiobutton(aihtml_html:html(), aihtml_lib_choice:value() | undefined,
                     aihtml_html:css(), aihtml_html:attrs()) -> #ah_radiobutton{}.
ah_radiobutton(Content, Value, Css, Attrs) ->
    ?E:build(?MODULE, #ah_radiobutton{body = Content, value = Value}, Css, Attrs).

%% @doc The field names of #ah_radiobutton{}.
-spec fields(atom()) -> [atom()].
fields(ah_radiobutton) -> record_info(fields, ah_radiobutton).

%% `id', the postback and `attrs' go on the native input.
-spec render(#ah_radiobutton{}) -> aihtml_html:html().
render(#ah_radiobutton{body = Content, value = Value, checked = C, disabled = D,
                       box_size = BoxSize} = R) ->
    {Checked, Disabled, InputAttrs} =
        ?L:single_input(R#ah_radiobutton.attrs, C, D, R#ah_radiobutton{attrs = []}),
    ?H:el(label,
        [?L:input(radio, Value, InputAttrs, []), ?L:radio_box(Checked, BoxSize),
         ?L:label_span(<<"ah-radiobutton-label">>, Content)],
        [?E:classes(?MODULE, R),
         ?L:state(Disabled, <<"ah-radiobutton-disabled">>),
         ?L:state(Checked, <<"ah-radiobutton-checked">>)],
        [{data_ah, <<"radiobutton">>},
         {data_ah_locked, ?L:truthy(R#ah_radiobutton.locked)}]).

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => radiobutton, category => form,
       signature => <<"ah_radiobutton(Content, Value, Css, Attrs)">>,
       root => <<"ah-radiobutton">>, groups => #{size => {[sm, md, lg], none}},
       options => [locked, box_size],
       behavior => <<"radiobutton">>, events => [<<"change">>, <<"input">>],
       doc => <<"Radio button around a native input; Attrs go to the <input>, "
                "radios sharing a name are exclusive. Options: locked, box_size.">>,
       option_docs =>
           #{sm => <<"Small circle and text.">>, md => <<"Default size.">>,
             lg => <<"Large circle and text.">>,
             locked => <<"true: focusable but the user cannot select it.">>,
             box_size => <<"Circle size in px.">>},
       methods => [?L:set_checked(<<"true | false">>), ?L:get_value(<<"true | false">>),
                   ?L:set_disabled()]}].
