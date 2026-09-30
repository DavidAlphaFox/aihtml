%%%-------------------------------------------------------------------
%%% @doc A checkbox ported from sigil. It wraps a real, visually hidden
%%% `<input>' in a `<label>' that carries sigil's markup (box, check mark).
%%% `Attrs' go to that input, so `name', `checked', `disabled', `id' and
%%% `aihtml:on(change, ...)' behave natively and the control takes part in
%%% form submission.
%%%
%%% ah_checkbox/4 builds an #ah_checkbox{} (include/aihtml_checkbox.hrl) and
%%% render/1 turns it into HTML, so pages may also write the record
%%% directly (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_checkbox).
-behaviour(aihtml_element).

-include("aihtml_checkbox.hrl").

-export([ah_checkbox/4, render/1, fields/1, catalog/0]).

-define(H, aihtml_html).
-define(E, aihtml_element).
-define(L, aihtml_lib_choice).

%% @doc A checkbox. `Value' is the input's form value (`undefined' keeps
%% the browser default "on"). Options in Attrs: `indeterminate',
%% `three_states' (click cycles checked -> mixed -> unchecked), `locked'
%% (focusable but cannot be toggled), `box_size' (px).
-spec ah_checkbox(aihtml_html:html(), aihtml_lib_choice:value() | undefined, aihtml_html:css(),
                  aihtml_html:attrs()) -> #ah_checkbox{}.
ah_checkbox(Content, Value, Css, Attrs) ->
    ?E:build(?MODULE, #ah_checkbox{body = Content, value = Value}, Css, Attrs).

%% @doc The field names of #ah_checkbox{}.
-spec fields(atom()) -> [atom()].
fields(ah_checkbox) -> record_info(fields, ah_checkbox).

%% `id', the postback and `attrs' go on the native input, after its value
%% and before `checked' / `disabled'.
-spec render(#ah_checkbox{}) -> aihtml_html:html().
render(#ah_checkbox{body = Content, value = Value, checked = C, disabled = D,
                    indeterminate = Indet0, box_size = BoxSize} = R) ->
    {Checked, Disabled, InputAttrs} =
        ?L:single_input(R#ah_checkbox.attrs, C, D, R#ah_checkbox{attrs = []}),
    Indet = ?L:truthy(Indet0),
    ?H:el(label,
        [?L:input(checkbox, Value, InputAttrs, []), ?L:check_box(Checked, Indet, BoxSize),
         ?L:label_span(<<"ah-checkbox-label">>, Content)],
        [?E:classes(?MODULE, R),
         ?L:state(Disabled, <<"ah-checkbox-disabled">>),
         ?L:state(Checked andalso not Indet, <<"ah-checkbox-checked">>),
         ?L:state(Indet, <<"ah-checkbox-indeterminate">>)],
        [{data_ah, <<"checkbox">>},
         {data_ah_three_states, ?L:truthy(R#ah_checkbox.three_states)},
         {data_ah_locked, ?L:truthy(R#ah_checkbox.locked)}]).

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => checkbox, category => form,
       signature => <<"ah_checkbox(Content, Value, Css, Attrs)">>,
       root => <<"ah-checkbox">>, groups => #{size => {[sm, md, lg], none}},
       options => [indeterminate, three_states, locked, box_size],
       behavior => <<"checkbox">>, events => [<<"change">>, <<"input">>],
       doc => <<"Checkbox around a native input. Attrs (name, checked, disabled, id, "
                "on/2) go to the <input>; Value is its form value. Options: "
                "indeterminate, three_states, locked, box_size.">>,
       option_docs =>
           #{sm => <<"Small box (14px) and text.">>,
             md => <<"Default box (16px).">>,
             lg => <<"Large box (20px) and text.">>,
             indeterminate => <<"true: start in the mixed state (input.indeterminate).">>,
             three_states => <<"true: a click cycles checked, mixed, unchecked.">>,
             locked => <<"true: focusable but the user cannot toggle it.">>,
             box_size => <<"Box size in px, overrides the size modifier.">>},
       methods => [?L:set_checked(<<"true | false | \"mixed\"">>),
                   ?L:get_value(<<"true | false | \"mixed\"">>),
                   ?L:set_disabled()]}].
