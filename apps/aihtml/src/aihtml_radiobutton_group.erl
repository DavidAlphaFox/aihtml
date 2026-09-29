%%%-------------------------------------------------------------------
%%% @doc Mutually exclusive radio buttons ported from sigil, one native
%%% input per item. `Attrs' are split: `name', `disabled', `required' and
%%% `form' go to every input, everything else goes to the root, which
%%% carries `data-ah-value'; the behaviour stops the inputs' own `change'
%%% at the root and fires one `change' on the root instead.
%%%
%%% radiobutton_group/4 builds an #ah_radiobutton_group{}
%%% (include/aihtml_radiobutton_group.hrl) and render/1 turns it into
%%% HTML, so pages may also write the record directly
%%% (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_radiobutton_group).
-behaviour(aihtml_element).

-include("aihtml_radiobutton_group.hrl").

-export([radiobutton_group/4, render/1, fields/1, catalog/0]).

-define(E, aihtml_element).
-define(L, aihtml_lib_choice).

%% @doc Mutually exclusive radio buttons; `Value' is the selected one (or
%% `undefined'). Arrow keys move the selection, as in sigil. Give the
%% group a `name' so it is exclusive without JS too.
-spec radiobutton_group([aihtml_lib_choice:item()], aihtml_lib_choice:value() | undefined,
                        aihtml_html:css(), aihtml_html:attrs()) -> #ah_radiobutton_group{}.
radiobutton_group(Items, Value, Css, Attrs) ->
    ?E:build(?MODULE, #ah_radiobutton_group{items = Items, value = Value}, Css, Attrs).

%% @doc The field names of #ah_radiobutton_group{}.
-spec fields(atom()) -> [atom()].
fields(ah_radiobutton_group) -> record_info(fields, ah_radiobutton_group).

-spec render(#ah_radiobutton_group{}) -> aihtml_html:html().
render(#ah_radiobutton_group{items = Items, value = Value} = R) ->
    Sel = ?L:opt_bin(Value),
    #ah_radiobutton_group{name = N, disabled = D, required = Rq, form = F} = R,
    {InputAttrs, Root} = ?L:group_attrs(R#ah_radiobutton_group.attrs, N, D, Rq, F),
    ?L:group(radio, <<"ah-radiobutton-group">>, <<"radiogroup">>, <<"radiobutton-group">>,
             Items, fun(V) -> V =:= Sel end, ?L:selected_value(Sel, Items),
             R#ah_radiobutton_group.size, R#ah_radiobutton_group.label_before,
             ?E:classes(?MODULE, R),
             InputAttrs, ?E:root_attrs(R#ah_radiobutton_group{attrs = Root}, change)).

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [maps:merge(
       #{name => radiobutton_group, category => form,
         signature => <<"radiobutton_group(Items, Value, Css, Attrs)">>,
         root => <<"ah-radiobutton-group">>,
         groups => #{layout => {[vertical, horizontal], vertical},
                     size => {[sm, md, lg], none}},
         flags => [label_before],
         classes => #{sm => [], md => [], lg => [], label_before => []},
         behavior => <<"radiobutton-group">>, events => [<<"change">>],
         doc => <<"Radio buttons from Items; arrow keys move the selection. name, "
                  "disabled, required, form go to every input, other Attrs to the root "
                  "(data-ah-value, one change event).">>},
       ?L:group_api(false))].
