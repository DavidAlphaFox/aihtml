%%%-------------------------------------------------------------------
%%% @doc Several checkboxes ported from sigil, one native input per item.
%%% `Attrs' are split: `name', `disabled', `required' and `form' go to
%%% every input, everything else (id, class, data, aria, `aihtml:on/2')
%%% goes to the root. The root carries `data-ah-value' (the checked values,
%%% comma separated by aihtml_value:join/1); the behaviour keeps it in sync, stops the inputs' own
%%% `change' at the root and fires one `change' on the root instead.
%%%
%%% ah_checkbox_group/4 builds an #ah_checkbox_group{}
%%% (include/aihtml_checkbox_group.hrl) and render/1 turns it into HTML, so
%%% pages may also write the record directly (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_checkbox_group).
-behaviour(aihtml_element).

-include("aihtml_checkbox_group.hrl").

-export([ah_checkbox_group/4, render/1, fields/1, catalog/0]).

-define(E, aihtml_element).
-define(L, aihtml_lib_choice).

%% @doc Several checkboxes; `Values' lists the checked ones.
-spec ah_checkbox_group([aihtml_lib_choice:item()], [aihtml_lib_choice:value()],
                        aihtml_html:css(), aihtml_html:attrs()) -> #ah_checkbox_group{}.
ah_checkbox_group(Items, Values, Css, Attrs) ->
    ?E:build(?MODULE, #ah_checkbox_group{items = Items, value = Values}, Css, Attrs).

%% @doc The field names of #ah_checkbox_group{}.
-spec fields(atom()) -> [atom()].
fields(ah_checkbox_group) -> record_info(fields, ah_checkbox_group).

-spec render(#ah_checkbox_group{}) -> aihtml_html:html().
render(#ah_checkbox_group{items = Items, value = Values} = R) ->
    Selected = [?L:to_bin(V) || V <- Values],
    #ah_checkbox_group{name = N, disabled = D, required = Rq, form = F} = R,
    {InputAttrs, Root} = ?L:group_attrs(R#ah_checkbox_group.attrs, N, D, Rq, F),
    ?L:group(checkbox, <<"ah-checkbox-group">>, <<"group">>, <<"checkbox-group">>,
             Items, fun(V) -> lists:member(V, Selected) end,
             ?L:join_values([V || {V, _, _} <- ?L:norm_items(Items), lists:member(V, Selected)]),
             R#ah_checkbox_group.size, R#ah_checkbox_group.label_before,
             ?E:classes(?MODULE, R),
             InputAttrs, ?E:root_attrs(R#ah_checkbox_group{attrs = Root}, change)).

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [maps:merge(
       #{name => checkbox_group, category => form,
         signature => <<"ah_checkbox_group(Items, Values, Css, Attrs)">>,
         root => <<"ah-checkbox-group">>,
         groups => #{layout => {[vertical, horizontal], vertical},
                     size => {[sm, md, lg], none}},
         flags => [label_before],
         classes => #{sm => [], md => [], lg => [], label_before => []},
         behavior => <<"checkbox-group">>, events => [<<"change">>],
         doc => <<"Checkboxes from Items [{Value, Label} | {Value, Label, Opts}] "
                  "(Opts: disabled, class). name, disabled, required, form go to every "
                  "input; other Attrs (id, on/2, ...) to the root, whose data-ah-value "
                  "is the checked values, comma separated (a comma inside a value is "
                  "written \\,, see aihtml_value), and which fires one change.">>},
       ?L:group_api(true))].
