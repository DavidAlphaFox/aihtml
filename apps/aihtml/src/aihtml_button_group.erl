%%%-------------------------------------------------------------------
%%% @doc Joined buttons, optionally radio or checkbox, ported from sigil's
%%% form components. In radio and checkbox mode the group keeps its value
%%% in `data-ah-value' on the root, renders a hidden input when `Attrs' has
%%% a `name', and fires `change' on the root (designs/04-components.md).
%%%
%%% Items are `Label | {Value, Label} | {Value, Label, ItemAttrs}'
%%% (aihtml_lib_button). button_group/4 builds an #ah_button_group{}
%%% (include/aihtml_button_group.hrl) and render/1 turns it into HTML, so
%%% pages may also write the record directly (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_button_group).
-behaviour(aihtml_element).

-include("aihtml_button_group.hrl").

-export([button_group/4, render/1, fields/1, catalog/0]).

-define(H, aihtml_html).
-define(E, aihtml_element).
-define(L, aihtml_lib_button).

%% @doc Joined buttons. In `radio' mode `Value' is the selected item's
%% value; in `checkbox' mode a list of values (or "a,b"). In the default
%% mode `Value' is ignored and each button is a plain button whose own
%% `value' is the item value, so `ItemAttrs' can carry `on(click, ...)'.
-spec button_group([aihtml_lib_button:item()], term(), aihtml_html:css(),
                   aihtml_html:attrs()) -> #ah_button_group{}.
button_group(Items, Value, Css, Attrs) ->
    ?E:build(?MODULE, #ah_button_group{items = Items, value = Value}, Css, Attrs).

%% @doc The field names of #ah_button_group{}.
-spec fields(atom()) -> [atom()].
fields(ah_button_group) -> record_info(fields, ah_button_group).

-spec render(#ah_button_group{}) -> aihtml_html:html().
render(#ah_button_group{items = Items0, value = Value, name = Name, mode = Mode,
                        disabled = Disabled} = B) ->
    Items = [?L:item(I) || I <- Items0],
    Selected = case Mode of
                   radio -> [?L:bin(Value) || Value =/= undefined];
                   checkbox -> ?L:values(Value);
                   default -> []
               end,
    N = length(Items),
    Focus = ?L:roving_focus(Mode =:= radio, Items, Selected, Disabled),
    Buttons =
        [begin
             Sel = lists:member(V, Selected),
             Off = Disabled orelse ?L:is_disabled(IA),
             ?H:el(button, Label,
                   [<<"ah-btn-group-btn">>,
                    [<<"ah-btn-group-btn-first">> || Idx =:= 1],
                    [<<"ah-btn-group-btn-last">> || Idx =:= N],
                    [<<"ah-btn-group-btn-disabled">> || Off],
                    [<<"ah-btn-group-btn-selected">> || Sel]],
                   [[{type, button}, {value, V}, {data_value, V},
                     {disabled, Off},
                     {tabindex, case Mode of
                                    radio when V =:= Focus -> <<"0">>;
                                    radio -> <<"-1">>;
                                    _ -> undefined
                                end}],
                    case Mode of
                        radio -> [{role, radio}, {aria_checked, atom_to_binary(Sel, utf8)}];
                        checkbox -> [{aria_pressed, atom_to_binary(Sel, utf8)}];
                        default -> []
                    end,
                    IA])
         end || {Idx, {V, Label, IA}} <- lists:zip(lists:seq(1, N), Items)],
    Joined = ?L:join(Selected),
    ValueAttrs = case Mode of
                     default -> [];
                     _ -> [{data_ah_value, Joined}]
                 end,
    ?H:el('div',
          [Buttons, [?L:hidden_input(Name, Joined) || Mode =/= default]],
          [?E:classes(?MODULE, B), [<<"ah-btn-group-disabled">> || Disabled]],
          [[{role, case Mode of radio -> radiogroup; _ -> group end},
            {aria_disabled, Disabled andalso <<"true">>},
            {data_ah, <<"button-group">>} | ValueAttrs],
           ?E:root_attrs(B, case Mode of default -> click; _ -> change end)]).

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => button_group, category => form,
       signature => <<"button_group(Items, Value, Css, Attrs)">>,
       root => <<"ah-btn-group">>,
       groups => #{mode => {[default, radio, checkbox], default},
                   orientation => {[horizontal, vertical], horizontal},
                   shape => {[rounded, square], rounded},
                   fill => {[filled, outlined], none}},
       classes => #{default => [], square => []},
       behavior => <<"button-group">>, events => [<<"change">>],
       doc => <<"Joined buttons; radio and checkbox modes keep a selection.">>,
       methods => [#{name => setValue, args => <<"(Value)">>, doc => <<"Select a value, or in checkbox mode a list or \"a,b\", without firing change.">>},
                   #{name => getValue, args => <<"()">>, doc => <<"Return data-ah-value (comma separated in checkbox mode).">>},
                   #{name => clear, args => <<"()">>, doc => <<"Clear the selection without firing change.">>}]}].
