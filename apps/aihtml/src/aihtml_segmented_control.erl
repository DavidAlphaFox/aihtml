%%%-------------------------------------------------------------------
%%% @doc Mutually exclusive segments (role tablist, as in sigil), ported
%%% from sigil's form components. The value is in `data-ah-value' on the
%%% root, a hidden input carries it when `Attrs' has a `name', and
%%% `change' fires on the root (designs/04-components.md).
%%%
%%% Items are `Label | {Value, Label} | {Value, Label, ItemAttrs}'
%%% (aihtml_lib_button). ah_segmented_control/4 builds an
%%% #ah_segmented_control{} (include/aihtml_segmented_control.hrl) and
%%% render/1 turns it into HTML, so pages may also write the record
%%% directly (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_segmented_control).
-behaviour(aihtml_element).

-include("aihtml_segmented_control.hrl").

-export([ah_segmented_control/4, render/1, fields/1, catalog/0]).

-define(H, aihtml_html).
-define(E, aihtml_element).
-define(L, aihtml_lib_button).

%% @doc Mutually exclusive segments (role tablist, as in sigil). `Value'
%% is the selected item's value.
-spec ah_segmented_control([aihtml_lib_button:item()], term(), aihtml_html:css(),
                           aihtml_html:attrs()) -> #ah_segmented_control{}.
ah_segmented_control(Items, Value, Css, Attrs) ->
    ?E:build(?MODULE, #ah_segmented_control{items = Items, value = Value}, Css, Attrs).

%% @doc The field names of #ah_segmented_control{}.
-spec fields(atom()) -> [atom()].
fields(ah_segmented_control) -> record_info(fields, ah_segmented_control).

-spec render(#ah_segmented_control{}) -> aihtml_html:html().
render(#ah_segmented_control{items = Items0, value = Value, name = Name, size = Size,
                             full_width = Full, disabled = Disabled} = B) ->
    Items = [?L:item(I) || I <- Items0],
    Current = case Value of undefined -> undefined; _ -> ?L:bin(Value) end,
    Focus = ?L:roving_focus(true, Items, [Current], Disabled),
    Buttons =
        [begin
             Active = V =:= Current,
             Off = ?L:is_disabled(IA),
             ?H:el(button, Label, [<<"ah-segmented-control__item">>],
                   [[{type, button}, {role, tab},
                     {aria_selected, atom_to_binary(Active, utf8)},
                     {data_value, V},
                     {data_state, case Active of true -> active; false -> inactive end},
                     {data_disabled, atom_to_binary(Off, utf8)},
                     {tabindex, case V =:= Focus of true -> <<"0">>; false -> <<"-1">> end},
                     {disabled, Off orelse Disabled}],
                    IA])
         end || {V, Label, IA} <- Items],
    Cur = case Current of undefined -> <<>>; _ -> Current end,
    ?H:el('div', [Buttons, ?L:hidden_input(Name, Cur)], ?E:classes(?MODULE, B),
          [[{role, tablist},
            {data_size, Size},
            {data_full_width, atom_to_binary(Full, utf8)},
            {data_disabled, atom_to_binary(Disabled, utf8)},
            {aria_disabled, Disabled andalso <<"true">>},
            {data_ah, <<"segmented-control">>},
            {data_ah_value, Cur}],
           ?E:root_attrs(B, change)]).

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => segmented_control, category => form,
       signature => <<"ah_segmented_control(Items, Value, Css, Attrs)">>,
       root => <<"ah-segmented-control">>,
       groups => #{size => {[sm, md, lg], md}}, flags => [full_width],
       classes => #{sm => [], md => [], lg => [], full_width => []},
       behavior => <<"segmented-control">>, events => [<<"change">>],
       doc => <<"A row of mutually exclusive segments.">>,
       option_docs => #{full_width => <<"Stretch to the container width with equal segments.">>},
       methods => [#{name => setValue, args => <<"(Value)">>, doc => <<"Select a segment without firing change.">>},
                   #{name => getValue, args => <<"()">>, doc => <<"Return the selected value.">>}]}].
