%%%-------------------------------------------------------------------
%%% @doc Selectable cards (a radio group) ported from sigil, one native
%%% input per card. `Attrs' are split as in radiobutton_group: `name',
%%% `disabled', `required' and `form' go to every input, the rest to the
%%% root, which carries `data-ah-value' and fires one `change'.
%%%
%%% ah_radio_cards/4 builds an #ah_radio_cards{} (include/aihtml_radio_cards.hrl)
%%% and render/1 turns it into HTML, so pages may also write the record
%%% directly (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_radio_cards).
-behaviour(aihtml_element).

-include("aihtml_radio_cards.hrl").

-export([ah_radio_cards/4, render/1, fields/1, catalog/0]).

-export_type([columns/0, align/0]).

-define(H, aihtml_html).
-define(E, aihtml_element).
-define(L, aihtml_lib_choice).

-type columns() :: 1 | 2 | 3 | auto.
-type align() :: center | start.

%% @doc Selectable cards with a title, an optional description and icon
%% (item Opts `description', `icon'). Options in Attrs: `columns'
%% (1 | 2 | 3 | auto) and `align' (center | start).
-spec ah_radio_cards([aihtml_lib_choice:item()], aihtml_lib_choice:value() | undefined,
                     aihtml_html:css(), aihtml_html:attrs()) -> #ah_radio_cards{}.
ah_radio_cards(Items, Value, Css, Attrs) ->
    ?E:build(?MODULE, #ah_radio_cards{items = Items, value = Value}, Css, Attrs).

%% @doc The field names of #ah_radio_cards{}.
-spec fields(atom()) -> [atom()].
fields(ah_radio_cards) -> record_info(fields, ah_radio_cards).

-spec render(#ah_radio_cards{}) -> aihtml_html:html().
render(#ah_radio_cards{items = Items, value = Value} = R) ->
    #ah_radio_cards{name = N, disabled = D, required = Rq, form = F} = R,
    {InputAttrs, Root} = ?L:group_attrs(R#ah_radio_cards.attrs, N, D, Rq, F),
    GroupDisabled = ?L:truthy(?L:attr(<<"disabled">>, InputAttrs)),
    Sel = ?L:opt_bin(Value),
    Columns = ?L:check_opt(radio_cards, columns, ?L:to_bin(R#ah_radio_cards.columns),
                           [<<"1">>, <<"2">>, <<"3">>, <<"auto">>]),
    Align = ?L:check_opt(radio_cards, align, ?L:to_bin(R#ah_radio_cards.align),
                         [<<"center">>, <<"start">>]),
    Cards = [begin
                 Off = GroupDisabled orelse ?L:truthy(?L:opt(disabled, IOpts)),
                 On = V =:= Sel,
                 ?H:el(label,
                     [?L:input(radio, V, InputAttrs, [{checked, On}, {disabled, Off}]),
                      icon_span(?L:opt(icon, IOpts)),
                      ?H:el(span,
                          [?H:el(span, Label, [<<"ah-radio-cards__label">>], []),
                           desc_span(?L:opt(description, IOpts))],
                          [<<"ah-radio-cards__text">>], [])],
                     [<<"ah-radio-cards__card">>, ?L:opt_class(IOpts)],
                     [{data_value, V}, {data_index, I - 1},
                      {data_selected, ?L:bool(On)}, {data_disabled, ?L:bool(Off)},
                      {data_ah_item_disabled, ?L:truthy(?L:opt(disabled, IOpts))}])
             end || {I, {V, Label, IOpts}} <- ?L:enumerate(?L:norm_items(Items))],
    ?H:el('div', Cards, ?E:classes(?MODULE, R),
        [{data_ah, <<"radio-cards">>}, {role, radiogroup},
         {data_ah_value, ?L:selected_value(Sel, Items)},
         {data_columns, Columns}, {data_align, Align},
         {data_disabled, ?L:bool(GroupDisabled)},
         {aria_disabled, GroupDisabled andalso <<"true">>},
         ?E:root_attrs(R#ah_radio_cards{attrs = Root}, change)]).

icon_span(undefined) -> [];
icon_span(Icon) -> ?H:el(span, Icon, [<<"ah-radio-cards__icon">>], [{aria_hidden, <<"true">>}]).

desc_span(undefined) -> [];
desc_span(D) -> ?H:el(span, D, [<<"ah-radio-cards__description">>], []).

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => radio_cards, category => form,
       signature => <<"ah_radio_cards(Items, Value, Css, Attrs)">>,
       root => <<"ah-radio-cards">>, options => [columns, align],
       behavior => <<"radio-cards">>, events => [<<"change">>],
       doc => <<"Card radio group; item Opts: description, icon, disabled, class. "
                "Options: columns (1|2|3|auto), align (center|start). Attrs split as "
                "in radiobutton_group.">>,
       option_docs =>
           #{columns => <<"1 | 2 | 3 | auto (default: as many 200px columns as fit).">>,
             align => <<"center (default) | start: top-align cards with long descriptions.">>},
       methods => [?L:set_value(false), ?L:group_get_value(false), ?L:group_set_disabled()]}].
