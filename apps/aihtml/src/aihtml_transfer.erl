%%%-------------------------------------------------------------------
%%% @doc sigil's transfer (form/transfer): two lists with move buttons.
%%% See designs/04-components.md.
%%%
%%%   transfer(Items, Value, Css, Attrs)
%%%
%%% A value-bearing component: `Attrs' go to the root, which carries
%%% `data-ah-value' (the keys on the right joined by aihtml_value:join/1)
%%% and fires
%%% `change'; `name' goes to a hidden input. Both lists are rendered here;
%%% the behaviour (assets/js/components/transfer.js) moves an item by
%%% moving its node.
%%%
%%% The component function builds an #ah_transfer{} record (include/
%%% aihtml_transfer.hrl) and render/1 turns it into HTML
%%% (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_transfer).
-behaviour(aihtml_element).

-include("aihtml_transfer.hrl").

-export([transfer/4, render/1, fields/1, catalog/0]).

-import(aihtml_lib_list, [item/1, ensure_id/1, sub_id/2, hidden/2, text/1, join/1]).

-define(H, aihtml_html).
-define(E, aihtml_element).

%% @doc Sigil's transfer: the items not chosen on the left, the chosen
%% ones on the right. `Value' is the list of chosen values, in the order
%% of the right list; `data-ah-value' joins them with commas (a comma inside a value is
%% escaped as `\,', see aihtml_value). Click rows
%% to select them (Space and arrows in a focused list), then move them
%% with the buttons, Enter or a double click.
%%
%% Css: `disabled', `no_filter' (no filter fields).
%% Options (in Attrs): `source_title' (default "Source"), `target_title'
%% (default "Target"), `filter_placeholder' (default "Search"),
%% `empty_text' (an empty list, default "No data").
-spec transfer([aihtml_lib_list:item()], [term()], aihtml_html:css(),
               aihtml_html:attrs()) -> #ah_transfer{}.
transfer(Items, Value, Css, Attrs) ->
    ?E:build(?MODULE, #ah_transfer{items = Items, value = Value}, Css, Attrs).

-spec render(#ah_transfer{}) -> aihtml_html:html().
render(#ah_transfer{items = Items0, value = Value0, name = Name,
                    disabled = Disabled} = R0) ->
    {Id, R} = ensure_id(R0),
    Classes = ?E:classes(?MODULE, R),
    is_list(Value0) orelse error({aihtml, {bad_option, value, Value0}}),
    Items = lists:zip(lists:seq(0, length(Items0) - 1), [item(I) || I <- Items0]),
    Chosen = [text(V) || V <- Value0],
    Source = [I || {_, #{value := V}} = I <- Items, not lists:member(V, Chosen)],
    Target = lists:append([[I || {_, #{value := V1}} = I <- Items, V1 =:= V] || V <- Chosen]),
    Value = [V || {_, #{value := V}} <- Target],
    Filter = not R#ah_transfer.no_filter,
    Panel = fun(Side, Title, Rows) ->
                    SideB = atom_to_binary(Side),
                    ListId = sub_id(Id, SideB),
                    ?H:el('div',
                          [?H:el('div',
                                 [?H:el(span, Title, [<<"ah-transfer-panel-title">>],
                                        [{id, sub_id(ListId, <<"title">>)}]),
                                  ?H:el(span, integer_to_binary(length(Rows)),
                                        [<<"ah-transfer-panel-count">>], [])],
                                 [<<"ah-transfer-panel-header">>], []),
                           [?H:el('div',
                                  ?H:void(input, [<<"ah-transfer-filter-input">>],
                                          [{type, text}, {autocomplete, off},
                                           {placeholder, R#ah_transfer.filter_placeholder},
                                           {aria_label, R#ah_transfer.filter_placeholder},
                                           {disabled, Disabled}, {data_panel, SideB}]),
                                  [<<"ah-transfer-filter">>], [])
                            || Filter],
                           ?H:el('div',
                                 ?H:el(ul, [transfer_row(N, I, SideB, Id) || {N, I} <- Rows],
                                       [<<"ah-transfer-list">>],
                                       [{id, ListId}, {data_panel, SideB}, {role, listbox},
                                        {tabindex, case Disabled of
                                                       true -> <<"-1">>;
                                                       false -> <<"0">>
                                                   end},
                                        {aria_multiselectable, <<"true">>},
                                        {aria_labelledby, sub_id(ListId, <<"title">>)},
                                        {data_empty_text, R#ah_transfer.empty_text}]),
                                 [<<"ah-transfer-panel-content">>], [])],
                          [<<"ah-transfer-panel">>,
                           <<"ah-transfer-panel-", SideB/binary>>], [])
            end,
    Button = fun(Dir, Label) ->
                     ?H:el(button, <<"›"/utf8>>,
                           [<<"ah-transfer-btn">>, <<"ah-transfer-btn-", Dir/binary>>,
                            <<"ah-transfer-btn-disabled">>],
                           [{type, button}, {data_direction, Dir}, {title, Label},
                            {aria_label, Label}, {disabled, true}])
             end,
    ?H:el('div',
          [?H:el('div',
                 [Panel(source, R#ah_transfer.source_title, Source),
                  ?H:el('div', [Button(<<"to-target">>, <<"Move to target">>),
                                Button(<<"to-source">>, <<"Move to source">>)],
                        [<<"ah-transfer-buttons">>], []),
                  Panel(target, R#ah_transfer.target_title, Target)],
                 [<<"ah-transfer-panels">>], []),
           hidden(Name, join(Value))],
          Classes,
          [[{id, Id}, {data_ah, <<"transfer">>}, {data_ah_value, join(Value)},
            {aria_disabled, Disabled andalso <<"true">>}],
           ?E:root_attrs(R, change)]).

transfer_row(N, #{value := V, label := L} = I, Side, Id) ->
    Dis = maps:get(disabled, I, false),
    ?H:el(li,
          [[?H:el(span, Icon, [<<"ah-transfer-item-icon">>], [{aria_hidden, <<"true">>}])
            || #{icon := Icon} <- [I]],
           ?H:el(span, L, [<<"ah-transfer-item-label">>], [])],
          [<<"ah-transfer-item">>, [<<"ah-transfer-item-disabled">> || Dis]],
          [{id, sub_id(Id, <<"i-", (integer_to_binary(N))/binary>>)},
           {role, option}, {aria_selected, <<"false">>},
           {aria_disabled, Dis andalso <<"true">>},
           {data_value, V}, {data_idx, N}, {data_source, Side}]).

%%%===================================================================
%%% Record and catalog
%%%===================================================================

%% @doc The field names of #ah_transfer{}.
-spec fields(atom()) -> [atom()].
fields(ah_transfer) -> record_info(fields, ah_transfer).

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => transfer, category => form,
       signature => <<"transfer(Items, Value, Css, Attrs)">>,
       root => <<"ah-transfer">>,
       flags => [disabled, no_filter],
       classes => #{no_filter => [<<"ah-transfer-no-filter">>]},
       options => [source_title, target_title, filter_placeholder, empty_text],
       behavior => <<"transfer">>,
       events => [<<"change">>],
       doc => <<"Two lists with move buttons: the value is the list of keys on the right, "
                "in order, joined with commas (a comma inside a key is escaped as \\,; "
                "aihtml_value:split/1 reads it). Rows are selected by click or keyboard and moved by the buttons, "
                "Enter or a double click.">>,
       option_docs =>
           #{disabled => <<"Nothing can be selected or moved.">>,
             no_filter => <<"No filter fields above the lists.">>,
             source_title => <<"Title of the left list (default \"Source\").">>,
             target_title => <<"Title of the right list (default \"Target\").">>,
             filter_placeholder => <<"Placeholder of the filters (default \"Search\").">>,
             empty_text => <<"Shown in an empty list (default \"No data\").">>},
       methods =>
           [#{name => setValue, args => <<"([Key] | \"k1,k2\")">>,
              doc => <<"Put these keys on the right, in this order, without firing change.">>},
            #{name => getValue, args => <<"()">>, doc => <<"Return data-ah-value.">>},
            #{name => moveToTarget, args => <<"()">>,
              doc => <<"Move the selected rows of the left list; fires change.">>},
            #{name => moveToSource, args => <<"()">>,
              doc => <<"Move the selected rows of the right list; fires change.">>},
            #{name => selectAll, args => <<"(\"source\" | \"target\")">>,
              doc => <<"Select every visible enabled row of a list.">>},
            #{name => clearSelection, args => <<"(\"source\" | \"target\")">>,
              doc => <<"Unselect the rows of a list.">>}]}].
