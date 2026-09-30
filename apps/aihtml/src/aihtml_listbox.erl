%%%-------------------------------------------------------------------
%%% @doc sigil's listbox (form/listbox): a single or multi selection
%%% list. See designs/04-components.md.
%%%
%%%   ah_listbox(Items, Value, Css, Attrs)  the component
%%%   listbox_items(Ctx, Event, Items)      (in an action) new list rows
%%%
%%% A value-bearing component: `Attrs' go to the root, which carries
%%% `data-ah-value' (with `multiple' the values joined by aihtml_value:join/1)
%%% and fires `change'; `name'
%%% goes to a hidden input. The rows are rendered here; the behaviour
%%% (assets/js/components/listbox.ts) shows, hides and marks them.
%%%
%%% == Server-side search ==
%%%
%%% `ah_listbox(Items, Value, [filterable], [{search, Ref}])' binds
%%% `aihtml:on(input, Ref, #{debounce => 250})' to the filter field; the
%%% action gets the query in `Event.value' and answers with
%%% `listbox_items(Ctx, Event, Items)', which morphs the rendered rows into
%%% the list and calls the behaviour method `itemsLoaded'.
%%%
%%% The component function builds an #ah_listbox{} record (include/
%%% aihtml_listbox.hrl) and render/1 turns it into HTML
%%% (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_listbox).
-behaviour(aihtml_element).

-include("aihtml_listbox.hrl").

-export([ah_listbox/4, listbox_items/3, listbox_items/4,
         render/1, fields/1, catalog/0, facade_extras/0]).

-import(aihtml_lib_list, [item/1, ensure_id/1, sub_id/2, hidden/2, text/1, value/2]).

-define(H, aihtml_html).
-define(E, aihtml_element).

%% @doc A focusable list, sigil's listbox. `Value' is the selected value
%% (a list of values with `multiple' or `checkboxes'), or `undefined'.
%%
%% Css: `disabled', `multiple' (Ctrl/Shift click, Shift+arrows), `checkboxes'
%% (multiple, a click toggles a row), `check_all' (a "select all" row, with
%% checkboxes), `filterable' (a filter field above the list). Size the
%% list with Css classes on the root, e.g. `<<"w-64 h-72">>'.
%% Options (in Attrs): `empty_text' (default "No data"),
%% `filter_placeholder' (default "Search"), `check_all_label' (default
%% "Select all"), `search' (an action ref: the filter asks the server,
%% see the module doc).
-spec ah_listbox([aihtml_lib_list:item()], term() | [term()] | undefined,
                 aihtml_html:css(), aihtml_html:attrs()) -> #ah_listbox{}.
ah_listbox(Items, Value, Css, Attrs) ->
    ?E:build(?MODULE, #ah_listbox{items = Items, value = Value}, Css, Attrs).

-spec render(#ah_listbox{}) -> aihtml_html:html().
render(#ah_listbox{items = Items0, value = Value, name = Name,
                   disabled = Disabled, checkboxes = Checkboxes} = R0) ->
    {Id, R} = ensure_id(R0),
    Classes = ?E:classes(?MODULE, R),
    Multi = R#ah_listbox.multiple orelse Checkboxes,
    Items = [item(I) || I <- Items0],
    Selected = selected(Multi, Value),
    Search = case R#ah_listbox.search of
                 undefined -> [];
                 Ref -> aihtml:on(input, Ref, #{debounce => 250})
             end,
    Filter = [?H:el('div',
                    ?H:void(input, [<<"ah-listbox-filter-input">>],
                            [[{type, text}, {id, sub_id(Id, <<"filter">>)},
                              {autocomplete, off},
                              {placeholder, R#ah_listbox.filter_placeholder},
                              {aria_label, R#ah_listbox.filter_placeholder},
                              {aria_controls, Id}, {disabled, Disabled},
                              {data_listbox, Id},
                              {data_checkboxes, Checkboxes andalso <<"true">>}],
                             Search]),
                    [<<"ah-listbox-filter">>], [])
              || R#ah_listbox.filterable orelse Search =/= []],
    CheckAll = [?H:el('div',
                      [?H:el(span, [], [<<"ah-listbox-checkbox">>], []),
                       ?H:el(span, R#ah_listbox.check_all_label, [<<"ah-listbox-label">>], [])],
                      [<<"ah-listbox-check-all">>], [{role, button}, {aria_pressed, <<"false">>}])
                || Checkboxes, R#ah_listbox.check_all],
    ?H:el('div',
          [Filter, CheckAll,
           ?H:el('div',
                 [?H:el(ul, listbox_rows(Items, Selected, Checkboxes, Id),
                        [<<"ah-listbox-list">>], [{id, sub_id(Id, <<"list">>)}, {role, none}]),
                  ?H:el('div', R#ah_listbox.empty_text, [<<"ah-listbox-empty">>],
                        [{hidden, Items =/= []}])],
                 [<<"ah-listbox-content">>], []),
           hidden(Name, value(Multi, Selected))],
          [Classes, [<<"ah-listbox-remote">> || Search =/= []]],
          [[{id, Id}, {data_ah, <<"listbox">>}, {data_ah_value, value(Multi, Selected)},
            {tabindex, case Disabled of true -> <<"-1">>; false -> <<"0">> end},
            {role, listbox}, {aria_multiselectable, atom_to_binary(Multi)},
            {aria_disabled, Disabled andalso <<"true">>}],
           ?E:root_attrs(R, change)]).

selected(_, undefined) -> [];
selected(true, []) -> [];
selected(true, [V1 | _] = Vs) when not is_integer(V1) -> [text(V) || V <- Vs];
selected(_, V) -> [text(V)].

%% Grouped like sigil's group-by (in order of first appearance); the row
%% index (data-idx, the id suffix) is the position in `Items'.
listbox_rows(Items, Selected, Checkboxes, Id) ->
    Indexed = lists:zip(lists:seq(0, length(Items) - 1), Items),
    Groups = lists:foldl(fun({_, I}, Acc) ->
                                 G = maps:get(group, I, undefined),
                                 case lists:member(G, Acc) of
                                     true -> Acc;
                                     false -> Acc ++ [G]
                                 end
                         end, [], Indexed),
    [[[?H:el(li, G, [<<"ah-listbox-group">>], [{role, presentation}]) || G =/= undefined],
      [listbox_row(N, I, Selected, Checkboxes, Id)
       || {N, I} <- Indexed, maps:get(group, I, undefined) =:= G]]
     || G <- Groups].

listbox_row(N, #{value := V, label := L} = I, Selected, Checkboxes, Id) ->
    Sel = lists:member(V, Selected),
    Dis = maps:get(disabled, I, false),
    ?H:el(li,
          [[?H:el(span, [], [<<"ah-listbox-checkbox">>,
                             [<<"ah-listbox-checkbox-checked">> || Sel]], [])
            || Checkboxes],
           [?H:void(img, [<<"ah-listbox-icon">>], [{src, Src}, {alt, <<>>}])
            || #{icon := Src} <- [I]],
           ?H:el(span, L, [<<"ah-listbox-label">>], [])],
          [<<"ah-listbox-item">>, [<<"ah-listbox-item-selected">> || Sel],
           [<<"ah-listbox-item-disabled">> || Dis]],
          [{id, sub_id(Id, <<"o-", (integer_to_binary(N))/binary>>)},
           {role, option}, {aria_selected, atom_to_binary(Sel)},
           {aria_disabled, Dis andalso <<"true">>},
           {data_idx, N}, {data_value, V}]).

%% @doc Answer a listbox's `search' action: `listbox_items(Ctx, Event,
%% Items)'. Morphs the rendered rows into the list (`<root id>-list') and
%% calls the behaviour method `itemsLoaded', which marks the current
%% selection and shows the empty message when there are no rows.
-spec listbox_items(aihtml_action:ctx(), aihtml_action:event() | {id, iodata() | atom()},
                    [aihtml_lib_list:item()]) -> ok.
listbox_items(Ctx, #{data := Data}, Items) ->
    listbox_items(Ctx, {id, maps:get(<<"listbox">>, Data)}, Items,
                  #{checkboxes => maps:get(<<"checkboxes">>, Data, <<>>) =:= <<"true">>});
listbox_items(Ctx, Target, Items) ->
    listbox_items(Ctx, Target, Items, #{}).

%% @doc `listbox_items/3' with options: `checkboxes' (render check boxes,
%% default false) and `selected' (values to mark; the browser marks its
%% current selection anyway).
-spec listbox_items(aihtml_action:ctx(), {id, iodata() | atom()},
                    [aihtml_lib_list:item()],
                    #{checkboxes => boolean(), selected => [term()]}) -> ok.
listbox_items(Ctx, {id, Id0}, Items, Opts) ->
    Id = text(Id0),
    Html = listbox_rows([item(I) || I <- Items],
                        [text(V) || V <- maps:get(selected, Opts, [])],
                        maps:get(checkboxes, Opts, false), Id),
    aihtml_action:html(Ctx, {id, sub_id(Id, <<"list">>)}, Html, morph_inner),
    aihtml_action:call(Ctx, {id, Id}, itemsLoaded, []).

%% @doc Functions besides the component that the aihtml facade re-exports.
-spec facade_extras() -> [{atom(), arity()}].
facade_extras() -> [{listbox_items, 3}, {listbox_items, 4}].

%%%===================================================================
%%% Record and catalog
%%%===================================================================

%% @doc The field names of #ah_listbox{}.
-spec fields(atom()) -> [atom()].
fields(ah_listbox) -> record_info(fields, ah_listbox).

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => listbox, category => form,
       signature => <<"ah_listbox(Items, Value, Css, Attrs)">>,
       root => <<"ah-listbox">>,
       flags => [disabled, multiple, checkboxes, check_all, filterable],
       %% sigil's ah-listbox-check-all is the check-all row itself
       classes => #{check_all => []},
       options => [empty_text, filter_placeholder, check_all_label, search],
       behavior => <<"listbox">>,
       events => [<<"change">>],
       doc => <<"A focusable list with single or multiple selection, check boxes, groups, "
                "a filter and full keyboard navigation (arrows, Home/End, PageUp/PageDown, "
                "type-ahead).">>,
       option_docs =>
           #{disabled => <<"Not focusable, no selection.">>,
             multiple => <<"Several values: Ctrl+click toggles, Shift+click and Shift+arrows "
                           "select a range, Space toggles; value \"a,b,c\" (a comma inside a "
                           "value is escaped as \\,; aihtml_value:split/1 reads it).">>,
             checkboxes => <<"Multiple, with a check box on every row; a click toggles it.">>,
             check_all => <<"With checkboxes: a row above the list that checks every visible "
                            "row.">>,
             filterable => <<"A filter field above the list (hides the rows that do not "
                             "contain the text).">>,
             empty_text => <<"Shown when there are no rows (default \"No data\").">>,
             filter_placeholder => <<"Placeholder of the filter (default \"Search\").">>,
             check_all_label => <<"Text of the check-all row (default \"Select all\").">>,
             search => <<"Action ref {Module, Action, Args} run (debounced) as the user types "
                         "in the filter (implies it); Event.value is the query, the action "
                         "answers with listbox_items/3.">>},
       methods =>
           [#{name => setValue, args => <<"(Value | [Value])">>,
              doc => <<"Set the selection without firing change.">>},
            #{name => getValue, args => <<"()">>, doc => <<"Return data-ah-value.">>},
            #{name => clear, args => <<"()">>, doc => <<"Empty the selection and fire change.">>},
            #{name => filter, args => <<"(Text)">>, doc => <<"Filter the rows by text.">>},
            #{name => itemsLoaded, args => <<"()">>,
              doc => <<"Re-read the rows after listbox_items/3 morphed them in; called by "
                       "listbox_items itself.">>}]}].
