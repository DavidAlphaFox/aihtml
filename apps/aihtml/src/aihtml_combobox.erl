%%%-------------------------------------------------------------------
%%% @doc An editable field with a filtered list, ported from sigil
%%% (form/combobox). See designs/04-components.md.
%%%
%%%   combobox(Items, Value, Css, Attrs)   an editable field with a filtered list
%%%   set_items(Ctx, Target, Items[, Opts]) (in an action) replace a combobox's list
%%%
%%% A value-bearing component: `Attrs' go to the root, which carries
%%% `data-ah-value' and fires `change'; `name' goes to a hidden input. The
%%% popup is rendered inside the root and driven by the `combobox'
%%% behaviour (assets/js/components/combobox.ts).
%%%
%%% == Server-side search ==
%%%
%%% `combobox(Items, Value, Css, [{search, {Mod, Action, Args}}])' binds
%%% `aihtml:on(input, Ref, #{debounce => 250})' to the text field. Each
%%% pause in typing POSTs the action with
%%%
%%%   Event.value                  the text typed so far (the query)
%%%   Event.data                   #{<<"combobox">> => <root id>}
%%%
%%% and the action answers with `set_items(Ctx, Event, Items)'. The items
%%% are rendered here, by the same code as the first render, and morphed
%%% into the list (`aihtml_action:html(Ctx, {id, <root id>-list}, Items,
%%% morph_inner)'), so the text field keeps its focus and caret; then the
%%% behaviour method `itemsLoaded' re-reads the list, highlights the query
%%% and opens the popup. While a query is pending the popup shows
%%% "Loading...". The client does not filter search results again.
%%%
%%% The browser builds tags from the shared template
%%% templates/combobox_tag.mustache.
%%%
%%% combobox/4 builds an #ah_combobox{} (include/aihtml_combobox.hrl) and
%%% render/1 turns it into HTML, so pages may also write the record
%%% directly (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_combobox).
-behaviour(aihtml_element).

-include("aihtml_combobox.hrl").

-export([combobox/4, set_items/3, set_items/4,
         render/1, fields/1, catalog/0, facade_extras/0]).

-export_type([item/0, search_mode/0, element/0]).

-define(H, aihtml_html).
-define(E, aihtml_element).
-define(L, aihtml_lib_list).

%% Shared template (see aihtml_tpl): also compiled to AH.tpl.* for the browser.
-compile({parse_transform, beamai_mustache_transform}).
-mustache_template({tpl_combobox_tag, "../templates/combobox_tag.mustache"}).

%% A combobox item: a text that is both value and label, `{Value, Label}',
%% or a map with `value' and optionally `label', `description', `group'
%% and `disabled'.
-type item() :: binary() | atom() | integer() | {term(), term()}
              | #{value := term(), label => term(), description => term(),
                  group => term(), disabled => boolean()}.
-type search_mode() :: contains_ignore_case | contains | starts_with_ignore_case
                     | starts_with | equals_ignore_case | equals | none.
-type element() :: #ah_combobox{}.

%% @doc An editable text field with sigil's filtered popup list. `Value' is
%% the selected item's value (a list of values with `multiple' or
%% `checkboxes'), or `undefined'.
%%
%% Css: `disabled', `no_arrow', `multiple' (tags), `checkboxes' (multiple
%% with check boxes), `free_text' (the typed text is a value too; without
%% it the value is always one of the items).
%% Options (in Attrs): `placeholder', `search_mode' (contains_ignore_case
%% (default), contains, starts_with_ignore_case, starts_with,
%% equals_ignore_case, equals, none), `min_length' (characters before the
%% list opens while typing, default 0), `empty_text' (default "No results
%% found"), `dropdown_height' (px, default 240), `search' (an action ref,
%% see the module doc).
-spec combobox([item()], term() | [term()] | undefined, aihtml_html:css(),
               aihtml_html:attrs()) -> #ah_combobox{}.
combobox(Items, Value, Css, Attrs) ->
    ?E:build(?MODULE, #ah_combobox{items = Items, value = Value}, Css, Attrs).

%% @doc The field names of #ah_combobox{}.
-spec fields(atom()) -> [atom()].
fields(ah_combobox) -> record_info(fields, ah_combobox).

-spec render(element()) -> aihtml_html:html().
render(#ah_combobox{items = Items0, value = Value, name = Name,
                    checkboxes = Checkboxes, disabled = Disabled,
                    placeholder = Placeholder, search_mode = Mode} = R0) ->
    {Id, R} = ensure_id(R0),
    Classes = ?E:classes(?MODULE, R),           % checks the flag fields first
    Multi = R#ah_combobox.multiple orelse Checkboxes,
    Items = [item(I) || I <- Items0],
    Selected = case {Multi, Value} of
                   {_, undefined} -> [];
                   {true, []} -> [];
                   {true, [V1 | _] = Vs} when not is_integer(V1) -> [text(V) || V <- Vs];
                   {_, V} -> [text(V)]
               end,
    lists:member(Mode, [contains_ignore_case, contains, starts_with_ignore_case,
                        starts_with, equals_ignore_case, equals, none])
        orelse error({aihtml, {bad_search_mode, Mode}}),
    Search = case R#ah_combobox.search of
                 undefined -> [];
                 Ref -> aihtml:on(input, Ref, #{debounce => 250})
             end,
    ListId = sub_id(Id, <<"list">>),
    Input = ?H:void(input, [<<"ah-combobox-input">>],
                    [[{type, text}, {id, sub_id(Id, <<"input">>)},
                      {autocomplete, off}, {spellcheck, <<"false">>},
                      {placeholder, case Multi andalso Selected =/= [] of
                                        true -> undefined;
                                        false -> Placeholder
                                    end},
                      {value, case Multi of
                                  true -> <<>>;
                                  false -> case Selected of
                                               [S] -> label_of(S, Items);
                                               [] -> <<>>
                                           end
                              end},
                      {disabled, Disabled},
                      {role, combobox}, {aria_autocomplete, list},
                      {aria_haspopup, listbox}, {aria_expanded, <<"false">>},
                      {aria_controls, ListId},
                      {data_combobox, Id},
                      {data_checkboxes, Checkboxes andalso <<"true">>}],
                     Search]),
    Field = case Multi of
                false -> Input;
                true -> ?H:el('div', [[tag(S, label_of(S, Items)) || S <- Selected], Input],
                              [<<"ah-combobox-tags">>], [])
            end,
    Arrow = ?H:el(span, ?H:el(span, <<"▼"/utf8>>, [<<"ah-combobox-arrow-icon">>], []),
                  [<<"ah-combobox-arrow">>], [{aria_hidden, <<"true">>}]),
    Height = R#ah_combobox.dropdown_height,
    Popup = ?H:el('div',
                  ?H:el(ul, render_items(Items, Selected, Checkboxes, Id),
                        [<<"ah-combobox-list">>],
                        [{id, ListId}, {role, listbox},
                         {aria_multiselectable, Multi andalso <<"true">>}]),
                  [<<"ah-combobox-popup">>],
                  [{style, [<<"max-height:", (integer_to_binary(Height))/binary, "px">>
                            || is_integer(Height)]}]),
    ?H:el('div',
          [?H:el('div', [Field, Arrow], [<<"ah-combobox-input-area">>], []),
           hidden(Name, ?L:value(Multi, Selected)),
           Popup],
          Classes,
          %% the id comes first, as before; root_attrs repeats it in place
          [[{id, Id}, {data_ah, <<"combobox">>}, {data_ah_value, ?L:value(Multi, Selected)},
            {data_ah_search_mode, Mode},
            {data_ah_min_length, R#ah_combobox.min_length},
            {data_ah_empty, R#ah_combobox.empty_text},
            {data_ah_remote, Search =/= []},
            {data_ah_placeholder, Multi andalso Placeholder},
            {aria_disabled, Disabled andalso <<"true">>}],
           ?E:root_attrs(R, change)]).

item(#{value := V} = M) ->
    maps:merge(#{label => text(maps:get(label, M, V))},
               maps:map(fun(disabled, B) when is_boolean(B) -> B;
                           (_, X) -> text(X)
                        end, maps:with([value, description, group, disabled], M)));
item({V, L}) -> #{value => text(V), label => text(L)};
item(V) when is_binary(V); is_atom(V); is_integer(V); is_list(V) ->
    T = text(V), #{value => T, label => T};
item(Other) -> error({aihtml, {bad_combobox_item, Other}}).

label_of(V, Items) ->
    case [L || #{value := V1, label := L} <- Items, V1 =:= V] of
        [L | _] -> L;
        [] -> V
    end.

%% Same markup as tags the browser adds: templates/combobox_tag.mustache
tag(Value, Label) ->
    aihtml_tpl:safe(tpl_combobox_tag(#{value => Value, label => Label})).

%% Grouped like sigil (group-by, in order of first appearance); the JS
%% renders the same markup from the same data.
render_items(Items, Selected, Checkboxes, Id) ->
    Groups = lists:foldl(fun(I, Acc) ->
                                 G = maps:get(group, I, undefined),
                                 case lists:keyfind(G, 1, Acc) of
                                     false -> Acc ++ [{G, [I]}];
                                     {G, Is} -> lists:keyreplace(G, 1, Acc, {G, Is ++ [I]})
                                 end
                         end, [], Items),
    Ordered = lists:append([Is || {_, Is} <- Groups]),
    Indexed = lists:zip(lists:seq(0, length(Ordered) - 1), Ordered),
    [[[?H:el(li, G, [<<"ah-combobox-group-header">>], [{role, presentation}])
       || G =/= undefined],
      [render_item(N, I, Selected, Checkboxes, Id)
       || {N, I} <- Indexed, maps:get(group, I, undefined) =:= G]]
     || {G, _} <- Groups].

render_item(N, #{value := V, label := L} = I, Selected, Checkboxes, Id) ->
    Sel = lists:member(V, Selected),
    Dis = maps:get(disabled, I, false),
    ?H:el(li,
          [[?H:el(span, [?H:el(span, <<"✓"/utf8>>, [<<"ah-combobox-checkbox-icon">>], [])
                         || Sel],
                  [<<"ah-combobox-checkbox">>, [<<"ah-combobox-checkbox-checked">> || Sel]], [])
            || Checkboxes],
           ?H:el('div',
                 [?H:el('div', L, [<<"ah-combobox-item-label">>], []),
                  [?H:el('div', D, [<<"ah-combobox-item-desc">>], [])
                   || #{description := D} <- [I]]],
                 [<<"ah-combobox-item-content">>], [])],
          [<<"ah-combobox-item">>, [<<"ah-combobox-item-selected">> || Sel],
           [<<"ah-combobox-item-disabled">> || Dis]],
          [{id, sub_id(Id, <<"opt-", (integer_to_binary(N))/binary>>)},
           {role, option}, {aria_selected, atom_to_binary(Sel)},
           {aria_disabled, Dis andalso <<"true">>},
           {data_index, N}, {data_value, V}, {data_label, L},
           {data_desc, maps:get(description, I, undefined)},
           {data_group, maps:get(group, I, undefined)}]).

%%%===================================================================
%%% Server-side search
%%%===================================================================

%% @doc Replace the items of a combobox from inside an action, typically
%% the `search' action: `set_items(Ctx, Event, Items)'. `Target' is the
%% search action's event (whose `data' names the combobox and tells
%% whether it has check boxes) or `{id, RootId}'. Items take the same forms
%% as in `combobox/4'. Sends two operations: the rendered items morphed
%% into `<root id>-list' (morph_inner), and a call of the behaviour method
%% `itemsLoaded' on the root, which re-reads the list, marks the selected
%% items, highlights the query and opens the popup.
-spec set_items(aihtml_action:ctx(), {id, iodata() | atom()} | aihtml_action:event(),
                [item()]) -> ok.
set_items(Ctx, #{data := Data}, Items) ->
    set_items(Ctx, {id, maps:get(<<"combobox">>, Data)}, Items,
              #{checkboxes => maps:get(<<"checkboxes">>, Data, <<>>) =:= <<"true">>});
set_items(Ctx, Target, Items) ->
    set_items(Ctx, Target, Items, #{}).

%% @doc `set_items/3' with options: `checkboxes' (render check boxes,
%% default false) and `selected' (values to mark, default none; the
%% browser marks its current selection anyway).
-spec set_items(aihtml_action:ctx(), {id, iodata() | atom()}, [item()],
                #{checkboxes => boolean(), selected => [term()]}) -> ok.
set_items(Ctx, {id, Id0}, Items, Opts) ->
    Id = text(Id0),
    Html = render_items([item(I) || I <- Items],
                        [text(V) || V <- maps:get(selected, Opts, [])],
                        maps:get(checkboxes, Opts, false), Id),
    aihtml_action:html(Ctx, {id, sub_id(Id, <<"list">>)}, Html, morph_inner),
    aihtml_action:call(Ctx, {id, Id}, itemsLoaded, []).

%% @doc Functions besides the component that the aihtml facade re-exports.
-spec facade_extras() -> [{atom(), arity()}].
facade_extras() -> [{set_items, 3}, {set_items, 4}].

%%%===================================================================
%%% Internal
%%%===================================================================

%% `name' goes to the hidden input, `id' stays on the root (and derives the
%% ids of the parts). A root without an id gets one: the parts refer to
%% each other by id (aria-controls, the search event's data). Returns the
%% id and the record holding it, for root_attrs/2.
ensure_id(R) ->
    Id = case R#ah_combobox.id of
             undefined -> <<"ah-p", (integer_to_binary(erlang:unique_integer([positive])))/binary>>;
             Id0 -> text(Id0)
         end,
    {Id, R#ah_combobox{id = Id}}.

sub_id(Id, Part) -> <<Id/binary, "-", Part/binary>>.

hidden(undefined, _) -> [];
hidden(Name, Value) -> ?H:void(input, [], [{type, hidden}, {name, Name}, {value, Value}]).

text(undefined) -> <<>>;
text(B) when is_binary(B) -> B;
text(L) when is_list(L) -> unicode:characters_to_binary(L);
text(X) -> beamai_html_escape:to_binary(X, aihtml).

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => combobox, category => form,
       signature => <<"combobox(Items, Value, Css, Attrs)">>,
       root => <<"ah-combobox">>,
       flags => [disabled, no_arrow, multiple, checkboxes, free_text],
       classes => #{no_arrow => [<<"ah-combobox-no-arrow">>],
                    free_text => [<<"ah-combobox-free-text">>]},
       options => [placeholder, search_mode, min_length, empty_text,
                   dropdown_height, search],
       behavior => <<"combobox">>,
       events => [<<"change">>, <<"ah:open">>, <<"ah:close">>],
       doc => <<"An editable field with a filtered, keyboard navigable list; "
                "single or multiple values, local or server-side search.">>,
       option_docs =>
           #{disabled => <<"Not editable.">>,
             no_arrow => <<"Hide the dropdown arrow.">>,
             multiple => <<"Several values, shown as tags; value \"a,b,c\" (a comma inside a "
                          "value is escaped as \\,; aihtml_value:split/1 reads it).">>,
             checkboxes => <<"Multiple, with a check box on every row.">>,
             free_text => <<"The typed text becomes the value on Enter or blur.">>,
             placeholder => <<"Text of the empty field.">>,
             search_mode => <<"contains_ignore_case (default), contains, starts_with_ignore_case, "
                              "starts_with, equals_ignore_case, equals or none.">>,
             min_length => <<"Characters typed before the list opens (default 0).">>,
             empty_text => <<"Shown when nothing matches (default \"No results found\").">>,
             dropdown_height => <<"Maximum list height in px (default 240).">>,
             search => <<"Action ref {Module, Action, Args} run (debounced) as the user types; "
                         "Event.value is the query, the action answers with set_items/3.">>},
       methods =>
           [#{name => itemsLoaded, args => <<"()">>,
              doc => <<"Re-read the list after set_items/3 morphed new rows in; "
                       "called by set_items itself.">>},
            #{name => setValue, args => <<"(Value | [Value])">>,
              doc => <<"Set the value without firing change.">>},
            #{name => getValue, args => <<"()">>, doc => <<"Return data-ah-value.">>},
            #{name => clear, args => <<"()">>, doc => <<"Empty the value and fire change.">>},
            #{name => open, args => <<"()">>, doc => <<"Open the list.">>},
            #{name => close, args => <<"()">>, doc => <<"Close the list.">>}]}].
