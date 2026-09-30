%%%-------------------------------------------------------------------
%%% @doc The command palette, ported from sigil (overlay/command): a
%%% search field over grouped commands with keyboard navigation, inline
%%% or in a centred ⌘K overlay. DOM and class names are the ones sigil
%%% renders, so the styles in priv/css/sigil apply unchanged; the
%%% behaviour is in assets/js/components/command.ts.
%%%
%%%   ah_command(Items, Css, Attrs)            a command palette (⌘K)
%%%   set_command_items(Ctx, Target, Items)    (in an action) replace a
%%%                                            command palette's list
%%%
%%% command fires `ah:select' with the chosen command's value in
%%% `data-ah-value'.
%%%
%%% == Command filtering ==
%%%
%%% By default every command is rendered here and the behaviour hides the
%%% ones that do not match what the user types (a case-insensitive
%%% substring of the value, label or description, as in sigil). With
%%% `{search, {Mod, Action, Args}}' the text field instead posts the
%%% action (debounced) with `Event.value' = the query and `Event.data' =
%%% `#{<<"command">> => RootId}'; the action answers with
%%% `set_command_items(Ctx, Event, Items)', which renders the list here
%%% and morphs it into the palette. The browser builds no HTML.
%%%
%%% ah_command/3 builds an element record (#ah_command{},
%%% include/aihtml_command.hrl) and render/1 turns it into HTML
%%% (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_command).
-behaviour(aihtml_element).

-include("aihtml_command.hrl").

-export([ah_command/3, set_command_items/3, render/1, fields/1, catalog/0, facade_extras/0]).

-export_type([item/0, entry/0, action_ref/0]).

-define(H, aihtml_html).
-define(E, aihtml_element).

-type css() :: aihtml_html:css().
-type attrs() :: aihtml_html:attrs().
%% Label | {Value, Label} | #{value, label, description, icon, shortcut,
%% href, disabled}.
-type item() :: iodata() | atom() | integer()
              | {term(), aihtml_html:html()}
              | #{value := term(),
                  label => aihtml_html:html(),
                  description => aihtml_html:html(),
                  icon => aihtml_html:html(),
                  shortcut => aihtml_html:html(),
                  href => iodata(),
                  disabled => boolean()}.
%% A command item, or a group of them under a heading.
-type entry() :: item() | #{heading => aihtml_html:html(), items := [item()]}.
-type action_ref() :: {module(), atom(), term()}.

%%%===================================================================
%%% Builder
%%%===================================================================

%% @doc A command palette: a search field over grouped commands. `Items'
%% are commands (`Label', `{Value, Label}' or `#{value, label,
%% description, icon, shortcut, href, disabled}') and groups
%% `#{heading, items}'. Css `palette' renders it in a hidden centred
%% overlay, opened with the `open' method or the `hotkey' option.
-spec ah_command([entry()], css(), attrs()) -> #ah_command{}.
ah_command(Items, Css, Attrs) ->
    ?E:build(?MODULE, #ah_command{items = Items}, Css, Attrs).

%% @doc The field names of the record.
-spec fields(atom()) -> [atom()].
fields(ah_command) -> record_info(fields, ah_command).

%% @doc Functions besides the component that the aihtml facade re-exports.
-spec facade_extras() -> [{atom(), arity()}].
facade_extras() -> [{set_command_items, 3}].

%%%===================================================================
%%% Rendering
%%%===================================================================

-spec render(#ah_command{}) -> aihtml_html:html().
render(#ah_command{palette = Palette} = R0) ->
    {Id, R} = ensure_id(R0),
    Classes = ?E:classes(?MODULE, R),
    #ah_command{placeholder = Placeholder, query = Query, search = Search,
                hotkey = Hotkey} = R,
    is_boolean(R#ah_command.close_on_select)
        orelse error({aihtml, {bad_option, close_on_select, R#ah_command.close_on_select}}),
    SearchAttrs = case Search of
                      undefined -> [];
                      {M, A, _} = Ref when is_atom(M), is_atom(A) ->
                          aihtml:on(input, Ref, #{debounce => 250});
                      Other -> error({aihtml, {bad_option, search, Other}})
                  end,
    ListId = sub_id(Id, <<"list">>),
    List = command_list(Id, R#ah_command.items, R#ah_command.empty_text),
    Input = ?H:void(input, [<<"ah-command__input">>],
                    [[{type, text}, {id, sub_id(Id, <<"input">>)},
                      {placeholder, Placeholder}, {value, Query},
                      {autocomplete, off}, {spellcheck, <<"false">>},
                      {aria_label, <<"command input">>},
                      {role, combobox}, {aria_expanded, <<"true">>},
                      {aria_autocomplete, list}, {aria_controls, ListId},
                      {data_command, Id}, {data_empty, R#ah_command.empty_text}],
                     SearchAttrs]),
    Root = ?H:el('div',
                 [?H:el('div', Input, [<<"ah-command__input-wrap">>], []),
                  ?H:el('div', List, [<<"ah-command__list">>],
                        [{id, ListId}, {role, listbox}])],
                 Classes,
                 [[{id, Id},
                   {role, Palette andalso dialog}, {aria_modal, Palette andalso <<"true">>},
                   {aria_label, Palette andalso <<"Command palette">>},
                   {data_ah, <<"command">>},
                   {data_ah_remote, Search =/= undefined},
                   {data_ah_query, Query},
                   {data_hotkey, Hotkey},
                   {data_auto_focus, R#ah_command.auto_focus},
                   {data_close_on_select, atom_to_binary(R#ah_command.close_on_select, utf8)}],
                  ?E:root_attrs(R, 'ah:select')]),
    case Palette of
        true -> ?H:el('div', Root, [<<"ah-command-overlay">>], [{hidden, true}]);
        false -> Root
    end.

%% The groups, items and the empty message, as the list's children. Items
%% are numbered across groups; the first one is active.
command_list(Id, Entries, EmptyText) ->
    Groups = command_groups(Entries),
    {Rendered, N} =
        lists:mapfoldl(
          fun({Heading, Items}, I0) ->
                  {Html, I1} = lists:mapfoldl(fun(It, I) -> {command_item(Id, It, I), I + 1} end,
                                              I0, Items),
                  {?H:el('div',
                         [[?H:el('div', Heading, [<<"ah-command__group-heading">>],
                                 [{role, presentation}]) || Heading =/= undefined],
                          Html],
                         [<<"ah-command__group">>], [{role, group}]), I1}
          end, 0, [G || {_, [_ | _]} = G <- Groups]),
    [Rendered,
     ?H:el('div', EmptyText, [<<"ah-command__empty">>], [{hidden, N > 0}])].

%% Consecutive bare commands form an unnamed group.
command_groups(Entries) ->
    [{case H of bare -> undefined; _ -> H end, Items}
     || {H, Items} <- fold_groups(Entries, [])].

fold_groups([], Acc) -> lists:reverse(Acc);
fold_groups([#{items := Items} = G | Rest], Acc) when is_list(Items) ->
    fold_groups(Rest, [{maps:get(heading, G, undefined), [cmd_item(I) || I <- Items]} | Acc]);
fold_groups([I | Rest], [{bare, Is} | Acc]) ->
    fold_groups(Rest, [{bare, Is ++ [cmd_item(I)]} | Acc]);
fold_groups([I | Rest], Acc) ->
    fold_groups(Rest, [{bare, [cmd_item(I)]} | Acc]).

cmd_item(#{value := V} = M) ->
    maps:merge(#{label => maps:get(label, M, text(V))},
               maps:put(value, text(V), maps:with([description, icon, shortcut, href,
                                                    disabled], M)));
cmd_item({V, L}) -> #{value => text(V), label => L};
cmd_item(V) when is_binary(V); is_atom(V); is_integer(V) ->
    T = text(V), #{value => T, label => T};
cmd_item(V) when is_list(V) ->
    case io_lib:printable_unicode_list(V) of
        true -> T = text(V), #{value => T, label => T};
        false -> error({aihtml, {bad_command_item, V}})
    end;
cmd_item(Other) -> error({aihtml, {bad_command_item, Other}}).

command_item(Id, #{value := V, label := L} = It, I) ->
    Idx = integer_to_binary(I),
    Active = I =:= 0,
    Off = maps:get(disabled, It, false) =:= true,
    Desc = maps:get(description, It, undefined),
    Icon = maps:get(icon, It, undefined),
    Shortcut = maps:get(shortcut, It, undefined),
    ?H:el('div',
          [[?H:el(span, Icon, [<<"ah-command__item-icon">>], [{aria_hidden, <<"true">>}])
            || Icon =/= undefined],
           ?H:el('div',
                 [?H:el(span, L, [<<"ah-command__item-label">>], []),
                  [?H:el(span, Desc, [<<"ah-command__item-desc">>], []) || Desc =/= undefined]],
                 [<<"ah-command__item-text">>], []),
           [?H:el(kbd, Shortcut, [<<"ah-kbd">>, <<"ah-command__item-shortcut">>], [])
            || Shortcut =/= undefined]],
          [<<"ah-command__item">>],
          [{id, sub_id(Id, <<"item-", Idx/binary>>)}, {role, option},
           {data_value, V}, {data_index, Idx},
           {data_active, atom_to_binary(Active, utf8)},
           {data_disabled, atom_to_binary(Off, utf8)},
           {data_href, maps:get(href, It, undefined)},
           {aria_selected, atom_to_binary(Active, utf8)},
           {aria_disabled, Off andalso <<"true">>}]).

%%%===================================================================
%%% Server-side search
%%%===================================================================

%% @doc Replace the commands of a palette from inside an action, typically
%% its `search' action: `set_command_items(Ctx, Event, Items)'. `Target'
%% is the search event (whose `data' names the palette and its empty
%% text) or `{id, RootId}'.
%% Items take the same forms as in `ah_command/3'. The list is rendered
%% here and morphed into `<root id>-list' (morph_inner), so the text
%% field keeps its focus and caret; then the behaviour method
%% `itemsLoaded' marks the first command active.
-spec set_command_items(aihtml_action:ctx(), {id, iodata() | atom()} | aihtml_action:event(),
                        [entry()]) -> ok.
set_command_items(Ctx, #{data := #{<<"command">> := Id} = Data}, Items) ->
    set_items(Ctx, text(Id), Items, maps:get(<<"empty">>, Data, <<"No results found.">>));
set_command_items(Ctx, {id, Id}, Items) ->
    set_items(Ctx, text(Id), Items, <<"No results found.">>).

set_items(Ctx, Id, Items, EmptyText) ->
    aihtml_action:html(Ctx, {id, sub_id(Id, <<"list">>)},
                       command_list(Id, Items, EmptyText), morph_inner),
    aihtml_action:call(Ctx, {id, Id}, itemsLoaded, []).

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => command, category => overlay,
       signature => <<"ah_command(Items, Css, Attrs)">>,
       root => <<"ah-command">>,
       flags => [palette, auto_focus],
       classes => #{palette => [<<"ah-command-panel">>], auto_focus => []},
       options => [placeholder, empty_text, query, search, hotkey, close_on_select],
       behavior => <<"command">>,
       events => [<<"ah:select">>, <<"ah:query">>, <<"ah:open">>, <<"ah:close">>],
       doc => <<"A command palette: a search field over grouped commands with keyboard "
                "navigation, inline or in a centred ⌘K overlay."/utf8>>,
       option_docs =>
           #{palette => <<"Render in a hidden centred overlay; open with the open method or hotkey.">>,
             auto_focus => <<"Focus the search field when the page loads (inline palettes).">>,
             placeholder => <<"Placeholder of the search field.">>,
             empty_text => <<"Shown when nothing matches (default \"No results found.\").">>,
             query => <<"Initial search text.">>,
             search => <<"Action ref {Module, Action, Args} run (debounced) as the user types; "
                         "Event.value is the query and the action answers with "
                         "set_command_items(Ctx, Event, Items). The browser does not filter.">>,
             hotkey => <<"A letter: Ctrl+letter or ⌘+letter toggles the palette, e.g. <<\"k\">>."/utf8>>,
             close_on_select => <<"Close the overlay after a command is chosen (default true).">>},
       methods => [#{name => open, args => <<"()">>, doc => <<"Open the overlay and focus the field.">>},
                   #{name => close, args => <<"()">>, doc => <<"Close the overlay.">>},
                   #{name => toggle, args => <<"()">>, doc => <<"Open or close the overlay.">>},
                   #{name => setQuery, args => <<"(Text)">>,
                     doc => <<"Set the search text and filter (or run the search action).">>},
                   #{name => focus, args => <<"()">>, doc => <<"Focus the search field.">>},
                   #{name => itemsLoaded, args => <<"()">>,
                     doc => <<"Re-read the list after the server replaced it (set_command_items does this).">>}]}].

%%%===================================================================
%%% Internal
%%%===================================================================

%% The parts refer to each other by id, so a root without one gets one.
ensure_id(R) ->
    Id = case element(3, R) of
             undefined -> <<"ah-b", (integer_to_binary(erlang:unique_integer([positive])))/binary>>;
             Id0 -> text(Id0)
         end,
    {Id, setelement(3, R, Id)}.

sub_id(Id, Part) -> <<Id/binary, "-", Part/binary>>.

text(undefined) -> <<>>;
text(B) when is_binary(B) -> B;
text(A) when is_atom(A) -> atom_to_binary(A, utf8);
text(I) when is_integer(I) -> integer_to_binary(I);
text(F) when is_float(F) -> float_to_binary(F, [short]);
text(L) when is_list(L) ->
    case unicode:characters_to_binary(L) of
        B when is_binary(B) -> B;
        _ -> error({aihtml, {bad_value, L}})
    end;
text(Other) -> error({aihtml, {bad_value, Other}}).
