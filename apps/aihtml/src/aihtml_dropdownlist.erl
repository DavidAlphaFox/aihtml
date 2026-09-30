%%%-------------------------------------------------------------------
%%% @doc sigil's DropDownList (sigil.components.form.dropdownlist): a
%%% custom popup list with one choice.
%%%
%%%   ah_dropdownlist(Items, Value, Css, Attrs)
%%%
%%% Items are as in aihtml_lib_select. The markup and classes are
%%% sigil's, so the ported stylesheets under priv/css/sigil/components
%%% apply; behaviour is in assets/js/components/dropdownlist.ts.
%%%
%%% The component function builds an #ah_dropdownlist{} record (include/
%%% aihtml_dropdownlist.hrl) and render/1 turns it into HTML, so pages may
%%% also write the record directly (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_dropdownlist).
-behaviour(aihtml_element).

-include("aihtml_dropdownlist.hrl").

-export([ah_dropdownlist/4, render/1, fields/1, catalog/0]).

-define(E, aihtml_element).
-define(F, aihtml_lib_form).

%% @doc sigil's DropDownList: a combobox root showing the current label and
%% a popup listbox. Value-bearing: data-ah-value on the root, a hidden
%% input when the `name' option is given, `change' on the root.
-spec ah_dropdownlist([aihtml_lib_select:item()], aihtml_lib_select:value() | undefined,
                      aihtml_html:css(), aihtml_html:attrs()) -> #ah_dropdownlist{}.
ah_dropdownlist(Items, Value, Css, Attrs) ->
    ?E:build(?MODULE, #ah_dropdownlist{items = Items, value = Value}, Css, Attrs).

-spec render(#ah_dropdownlist{}) -> aihtml_html:html().
render(#ah_dropdownlist{items = Items, value = Value, disabled = Disabled,
                        placeholder = Placeholder} = R) ->
    Classes = ?E:classes(?MODULE, R),   % checks the modifier and flag fields first
    Norm = aihtml_lib_select:norm_items(Items),
    Val = opt_bin(Value),
    Content = case find_label(Val, Norm) of
                  none -> el(span, Placeholder, [<<"ah-dropdownlist-content">>,
                                                 <<"ah-dropdownlist-content-placeholder">>], []);
                  {ok, L} -> el(span, L, [<<"ah-dropdownlist-content">>], [])
              end,
    Arrow = case R#ah_dropdownlist.simple of
                true -> [];
                false -> el(span, el(span, <<"▼"/utf8>>, [<<"ah-dropdownlist-arrow-icon">>], []),
                            [<<"ah-dropdownlist-arrow">>], [{aria_hidden, <<"true">>}])
            end,
    Filter = case R#ah_dropdownlist.filterable of
                 true ->
                     el('div', aihtml_html:void(input, [<<"ah-listbox-filter-input">>],
                                                [{type, text}, {autocomplete, off},
                                                 {aria_label, aihtml_i18n:text(common, filter)},
                                                 {placeholder, R#ah_dropdownlist.filter_placeholder}]),
                        [<<"ah-listbox-filter">>], []);
                 false -> []
             end,
    Height = R#ah_dropdownlist.dropdown_height,
    List = case Norm of
               [] -> el('div', aihtml_i18n:text(common, no_data), [<<"ah-listbox-empty">>], []);
               _ -> el(ul, list_items(Norm, Val), [<<"ah-listbox-list">>], [{role, listbox}])
           end,
    Popup = el('div',
               el('div', [Filter,
                          el('div', List, [<<"ah-listbox-content">>],
                             [{style, [<<"max-height:">>, ?F:css_size(Height)]}])],
                  [<<"ah-listbox">>], []),
               [<<"ah-dropdownlist-popup">>], []),
    Hidden = hidden(R#ah_dropdownlist.name, Val),
    el('div', [el('div', [Content, Arrow], [<<"ah-dropdownlist-input-area">>], []),
               Popup, Hidden],
       Classes,
       [[{role, combobox}, {tabindex, case Disabled of true -> -1; false -> 0 end},
         {aria_haspopup, listbox}, {aria_expanded, <<"false">>},
         {aria_disabled, aria(Disabled)},
         {data_ah, <<"dropdownlist">>}, {data_ah_value, Val},
         {data_ah_placeholder, Placeholder}],
        ?E:root_attrs(R, change)]).

list_items(Norm, Val) ->
    {Html, _} = lists:mapfoldl(
                  fun({group, Label, Sub}, Idx) ->
                          {Lis, Idx1} = lists:mapfoldl(fun(I, N) -> {list_item(I, N, Val), N + 1} end,
                                                       Idx, Sub),
                          {[el(li, Label, [<<"ah-listbox-group">>], [{role, presentation}]) | Lis], Idx1};
                     (Item, Idx) ->
                          {list_item(Item, Idx, Val), Idx + 1}
                  end, 0, Norm),
    Html.

list_item({item, V, L, Dis}, Idx, Val) ->
    Sel = V =:= Val,
    el(li, el(span, L, [<<"ah-listbox-label">>], []),
       [<<"ah-listbox-item">>,
        [<<"ah-listbox-item-selected">> || Sel],
        [<<"ah-listbox-item-disabled">> || Dis]],
       [{role, option}, {data_idx, Idx}, {data_value, V},
        {aria_selected, atom_to_binary(Sel)}, {aria_disabled, aria(Dis)}]).

find_label(<<>>, _) -> none;
find_label(Val, Norm) ->
    case [L || {item, V, L, _} <- flat_items(Norm), V =:= Val] of
        [L | _] -> {ok, L};
        [] -> none
    end.

flat_items(Norm) ->
    lists:flatmap(fun({group, _, Sub}) -> Sub; (I) -> [I] end, Norm).

%%%===================================================================
%%% Record and catalog
%%%===================================================================

%% @doc The field names of #ah_dropdownlist{}.
-spec fields(atom()) -> [atom()].
fields(ah_dropdownlist) -> record_info(fields, ah_dropdownlist).

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => dropdownlist, category => form,
       signature => <<"ah_dropdownlist(Items, Value, Css, Attrs)">>,
       root => <<"ah-dropdownlist">>,
       groups => #{template => {[primary, success, warning, danger], none}},
       flags => [simple, disabled, block],
       options => [name, placeholder, filterable, filter_placeholder, dropdown_height],
       behavior => <<"dropdownlist">>,
       events => [<<"change">>, <<"ah:open">>, <<"ah:close">>],
       option_docs => #{primary => <<"Primary-coloured border.">>,
                        success => <<"Success-coloured border.">>,
                        warning => <<"Warning-coloured border.">>,
                        danger => <<"Danger-coloured border.">>,
                        simple => <<"No arrow segment.">>,
                        disabled => <<"Greyed out, not focusable, does not open.">>,
                        block => <<"Fill the container width.">>,
                        name => <<"Name of the hidden input that submits the value.">>,
                        placeholder => <<"Text shown while nothing is selected (default \"Select…\").">>,
                        filterable => <<"true: a filter box at the top of the popup.">>,
                        filter_placeholder => <<"Placeholder of the filter box.">>,
                        dropdown_height => <<"Maximum list height, px or CSS length (default 200).">>},
       methods => [#{name => open, args => <<"()">>, doc => <<"Open the popup.">>},
                   #{name => close, args => <<"()">>, doc => <<"Close the popup.">>},
                   #{name => getValue, args => <<"()">>, doc => <<"Return data-ah-value.">>},
                   #{name => setValue, args => <<"(Value, Silent)">>, doc => <<"Select the item with this value (\"\" clears); fires change unless Silent is true.">>},
                   #{name => disable, args => <<"()">>, doc => <<"Disable the control.">>},
                   #{name => enable, args => <<"()">>, doc => <<"Enable the control.">>}],
       doc => <<"Single-choice popup list with keyboard navigation, type-ahead and optional "
                "filtering. Items: Value | {Value, Label} | {Value, Label, #{disabled => true}} "
                "| {group, Label, Items}. `simple' hides the arrow; `block' fills the width.">>}].

%%%===================================================================
%%% Internal
%%%===================================================================

el(Tag, Children, Css, Attrs) -> aihtml_html:el(Tag, Children, Css, Attrs).

hidden(undefined, _Val) -> [];
hidden(Name, Val) -> aihtml_html:void(input, [], [{type, hidden}, {name, Name}, {value, Val}]).

aria(true) -> <<"true">>;
aria(false) -> undefined.

opt_bin(undefined) -> <<>>;
opt_bin(V) -> ?F:bin(V).
