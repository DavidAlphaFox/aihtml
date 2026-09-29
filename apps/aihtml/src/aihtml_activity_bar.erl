%%%-------------------------------------------------------------------
%%% @doc The activity bar, ported from sigil (layout/activity_bar): a VS
%%% Code-style vertical rail of icon buttons. DOM and class names are the
%%% ones sigil renders, so the styles in priv/css/sigil apply unchanged;
%%% the behaviour is in assets/js/components/activity_bar.ts.
%%%
%%% The value (the active item) is in `data-ah-value' on the root, a
%%% `name' renders a hidden input, and user changes fire `change' on the
%%% root.
%%%
%%% activity_bar/4 builds an element record (#ah_activity_bar{},
%%% include/aihtml_activity_bar.hrl) and render/1 turns it into HTML
%%% (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_activity_bar).
-behaviour(aihtml_element).

-include("aihtml_activity_bar.hrl").

-export([activity_bar/4, render/1, fields/1, catalog/0]).

-export_type([item/0]).

%% the last clause rejects items outside the declared types at run time
-dialyzer({no_match, [activity_item/1]}).

-define(H, aihtml_html).
-define(E, aihtml_element).

-type css() :: aihtml_html:css().
-type attrs() :: aihtml_html:attrs().
%% {Value, Icon, Label} | {Value, Icon, Label, ItemAttrs} | divider.
%% Icon is HTML (a glyph or an SVG); Label is the tooltip and aria-label.
-type item() :: {term(), aihtml_html:html(), iodata()}
              | {term(), aihtml_html:html(), iodata(), aihtml_html:attrs()}
              | divider.

%%%===================================================================
%%% Builder
%%%===================================================================

%% @doc A vertical rail of icon buttons (role tablist). `Items' are
%% `{Value, Icon, Label}', `{Value, Icon, Label, ItemAttrs}' or `divider';
%% `Icon' is HTML, `Label' the tooltip and aria-label, `ItemAttrs' HTML
%% attributes of the button (`{disabled, true}' disables it). `Value' is
%% the active item. Css: `left' (default) or `right' places the active
%% marker on that edge.
-spec activity_bar([item()], term(), css(), attrs()) -> #ah_activity_bar{}.
activity_bar(Items, Value, Css, Attrs) ->
    ?E:build(?MODULE, #ah_activity_bar{items = Items, value = Value}, Css, Attrs).

%% @doc The field names of the record.
-spec fields(atom()) -> [atom()].
fields(ah_activity_bar) -> record_info(fields, ah_activity_bar).

%%%===================================================================
%%% Rendering
%%%===================================================================

-spec render(#ah_activity_bar{}) -> aihtml_html:html().
render(#ah_activity_bar{items = Items0, value = Value, name = Name,
                        placement = Placement} = R) ->
    Classes = ?E:classes(?MODULE, R),           % checks placement
    Cur = value_text(Value),
    Items = [activity_item(I) || I <- Items0],
    Enabled = [V || {V, _, _, IA} <- Items, not is_disabled(IA)],
    Focus = case lists:member(Cur, Enabled) of
                true -> Cur;
                false -> case Enabled of [F | _] -> F; [] -> undefined end
            end,
    Children =
        [case I of
             divider ->
                 ?H:el('div', [], [<<"ah-activity-bar__divider">>],
                       [{role, presentation}, {data_index, Idx}]);
             {V, Icon, Label, IA} ->
                 Active = V =:= Cur,
                 Off = is_disabled(IA),
                 ?H:el(button, ?H:el(span, Icon, [<<"ah-activity-bar__icon">>], []),
                       [<<"ah-activity-bar__item">>],
                       [[{type, button}, {role, tab}, {data_id, V},
                         {data_active, atom_to_binary(Active, utf8)},
                         {data_disabled, atom_to_binary(Off, utf8)},
                         {aria_selected, atom_to_binary(Active, utf8)},
                         {aria_label, Label}, {title, Label},
                         {tabindex, case V =:= Focus of true -> <<"0">>; false -> <<"-1">> end},
                         {disabled, Off}],
                        IA])
         end || {Idx, I} <- lists:zip(lists:seq(0, length(Items) - 1), Items)],
    ?H:el('div', [hidden_input(Name, Cur) | Children], Classes,
          [[{role, tablist}, {aria_orientation, vertical},
            {data_placement, Placement},
            {data_ah, <<"activity-bar">>}, {data_ah_value, Cur}],
           ?E:root_attrs(R, change)]).

activity_item(divider) -> divider;
activity_item({V, Icon, Label}) -> {text(V), Icon, text(Label), []};
activity_item({V, Icon, Label, IA}) -> {text(V), Icon, text(Label), IA};
activity_item(Other) -> error({aihtml, {bad_activity_bar_item, Other}}).

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => activity_bar, category => layout,
       signature => <<"activity_bar(Items, Value, Css, Attrs)">>,
       root => <<"ah-activity-bar">>,
       groups => #{placement => {[left, right], left}},
       classes => #{left => [], right => []},
       behavior => <<"activity-bar">>,
       events => [<<"change">>, <<"ah:select">>],
       doc => <<"A narrow VS Code-style rail of icon buttons; the value is the active item.">>,
       option_docs => #{},
       methods => [#{name => setValue, args => <<"(Value)">>,
                     doc => <<"Make an item active without firing change.">>},
                   #{name => getValue, args => <<"()">>, doc => <<"Return the active item.">>}]}].

%%%===================================================================
%%% Internal
%%%===================================================================

is_disabled(Attrs) ->
    case lists:keyfind(<<"disabled">>, 1, ?H:attrs(Attrs)) of
        {_, V} -> V =/= <<"false">>;
        false -> false
    end.

hidden_input(undefined, _) -> [];
hidden_input(Name, Value) ->
    ?H:void(input, [], [{type, hidden}, {name, Name}, {value, Value}]).

value_text(undefined) -> <<>>;
value_text(V) -> text(V).

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
