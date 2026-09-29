%%%-------------------------------------------------------------------
%%% @doc Internal: what the button components share (aihtml_button,
%%% aihtml_link_button, aihtml_toggle_button, aihtml_button_group,
%%% aihtml_segmented_control, aihtml_dropdown_button, aihtml_split_button).
%%%
%%% Items (button_group, segmented_control, menus) are
%%%   Label | {Value, Label} | {Value, Label, ItemAttrs}
%%% and a menu also takes `divider'. `ItemAttrs' are HTML attributes of
%%% the item's button, e.g. `[{disabled, true}]'; menus also read `icon'.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_lib_button).

-export([btn_groups/0, icon_option_docs/0, with_icon/4, menu_icon/2, item/1,
         roving_focus/4, take/2, is_disabled/1, hidden_input/2, value_attr/1,
         values/1, join/1, bin/1]).

-export_type([item/0]).

-define(H, aihtml_html).

%% Label | {Value, Label} | {Value, Label, ItemAttrs}; menus also take divider.
-type item() :: aihtml_html:html() | {term(), aihtml_html:html()}
              | {term(), aihtml_html:html(), aihtml_html:attrs()} | divider.

%% @doc The catalog groups of the `ah-btn' components (button, link_button,
%% toggle_button).
-spec btn_groups() -> #{atom() => {[atom()], atom()}}.
btn_groups() ->
    #{variant => {[primary, secondary, outlined, success, warning, error,
                   info, default, borderless], primary},
      size => {[sm, md, lg], md}}.

%% @doc The option docs of `round' and the icon options.
-spec icon_option_docs() -> #{atom() => binary()}.
icon_option_docs() ->
    #{round => <<"Pill-shaped corners.">>,
      icon => <<"HTML shown beside the text, e.g. a glyph or an SVG.">>,
      img => <<"URL of a 16px image shown beside the text.">>,
      icon_position => <<"Where the icon goes: left (default), right, top or bottom.">>}.

%% @doc A button's body with its icon or image, and the class that places it.
-spec with_icon(aihtml_html:html(), aihtml_html:html(), undefined | iodata(),
                aihtml_button:icon_position()) -> {aihtml_html:html(), aihtml_html:css()}.
with_icon(Content, Icon0, Img, Pos) ->
    lists:member(Pos, [left, right, top, bottom])
        orelse error({aihtml, {bad_option, icon_position, Pos}}),
    Icon = if
               Img =/= undefined ->
                   ?H:void(img, [<<"ah-btn-img">>],
                           [{src, Img}, {width, 16}, {height, 16}, {alt, <<>>}]);
               Icon0 =/= undefined ->
                   ?H:el(span, Icon0, [<<"ah-btn-img">>], [{aria_hidden, <<"true">>}]);
               true -> none
           end,
    case Icon of
        none -> {Content, []};
        _ ->
            Text = ?H:el(span, Content, [<<"ah-btn-text">>], []),
            Body = case Pos of
                       P when P =:= left; P =:= top -> [Icon, Text];
                       _ -> [Text, Icon]
                   end,
            {Body, [<<"ah-btn-img-", (atom_to_binary(Pos, utf8))/binary>>]}
    end.

%% @doc The icon of a menu item.
-spec menu_icon(binary(), term()) -> aihtml_html:html().
menu_icon(_Cls, undefined) -> [];
menu_icon(Cls, Icon) -> ?H:el(span, Icon, [Cls], [{aria_hidden, <<"true">>}]).

%% @doc An item as {Value, Label, ItemAttrs}.
-spec item(item()) -> {binary(), aihtml_html:html(), aihtml_html:attrs()}.
item({V, Label, IA}) -> {bin(V), Label, IA};
item({V, Label}) -> {bin(V), Label, []};
item(divider) -> error({aihtml, {divider_not_allowed_here}});
item(Label) -> {bin(Label), Label, []}.

%% @doc The value that gets keyboard focus in a roving-tabindex group.
-spec roving_focus(boolean(), [{binary(), aihtml_html:html(), aihtml_html:attrs()}],
                   [binary() | undefined], boolean()) -> binary() | undefined.
roving_focus(false, _, _, _) -> undefined;
roving_focus(true, Items, Selected, Disabled) ->
    Enabled = [V || {V, _, IA} <- Items, not Disabled, not is_disabled(IA)],
    case [V || V <- Enabled, lists:member(V, Selected)] of
        [V | _] -> V;
        [] -> case Enabled of [V | _] -> V; [] -> undefined end
    end.

%% @doc Normalised attributes, with Key taken out.
-spec take(binary(), aihtml_html:attrs()) -> {term(), [{binary(), term()}]}.
take(Key, Attrs) ->
    N = ?H:attrs(Attrs),
    case lists:keytake(Key, 1, N) of
        {value, {_, V}, Rest} -> {V, Rest};
        false -> {undefined, N}
    end.

-spec is_disabled(aihtml_html:attrs()) -> boolean().
is_disabled(Attrs) ->
    lists:keymember(<<"disabled">>, 1, ?H:attrs(Attrs)).

-spec hidden_input(undefined | atom() | iodata(), iodata()) -> aihtml_html:html().
hidden_input(undefined, _) -> [];
hidden_input(Name, Value) ->
    ?H:void(input, [], [{type, hidden}, {name, Name}, {value, Value},
                        {data_ah_input, true}]).

-spec value_attr(term()) -> binary() | undefined.
value_attr(undefined) -> undefined;
value_attr(V) -> bin(V).

%% @doc The selected values of a checkbox-mode group: a list or "a,b".
-spec values(term()) -> [binary()].
values(undefined) -> [];
values(B) when is_binary(B) -> binary:split(B, <<",">>, [global, trim_all]);
values(L) when is_list(L) ->
    case io_lib:printable_unicode_list(L) of
        true -> values(bin(L));
        false -> [bin(X) || X <- L]
    end;
values(X) -> [bin(X)].

-spec join([binary()]) -> binary().
join(Vs) -> iolist_to_binary(lists:join(<<",">>, Vs)).

-spec bin(term()) -> binary().
bin(B) when is_binary(B) -> B;
bin(A) when is_atom(A) -> atom_to_binary(A, utf8);
bin(I) when is_integer(I) -> integer_to_binary(I);
bin(F) when is_float(F) -> float_to_binary(F, [short]);
bin(L) when is_list(L) ->
    case unicode:characters_to_binary(L) of
        B when is_binary(B) -> B;
        _ -> error({aihtml, {bad_value, L}})
    end;
bin(Other) -> error({aihtml, {bad_value, Other}}).
