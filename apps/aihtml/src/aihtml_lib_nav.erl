%%%-------------------------------------------------------------------
%%% @doc Internal: what the navigation components (menu, navbar, sidenav,
%%% toolbar, splitter, listmenu) share: the item shape and its helpers.
%%%
%%% Menus, the navbar, the sidenav and the listmenu take the same item
%%% shape (`item()'):
%%%
%%%   #{key => Key,            value written to data-ah-value on selection
%%%     label => Html,
%%%     icon => Icon,          binary: an image URL; other html: inline (SVG)
%%%     href => Url,           follow the link instead of selecting
%%%     target => Target,
%%%     disabled => boolean(),
%%%     children => [item()],  submenu / drill-down page / tree node
%%%     columns => [#{header => Html, children => [item()]}],   menu only
%%%     open => left | up | [left | up],                        menu only
%%%     expanded => boolean()} sidenav only: node open initially
%%%   | divider                a separator
%%%   | {Key, Label}           shorthand for #{key => Key, label => Label}
%%%
%%% Selecting an item without `href' sets `data-ah-value' on the root to
%%% the item's key and fires `change' there, so `on(change, Action)' in the
%%% root's Attrs receives it (Event.value is the key).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_lib_nav).

-export([norm/1, label/1, key_attr/1, key_bin/1, value_attr/1, is_value/2,
         hidden_input/2, icon/2, text_or/2, px/1, num/1]).

-export_type([key/0, icon/0, item/0, name/0, px/0]).

-type key() :: atom() | binary() | integer().
%% binary: an image URL; other html: inline (SVG).
-type icon() :: binary() | aihtml_html:html().
%% A menu / navbar / sidenav / listmenu item (see the module doc).
-type item() :: #{key => key(), label => aihtml_html:html(),
                  icon => icon(), href => binary(), target => binary(),
                  disabled => boolean(),
                  children => [item()],
                  columns => [#{header => aihtml_html:html(),
                                children => [item()]}],
                  open => left | up | [left | up],
                  expanded => boolean(),
                  divider => true}
              | divider | {key(), aihtml_html:html()}.
-type name() :: undefined | atom() | iodata().
%% Pixels, or a CSS length as a binary.
-type px() :: number() | binary().

%% @doc An item in its map form, or `divider'.
-spec norm(term()) -> map() | divider.
norm(divider) -> divider;
norm(separator) -> divider;
norm(#{divider := true}) -> divider;
norm(#{} = M) -> M;
norm({K, L}) -> #{key => K, label => L}.

%% @doc The label of a normalised item (its key when it has none).
-spec label(map()) -> aihtml_html:html().
label(Item) -> maps:get(label, Item, key_label(maps:get(key, Item, <<>>))).

key_label(K) when is_atom(K) -> atom_to_binary(K);
key_label(K) -> K.

%% @doc The item's key as an attribute value.
-spec key_attr(map()) -> binary() | undefined.
key_attr(#{key := K}) -> key_bin(K);
key_attr(_) -> undefined.

-spec key_bin(key() | string()) -> binary().
key_bin(K) when is_atom(K) -> atom_to_binary(K);
key_bin(K) when is_integer(K) -> integer_to_binary(K);
key_bin(K) when is_binary(K) -> K;
key_bin(K) when is_list(K) -> unicode:characters_to_binary(K).

-spec value_attr(key() | undefined) -> binary() | undefined.
value_attr(undefined) -> undefined;
value_attr(V) -> key_bin(V).

%% @doc Whether a normalised item has the key `V'.
-spec is_value(term(), key() | undefined) -> boolean().
is_value(_, undefined) -> false;
is_value(#{key := K}, V) -> key_bin(K) =:= key_bin(V);
is_value(_, _) -> false.

-spec hidden_input(name(), key() | undefined) -> aihtml_html:html().
hidden_input(undefined, _Value) -> [];
hidden_input(Name, Value) ->
    aihtml_html:void(input, [], [{type, hidden}, {name, Name},
                                 {value, case Value of
                                             undefined -> <<>>;
                                             _ -> key_bin(Value)
                                         end}]).

%% @doc An icon: an image for a binary (URL), a span around other html.
-spec icon(binary(), icon() | undefined) -> aihtml_html:html().
icon(_Class, undefined) -> [];
icon(Class, Src) when is_binary(Src) ->
    aihtml_html:void(img, [Class], [{src, Src}, {alt, <<>>}]);
icon(Class, Html) ->
    aihtml_html:el(span, Html, [Class], [{aria_hidden, <<"true">>}]).

-spec text_or(term(), Default) -> binary() | Default.
text_or(B, _) when is_binary(B), B =/= <<>> -> B;
text_or(_, Default) -> Default.

-spec px(px()) -> iodata().
px(N) when is_integer(N) -> [integer_to_binary(N), <<"px">>];
px(N) when is_float(N) -> [num(N), <<"px">>];
px(B) when is_binary(B) -> B.

%% @doc A number with at most three decimals and no trailing zeros.
-spec num(number()) -> binary().
num(F) ->
    R = round(F * 1000) / 1000,
    case R == trunc(R) of
        true -> integer_to_binary(trunc(R));
        false -> float_to_binary(R, [{decimals, 3}, compact])
    end.
