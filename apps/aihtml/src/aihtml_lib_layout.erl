%%%-------------------------------------------------------------------
%%% @doc Helpers shared by the layout components (card, panel, expander,
%%% tabs, tab_bar, breadcrumbs, pagination, steps, skeleton, loader,
%%% empty) and the types their records share. Internal to aihtml.
%%%
%%% Value-bearing layout components (tabs, tab_bar, pagination, steps,
%%% expander) keep their value in `data-ah-value' on the root and fire
%%% `change' there when the user changes it, so `on(change, Action)' on the
%%% root receives the new value as `Event.value'; `hidden/2' renders the
%%% hidden input of a `name'. The browser side is AH.lib.layout
%%% (assets/js/components/_lib_layout.js).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_lib_layout).

-export([maybe_el/3, maybe_el/4, with_id/2, bool/2, one_of/3, hidden/2, tf/1, bin/1,
         text_of/1, len/1, style/1, active_key/2]).

-export_type([key/0, name/0, css_length/0]).

%% A tab, item or step key.
-type key() :: binary() | atom() | integer() | string().
%% The name of a hidden input (or an accordion group).
-type name() :: undefined | atom() | iodata().
%% Integer pixels or a CSS length.
-type css_length() :: undefined | integer() | binary() | string().

-type html() :: aihtml_html:html().

%% @doc `X' in a `Tag' with class `Class', nothing when it is undefined.
-spec maybe_el(atom(), undefined | html(), binary()) -> html().
maybe_el(Tag, X, Class) -> maybe_el(Tag, X, Class, []).

-spec maybe_el(atom(), undefined | html(), binary(), aihtml_html:attrs()) -> html().
maybe_el(_Tag, undefined, _Class, _Attrs) -> [];
maybe_el(Tag, X, Class, Attrs) -> aihtml_html:el(Tag, X, [Class], Attrs).

%% @doc The root id as a binary and the record carrying it: the `id' field,
%% an id among the attrs, or a generated Prefix-N.
-spec with_id(aihtml_element:element(), binary()) -> {binary(), aihtml_element:element()}.
with_id(R, Prefix) ->
    Id = case element(3, R) of
             undefined ->
                 case lists:keyfind(<<"id">>, 1, aihtml_html:attrs(element(5, R))) of
                     {_, V} when is_binary(V) -> V;
                     _ -> <<Prefix/binary, "-",
                            (integer_to_binary(erlang:unique_integer([positive])))/binary>>
                 end;
             V -> bin(V)
         end,
    {Id, setelement(3, R, Id)}.

%% @doc `V' if it is a boolean, else a bad_option error for `Field'.
-spec bool(atom(), term()) -> boolean().
bool(Field, V) -> one_of(Field, V, [true, false]).

%% @doc `V' if it is one of `Allowed', else a bad_option error for `Field'.
-spec one_of(atom(), T, [term()]) -> T.
one_of(Field, V, Allowed) ->
    lists:member(V, Allowed) orelse error({aihtml, {bad_option, Field, V}}),
    V.

%% @doc The hidden input of a component with a `name' (nothing without).
-spec hidden(name(), term()) -> html().
hidden(undefined, _) -> [];
hidden(Name, Value) ->
    aihtml_html:void(input, [], [{type, hidden}, {name, Name}, {value, Value}]).

-spec tf(boolean()) -> binary().
tf(true) -> <<"true">>;
tf(false) -> <<"false">>.

-spec bin(binary() | atom() | integer() | string()) -> binary().
bin(B) when is_binary(B) -> B;
bin(A) when is_atom(A) -> atom_to_binary(A, utf8);
bin(I) when is_integer(I) -> integer_to_binary(I);
bin(L) when is_list(L) -> unicode:characters_to_binary(L).

%% @doc Plain text of a label, for title/aria attributes (<<>> for markup).
-spec text_of(term()) -> binary().
text_of(B) when is_binary(B) -> B;
text_of(A) when is_atom(A) -> atom_to_binary(A, utf8);
text_of(L) when is_list(L) ->
    case io_lib:printable_unicode_list(L) of
        true -> unicode:characters_to_binary(L);
        false -> iolist_to_binary([text_of(X) || X <- L])
    end;
text_of(I) when is_integer(I) -> integer_to_binary(I);
text_of(_) -> <<>>.

%% @doc A CSS length: integers are pixels; false for undefined.
-spec len(css_length()) -> false | binary().
len(undefined) -> false;
len(N) when is_integer(N) -> <<(integer_to_binary(N))/binary, "px">>;
len(V) -> bin(V).

%% @doc A style attribute value from declarations; false and undefined
%% values are left out, undefined when none is left.
-spec style([{binary(), term()}]) -> undefined | binary().
style(Decls) ->
    case [[K, $:, V, $;] || {K, V} <- Decls, V =/= false, V =/= undefined] of
        [] -> undefined;
        S -> iolist_to_binary(S)
    end.

%% @doc The active key as a binary: `Active', or with undefined the first
%% key not disabled in `[{Key, Disabled}]' (<<>> if none).
-spec active_key(undefined | key(), [{binary(), boolean()}]) -> binary().
active_key(undefined, KDs) ->
    case [K || {K, false} <- KDs] of
        [K | _] -> K;
        [] -> <<>>
    end;
active_key(Active, _) -> bin(Active).
