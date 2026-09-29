%%%-------------------------------------------------------------------
%%% @doc Internal helpers shared by dropdownlist and select: their item
%%% lists. Not part of the public API.
%%%
%%% Both take a list of
%%%
%%%   Value                           value and label are the same
%%%   {Value, Label}
%%%   {Value, Label, #{disabled => true}}
%%%   {group, Label, [Item]}          a group heading (select: optgroup)
%%%
%%% Values are binaries, atoms or numbers and are compared as text.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_lib_select).

-export([norm_items/1]).

-export_type([value/0, item/0, template/0, norm_item/0]).

-type value() :: binary() | atom() | number().
%% Value | {Value, Label} | {Value, Label, #{disabled => true}}
%% | {group, Label, [Item]}
-type item() :: value() | {value(), aihtml_html:html()}
              | {value(), aihtml_html:html(), #{disabled => boolean()}}
              | {group, aihtml_html:html(), [item()]}.
-type template() :: primary | success | warning | danger.
%% An item with its value as text: {item, Value, Label, Disabled}.
-type norm_item() :: {item, binary(), aihtml_html:html(), boolean()}
                   | {group, aihtml_html:html(), [norm_item()]}.

%% @doc The items with their values as text (aihtml_lib_form:bin/1).
-spec norm_items([item()]) -> [norm_item()].
norm_items(Items) ->
    [norm_item(I) || I <- Items].

norm_item({group, Label, Sub}) when is_list(Sub) ->
    {group, Label, [norm_item(I) || I <- Sub]};
norm_item({V, L}) -> {item, bin(V), L, false};
norm_item({V, L, #{} = O}) -> {item, bin(V), L, maps:get(disabled, O, false) =:= true};
norm_item(V) when is_binary(V); is_atom(V); is_number(V) -> {item, bin(V), bin(V), false};
norm_item(Other) -> error({aihtml, {bad_item, Other}}).

bin(V) -> aihtml_lib_form:bin(V).
