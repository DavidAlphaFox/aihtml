%%%-------------------------------------------------------------------
%%% @doc The four theme axes, borrowed from sigil.
%%%
%%% A theme is four independent attributes on `<html>'. Each axis owns one
%%% slice of the `--ah-*' CSS custom properties, and no axis writes another
%%% axis's properties (see designs/01-architecture.md):
%%%
%%%   appearance  data-theme       neutrals: surfaces, text, borders
%%%   palette     data-palette     accent colours only
%%%   typography  data-typography  font families only
%%%   skin        data-skin        shape only: radius, border width, shadow
%%%
%%% The values and their CSS come from sigil (priv/css/sigil).
%%%
%%% Because prefabs emit semantic classes whose styles read those
%%% properties, switching an axis restyles every prefab without touching
%%% the HTML.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_theme).

-export([axes/0, default/0, attrs/1, normalize/1, switcher/2, catalog/0]).

-export_type([axis/0, theme/0]).

-type axis() :: appearance | palette | typography | skin.
-type theme() :: #{axis() => atom()}.

%% @doc Axis, attribute, allowed values, default. The CSS under
%% priv/css/sigil must define every value listed here except the defaults,
%% which are the unqualified :root values.
-spec axes() -> [{axis(), binary(), [atom()], atom()}].
axes() ->
    [{appearance, <<"data-theme">>, [light, dark, paper], light},
     {palette, <<"data-palette">>,
      [default, green, luxury, retro, arctic, nature, editorial, ember, dracula,
       midnight, brutal, 'brutal-blue', island, phoqus], default},
     {typography, <<"data-typography">>, [default, serif, grotesk], default},
     {skin, <<"data-skin">>, [default, brutal, island, phoqus], default}].

-spec default() -> theme().
default() ->
    maps:from_list([{Axis, Default} || {Axis, _, _, Default} <- axes()]).

%% @doc Fill in defaults and validate every axis value.
-spec normalize(theme()) -> theme().
normalize(Theme) ->
    Unknown = maps:keys(Theme) -- [A || {A, _, _, _} <- axes()],
    Unknown =:= [] orelse error({aihtml, {unknown_theme_axis, Unknown}}),
    maps:from_list(
      [{Axis, check(Axis, maps:get(Axis, Theme, Default), Values)}
       || {Axis, _, Values, Default} <- axes()]).

%% @doc The `<html>' attributes for a theme.
-spec attrs(theme()) -> [{binary(), binary()}].
attrs(Theme) ->
    T = normalize(Theme),
    [{Attr, atom_to_binary(maps:get(Axis, T), utf8)} || {Axis, Attr, _, _} <- axes()].

check(Axis, V, Values) when is_binary(V) ->
    check(Axis, binary_to_existing(V, Axis), Values);
check(Axis, V, Values) ->
    case lists:member(V, Values) of
        true  -> V;
        false -> error({aihtml, {bad_theme_value, Axis, V, Values}})
    end.

binary_to_existing(V, Axis) ->
    try binary_to_existing_atom(V, utf8)
    catch error:badarg -> error({aihtml, {bad_theme_value, Axis, V}})
    end.

%%%===================================================================
%%% Theme switcher
%%%===================================================================

%% @doc Four selects, one per axis; the runtime keeps them in step with
%% <html> and saves the choice (AH.theme).
-spec switcher(aihtml_html:css(), aihtml_html:attrs()) -> aihtml_html:element().
switcher(Css, Attrs) ->
    Items = [aihtml_html:el(label,
                 [aihtml_html:el(span, atom_to_binary(Axis, utf8),
                                 [<<"ah-theme-switcher-label">>], []),
                  aihtml_html:el(select,
                      [aihtml_html:el(option, V, [], [{value, V}, {selected, V =:= Default}])
                       || V <- Values],
                      [<<"ah-theme-switcher-select">>],
                      [{data_ah_axis, Axis}, {aria_label, Axis}])],
                 [<<"ah-theme-switcher-item">>], [])
             || {Axis, _Attr, Values, Default} <- axes()],
    aihtml_html:el('div', Items,
                   aihtml_catalog:classes(hd(catalog()), Css),
                   [[{data_ah, <<"theme-switcher">>}], Attrs]).

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => theme_switcher, category => theme,
       signature => <<"theme_switcher(Css, Attrs)">>,
       root => <<"ah-theme-switcher">>, behavior => <<"theme-switcher">>,
       events => [<<"ah:theme">>],
       doc => <<"Four selects, one per theme axis.">>}].
