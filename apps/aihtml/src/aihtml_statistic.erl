%%%-------------------------------------------------------------------
%%% @doc The statistic component (designs/04-components.md): `ah_statistic/3'
%%% builds an #ah_statistic{} element record (include/aihtml_statistic.hrl)
%%% and render/1 turns it into HTML, so pages may also write the record
%%% directly (designs/05-records.md).
%%%
%%% The DOM and class names follow sigil so the ported stylesheets in
%%% priv/css/sigil/components apply unchanged.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_statistic).
-behaviour(aihtml_element).

-include("aihtml_statistic.hrl").

-export([ah_statistic/3, render/1, fields/1, catalog/0]).

-import(aihtml_lib_display, [blank/1, tf/1, none_for/1]).

-define(H, aihtml_html).
-define(E, aihtml_element).

-type html() :: aihtml_html:html().
-type css() :: aihtml_html:css().
-type attrs() :: aihtml_html:attrs().

%%%===================================================================
%%% Builder
%%%===================================================================

%% @doc Statistic: title, prefix, number, suffix and a delta arrow.
%% Options: `title', `prefix', `suffix', `precision', `group_separator'
%% (true), `delta'.
-spec ah_statistic(number() | html(), css(), attrs()) -> #ah_statistic{}.
ah_statistic(Value, Css, Attrs) ->
    ?E:build(?MODULE, #ah_statistic{value = Value}, Css, Attrs).

%% @doc The field names of #ah_statistic{}.
-spec fields(atom()) -> [atom()].
fields(ah_statistic) -> record_info(fields, ah_statistic).

%%%===================================================================
%%% Rendering
%%%===================================================================

-spec render(#ah_statistic{}) -> html().
render(#ah_statistic{value = Value, title = Title, prefix = Prefix, suffix = Suffix,
                     precision = Prec, delta = Delta, loading = Loading} = R) ->
    Cls = ?E:classes(?MODULE, R),
    Group = R#ah_statistic.group_separator =/= false,
    Dir = if not is_number(Delta) -> undefined;
             Delta > 0 -> up; Delta < 0 -> down; true -> flat end,
    Arrow = case Dir of up -> <<"▲"/utf8>>; down -> <<"▼"/utf8>>; _ -> <<"—"/utf8>> end,
    ?H:el('div',
          [[?H:el('div', Title, [<<"ah-statistic__title">>], []) || not blank(Title)],
           ?H:el('div', [[?H:el(span, Prefix, [<<"ah-statistic__prefix">>], []) || not blank(Prefix)],
                         ?H:el(span, format_number(Value, Prec, Group),
                               [<<"ah-statistic__number">>], []),
                         [?H:el(span, Suffix, [<<"ah-statistic__suffix">>], []) || not blank(Suffix)]],
                 [<<"ah-statistic__value">>], []),
           [?H:el('div', [?H:el(span, Arrow, [<<"ah-statistic__delta-arrow">>],
                                [{aria_hidden, <<"true">>}]),
                          ?H:el(span, format_number(abs(Delta), Prec, Group), [], [])],
                  [<<"ah-statistic__delta">>], [{data_direction, Dir}])
            || Dir =/= undefined]],
          Cls,
          [[{data_color, R#ah_statistic.color},
            {data_loading, tf(Loading)},
            {aria_busy, Loading andalso <<"true">>}],
           ?E:root_attrs(R, none)]).

%% sigil's statistic number: precision (toFixed) and thousands separators.
format_number(V, Prec, Group) when is_number(V) ->
    Fixed = case Prec of
                P when is_integer(P), P >= 0 -> float_to_binary(float(V), [{decimals, P}]);
                _ when is_integer(V) -> integer_to_binary(V);
                _ -> num_js(V)
            end,
    {Sign, Digits} = case Fixed of
                         <<"-", D/binary>> -> {<<"-">>, D};
                         D -> {<<>>, D}
                     end,
    {Int, Dec} = case binary:split(Digits, <<".">>) of
                     [I] -> {I, <<>>};
                     [I, F] -> {I, <<".", F/binary>>}
                 end,
    iolist_to_binary([Sign, if Group -> group3(Int); true -> Int end, Dec]);
format_number(V, _Prec, _Group) -> V.

%% JavaScript's String(number): integral floats print without a fraction.
num_js(F) when F == trunc(F), abs(F) < 1.0e21 -> integer_to_binary(trunc(F));
num_js(F) -> float_to_binary(F, [short]).

group3(Int) ->
    N = byte_size(Int),
    case N =< 3 of
        true -> Int;
        false ->
            Head = N rem 3,
            <<H:Head/binary, Tail/binary>> = Int,
            Chunks = [C || <<C:3/binary>> <= Tail],
            iolist_to_binary(lists:join(<<",">>, [H || H =/= <<>>] ++ Chunks))
    end.

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [maps:merge(entry(), api())].

entry() ->
    #{name => statistic, category => data, root => <<"ah-statistic">>,
      signature => <<"ah_statistic(Value, Css, Attrs)">>,
      groups => #{color => {[default, primary, success, warning, error], default}},
      flags => [loading],
      classes => none_for([default, primary, success, warning, error, loading]),
      options => [title, prefix, suffix, precision, group_separator, delta],
      doc => <<"A number with title, prefix/suffix, grouping and a delta arrow.">>}.

%% The API tab of the docs page: options, flags and client methods.
api() ->
    #{option_docs => #{title => <<"Caption above the number.">>,
                       prefix => <<"Text before the number, e.g. a currency sign.">>,
                       suffix => <<"Text after the number, e.g. %.">>,
                       precision => <<"Decimal places.">>,
                       group_separator => <<"Thousands separators (default true).">>,
                       delta => <<"Change shown with ▲ (> 0), ▼ (< 0) or — (0)."/utf8>>,
                       loading => <<"Skeleton shimmer instead of the text.">>},
      methods => []}.
