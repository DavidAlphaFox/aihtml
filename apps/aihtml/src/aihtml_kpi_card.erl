%%%-------------------------------------------------------------------
%%% @doc The kpi_card component (designs/04-components.md): `kpi_card/3'
%%% builds an #ah_kpi_card{} element record (include/aihtml_kpi_card.hrl)
%%% and render/1 turns it into HTML, so pages may also write the record
%%% directly (designs/05-records.md).
%%%
%%% The DOM and class names follow sigil so the ported stylesheets in
%%% priv/css/sigil/components apply unchanged.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_kpi_card).
-behaviour(aihtml_element).

-include("aihtml_kpi_card.hrl").

-export([kpi_card/3, render/1, fields/1, catalog/0]).

-export_type([icon/0]).

-import(aihtml_lib_display, [blank/1, method/3, svg/1, circle/3, line/4, polyline/1, path/1]).

-define(H, aihtml_html).
-define(E, aihtml_element).

%% Records may hold any value, whatever their field types say, so these
%% keep rejecting values outside the types at render time.
-dialyzer({no_match, [trend_class/1]}).

-type html() :: aihtml_html:html().
-type css() :: aihtml_html:css().
-type attrs() :: aihtml_html:attrs().
-type icon() :: users | download | install | star | trending_up | trending_down.

%%%===================================================================
%%% Builder
%%%===================================================================

%% @doc KPI card: title, big value and a trend badge. Options: `title',
%% `trend' (percent, > 0 up), `trend_label', `icon' (users, download,
%% install, star, trending_up, trending_down, or html).
-spec kpi_card(html(), css(), attrs()) -> #ah_kpi_card{}.
kpi_card(Value, Css, Attrs) ->
    ?E:build(?MODULE, #ah_kpi_card{value = Value}, Css, Attrs).

%% @doc The field names of #ah_kpi_card{}.
-spec fields(atom()) -> [atom()].
fields(ah_kpi_card) -> record_info(fields, ah_kpi_card).

%%%===================================================================
%%% Rendering
%%%===================================================================

-spec render(#ah_kpi_card{}) -> html().
render(#ah_kpi_card{value = Value, title = Title, trend = Trend, trend_label = TrendL} = R) ->
    Cls = ?E:classes(?MODULE, R),
    Icon = case R#ah_kpi_card.icon of
               undefined -> [];
               A when is_atom(A) -> kpi_icon(A);
               Html -> Html
           end,
    TrendCls = trend_class(Trend),
    TrendEl = [?H:el(span,
                     [?H:el(span, [?H:el(span, kpi_icon(if Trend > 0 -> trending_up;
                                                           true -> trending_down end),
                                         [<<"ah-kpi-card-trend-icon">>],
                                         [{aria_hidden, <<"true">>}]),
                                   ?H:el(span, format_trend(Trend),
                                         [<<"ah-kpi-card-trend-value">>], [])],
                           [TrendCls], []),
                      [?H:el(span, TrendL, [<<"ah-kpi-card-trend-label">>], []) || not blank(TrendL)]],
                     [<<"ah-kpi-card-trend">>], [])
               || Trend =/= undefined],
    ?H:el('div',
          ?H:el('div',
                [[?H:el('div', ?H:el(span, Icon, [<<"ah-kpi-card-icon">>], []),
                        [<<"ah-kpi-card-icon-wrapper">>], []) || Icon =/= []],
                 TrendEl,
                 ?H:el('div', ?H:el('div', [[?H:el('div', Title, [<<"ah-kpi-card-title">>], [])
                                             || not blank(Title)],
                                            ?H:el('div', Value, [<<"ah-kpi-card-value">>], [])],
                                    [<<"ah-kpi-card-value-section">>], []),
                       [<<"ah-kpi-card-body">>], [])],
                [<<"ah-kpi-card-content">>], []),
          [Cls, TrendCls], [[{data_ah, <<"kpi-card">>}], ?E:root_attrs(R, none)]).

trend_class(undefined) -> [];
trend_class(T) when is_number(T), T > 0 -> <<"ah-kpi-card-trend-up">>;
trend_class(T) when is_number(T) -> <<"ah-kpi-card-trend-down">>;
trend_class(T) -> error({aihtml, {bad_trend, T}}).

format_trend(T) ->
    [if T > 0 -> <<"+">>; true -> <<>> end,
     float_to_binary(float(T), [{decimals, 1}]), <<"%">>].

kpi_icon(trending_up) ->
    svg([polyline(<<"23 6 13.5 15.5 8.5 10.5 1 18">>), polyline(<<"17 6 23 6 23 12">>)]);
kpi_icon(trending_down) ->
    svg([polyline(<<"23 18 13.5 8.5 8.5 13.5 1 6">>), polyline(<<"17 18 23 18 23 12">>)]);
kpi_icon(users) ->
    svg([path(<<"M17 21v-2a4 4 0 0 0-4-4H5a4 4 0 0 0-4 4v2">>), circle(9, 7, 4),
         path(<<"M23 21v-2a4 4 0 0 0-3-3.87">>), path(<<"M16 3.13a4 4 0 0 1 0 7.75">>)]);
kpi_icon(download) ->
    svg([path(<<"M21 15v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4">>),
         polyline(<<"7 10 12 15 17 10">>), line(12, 15, 12, 3)]);
kpi_icon(install) ->
    svg([path(<<"M12 2L2 7l10 5 10-5-10-5z">>), path(<<"M2 17l10 5 10-5">>),
         path(<<"M2 12l10 5 10-5">>)]);
kpi_icon(star) ->
    svg([?H:el(polygon, [], [], [{points, <<"12 2 15.09 8.26 22 9.27 17 14.14 18.18 21.02 "
                                            "12 17.77 5.82 21.02 7 14.14 2 9.27 8.91 8.26 12 2">>}])]);
kpi_icon(Other) -> error({aihtml, {unknown_icon, Other}}).

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [maps:merge(entry(), api())].

entry() ->
    #{name => kpi_card, category => data, root => <<"ah-kpi-card">>,
      signature => <<"kpi_card(Value, Css, Attrs)">>,
      groups => #{color => {[primary, success, warning, info, error], none}},
      flags => [disabled],
      classes => maps:from_list([{M, [<<"ah-kpi-card--", (atom_to_binary(M))/binary>>]}
                                 || M <- [primary, success, warning, info, error]]),
      options => [title, trend, trend_label, icon], behavior => <<"kpi-card">>,
      doc => <<"Metric card with value and trend. Methods setValue(text), "
               "setTrend(pct).">>}.

%% The API tab of the docs page: options, flags and client methods.
api() ->
    #{option_docs => #{title => <<"Metric name.">>,
                       trend => <<"Percent change; > 0 is up (green), otherwise down (red).">>,
                       trend_label => <<"Small text after the trend, e.g. \"vs last month\".">>,
                       icon => <<"users, download, install, star, trending_up, trending_down, "
                                 "or HTML.">>,
                       disabled => <<"Dimmed and inert.">>},
      methods => [method(setValue, <<"(Text)">>, <<"Replace the value.">>),
                  method(setTrend, <<"(Percent)">>, <<"Replace the trend and its direction.">>)]}.
