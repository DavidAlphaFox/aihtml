%%%-------------------------------------------------------------------
%%% @doc The echarts chart, ported from sigil (data/chart). It draws any
%%% echarts option; the browser loads echarts on demand (a lazily loaded
%%% chunk) when the first chart mounts. The convenience
%%% charts (aihtml_area_chart, aihtml_bar_chart, aihtml_donut_chart,
%%% aihtml_radar_chart, aihtml_relation_graph) build their option from
%%% simple data and draw it the same way; what they share is in
%%% aihtml_lib_chart.
%%%
%%%   chart(Option, Css, Attrs)             any echarts option (an Erlang map)
%%%   chart_option(Chart)                   the echarts option of a chart record
%%%   chart_update(Ctx, Target, Chart)      (in an action) redraw a chart in place
%%%
%%% == How it works ==
%%%
%%% The server builds the whole echarts option (the convenience charts
%%% build it from their simple data, as sigil's helpers do) and writes it
%%% as JSON into a data island inside the chart's root:
%%%
%%%   <div class="ah-chart" data-ah="chart" role="figure" aria-describedby="c-data">
%%%     <script type="application/json" class="ah-chart-data">{...}</script>
%%%     <div class="ah-chart-text ah-sr-only" id="c-data"><table>...</table></div>
%%%   </div>
%%%
%%% The table is the chart's data as text, for search engines and screen
%%% readers (echarts draws on a canvas): the title as caption and the
%%% rows and columns of simple option shapes (a dataset, series on a
%%% category axis, pie / funnel, radar); for other shapes just the
%%% caption. It is hidden visually only (ah-sr-only), and kept in step
%%% with the data: chart_update/3 with a record sends the server's new
%%% table, other updates (a map, setOption and setData in the browser)
%%% have the browser rebuild it from echarts' merged option by the same
%%% rules (see aihtml_lib_chart:data_text/2).
%%%
%%% The behaviour (assets/js/components/_lib_chart.js) loads echarts,
%%% themes it from the --ah-* custom properties (palette, text, border,
%%% paper colours, font), draws the option, follows size changes and
%%% theme changes, and fires 'ah:chart-click' (and 'ah:chart-dblclick',
%%% 'ah:chart-legendselectchanged', ...) with the clicked item. Strings
%%% "--ah-color-x" or "var(--ah-color-x)" in an option are resolved
%%% against the theme in the browser, so options may name theme colours.
%%%
%%% An action updates a chart without re-rendering it with
%%% `chart_update(Ctx, Target, ChartOrOption)', which calls the behaviour
%%% method setOption with the option of a chart record (merged, so series
%%% animate to their new values) or with an option map; the methods
%%% setData, resize, showLoading, ... are reachable with
%%% aihtml_action:call/4 too.
%%%
%%% chart/3 builds an element record (#ah_chart{}, defined in
%%% include/aihtml_chart.hrl) and render/1 turns it into HTML, so pages
%%% may also write the record directly (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_chart).
-behaviour(aihtml_element).

-include("aihtml_chart.hrl").

-export([chart/3, chart_option/1, chart_update/3,
         render/1, fields/1, catalog/0, facade_extras/0]).

-export_type([element/0, option/0, chart_record/0, chart/0]).

-define(E, aihtml_element).
-define(L, aihtml_lib_chart).

-type css() :: aihtml_html:css().
-type attrs() :: aihtml_html:attrs().
-type html() :: aihtml_html:html().
-type option() :: aihtml_lib_chart:option().
-type element() :: #ah_chart{}.
%% A record of any chart component.
-type chart_record() :: element() | aihtml_area_chart:element() | aihtml_bar_chart:element()
                      | aihtml_donut_chart:element() | aihtml_radar_chart:element()
                      | aihtml_relation_graph:element().
%% A chart record, or an option map for chart_update/3.
-type chart() :: chart_record() | option().

%%%===================================================================
%%% Builder
%%%===================================================================

%% @doc A chart of any echarts option. `Option' is a map as echarts
%% documents it (binaries for text; "--ah-color-*" strings are theme
%% colours). Css: `loading', `disabled'. Options: `height', `width'
%% (pixels or a CSS length; the default height is 400px), `renderer'
%% (canvas | svg).
-spec chart(option(), css(), attrs()) -> #ah_chart{}.
chart(Option, Css, Attrs) ->
    ?E:build(?MODULE, #ah_chart{option = Option}, Css, Attrs).

%% @doc The field names of the record.
-spec fields(atom()) -> [atom()].
fields(ah_chart) -> record_info(fields, ah_chart).

%% @doc Functions besides the components that the aihtml facade re-exports.
-spec facade_extras() -> [{atom(), arity()}].
facade_extras() -> [{chart_option, 1}, {chart_update, 3}].

%%%===================================================================
%%% Updating a chart from an action
%%%===================================================================

%% @doc The echarts option a chart record draws (for #ah_chart{} its
%% `option'), after the same checks as render/1.
-spec chart_option(chart_record()) -> option().
chart_option(#ah_chart{option = O}) -> ?L:check_option(O);
chart_option(R) when is_tuple(R), tuple_size(R) > 0 ->
    case element(1, R) of
        ah_area_chart -> aihtml_area_chart:option(R);
        ah_bar_chart -> aihtml_bar_chart:option(R);
        ah_donut_chart -> aihtml_donut_chart:option(R);
        ah_radar_chart -> aihtml_radar_chart:option(R);
        ah_relation_graph -> aihtml_relation_graph:option(R);
        _ -> error({aihtml, {not_a_chart, R}})
    end;
chart_option(Other) -> error({aihtml, {not_a_chart, Other}}).

%% @doc In an action: redraw the chart `Target' (usually `{id, Id}') in
%% place. With a chart record the chart gets that record's option, merged
%% into the current one so that series animate to their new data (a
%% relation graph is replaced, since nodes may have gone), and the
%% server's readable data table of that record replaces the chart's
%% (aihtml_lib_chart:data_text/2); with a map the map is merged like
%% echarts' setOption and the browser rebuilds the table from the merged
%% option. Other fields of the record (size, css, attrs) are not applied:
%% re-render the chart for those.
-spec chart_update(aihtml_action:ctx(), aihtml_action:target(), chart()) -> ok.
chart_update(Ctx, Target, Option) when is_map(Option) ->
    aihtml_action:call(Ctx, Target, setOption, [?L:check_option(Option), false]);
chart_update(Ctx, Target, R) ->
    Opt = chart_option(R),
    aihtml_action:call(Ctx, Target, setOption, [Opt, element(1, R) =:= ah_relation_graph,
                                                ?L:data_text_update(R, Opt)]).

%%%===================================================================
%%% Rendering
%%%===================================================================

-spec render(element()) -> html().
render(#ah_chart{option = O, loading = L, disabled = D, width = W, height = H,
                 renderer = Rd} = R) ->
    Classes = ?E:classes(?MODULE, R),
    ?L:chart_root(R, Classes, ?L:check_option(O), L, D, W, H, Rd).

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => chart, category => data,
       signature => <<"chart(Option, Css, Attrs)">>,
       root => <<"ah-chart">>, flags => [loading, disabled],
       classes => #{loading => []},
       options => [width, height, renderer],
       behavior => <<"chart">>, events => ?L:events(),
       doc => <<"An echarts chart of any option built on the server, themed from the "
                "current theme and redrawn when it changes; echarts loads on demand.">>,
       option_docs => ?L:size_docs(),
       methods => ?L:methods()}].
