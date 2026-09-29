%% The element record of aihtml_area_chart (designs/05-records.md). Field
%% names follow the catalog: modifier groups, flags and options are fields
%% of the same name, with the catalog's defaults (aihtml_area_chart_tests
%% checks that they agree).
-ifndef(AIHTML_AREA_CHART_HRL).
-define(AIHTML_AREA_CHART_HRL, true).

-include("aihtml_element.hrl").

%% A line chart with filled areas (a plain line chart with `line'), the
%% option built here from series and categories. Postback fires on
%% 'ah:chart-click', as for chart.
-record(ah_area_chart, {?AH_BASE(aihtml_area_chart),
                        series = [] :: [aihtml_lib_chart:series()],
                        line = false :: boolean(),
                        straight = false :: boolean(),
                        stack = false :: boolean(),
                        loading = false :: boolean(),
                        disabled = false :: boolean(),
                        categories = [] :: [aihtml_lib_chart:text()],
                        title = undefined :: undefined | aihtml_lib_chart:text(),
                        colors = undefined :: undefined | [binary()],
                        y_name = undefined :: undefined | aihtml_lib_chart:text(),
                        legend = bottom :: aihtml_lib_chart:legend(),
                        grid = true :: boolean(),
                        tooltip = true :: boolean(),
                        width = undefined :: aihtml_lib_chart:size(),
                        height = undefined :: aihtml_lib_chart:size(),
                        renderer = canvas :: canvas | svg}).

-endif.
