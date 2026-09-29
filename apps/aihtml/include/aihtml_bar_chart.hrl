%% The element record of aihtml_bar_chart (designs/05-records.md). Field
%% names follow the catalog: modifier groups, flags and options are fields
%% of the same name, with the catalog's defaults (aihtml_bar_chart_tests
%% checks that they agree).
-ifndef(AIHTML_BAR_CHART_HRL).
-define(AIHTML_BAR_CHART_HRL, true).

-include("aihtml_element.hrl").

%% A bar chart, vertical or horizontal, grouped or stacked. Postback fires
%% on 'ah:chart-click', as for chart.
-record(ah_bar_chart, {?AH_BASE(aihtml_bar_chart),
                       series = [] :: [aihtml_lib_chart:series()],
                       horizontal = false :: boolean(),
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
                       bar_width = undefined :: undefined | number() | binary(),
                       width = undefined :: aihtml_lib_chart:size(),
                       height = undefined :: aihtml_lib_chart:size(),
                       renderer = canvas :: canvas | svg}).

-endif.
