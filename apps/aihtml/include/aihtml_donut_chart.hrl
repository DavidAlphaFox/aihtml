%% The element record of aihtml_donut_chart (designs/05-records.md). Field
%% names follow the catalog: modifier groups, flags and options are fields
%% of the same name, with the catalog's defaults (aihtml_donut_chart_tests
%% checks that they agree).
-ifndef(AIHTML_DONUT_CHART_HRL).
-define(AIHTML_DONUT_CHART_HRL, true).

-include("aihtml_element.hrl").

%% A donut (or with `pie' a full pie) chart of named values. Postback
%% fires on 'ah:chart-click', as for chart.
-record(ah_donut_chart, {?AH_BASE(aihtml_donut_chart),
                         items = [] :: [aihtml_donut_chart:item()],
                         pie = false :: boolean(),
                         loading = false :: boolean(),
                         disabled = false :: boolean(),
                         title = undefined :: undefined | aihtml_lib_chart:text(),
                         colors = undefined :: undefined | [binary()],
                         legend = right :: aihtml_lib_chart:legend(),
                         labels = true :: boolean(),
                         tooltip = true :: boolean(),
                         radius = {<<"50%">>, <<"70%">>} :: {number() | binary(), number() | binary()},
                         center = undefined :: undefined | {number() | binary(), number() | binary()},
                         width = undefined :: aihtml_lib_chart:size(),
                         height = undefined :: aihtml_lib_chart:size(),
                         renderer = canvas :: canvas | svg}).

-endif.
