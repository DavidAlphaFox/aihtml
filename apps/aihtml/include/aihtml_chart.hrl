%% The element record of aihtml_chart (designs/05-records.md). Field
%% names follow the catalog: flags and options are fields of the same
%% name, with the catalog's defaults (aihtml_chart_tests checks that they
%% agree).
-ifndef(AIHTML_CHART_HRL).
-define(AIHTML_CHART_HRL, true).

-include("aihtml_element.hrl").

%% A chart that draws any echarts option (loaded on demand with
%% AH.vendor("echarts")), themed from the --ah-* custom properties.
%% Postback fires on 'ah:chart-click' (Event.value is the clicked item's
%% name; Event.data has series, seriesIndex, name, value, index, kind).
-record(ah_chart, {?AH_BASE(aihtml_chart),
                   option = #{} :: aihtml_lib_chart:option(),
                   loading = false :: boolean(),
                   disabled = false :: boolean(),
                   width = undefined :: aihtml_lib_chart:size(),
                   height = undefined :: aihtml_lib_chart:size(),
                   renderer = canvas :: canvas | svg}).

-endif.
