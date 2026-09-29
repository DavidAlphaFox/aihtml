%% The element record of aihtml_radar_chart (designs/05-records.md). Field
%% names follow the catalog: modifier groups, flags and options are fields
%% of the same name, with the catalog's defaults (aihtml_radar_chart_tests
%% checks that they agree).
-ifndef(AIHTML_RADAR_CHART_HRL).
-define(AIHTML_RADAR_CHART_HRL, true).

-include("aihtml_element.hrl").

%% A radar chart: one polygon per series over named axes (indicators).
%% Postback fires on 'ah:chart-click', as for chart.
-record(ah_radar_chart, {?AH_BASE(aihtml_radar_chart),
                         series = [] :: [aihtml_lib_chart:series()],
                         shape = polygon :: polygon | circle,
                         loading = false :: boolean(),
                         disabled = false :: boolean(),
                         indicators = [] :: [aihtml_radar_chart:indicator()],
                         title = undefined :: undefined | aihtml_lib_chart:text(),
                         colors = undefined :: undefined | [binary()],
                         legend = bottom :: aihtml_lib_chart:legend(),
                         tooltip = true :: boolean(),
                         split_number = 4 :: pos_integer(),
                         radius = <<"62%">> :: number() | binary(),
                         area_opacity = 0.18 :: number(),
                         width = undefined :: aihtml_lib_chart:size(),
                         height = undefined :: aihtml_lib_chart:size(),
                         renderer = canvas :: canvas | svg}).

-endif.
