%% Element record of aihtml_kpi_card (designs/05-records.md). Field
%% names follow the catalog: modifier groups, flags and options are
%% fields of the same name, with the catalog's defaults (aihtml_kpi_card_tests
%% checks that they agree). Option fields default to what the component
%% used when the option was left out.
-ifndef(AIHTML_KPI_CARD_HRL).
-define(AIHTML_KPI_CARD_HRL, true).

-include("aihtml_element.hrl").

%% A metric card; `trend' is a percentage (> 0 up). No postback event.
-record(ah_kpi_card, {?AH_BASE(aihtml_kpi_card),
                      value = [] :: aihtml_html:html(),
                      color = undefined :: undefined | primary | success | warning | info
                                         | error,
                      disabled = false :: boolean(),
                      title = undefined :: aihtml_html:html(),
                      trend = undefined :: undefined | number(),
                      trend_label = undefined :: aihtml_html:html(),
                      icon = undefined :: undefined | aihtml_kpi_card:icon()
                                        | aihtml_html:html()}).

-endif.
