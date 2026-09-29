%% Element record of aihtml_meter (designs/05-records.md). Field
%% names follow the catalog: modifier groups, flags and options are
%% fields of the same name, with the catalog's defaults (aihtml_meter_tests
%% checks that they agree). Option fields default to what the component
%% used when the option was left out.
-ifndef(AIHTML_METER_HRL).
-define(AIHTML_METER_HRL, true).

-include("aihtml_element.hrl").

%% A measurement in [min, max], coloured low / optimum / high. No postback
%% event.
-record(ah_meter, {?AH_BASE(aihtml_meter),
                   value = 0 :: number(),
                   size = md :: sm | md | lg,
                   min = 0 :: number(),
                   max = 100 :: number(),
                   low = undefined :: undefined | number(),
                   high = undefined :: undefined | number(),
                   optimum = undefined :: undefined | number(),
                   label = undefined :: aihtml_html:html(),
                   helper_text = undefined :: aihtml_html:html(),
                   show_value = false :: boolean()}).

-endif.
