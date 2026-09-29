%% Element record of aihtml_statistic (designs/05-records.md). Field
%% names follow the catalog: modifier groups, flags and options are
%% fields of the same name, with the catalog's defaults (aihtml_statistic_tests
%% checks that they agree). Option fields default to what the component
%% used when the option was left out.
-ifndef(AIHTML_STATISTIC_HRL).
-define(AIHTML_STATISTIC_HRL, true).

-include("aihtml_element.hrl").

%% A number with title, prefix / suffix and a delta arrow; a non-number
%% `value' is shown as is. No postback event.
-record(ah_statistic, {?AH_BASE(aihtml_statistic),
                       value = 0 :: number() | aihtml_html:html(),
                       color = default :: default | primary | success | warning | error,
                       loading = false :: boolean(),
                       title = undefined :: aihtml_html:html(),
                       prefix = undefined :: aihtml_html:html(),
                       suffix = undefined :: aihtml_html:html(),
                       precision = undefined :: undefined | non_neg_integer(),
                       group_separator = true :: boolean(),
                       delta = undefined :: undefined | number()}).

-endif.
