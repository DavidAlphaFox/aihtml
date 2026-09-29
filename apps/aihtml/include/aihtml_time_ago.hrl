%% Element record of aihtml_time_ago (designs/05-records.md). Field
%% names follow the catalog: modifier groups, flags and options are
%% fields of the same name, with the catalog's defaults (aihtml_time_ago_tests
%% checks that they agree). Option fields default to what the component
%% used when the option was left out.
-ifndef(AIHTML_TIME_AGO_HRL).
-define(AIHTML_TIME_AGO_HRL, true).

-include("aihtml_element.hrl").

%% Relative time. `timestamp' is Unix seconds, a UTC datetime or an RFC
%% 3339 binary; `now' undefined is the time of rendering. No postback
%% event.
-record(ah_time_ago, {?AH_BASE(aihtml_time_ago),
                      timestamp = undefined :: undefined | integer() | calendar:datetime()
                                             | binary(),
                      now = undefined :: undefined | integer(),
                      labels = #{} :: #{atom() => iodata()},
                      live = true :: boolean(),
                      title = true :: boolean()}).

-endif.
