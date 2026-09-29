%% Element record of aihtml_rating_group (designs/05-records.md). Field names
%% follow the catalog: modifier groups, flags and options are fields of
%% the same name, with the catalog's defaults (aihtml_rating_group_tests checks
%% that they agree).
-ifndef(AIHTML_RATING_GROUP_HRL).
-define(AIHTML_RATING_GROUP_HRL, true).

-include("aihtml_element.hrl").

%% Star rating from 0 to `max'; `name' adds a hidden input. Postback fires
%% on change.
-record(ah_rating_group, {?AH_BASE(aihtml_rating_group),
                          max = 5 :: pos_integer(),
                          value = undefined :: undefined | number(),
                          size = md :: aihtml_lib_choice:size(),
                          color = warning :: aihtml_rating_group:color(),
                          name = undefined :: aihtml_lib_choice:name(),
                          precision = 1 :: number(),  % 1 or 0.5
                          allow_clear = true :: boolean(),
                          readonly = false :: boolean(),
                          disabled = false :: boolean()}).

-endif.
