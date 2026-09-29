%% Element record of aihtml_diff (designs/05-records.md). Field names follow
%% the catalog: modifier groups, flags and options are fields of the same
%% name, with the catalog's defaults (aihtml_diff_tests checks that they
%% agree). The field types are aihtml_diff's.
-ifndef(AIHTML_DIFF_HRL).
-define(AIHTML_DIFF_HRL, true).

-include("aihtml_element.hrl").

%% A read-only view of the differences between two texts, computed on the
%% server (line or word level, unified or side by side); no postback.
-record(ah_diff, {?AH_BASE(aihtml_diff),
                  old = <<>> :: unicode:chardata(),
                  new = <<>> :: unicode:chardata(),
                  mode = line :: line | word,
                  view = unified :: unified | split,
                  line_numbers = false :: boolean(),
                  stats = false :: boolean()}).

-endif.
