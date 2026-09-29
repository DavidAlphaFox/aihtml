%% Element record of aihtml_splitter (designs/05-records.md). Field names
%% follow the catalog: modifier groups, flags and options are fields of
%% the same name, with the catalog's defaults (aihtml_splitter_tests
%% checks that they agree).
-ifndef(AIHTML_SPLITTER_HRL).
-define(AIHTML_SPLITTER_HRL, true).

-include("aihtml_element.hrl").

%% Two panes and a split bar; exactly one or two panes. Postback fires on
%% change (end of a resize; the value is "First,Second" in percent).
-record(ah_splitter, {?AH_BASE(aihtml_splitter),
                      panes = [] :: [aihtml_splitter:pane()],
                      orientation = vertical :: vertical | horizontal,
                      disabled = false :: boolean(),
                      splitbar_size = 5 :: aihtml_lib_nav:px(),
                      resizable = true :: boolean(),
                      step = undefined :: undefined | aihtml_lib_nav:px(),
                      name = undefined :: aihtml_lib_nav:name()}).

-endif.
