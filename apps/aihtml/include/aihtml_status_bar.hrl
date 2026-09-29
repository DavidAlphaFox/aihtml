%% Element record of aihtml_status_bar (designs/05-records.md). Field
%% names follow the catalog: modifier groups, flags and options are fields
%% of the same name, with the catalog's defaults (aihtml_status_bar_tests
%% checks that they agree).
-ifndef(AIHTML_STATUS_BAR_HRL).
-define(AIHTML_STATUS_BAR_HRL, true).

-include("aihtml_element.hrl").

%% Status bar; no postback event. `content' adds the word count segment,
%% `dirty' the saved / unsaved dot.
-record(ah_status_bar, {?AH_BASE(aihtml_status_bar),
                        segments = [] :: [aihtml_status_bar:segment()],
                        content = undefined :: undefined | unicode:chardata(),
                        dirty = undefined :: undefined | boolean(),
                        labels = #{} :: #{atom() => aihtml_html:html()}}).

-endif.
