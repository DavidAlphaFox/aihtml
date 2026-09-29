%% Element record of aihtml_heatmap_calendar (designs/05-records.md). Field names follow
%% the catalog: modifier groups, flags and options are fields of the same
%% name, with the catalog's defaults (aihtml_heatmap_calendar_tests checks that they
%% agree). The field types are aihtml_heatmap_calendar's.
-ifndef(AIHTML_HEATMAP_CALENDAR_HRL).
-define(AIHTML_HEATMAP_CALENDAR_HRL, true).

-include("aihtml_element.hrl").

%% A GitHub-style contribution heatmap: one column per week, one cell per
%% day, coloured by thresholds. Postback fires on 'ah:select' (a click on
%% a day; Event.value is its date).
-record(ah_heatmap_calendar, {?AH_BASE(aihtml_heatmap_calendar),
                              data = #{} :: aihtml_heatmap_calendar:data(),
                              months = 12 :: pos_integer(),
                              end_date = undefined :: undefined | aihtml_heatmap_calendar:day(),
                              thresholds = [0, 1, 3, 6] :: [number()],
                              weekday_labels = undefined :: undefined | [unicode:chardata()],
                              month_labels = undefined :: undefined | [unicode:chardata()],
                              legend = {<<"Less">>, <<"More">>}
                                  :: false | {aihtml_html:html(), aihtml_html:html()},
                              tooltip = <<"{value} · {date}"/utf8>> :: unicode:chardata()}).

-endif.
