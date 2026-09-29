%% Element record of aihtml_gantt (designs/05-records.md). Field names
%% follow the catalog: flags and options are fields of the same name,
%% with the catalog's defaults (aihtml_gantt_tests checks that they
%% agree).
-ifndef(AIHTML_GANTT_HRL).
-define(AIHTML_GANTT_HRL, true).

-include("aihtml_element.hrl").

%% A gantt chart: rows in a sidebar, task bars on a day scale, dependency
%% lines, today marker. With `editable', bars are dragged (move, resize)
%% and the postback fires on ah:task-change (Event.data: task, from, to,
%% row, kind, days). Without an `id' one is generated at render.
-record(ah_gantt, {?AH_BASE(aihtml_gantt),
                   items = [] :: [aihtml_gantt:task()],
                   editable = false :: boolean(),
                   no_dependencies = false :: boolean(),
                   rows = undefined :: undefined | [aihtml_gantt:row()],
                   collapsed = [] :: [term()],
                   height = 500 :: undefined | pos_integer(),
                   sidebar_width = 250 :: pos_integer(),
                   column_width = 60 :: pos_integer(),
                   row_height = 40 :: pos_integer(),
                   today = undefined :: aihtml_lib_date:date(),
                   labels = #{} :: aihtml_gantt:labels()}).

-endif.
