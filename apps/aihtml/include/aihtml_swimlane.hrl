%% Element record of aihtml_swimlane (designs/05-records.md). Field names
%% follow the catalog: flags and options are fields of the same name,
%% with the catalog's defaults (aihtml_swimlane_tests checks that they
%% agree).
-ifndef(AIHTML_SWIMLANE_HRL).
-define(AIHTML_SWIMLANE_HRL, true).

-include("aihtml_element.hrl").

%% A swimlane (cross-functional flow chart): lanes by phases, nodes in
%% the cells, flows as orthogonal lines. Clicking a node selects it and
%% highlights its flows (ah:select); with `editable' nodes are dragged to
%% another cell and the postback fires on ah:node-change (Event.data:
%% node, lane, phase, oldLane, oldPhase). Without an `id' one is
%% generated at render.
-record(ah_swimlane, {?AH_BASE(aihtml_swimlane),
                      items = [] :: [aihtml_swimlane:item()],
                      editable = false :: boolean(),
                      legend = false :: boolean(),
                      lanes = [] :: [aihtml_swimlane:lane()],
                      phases = [] :: [aihtml_swimlane:phase()],
                      flows = [] :: [aihtml_swimlane:flow()],
                      selected = undefined :: term(),
                      axis = discrete :: discrete | continuous,
                      value_domain = undefined :: undefined | {number(), number()},
                      value_ticks = undefined :: undefined | [number()],
                      axis_width = undefined :: undefined | pos_integer(),
                      lane_height = 110 :: pos_integer(),
                      phase_width = 190 :: pos_integer(),
                      node_width = 132 :: pos_integer(),
                      node_height = 52 :: pos_integer(),
                      lane_label_width = 150 :: pos_integer(),
                      height = undefined :: undefined | pos_integer(),
                      labels = #{} :: aihtml_swimlane:labels()}).

-endif.
