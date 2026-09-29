%% The element record of aihtml_dock_layout (designs/05-records.md).
-ifndef(AIHTML_DOCK_LAYOUT_HRL).
-define(AIHTML_DOCK_LAYOUT_HRL, true).

-include("aihtml_element.hrl").

%% An IDE-style layout (sigil's dock_layout): splits, tab groups and a
%% document area; tabs are dragged to dock them elsewhere or to float,
%% groups auto hide at an edge, splitbars resize. `layout' is the tree
%% (or the saved JSON), `panels' the panels it refers to by id. The value
%% is the layout JSON; postback fires on change (after the user
%% rearranged, resized, closed or switched tabs). ah:panel-close carries
%% the closed panel ids in Event.data (panels, comma separated).
-record(ah_dock_layout, {?AH_BASE(aihtml_dock_layout),
                         layout = [] :: aihtml_dock_layout:layout(),
                         disabled = false :: boolean(),
                         panels = [] :: [aihtml_dock_layout:panel()],
                         resizable = true :: boolean(),
                         resize_mode = live :: live | feedback,
                         allow_float = true :: boolean(),
                         allow_dock = true :: boolean(),
                         min_size = 100 :: non_neg_integer(),
                         labels = #{} :: aihtml_dock_layout:labels(),
                         name = undefined :: undefined | atom() | iodata()}).

-endif.
