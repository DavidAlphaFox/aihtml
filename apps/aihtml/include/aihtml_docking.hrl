%% The element record of aihtml_docking (designs/05-records.md).
-ifndef(AIHTML_DOCKING_HRL).
-define(AIHTML_DOCKING_HRL, true).

-include("aihtml_element.hrl").

%% Panels of windows (sigil's docking): windows are dragged by their
%% header between panels, collapsed, closed or left floating. `items' are
%% the panels; `layout' is a saved value (data-ah-value) applied to them.
%% The value is the layout JSON; postback fires on change (after a drop,
%% collapse, expand or close). ah:window-close, ah:window-collapse and
%% ah:window-expand carry the window id in Event.data (window).
-record(ah_docking, {?AH_BASE(aihtml_docking),
                     items = [] :: [aihtml_docking:panel()],
                     orientation = horizontal :: aihtml_docking:orientation(),
                     disabled = false :: boolean(),
                     layout = undefined :: aihtml_docking:saved(),
                     allow_float = true :: boolean(),
                     offset = undefined :: undefined | non_neg_integer(),
                     drag_opacity = 0.3 :: number(),
                     close_buttons = true :: boolean(),
                     collapse_buttons = true :: boolean(),
                     labels = #{} :: #{collapse | close => unicode:chardata()},
                     name = undefined :: undefined | atom() | iodata()}).

-endif.
