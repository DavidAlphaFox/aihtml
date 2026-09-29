%% The element record of aihtml_tile_layout (designs/05-records.md): sigil's
%% IDE-style layout of resizable, tabbed panes. Field names follow the
%% catalog: options are fields of the same name, with the catalog's
%% defaults (aihtml_tile_layout_tests checks that they agree).
-ifndef(AIHTML_TILE_LAYOUT_HRL).
-define(AIHTML_TILE_LAYOUT_HRL, true).

-include("aihtml_element.hrl").

%% A layout of resizable panes and tab groups whose tabs the user drags
%% between groups and to the edges of panes (sigil's tile layout). The
%% arrangement is view state: `value' is a saved arrangement (the JSON of
%% data-ah-value) to render instead of the layout's own; each resize,
%% move, close or tab switch updates data-ah-value and fires change, and
%% postback fires on change.
-record(ah_tile_layout, {?AH_BASE(aihtml_tile_layout),
                         layout = {columns, []} :: aihtml_tile_layout:layout_node(),
                         value = undefined :: undefined | iodata() | map(),
                         name = undefined :: undefined | atom() | iodata(),
                         splitbar_size = 4 :: pos_integer(),
                         height = undefined :: undefined | integer() | iodata(),
                         disabled = false :: boolean()}).

-endif.
