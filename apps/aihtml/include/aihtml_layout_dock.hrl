%% Element records of aihtml_layout_dock (designs/05-records.md).
-ifndef(AIHTML_LAYOUT_DOCK_HRL).
-define(AIHTML_LAYOUT_DOCK_HRL, true).

-include("aihtml_element.hrl").

%% An id of a docking window, a dock layout panel or group (written as
%% text in the markup and the layout JSON).
-type ah_dock_id() :: atom() | binary() | integer().

%% Options of a docking window: `collapsed' (only its header shows),
%% `pinned' (cannot be dragged) and `floating' ({X, Y} or {X, Y, Width}
%% in px from the container's top left: the window floats over the
%% panels).
-type ah_dock_window_opts() :: #{collapsed => boolean(), pinned => boolean(),
                                 floating => {number(), number()}
                                           | {number(), number(), number()}}.
%% A docking window: {Id, Title, Body}, {Id, Title, Body, Opts} or a map
%% with `id', `title', `body' and the options.
-type ah_dock_window() :: {ah_dock_id(), unicode:chardata(), aihtml_html:html()}
                        | {ah_dock_id(), unicode:chardata(), aihtml_html:html(),
                           ah_dock_window_opts()}
                        | #{id := ah_dock_id(), title => unicode:chardata(),
                            body => aihtml_html:html(), collapsed => boolean(),
                            pinned => boolean(), floating => tuple()}.
%% A docking panel (a column or row of windows): {Id, Windows} or
%% #{id := Id, windows := Windows}.
-type ah_dock_panel() :: {ah_dock_id(), [ah_dock_window()]}
                       | #{id := ah_dock_id(), windows := [ah_dock_window()]}.
%% The saved docking layout: the JSON of data-ah-value (a binary) or its
%% json:decode/1 map.
-type ah_dock_saved() :: undefined | binary() | map().

%% A dock layout panel: {Id, Title, Body} or #{id, title, body}. Title is
%% text; Body is ordinary server-rendered HTML.
-type ah_dl_panel() :: {ah_dock_id(), unicode:chardata(), aihtml_html:html()}
                     | #{id := ah_dock_id(), title => unicode:chardata(),
                         body => aihtml_html:html()}.
%% A panel in a layout tree: its id (looked up in the `panels' option)
%% or the panel itself.
-type ah_dl_ref() :: ah_dock_id() | ah_dl_panel().
%% Options of a layout node. size: share of the parent in percent (a
%% number or <<"22%">>), px for autohide; active: the id of the shown tab;
%% id: the group id; pin, close: the auto hide and close buttons of a tab
%% group (default true, close defaults to false for documents); x, y,
%% width, height: a float window in px.
-type ah_dl_opts() :: #{size => number() | binary(), active => ah_dock_id(),
                        id => ah_dock_id(), pin => boolean(), close => boolean(),
                        x => number(), y => number(), width => number(),
                        height => number()}.
%% A node of the layout tree:
%%   {split, horizontal | vertical, Children[, Opts]}  side by side / stacked
%%   {tabs, Refs[, Opts]}          a tab group (tool windows)
%%   {documents, Refs[, Opts]}     the document area (tabs, no auto hide)
%%   {panel, Ref[, Opts]}          a fixed panel with a header
%%   {float, Refs[, Opts]}         (top level) a floating window
%%   {autohide, left | right | top | bottom, Refs[, Opts]}
%%                                 (top level) a group hidden at an edge
%% or the same as a map with `type' (split, tabs, documents, panel,
%% float, autohide) and `orientation', `items' / `item', `edge' and the
%% options, which is the form of the layout JSON.
-type ah_dl_node() :: {split, horizontal | vertical, [ah_dl_node()]}
                    | {split, horizontal | vertical, [ah_dl_node()], ah_dl_opts()}
                    | {tabs | documents | float, [ah_dl_ref()]}
                    | {tabs | documents | float, [ah_dl_ref()], ah_dl_opts()}
                    | {panel, ah_dl_ref()} | {panel, ah_dl_ref(), ah_dl_opts()}
                    | {autohide, left | right | top | bottom, [ah_dl_ref()]}
                    | {autohide, left | right | top | bottom, [ah_dl_ref()], ah_dl_opts()}
                    | map().
%% A layout: one node, a list of top-level nodes (laid out in a row, plus
%% float and autohide nodes), or the saved JSON (data-ah-value, a binary).
-type ah_dl_layout() :: ah_dl_node() | [ah_dl_node()] | binary().
-type ah_dl_label_key() :: auto_hide | float | dock | close.
-type ah_dl_labels() :: #{ah_dl_label_key() => unicode:chardata()}.

%% Panels of windows (sigil's docking): windows are dragged by their
%% header between panels, collapsed, closed or left floating. `items' are
%% the panels; `layout' is a saved value (data-ah-value) applied to them.
%% The value is the layout JSON; postback fires on change (after a drop,
%% collapse, expand or close). ah:window-close, ah:window-collapse and
%% ah:window-expand carry the window id in Event.data (window).
-record(ah_docking, {?AH_BASE(aihtml_layout_dock),
                     items = [] :: [ah_dock_panel()],
                     orientation = horizontal :: horizontal | vertical,
                     disabled = false :: boolean(),
                     layout = undefined :: ah_dock_saved(),
                     allow_float = true :: boolean(),
                     offset = undefined :: undefined | non_neg_integer(),
                     drag_opacity = 0.3 :: number(),
                     close_buttons = true :: boolean(),
                     collapse_buttons = true :: boolean(),
                     labels = #{} :: #{collapse | close => unicode:chardata()},
                     name = undefined :: undefined | atom() | iodata()}).

%% An IDE-style layout (sigil's dock_layout): splits, tab groups and a
%% document area; tabs are dragged to dock them elsewhere or to float,
%% groups auto hide at an edge, splitbars resize. `layout' is the tree
%% (or the saved JSON), `panels' the panels it refers to by id. The value
%% is the layout JSON; postback fires on change (after the user
%% rearranged, resized, closed or switched tabs). ah:panel-close carries
%% the closed panel ids in Event.data (panels, comma separated).
-record(ah_dock_layout, {?AH_BASE(aihtml_layout_dock),
                         layout = [] :: ah_dl_layout(),
                         disabled = false :: boolean(),
                         panels = [] :: [ah_dl_panel()],
                         resizable = true :: boolean(),
                         resize_mode = live :: live | feedback,
                         allow_float = true :: boolean(),
                         allow_dock = true :: boolean(),
                         min_size = 100 :: non_neg_integer(),
                         labels = #{} :: ah_dl_labels(),
                         name = undefined :: undefined | atom() | iodata()}).

-endif.
