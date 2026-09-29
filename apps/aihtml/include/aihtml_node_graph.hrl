%% The element record of aihtml_node_graph (designs/05-records.md). Field
%% names follow the catalog: flags and options are fields of the same
%% name, with the catalog's defaults (aihtml_node_graph_tests checks that
%% they agree).
-ifndef(AIHTML_NODE_GRAPH_HRL).
-define(AIHTML_NODE_GRAPH_HRL, true).

-include("aihtml_element.hrl").

%% A node editor: node cards with typed slots, links dragged between
%% them, pan and zoom. Postback fires on change: every edit (move,
%% connect, disconnect, delete, add, resize, rename, collapse, groups,
%% undo/redo), with the whole graph as JSON in Event.value and the edit
%% in Event.data (op, changed, removed). Without an `id' one is generated
%% at render.
-record(ah_node_graph, {?AH_BASE(aihtml_node_graph),
                        graph = #{} :: aihtml_node_graph:graph(),
                        name = undefined :: undefined | atom() | iodata(),
                        read_only = false :: boolean(),
                        minimap = false :: boolean(),
                        auto_fit = false :: boolean(),
                        allow_cycles = false :: boolean(),
                        no_toolbar = false :: boolean(),
                        no_grid = false :: boolean(),
                        link_mode = spline :: aihtml_node_graph:link_mode(),
                        snap = undefined :: undefined | pos_integer(),
                        height = 400 :: pos_integer() | auto | iodata(),
                        library = [] :: [aihtml_node_graph:library_item()],
                        layout = none :: none | auto,
                        label = <<"Node graph">> :: unicode:chardata()}).

-endif.
