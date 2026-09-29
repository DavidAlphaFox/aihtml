%% Element records of aihtml_data_graph (designs/05-records.md). Field
%% names follow the catalog: flags and options are fields of the same
%% name, with the catalog's defaults (aihtml_data_graph_tests checks that
%% they agree).
-ifndef(AIHTML_DATA_GRAPH_HRL).
-define(AIHTML_DATA_GRAPH_HRL, true).

-include("aihtml_element.hrl").

%% Node, link and group ids: written as binaries in the browser.
-type ah_graph_id() :: binary() | atom() | integer() | string().
%% A point of the graph's world coordinates, `{X, Y}' or `[X, Y]'.
-type ah_graph_point() :: {number(), number()} | [number()].
%% A slot (port) of a node: a name (any type), `{Name, Type}', or a map.
%% `type' is a data type such as <<"IMAGE">> ("IMAGE,MASK" for several,
%% "*" or none for any); slots of compatible types connect. `shape' is
%% circle (default), square, grid or hollow.
-type ah_graph_slot() :: binary() | atom() | {term(), term()}
                       | #{name := term(), type => term(), label => term(),
                           optional => boolean(), shape => circle | square | grid | hollow}.
%% A node card. `pos' is its top left corner; nodes without one are placed
%% by the server's layered layout. `widgets' are rows of HTML (aihtml
%% elements) shown under the slots, `body' free HTML after them; `data'
%% is any JSON-encodable term carried along untouched.
-type ah_graph_node() :: #{id := ah_graph_id(),
                           type => term(), title => term(),
                           pos => ah_graph_point(),
                           width => number(), height => number(),
                           collapsed => boolean(), color => term(),
                           inputs => [ah_graph_slot()], outputs => [ah_graph_slot()],
                           widgets => [aihtml_html:html()], body => aihtml_html:html(),
                           data => term()}.
%% A link from output `Index' of `source' to input `Index' of `target',
%% through the optional reroute `points'. `{Source, Target}' is short for
%% a map without id.
-type ah_graph_endpoint() :: {ah_graph_id(), non_neg_integer()} | [ah_graph_id() | non_neg_integer()].
-type ah_graph_link() :: #{id => ah_graph_id(),
                           source := ah_graph_endpoint(), target := ah_graph_endpoint(),
                           points => [ah_graph_point()]}
                       | {ah_graph_endpoint(), ah_graph_endpoint()}.
%% A group frame: a titled box drawn behind the nodes; moving it moves the
%% nodes it fully contains. `bounds' is `{X, Y, W, H}'.
-type ah_graph_group() :: #{id => ah_graph_id(), title => term(),
                            bounds := {number(), number(), number(), number()} | [number()],
                            color => term()}.
-type ah_graph() :: #{nodes => [ah_graph_node()], links => [ah_graph_link()],
                      groups => [ah_graph_group()]}.
%% An entry of the node search menu (right click on the canvas, or a link
%% dropped on empty canvas): the node it adds, with `label' and `category'
%% for the menu.
-type ah_graph_library_item() :: #{type := term(), label => term(), category => term(),
                                   title => term(), width => number(), color => term(),
                                   inputs => [ah_graph_slot()], outputs => [ah_graph_slot()],
                                   widgets => [aihtml_html:html()], body => aihtml_html:html(),
                                   data => term()}.
-type ah_graph_link_mode() :: spline | linear | straight.

%% A node editor: node cards with typed slots, links dragged between
%% them, pan and zoom. Postback fires on change: every edit (move,
%% connect, disconnect, delete, add, resize, rename, collapse, groups,
%% undo/redo), with the whole graph as JSON in Event.value and the edit
%% in Event.data (op, changed, removed). Without an `id' one is generated
%% at render.
-record(ah_node_graph, {?AH_BASE(aihtml_data_graph),
                        graph = #{} :: ah_graph(),
                        name = undefined :: undefined | atom() | iodata(),
                        read_only = false :: boolean(),
                        minimap = false :: boolean(),
                        auto_fit = false :: boolean(),
                        allow_cycles = false :: boolean(),
                        no_toolbar = false :: boolean(),
                        no_grid = false :: boolean(),
                        link_mode = spline :: ah_graph_link_mode(),
                        snap = undefined :: undefined | pos_integer(),
                        height = 400 :: pos_integer() | auto | iodata(),
                        library = [] :: [ah_graph_library_item()],
                        layout = none :: none | auto,
                        label = <<"Node graph">> :: unicode:chardata()}).

-endif.
