%% Element records of aihtml_data_charts (designs/05-records.md). Field
%% names follow the catalog: modifier groups, flags and options are fields
%% of the same name, with the catalog's defaults (aihtml_data_charts_tests
%% checks that they agree).
-ifndef(AIHTML_DATA_CHARTS_HRL).
-define(AIHTML_DATA_CHARTS_HRL, true).

-include("aihtml_element.hrl").

%% An echarts option as echarts documents it: maps with atom or binary
%% keys, binaries, numbers, booleans, null and lists (no tuples, and text
%% as binaries, not Erlang strings). A string "--ah-color-primary" or
%% "var(--ah-color-primary)" anywhere in it is replaced in the browser by
%% the current theme's value, again after every theme change.
-type ah_ch_option() :: #{atom() | binary() => term()}.
%% A name or label: a binary, an atom, a number or an Erlang string.
-type ah_ch_text() :: binary() | atom() | number() | string().
%% A CSS size: pixels, or any CSS length as a binary (<<"50vh">>).
-type ah_ch_size() :: undefined | pos_integer() | binary().
%% A data series of the axis and radar charts: `{Name, Values}' or a map
%% (`color' is a CSS colour or an "--ah-color-*" token).
-type ah_ch_series() :: {ah_ch_text(), [number() | null]}
                      | #{name => ah_ch_text(), data := [number() | null],
                          color => binary()}.
%% A slice of a donut chart.
-type ah_ch_item() :: {ah_ch_text(), number()}
                    | #{name := ah_ch_text(), value := number(), color => binary()}.
%% A radar axis: `{Name, Max}' or a map.
-type ah_ch_indicator() :: {ah_ch_text(), number()}
                         | #{name := ah_ch_text(), max => number(), min => number()}.
-type ah_ch_legend() :: top | bottom | left | right | none.
%% A relation graph node: an id that is also its label, `{Id, Label}' or a
%% map. `category' is a category name or index, `root' draws it larger,
%% `parent' links it to its parent in the tree layout when there are no
%% edges, `x'/`y' place it in the fixed layout.
-type ah_ch_node() :: ah_ch_text()
                    | {ah_ch_text(), ah_ch_text()}
                    | #{id := ah_ch_text(), label => ah_ch_text(),
                        category => ah_ch_text() | non_neg_integer(),
                        root => boolean(), parent => ah_ch_text(),
                        x => number(), y => number(), collapsed => boolean(),
                        item_style => ah_ch_option()}.
%% A relation graph edge: `{Source, Target}', `{Source, Target, Label}' or
%% a map; `kind => dashed' draws a dashed line.
-type ah_ch_edge() :: {ah_ch_text(), ah_ch_text()}
                    | {ah_ch_text(), ah_ch_text(), ah_ch_text()}
                    | #{source := ah_ch_text(), target := ah_ch_text(),
                        label => ah_ch_text(), kind => solid | dashed,
                        line_style => ah_ch_option(), symbol => binary(),
                        symbol_size => number()}.
%% A node category: its name, or a map with a colour.
-type ah_ch_category() :: ah_ch_text() | #{name := ah_ch_text(), color => binary()}.
%% The data of a relation graph; `{Nodes, Edges}' is short for a map
%% without categories.
-type ah_ch_graph() :: #{nodes := [ah_ch_node()], edges => [ah_ch_edge()],
                         categories => [ah_ch_category()]}
                     | {[ah_ch_node()], [ah_ch_edge()]}.

%% A chart that draws any echarts option (loaded on demand with
%% AH.vendor("echarts")), themed from the --ah-* custom properties.
%% Postback fires on 'ah:chart-click' (Event.value is the clicked item's
%% name; Event.data has series, seriesIndex, name, value, index, kind).
-record(ah_chart, {?AH_BASE(aihtml_data_charts),
                   option = #{} :: ah_ch_option(),
                   loading = false :: boolean(),
                   disabled = false :: boolean(),
                   width = undefined :: ah_ch_size(),
                   height = undefined :: ah_ch_size(),
                   renderer = canvas :: canvas | svg}).

%% A line chart with filled areas (a plain line chart with `line'), the
%% option built here from series and categories. Postback fires on
%% 'ah:chart-click', as for chart.
-record(ah_area_chart, {?AH_BASE(aihtml_data_charts),
                        series = [] :: [ah_ch_series()],
                        line = false :: boolean(),
                        straight = false :: boolean(),
                        stack = false :: boolean(),
                        loading = false :: boolean(),
                        disabled = false :: boolean(),
                        categories = [] :: [ah_ch_text()],
                        title = undefined :: undefined | ah_ch_text(),
                        colors = undefined :: undefined | [binary()],
                        y_name = undefined :: undefined | ah_ch_text(),
                        legend = bottom :: ah_ch_legend(),
                        grid = true :: boolean(),
                        tooltip = true :: boolean(),
                        width = undefined :: ah_ch_size(),
                        height = undefined :: ah_ch_size(),
                        renderer = canvas :: canvas | svg}).

%% A bar chart, vertical or horizontal, grouped or stacked. Postback fires
%% on 'ah:chart-click', as for chart.
-record(ah_bar_chart, {?AH_BASE(aihtml_data_charts),
                       series = [] :: [ah_ch_series()],
                       horizontal = false :: boolean(),
                       stack = false :: boolean(),
                       loading = false :: boolean(),
                       disabled = false :: boolean(),
                       categories = [] :: [ah_ch_text()],
                       title = undefined :: undefined | ah_ch_text(),
                       colors = undefined :: undefined | [binary()],
                       y_name = undefined :: undefined | ah_ch_text(),
                       legend = bottom :: ah_ch_legend(),
                       grid = true :: boolean(),
                       tooltip = true :: boolean(),
                       bar_width = undefined :: undefined | number() | binary(),
                       width = undefined :: ah_ch_size(),
                       height = undefined :: ah_ch_size(),
                       renderer = canvas :: canvas | svg}).

%% A donut (or with `pie' a full pie) chart of named values. Postback
%% fires on 'ah:chart-click', as for chart.
-record(ah_donut_chart, {?AH_BASE(aihtml_data_charts),
                         items = [] :: [ah_ch_item()],
                         pie = false :: boolean(),
                         loading = false :: boolean(),
                         disabled = false :: boolean(),
                         title = undefined :: undefined | ah_ch_text(),
                         colors = undefined :: undefined | [binary()],
                         legend = right :: ah_ch_legend(),
                         labels = true :: boolean(),
                         tooltip = true :: boolean(),
                         radius = {<<"50%">>, <<"70%">>} :: {number() | binary(), number() | binary()},
                         center = undefined :: undefined | {number() | binary(), number() | binary()},
                         width = undefined :: ah_ch_size(),
                         height = undefined :: ah_ch_size(),
                         renderer = canvas :: canvas | svg}).

%% A radar chart: one polygon per series over named axes (indicators).
%% Postback fires on 'ah:chart-click', as for chart.
-record(ah_radar_chart, {?AH_BASE(aihtml_data_charts),
                         series = [] :: [ah_ch_series()],
                         shape = polygon :: polygon | circle,
                         loading = false :: boolean(),
                         disabled = false :: boolean(),
                         indicators = [] :: [ah_ch_indicator()],
                         title = undefined :: undefined | ah_ch_text(),
                         colors = undefined :: undefined | [binary()],
                         legend = bottom :: ah_ch_legend(),
                         tooltip = true :: boolean(),
                         split_number = 4 :: pos_integer(),
                         radius = <<"62%">> :: number() | binary(),
                         area_opacity = 0.18 :: number(),
                         width = undefined :: ah_ch_size(),
                         height = undefined :: ah_ch_size(),
                         renderer = canvas :: canvas | svg}).

%% A node-link graph (echarts graph / tree series) in a panel with a
%% toolbar, a detail card and loading / error / empty states. Postback
%% fires on 'ah:select' (Event.value is the selected node id, "" when the
%% selection is cleared).
-record(ah_relation_graph, {?AH_BASE(aihtml_data_charts),
                            graph = #{nodes => []} :: ah_ch_graph(),
                            layout = force :: force | circular | fixed | tree,
                            orient = lr :: lr | tb | rl | bt,
                            node_shape = circle :: circle | square | round_rect,
                            directed = false :: boolean(),
                            loading = false :: boolean(),
                            edge_labels = auto :: auto | boolean(),
                            roam = true :: boolean(),
                            selected = undefined :: undefined | ah_ch_text(),
                            focus = undefined :: undefined | ah_ch_text(),
                            details = #{} :: #{ah_ch_text() => aihtml_html:html()},
                            error = undefined :: undefined | aihtml_html:html(),
                            empty_text = <<"No data">> :: aihtml_html:html(),
                            toolbar = true :: boolean(),
                            width = undefined :: ah_ch_size(),
                            height = 420 :: ah_ch_size(),
                            renderer = canvas :: canvas | svg}).

-endif.
