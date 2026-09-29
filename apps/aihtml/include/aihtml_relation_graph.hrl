%% The element record of aihtml_relation_graph (designs/05-records.md).
%% Field names follow the catalog: modifier groups, flags and options are
%% fields of the same name, with the catalog's defaults
%% (aihtml_relation_graph_tests checks that they agree).
-ifndef(AIHTML_RELATION_GRAPH_HRL).
-define(AIHTML_RELATION_GRAPH_HRL, true).

-include("aihtml_element.hrl").

%% A node-link graph (echarts graph / tree series) in a panel with a
%% toolbar, a detail card and loading / error / empty states. Postback
%% fires on 'ah:select' (Event.value is the selected node id, "" when the
%% selection is cleared).
-record(ah_relation_graph, {?AH_BASE(aihtml_relation_graph),
                            graph = #{nodes => []} :: aihtml_relation_graph:graph(),
                            layout = force :: force | circular | fixed | tree,
                            orient = lr :: lr | tb | rl | bt,
                            node_shape = circle :: circle | square | round_rect,
                            directed = false :: boolean(),
                            loading = false :: boolean(),
                            edge_labels = auto :: auto | boolean(),
                            roam = true :: boolean(),
                            selected = undefined :: undefined | aihtml_lib_chart:text(),
                            focus = undefined :: undefined | aihtml_lib_chart:text(),
                            details = #{} :: #{aihtml_lib_chart:text() => aihtml_html:html()},
                            error = undefined :: undefined | aihtml_html:html(),
                            empty_text = <<"No data">> :: aihtml_html:html(),
                            toolbar = true :: boolean(),
                            width = undefined :: aihtml_lib_chart:size(),
                            height = 420 :: aihtml_lib_chart:size(),
                            renderer = canvas :: canvas | svg}).

-endif.
