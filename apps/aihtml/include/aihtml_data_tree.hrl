%% Element records of aihtml_data_tree (designs/05-records.md). Field
%% names follow the catalog: modifier groups, flags and options are fields
%% of the same name, with the catalog's defaults (aihtml_data_tree_tests
%% checks that they agree).
-ifndef(AIHTML_DATA_TREE_HRL).
-define(AIHTML_DATA_TREE_HRL, true).

-include("aihtml_element.hrl").

%% A tree node: a text that is both value and label, `{Value, Label}',
%% `{Value, Label, Children}', or a map. `lazy' marks a node whose
%% children the tree's `load' action supplies when it is first expanded.
-type ah_dt_tree_item() :: binary() | atom() | integer()
                         | {term(), aihtml_html:html()}
                         | {term(), aihtml_html:html(), [ah_dt_tree_item()]}
                         | #{value => term(), label => aihtml_html:html(),
                             icon => aihtml_html:html(), expanded => boolean(),
                             disabled => boolean(), lazy => boolean(),
                             items => [ah_dt_tree_item()]}.
%% A nav tree entry: a link (`route' or `href'), a collapsible node
%% (`items'), `{Label, Route}' for short, or a group of entries under a
%% small caps heading (`group').
-type ah_dt_nav_item() :: {aihtml_html:html(), iodata()}
                        | #{label := aihtml_html:html(), icon => aihtml_html:html(),
                            route => iodata(), href => iodata(),
                            items => [ah_dt_nav_item()]}
                        | #{group := aihtml_html:html() | undefined,
                            items := [ah_dt_nav_item()]}.
%% An ISO date (<<"2026-09-29">> or "2026-09-29") or a calendar:date().
-type ah_dt_date() :: binary() | string() | calendar:date().
%% Values per day, as a map or a list of pairs.
-type ah_dt_heatmap_data() :: #{ah_dt_date() => number()} | [{ah_dt_date(), number()}].

%% A hierarchical list with expand / collapse, single selection and
%% keyboard navigation; postback fires on change (the selected value).
%% `load' is an action ref that supplies the children of lazy nodes (see
%% aihtml_data_tree:set_children/3). Without an `id' one is generated.
-record(ah_tree, {?AH_BASE(aihtml_data_tree),
                  items = [] :: [ah_dt_tree_item()],
                  value = undefined :: term(),
                  name = undefined :: undefined | atom() | iodata(),
                  disabled = false :: boolean(),
                  toggle_mode = click :: click | dblclick,
                  animation = slide :: slide | none,
                  load = undefined :: undefined | aihtml_action:ref()}).

%% A grouped side navigation of links with collapsible nodes (native
%% <details>); `value' is the active route. Postback fires on change when
%% a link is chosen.
-record(ah_nav_tree, {?AH_BASE(aihtml_data_tree),
                      items = [] :: [ah_dt_nav_item()],
                      value = undefined :: undefined | iodata() | atom(),
                      route_prefix = <<"#/">> :: iodata()}).

%% A read-only view of the differences between two texts, computed on the
%% server (line or word level, unified or side by side); no postback.
-record(ah_diff, {?AH_BASE(aihtml_data_tree),
                  old = <<>> :: unicode:chardata(),
                  new = <<>> :: unicode:chardata(),
                  mode = line :: line | word,
                  view = unified :: unified | split,
                  line_numbers = false :: boolean(),
                  stats = false :: boolean()}).

%% A GitHub-style contribution heatmap: one column per week, one cell per
%% day, coloured by thresholds. Postback fires on 'ah:select' (a click on
%% a day; Event.value is its date).
-record(ah_heatmap_calendar, {?AH_BASE(aihtml_data_tree),
                              data = #{} :: ah_dt_heatmap_data(),
                              months = 12 :: pos_integer(),
                              end_date = undefined :: undefined | ah_dt_date(),
                              thresholds = [0, 1, 3, 6] :: [number()],
                              weekday_labels = undefined :: undefined | [unicode:chardata()],
                              month_labels = undefined :: undefined | [unicode:chardata()],
                              legend = {<<"Less">>, <<"More">>}
                                  :: false | {aihtml_html:html(), aihtml_html:html()},
                              tooltip = <<"{value} · {date}"/utf8>> :: unicode:chardata()}).

-endif.
