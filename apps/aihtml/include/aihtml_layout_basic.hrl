%% Element records of aihtml_layout_basic (designs/05-records.md). Field
%% names follow the catalog: modifier groups, flags and options are fields
%% of the same name, with the catalog's defaults (aihtml_layout_basic_tests
%% checks that they agree). Options default to what the component does
%% when the option is left out.
-ifndef(AIHTML_LAYOUT_BASIC_HRL).
-define(AIHTML_LAYOUT_BASIC_HRL, true).

-include("aihtml_element.hrl").

-type ah_lb_key() :: binary() | atom() | integer() | string().
%% {Key, Label, Panel} | {Key, Label, Panel, #{disabled => true}}
-type ah_lb_tab() :: {ah_lb_key(), aihtml_html:html(), aihtml_html:html()}
                   | {ah_lb_key(), aihtml_html:html(), aihtml_html:html(), map()}.
%% {Id, Title} | {Id, Title, #{dirty => true, icon => Html}}
-type ah_lb_bar_tab() :: {ah_lb_key(), aihtml_html:html()}
                       | {ah_lb_key(), aihtml_html:html(), map()}.
%% Label | {Label, Href} | #{label, href, icon, attrs}
-type ah_lb_crumb() :: aihtml_html:html() | {aihtml_html:html(), binary() | undefined} | map().
%% Title | {Title, Description} | #{title, description, content, status, disabled}
-type ah_lb_step() :: aihtml_html:html() | {aihtml_html:html(), aihtml_html:html()} | map().
%% Integer pixels or a CSS length.
-type ah_lb_length() :: undefined | integer() | binary() | string().
-type ah_lb_name() :: undefined | atom() | iodata().

%% A container with optional media, header and footer; no postback event.
-record(ah_card, {?AH_BASE(aihtml_layout_basic),
                  body = [] :: aihtml_html:html(),
                  hover = false :: boolean(),
                  flush = false :: boolean(),
                  title = undefined :: undefined | aihtml_html:html(),
                  subtitle = undefined :: undefined | aihtml_html:html(),
                  extra = undefined :: undefined | aihtml_html:html(),
                  header = undefined :: undefined | aihtml_html:html(),
                  media = undefined :: undefined | aihtml_html:html(),
                  footer = undefined :: undefined | aihtml_html:html()}).

%% A scrollable container, optionally collapsible; no postback event.
%% Without an `id' one is generated (ah-panel-N), the header and body ids
%% derive from it.
-record(ah_panel, {?AH_BASE(aihtml_layout_basic),
                   body = [] :: aihtml_html:html(),
                   bordered = false :: boolean(),
                   title = undefined :: undefined | aihtml_html:html(),
                   actions = undefined :: undefined | aihtml_html:html(),
                   collapsible = false :: boolean(),
                   collapsed = false :: boolean(),
                   height = undefined :: ah_lb_length(),
                   max_height = undefined :: ah_lb_length(),
                   toggle_label = <<"Toggle">> :: aihtml_html:html()}).

%% A collapsible section, value "true" / "false"; postback fires on change.
%% Without an `id' one is generated (ah-expander-N).
-record(ah_expander, {?AH_BASE(aihtml_layout_basic),
                      body = [] :: aihtml_html:html(),
                      position = top :: top | bottom,
                      square = false :: boolean(),
                      no_gutters = false :: boolean(),
                      disabled = false :: boolean(),
                      header = <<>> :: aihtml_html:html() | #{atom() => aihtml_html:html()},
                      actions = undefined :: undefined | aihtml_html:html(),
                      expanded = true :: boolean(),
                      toggle_mode = click :: click | dblclick | none,
                      animation = undefined :: undefined | slide | fade | none,
                      duration = undefined :: undefined | non_neg_integer(),
                      show_arrow = true :: boolean(),
                      arrow_position = right :: right | left,
                      expand_icon = undefined :: undefined | aihtml_html:html(),
                      collapse_icon = undefined :: undefined | aihtml_html:html(),
                      accordion = undefined :: ah_lb_name(),
                      name = undefined :: ah_lb_name()}).

%% Tabbed panels; `value' is the active key (undefined: the first enabled
%% tab) and postback fires on change. Without an `id' one is generated
%% (ah-tabs-N), the tab and panel ids derive from it.
-record(ah_tabs, {?AH_BASE(aihtml_layout_basic),
                  items = [] :: [ah_lb_tab()],
                  value = undefined :: undefined | ah_lb_key(),
                  position = top :: top | bottom | left | right,
                  disabled = false :: boolean(),
                  animation = undefined :: undefined | fade | none,
                  selection_mode = undefined :: undefined | click | hover,
                  scrollable = false :: boolean(),
                  name = undefined :: ah_lb_name()}).

%% An editor-style strip of closable tabs; `value' is the active id and
%% postback fires on change.
-record(ah_tab_bar, {?AH_BASE(aihtml_layout_basic),
                     items = [] :: [ah_lb_bar_tab()],
                     value = undefined :: undefined | ah_lb_key(),
                     closable = true :: boolean(),
                     close_label = <<"close">> :: aihtml_html:html(),
                     name = undefined :: ah_lb_name()}).

%% An ancestor path; no postback event.
-record(ah_breadcrumbs, {?AH_BASE(aihtml_layout_basic),
                         items = [] :: [ah_lb_crumb()],
                         separator = <<"/">> :: aihtml_html:html() | none,
                         active_last = false :: boolean(),
                         max_items = undefined :: undefined | pos_integer(),
                         label = <<"breadcrumb">> :: aihtml_html:html()}).

%% Page navigation for `total' items, `value' being the current page (from
%% 1); postback fires on change.
-record(ah_pagination, {?AH_BASE(aihtml_layout_basic),
                        total = 0 :: non_neg_integer(),
                        value = 1 :: integer(),
                        simple = false :: boolean(),
                        disabled = false :: boolean(),
                        page_size = 10 :: integer(),
                        page_sizes = [10, 20, 50, 100] :: [pos_integer()],
                        show_size_selector = true :: boolean(),
                        show_jumper = false :: boolean(),
                        show_first_last = false :: boolean(),
                        show_total = false :: boolean(),
                        max_visible = 7 :: pos_integer(),
                        siblings = undefined :: undefined | non_neg_integer(),
                        href = undefined :: undefined | iodata(),
                        labels = #{} :: #{atom() => aihtml_html:html()},
                        name = undefined :: ah_lb_name()}).

%% A step indicator; `value' is the 0-based current step and postback
%% fires on change. `show_nav' undefined: shown when a step has content.
-record(ah_steps, {?AH_BASE(aihtml_layout_basic),
                   items = [] :: [ah_lb_step()],
                   value = 0 :: integer(),
                   orientation = horizontal :: horizontal | vertical,
                   disabled = false :: boolean(),
                   clickable = true :: boolean(),
                   show_nav = undefined :: undefined | boolean(),
                   prev_label = <<"\x{2190} Previous"/utf8>> :: aihtml_html:html(),
                   next_label = <<"Next \x{2192}"/utf8>> :: aihtml_html:html(),
                   name = undefined :: ah_lb_name()}).

%% A shimmering placeholder; no postback event. `variant' undefined
%% renders the text variant without its class.
-record(ah_skeleton, {?AH_BASE(aihtml_layout_basic),
                      variant = undefined :: undefined | text | circle | rect,
                      static = false :: boolean(),
                      done = false :: boolean(),
                      lines = 3 :: integer(),
                      width = undefined :: ah_lb_length(),
                      height = undefined :: ah_lb_length(),
                      radius = undefined :: ah_lb_length(),
                      label = <<"Loading">> :: aihtml_html:html()}).

%% A spinner; no postback event.
-record(ah_loader, {?AH_BASE(aihtml_layout_basic),
                    text_position = bottom :: bottom | top | left | right,
                    hidden = false :: boolean(),
                    inline = false :: boolean(),
                    center = false :: boolean(),
                    disabled = false :: boolean(),
                    text = <<"Loading...">> :: aihtml_html:html(),
                    modal = false :: boolean()}).

%% An empty-state placeholder, `body' being the action area; no postback
%% event.
-record(ah_empty, {?AH_BASE(aihtml_layout_basic),
                   body = [] :: aihtml_html:html(),
                   compact = false :: boolean(),
                   icon = undefined :: undefined | aihtml_html:html(),
                   title = undefined :: undefined | aihtml_html:html(),
                   description = undefined :: undefined | aihtml_html:html()}).

-endif.
