%% Element records of aihtml_layout_nav (designs/05-records.md). Field
%% names follow the catalog: modifier groups, flags and options are fields
%% of the same name, with the catalog's defaults (aihtml_layout_nav_tests
%% checks that they agree).
-ifndef(AIHTML_LAYOUT_NAV_HRL).
-define(AIHTML_LAYOUT_NAV_HRL, true).

-include("aihtml_element.hrl").

-type ah_nav_key() :: atom() | binary() | integer().
%% binary: an image URL; other html: inline (SVG).
-type ah_nav_icon() :: binary() | aihtml_html:html().
%% A menu / navbar / sidenav / listmenu item (see aihtml_layout_nav).
-type ah_nav_item() :: #{key => ah_nav_key(), label => aihtml_html:html(),
                         icon => ah_nav_icon(), href => binary(), target => binary(),
                         disabled => boolean(),
                         children => [ah_nav_item()],
                         columns => [#{header => aihtml_html:html(),
                                       children => [ah_nav_item()]}],
                         open => left | up | [left | up],
                         expanded => boolean(),
                         divider => true}
                     | divider | {ah_nav_key(), aihtml_html:html()}.
%% A sidenav group.
-type ah_nav_group() :: #{label => aihtml_html:html(), items := [ah_nav_item()]}.
%% A toolbar tool: a button (map), a separator, or any other html (custom).
-type ah_nav_tool() :: #{key => ah_nav_key(), label => aihtml_html:html(),
                         icon => ah_nav_icon(), title => binary(), disabled => boolean(),
                         toggle => boolean(), pressed => boolean(),
                         minimizable => boolean()}
                     | separator | {custom, aihtml_html:html()} | aihtml_html:html().
%% A splitter pane: its content, or a map with its initial size
%% (<<"30%">> or pixels) and minimum size in pixels.
-type ah_nav_pane() :: #{content => aihtml_html:html(), size => binary() | number(),
                         min => non_neg_integer()}
                     | aihtml_html:html().
-type ah_nav_segment() :: #{content := aihtml_html:html(), align => left | right}
                        | #{count := integer(), label => aihtml_html:html(),
                            details => [{aihtml_html:html(), aihtml_html:html()}],
                            align => left | right}
                        | aihtml_html:html().
-type ah_nav_name() :: undefined | atom() | iodata().
%% Pixels, or a CSS length as a binary.
-type ah_nav_px() :: number() | binary().

%% Menu bar or context menu; `value' is the active item. Postback fires
%% on change (an item without href was chosen).
-record(ah_menu, {?AH_BASE(aihtml_layout_nav),
                  items = [] :: [ah_nav_item()],
                  value = undefined :: undefined | ah_nav_key(),
                  mode = horizontal :: horizontal | vertical | popup,
                  show_arrows = false :: boolean(),
                  disabled = false :: boolean(),
                  title = undefined :: undefined | binary(),
                  name = undefined :: ah_nav_name(),
                  click_to_open = false :: boolean(),
                  keyboard = true :: boolean(),
                  minimize_width = undefined :: undefined | ah_nav_px(),
                  popup_target = undefined :: undefined | iodata()}).

%% Bar of selectable items; `value' is the selected item. Postback fires
%% on change.
-record(ah_navbar, {?AH_BASE(aihtml_layout_nav),
                    items = [] :: [ah_nav_item()],
                    value = undefined :: undefined | ah_nav_key(),
                    orientation = horizontal :: horizontal | vertical,
                    minimized = false :: boolean(),
                    disabled = false :: boolean(),
                    brand = undefined :: aihtml_html:html(),
                    extra = undefined :: aihtml_html:html(),
                    title = <<>> :: aihtml_html:html(),
                    minimized_height = 36 :: ah_nav_px(),
                    minimize_width = undefined :: undefined | ah_nav_px(),
                    columns = [] :: [ah_nav_px()],
                    selection = true :: boolean(),
                    name = undefined :: ah_nav_name()}).

%% Application sidebar. `groups' is [ah_nav_group()] or a plain item
%% list; `value' is the active item. Postback fires on change.
-record(ah_sidenav, {?AH_BASE(aihtml_layout_nav),
                     groups = [] :: [ah_nav_group()] | [ah_nav_item()],
                     value = undefined :: undefined | ah_nav_key(),
                     collapsed = false :: boolean(),
                     brand = undefined :: undefined
                                        | #{name => aihtml_html:html(), logo => ah_nav_icon(),
                                            href => iodata()}
                                        | aihtml_html:html(),
                     footer = undefined :: aihtml_html:html(),
                     collapsible = false :: boolean(),
                     route_prefix = undefined :: undefined | iodata(),
                     name = undefined :: ah_nav_name()}).

%% Row of tools with an overflow popup. Postback fires on change (a tool
%% with a key was clicked; data-ah-value is its key).
-record(ah_toolbar, {?AH_BASE(aihtml_layout_nav),
                     tools = [] :: [ah_nav_tool()],
                     disabled = false :: boolean(),
                     popup_width = undefined :: undefined | ah_nav_px()}).

%% Two panes and a split bar; exactly one or two panes. Postback fires on
%% change (end of a resize; the value is "First,Second" in percent).
-record(ah_splitter, {?AH_BASE(aihtml_layout_nav),
                      panes = [] :: [ah_nav_pane()],
                      orientation = vertical :: vertical | horizontal,
                      disabled = false :: boolean(),
                      splitbar_size = 5 :: ah_nav_px(),
                      resizable = true :: boolean(),
                      step = undefined :: undefined | ah_nav_px(),
                      name = undefined :: ah_nav_name()}).

%% Drill-down list menu; `value' is the selected leaf. Postback fires on
%% change. `filter_placeholder' undefined means "Filter..." (aria-label
%% "Filter").
-record(ah_listmenu, {?AH_BASE(aihtml_layout_nav),
                      items = [] :: [ah_nav_item()],
                      value = undefined :: undefined | ah_nav_key(),
                      disabled = false :: boolean(),
                      header = true :: boolean(),
                      back_button = true :: boolean(),
                      filter = false :: boolean(),
                      arrows = true :: boolean(),
                      back_label = <<"Back">> :: aihtml_html:html(),
                      filter_placeholder = undefined :: undefined | binary(),
                      animation = undefined :: undefined | slide | fade | none,
                      name = undefined :: ah_nav_name()}).

%% Status bar; no postback event. `content' adds the word count segment,
%% `dirty' the saved / unsaved dot.
-record(ah_status_bar, {?AH_BASE(aihtml_layout_nav),
                        segments = [] :: [ah_nav_segment()],
                        content = undefined :: undefined | unicode:chardata(),
                        dirty = undefined :: undefined | boolean(),
                        labels = #{} :: #{atom() => aihtml_html:html()}}).

-endif.
