%% Element records of aihtml_layout_bars (designs/05-records.md): the
%% activity bar, the navigation bar (sigil's accordion-style
%% navigationbar) and the command palette. Field names follow the
%% catalog: modifier groups, flags and options are fields of the same
%% name, with the catalog's defaults (aihtml_layout_bars_tests checks
%% that they agree).
-ifndef(AIHTML_LAYOUT_BARS_HRL).
-define(AIHTML_LAYOUT_BARS_HRL, true).

-include("aihtml_element.hrl").

%% {Value, Icon, Label} | {Value, Icon, Label, ItemAttrs} | divider.
%% Icon is HTML (a glyph or an SVG); Label is the tooltip and aria-label.
-type ah_bars_activity_item() :: {term(), aihtml_html:html(), iodata()}
                               | {term(), aihtml_html:html(), iodata(), aihtml_html:attrs()}
                               | divider.
%% A header is HTML or #{title, subheader, extra} (a three-column header).
-type ah_bars_nav_header() :: aihtml_html:html()
                            | #{title := aihtml_html:html(),
                                subheader => aihtml_html:html(),
                                extra => aihtml_html:html()}.
%% {Header, Content} | {Header, Content, Opts} (Opts: disabled, actions)
%% | #{header, content, actions, disabled}.
-type ah_bars_nav_item() :: {ah_bars_nav_header(), aihtml_html:html()}
                          | {ah_bars_nav_header(), aihtml_html:html(), aihtml_html:attrs()}
                          | #{header := ah_bars_nav_header(),
                              content => aihtml_html:html(),
                              actions => aihtml_html:html(),
                              disabled => boolean()}.
%% Expanded item indexes (0-based): N, [N], "0,2" or undefined (none).
-type ah_bars_nav_value() :: undefined | non_neg_integer() | [non_neg_integer()] | binary().
%% Label | {Value, Label} | #{value, label, description, icon, shortcut,
%% href, disabled}.
-type ah_bars_cmd_item() :: iodata() | atom() | integer()
                          | {term(), aihtml_html:html()}
                          | #{value := term(),
                              label => aihtml_html:html(),
                              description => aihtml_html:html(),
                              icon => aihtml_html:html(),
                              shortcut => aihtml_html:html(),
                              href => iodata(),
                              disabled => boolean()}.
%% A command item, or a group of them under a heading.
-type ah_bars_cmd_entry() :: ah_bars_cmd_item()
                           | #{heading => aihtml_html:html(), items := [ah_bars_cmd_item()]}.
-type ah_bars_action_ref() :: {module(), atom(), term()}.

%% A VS Code-style vertical icon rail; `value' is the active item and
%% postback fires on change.
-record(ah_activity_bar, {?AH_BASE(aihtml_layout_bars),
                          items = [] :: [ah_bars_activity_item()],
                          value = undefined :: term(),
                          name = undefined :: undefined | atom() | iodata(),
                          placement = left :: left | right}).

%% Collapsible sections (sigil's navigationbar, an accordion); `value'
%% holds the expanded indexes and postback fires on change.
-record(ah_navigationbar, {?AH_BASE(aihtml_layout_bars),
                           items = [] :: [ah_bars_nav_item()],
                           value = undefined :: ah_bars_nav_value(),
                           name = undefined :: undefined | atom() | iodata(),
                           square = false :: boolean(),
                           disable_gutters = false :: boolean(),
                           no_arrow = false :: boolean(),
                           expand_mode = single_fit_height :: single | single_fit_height
                                                            | multiple | toggle | none,
                           animation = slide :: slide | fade | none,
                           toggle_mode = click :: click | dblclick | none,
                           arrow_position = right :: left | right,
                           expand_icon = undefined :: aihtml_html:html(),
                           collapse_icon = undefined :: aihtml_html:html(),
                           expand_duration = 250 :: non_neg_integer(),
                           collapse_duration = 250 :: non_neg_integer(),
                           width = undefined :: undefined | integer() | iodata(),
                           height = undefined :: undefined | integer() | iodata(),
                           disabled = false :: boolean()}).

%% A command palette: a search field over grouped commands with keyboard
%% navigation. Postback fires on 'ah:select' (Event.value is the chosen
%% command's value).
-record(ah_command, {?AH_BASE(aihtml_layout_bars),
                     items = [] :: [ah_bars_cmd_entry()],
                     palette = false :: boolean(),
                     auto_focus = false :: boolean(),
                     placeholder = <<"Type a command or search…"/utf8>> :: iodata(),
                     empty_text = <<"No results found.">> :: iodata(),
                     query = <<>> :: iodata(),
                     search = undefined :: undefined | ah_bars_action_ref(),
                     hotkey = undefined :: undefined | iodata(),
                     close_on_select = true :: boolean()}).

-endif.
