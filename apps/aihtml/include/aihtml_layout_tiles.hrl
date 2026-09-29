%% Element records of aihtml_layout_tiles (designs/05-records.md): the
%% Office-style ribbon and the tile layout (sigil's IDE-style layout of
%% resizable, tabbed panes). Field names follow the catalog: modifier
%% groups, flags and options are fields of the same name, with the
%% catalog's defaults (aihtml_layout_tiles_tests checks that they agree).
-ifndef(AIHTML_LAYOUT_TILES_HRL).
-define(AIHTML_LAYOUT_TILES_HRL, true).

-include("aihtml_element.hrl").

%%% Ribbon -------------------------------------------------------------

%% A dropdown menu entry: {Key, Label} | #{key, label, icon, disabled} | divider.
-type ah_tiles_menu_item() :: {term(), aihtml_html:html()}
                            | #{key := term(), label := aihtml_html:html(),
                                icon => aihtml_html:html(), disabled => boolean()}
                            | divider.
%% A command in a ribbon group:
%%   {Key, Icon, Label}                        a small button
%%   #{key, label, icon, size => small | large, title, disabled,
%%     toggle, pressed, items => [ah_tiles_menu_item()]}
%%                                             (items: a dropdown; toggle:
%%                                             a pressed / released button)
%%   {stack, [Command]}                        small buttons stacked in a column
%%   separator                                 a vertical rule
%%   {html, Html}                              any markup (a select, ...)
-type ah_tiles_ribbon_cmd() :: {term(), aihtml_html:html(), aihtml_html:html()}
                             | #{key := term(), label := aihtml_html:html(),
                                 icon => aihtml_html:html(), size => small | large,
                                 title => iodata(), disabled => boolean(),
                                 toggle => boolean(), pressed => boolean(),
                                 items => [ah_tiles_menu_item()]}
                             | {stack, [ah_tiles_ribbon_cmd()]}
                             | separator
                             | {html, aihtml_html:html()}.
%% A labelled group of commands: {Label, [Command]} | #{label, items}.
-type ah_tiles_ribbon_group() :: {aihtml_html:html(), [ah_tiles_ribbon_cmd()]}
                               | #{label := aihtml_html:html(),
                                   items := [ah_tiles_ribbon_cmd()]}.
%% A tab's panel: any HTML, or {groups, [Group]} for Office-style groups.
-type ah_tiles_ribbon_content() :: aihtml_html:html() | {groups, [ah_tiles_ribbon_group()]}.
%% {Key, Label, Content} | {Key, Label, Content, Opts} (Opts: icon, disabled)
%% | #{key, label, icon, disabled, content, groups}.
-type ah_tiles_ribbon_tab() :: {term(), aihtml_html:html(), ah_tiles_ribbon_content()}
                             | {term(), aihtml_html:html(), ah_tiles_ribbon_content(),
                                aihtml_html:attrs()}
                             | #{key := term(), label := aihtml_html:html(),
                                 icon => aihtml_html:html(), disabled => boolean(),
                                 content => aihtml_html:html(),
                                 groups => [ah_tiles_ribbon_group()]}.

%% An Office-style ribbon: tabs over panels of command groups. `value' is
%% the active tab's key (a user switch fires change); postback fires on
%% 'ah:command' (a command was clicked: Event.data.command is its key,
%% Event.data.pressed the new state of a toggle).
-record(ah_ribbon, {?AH_BASE(aihtml_layout_tiles),
                    items = [] :: [ah_tiles_ribbon_tab()],
                    value = undefined :: term(),
                    name = undefined :: undefined | atom() | iodata(),
                    position = top :: top | bottom | left | right,
                    mode = default :: default | collapsed | popup,
                    color = undefined :: undefined | primary | success | warning | danger,
                    animation = undefined :: undefined | slide | fade,
                    collapsible = false :: boolean(),
                    selection_mode = click :: click | hover,
                    width = undefined :: undefined | integer() | iodata(),
                    height = undefined :: undefined | integer() | iodata(),
                    disabled = false :: boolean()}).

%%% Tile layout --------------------------------------------------------

-type ah_tiles_size() :: undefined | integer() | iodata().
%% A tab of a tab group: {Id, Label, Content} | #{id, label, content,
%% close (default true), drag (default true)}. Label is text.
-type ah_tiles_tab() :: {term(), iodata(), aihtml_html:html()}
                      | #{id := term(), label := iodata(), content => aihtml_html:html(),
                          close => boolean(), drag => boolean()}.
%% A node of the layout tree:
%%   {columns, [Node]} | {rows, [Node]}     panes side by side / stacked,
%%                                          with splitbars between them
%%   #{columns | rows := [Node], id, size, min, resize}
%%   {tabs, [Tab]}                          a tab group
%%   #{tabs := [Tab], id, size, min, position, active}
%%   #{id := Id, content := Html, label, size, min}  a plain tile
%% size is a grid track (px as an integer, or <<"25%">>, <<"2fr">>, ...),
%% min the smallest size in px a splitbar may leave.
-type ah_tiles_node() :: {columns | rows, [ah_tiles_node()]}
                       | {tabs, [ah_tiles_tab()]}
                       | #{columns => [ah_tiles_node()], rows => [ah_tiles_node()],
                           tabs => [ah_tiles_tab()], id => term(), content => aihtml_html:html(),
                           label => iodata(), size => ah_tiles_size(), min => non_neg_integer(),
                           resize => boolean(), position => top | bottom | left | right,
                           active => term()}.

%% A layout of resizable panes and tab groups whose tabs the user drags
%% between groups and to the edges of panes (sigil's tile layout). The
%% arrangement is view state: `value' is a saved arrangement (the JSON of
%% data-ah-value) to render instead of the layout's own; each resize,
%% move, close or tab switch updates data-ah-value and fires change, and
%% postback fires on change.
-record(ah_tile_layout, {?AH_BASE(aihtml_layout_tiles),
                         layout = {columns, []} :: ah_tiles_node(),
                         value = undefined :: undefined | iodata() | map(),
                         name = undefined :: undefined | atom() | iodata(),
                         splitbar_size = 4 :: pos_integer(),
                         height = undefined :: undefined | integer() | iodata(),
                         disabled = false :: boolean()}).

-endif.
