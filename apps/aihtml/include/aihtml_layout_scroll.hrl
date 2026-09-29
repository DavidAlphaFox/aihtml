%% Element records of aihtml_layout_scroll (designs/05-records.md). Field
%% names follow the catalog: modifier groups, flags and options are fields
%% of the same name, with the catalog's defaults (aihtml_layout_scroll_tests
%% checks that they agree). Options default to what the component does
%% when the option is left out.
-ifndef(AIHTML_LAYOUT_SCROLL_HRL).
-define(AIHTML_LAYOUT_SCROLL_HRL, true).

-include("aihtml_element.hrl").

%% Integer pixels or a CSS length.
-type ah_scr_length() :: undefined | integer() | binary() | string().
-type ah_scr_name() :: undefined | atom() | iodata().
%% An action reference {Module, Action, Args}.
-type ah_scr_action() :: undefined | {module(), atom(), term()}.

%% A horizontal pager (carousel): one page of `body' at a time, dragged,
%% swiped or picked with the dots. The value is the 0-based page index;
%% postback fires on change. Without an `id' one is generated.
-record(ah_scrollview, {?AH_BASE(aihtml_layout_scroll),
                        body = [] :: [aihtml_html:html()],
                        disabled = false :: boolean(),
                        name = undefined :: ah_scr_name(),
                        current_page = 0 :: non_neg_integer(),
                        width = undefined :: ah_scr_length(),
                        height = undefined :: ah_scr_length(),
                        show_buttons = true :: boolean(),
                        slide_show = false :: boolean(),
                        slide_duration = 3000 :: pos_integer(),
                        animation_duration = 300 :: non_neg_integer(),
                        move_threshold = 0.5 :: number(),
                        bounce = true :: boolean(),
                        label = <<"Carousel">> :: unicode:chardata()}).

%% A custom scrollbar. With an empty `body' it is a standalone bar whose
%% value runs from `min' to `max' (postback fires on change); with content
%% it is a scroll area whose content scrolls natively under custom
%% vertical and horizontal bars (no value, `orientation' is ignored).
%% Without an `id' one is generated.
-record(ah_scrollbar, {?AH_BASE(aihtml_layout_scroll),
                       body = [] :: aihtml_html:html(),
                       orientation = horizontal :: horizontal | vertical,
                       disabled = false :: boolean(),
                       name = undefined :: ah_scr_name(),
                       value = 0 :: number(),
                       min = 0 :: number(),
                       max = 1000 :: number(),
                       step = 10 :: number(),
                       large_step = 50 :: number(),
                       thumb_min_size = 10 :: non_neg_integer(),
                       show_buttons = true :: boolean(),
                       width = undefined :: ah_scr_length(),
                       height = undefined :: ah_scr_length(),
                       label = undefined :: undefined | unicode:chardata()}).

%% A panel that shows its content in place while its parent is wider than
%% `breakpoint' and folds into a toggle button with a floating overlay
%% below it. Fires ah:collapse / ah:expand / ah:open / ah:close; no
%% postback event. `load' is an action ref fired (event ah:load on the
%% content) the first time the content is shown. Without an `id' one is
%% generated; the content's id is Id-content.
-record(ah_responsive_panel, {?AH_BASE(aihtml_layout_scroll),
                              body = [] :: aihtml_html:html(),
                              disabled = false :: boolean(),
                              breakpoint = 1000 :: non_neg_integer(),
                              collapse_width = 200 :: ah_scr_length(),
                              height = undefined :: ah_scr_length(),
                              animation = fade :: fade | slide | none,
                              show_duration = 200 :: non_neg_integer(),
                              hide_duration = 200 :: non_neg_integer(),
                              auto_close = true :: boolean(),
                              toggle_button = undefined :: undefined | iodata(),
                              toggle_size = 30 :: pos_integer(),
                              toggle_content = <<"☰"/utf8>> :: aihtml_html:html(),
                              toggle_label = <<"Toggle panel">> :: unicode:chardata(),
                              load = undefined :: ah_scr_action()}).

-endif.
