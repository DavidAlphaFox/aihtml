%% Element record of aihtml_responsive_panel (designs/05-records.md).
%% Field names follow the catalog: modifier groups, flags and options are
%% fields of the same name, with the catalog's defaults
%% (aihtml_responsive_panel_tests checks that they agree). Options default
%% to what the component does when the option is left out.
-ifndef(AIHTML_RESPONSIVE_PANEL_HRL).
-define(AIHTML_RESPONSIVE_PANEL_HRL, true).

-include("aihtml_element.hrl").

%% A panel that shows its content in place while its parent is wider than
%% `breakpoint' and folds into a toggle button with a floating overlay
%% below it. Fires ah:collapse / ah:expand / ah:open / ah:close; no
%% postback event. `load' is an action ref fired (event ah:load on the
%% content) the first time the content is shown. Without an `id' one is
%% generated; the content's id is Id-content.
-record(ah_responsive_panel, {?AH_BASE(aihtml_responsive_panel),
                              body = [] :: aihtml_html:html(),
                              disabled = false :: boolean(),
                              breakpoint = 1000 :: non_neg_integer(),
                              collapse_width = 200 :: aihtml_lib_scroll:css_length(),
                              height = undefined :: aihtml_lib_scroll:css_length(),
                              animation = fade :: fade | slide | none,
                              show_duration = 200 :: non_neg_integer(),
                              hide_duration = 200 :: non_neg_integer(),
                              auto_close = true :: boolean(),
                              toggle_button = undefined :: undefined | iodata(),
                              toggle_size = 30 :: pos_integer(),
                              toggle_content = <<"☰"/utf8>> :: aihtml_html:html(),
                              toggle_label = <<"Toggle panel">> :: unicode:chardata(),
                              load = undefined :: aihtml_lib_scroll:action()}).

-endif.
