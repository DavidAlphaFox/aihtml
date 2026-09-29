%% Element record of aihtml_scrollbar (designs/05-records.md). Field
%% names follow the catalog: modifier groups, flags and options are fields
%% of the same name, with the catalog's defaults (aihtml_scrollbar_tests
%% checks that they agree). Options default to what the component does
%% when the option is left out.
-ifndef(AIHTML_SCROLLBAR_HRL).
-define(AIHTML_SCROLLBAR_HRL, true).

-include("aihtml_element.hrl").

%% A custom scrollbar. With an empty `body' it is a standalone bar whose
%% value runs from `min' to `max' (postback fires on change); with content
%% it is a scroll area whose content scrolls natively under custom
%% vertical and horizontal bars (no value, `orientation' is ignored).
%% Without an `id' one is generated.
-record(ah_scrollbar, {?AH_BASE(aihtml_scrollbar),
                       body = [] :: aihtml_html:html(),
                       orientation = horizontal :: horizontal | vertical,
                       disabled = false :: boolean(),
                       name = undefined :: aihtml_lib_scroll:name(),
                       value = 0 :: number(),
                       min = 0 :: number(),
                       max = 1000 :: number(),
                       step = 10 :: number(),
                       large_step = 50 :: number(),
                       thumb_min_size = 10 :: non_neg_integer(),
                       show_buttons = true :: boolean(),
                       width = undefined :: aihtml_lib_scroll:css_length(),
                       height = undefined :: aihtml_lib_scroll:css_length(),
                       label = undefined :: undefined | unicode:chardata()}).

-endif.
