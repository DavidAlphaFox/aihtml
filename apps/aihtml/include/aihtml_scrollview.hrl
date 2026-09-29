%% Element record of aihtml_scrollview (designs/05-records.md). Field
%% names follow the catalog: modifier groups, flags and options are fields
%% of the same name, with the catalog's defaults (aihtml_scrollview_tests
%% checks that they agree). Options default to what the component does
%% when the option is left out.
-ifndef(AIHTML_SCROLLVIEW_HRL).
-define(AIHTML_SCROLLVIEW_HRL, true).

-include("aihtml_element.hrl").

%% A horizontal pager (carousel): one page of `body' at a time, dragged,
%% swiped or picked with the dots. The value is the 0-based page index;
%% postback fires on change. Without an `id' one is generated.
-record(ah_scrollview, {?AH_BASE(aihtml_scrollview),
                        body = [] :: [aihtml_html:html()],
                        disabled = false :: boolean(),
                        name = undefined :: aihtml_lib_scroll:name(),
                        current_page = 0 :: non_neg_integer(),
                        width = undefined :: aihtml_lib_scroll:css_length(),
                        height = undefined :: aihtml_lib_scroll:css_length(),
                        show_buttons = true :: boolean(),
                        slide_show = false :: boolean(),
                        slide_duration = 3000 :: pos_integer(),
                        animation_duration = 300 :: non_neg_integer(),
                        move_threshold = 0.5 :: number(),
                        bounce = true :: boolean(),
                        label = <<"Carousel">> :: unicode:chardata()}).

-endif.
