%% Element record of aihtml_tag_cloud (designs/05-records.md). Field
%% names follow the catalog: modifier groups, flags and options are
%% fields of the same name, with the catalog's defaults (aihtml_tag_cloud_tests
%% checks that they agree). Option fields default to what the component
%% used when the option was left out.
-ifndef(AIHTML_TAG_CLOUD_HRL).
-define(AIHTML_TAG_CLOUD_HRL, true).

-include("aihtml_element.hrl").

%% Tags sized by weight. Postback fires on 'ah:tag-click'.
-record(ah_tag_cloud, {?AH_BASE(aihtml_tag_cloud),
                       items = [] :: [aihtml_tag_cloud:tag()],
                       disabled = false :: boolean(),
                       min_font_size = 10 :: number(),
                       max_font_size = 24 :: number(),
                       font_size_unit = px :: px | em | 'rem' | pt | '%' | binary(),
                       url_base = <<>> :: iodata(),
                       display_value = false :: boolean(),
                       sort_by = none :: none | label | value,
                       sort_order = ascending :: ascending | descending,
                       text_case = none :: none | all_lower | all_upper | first_upper
                                         | title_case,
                       text_color = undefined :: undefined | aihtml_lib_color:css_color(),
                       min_color = undefined :: undefined | iodata(),
                       max_color = undefined :: undefined | iodata(),
                       min_value = 0 :: number(),
                       max_value = 0 :: number(),
                       display_limit = undefined :: undefined | pos_integer(),
                       take_top_weighted = false :: boolean()}).

-endif.
