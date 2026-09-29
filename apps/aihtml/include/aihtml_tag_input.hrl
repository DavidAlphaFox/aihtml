%% Element record of aihtml_tag_input (designs/05-records.md). Field names
%% follow the catalog: modifier groups, flags and options are fields of
%% the same name, with the catalog's defaults (aihtml_tag_input_tests checks
%% that they agree).
-ifndef(AIHTML_TAG_INPUT_HRL).
-define(AIHTML_TAG_INPUT_HRL, true).

-include("aihtml_element.hrl").

%% Chips plus a text field; `value' is the list of tags and `name' goes to
%% a hidden input (tags joined with commas). Postback fires on change.
-record(ah_tag_input, {?AH_BASE(aihtml_tag_input),
                       value = [] :: [unicode:chardata()],
                       name = undefined :: undefined | atom() | iodata(),
                       disabled = false :: boolean(),
                       placeholder = undefined :: undefined | iodata(),
                       max_tags = undefined :: undefined | pos_integer(),
                       allow_duplicates = false :: boolean(),
                       chip_color = primary :: aihtml_tag_input:chip_color(),
                       chip_variant = soft :: aihtml_tag_input:chip_variant()}).

-endif.
