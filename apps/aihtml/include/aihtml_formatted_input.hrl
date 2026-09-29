%% Element record of aihtml_formatted_input (designs/05-records.md). Field names
%% follow the catalog: modifier groups, flags and options are fields of
%% the same name, with the catalog's defaults (aihtml_formatted_input_tests checks
%% that they agree).
-ifndef(AIHTML_FORMATTED_INPUT_HRL).
-define(AIHTML_FORMATTED_INPUT_HRL, true).

-include("aihtml_element.hrl").

%% An integer field in binary, octal, decimal or hexadecimal, with spin
%% buttons and a radix menu; postback fires on change. `value' is the
%% decimal value (any size).
-record(ah_formatted_input, {?AH_BASE(aihtml_formatted_input),
                             value = 0 :: aihtml_formatted_input:integer_value(),
                             name = undefined :: undefined | atom() | iodata(),
                             disabled = false :: boolean(),
                             radix = 10 :: aihtml_formatted_input:radix(),
                             min = undefined :: undefined | aihtml_formatted_input:integer_value(),
                             max = undefined :: undefined | aihtml_formatted_input:integer_value(),
                             upper_case = false :: boolean(),
                             spin_buttons = true :: boolean(),
                             spin_step = 1 :: aihtml_formatted_input:integer_value(),
                             drop_down = true :: boolean(),
                             drop_down_width = undefined :: undefined | pos_integer(),
                             notation = default :: default | exponential,
                             placeholder = <<>> :: unicode:chardata()}).

-endif.
