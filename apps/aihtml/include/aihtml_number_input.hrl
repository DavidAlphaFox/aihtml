%% Element record of aihtml_number_input (designs/05-records.md). Field names
%% follow the catalog: modifier groups, flags and options are fields of
%% the same name, with the catalog's defaults (aihtml_number_input_tests checks
%% that they agree).
-ifndef(AIHTML_NUMBER_INPUT_HRL).
-define(AIHTML_NUMBER_INPUT_HRL, true).

-include("aihtml_element.hrl").

%% A numeric field with spin buttons. `id', `attrs' and the postback go to
%% the native <input>; postback fires on change. min, max and step may
%% also be numeric binaries.
-record(ah_number_input, {?AH_BASE(aihtml_number_input),
                          value = undefined :: undefined | aihtml_number_input:numeric(),
                          size = undefined :: undefined | aihtml_lib_input:size(),
                          state = undefined :: undefined | aihtml_lib_input:state(),
                          disabled = false :: boolean(),
                          readonly = false :: boolean(),
                          min = undefined :: undefined | aihtml_number_input:numeric(),
                          max = undefined :: undefined | aihtml_number_input:numeric(),
                          step = 1 :: aihtml_number_input:numeric(),
                          decimals = undefined :: undefined | non_neg_integer(),
                          spin = true :: boolean(),
                          symbol = undefined :: undefined | aihtml_html:html(),
                          symbol_position = left :: left | right,
                          allow_null = true :: boolean(),
                          label = undefined :: undefined | aihtml_html:html()}).

-endif.
