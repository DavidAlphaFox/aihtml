%% Element record of aihtml_masked_input (designs/05-records.md). Field names
%% follow the catalog: modifier groups, flags and options are fields of
%% the same name, with the catalog's defaults (aihtml_masked_input_tests checks
%% that they agree).
-ifndef(AIHTML_MASKED_INPUT_HRL).
-define(AIHTML_MASKED_INPUT_HRL, true).

-include("aihtml_element.hrl").

%% A text field with an input mask such as "(999) 999-9999", enforced in
%% the browser; postback fires on change (blur after an edit). `value'
%% holds the typed characters without the literals (with them when
%% `include_literals' is set).
-record(ah_masked_input, {?AH_BASE(aihtml_masked_input),
                          value = undefined :: undefined | unicode:chardata(),
                          name = undefined :: undefined | atom() | iodata(),
                          disabled = false :: boolean(),
                          readonly = false :: boolean(),
                          square = false :: boolean(),
                          floating_label = false :: boolean(),
                          mask = <<"99999">> :: unicode:chardata(),
                          prompt_char = <<"_">> :: unicode:chardata(),
                          placeholder = <<>> :: unicode:chardata(),
                          include_literals = false :: boolean()}).

-endif.
