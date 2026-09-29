%% Element record of aihtml_input_otp (designs/05-records.md). Field names
%% follow the catalog: modifier groups, flags and options are fields of
%% the same name, with the catalog's defaults (aihtml_input_otp_tests checks
%% that they agree).
-ifndef(AIHTML_INPUT_OTP_HRL).
-define(AIHTML_INPUT_OTP_HRL, true).

-include("aihtml_element.hrl").

%% One box per character; `name' goes to a hidden input. Postback fires on
%% change.
-record(ah_input_otp, {?AH_BASE(aihtml_input_otp),
                       length = 6 :: pos_integer(),
                       value = undefined :: undefined | iodata(),
                       name = undefined :: undefined | atom() | iodata(),
                       disabled = false :: boolean(),
                       pattern = digit :: digit | alphanumeric,
                       separator_at = undefined :: undefined | pos_integer()}).

-endif.
