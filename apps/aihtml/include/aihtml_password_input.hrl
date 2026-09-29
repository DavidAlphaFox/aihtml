%% Element record of aihtml_password_input (designs/05-records.md). Field names
%% follow the catalog: modifier groups, flags and options are fields of
%% the same name, with the catalog's defaults (aihtml_password_input_tests checks
%% that they agree).
-ifndef(AIHTML_PASSWORD_INPUT_HRL).
-define(AIHTML_PASSWORD_INPUT_HRL, true).

-include("aihtml_element.hrl").

%% A password field with a show/hide toggle. `id', `attrs' and the
%% postback go to the native <input>; postback fires on change.
-record(ah_password_input, {?AH_BASE(aihtml_password_input),
                            value = undefined :: undefined | aihtml_lib_input:value(),
                            size = undefined :: undefined | aihtml_lib_input:size(),
                            state = undefined :: undefined | aihtml_lib_input:state(),
                            disabled = false :: boolean(),
                            toggle = true :: boolean(),
                            strength = false :: boolean(),
                            label = undefined :: undefined | aihtml_html:html()}).

-endif.
