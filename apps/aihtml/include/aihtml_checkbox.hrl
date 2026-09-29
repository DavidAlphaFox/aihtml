%% Element record of aihtml_checkbox (designs/05-records.md). Field names
%% follow the catalog: modifier groups, flags and options are fields of
%% the same name, with the catalog's defaults (aihtml_checkbox_tests checks
%% that they agree).
-ifndef(AIHTML_CHECKBOX_HRL).
-define(AIHTML_CHECKBOX_HRL, true).

-include("aihtml_element.hrl").

%% A checkbox around a native input. `value' is the input's form value,
%% `checked' its state; `id', `attrs' and the postback go to the input.
%% Postback fires on change.
-record(ah_checkbox, {?AH_BASE(aihtml_checkbox),
                      body = [] :: aihtml_html:html(),
                      value = undefined :: undefined | aihtml_lib_choice:value(),
                      checked = false :: boolean(),
                      disabled = false :: boolean(),
                      size = undefined :: undefined | aihtml_lib_choice:size(),
                      indeterminate = false :: boolean(),
                      three_states = false :: boolean(),
                      locked = false :: boolean(),
                      box_size = undefined :: undefined | pos_integer()}).

-endif.
