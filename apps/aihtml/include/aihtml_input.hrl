%% Element record of aihtml_input (designs/05-records.md). Field names
%% follow the catalog: modifier groups, flags and options are fields of
%% the same name, with the catalog's defaults (aihtml_input_tests checks
%% that they agree).
-ifndef(AIHTML_INPUT_HRL).
-define(AIHTML_INPUT_HRL, true).

-include("aihtml_element.hrl").

%% One line of text. `id', `attrs' and the postback go to the native
%% <input>; postback fires on change.
-record(ah_input, {?AH_BASE(aihtml_input),
                   value = undefined :: undefined | aihtml_lib_input:value(),
                   size = undefined :: undefined | aihtml_lib_input:size(),
                   state = undefined :: undefined | aihtml_lib_input:state(),
                   disabled = false :: boolean(),
                   no_rounded = false :: boolean(),
                   clearable = false :: boolean(),
                   prefix = undefined :: undefined | aihtml_html:html(),
                   suffix = undefined :: undefined | aihtml_html:html(),
                   label = undefined :: undefined | aihtml_html:html()}).

-endif.
