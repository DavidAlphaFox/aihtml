%% Element record of aihtml_textarea (designs/05-records.md). Field names
%% follow the catalog: modifier groups, flags and options are fields of
%% the same name, with the catalog's defaults (aihtml_textarea_tests checks
%% that they agree).
-ifndef(AIHTML_TEXTAREA_HRL).
-define(AIHTML_TEXTAREA_HRL, true).

-include("aihtml_element.hrl").

%% Several lines of text, styled like ah_input. `id', `attrs' and the
%% postback go to the native <textarea>; postback fires on change.
-record(ah_textarea, {?AH_BASE(aihtml_textarea),
                      value = undefined :: undefined | aihtml_lib_input:value(),
                      size = undefined :: undefined | aihtml_lib_input:size(),
                      state = undefined :: undefined | aihtml_lib_input:state(),
                      disabled = false :: boolean(),
                      no_rounded = false :: boolean(),
                      label = undefined :: undefined | aihtml_html:html()}).

-endif.
