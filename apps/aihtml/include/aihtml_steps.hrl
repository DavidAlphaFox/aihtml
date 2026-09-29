%% Element record of aihtml_steps (designs/05-records.md). Field names
%% follow the catalog: modifier groups, flags and options are fields of
%% the same name, with the catalog's defaults (aihtml_steps_tests checks
%% that they agree). Options default to what the component does when
%% the option is left out.
-ifndef(AIHTML_STEPS_HRL).
-define(AIHTML_STEPS_HRL, true).

-include("aihtml_element.hrl").

%% A step indicator; `value' is the 0-based current step and postback
%% fires on change. `show_nav' undefined: shown when a step has content.
-record(ah_steps, {?AH_BASE(aihtml_steps),
                   items = [] :: [aihtml_steps:step()],
                   value = 0 :: integer(),
                   orientation = horizontal :: horizontal | vertical,
                   disabled = false :: boolean(),
                   clickable = true :: boolean(),
                   show_nav = undefined :: undefined | boolean(),
                   prev_label = <<"\x{2190} Previous"/utf8>> :: aihtml_html:html(),
                   next_label = <<"Next \x{2192}"/utf8>> :: aihtml_html:html(),
                   name = undefined :: aihtml_lib_layout:name()}).

-endif.
