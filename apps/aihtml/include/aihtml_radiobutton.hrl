%% Element record of aihtml_radiobutton (designs/05-records.md). Field names
%% follow the catalog: modifier groups, flags and options are fields of
%% the same name, with the catalog's defaults (aihtml_radiobutton_tests checks
%% that they agree).
-ifndef(AIHTML_RADIOBUTTON_HRL).
-define(AIHTML_RADIOBUTTON_HRL, true).

-include("aihtml_element.hrl").

%% A radio button around a native input; `id', `attrs' and the postback go
%% to the input. Postback fires on change.
-record(ah_radiobutton, {?AH_BASE(aihtml_radiobutton),
                         body = [] :: aihtml_html:html(),
                         value = undefined :: undefined | aihtml_lib_choice:value(),
                         checked = false :: boolean(),
                         disabled = false :: boolean(),
                         size = undefined :: undefined | aihtml_lib_choice:size(),
                         locked = false :: boolean(),
                         box_size = undefined :: undefined | pos_integer()}).

-endif.
