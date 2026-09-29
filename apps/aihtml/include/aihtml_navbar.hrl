%% Element record of aihtml_navbar (designs/05-records.md). Field names
%% follow the catalog: modifier groups, flags and options are fields of
%% the same name, with the catalog's defaults (aihtml_navbar_tests checks
%% that they agree).
-ifndef(AIHTML_NAVBAR_HRL).
-define(AIHTML_NAVBAR_HRL, true).

-include("aihtml_element.hrl").

%% Bar of selectable items; `value' is the selected item. Postback fires
%% on change.
-record(ah_navbar, {?AH_BASE(aihtml_navbar),
                    items = [] :: [aihtml_lib_nav:item()],
                    value = undefined :: undefined | aihtml_lib_nav:key(),
                    orientation = horizontal :: horizontal | vertical,
                    minimized = false :: boolean(),
                    disabled = false :: boolean(),
                    brand = undefined :: aihtml_html:html(),
                    extra = undefined :: aihtml_html:html(),
                    title = <<>> :: aihtml_html:html(),
                    minimized_height = 36 :: aihtml_lib_nav:px(),
                    minimize_width = undefined :: undefined | aihtml_lib_nav:px(),
                    columns = [] :: [aihtml_lib_nav:px()],
                    selection = true :: boolean(),
                    name = undefined :: aihtml_lib_nav:name()}).

-endif.
