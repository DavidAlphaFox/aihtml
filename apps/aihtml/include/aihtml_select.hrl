%% Element record of aihtml_select (designs/05-records.md). Field names
%% follow the catalog: modifier groups, flags and options are fields of
%% the same name, with the catalog's defaults (aihtml_select_tests checks
%% that they agree).
-ifndef(AIHTML_SELECT_HRL).
-define(AIHTML_SELECT_HRL, true).

-include("aihtml_element.hrl").

%% A native <select> in a styled wrapper. id, attrs and the postback go to
%% the <select> (css to the wrapper); postback fires on change. `size' is
%% the modifier (sm | lg): the HTML size attribute of a list box goes in
%% `attrs' (the builder moves `{size, N}' there).
-record(ah_select, {?AH_BASE(aihtml_select),
                    items = [] :: [aihtml_lib_select:item()],
                    value = undefined :: undefined | aihtml_lib_select:value()
                                       | [aihtml_lib_select:value()],
                    size = undefined :: undefined | sm | lg,
                    template = undefined :: undefined | aihtml_lib_select:template(),
                    block = false :: boolean(),
                    placeholder = undefined :: undefined | aihtml_html:html()}).

-endif.
