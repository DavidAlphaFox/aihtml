%% Element record of aihtml_dropdownlist (designs/05-records.md). Field names
%% follow the catalog: modifier groups, flags and options are fields of
%% the same name, with the catalog's defaults (aihtml_dropdownlist_tests checks
%% that they agree).
-ifndef(AIHTML_DROPDOWNLIST_HRL).
-define(AIHTML_DROPDOWNLIST_HRL, true).

-include("aihtml_element.hrl").

%% A popup list with one choice; postback fires on change. `name' adds a
%% hidden input carrying the value.
-record(ah_dropdownlist, {?AH_BASE(aihtml_dropdownlist),
                          items = [] :: [aihtml_lib_select:item()],
                          value = undefined :: undefined | aihtml_lib_select:value(),
                          template = undefined :: undefined | aihtml_lib_select:template(),
                          simple = false :: boolean(),
                          disabled = false :: boolean(),
                          block = false :: boolean(),
                          name = undefined :: undefined | atom() | iodata(),
                          placeholder = <<"Select…"/utf8>> :: aihtml_html:html(),
                          filterable = false :: boolean(),
                          filter_placeholder = <<"Search…"/utf8>> :: iodata(),
                          dropdown_height = 200 :: aihtml_lib_form:size()}).

-endif.
