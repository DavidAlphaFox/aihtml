%% Element record of aihtml_pagination (designs/05-records.md). Field names
%% follow the catalog: modifier groups, flags and options are fields of
%% the same name, with the catalog's defaults (aihtml_pagination_tests checks
%% that they agree). Options default to what the component does when
%% the option is left out.
-ifndef(AIHTML_PAGINATION_HRL).
-define(AIHTML_PAGINATION_HRL, true).

-include("aihtml_element.hrl").

%% Page navigation for `total' items, `value' being the current page (from
%% 1); postback fires on change.
-record(ah_pagination, {?AH_BASE(aihtml_pagination),
                        total = 0 :: non_neg_integer(),
                        value = 1 :: integer(),
                        simple = false :: boolean(),
                        disabled = false :: boolean(),
                        page_size = 10 :: integer(),
                        page_sizes = [10, 20, 50, 100] :: [pos_integer()],
                        show_size_selector = true :: boolean(),
                        show_jumper = false :: boolean(),
                        show_first_last = false :: boolean(),
                        show_total = false :: boolean(),
                        max_visible = 7 :: pos_integer(),
                        siblings = undefined :: undefined | non_neg_integer(),
                        href = undefined :: undefined | iodata(),
                        labels = #{} :: #{atom() => aihtml_html:html()},
                        name = undefined :: aihtml_lib_layout:name()}).

-endif.
