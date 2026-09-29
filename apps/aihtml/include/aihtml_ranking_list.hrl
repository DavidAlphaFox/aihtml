%% Element record of aihtml_ranking_list (designs/05-records.md). Field
%% names follow the catalog: modifier groups, flags and options are
%% fields of the same name, with the catalog's defaults (aihtml_ranking_list_tests
%% checks that they agree). Option fields default to what the component
%% used when the option was left out.
-ifndef(AIHTML_RANKING_LIST_HRL).
-define(AIHTML_RANKING_LIST_HRL, true).

-include("aihtml_element.hrl").

%% A top N list. Postback fires on 'ah:item-click' (clickable rows).
-record(ah_ranking_list, {?AH_BASE(aihtml_ranking_list),
                          items = [] :: [aihtml_ranking_list:item()],
                          dense = false :: boolean(),
                          disabled = false :: boolean(),
                          clickable = false :: boolean(),
                          title = undefined :: aihtml_html:html(),
                          max_items = undefined :: undefined | non_neg_integer(),
                          show_rank = true :: boolean(),
                          flag_style = emoji :: emoji | flag_icons | none,
                          tag_colors = #{} :: #{binary() => success | warning | error | info}}).

-endif.
