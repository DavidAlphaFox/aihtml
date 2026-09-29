%% Element record of aihtml_navigationbar (designs/05-records.md): sigil's
%% accordion-style navigationbar. Field names follow the catalog: modifier
%% groups, flags and options are fields of the same name, with the
%% catalog's defaults (aihtml_navigationbar_tests checks that they agree).
-ifndef(AIHTML_NAVIGATIONBAR_HRL).
-define(AIHTML_NAVIGATIONBAR_HRL, true).

-include("aihtml_element.hrl").

%% Collapsible sections (sigil's navigationbar, an accordion); `value'
%% holds the expanded indexes and postback fires on change.
-record(ah_navigationbar, {?AH_BASE(aihtml_navigationbar),
                           items = [] :: [aihtml_navigationbar:item()],
                           value = undefined :: aihtml_navigationbar:value(),
                           name = undefined :: undefined | atom() | iodata(),
                           square = false :: boolean(),
                           disable_gutters = false :: boolean(),
                           no_arrow = false :: boolean(),
                           expand_mode = single_fit_height :: single | single_fit_height
                                                            | multiple | toggle | none,
                           animation = slide :: slide | fade | none,
                           toggle_mode = click :: click | dblclick | none,
                           arrow_position = right :: left | right,
                           expand_icon = undefined :: aihtml_html:html(),
                           collapse_icon = undefined :: aihtml_html:html(),
                           expand_duration = 250 :: non_neg_integer(),
                           collapse_duration = 250 :: non_neg_integer(),
                           width = undefined :: undefined | integer() | iodata(),
                           height = undefined :: undefined | integer() | iodata(),
                           disabled = false :: boolean()}).

-endif.
