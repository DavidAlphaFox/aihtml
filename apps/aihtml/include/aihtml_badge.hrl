%% Element record of aihtml_badge (designs/05-records.md). Field
%% names follow the catalog: modifier groups, flags and options are
%% fields of the same name, with the catalog's defaults (aihtml_badge_tests
%% checks that they agree). Option fields default to what the component
%% used when the option was left out.
-ifndef(AIHTML_BADGE_HRL).
-define(AIHTML_BADGE_HRL, true).

-include("aihtml_element.hrl").

%% A count or status dot on the corner of `body' (standalone when blank).
%% No postback event.
-record(ah_badge, {?AH_BASE(aihtml_badge),
                   body = [] :: aihtml_html:html(),
                   variant = standard :: standard | dot | online | away | busy | offline
                                       | invisible,
                   color = primary :: default | aihtml_lib_color:theme_color(),
                   overlap = rect :: rect | circular,
                   vertical = top :: top | bottom,
                   horizontal = right :: left | right,
                   show_zero = false :: boolean(),
                   count = undefined :: undefined | number() | aihtml_html:html(),
                   max = 99 :: number()}).

-endif.
