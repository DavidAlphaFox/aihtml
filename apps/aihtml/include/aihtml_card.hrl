%% Element record of aihtml_card (designs/05-records.md). Field names
%% follow the catalog: modifier groups, flags and options are fields of
%% the same name, with the catalog's defaults (aihtml_card_tests checks
%% that they agree). Options default to what the component does when
%% the option is left out.
-ifndef(AIHTML_CARD_HRL).
-define(AIHTML_CARD_HRL, true).

-include("aihtml_element.hrl").

%% A container with optional media, header and footer; no postback event.
-record(ah_card, {?AH_BASE(aihtml_card),
                  body = [] :: aihtml_html:html(),
                  hover = false :: boolean(),
                  flush = false :: boolean(),
                  title = undefined :: undefined | aihtml_html:html(),
                  subtitle = undefined :: undefined | aihtml_html:html(),
                  extra = undefined :: undefined | aihtml_html:html(),
                  header = undefined :: undefined | aihtml_html:html(),
                  media = undefined :: undefined | aihtml_html:html(),
                  footer = undefined :: undefined | aihtml_html:html()}).

-endif.
