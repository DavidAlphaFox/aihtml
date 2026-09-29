%% Element record of aihtml_alert (designs/05-records.md). Field
%% names follow the catalog: modifier groups, flags and options are
%% fields of the same name, with the catalog's defaults (aihtml_alert_tests
%% checks that they agree). Option fields default to what the component
%% used when the option was left out.
-ifndef(AIHTML_ALERT_HRL).
-define(AIHTML_ALERT_HRL, true).

-include("aihtml_element.hrl").

%% An inline message box. `icon' is true (the variant's icon), false or
%% HTML. Postback fires on 'ah:dismiss'.
-record(ah_alert, {?AH_BASE(aihtml_alert),
                   body = [] :: aihtml_html:html(),
                   variant = info :: info | success | warning | error,
                   dismissible = false :: boolean(),
                   title = undefined :: aihtml_html:html(),
                   icon = true :: boolean() | aihtml_html:html()}).

-endif.
