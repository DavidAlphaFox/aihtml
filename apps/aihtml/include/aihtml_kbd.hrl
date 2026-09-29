%% Element record of aihtml_kbd (designs/05-records.md). Field
%% names follow the catalog: modifier groups, flags and options are
%% fields of the same name, with the catalog's defaults (aihtml_kbd_tests
%% checks that they agree). Option fields default to what the component
%% used when the option was left out.
-ifndef(AIHTML_KBD_HRL).
-define(AIHTML_KBD_HRL, true).

-include("aihtml_element.hrl").

%% One key, or a combination when `keys' is a list of keys. No postback
%% event.
-record(ah_kbd, {?AH_BASE(aihtml_kbd),
                 keys = [] :: aihtml_html:html() | [aihtml_html:html()],
                 size = md :: md | lg,
                 separator = <<"+">> :: aihtml_html:html()}).

-endif.
