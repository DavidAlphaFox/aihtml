%% Element record of aihtml_loader (designs/05-records.md). Field names
%% follow the catalog: modifier groups, flags and options are fields of
%% the same name, with the catalog's defaults (aihtml_loader_tests checks
%% that they agree). Options default to what the component does when
%% the option is left out.
-ifndef(AIHTML_LOADER_HRL).
-define(AIHTML_LOADER_HRL, true).

-include("aihtml_element.hrl").

%% A spinner; no postback event.
-record(ah_loader, {?AH_BASE(aihtml_loader),
                    text_position = bottom :: bottom | top | left | right,
                    hidden = false :: boolean(),
                    inline = false :: boolean(),
                    center = false :: boolean(),
                    disabled = false :: boolean(),
                    text = <<"Loading...">> :: aihtml_html:html(),
                    modal = false :: boolean()}).

-endif.
