%% Element record of aihtml_chip (designs/05-records.md). Field
%% names follow the catalog: modifier groups, flags and options are
%% fields of the same name, with the catalog's defaults (aihtml_chip_tests
%% checks that they agree). Option fields default to what the component
%% used when the option was left out.
-ifndef(AIHTML_CHIP_HRL).
-define(AIHTML_CHIP_HRL, true).

-include("aihtml_element.hrl").

%% A compact label. `value' defaults to a binary `body'. Postback fires on
%% change (after the chip removed itself) for a removable chip that is not
%% clickable, otherwise on click.
-record(ah_chip, {?AH_BASE(aihtml_chip),
                  body = [] :: aihtml_html:html(),
                  variant = filled :: filled | outlined | soft,
                  color = default :: default | aihtml_lib_color:theme_color(),
                  size = medium :: small | medium,
                  removable = false :: boolean(),
                  clickable = false :: boolean(),
                  disabled = false :: boolean(),
                  avatar = undefined :: aihtml_html:html(),
                  icon = undefined :: aihtml_html:html(),
                  value = undefined :: term()}).

-endif.
