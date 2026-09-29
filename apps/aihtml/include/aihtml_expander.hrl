%% Element record of aihtml_expander (designs/05-records.md). Field names
%% follow the catalog: modifier groups, flags and options are fields of
%% the same name, with the catalog's defaults (aihtml_expander_tests checks
%% that they agree). Options default to what the component does when
%% the option is left out.
-ifndef(AIHTML_EXPANDER_HRL).
-define(AIHTML_EXPANDER_HRL, true).

-include("aihtml_element.hrl").

%% A collapsible section, value "true" / "false"; postback fires on change.
%% Without an `id' one is generated (ah-expander-N).
-record(ah_expander, {?AH_BASE(aihtml_expander),
                      body = [] :: aihtml_html:html(),
                      position = top :: top | bottom,
                      square = false :: boolean(),
                      no_gutters = false :: boolean(),
                      disabled = false :: boolean(),
                      header = <<>> :: aihtml_html:html() | #{atom() => aihtml_html:html()},
                      actions = undefined :: undefined | aihtml_html:html(),
                      expanded = true :: boolean(),
                      toggle_mode = click :: click | dblclick | none,
                      animation = undefined :: undefined | slide | fade | none,
                      duration = undefined :: undefined | non_neg_integer(),
                      show_arrow = true :: boolean(),
                      arrow_position = right :: right | left,
                      expand_icon = undefined :: undefined | aihtml_html:html(),
                      collapse_icon = undefined :: undefined | aihtml_html:html(),
                      accordion = undefined :: aihtml_lib_layout:name(),
                      name = undefined :: aihtml_lib_layout:name()}).

-endif.
