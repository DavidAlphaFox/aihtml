%% Element records of aihtml_form_choice (designs/05-records.md). Field
%% names follow the catalog: modifier groups, flags and options are fields
%% of the same name, with the catalog's defaults (aihtml_form_choice_tests
%% checks that they agree).
-ifndef(AIHTML_FORM_CHOICE_HRL).
-define(AIHTML_FORM_CHOICE_HRL, true).

-include("aihtml_element.hrl").

-type ah_choice_value() :: binary() | atom() | integer() | float() | string().
%% Value | {Value, Label} | {Value, Label, Opts}. Opts (a map or proplist):
%% disabled, class; radio_cards also description and icon (html).
-type ah_choice_item() :: ah_choice_value() | {ah_choice_value(), aihtml_html:html()}
                        | {ah_choice_value(), aihtml_html:html(), map() | list()}.
-type ah_choice_size() :: sm | md | lg.
-type ah_choice_layout() :: vertical | horizontal.
-type ah_choice_name() :: undefined | atom() | iodata().

%% A checkbox around a native input. `value' is the input's form value,
%% `checked' its state; `id', `attrs' and the postback go to the input.
%% Postback fires on change.
-record(ah_checkbox, {?AH_BASE(aihtml_form_choice),
                      body = [] :: aihtml_html:html(),
                      value = undefined :: undefined | ah_choice_value(),
                      checked = false :: boolean(),
                      disabled = false :: boolean(),
                      size = undefined :: undefined | ah_choice_size(),
                      indeterminate = false :: boolean(),
                      three_states = false :: boolean(),
                      locked = false :: boolean(),
                      box_size = undefined :: undefined | pos_integer()}).

%% A radio button around a native input; `id', `attrs' and the postback go
%% to the input. Postback fires on change.
-record(ah_radiobutton, {?AH_BASE(aihtml_form_choice),
                         body = [] :: aihtml_html:html(),
                         value = undefined :: undefined | ah_choice_value(),
                         checked = false :: boolean(),
                         disabled = false :: boolean(),
                         size = undefined :: undefined | ah_choice_size(),
                         locked = false :: boolean(),
                         box_size = undefined :: undefined | pos_integer()}).

%% An on/off switch (a native checkbox with role=switch); `id', `attrs'
%% and the postback go to the input. Postback fires on change.
-record(ah_switch_button, {?AH_BASE(aihtml_form_choice),
                           body = [] :: aihtml_html:html(),
                           value = undefined :: undefined | ah_choice_value(),
                           checked = false :: boolean(),
                           disabled = false :: boolean(),
                           size = undefined :: undefined | ah_choice_size(),
                           on_label = undefined :: aihtml_html:html(),
                           off_label = undefined :: aihtml_html:html(),
                           locked = false :: boolean(),
                           width = undefined :: undefined | pos_integer(),
                           height = undefined :: undefined | pos_integer(),
                           thumb_size = undefined :: undefined | pos_integer()}).

%% Checkboxes; `value' lists the checked ones. name, disabled, required
%% and form go to every input. Postback fires on change of the root.
-record(ah_checkbox_group, {?AH_BASE(aihtml_form_choice),
                            items = [] :: [ah_choice_item()],
                            value = [] :: [ah_choice_value()],
                            name = undefined :: ah_choice_name(),
                            disabled = false :: boolean(),
                            required = false :: boolean(),
                            form = undefined :: ah_choice_name(),
                            layout = vertical :: ah_choice_layout(),
                            size = undefined :: undefined | ah_choice_size(),
                            label_before = false :: boolean()}).

%% Mutually exclusive radio buttons; `value' is the selected one. Postback
%% fires on change of the root.
-record(ah_radiobutton_group, {?AH_BASE(aihtml_form_choice),
                               items = [] :: [ah_choice_item()],
                               value = undefined :: undefined | ah_choice_value(),
                               name = undefined :: ah_choice_name(),
                               disabled = false :: boolean(),
                               required = false :: boolean(),
                               form = undefined :: ah_choice_name(),
                               layout = vertical :: ah_choice_layout(),
                               size = undefined :: undefined | ah_choice_size(),
                               label_before = false :: boolean()}).

%% Selectable cards (item Opts description, icon). Postback fires on
%% change of the root.
-record(ah_radio_cards, {?AH_BASE(aihtml_form_choice),
                         items = [] :: [ah_choice_item()],
                         value = undefined :: undefined | ah_choice_value(),
                         name = undefined :: ah_choice_name(),
                         disabled = false :: boolean(),
                         required = false :: boolean(),
                         form = undefined :: ah_choice_name(),
                         columns = auto :: 1 | 2 | 3 | auto,
                         align = center :: center | start}).

%% Star rating from 0 to `max'; `name' adds a hidden input. Postback fires
%% on change.
-record(ah_rating_group, {?AH_BASE(aihtml_form_choice),
                          max = 5 :: pos_integer(),
                          value = undefined :: undefined | number(),
                          size = md :: ah_choice_size(),
                          color = warning :: warning | primary | success | error,
                          name = undefined :: ah_choice_name(),
                          precision = 1 :: number(),  % 1 or 0.5
                          allow_clear = true :: boolean(),
                          readonly = false :: boolean(),
                          disabled = false :: boolean()}).

-endif.
