%% Element records of the button group (aihtml_form_buttons). Field names
%% follow the catalog: modifier groups, flags and options are fields of
%% the same name, with the catalog's defaults (aihtml_form_buttons_tests
%% checks that they agree).
-ifndef(AIHTML_FORM_BUTTONS_HRL).
-define(AIHTML_FORM_BUTTONS_HRL, true).

-include("aihtml_element.hrl").

-type ah_btn_variant() :: primary | secondary | outlined | success | warning | error
                        | info | default | borderless.
-type ah_btn_size() :: sm | md | lg.
-type ah_btn_icon_position() :: left | right | top | bottom.
%% Label | {Value, Label} | {Value, Label, ItemAttrs}; menus also take divider.
-type ah_btn_item() :: aihtml_html:html() | {term(), aihtml_html:html()}
                     | {term(), aihtml_html:html(), aihtml_html:attrs()} | divider.

%% A native <button type="button">; `value' becomes its value attribute.
-record(ah_button, {?AH_BASE(aihtml_form_buttons),
                    body = [] :: aihtml_html:html(),
                    value = undefined :: term(),
                    variant = primary :: ah_btn_variant(),
                    size = md :: ah_btn_size(),
                    round = false :: boolean(),
                    disabled = false :: boolean(),
                    icon = undefined :: aihtml_html:html(),
                    img = undefined :: undefined | iodata(),
                    icon_position = left :: ah_btn_icon_position()}).

%% An <a href> styled as a button; disabled drops the href.
-record(ah_link_button, {?AH_BASE(aihtml_form_buttons),
                         body = [] :: aihtml_html:html(),
                         href = undefined :: undefined | iodata(),
                         variant = primary :: ah_btn_variant(),
                         size = md :: ah_btn_size(),
                         round = false :: boolean(),
                         disabled = false :: boolean()}).

%% A pressed / released button; postback fires on change.
-record(ah_toggle_button, {?AH_BASE(aihtml_form_buttons),
                           body = [] :: aihtml_html:html(),
                           value = false :: boolean(),
                           name = undefined :: undefined | atom() | iodata(),
                           variant = primary :: ah_btn_variant(),
                           size = md :: ah_btn_size(),
                           round = false :: boolean(),
                           disabled = false :: boolean(),
                           icon = undefined :: aihtml_html:html(),
                           img = undefined :: undefined | iodata(),
                           icon_position = left :: ah_btn_icon_position()}).

%% Joined buttons. `value' is the selected item in radio mode, a list (or
%% "a,b") in checkbox mode, ignored in default mode. Postback fires on
%% change in radio and checkbox mode, on click in default mode.
-record(ah_button_group, {?AH_BASE(aihtml_form_buttons),
                          items = [] :: [ah_btn_item()],
                          value = undefined :: term(),
                          name = undefined :: undefined | atom() | iodata(),
                          mode = default :: default | radio | checkbox,
                          orientation = horizontal :: horizontal | vertical,
                          shape = rounded :: rounded | square,
                          fill = undefined :: undefined | filled | outlined,
                          disabled = false :: boolean()}).

%% Mutually exclusive segments; postback fires on change.
-record(ah_segmented_control, {?AH_BASE(aihtml_form_buttons),
                               items = [] :: [ah_btn_item()],
                               value = undefined :: term(),
                               name = undefined :: undefined | atom() | iodata(),
                               size = md :: ah_btn_size(),
                               full_width = false :: boolean(),
                               disabled = false :: boolean()}).

%% A button that opens a menu; `value' is the initially selected item and
%% postback fires on change.
-record(ah_dropdown_button, {?AH_BASE(aihtml_form_buttons),
                             body = [] :: aihtml_html:html(),
                             items = [] :: [ah_btn_item()],
                             value = undefined :: term(),
                             name = undefined :: undefined | atom() | iodata(),
                             variant = undefined :: undefined | primary | success | warning
                                                  | error | outlined,
                             size = md :: ah_btn_size(),
                             rounded = false :: boolean(),
                             auto_open = false :: boolean(),
                             disabled = false :: boolean()}).

%% A main action plus an arrow that opens a menu; postback fires on a
%% click on the main half.
-record(ah_split_button, {?AH_BASE(aihtml_form_buttons),
                          body = [] :: aihtml_html:html(),
                          items = [] :: [ah_btn_item()],
                          value = undefined :: term(),
                          name = undefined :: undefined | atom() | iodata(),
                          variant = primary :: primary | secondary | success | warning
                                             | error | info | outlined,
                          size = md :: ah_btn_size(),
                          menu_align = 'end' :: start | 'end',
                          disabled = false :: boolean()}).

-endif.
