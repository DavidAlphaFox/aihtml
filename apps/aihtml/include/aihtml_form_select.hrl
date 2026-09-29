%% Element records of aihtml_form_select (designs/05-records.md). Field
%% names follow the catalog: modifier groups, flags and options are fields
%% of the same name, with the catalog's defaults (aihtml_form_select_tests
%% checks that they agree).
-ifndef(AIHTML_FORM_SELECT_HRL).
-define(AIHTML_FORM_SELECT_HRL, true).

-include("aihtml_element.hrl").

-type ah_select_value() :: binary() | atom() | number().
%% Value | {Value, Label} | {Value, Label, #{disabled => true}}
%% | {group, Label, [Item]}
-type ah_select_item() :: ah_select_value() | {ah_select_value(), aihtml_html:html()}
                        | {ah_select_value(), aihtml_html:html(), #{disabled => boolean()}}
                        | {group, aihtml_html:html(), [ah_select_item()]}.
-type ah_select_template() :: primary | success | warning | danger.
%% px (integer) or a CSS length
-type ah_select_size() :: integer() | binary().
-type ah_select_range() :: {number(), number()} | {number(), number(), number()}.
-type ah_select_label_position() :: left | top | right | bottom.
-type ah_select_control() :: aihtml_html:html() | fun((term()) -> aihtml_html:html()).
-type ah_select_field_spec() :: {aihtml_html:html(), ah_select_control()}
                              | #{label => aihtml_html:html(), control := ah_select_control(),
                                  atom() => term()}
                              | {columns, [ah_select_field_spec()]}
                              | {text, aihtml_html:html()}
                              | blank | {blank, pos_integer()}.

%% A popup list with one choice; postback fires on change. `name' adds a
%% hidden input carrying the value.
-record(ah_dropdownlist, {?AH_BASE(aihtml_form_select),
                          items = [] :: [ah_select_item()],
                          value = undefined :: undefined | ah_select_value(),
                          template = undefined :: undefined | ah_select_template(),
                          simple = false :: boolean(),
                          disabled = false :: boolean(),
                          block = false :: boolean(),
                          name = undefined :: undefined | atom() | iodata(),
                          placeholder = <<"Select…"/utf8>> :: aihtml_html:html(),
                          filterable = false :: boolean(),
                          filter_placeholder = <<"Search…"/utf8>> :: iodata(),
                          dropdown_height = 200 :: ah_select_size()}).

%% A native <select> in a styled wrapper. id, attrs and the postback go to
%% the <select> (css to the wrapper); postback fires on change. `size' is
%% the modifier (sm | lg): the HTML size attribute of a list box goes in
%% `attrs' (the builder moves `{size, N}' there).
-record(ah_select, {?AH_BASE(aihtml_form_select),
                    items = [] :: [ah_select_item()],
                    value = undefined :: undefined | ah_select_value() | [ah_select_value()],
                    size = undefined :: undefined | sm | lg,
                    template = undefined :: undefined | ah_select_template(),
                    block = false :: boolean(),
                    placeholder = undefined :: undefined | aihtml_html:html()}).

%% A slider; `value' is a number or {Lo, Hi} for two thumbs. Postback fires
%% on change (on release and on each keyboard or button step).
-record(ah_slider, {?AH_BASE(aihtml_form_select),
                    range = {0, 100} :: ah_select_range(),
                    value = undefined :: undefined | number() | {number(), number()},
                    orientation = horizontal :: horizontal | vertical,
                    template = undefined :: undefined | ah_select_template() | info | secondary,
                    disabled = false :: boolean(),
                    buttons = false :: boolean(),
                    tooltip = false :: boolean(),
                    name = undefined :: undefined | atom() | iodata(),
                    ticks = false :: false | number(),
                    minor_ticks = false :: false | number(),
                    labels = true :: boolean(),
                    ticks_position = bottom :: top | bottom | both,
                    min_range = undefined :: undefined | number()}).

%% A labelled form row around `body' (the control). No postback event.
-record(ah_field, {?AH_BASE(aihtml_form_select),
                   label = undefined :: undefined | aihtml_html:html(),
                   body = [] :: aihtml_html:html(),
                   label_position = left :: ah_select_label_position(),
                   for = undefined :: undefined | atom() | iodata(),
                   help = undefined :: undefined | aihtml_html:html(),
                   error = undefined :: undefined | aihtml_html:html(),
                   required = false :: boolean(),
                   info = undefined :: undefined | aihtml_html:html(),
                   label_width = undefined :: undefined | ah_select_size()}).

%% sigil's declarative form: rows from `fields', controls filled from
%% `values'. Postback fires on submit (tag form); a div has no postback
%% event.
-record(ah_form_layout, {?AH_BASE(aihtml_form_select),
                         fields = [] :: [ah_select_field_spec()],
                         values = #{} :: #{term() => term()},
                         bordered = false :: boolean(),
                         bg = false :: boolean(),
                         disabled = false :: boolean(),
                         label_position = undefined :: undefined | ah_select_label_position(),
                         label_width = undefined :: undefined | ah_select_size(),
                         padding = 10 :: ah_select_size()
                                       | {ah_select_size(), ah_select_size(),
                                          ah_select_size(), ah_select_size()},
                         tag = form :: form | 'div'}).

-endif.
