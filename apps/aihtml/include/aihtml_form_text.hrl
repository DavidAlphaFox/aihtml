%% Element records of aihtml_form_text (designs/05-records.md). Field
%% names follow the catalog: modifier groups, flags and options are fields
%% of the same name, with the catalog's defaults (aihtml_form_text_tests
%% checks that they agree).
-ifndef(AIHTML_FORM_TEXT_HRL).
-define(AIHTML_FORM_TEXT_HRL, true).

-include("aihtml_element.hrl").

-type ah_text_size() :: sm | lg.
-type ah_text_state() :: invalid | valid.
-type ah_text_number() :: number() | binary().
%% The text of a field; numbers and atoms are written as text.
-type ah_text_value() :: iodata() | number() | atom().
-type ah_text_chip_color() :: primary | secondary | success | warning | error | info.
-type ah_text_chip_variant() :: soft | filled | outlined.

%% One line of text. `id', `attrs' and the postback go to the native
%% <input>; postback fires on change.
-record(ah_input, {?AH_BASE(aihtml_form_text),
                   value = undefined :: undefined | ah_text_value(),
                   size = undefined :: undefined | ah_text_size(),
                   state = undefined :: undefined | ah_text_state(),
                   disabled = false :: boolean(),
                   no_rounded = false :: boolean(),
                   clearable = false :: boolean(),
                   prefix = undefined :: undefined | aihtml_html:html(),
                   suffix = undefined :: undefined | aihtml_html:html(),
                   label = undefined :: undefined | aihtml_html:html()}).

%% Several lines of text, styled like ah_input. `id', `attrs' and the
%% postback go to the native <textarea>; postback fires on change.
-record(ah_textarea, {?AH_BASE(aihtml_form_text),
                      value = undefined :: undefined | ah_text_value(),
                      size = undefined :: undefined | ah_text_size(),
                      state = undefined :: undefined | ah_text_state(),
                      disabled = false :: boolean(),
                      no_rounded = false :: boolean(),
                      label = undefined :: undefined | aihtml_html:html()}).

%% A password field with a show/hide toggle. `id', `attrs' and the
%% postback go to the native <input>; postback fires on change.
-record(ah_password_input, {?AH_BASE(aihtml_form_text),
                            value = undefined :: undefined | ah_text_value(),
                            size = undefined :: undefined | ah_text_size(),
                            state = undefined :: undefined | ah_text_state(),
                            disabled = false :: boolean(),
                            toggle = true :: boolean(),
                            strength = false :: boolean(),
                            label = undefined :: undefined | aihtml_html:html()}).

%% A numeric field with spin buttons. `id', `attrs' and the postback go to
%% the native <input>; postback fires on change. min, max and step may
%% also be numeric binaries.
-record(ah_number_input, {?AH_BASE(aihtml_form_text),
                          value = undefined :: undefined | ah_text_number(),
                          size = undefined :: undefined | ah_text_size(),
                          state = undefined :: undefined | ah_text_state(),
                          disabled = false :: boolean(),
                          readonly = false :: boolean(),
                          min = undefined :: undefined | ah_text_number(),
                          max = undefined :: undefined | ah_text_number(),
                          step = 1 :: ah_text_number(),
                          decimals = undefined :: undefined | non_neg_integer(),
                          spin = true :: boolean(),
                          symbol = undefined :: undefined | aihtml_html:html(),
                          symbol_position = left :: left | right,
                          allow_null = true :: boolean(),
                          label = undefined :: undefined | aihtml_html:html()}).

%% One box per character; `name' goes to a hidden input. Postback fires on
%% change.
-record(ah_input_otp, {?AH_BASE(aihtml_form_text),
                       length = 6 :: pos_integer(),
                       value = undefined :: undefined | iodata(),
                       name = undefined :: undefined | atom() | iodata(),
                       disabled = false :: boolean(),
                       pattern = digit :: digit | alphanumeric,
                       separator_at = undefined :: undefined | pos_integer()}).

%% Chips plus a text field; `value' is the list of tags and `name' goes to
%% a hidden input (tags joined with commas). Postback fires on change.
-record(ah_tag_input, {?AH_BASE(aihtml_form_text),
                       value = [] :: [unicode:chardata()],
                       name = undefined :: undefined | atom() | iodata(),
                       disabled = false :: boolean(),
                       placeholder = undefined :: undefined | iodata(),
                       max_tags = undefined :: undefined | pos_integer(),
                       allow_duplicates = false :: boolean(),
                       chip_color = primary :: ah_text_chip_color(),
                       chip_variant = soft :: ah_text_chip_variant()}).

-endif.
