%% Element record of aihtml_expandable_text (designs/05-records.md). Field
%% names follow the catalog: modifier groups, flags and options are
%% fields of the same name, with the catalog's defaults (aihtml_expandable_text_tests
%% checks that they agree). Option fields default to what the component
%% used when the option was left out.
-ifndef(AIHTML_EXPANDABLE_TEXT_HRL).
-define(AIHTML_EXPANDABLE_TEXT_HRL, true).

-include("aihtml_element.hrl").

%% Text cut at `threshold' characters with a toggle; postback fires on
%% 'ah:toggle'.
-record(ah_expandable_text, {?AH_BASE(aihtml_expandable_text),
                             text = <<>> :: undefined | unicode:chardata(),
                             threshold = 100 :: non_neg_integer(),
                             expanded = false :: boolean(),
                             expand_label = <<"展开"/utf8>> :: aihtml_html:html(),
                             collapse_label = <<"收起"/utf8>> :: aihtml_html:html()}).

-endif.
