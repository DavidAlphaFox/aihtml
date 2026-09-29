%% Element record of aihtml_field (designs/05-records.md). Field names
%% follow the catalog: modifier groups, flags and options are fields of
%% the same name, with the catalog's defaults (aihtml_field_tests checks
%% that they agree).
-ifndef(AIHTML_FIELD_HRL).
-define(AIHTML_FIELD_HRL, true).

-include("aihtml_element.hrl").

%% A labelled form row around `body' (the control). No postback event.
-record(ah_field, {?AH_BASE(aihtml_field),
                   label = undefined :: undefined | aihtml_html:html(),
                   body = [] :: aihtml_html:html(),
                   label_position = left :: aihtml_lib_form:label_position(),
                   for = undefined :: undefined | atom() | iodata(),
                   help = undefined :: undefined | aihtml_html:html(),
                   error = undefined :: undefined | aihtml_html:html(),
                   required = false :: boolean(),
                   info = undefined :: undefined | aihtml_html:html(),
                   label_width = undefined :: undefined | aihtml_lib_form:size()}).

-endif.
