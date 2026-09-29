%% Element record of aihtml_form_layout (designs/05-records.md). Field names
%% follow the catalog: modifier groups, flags and options are fields of
%% the same name, with the catalog's defaults (aihtml_form_layout_tests checks
%% that they agree).
-ifndef(AIHTML_FORM_LAYOUT_HRL).
-define(AIHTML_FORM_LAYOUT_HRL, true).

-include("aihtml_element.hrl").

%% sigil's declarative form: rows from `fields', controls filled from
%% `values'. Postback fires on submit (tag form); a div has no postback
%% event.
-record(ah_form_layout, {?AH_BASE(aihtml_form_layout),
                         fields = [] :: [aihtml_form_layout:field_spec()],
                         values = #{} :: #{term() => term()},
                         bordered = false :: boolean(),
                         bg = false :: boolean(),
                         disabled = false :: boolean(),
                         label_position = undefined :: undefined
                                                     | aihtml_lib_form:label_position(),
                         label_width = undefined :: undefined | aihtml_lib_form:size(),
                         padding = 10 :: aihtml_lib_form:size()
                                       | {aihtml_lib_form:size(), aihtml_lib_form:size(),
                                          aihtml_lib_form:size(), aihtml_lib_form:size()},
                         tag = form :: form | 'div'}).

-endif.
