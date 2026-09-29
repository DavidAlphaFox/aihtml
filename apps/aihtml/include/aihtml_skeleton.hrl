%% Element record of aihtml_skeleton (designs/05-records.md). Field names
%% follow the catalog: modifier groups, flags and options are fields of
%% the same name, with the catalog's defaults (aihtml_skeleton_tests checks
%% that they agree). Options default to what the component does when
%% the option is left out.
-ifndef(AIHTML_SKELETON_HRL).
-define(AIHTML_SKELETON_HRL, true).

-include("aihtml_element.hrl").

%% A shimmering placeholder; no postback event. `variant' undefined
%% renders the text variant without its class.
-record(ah_skeleton, {?AH_BASE(aihtml_skeleton),
                      variant = undefined :: undefined | text | circle | rect,
                      static = false :: boolean(),
                      done = false :: boolean(),
                      lines = 3 :: integer(),
                      width = undefined :: aihtml_lib_layout:css_length(),
                      height = undefined :: aihtml_lib_layout:css_length(),
                      radius = undefined :: aihtml_lib_layout:css_length(),
                      label = <<"Loading">> :: aihtml_html:html()}).

-endif.
