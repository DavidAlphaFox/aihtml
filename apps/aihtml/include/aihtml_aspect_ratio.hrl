%% Element record of aihtml_aspect_ratio (designs/05-records.md). Field
%% names follow the catalog: modifier groups, flags and options are
%% fields of the same name, with the catalog's defaults (aihtml_aspect_ratio_tests
%% checks that they agree). Option fields default to what the component
%% used when the option was left out.
-ifndef(AIHTML_ASPECT_RATIO_HRL).
-define(AIHTML_ASPECT_RATIO_HRL, true).

-include("aihtml_element.hrl").

%% A box locked to `ratio' (<<"16/9">>, <<"4:3">>, 1.5, {W, H}; undefined
%% is 16 / 9). `style' is appended to the aspect-ratio style. No postback
%% event.
-record(ah_aspect_ratio, {?AH_BASE(aihtml_aspect_ratio),
                          body = [] :: aihtml_html:html(),
                          ratio = undefined :: undefined | number() | {number(), number()}
                                             | iodata(),
                          style = undefined :: undefined | iodata()}).

-endif.
