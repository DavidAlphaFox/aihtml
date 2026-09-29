%% Element record of aihtml_avatar (designs/05-records.md). Field
%% names follow the catalog: modifier groups, flags and options are
%% fields of the same name, with the catalog's defaults (aihtml_avatar_tests
%% checks that they agree). Option fields default to what the component
%% used when the option was left out.
-ifndef(AIHTML_AVATAR_HRL).
-define(AIHTML_AVATAR_HRL, true).

-include("aihtml_element.hrl").

%% An image with an initials fallback (`body'). No postback event.
-record(ah_avatar, {?AH_BASE(aihtml_avatar),
                    body = [] :: aihtml_html:html(),
                    size = md :: sm | md | lg | xl,
                    shape = circle :: circle | square | rounded,
                    color = primary :: aihtml_lib_color:theme_color(),
                    src = undefined :: undefined | iodata(),
                    alt = <<>> :: iodata()}).

-endif.
