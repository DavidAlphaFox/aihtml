%% Element record of aihtml_sidenav (designs/05-records.md). Field names
%% follow the catalog: modifier groups, flags and options are fields of
%% the same name, with the catalog's defaults (aihtml_sidenav_tests
%% checks that they agree).
-ifndef(AIHTML_SIDENAV_HRL).
-define(AIHTML_SIDENAV_HRL, true).

-include("aihtml_element.hrl").

%% Application sidebar. `groups' is [aihtml_sidenav:group()] or a plain
%% item list; `value' is the active item. Postback fires on change.
-record(ah_sidenav, {?AH_BASE(aihtml_sidenav),
                     groups = [] :: [aihtml_sidenav:group()] | [aihtml_lib_nav:item()],
                     value = undefined :: undefined | aihtml_lib_nav:key(),
                     collapsed = false :: boolean(),
                     brand = undefined :: undefined
                                        | #{name => aihtml_html:html(),
                                            logo => aihtml_lib_nav:icon(),
                                            href => iodata()}
                                        | aihtml_html:html(),
                     footer = undefined :: aihtml_html:html(),
                     collapsible = false :: boolean(),
                     route_prefix = undefined :: undefined | iodata(),
                     name = undefined :: aihtml_lib_nav:name()}).

-endif.
