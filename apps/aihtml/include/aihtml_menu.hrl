%% Element record of aihtml_menu (designs/05-records.md). Field names
%% follow the catalog: modifier groups, flags and options are fields of
%% the same name, with the catalog's defaults (aihtml_menu_tests checks
%% that they agree).
-ifndef(AIHTML_MENU_HRL).
-define(AIHTML_MENU_HRL, true).

-include("aihtml_element.hrl").

%% Menu bar or context menu; `value' is the active item. Postback fires
%% on change (an item without href was chosen).
-record(ah_menu, {?AH_BASE(aihtml_menu),
                  items = [] :: [aihtml_lib_nav:item()],
                  value = undefined :: undefined | aihtml_lib_nav:key(),
                  mode = horizontal :: horizontal | vertical | popup,
                  show_arrows = false :: boolean(),
                  disabled = false :: boolean(),
                  title = undefined :: undefined | binary(),
                  name = undefined :: aihtml_lib_nav:name(),
                  click_to_open = false :: boolean(),
                  keyboard = true :: boolean(),
                  minimize_width = undefined :: undefined | aihtml_lib_nav:px(),
                  popup_target = undefined :: undefined | iodata()}).

-endif.
