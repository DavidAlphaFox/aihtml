%% Element record of aihtml_command (designs/05-records.md): the command
%% palette. Field names follow the catalog: modifier groups, flags and
%% options are fields of the same name, with the catalog's defaults
%% (aihtml_command_tests checks that they agree).
-ifndef(AIHTML_COMMAND_HRL).
-define(AIHTML_COMMAND_HRL, true).

-include("aihtml_element.hrl").

%% A command palette: a search field over grouped commands with keyboard
%% navigation. Postback fires on 'ah:select' (Event.value is the chosen
%% command's value).
-record(ah_command, {?AH_BASE(aihtml_command),
                     items = [] :: [aihtml_command:entry()],
                     palette = false :: boolean(),
                     auto_focus = false :: boolean(),
                     placeholder = <<"Type a command or search…"/utf8>> :: iodata(),
                     empty_text = <<"No results found.">> :: iodata(),
                     query = <<>> :: iodata(),
                     search = undefined :: undefined | aihtml_command:action_ref(),
                     hotkey = undefined :: undefined | iodata(),
                     close_on_select = true :: boolean()}).

-endif.
