%% Element record of aihtml_markdown_view (designs/05-records.md). The
%% component has no modifiers or options; aihtml_markdown_view_tests checks
%% the record against the catalog.
-ifndef(AIHTML_MARKDOWN_VIEW_HRL).
-define(AIHTML_MARKDOWN_VIEW_HRL, true).

-include("aihtml_element.hrl").

%% Markdown rendered to HTML on the server (aihtml_lib_markdown); `markdown'
%% is the Markdown text. No postback event.
-record(ah_markdown_view, {?AH_BASE(aihtml_markdown_view),
                           markdown = <<>> :: undefined | unicode:chardata()}).

-endif.
