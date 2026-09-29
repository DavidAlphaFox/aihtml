%% Element record of aihtml_markdown_editor (designs/05-records.md). Field
%% names follow the catalog: flags and options are fields of the same
%% name, with the catalog's defaults (aihtml_markdown_editor_tests checks
%% that they agree).
-ifndef(AIHTML_MARKDOWN_EDITOR_HRL).
-define(AIHTML_MARKDOWN_EDITOR_HRL, true).

-include("aihtml_element.hrl").

%% A WYSIWYG Markdown editor (ProseMirror, loaded on demand); `value' is
%% the Markdown text. The postback fires on change (when the editor loses
%% focus with an edited value). Without an `id' one is generated at render.
-record(ah_markdown_editor, {?AH_BASE(aihtml_markdown_editor),
                             value = <<>> :: undefined | unicode:chardata(),
                             name = undefined :: undefined | atom() | iodata(),
                             disabled = false :: boolean(),
                             readonly = false :: boolean(),
                             placeholder = <<"Start typing...">> :: undefined | unicode:chardata(),
                             show_stats = false :: boolean(),
                             max_chars = undefined :: undefined | pos_integer(),
                             height = undefined :: undefined | pos_integer() | unicode:chardata(),
                             labels = #{} :: aihtml_markdown_editor:labels()}).

-endif.
