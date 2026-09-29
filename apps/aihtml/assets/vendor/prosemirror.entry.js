// ProseMirror and markdown-it for markdown_editor, as one ES module. Vite
// bundles it (with the packages it imports) into its own lazily loaded
// chunk; the behaviour gets it with AH.vendor("prosemirror"), which
// imports this module dynamically and resolves with its namespace:
//
//   P.state.EditorState, P.view.EditorView, ...
//   P.markdownit(...)     markdown-it (the parser behind
//                         prosemirror-markdown's MarkdownParser)
//
// ProseMirror (prosemirror-*) and markdown-it are MIT licensed; the
// licences of everything in the bundle are in js/THIRD-PARTY-LICENSES.txt.
export * as model from "prosemirror-model";
export * as state from "prosemirror-state";
export * as view from "prosemirror-view";
export * as transform from "prosemirror-transform";
export * as commands from "prosemirror-commands";
export * as keymap from "prosemirror-keymap";
export * as history from "prosemirror-history";
export * as inputrules from "prosemirror-inputrules";
export * as schemaList from "prosemirror-schema-list";
export * as dropcursor from "prosemirror-dropcursor";
export * as gapcursor from "prosemirror-gapcursor";
export * as tables from "prosemirror-tables";
export * as markdown from "prosemirror-markdown";
export { default as markdownit } from "markdown-it";
