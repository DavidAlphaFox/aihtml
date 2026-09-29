// Entry of the ProseMirror vendor bundle (priv/static/vendor/prosemirror.min.js),
// bundled by scripts/vendor.mjs with esbuild into one IIFE whose global is
// window.AHProseMirror. The markdown_editor behaviour loads it on demand
// with AH.vendor("prosemirror").
//
//   AHProseMirror.state.EditorState, AHProseMirror.view.EditorView, ...
//   AHProseMirror.markdownit(...)     markdown-it (the parser behind
//                                     prosemirror-markdown's MarkdownParser)
//
// ProseMirror (prosemirror-*) and markdown-it are MIT licensed; the
// licences of everything in the bundle are in prosemirror.LICENSE.txt.
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
