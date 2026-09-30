/* Behaviour of the markdown editor (designs/04-components.md). Ported from
 * sigil: text/markdown_editor and the parts of text/prose_editor it uses
 * (schema, markdown, core keymap, plugins: input rules, list keys, table
 * keys, task list, clipboard, placeholder, slash menu, block handle, drag,
 * stats footer).
 *
 * ProseMirror and markdown-it are imported here, so they are part of this
 * component's lazily loaded chunk: a page without a markdown editor never
 * downloads them. Until the chunk has loaded the root shows the server's
 * <textarea> with the Markdown source; the editor starts from what it
 * holds, and it then stays in the DOM, hidden, as the form field.
 *
 * The server renders every piece of UI besides ProseMirror's own document:
 * the slash menu, the block handle, the drag indicator and the stats footer
 * are in the root from the start; this file only shows, moves and fills
 * them (designs/04-components.md, "浏览器端生成 HTML 的规则"). The only
 * nodes built here are ProseMirror's (the document, the task item node
 * view, the placeholder widget) and the drag clone (a copy of the block).
 *
 * Value contract: data-ah-value holds the Markdown; every edit updates it
 * and the textarea and fires `input' on the root; `change' fires when the
 * focus leaves the component with a value that differs from the last
 * `change' (as a native textarea), or at once for an edit made while the
 * editor has no focus (a drag, a task checkbox, exec from the server).
 * setValue changes the value silently. Neither event has a detail.
 *
 * The parts: MarkdownKit (schema, parser, serializer; one per page),
 * EditorSession (one mounted editor and its value), and the plugins that
 * drive the server-rendered UI (SlashMenu, BlockHandle, BlockDrag,
 * StatsFooter). */
import AH from "../core.ts";
import type { FloatHandle } from "../core.ts";
import { Schema, Slice, DOMSerializer } from "prosemirror-model";
import type { DOMOutputSpec, MarkSpec, MarkType, Node as PMNode, NodeSpec, NodeType } from "prosemirror-model";
import { EditorState, Plugin, Selection, TextSelection } from "prosemirror-state";
import type { Command, Transaction } from "prosemirror-state";
import { EditorView, Decoration, DecorationSet } from "prosemirror-view";
import type { NodeView, ViewMutationRecord } from "prosemirror-view";
import { findWrapping } from "prosemirror-transform";
import {
  baseKeymap, chainCommands, createParagraphNear, deleteSelection, exitCode, joinBackward,
  joinForward, liftEmptyBlock, newlineInCode, selectNodeBackward, selectNodeForward,
  setBlockType, splitBlock, toggleMark, wrapIn
} from "prosemirror-commands";
import { keymap } from "prosemirror-keymap";
import { history, undo, redo } from "prosemirror-history";
import {
  InputRule, inputRules, textblockTypeInputRule, undoInputRule, wrappingInputRule
} from "prosemirror-inputrules";
import { wrapInList, liftListItem, sinkListItem, splitListItem } from "prosemirror-schema-list";
import { dropCursor } from "prosemirror-dropcursor";
import { gapCursor } from "prosemirror-gapcursor";
import { addRowAfter, goToNextCell, isInTable, tableEditing } from "prosemirror-tables";
import { MarkdownParser, MarkdownSerializer, defaultMarkdownSerializer } from "prosemirror-markdown";
import type { MarkdownSerializerState } from "prosemirror-markdown";
import markdownit from "markdown-it";
import type Token from "markdown-it/lib/token.mjs";
import type StateCore from "markdown-it/lib/rules_core/state_core.mjs";

/** The result of stats(): counts of the document. */
export interface EditorStats { chars: number; words: number; paragraphs: number; }

/** Options of exec(cmd, opts). */
export interface ExecOptions { href?: string; title?: string; level?: number | string; language?: string; }

/** data-ah-labels: the texts the server translated (enter_url,
 *  enter_image_url, editor). */
type Labels = Record<string, string>;

function fire(el: Element, type: string): void {
  el.dispatchEvent(new CustomEvent(type, { bubbles: true, cancelable: true }));
}

// ProseMirror's own input/change events are not the component's.
function swallow(e: Event): void { e.stopPropagation(); }

// ==================================================================
// Schema, Markdown parser and serializer (prose_editor/schema.cljs,
// prose_editor/markdown.cljs), limited to what Markdown can express
// ==================================================================

function cellSpec(tag: string, role: string): NodeSpec {
  return { content: "inline*", tableRole: role, isolating: true,
           attrs: { colspan: { default: 1 }, rowspan: { default: 1 },
                    colwidth: { default: null } },
           parseDOM: [{ tag: tag, getAttrs: (dom: HTMLElement) => ({
             colspan: +(dom.getAttribute("colspan") ?? "") || 1,
             rowspan: +(dom.getAttribute("rowspan") ?? "") || 1
           }) }],
           toDOM: (): DOMOutputSpec => [tag, 0] };
}

const NODES: Record<string, NodeSpec> = {
  doc: { content: "block+" },
  paragraph: { content: "inline*", group: "block",
               parseDOM: [{ tag: "p" }], toDOM: () => ["p", 0] },
  blockquote: { content: "block+", group: "block", defining: true,
                parseDOM: [{ tag: "blockquote" }],
                toDOM: () => ["blockquote", { "class": "ah-md-blockquote" }, 0] },
  horizontal_rule: { group: "block", parseDOM: [{ tag: "hr" }],
                     toDOM: () => ["hr", { "class": "ah-md-hr" }] },
  heading: { content: "inline*", group: "block", defining: true,
             attrs: { level: { default: 1 } },
             parseDOM: [1, 2, 3, 4, 5, 6].map((i) => ({ tag: "h" + i, attrs: { level: i } })),
             toDOM: (n) => {
               const level = String(n.attrs["level"]);
               return ["h" + level, { "class": "ah-md-h" + level }, 0];
             } },
  code_block: { content: "text*", group: "block", marks: "", code: true, defining: true,
                attrs: { language: { default: "plaintext" } },
                parseDOM: [{ tag: "pre", preserveWhitespace: "full", getAttrs: (dom: HTMLElement) => {
                  const c = dom.querySelector("code");
                  const lang = c && (c.getAttribute("data-language") ||
                                     (c.className || "").replace(/^language-/, ""));
                  return { language: lang || "plaintext" };
                } }],
                toDOM: (n) => {
                  const lang = String(n.attrs["language"]);
                  return ["pre", { "class": "ah-md-code" },
                          ["code", { "class": "language-" + lang, "data-language": lang }, 0]];
                } },
  text: { group: "inline" },
  image: { inline: true, group: "inline", draggable: true,
           attrs: { src: {}, alt: { default: "" }, title: { default: "" } },
           parseDOM: [{ tag: "img[src]", getAttrs: (dom: HTMLElement) => ({
             src: dom.getAttribute("src"), alt: dom.getAttribute("alt") || "",
             title: dom.getAttribute("title") || ""
           }) }],
           toDOM: (n) => ["img", { src: n.attrs["src"], alt: n.attrs["alt"], title: n.attrs["title"] || null,
                                   "class": "ah-md-image" }] },
  hard_break: { inline: true, group: "inline", selectable: false,
                parseDOM: [{ tag: "br" }], toDOM: () => ["br"] },
  bullet_list: { content: "list_item+", group: "block", attrs: { tight: { default: true } },
                 parseDOM: [{ tag: "ul" }],
                 toDOM: () => ["ul", { "class": "ah-md-list" }, 0] },
  ordered_list: { content: "list_item+", group: "block",
                  attrs: { order: { default: 1 }, tight: { default: true } },
                  parseDOM: [{ tag: "ol", getAttrs: (dom: HTMLElement) => {
                    const s = parseInt(dom.getAttribute("start") ?? "", 10);
                    return { order: isNaN(s) ? 1 : s };
                  } }],
                  toDOM: (n) => n.attrs["order"] === 1 ? ["ol", { "class": "ah-md-list" }, 0]
                    : ["ol", { start: n.attrs["order"], "class": "ah-md-list" }, 0] },
  list_item: { content: "paragraph block*", defining: true,
               parseDOM: [{ tag: "li" }], toDOM: () => ["li", 0] },
  task_list: { content: "task_item+", group: "block", attrs: { tight: { default: true } },
               parseDOM: [{ tag: "ul.ah-md-tasklist", priority: 60 }],
               toDOM: () => ["ul", { "class": "ah-md-tasklist" }, 0] },
  task_item: { content: "paragraph block*", defining: true,
               attrs: { checked: { default: false } },
               parseDOM: [{ tag: "li.task-list-item", priority: 60, getAttrs: (dom: HTMLElement) => {
                 const cb = dom.querySelector<HTMLInputElement>("input[type=checkbox]");
                 return { checked: !!(cb && cb.checked) };
               } }],
               toDOM: (n) => ["li", { "class": "task-list-item" },
                              ["input", { type: "checkbox", "class": "task-list-item-checkbox",
                                          checked: n.attrs["checked"] ? "" : null }],
                              ["span", { "class": "task-list-item-content" }, 0]] },
  table: { content: "table_row+", group: "block", tableRole: "table", isolating: true,
           parseDOM: [{ tag: "table" }],
           toDOM: () => ["table", { "class": "ah-md-table" }, ["tbody", 0]] },
  table_row: { content: "(table_cell | table_header)*", tableRole: "row",
               parseDOM: [{ tag: "tr" }], toDOM: () => ["tr", 0] },
  table_cell: cellSpec("td", "cell"),
  table_header: cellSpec("th", "header_cell")
};

const MARKS: Record<string, MarkSpec> = {
  link: { attrs: { href: {}, title: { default: "" } }, inclusive: false,
          parseDOM: [{ tag: "a[href]", getAttrs: (dom: HTMLElement) => ({
            href: dom.getAttribute("href"), title: dom.getAttribute("title") || ""
          }) }],
          toDOM: (m) => ["a", { href: m.attrs["href"], title: m.attrs["title"] || null,
                                "class": "ah-md-link" }, 0] },
  em: { parseDOM: [{ tag: "i" }, { tag: "em" }, { style: "font-style=italic" }],
        toDOM: () => ["em", 0] },
  strong: { parseDOM: [{ tag: "strong" }, { tag: "b" },
                       { style: "font-weight", getAttrs: (v: string) => /^(bold(er)?|[5-9]\d{2,})$/.test(v) && null }],
            toDOM: () => ["strong", 0] },
  code: { parseDOM: [{ tag: "code" }],
          toDOM: () => ["code", { "class": "ah-md-inline-code" }, 0] },
  strikethrough: { parseDOM: [{ tag: "s" }, { tag: "del" }, { tag: "strike" },
                              { style: "text-decoration", getAttrs: (v: string) => /line-through/.test(v) && null }],
                   toDOM: () => ["s", 0] }
};

// A list is tight when its paragraphs are hidden (prosemirror-markdown's
// listIsTight); tight lists serialize without blank lines between items.
function tight(toks: Token[], i: number): boolean {
  while (++i < toks.length) {
    const t = toks[i];
    if (t && !/_item_open$/.test(t.type)) { return t.hidden; }
  }
  return false;
}

// markdown-it core rule (sigil's install-task-list-rule!): a bullet list
// whose direct items all start with "[ ]" / "[x]" becomes a task list.
const RE_TASK = /^\[([ xX])\]\s?/;
function taskListRule(st: StateCore): void {
  const toks = st.tokens;
  const at = (k: number): Token => toks[k]!;          // k < toks.length throughout
  for (let i = 0; i < toks.length; i++) {
    if (at(i).type !== "bullet_list_open") { continue; }
    let depth = 1, close = -1;
    const items: number[] = [];
    for (let j = i + 1; j < toks.length && close < 0; j++) {
      const t = at(j).type;
      if (/_list_open$/.test(t)) { depth++; }
      else if (/_list_close$/.test(t)) { depth--; if (depth === 0) { close = j; } }
      else if (t === "list_item_open" && depth === 1) { items.push(j); }
    }
    if (close < 0 || !items.length) { continue; }
    const inline = items.map((k) => {
      for (let m = k + 1; m < close; m++) {
        if (at(m).type === "inline") { return m; }
        if (at(m).type === "list_item_close") { return -1; }
      }
      return -1;
    });
    if (!inline.every((m) => m >= 0 && RE_TASK.test(at(m).content))) { continue; }
    at(i).type = "task_list_open";
    at(close).type = "task_list_close";
    items.forEach((k, n) => {
      at(k).type = "task_item_open";
      let d = 0;
      for (let m = k + 1; m < close; m++) {
        if (at(m).type === "list_item_open") { d++; }
        if (at(m).type === "list_item_close") {
          if (d === 0) { at(m).type = "task_item_close"; break; }
          d--;
        }
      }
      const inl = at(inline[n] ?? -1), match = RE_TASK.exec(inl.content);
      if (!match) { return; }                            // tested above
      at(k).attrSet("checked", String(match[1] !== " "));
      inl.content = inl.content.slice(match[0].length);
      const first = inl.children && inl.children[0];
      if (first && first.type === "text") { first.content = first.content.replace(RE_TASK, ""); }
    });
    i = close;
  }
}

/** The schema, the Markdown parser and serializer: built once per page,
 *  when the first editor mounts. */
class MarkdownKit {
  static #page: MarkdownKit | null = null;
  static get(): MarkdownKit { return (MarkdownKit.#page ??= new MarkdownKit()); }

  readonly schema: Schema;
  readonly parser: MarkdownParser;
  readonly serializer: MarkdownSerializer;

  constructor() {
    const schema = this.schema = new Schema({ nodes: NODES, marks: MARKS });

    // markdown-it, GFM tables and strikethrough, no raw HTML, plus sigil's
    // rule turning "- [ ] x" lists into task lists.
    const md = markdownit("default", { html: false });
    md.core.ruler.push("task_lists", taskListRule);

    this.parser = new MarkdownParser(schema, md, {
      blockquote: { block: "blockquote" },
      paragraph: { block: "paragraph" },
      list_item: { block: "list_item" },
      bullet_list: { block: "bullet_list", getAttrs: (_tok, toks, i) => ({ tight: tight(toks, i) }) },
      ordered_list: { block: "ordered_list", getAttrs: (tok, toks, i) => ({
        order: +(tok.attrGet("start") ?? "") || 1, tight: tight(toks, i)
      }) },
      heading: { block: "heading", getAttrs: (tok) => ({ level: +tok.tag.slice(1) }) },
      code_block: { block: "code_block", noCloseToken: true },
      fence: { block: "code_block", noCloseToken: true, getAttrs: (tok) => {
        const info = (tok.info || "").trim().split(/\s+/)[0];
        return { language: info || "plaintext" };
      } },
      hr: { node: "horizontal_rule" },
      image: { node: "image", getAttrs: (tok) => {
        const alt = tok.children && tok.children[0];
        return { src: tok.attrGet("src"), title: tok.attrGet("title") || "",
                 alt: (alt && alt.content) || "" };
      } },
      hardbreak: { node: "hard_break" },
      em: { mark: "em" },
      strong: { mark: "strong" },
      s: { mark: "strikethrough" },
      link: { mark: "link", getAttrs: (tok) => ({ href: tok.attrGet("href"), title: tok.attrGet("title") || "" }) },
      code_inline: { mark: "code", noCloseToken: true },
      task_list: { block: "task_list", getAttrs: (_tok, toks, i) => ({ tight: tight(toks, i) }) },
      task_item: { block: "task_item", getAttrs: (tok) => ({ checked: tok.attrGet("checked") === "true" }) },
      table: { block: "table" },
      thead: { ignore: true },
      tbody: { ignore: true },
      tr: { block: "table_row" },
      th: { block: "table_header" },
      td: { block: "table_cell" }
    });

    const dm = defaultMarkdownSerializer;
    type S = MarkdownSerializerState;
    this.serializer = new MarkdownSerializer({
      doc: (s: S, n: PMNode) => { s.renderContent(n); },
      paragraph: (s: S, n: PMNode) => { s.renderInline(n); s.closeBlock(n); },
      blockquote: (s: S, n: PMNode) => { s.wrapBlock("> ", null, n, () => { s.renderContent(n); }); },
      horizontal_rule: (s: S, n: PMNode) => { s.write("---"); s.closeBlock(n); },
      heading: (s: S, n: PMNode) => {
        s.write(new Array(Number(n.attrs["level"]) + 1).join("#") + " ");
        s.renderInline(n, false);
        s.closeBlock(n);
      },
      code_block: (s: S, n: PMNode) => {
        const runs = n.textContent.match(/`{3,}/gm);
        const fence = runs ? runs.sort().slice(-1)[0] + "`" : "```";
        const lang = n.attrs["language"] === "plaintext" ? "" : String(n.attrs["language"]);
        s.write(fence + lang + "\n");
        s.text(n.textContent, false);
        s.write("\n");
        s.write(fence);
        s.closeBlock(n);
      },
      bullet_list: (s: S, n: PMNode) => { s.renderList(n, "  ", () => "- "); },
      ordered_list: (s: S, n: PMNode) => {
        const start = Number(n.attrs["order"]) || 1;
        const width = String(start + n.childCount - 1).length;
        const space = new Array(width + 3).join(" ");
        s.renderList(n, space, (i) => {
          const nStr = String(start + i);
          return new Array(width - nStr.length + 1).join(" ") + nStr + ". ";
        });
      },
      list_item: (s: S, n: PMNode) => { s.renderContent(n); },
      task_list: (s: S, n: PMNode) => { s.renderList(n, "  ", () => "- "); },
      task_item: (s: S, n: PMNode) => {
        s.write(n.attrs["checked"] ? "[x] " : "[ ] ");
        s.renderContent(n);
      },
      image: dm.nodes["image"]!,
      hard_break: dm.nodes["hard_break"]!,
      text: dm.nodes["text"]!,
      table: (s: S, n: PMNode) => {
        n.forEach((row, _, i) => {
          const cells: string[] = [];
          row.forEach((cell) => { cells.push(this.cellMarkdown(cell)); });
          s.write("| " + cells.join(" | ") + " |\n");
          if (i === 0) {
            s.write("|" + cells.map(() => " --- ").join("|") + "|\n");
          }
        });
        s.closeBlock(n);
      },
      table_row: () => {},
      table_cell: () => {},
      table_header: () => {}
    }, {
      em: dm.marks["em"]!,
      strong: dm.marks["strong"]!,
      link: dm.marks["link"]!,
      code: dm.marks["code"]!,
      strikethrough: { open: "~~", close: "~~", mixable: true, expelEnclosingWhitespace: true }
    });
  }

  node(name: string): NodeType { return this.schema.nodes[name]!; }   // names of NODES
  mark(name: string): MarkType { return this.schema.marks[name]!; }   // names of MARKS

  parse(text: string): PMNode {
    const doc = text ? this.parser.parse(text) : null;
    return doc && doc.childCount ? doc
      : this.schema.node("doc", null, [this.schema.node("paragraph")]);
  }

  serialize(doc: PMNode): string { return this.serializer.serialize(doc); }

  // A cell's inline content as Markdown on one line (sigil wrote the
  // plain text; this keeps bold, links, code ...).
  private cellMarkdown(cell: PMNode): string {
    if (!cell.childCount) { return ""; }
    const doc = this.schema.node("doc", null, [this.schema.node("paragraph", null, cell.content)]);
    return this.serializer.serialize(doc).trim().replace(/\\\n/g, " ").replace(/\n/g, " ")
      .replace(/\|/g, "\\|");
  }
}

// ==================================================================
// Commands and keymaps (prose_editor/core.cljs, plugins/list_keys.cljs,
// plugins/table_keys.cljs, plugins/input_rules.cljs)
// ==================================================================

// Arrow keys leave a code block at its first / last line.
function codeExit(K: MarkdownKit, dir: -1 | 1): Command {
  return (state, dispatch, view) => {
    const sel = state.selection, $head = sel.$head;
    if (!sel.empty || !$head.parent.type.spec.code || !view ||
        !view.endOfTextblock(dir < 0 ? "up" : "down")) { return false; }
    const pos = dir < 0 ? $head.before() : $head.after();
    const $pos = state.doc.resolve(pos);
    if (dir < 0 ? $pos.nodeBefore : $pos.nodeAfter) {
      if (dispatch) {
        dispatch(state.tr.setSelection(Selection.near($pos, dir)).scrollIntoView());
      }
      return true;
    }
    if (dir > 0) { return exitCode(state, dispatch); }
    if (dispatch) {
      const para = K.node("paragraph").createAndFill();
      if (!para) { return true; }
      const tr = state.tr.insert(pos, para);
      dispatch(tr.setSelection(Selection.near(tr.doc.resolve(pos), 1)).scrollIntoView());
    }
    return true;
  };
}

// Alt-ArrowUp / Alt-ArrowDown: move the top-level block holding the
// selection (the keyboard counterpart of dragging the handle).
function moveBlockKey(dir: -1 | 1): Command {
  return (state, dispatch) => {
    const $from = state.selection.$from;
    if ($from.depth < 1) { return false; }
    const idx = $from.index(0), doc = state.doc;
    const target = idx + dir;
    if (target < 0 || target >= doc.childCount) { return false; }
    if (dispatch) {
      const from = $from.before(1), block = doc.child(idx);
      const other = doc.child(target);
      const tr = state.tr;
      const offset = $from.pos - from;
      if (dir < 0) {
        const to = from - other.nodeSize;
        tr.delete(from, from + block.nodeSize).insert(to, block);
        tr.setSelection(state.selection instanceof TextSelection
          ? TextSelection.create(tr.doc, to + offset) : Selection.near(tr.doc.resolve(to + 1)));
      } else {
        const dest = from + other.nodeSize;
        tr.delete(from, from + block.nodeSize).insert(dest, block);
        tr.setSelection(state.selection instanceof TextSelection
          ? TextSelection.create(tr.doc, dest + offset) : Selection.near(tr.doc.resolve(dest + 1)));
      }
      dispatch(tr.scrollIntoView());
    }
    return true;
  };
}

function markActive(state: EditorState, type: MarkType): boolean {
  const s = state.selection;
  return s.empty ? !!type.isInSet(state.storedMarks || s.$from.marks())
    : state.doc.rangeHasMark(s.from, s.to, type);
}

function linkPrompt(K: MarkdownKit, labels: Labels): Command {
  return (state, dispatch, view) => {
    const type = K.mark("link");
    if (state.selection.empty && !markActive(state, type)) { return false; }
    if (markActive(state, type)) { return toggleMark(type)(state, dispatch, view); }
    const href = window.prompt(labels["enter_url"] || AH.t("markdown_editor", "enter_url", "Enter URL:"));
    if (!href) { return true; }
    return toggleMark(type, { href: href })(state, dispatch, view);
  };
}

function coreKeymap(K: MarkdownKit, labels: Labels): Record<string, Command> {
  const hardBreak: Command = (state, dispatch) => {
    if (state.selection.$from.parent.type.spec.code) { return false; }
    if (dispatch) {
      dispatch(state.tr.replaceSelectionWith(K.node("hard_break").create()).scrollIntoView());
    }
    return true;
  };
  return {
    "Backspace": chainCommands(undoInputRule, deleteSelection, joinBackward,
                               selectNodeBackward),
    "Mod-z": undo, "Mod-y": redo, "Mod-Shift-z": redo,
    "Mod-b": toggleMark(K.mark("strong")),
    "Mod-i": toggleMark(K.mark("em")),
    "Mod-`": toggleMark(K.mark("code")),
    "Mod-Shift-x": toggleMark(K.mark("strikethrough")),
    "Mod-k": linkPrompt(K, labels),
    "Shift-Enter": hardBreak,
    "Enter": chainCommands(newlineInCode, createParagraphNear, liftEmptyBlock,
                           splitBlock),
    "Delete": chainCommands(deleteSelection, joinForward, selectNodeForward),
    "ArrowDown": codeExit(K, 1),
    "ArrowUp": codeExit(K, -1),
    "Alt-ArrowUp": moveBlockKey(-1),
    "Alt-ArrowDown": moveBlockKey(1)
  };
}

// plugins/list_keys.cljs
const LIST_TYPES = new Set(["bullet_list", "ordered_list", "task_list"]);

function listPlugins(K: MarkdownKit): Plugin[] {
  const li = K.node("list_item"), ti = K.node("task_item");
  function emptyItem(state: EditorState): boolean {
    const sel = state.selection, $f = sel.$from;
    return sel.empty && $f.parent.childCount === 0 &&
      ($f.depth <= 2 || $f.node($f.depth - 1).childCount === 1);
  }
  function atItemStart(state: EditorState): boolean {
    const sel = state.selection, $f = sel.$from;
    return sel.empty && $f.parentOffset === 0 && $f.depth >= 2 && $f.index($f.depth - 1) === 0;
  }
  function liftEmpty(type: NodeType): Command {
    return (state, dispatch) => emptyItem(state) && liftListItem(type)(state, dispatch);
  }
  function liftAtStart(type: NodeType): Command {
    return (state, dispatch) => atItemStart(state) && liftListItem(type)(state, dispatch);
  }
  const unwrapSingle: Command = (state, dispatch) => {
    const sel = state.selection, $f = sel.$from, d = $f.depth;
    if (!sel.empty || $f.parent.childCount !== 0 || d < 2) { return false; }
    for (let k = d - 1; k >= 1; k--) {
      const n = $f.node(k);
      if (LIST_TYPES.has(n.type.name)) {
        if (n.childCount !== 1) { return false; }
        const start = $f.before(k), end = $f.after(k);
        if (dispatch) {
          const tr = state.tr.replaceWith(start, end, K.node("paragraph").create());
          dispatch(tr.setSelection(Selection.near(tr.doc.resolve(start + 1))).scrollIntoView());
        }
        return true;
      }
    }
    return false;
  };
  // An empty paragraph right after a list: Backspace removes it and
  // puts the cursor at the end of the list (instead of joining it back).
  const deleteParaAfterList: Command = (state, dispatch) => {
    const sel = state.selection, $f = sel.$from;
    if (!sel.empty || $f.parentOffset !== 0 || $f.depth !== 1 || $f.parent.childCount !== 0 ||
        $f.index(0) === 0) { return false; }
    const prev = state.doc.child($f.index(0) - 1);
    if (!LIST_TYPES.has(prev.type.name)) { return false; }
    if (dispatch) {
      const from = $f.before(1), tr = state.tr.delete(from, $f.after(1));
      dispatch(tr.setSelection(Selection.near(tr.doc.resolve(Math.max(0, from - 1)), -1))
                 .scrollIntoView());
    }
    return true;
  };
  return [
    new Plugin({ props: { handleKeyDown: (view, e) => {
      if (e.key === "Backspace" && !e.shiftKey && !e.ctrlKey && !e.metaKey && !e.altKey &&
          deleteParaAfterList(view.state, view.dispatch)) {
        e.preventDefault();
        return true;
      }
      return false;
    } } }),
    keymap({
      "Enter": chainCommands(liftEmpty(ti), liftEmpty(li), unwrapSingle,
                             splitListItem(li), splitListItem(ti)),
      "Tab": chainCommands(sinkListItem(li), sinkListItem(ti)),
      "Shift-Tab": chainCommands(liftListItem(li), liftListItem(ti)),
      "Backspace": chainCommands(liftEmpty(li), liftEmpty(ti), liftAtStart(li),
                                 liftAtStart(ti), unwrapSingle)
    })
  ];
}

// plugins/table_keys.cljs: Tab / Shift-Tab between cells (Tab in the
// last cell adds a row), Enter a line break inside a cell.
function tablePlugins(K: MarkdownKit): Plugin[] {
  function nextCell(dir: -1 | 1): Command {
    return (state, dispatch, view) => {
      if (!isInTable(state)) { return false; }
      if (goToNextCell(dir)(state, dispatch)) { return true; }
      if (dir < 0 || !dispatch || !view) { return dir > 0; }
      addRowAfter(state, dispatch);
      goToNextCell(1)(view.state, view.dispatch);
      return true;
    };
  }
  return [
    keymap({
      "Tab": nextCell(1),
      "Shift-Tab": nextCell(-1),
      "Enter": (state, dispatch) => {
        if (!isInTable(state)) { return false; }
        if (dispatch) { dispatch(state.tr.replaceSelectionWith(K.node("hard_break").create())); }
        return true;
      }
    }),
    tableEditing()
  ];
}

// plugins/input_rules.cljs (without the maths rules), with a link rule
// and marks that keep the marks around them.
function inputRulesPlugin(K: MarkdownKit): Plugin {
  function markRule(re: RegExp, type: MarkType): InputRule {
    // re: group 1 the whole marked text with its delimiters, group 2 the text
    return new InputRule(re, (state, m, start, end) => {
      const text = m[2];
      if (!text) { return null; }
      const from = start + m[0].length - (m[1] ?? "").length;
      const $from = state.doc.resolve(from);
      if ($from.parent.type.spec.code) { return null; }
      const marks = type.create().addToSet($from.marks());
      return state.tr.replaceWith(from, end, K.schema.text(text, marks))
        .removeStoredMark(type);
    });
  }
  function taskRule(re: RegExp, checked: boolean): InputRule {
    return new InputRule(re, (state, _m, start, end) => {
      const tr = state.tr.delete(start, end);
      const range = tr.doc.resolve(start).blockRange();
      const wrap = range && findWrapping(range, K.node("task_list"));
      if (!range || !wrap) { return null; }
      wrap[wrap.length - 1] = { type: K.node("task_item"), attrs: { checked: checked } };
      return tr.wrap(range, wrap);
    });
  }
  function replaceText(re: RegExp, text: string, keep?: number): InputRule {
    return new InputRule(re, (state, m, start, end) =>
      state.tr.insertText(text, start + (keep ? m[0].length - keep : 0), end));
  }
  return inputRules({ rules: [
    textblockTypeInputRule(/^(#{1,6})\s$/, K.node("heading"),
                           (m) => ({ level: (m[1] ?? "").length })),
    wrappingInputRule(/^\s*>\s$/, K.node("blockquote")),
    wrappingInputRule(/^\s*[-*+]\s$/, K.node("bullet_list")),
    wrappingInputRule(/^\s*(\d+)\.\s$/, K.node("ordered_list"),
                      (m) => ({ order: +(m[1] ?? "") }),
                      (m, n) => n.childCount + Number(n.attrs["order"]) === +(m[1] ?? "")),
    textblockTypeInputRule(/^```(\w*)\s$/, K.node("code_block"),
                           (m) => ({ language: m[1] || "plaintext" })),
    new InputRule(/^(---|___|\*\*\*)\s$/, (state, m, start) => {
      const $s = state.doc.resolve(start);
      if ($s.parent.type.name !== "paragraph" || $s.parentOffset !== 0) { return null; }
      const from = $s.before(), to = $s.after();
      if ($s.parent.textContent.length !== m[0].length - 1) { return null; }
      const tr = state.tr.replaceWith(from, to, [K.node("horizontal_rule").create(),
                                                 K.node("paragraph").create()]);
      return tr.setSelection(TextSelection.create(tr.doc, from + 2));
    }),
    taskRule(/^\s*\[\s?\]\s$/, false),
    taskRule(/^\s*\[[xX]\]\s$/, true),
    markRule(/(\*\*([^\s*](?:[^*]*[^\s*])?)\*\*)$/, K.mark("strong")),
    markRule(/(?:^|[^*])(\*([^\s*](?:[^*]*[^\s*])?)\*)$/, K.mark("em")),
    markRule(/(?:^|[^_\w])(_([^\s_](?:[^_]*[^\s_])?)_)$/, K.mark("em")),
    markRule(/(`([^`]+)`)$/, K.mark("code")),
    markRule(/(~~([^\s~](?:[^~]*[^\s~])?)~~)$/, K.mark("strikethrough")),
    new InputRule(/\[([^\]]+)\]\(([^)\s]+)(?:\s+"([^"]*)")?\)$/, (state, m, start, end) => {
      const $s = state.doc.resolve(start);
      const link = K.mark("link").create({ href: m[2], title: m[3] || "" });
      return state.tr.replaceWith(start, end, K.schema.text(m[1] ?? "", link.addToSet($s.marks())));
    }),
    // typography (sigil): -- after a word, ..., smart double quotes
    replaceText(/[^\s-]--$/, "\u2014", 2),
    replaceText(/\.\.\.$/, "\u2026"),
    replaceText(/(?:^|[\s({\[])"$/, "\u201C", 1),
    replaceText(/[^\s({\[]"$/, "\u201D", 1)
  ] });
}

// plugins/task_list.cljs: a clickable checkbox for task items.
function taskItemView(n: PMNode, view: EditorView, getPos: () => number | undefined): NodeView {
  const li = document.createElement("li"), cb = document.createElement("input");
  const content = document.createElement("span");
  li.className = "task-list-item";
  cb.type = "checkbox";
  cb.className = "task-list-item-checkbox";
  cb.checked = !!n.attrs["checked"];
  cb.contentEditable = "false";
  cb.disabled = !view.editable;
  content.className = "task-list-item-content";
  li.appendChild(cb);
  li.appendChild(content);
  const onChange = (e: Event): void => {
    e.stopPropagation();
    const pos = getPos();
    if (typeof pos === "number") {
      view.dispatch(view.state.tr.setNodeMarkup(pos, null, { checked: cb.checked }));
    }
  };
  const onDown = (e: Event): void => { e.stopPropagation(); };
  cb.addEventListener("change", onChange);
  cb.addEventListener("mousedown", onDown);
  return {
    dom: li, contentDOM: content,
    update: (m: PMNode) => {
      if (m.type !== n.type) { return false; }
      cb.checked = !!m.attrs["checked"];
      return true;
    },
    stopEvent: (e: Event) => e.target === cb,
    ignoreMutation: (mu: ViewMutationRecord) => mu.target === cb,
    destroy: () => {
      cb.removeEventListener("change", onChange);
      cb.removeEventListener("mousedown", onDown);
    }
  };
}

// plugins/clipboard.cljs: pasted or dropped images become data URLs;
// plain text that looks like Markdown is parsed (HTML is left to
// ProseMirror's own clipboard parser).
const LOOKS_MD = /^(#{1,6}\s|[-*+]\s|\d+\.\s|>\s|```|---|\*\*|__|~~|\[.+\]\(.+\))/m;

function clipboardPlugin(K: MarkdownKit): Plugin {
  function images(dt: DataTransfer | null): File[] {
    return Array.from((dt && dt.files) || []).filter((f) => /^image\//.test(f.type));
  }
  function insertImages(view: EditorView, files: File[], pos: number | null): void {
    files.forEach((f) => {
      const r = new FileReader();
      r.onload = () => {
        const at = pos === null ? view.state.selection.from : pos;
        view.dispatch(view.state.tr.insert(at, K.node("image").create({ src: r.result })));
      };
      r.readAsDataURL(f);
    });
  }
  return new Plugin({ props: {
    handlePaste: (view, e) => {
      const dt = e.clipboardData, files = images(dt);
      if (files.length) { insertImages(view, files, null); return true; }
      if (!dt || dt.getData("text/html")) { return false; }
      const text = dt.getData("text/plain");
      if (!text || view.state.selection.$from.parent.type.spec.code || !LOOKS_MD.test(text)) {
        return false;
      }
      const doc = K.parse(text);
      view.dispatch(view.state.tr.replaceSelection(new Slice(doc.content, 0, 0)).scrollIntoView());
      return true;
    },
    handleDrop: (view, e) => {
      const files = images(e.dataTransfer);
      if (!files.length) { return false; }
      e.preventDefault();
      const at = view.posAtCoords({ left: e.clientX, top: e.clientY });
      insertImages(view, files, at ? at.pos : null);
      return true;
    }
  } });
}

// plugins/placeholder.cljs: the text of an empty document.
function placeholderPlugin(text: string): Plugin {
  return new Plugin({ props: { decorations: (state) => {
    const doc = state.doc, first = doc.firstChild;
    if (doc.childCount !== 1 || !first || !first.isTextblock || first.type.name !== "paragraph" ||
        first.childCount) { return null; }
    return DecorationSet.create(doc, [Decoration.widget(1, () => {
      const s = document.createElement("span");
      s.className = "ah-pm-placeholder";
      s.setAttribute("contenteditable", "false");
      s.setAttribute("aria-hidden", "true");
      s.textContent = text;
      return s;
    }, { key: "placeholder", side: -1 })]);
  } } });
}

// The block an item stands for.
function blockFor(K: MarkdownKit, type: string, level: number | null): PMNode {
  const p = (): PMNode => K.node("paragraph").create();
  switch (type) {
    case "heading": return K.node("heading").create({ level: level || 1 });
    case "code_block": return K.node("code_block").create({ language: "plaintext" });
    case "blockquote": return K.node("blockquote").create(null, [p()]);
    case "bullet_list":
    case "ordered_list":
      return K.node(type).create(null, [K.node("list_item").create(null, [p()])]);
    case "task_list":
      return K.node("task_list").create(null, [K.node("task_item").create(null, [p()])]);
    case "table": {
      const row = (t: string): PMNode =>
        K.node("table_row").create(null, [0, 1, 2].map(() => K.node(t).create()));
      return K.node("table").create(null, [row("table_header"), row("table_cell")]);
    }
    case "horizontal_rule": return K.node("horizontal_rule").create();
    default: return p();
  }
}

// ==================================================================
// Block handle and drag helpers
// ==================================================================

// The top-level block (a child of view.dom) holding a DOM node.
function topBlock(view: EditorView, start: Node | null): HTMLElement | null {
  let dom = start;
  while (dom && dom.parentNode !== view.dom) {
    if (dom === view.dom || !view.dom.contains(dom)) { return null; }
    dom = dom.parentNode;
  }
  return dom instanceof HTMLElement ? dom : null;
}

function posOf(view: EditorView, dom: Node): number | null {
  try { return view.posAtDOM(dom, 0); } catch (_e) { return null; }
}

// Move the top-level block at `pos' to the block boundary `to'
// (drag.cljs move-block!).
function moveBlock(view: EditorView, pos: number, to: number): void {
  const doc = view.state.doc, $p = doc.resolve(pos);
  if ($p.depth < 1) { return; }
  const from = $p.before(1), end = $p.after(1), blk = $p.node(1);
  if (to >= from && to <= end) { return; }
  const tr = view.state.tr;
  if (to < from) { tr.delete(from, end).insert(to, blk); }
  else { tr.insert(to, blk).delete(from, end); }
  view.dispatch(tr);
}

// ==================================================================
// Stats (plugins/stats_footer.cljs, stats.cljs)
// ==================================================================

const CJK = /[\u4e00-\u9fff\u3400-\u4dbf\uf900-\ufaff]/g;

function stats(doc: PMNode): EditorStats {
  const text = doc.textBetween(0, doc.content.size, "\n");
  const blank = !/\S/.test(text);
  let paragraphs = 0;
  doc.descendants((n) => {
    if (n.type.name === "paragraph" || n.type.name === "heading") { paragraphs++; }
  });
  return {
    chars: blank ? 0 : Array.from(text).length,
    words: blank ? 0 : (text.match(CJK) || []).length +
      text.replace(CJK, " ").split(/\s+/).filter(Boolean).length,
    paragraphs: paragraphs
  };
}

function labelsOf(el: Element): Labels {
  let data: unknown;
  try { data = JSON.parse(el.getAttribute("data-ah-labels") || "{}"); } catch (_e) { return {}; }
  const out: Labels = {};
  if (data && typeof data === "object") {
    Object.entries(data).forEach(([k, v]) => { if (typeof v === "string") { out[k] = v; } });
  }
  return out;
}

// ==================================================================
// Slash menu (plugins/slash_menu.cljs + slash_menu/{view,commands}.cljs)
// ==================================================================

class SlashMenu {
  #open = false;
  #pos: number | null = null;
  #float: FloatHandle | null = null;
  readonly #session: EditorSession;
  readonly #menu: HTMLElement;
  readonly #caret: HTMLElement;
  readonly #list: HTMLElement;

  constructor(session: EditorSession, menu: HTMLElement, caret: HTMLElement, list: HTMLElement) {
    this.#session = session;
    this.#menu = menu;
    this.#caret = caret;
    this.#list = list;
  }

  /** The menu of the root, or null when the server rendered none (the
   *  caret and the list are always inside it). */
  static create(session: EditorSession): SlashMenu | null {
    const el = session.el;
    const menu = el.querySelector<HTMLElement>(".ah-pm-slash-menu");
    if (!menu) { return null; }
    return new SlashMenu(session, menu, el.querySelector<HTMLElement>(".ah-md-editor-caret")!,
                         menu.querySelector<HTMLElement>(".ah-pm-slash-menu-content")!);
  }

  private items(): HTMLElement[] {
    return Array.from(this.#menu.querySelectorAll<HTMLElement>(".ah-pm-slash-menu-item"));
  }

  private selected(): number {
    return Math.max(0, this.items().findIndex((it) => it.classList.contains("selected")));
  }

  private select(i: number): void {
    const all = this.items(), n = all.length, list = this.#list;
    const it = all[(i + n) % n];
    all.forEach((x) => {
      x.classList.remove("selected");
      x.setAttribute("aria-selected", "false");
    });
    if (!it) { return; }
    it.classList.add("selected");
    it.setAttribute("aria-selected", "true");
    // scroll the list, not the page
    const top = it.offsetTop - list.offsetTop;
    if (top < list.scrollTop) { list.scrollTop = top; }
    else if (top + it.offsetHeight > list.scrollTop + list.clientHeight) {
      list.scrollTop = top + it.offsetHeight - list.clientHeight;
    }
    const g = it.closest(".ah-pm-slash-menu-group");
    const group = g ? g.getAttribute("data-group") : null;
    this.#menu.querySelectorAll(".ah-pm-slash-menu-tab").forEach((tab) => {
      tab.classList.toggle("active", tab.getAttribute("data-group") === group);
    });
    if (this.#session.view) { this.#session.view.dom.setAttribute("aria-activedescendant", it.id); }
  }

  show(pos: number): void {
    const view = this.#session.view, caret = this.#caret;
    if (!view) { return; }
    let c: { left: number; right: number; top: number; bottom: number };
    try { c = view.coordsAtPos(pos); } catch (_e) { return; }
    const content = caret.offsetParent || caret.parentElement;
    if (!content) { return; }
    const r = content.getBoundingClientRect();
    caret.style.left = (c.left - r.left + content.scrollLeft) + "px";
    caret.style.top = (c.top - r.top + content.scrollTop) + "px";
    caret.style.height = Math.max(1, c.bottom - c.top) + "px";
    this.#open = true;
    this.#pos = pos;
    this.#menu.classList.add("ah-pm-slash-menu--visible");
    this.#list.scrollTop = 0;
    this.select(0);
    if (this.#float) { this.#float.stop(); }
    this.#float = AH.float(this.#menu, caret, { placement: "bottom", align: "start", offset: 4 });
    view.dom.setAttribute("aria-expanded", "true");
  }

  hide(): void {
    if (!this.#open) { return; }
    this.#open = false;
    this.#pos = null;
    this.#menu.classList.remove("ah-pm-slash-menu--visible");
    if (this.#float) { this.#float.stop(); this.#float = null; }
    const view = this.#session.view;
    if (view) {
      view.dom.setAttribute("aria-expanded", "false");
      view.dom.removeAttribute("aria-activedescendant");
    }
  }

  private confirm(it: HTMLElement | null | undefined): void {
    if (!it || this.#pos === null) { return; }
    const pos = this.#pos;
    this.hide();
    this.#session.insertBlock(it.getAttribute("data-type") || "", +(it.getAttribute("data-level") ?? "") || null, pos);
  }

  plugin(): Plugin {
    const menu = this.#menu, list = this.#list;
    return new Plugin({
      view: () => {
        const off = new AbortController();
        menu.addEventListener("mousedown", (e) => {
          e.preventDefault();
          const t = e.target instanceof Element ? e.target : null;
          const tab = t && t.closest(".ah-pm-slash-menu-tab");
          if (tab) {
            const g = Array.from(menu.querySelectorAll<HTMLElement>(".ah-pm-slash-menu-group"))
              .find((x) => x.getAttribute("data-group") === tab.getAttribute("data-group"));
            if (g) {
              list.scrollTop = g.offsetTop - list.offsetTop;
              const first = g.querySelector<HTMLElement>(".ah-pm-slash-menu-item");
              this.select(first ? this.items().indexOf(first) : -1);
            }
            return;
          }
          this.confirm(t && t.closest<HTMLElement>(".ah-pm-slash-menu-item"));
        }, { signal: off.signal });
        document.addEventListener("mousedown", (e) => {
          if (this.#open && !(e.target instanceof Node && menu.contains(e.target))) { this.hide(); }
        }, { signal: off.signal });
        return {
          // "/" typed into an empty top-level paragraph opens the menu.
          // Read from the document rather than handleTextInput, which
          // ProseMirror skips when the browser rewrote the empty block.
          update: (view: EditorView, prev: EditorState) => {
            if (this.#open || !prev || prev.doc.eq(view.state.doc)) { return; }
            const sel = view.state.selection, $f = sel.$from;
            if (!sel.empty || $f.depth !== 1 || $f.parent.type.name !== "paragraph" ||
                $f.parent.textContent !== "/" || $f.parentOffset !== 1) { return; }
            const i = $f.index(0), was = prev.doc.childCount === view.state.doc.childCount &&
                prev.doc.child(i);
            if (was && was.type.name === "paragraph" && was.childCount === 0) { this.show(sel.from); }
          },
          destroy: () => {
            this.hide();
            off.abort();
          }
        };
      },
      props: {
        handleKeyDown: (_view, e) => {
          if (!this.#open) { return false; }
          switch (e.key) {
            case "ArrowDown": this.select(this.selected() + 1); e.preventDefault(); return true;
            case "ArrowUp": this.select(this.selected() - 1); e.preventDefault(); return true;
            case "Enter": this.confirm(this.items()[this.selected()]); e.preventDefault(); return true;
            case "Escape": this.hide(); e.preventDefault(); return true;
            case "Shift": case "Control": case "Alt": case "Meta": return false;
            default: this.hide(); return false;
          }
        }
      }
    });
  }
}

// ==================================================================
// Block handle and drag (plugins/handle.cljs, plugins/drag.cljs)
// ==================================================================

class BlockHandle {
  #timer: ReturnType<typeof setTimeout> | null = null;
  /** The top-level block the handle stands next to. */
  #block: HTMLElement | null = null;
  readonly #session: EditorSession;
  readonly #handle: HTMLElement;
  readonly #content: HTMLElement;

  constructor(session: EditorSession, handle: HTMLElement, content: HTMLElement) {
    this.#session = session;
    this.#handle = handle;
    this.#content = content;
  }

  static create(session: EditorSession): BlockHandle | null {
    const handle = session.el.querySelector<HTMLElement>(".ah-pm-block-handle");
    const content = handle && handle.parentElement;
    return handle && content ? new BlockHandle(session, handle, content) : null;
  }

  get block(): HTMLElement | null { return this.#block; }

  private cancel(): void {
    if (this.#timer !== null) { clearTimeout(this.#timer); }
    this.#timer = null;
  }

  private show(dom: HTMLElement): void {
    const handle = this.#handle, content = this.#content;
    this.cancel();
    const r = dom.getBoundingClientRect(), c = content.getBoundingClientRect();
    const lh = parseFloat(getComputedStyle(dom).lineHeight) || 24;
    const top = r.top - c.top + content.scrollTop + (Math.min(lh, r.height) - handle.offsetHeight) / 2;
    handle.style.top = Math.round(top) + "px";
    handle.classList.add("ah-pm-block-handle--visible");
    this.#block = dom;
  }

  private hideNow(): void {
    this.#handle.classList.remove("ah-pm-block-handle--visible");
    this.#block = null;
  }

  private hideLater(): void {
    this.cancel();
    this.#timer = setTimeout(() => { this.hideNow(); }, 150);
  }

  plugin(): Plugin {
    const handle = this.#handle, session = this.#session;
    return new Plugin({
      view: (view) => {
        const off = new AbortController(), opts = { signal: off.signal };
        const onAdd = (e: Event): boolean => e.target instanceof Element && !!e.target.closest(".ah-pm-block-handle-add");
        handle.addEventListener("mouseenter", () => {
          this.cancel();
          handle.classList.add("ah-pm-block-handle--visible");
        }, opts);
        handle.addEventListener("mouseleave", () => { this.hideLater(); }, opts);
        handle.addEventListener("mousedown", (e) => {
          if (onAdd(e)) { e.preventDefault(); }
        }, opts);
        handle.addEventListener("click", (e) => {
          if (!onAdd(e)) { return; }
          e.preventDefault();
          const v = session.view;
          const pos = this.#block && v ? posOf(v, this.#block) : null;
          if (pos === null || !v) { return; }
          const after = v.state.doc.resolve(pos).after(1);
          const tr = v.state.tr.insert(after, session.kit.node("paragraph").create());
          tr.setSelection(TextSelection.create(tr.doc, after + 1));
          v.dispatch(tr.scrollIntoView());
          v.focus();
          const menu = session.menu;
          if (menu) {
            requestAnimationFrame(() => { if (session.view) { menu.show(after + 1); } });
          }
        }, opts);
        return {
          update: () => {
            if (this.#block && !view.dom.contains(this.#block)) { this.hideNow(); }
          },
          destroy: () => { this.cancel(); this.hideNow(); off.abort(); }
        };
      },
      props: { handleDOMEvents: {
        mousemove: (view, e) => {
          if (session.drag && session.drag.dragging) { return false; }
          const b = e.target === view.dom || !(e.target instanceof Node) ? null : topBlock(view, e.target);
          if (!b) { this.hideLater(); }
          else if (b !== this.#block) { this.show(b); }
          return false;
        },
        mouseleave: () => { this.hideLater(); return false; }
      } }
    });
  }
}

class BlockDrag {
  #block: HTMLElement | null = null;
  #pos: number | null = null;
  #clone: HTMLElement | null = null;
  readonly #session: EditorSession;
  readonly #indicator: HTMLElement;
  readonly #content: HTMLElement;

  constructor(session: EditorSession, indicator: HTMLElement, content: HTMLElement) {
    this.#session = session;
    this.#indicator = indicator;
    this.#content = content;
  }

  static create(session: EditorSession): BlockDrag | null {
    const indicator = session.el.querySelector<HTMLElement>(".ah-pm-drag-indicator");
    const content = indicator && indicator.parentElement;
    return indicator && content ? new BlockDrag(session, indicator, content) : null;
  }

  /** A block is being dragged. */
  get dragging(): boolean { return this.#block !== null; }

  // The document position a drop at clientY lands on (a boundary between
  // top-level blocks) and the y of that boundary.
  private static dropAt(view: EditorView, y: number): { pos: number; y: number } | null {
    const kids = view.dom.children;
    let last: { pos: number; y: number } | null = null;
    for (const kid of Array.from(kids)) {
      const r = kid.getBoundingClientRect();
      const p = posOf(view, kid);
      if (p === null) { continue; }
      if (y < r.top + r.height / 2) {
        return { pos: view.state.doc.resolve(p).before(1), y: r.top };
      }
      last = { pos: view.state.doc.resolve(p).after(1), y: r.bottom };
    }
    return last;
  }

  private removeClone(): void { if (this.#clone) { this.#clone.remove(); this.#clone = null; } }

  private end(): void {
    if (this.#block) { this.#block.classList.remove("ah-pm-dragging"); }
    this.#indicator.style.display = "none";
    this.removeClone();
    this.#block = this.#pos = null;
  }

  plugin(): Plugin {
    const session = this.#session, indicator = this.#indicator, content = this.#content;
    return new Plugin({
      view: (view) => {
        const off = new AbortController();
        let moving: AbortController | null = null;
        session.el.addEventListener("mousedown", (e) => {
          const t = e.target instanceof Element ? e.target : null;
          if (!t || !t.closest(".ah-pm-block-handle-drag")) { return; }
          const b = session.handle ? session.handle.block : null, p = b && posOf(view, b);
          if (!b || p === null || e.button !== 0) { return; }
          e.preventDefault();
          this.#block = b;
          this.#pos = p;
          b.classList.add("ah-pm-dragging");
          // a copy of an HTMLElement is an HTMLElement
          const r = b.getBoundingClientRect(), c = b.cloneNode(true) as HTMLElement;
          c.classList.add("ah-pm-drag-clone");
          Object.assign(c.style, { position: "fixed", opacity: "0.6", pointerEvents: "none", zIndex: "1000",
                                   width: r.width + "px", transform: "rotate(1deg)", margin: "0",
                                   boxShadow: "0 4px 16px var(--ah-color-bg-overlay)",
                                   left: (e.clientX - 16) + "px", top: (e.clientY - 16) + "px" });
          document.body.appendChild(c);
          this.#clone = c;
          if (moving) { moving.abort(); }
          const mover = moving = new AbortController();
          const mv = { signal: typeof AbortSignal.any === "function"
            ? AbortSignal.any([mover.signal, off.signal]) : mover.signal };
          document.addEventListener("mousemove", (ev) => {
            const at = BlockDrag.dropAt(view, ev.clientY), cr = content.getBoundingClientRect();
            if (at) {
              indicator.style.top = (at.y - cr.top + content.scrollTop - 1) + "px";
              indicator.style.display = "block";
            }
            if (this.#clone) {
              this.#clone.style.left = (ev.clientX - 16) + "px";
              this.#clone.style.top = (ev.clientY - 16) + "px";
            }
          }, mv);
          document.addEventListener("mouseup", (ev) => {
            mover.abort();
            moving = null;
            const from = this.#pos, at = BlockDrag.dropAt(view, ev.clientY);
            this.end();
            if (from !== null && at) { moveBlock(view, from, at.pos); }
          }, mv);
        }, { signal: off.signal });
        return { destroy: () => {
          this.end();
          if (moving) { moving.abort(); moving = null; }
          off.abort();
        } };
      }
    });
  }
}

// ==================================================================
// Stats footer (plugins/stats_footer.cljs)
// ==================================================================

class StatsFooter {
  #timer: ReturnType<typeof setTimeout> | null = null;
  readonly #session: EditorSession;
  readonly #foot: HTMLElement;
  readonly #max: number;

  constructor(session: EditorSession, foot: HTMLElement) {
    this.#session = session;
    this.#foot = foot;
    this.#max = +(session.el.getAttribute("data-ah-max-chars") ?? "") || 0;
  }

  static create(session: EditorSession): StatsFooter | null {
    const foot = session.el.querySelector<HTMLElement>(".ah-pm-stats");
    return foot ? new StatsFooter(session, foot) : null;
  }

  private fill(doc: PMNode): void {
    const s = stats(doc), foot = this.#foot, max = this.#max;
    const put = (name: string, text: string | number): void => {
      foot.querySelectorAll('[data-stat="' + name + '"]').forEach((n) => { n.textContent = String(text); });
    };
    put("chars", s.chars);
    put("words", s.words);
    put("paragraphs", s.paragraphs);
    if (max) {
      const over = s.chars > max;
      put("limit", s.chars + "/" + max);
      foot.querySelectorAll('[data-stat="limit"]').forEach((n) => {
        n.classList.toggle("ah-pm-stats__value--warning", over);
      });
      foot.querySelectorAll<HTMLElement>(".ah-pm-stats__warning").forEach((n) => { n.hidden = !over; });
      this.#session.el.classList.toggle("ah-md-editor-over", over);
    }
  }

  private clear(): void {
    if (this.#timer !== null) { clearTimeout(this.#timer); }
    this.#timer = null;
  }

  plugin(): Plugin {
    return new Plugin({ view: (view) => {
      this.fill(view.state.doc);
      return {
        update: (v: EditorView, prev: EditorState) => {
          if (prev && prev.doc.eq(v.state.doc)) { return; }
          this.clear();
          this.#timer = setTimeout(() => {
            const cur = this.#session.view;
            if (cur) { this.fill(cur.state.doc); }
          }, 100);
        },
        destroy: () => { this.clear(); }
      };
    } });
  }
}

// ==================================================================
// One mounted editor
// ==================================================================

const MARK_COMMANDS = new Map<string, string>([
  ["bold", "strong"], ["italic", "em"], ["code", "code"], ["strikethrough", "strikethrough"]
]);

class EditorSession {
  readonly el: HTMLElement;
  readonly kit: MarkdownKit;
  readonly labels: Labels;
  /** The server's <textarea>: the form field. */
  readonly ta: HTMLTextAreaElement | null;
  /** The value of the last `change'. */
  committed: string;
  view: EditorView | null = null;
  menu: SlashMenu | null = null;
  handle: BlockHandle | null = null;
  drag: BlockDrag | null = null;
  #place: HTMLElement | null = null;

  constructor(el: HTMLElement) {
    this.el = el;
    this.kit = MarkdownKit.get();
    this.labels = labelsOf(el);
    this.ta = el.querySelector<HTMLTextAreaElement>(".ah-md-editor-source");
    this.committed = el.getAttribute("data-ah-value") || "";
  }

  // Value contract (designs/04-components.md): data-ah-value, the
  // textarea (the form field), `input' now, `change' when committed.
  publish(md: string): void {
    this.el.setAttribute("data-ah-value", md);
    if (this.ta && this.ta.value !== md) { this.ta.value = md; }
    fire(this.el, "input");
  }

  commit(): void {
    const v = this.el.getAttribute("data-ah-value") || "";
    if (v !== this.committed) {
      this.committed = v;
      fire(this.el, "change");
    }
  }

  // The document has the focus (the slash menu and the handle keep it
  // there; a task checkbox takes it).
  focused(): boolean {
    return !!this.view && document.activeElement === this.view.dom;
  }

  private onDocChanged(view: EditorView): void {
    this.publish(this.kit.serialize(view.state.doc));
    if (!this.focused()) { this.commit(); }
  }

  setDoc(md: string): void {
    const view = this.view;
    if (!view) { return; }
    view.updateState(EditorState.create({
      schema: this.kit.schema, doc: this.kit.parse(md), plugins: view.state.plugins
    }));
  }

  mount(): void {
    const K = this.kit, el = this.el;
    const content = el.querySelector(".ah-pm-content") || el;
    const editable = !el.hasAttribute("data-ah-readonly") && el.getAttribute("aria-disabled") !== "true";
    const place = document.createElement("div");
    content.insertBefore(place, content.firstChild);
    this.#place = place;

    const plugins: (Plugin | null)[] = [];
    if (editable) {
      this.menu = SlashMenu.create(this);
      this.handle = BlockHandle.create(this);
      this.drag = BlockDrag.create(this);
      plugins.push(this.menu && this.menu.plugin(), inputRulesPlugin(K));
      plugins.push(...listPlugins(K), ...tablePlugins(K));
      plugins.push(clipboardPlugin(K), placeholderPlugin(el.getAttribute("data-ah-placeholder") || ""),
                   this.handle && this.handle.plugin(), this.drag && this.drag.plugin());
    }
    const footer = StatsFooter.create(this);
    plugins.push(footer && footer.plugin());
    if (editable) {
      plugins.push(keymap(coreKeymap(K, this.labels)), keymap(baseKeymap),
                   history(), dropCursor(), gapCursor());
    }

    const attrs: Record<string, string> = { role: "textbox", "aria-multiline": "true",
                                            "aria-label": this.labels["editor"] ||
                                              AH.t("markdown_editor", "editor", "Markdown editor") };
    if (!editable) { attrs["aria-readonly"] = "true"; }
    if (el.getAttribute("aria-disabled") === "true") { attrs["aria-disabled"] = "true"; }
    if (this.menu || el.querySelector(".ah-pm-slash-menu")) {
      const list = el.querySelector(".ah-pm-slash-menu-content");
      attrs["aria-haspopup"] = "listbox";
      attrs["aria-expanded"] = "false";
      if (list) { attrs["aria-controls"] = list.id; }
    }
    const ph = el.getAttribute("data-ah-placeholder");
    if (ph && editable) { attrs["aria-placeholder"] = ph; }

    // The textarea is the editor until this code has loaded: start from
    // what it holds, so nothing typed there is lost.
    const md = this.ta ? this.ta.value : el.getAttribute("data-ah-value") || "";
    const view = new EditorView({ mount: place }, {
      state: EditorState.create({
        schema: K.schema, doc: K.parse(md), plugins: plugins.filter((p): p is Plugin => p !== null)
      }),
      editable: () => editable,
      attributes: attrs,
      nodeViews: { task_item: taskItemView },
      dispatchTransaction: (tr: Transaction) => {
        const v = this.view || view;
        v.updateState(v.state.apply(tr));
        if (tr.docChanged) { this.onDocChanged(v); }
      }
    });
    this.view = view;
    // ProseMirror's own input/change events are not the component's
    view.dom.addEventListener("input", swallow);
    view.dom.addEventListener("change", swallow);
    if (this.ta) { this.ta.hidden = true; }
    el.classList.add("ah-md-editor-ready");
    if (md !== (el.getAttribute("data-ah-value") || "")) { this.publish(md); }
  }

  unmount(): void {
    const view = this.view;
    if (view) {
      view.dom.removeEventListener("input", swallow);
      view.dom.removeEventListener("change", swallow);
      view.destroy();
    }
    this.view = null;
    if (this.#place) { this.#place.remove(); }
    this.#place = null;
    if (this.ta) { this.ta.hidden = false; }
    this.el.classList.remove("ah-md-editor-ready", "ah-md-editor-over");
  }

  // Replace the (empty or "/") top-level block at `pos' with the item's
  // block and put the cursor inside it.
  insertBlock(type: string, level: number | null, pos: number): void {
    const K = this.kit, view = this.view;
    if (!view) { return; }
    const $pos = view.state.doc.resolve(Math.min(pos, view.state.doc.content.size));
    if ($pos.depth < 1) { return; }
    const from = $pos.before(1), to = $pos.after(1);
    let block: PMNode;
    if (type === "image") {
      const src = window.prompt(this.labels["enter_image_url"] || AH.t("markdown_editor", "enter_image_url", "Image URL:"));
      view.focus();
      if (!src) { return; }
      block = K.node("paragraph").create(null, [K.node("image").create({ src: src })]);
    } else {
      block = blockFor(K, type, level);
    }
    const tr = view.state.tr.replaceWith(from, to, block);
    if (type === "horizontal_rule") {
      tr.insert(from + block.nodeSize, K.node("paragraph").create());
    }
    const target = type === "horizontal_rule" ? from + block.nodeSize + 1 : from + 1;
    const $t = tr.doc.resolve(target);
    tr.setSelection($t.parent.inlineContent ? TextSelection.create(tr.doc, target)
                    : Selection.near($t));
    view.dispatch(tr.scrollIntoView());
    view.focus();
  }

  // exec-command! (core.cljs): marks, blocks, undo / redo.
  exec(cmd: string, opts: ExecOptions): boolean {
    const K = this.kit, view = this.view;
    if (!view) { return false; }
    const run = (c: Command): boolean => c(view.state, view.dispatch, view);
    const markName = MARK_COMMANDS.get(cmd);
    if (markName) { return run(toggleMark(K.mark(markName))); }
    switch (cmd) {
      case "link":
        if (!opts.href) { return run(linkPrompt(K, this.labels)); }
        return run(toggleMark(K.mark("link"), { href: opts.href, title: opts.title || "" }));
      case "undo": return run(undo);
      case "redo": return run(redo);
      case "heading": return run(setBlockType(K.node("heading"), { level: +(opts.level ?? "") || 1 }));
      case "paragraph": return run(setBlockType(K.node("paragraph")));
      case "code_block":
        return run(setBlockType(K.node("code_block"), { language: opts.language || "plaintext" }));
      case "blockquote": return run(wrapIn(K.node("blockquote")));
      case "bullet_list":
      case "ordered_list":
      case "task_list":
        return run(wrapInList(K.node(cmd)));
      case "horizontal_rule":
        return run((state, dispatch) => {
          if (dispatch) { dispatch(state.tr.replaceSelectionWith(K.node("horizontal_rule").create())); }
          return true;
        });
      default:
        console.error("aihtml: unknown markdown editor command " + cmd);
        return false;
    }
  }
}

/** exec's options from the server: the fields it reads, as given. */
function execOptions(v: unknown): ExecOptions {
  const out: ExecOptions = {};
  if (!v || typeof v !== "object") { return out; }
  const o = v as Record<string, unknown>;
  if (o["href"]) { out.href = String(o["href"]); }
  if (o["title"]) { out.title = String(o["title"]); }
  if (typeof o["level"] === "number" || typeof o["level"] === "string") { out.level = o["level"]; }
  if (o["language"]) { out.language = String(o["language"]); }
  return out;
}

// ==================================================================
// The component
// ==================================================================

class MarkdownEditorController extends AH.Controller {
  // set in setup, before any method can run
  #s!: EditorSession;

  override setup(): void {
    const s = this.#s = new EditorSession(this.element);
    this.listen(this.element, "focusout", (e) => {
      if (s.view && e.target === s.view.dom) { s.commit(); }
    });
    s.mount();
  }

  override teardown(): void { this.#s.unmount(); }

  // The mounted view; the methods below run between setup and teardown.
  private get pm(): EditorView {
    const v = this.#s.view;
    if (!v) { throw new Error("aihtml: markdown editor not mounted"); }
    return v;
  }

  // methods (aihtml_action:call/4, AH.invoke)
  getValue(): string { return this.element.getAttribute("data-ah-value") || ""; }
  setValue(v: unknown): void {
    const s = this.#s;
    const md = v === null || v === undefined ? "" : String(v);
    this.element.setAttribute("data-ah-value", md);
    s.committed = md;
    if (s.ta) { s.ta.value = md; }
    s.setDoc(md);
  }
  getHtml(): string {
    const div = document.createElement("div");
    div.appendChild(DOMSerializer.fromSchema(this.#s.kit.schema)
                      .serializeFragment(this.pm.state.doc.content));
    return div.innerHTML;
  }
  getJson(): unknown { return this.pm.state.doc.toJSON(); }
  focus(): void { this.pm.focus(); }
  blur(): void {
    this.pm.dom.blur();
    if (!this.#s.focused()) { this.#s.commit(); }
  }
  exec(cmd: string, opts?: unknown): boolean { return this.#s.exec(cmd, execOptions(opts)); }
  stats(): EditorStats { return stats(this.pm.state.doc); }
  view(): EditorView | null { return this.#s.view; }
}

AH.register("markdown-editor", MarkdownEditorController);
