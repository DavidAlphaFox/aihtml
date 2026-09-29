/* Behaviour of the markdown editor (designs/04-components.md). Ported from
 * sigil: text/markdown_editor and the parts of text/prose_editor it uses
 * (schema, markdown, core keymap, plugins: input rules, list keys, table
 * keys, task list, clipboard, placeholder, slash menu, block handle, drag,
 * stats footer).
 *
 * ProseMirror and markdown-it are a chunk of their own
 * (assets/vendor/prosemirror.entry.js), loaded on demand with
 * AH.vendor("prosemirror"). Until it has loaded the root shows
 * the server's <textarea> with the Markdown source, which then stays in the
 * DOM, hidden, as the form field.
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
 * setValue changes the value silently. */
import AH from "../core.js";

var uid = 0;
var KIT = null;          // schema, parser, serializer: one per page

function fire(el, type) {
  el.dispatchEvent(new CustomEvent(type, { bubbles: true, cancelable: true }));
}

// ProseMirror's own input/change events are not the component's.
function swallow(e) { e.stopPropagation(); }

// ==================================================================
// Schema, Markdown parser and serializer (prose_editor/schema.cljs,
// prose_editor/markdown.cljs), limited to what Markdown can express
// ==================================================================

function buildKit(P) {
  var S = P.model;
  var nodes = {
    doc: { content: "block+" },
    paragraph: { content: "inline*", group: "block",
                 parseDOM: [{ tag: "p" }], toDOM: function () { return ["p", 0]; } },
    blockquote: { content: "block+", group: "block", defining: true,
                  parseDOM: [{ tag: "blockquote" }],
                  toDOM: function () { return ["blockquote", { "class": "ah-md-blockquote" }, 0]; } },
    horizontal_rule: { group: "block", parseDOM: [{ tag: "hr" }],
                       toDOM: function () { return ["hr", { "class": "ah-md-hr" }]; } },
    heading: { content: "inline*", group: "block", defining: true,
               attrs: { level: { default: 1 } },
               parseDOM: [1, 2, 3, 4, 5, 6].map(function (i) {
                 return { tag: "h" + i, attrs: { level: i } };
               }),
               toDOM: function (n) {
                 return ["h" + n.attrs.level, { "class": "ah-md-h" + n.attrs.level }, 0];
               } },
    code_block: { content: "text*", group: "block", marks: "", code: true, defining: true,
                  attrs: { language: { default: "plaintext" } },
                  parseDOM: [{ tag: "pre", preserveWhitespace: "full", getAttrs: function (dom) {
                    var c = dom.querySelector("code");
                    var lang = c && (c.getAttribute("data-language") ||
                                     (c.className || "").replace(/^language-/, ""));
                    return { language: lang || "plaintext" };
                  } }],
                  toDOM: function (n) {
                    var lang = n.attrs.language;
                    return ["pre", { "class": "ah-md-code" },
                            ["code", { "class": "language-" + lang, "data-language": lang }, 0]];
                  } },
    text: { group: "inline" },
    image: { inline: true, group: "inline", draggable: true,
             attrs: { src: {}, alt: { default: "" }, title: { default: "" } },
             parseDOM: [{ tag: "img[src]", getAttrs: function (dom) {
               return { src: dom.getAttribute("src"), alt: dom.getAttribute("alt") || "",
                        title: dom.getAttribute("title") || "" };
             } }],
             toDOM: function (n) {
               return ["img", { src: n.attrs.src, alt: n.attrs.alt, title: n.attrs.title || null,
                                "class": "ah-md-image" }];
             } },
    hard_break: { inline: true, group: "inline", selectable: false,
                  parseDOM: [{ tag: "br" }], toDOM: function () { return ["br"]; } },
    bullet_list: { content: "list_item+", group: "block", attrs: { tight: { default: true } },
                   parseDOM: [{ tag: "ul" }],
                   toDOM: function () { return ["ul", { "class": "ah-md-list" }, 0]; } },
    ordered_list: { content: "list_item+", group: "block",
                    attrs: { order: { default: 1 }, tight: { default: true } },
                    parseDOM: [{ tag: "ol", getAttrs: function (dom) {
                      var s = parseInt(dom.getAttribute("start"), 10);
                      return { order: isNaN(s) ? 1 : s };
                    } }],
                    toDOM: function (n) {
                      return n.attrs.order === 1 ? ["ol", { "class": "ah-md-list" }, 0]
                        : ["ol", { start: n.attrs.order, "class": "ah-md-list" }, 0];
                    } },
    list_item: { content: "paragraph block*", defining: true,
                 parseDOM: [{ tag: "li" }], toDOM: function () { return ["li", 0]; } },
    task_list: { content: "task_item+", group: "block", attrs: { tight: { default: true } },
                 parseDOM: [{ tag: "ul.ah-md-tasklist", priority: 60 }],
                 toDOM: function () { return ["ul", { "class": "ah-md-tasklist" }, 0]; } },
    task_item: { content: "paragraph block*", defining: true,
                 attrs: { checked: { default: false } },
                 parseDOM: [{ tag: "li.task-list-item", priority: 60, getAttrs: function (dom) {
                   var cb = dom.querySelector("input[type=checkbox]");
                   return { checked: !!(cb && cb.checked) };
                 } }],
                 toDOM: function (n) {
                   return ["li", { "class": "task-list-item" },
                           ["input", { type: "checkbox", "class": "task-list-item-checkbox",
                                       checked: n.attrs.checked ? "" : null }],
                           ["span", { "class": "task-list-item-content" }, 0]];
                 } },
    table: { content: "table_row+", group: "block", tableRole: "table", isolating: true,
             parseDOM: [{ tag: "table" }],
             toDOM: function () { return ["table", { "class": "ah-md-table" }, ["tbody", 0]]; } },
    table_row: { content: "(table_cell | table_header)*", tableRole: "row",
                 parseDOM: [{ tag: "tr" }], toDOM: function () { return ["tr", 0]; } },
    table_cell: cellSpec("td", "cell"),
    table_header: cellSpec("th", "header_cell")
  };
  function cellSpec(tag, role) {
    return { content: "inline*", tableRole: role, isolating: true,
             attrs: { colspan: { default: 1 }, rowspan: { default: 1 },
                      colwidth: { default: null } },
             parseDOM: [{ tag: tag, getAttrs: function (dom) {
               return { colspan: +dom.getAttribute("colspan") || 1,
                        rowspan: +dom.getAttribute("rowspan") || 1 };
             } }],
             toDOM: function () { return [tag, 0]; } };
  }
  var marks = {
    link: { attrs: { href: {}, title: { default: "" } }, inclusive: false,
            parseDOM: [{ tag: "a[href]", getAttrs: function (dom) {
              return { href: dom.getAttribute("href"), title: dom.getAttribute("title") || "" };
            } }],
            toDOM: function (m) {
              return ["a", { href: m.attrs.href, title: m.attrs.title || null,
                             "class": "ah-md-link" }, 0];
            } },
    em: { parseDOM: [{ tag: "i" }, { tag: "em" }, { style: "font-style=italic" }],
          toDOM: function () { return ["em", 0]; } },
    strong: { parseDOM: [{ tag: "strong" }, { tag: "b" },
                         { style: "font-weight", getAttrs: function (v) {
                           return /^(bold(er)?|[5-9]\d{2,})$/.test(v) && null;
                         } }],
              toDOM: function () { return ["strong", 0]; } },
    code: { parseDOM: [{ tag: "code" }],
            toDOM: function () { return ["code", { "class": "ah-md-inline-code" }, 0]; } },
    strikethrough: { parseDOM: [{ tag: "s" }, { tag: "del" }, { tag: "strike" },
                                { style: "text-decoration", getAttrs: function (v) {
                                  return /line-through/.test(v) && null;
                                } }],
                     toDOM: function () { return ["s", 0]; } }
  };
  var schema = new S.Schema({ nodes: nodes, marks: marks });

  // markdown-it, GFM tables and strikethrough, no raw HTML, plus sigil's
  // rule turning "- [ ] x" lists into task lists.
  var md = P.markdownit("default", { html: false });
  md.core.ruler.push("task_lists", taskListRule);

  var M = P.markdown;
  var parser = new M.MarkdownParser(schema, md, {
    blockquote: { block: "blockquote" },
    paragraph: { block: "paragraph" },
    list_item: { block: "list_item" },
    bullet_list: { block: "bullet_list", getAttrs: function (tok, toks, i) {
      return { tight: tight(toks, i) };
    } },
    ordered_list: { block: "ordered_list", getAttrs: function (tok, toks, i) {
      return { order: +tok.attrGet("start") || 1, tight: tight(toks, i) };
    } },
    heading: { block: "heading", getAttrs: function (tok) { return { level: +tok.tag.slice(1) }; } },
    code_block: { block: "code_block", noCloseToken: true },
    fence: { block: "code_block", noCloseToken: true, getAttrs: function (tok) {
      var info = (tok.info || "").trim().split(/\s+/)[0];
      return { language: info || "plaintext" };
    } },
    hr: { node: "horizontal_rule" },
    image: { node: "image", getAttrs: function (tok) {
      return { src: tok.attrGet("src"), title: tok.attrGet("title") || "",
               alt: (tok.children[0] && tok.children[0].content) || "" };
    } },
    hardbreak: { node: "hard_break" },
    em: { mark: "em" },
    strong: { mark: "strong" },
    s: { mark: "strikethrough" },
    link: { mark: "link", getAttrs: function (tok) {
      return { href: tok.attrGet("href"), title: tok.attrGet("title") || "" };
    } },
    code_inline: { mark: "code", noCloseToken: true },
    task_list: { block: "task_list", getAttrs: function (tok, toks, i) {
      return { tight: tight(toks, i) };
    } },
    task_item: { block: "task_item", getAttrs: function (tok) {
      return { checked: tok.attrGet("checked") === "true" };
    } },
    table: { block: "table" },
    thead: { ignore: true },
    tbody: { ignore: true },
    tr: { block: "table_row" },
    th: { block: "table_header" },
    td: { block: "table_cell" }
  });

  // A list is tight when its paragraphs are hidden (prosemirror-markdown's
  // listIsTight); tight lists serialize without blank lines between items.
  function tight(toks, i) {
    while (++i < toks.length) {
      if (!/_item_open$/.test(toks[i].type)) { return toks[i].hidden; }
    }
    return false;
  }

  var dm = M.defaultMarkdownSerializer;
  var serializer = new M.MarkdownSerializer({
    doc: function (s, n) { s.renderContent(n); },
    paragraph: function (s, n) { s.renderInline(n); s.closeBlock(n); },
    blockquote: function (s, n) { s.wrapBlock("> ", null, n, function () { s.renderContent(n); }); },
    horizontal_rule: function (s, n) { s.write("---"); s.closeBlock(n); },
    heading: function (s, n) {
      s.write(new Array(n.attrs.level + 1).join("#") + " ");
      s.renderInline(n, false);
      s.closeBlock(n);
    },
    code_block: function (s, n) {
      var runs = n.textContent.match(/`{3,}/gm);
      var fence = runs ? runs.sort().slice(-1)[0] + "`" : "```";
      var lang = n.attrs.language === "plaintext" ? "" : n.attrs.language;
      s.write(fence + lang + "\n");
      s.text(n.textContent, false);
      s.write("\n");
      s.write(fence);
      s.closeBlock(n);
    },
    bullet_list: function (s, n) { s.renderList(n, "  ", function () { return "- "; }); },
    ordered_list: function (s, n) {
      var start = n.attrs.order || 1;
      var width = String(start + n.childCount - 1).length;
      var space = new Array(width + 3).join(" ");
      s.renderList(n, space, function (i) {
        var nStr = String(start + i);
        return new Array(width - nStr.length + 1).join(" ") + nStr + ". ";
      });
    },
    list_item: function (s, n) { s.renderContent(n); },
    task_list: function (s, n) { s.renderList(n, "  ", function () { return "- "; }); },
    task_item: function (s, n) {
      s.write(n.attrs.checked ? "[x] " : "[ ] ");
      s.renderContent(n);
    },
    image: dm.nodes.image,
    hard_break: dm.nodes.hard_break,
    text: dm.nodes.text,
    table: function (s, n) {
      n.forEach(function (row, _, i) {
        var cells = [];
        row.forEach(function (cell) { cells.push(cellMarkdown(cell)); });
        s.write("| " + cells.join(" | ") + " |\n");
        if (i === 0) {
          s.write("|" + cells.map(function () { return " --- "; }).join("|") + "|\n");
        }
      });
      s.closeBlock(n);
    },
    table_row: function () {},
    table_cell: function () {},
    table_header: function () {}
  }, {
    em: dm.marks.em,
    strong: dm.marks.strong,
    link: dm.marks.link,
    code: dm.marks.code,
    strikethrough: { open: "~~", close: "~~", mixable: true, expelEnclosingWhitespace: true }
  });

  // A cell's inline content as Markdown on one line (sigil wrote the
  // plain text; this keeps bold, links, code ...).
  function cellMarkdown(cell) {
    if (!cell.childCount) { return ""; }
    var doc = schema.node("doc", null, [schema.node("paragraph", null, cell.content)]);
    return serializer.serialize(doc).trim().replace(/\\\n/g, " ").replace(/\n/g, " ")
      .replace(/\|/g, "\\|");
  }

  return {
    P: P, schema: schema, parser: parser, serializer: serializer,
    parse: function (text) {
      var doc = text ? parser.parse(text) : null;
      return doc && doc.childCount ? doc
        : schema.node("doc", null, [schema.node("paragraph")]);
    },
    serialize: function (doc) { return serializer.serialize(doc); }
  };
}

// markdown-it core rule (sigil's install-task-list-rule!): a bullet list
// whose direct items all start with "[ ]" / "[x]" becomes a task list.
var RE_TASK = /^\[([ xX])\]\s?/;
function taskListRule(st) {
  var toks = st.tokens;
  for (var i = 0; i < toks.length; i++) {
    if (toks[i].type !== "bullet_list_open") { continue; }
    var depth = 1, close = -1, items = [];
    for (var j = i + 1; j < toks.length && close < 0; j++) {
      var t = toks[j].type;
      if (/_list_open$/.test(t)) { depth++; }
      else if (/_list_close$/.test(t)) { depth--; if (depth === 0) { close = j; } }
      else if (t === "list_item_open" && depth === 1) { items.push(j); }
    }
    if (close < 0 || !items.length) { continue; }
    var inline = items.map(function (k) {
      for (var m = k + 1; m < close; m++) {
        if (toks[m].type === "inline") { return m; }
        if (toks[m].type === "list_item_close") { return -1; }
      }
      return -1;
    });
    if (!inline.every(function (m) { return m >= 0 && RE_TASK.test(toks[m].content); })) { continue; }
    toks[i].type = "task_list_open";
    toks[close].type = "task_list_close";
    items.forEach(function (k, n) {
      toks[k].type = "task_item_open";
      var d = 0;
      for (var m = k + 1; m < close; m++) {
        if (toks[m].type === "list_item_open") { d++; }
        if (toks[m].type === "list_item_close") {
          if (d === 0) { toks[m].type = "task_item_close"; break; }
          d--;
        }
      }
      var inl = toks[inline[n]], match = RE_TASK.exec(inl.content);
      toks[k].attrSet("checked", String(match[1] !== " "));
      inl.content = inl.content.slice(match[0].length);
      var first = inl.children && inl.children[0];
      if (first && first.type === "text") { first.content = first.content.replace(RE_TASK, ""); }
    });
    i = close;
  }
}

// ==================================================================
// Commands and keymaps (prose_editor/core.cljs, plugins/list_keys.cljs,
// plugins/table_keys.cljs, plugins/input_rules.cljs)
// ==================================================================

function node(K, n) { return K.schema.nodes[n]; }
function mark(K, n) { return K.schema.marks[n]; }

// Arrow keys leave a code block at its first / last line.
function codeExit(K, dir) {
  var P = K.P, Selection = P.state.Selection;
  return function (state, dispatch, view) {
    var sel = state.selection, $head = sel.$head;
    if (!sel.empty || !$head.parent.type.spec.code || !view ||
        !view.endOfTextblock(dir < 0 ? "up" : "down")) { return false; }
    var pos = dir < 0 ? $head.before() : $head.after();
    var $pos = state.doc.resolve(pos);
    if (dir < 0 ? $pos.nodeBefore : $pos.nodeAfter) {
      if (dispatch) {
        dispatch(state.tr.setSelection(Selection.near($pos, dir)).scrollIntoView());
      }
      return true;
    }
    if (dir > 0) { return P.commands.exitCode(state, dispatch); }
    if (dispatch) {
      var tr = state.tr.insert(pos, node(K, "paragraph").createAndFill());
      dispatch(tr.setSelection(Selection.near(tr.doc.resolve(pos), 1)).scrollIntoView());
    }
    return true;
  };
}

// Alt-ArrowUp / Alt-ArrowDown: move the top-level block holding the
// selection (the keyboard counterpart of dragging the handle).
function moveBlockKey(dir) {
  return function (state, dispatch) {
    var $from = state.selection.$from;
    if ($from.depth < 1) { return false; }
    var idx = $from.index(0), doc = state.doc;
    var target = idx + dir;
    if (target < 0 || target >= doc.childCount) { return false; }
    if (dispatch) {
      var from = $from.before(1), block = doc.child(idx);
      var other = doc.child(target);
      var tr = state.tr;
      var offset = $from.pos - from;
      if (dir < 0) {
        var to = from - other.nodeSize;
        tr.delete(from, from + block.nodeSize).insert(to, block);
        tr.setSelection(state.selection instanceof K0.TextSelection
          ? K0.TextSelection.create(tr.doc, to + offset) : K0.Selection.near(tr.doc.resolve(to + 1)));
      } else {
        var dest = from + other.nodeSize;
        tr.delete(from, from + block.nodeSize).insert(dest, block);
        tr.setSelection(state.selection instanceof K0.TextSelection
          ? K0.TextSelection.create(tr.doc, dest + offset) : K0.Selection.near(tr.doc.resolve(dest + 1)));
      }
      dispatch(tr.scrollIntoView());
    }
    return true;
  };
}
var K0 = null;                 // prosemirror-state, for moveBlockKey

function linkPrompt(K, labels) {
  return function (state, dispatch, view) {
    var type = mark(K, "link");
    if (state.selection.empty && !markActive(state, type)) { return false; }
    if (markActive(state, type)) { return K.P.commands.toggleMark(type)(state, dispatch, view); }
    var href = window.prompt(labels.enter_url || "Enter URL:");
    if (!href) { return true; }
    return K.P.commands.toggleMark(type, { href: href })(state, dispatch, view);
  };
}

function markActive(state, type) {
  var s = state.selection;
  return s.empty ? !!type.isInSet(state.storedMarks || s.$from.marks())
    : state.doc.rangeHasMark(s.from, s.to, type);
}

function coreKeymap(K, labels) {
  var C = K.P.commands, H = K.P.history, IR = K.P.inputrules;
  var hardBreak = function (state, dispatch) {
    if (state.selection.$from.parent.type.spec.code) { return false; }
    if (dispatch) {
      dispatch(state.tr.replaceSelectionWith(node(K, "hard_break").create()).scrollIntoView());
    }
    return true;
  };
  return {
    "Backspace": C.chainCommands(IR.undoInputRule, C.deleteSelection, C.joinBackward,
                                 C.selectNodeBackward),
    "Mod-z": H.undo, "Mod-y": H.redo, "Mod-Shift-z": H.redo,
    "Mod-b": C.toggleMark(mark(K, "strong")),
    "Mod-i": C.toggleMark(mark(K, "em")),
    "Mod-`": C.toggleMark(mark(K, "code")),
    "Mod-Shift-x": C.toggleMark(mark(K, "strikethrough")),
    "Mod-k": linkPrompt(K, labels),
    "Shift-Enter": hardBreak,
    "Enter": C.chainCommands(C.newlineInCode, C.createParagraphNear, C.liftEmptyBlock,
                             C.splitBlock),
    "Delete": C.chainCommands(C.deleteSelection, C.joinForward, C.selectNodeForward),
    "ArrowDown": codeExit(K, 1),
    "ArrowUp": codeExit(K, -1),
    "Alt-ArrowUp": moveBlockKey(-1),
    "Alt-ArrowDown": moveBlockKey(1)
  };
}

// plugins/list_keys.cljs
var LIST_TYPES = { bullet_list: 1, ordered_list: 1, task_list: 1 };

function listPlugins(K) {
  var P = K.P, SL = P.schemaList, C = P.commands, Selection = P.state.Selection;
  var li = node(K, "list_item"), ti = node(K, "task_item");
  function emptyItem(state) {
    var sel = state.selection, $f = sel.$from;
    return sel.empty && $f.parent.childCount === 0 &&
      ($f.depth <= 2 || $f.node($f.depth - 1).childCount === 1);
  }
  function atItemStart(state) {
    var sel = state.selection, $f = sel.$from;
    return sel.empty && $f.parentOffset === 0 && $f.depth >= 2 && $f.index($f.depth - 1) === 0;
  }
  function liftEmpty(type) {
    return function (state, dispatch) { return emptyItem(state) && SL.liftListItem(type)(state, dispatch); };
  }
  function liftAtStart(type) {
    return function (state, dispatch) { return atItemStart(state) && SL.liftListItem(type)(state, dispatch); };
  }
  function unwrapSingle(state, dispatch) {
    var sel = state.selection, $f = sel.$from, d = $f.depth;
    if (!sel.empty || $f.parent.childCount !== 0 || d < 2) { return false; }
    for (var k = d - 1; k >= 1; k--) {
      var n = $f.node(k);
      if (LIST_TYPES[n.type.name]) {
        if (n.childCount !== 1) { return false; }
        var start = $f.before(k), end = $f.after(k);
        if (dispatch) {
          var tr = state.tr.replaceWith(start, end, node(K, "paragraph").create());
          dispatch(tr.setSelection(Selection.near(tr.doc.resolve(start + 1))).scrollIntoView());
        }
        return true;
      }
    }
    return false;
  }
  // An empty paragraph right after a list: Backspace removes it and
  // puts the cursor at the end of the list (instead of joining it back).
  function deleteParaAfterList(state, dispatch) {
    var sel = state.selection, $f = sel.$from;
    if (!sel.empty || $f.parentOffset !== 0 || $f.depth !== 1 || $f.parent.childCount !== 0 ||
        $f.index(0) === 0) { return false; }
    var prev = state.doc.child($f.index(0) - 1);
    if (!LIST_TYPES[prev.type.name]) { return false; }
    if (dispatch) {
      var from = $f.before(1), tr = state.tr.delete(from, $f.after(1));
      dispatch(tr.setSelection(Selection.near(tr.doc.resolve(Math.max(0, from - 1)), -1))
                 .scrollIntoView());
    }
    return true;
  }
  return [
    new P.state.Plugin({ props: { handleKeyDown: function (view, e) {
      if (e.key === "Backspace" && !e.shiftKey && !e.ctrlKey && !e.metaKey && !e.altKey &&
          deleteParaAfterList(view.state, view.dispatch)) {
        e.preventDefault();
        return true;
      }
      return false;
    } } }),
    P.keymap.keymap({
      "Enter": C.chainCommands(liftEmpty(ti), liftEmpty(li), unwrapSingle,
                               SL.splitListItem(li), SL.splitListItem(ti)),
      "Tab": C.chainCommands(SL.sinkListItem(li), SL.sinkListItem(ti)),
      "Shift-Tab": C.chainCommands(SL.liftListItem(li), SL.liftListItem(ti)),
      "Backspace": C.chainCommands(liftEmpty(li), liftEmpty(ti), liftAtStart(li),
                                   liftAtStart(ti), unwrapSingle)
    })
  ];
}

// plugins/table_keys.cljs: Tab / Shift-Tab between cells (Tab in the
// last cell adds a row), Enter a line break inside a cell.
function tablePlugins(K) {
  var T = K.P.tables;
  function nextCell(dir) {
    return function (state, dispatch, view) {
      if (!T.isInTable(state)) { return false; }
      if (T.goToNextCell(dir)(state, dispatch)) { return true; }
      if (dir < 0 || !dispatch || !view) { return dir > 0; }
      T.addRowAfter(state, dispatch);
      T.goToNextCell(1)(view.state, view.dispatch);
      return true;
    };
  }
  return [
    K.P.keymap.keymap({
      "Tab": nextCell(1),
      "Shift-Tab": nextCell(-1),
      "Enter": function (state, dispatch) {
        if (!T.isInTable(state)) { return false; }
        if (dispatch) { dispatch(state.tr.replaceSelectionWith(node(K, "hard_break").create())); }
        return true;
      }
    }),
    T.tableEditing()
  ];
}

// plugins/input_rules.cljs (without the maths rules), with a link rule
// and marks that keep the marks around them.
function inputRulesPlugin(K) {
  var IR = K.P.inputrules, T = K.P.transform;
  var TextSelection = K.P.state.TextSelection;
  function markRule(re, type) {
    // re: group 1 the whole marked text with its delimiters, group 2 the text
    return new IR.InputRule(re, function (state, m, start, end) {
      var text = m[2];
      if (!text) { return null; }
      var from = start + m[0].length - m[1].length;
      var $from = state.doc.resolve(from);
      if ($from.parent.type.spec.code) { return null; }
      var marks = type.create().addToSet($from.marks());
      return state.tr.replaceWith(from, end, K.schema.text(text, marks))
        .removeStoredMark(type);
    });
  }
  function taskRule(re, checked) {
    return new IR.InputRule(re, function (state, m, start, end) {
      var tr = state.tr.delete(start, end);
      var range = tr.doc.resolve(start).blockRange();
      var wrap = range && T.findWrapping(range, node(K, "task_list"));
      if (!wrap) { return null; }
      wrap[wrap.length - 1] = { type: node(K, "task_item"), attrs: { checked: checked } };
      return tr.wrap(range, wrap);
    });
  }
  function replaceText(re, text, keep) {
    return new IR.InputRule(re, function (state, m, start, end) {
      return state.tr.insertText(text, start + (keep ? m[0].length - keep : 0), end);
    });
  }
  return IR.inputRules({ rules: [
    IR.textblockTypeInputRule(/^(#{1,6})\s$/, node(K, "heading"),
                              function (m) { return { level: m[1].length }; }),
    IR.wrappingInputRule(/^\s*>\s$/, node(K, "blockquote")),
    IR.wrappingInputRule(/^\s*[-*+]\s$/, node(K, "bullet_list")),
    IR.wrappingInputRule(/^\s*(\d+)\.\s$/, node(K, "ordered_list"),
                         function (m) { return { order: +m[1] }; },
                         function (m, n) { return n.childCount + n.attrs.order === +m[1]; }),
    IR.textblockTypeInputRule(/^```(\w*)\s$/, node(K, "code_block"),
                              function (m) { return { language: m[1] || "plaintext" }; }),
    new IR.InputRule(/^(---|___|\*\*\*)\s$/, function (state, m, start) {
      var $s = state.doc.resolve(start);
      if ($s.parent.type.name !== "paragraph" || $s.parentOffset !== 0) { return null; }
      var from = $s.before(), to = $s.after();
      if ($s.parent.textContent.length !== m[0].length - 1) { return null; }
      var tr = state.tr.replaceWith(from, to, [node(K, "horizontal_rule").create(),
                                                node(K, "paragraph").create()]);
      return tr.setSelection(TextSelection.create(tr.doc, from + 2));
    }),
    taskRule(/^\s*\[\s?\]\s$/, false),
    taskRule(/^\s*\[[xX]\]\s$/, true),
    markRule(/(\*\*([^\s*](?:[^*]*[^\s*])?)\*\*)$/, mark(K, "strong")),
    markRule(/(?:^|[^*])(\*([^\s*](?:[^*]*[^\s*])?)\*)$/, mark(K, "em")),
    markRule(/(?:^|[^_\w])(_([^\s_](?:[^_]*[^\s_])?)_)$/, mark(K, "em")),
    markRule(/(`([^`]+)`)$/, mark(K, "code")),
    markRule(/(~~([^\s~](?:[^~]*[^\s~])?)~~)$/, mark(K, "strikethrough")),
    new IR.InputRule(/\[([^\]]+)\]\(([^)\s]+)(?:\s+"([^"]*)")?\)$/, function (state, m, start, end) {
      var $s = state.doc.resolve(start);
      var link = mark(K, "link").create({ href: m[2], title: m[3] || "" });
      return state.tr.replaceWith(start, end, K.schema.text(m[1], link.addToSet($s.marks())));
    }),
    // typography (sigil): -- after a word, ..., smart double quotes
    replaceText(/[^\s-]--$/, "\u2014", 2),
    replaceText(/\.\.\.$/, "\u2026"),
    replaceText(/(?:^|[\s({\[])"$/, "\u201C", 1),
    replaceText(/[^\s({\[]"$/, "\u201D", 1)
  ] });
}

// plugins/task_list.cljs: a clickable checkbox for task items.
function taskItemView(n, view, getPos) {
  var li = document.createElement("li"), cb = document.createElement("input");
  var content = document.createElement("span");
  li.className = "task-list-item";
  cb.type = "checkbox";
  cb.className = "task-list-item-checkbox";
  cb.checked = !!n.attrs.checked;
  cb.contentEditable = "false";
  cb.disabled = !view.editable;
  content.className = "task-list-item-content";
  li.appendChild(cb);
  li.appendChild(content);
  function onChange(e) {
    e.stopPropagation();
    var pos = getPos();
    if (typeof pos === "number") {
      view.dispatch(view.state.tr.setNodeMarkup(pos, null, { checked: cb.checked }));
    }
  }
  function onDown(e) { e.stopPropagation(); }
  cb.addEventListener("change", onChange);
  cb.addEventListener("mousedown", onDown);
  return {
    dom: li, contentDOM: content,
    update: function (m) {
      if (m.type !== n.type) { return false; }
      cb.checked = !!m.attrs.checked;
      return true;
    },
    stopEvent: function (e) { return e.target === cb; },
    ignoreMutation: function (mu) { return mu.target === cb; },
    destroy: function () {
      cb.removeEventListener("change", onChange);
      cb.removeEventListener("mousedown", onDown);
    }
  };
}

// plugins/clipboard.cljs: pasted or dropped images become data URLs;
// plain text that looks like Markdown is parsed (HTML is left to
// ProseMirror's own clipboard parser).
var LOOKS_MD = /^(#{1,6}\s|[-*+]\s|\d+\.\s|>\s|```|---|\*\*|__|~~|\[.+\]\(.+\))/m;

function clipboardPlugin(K) {
  var Slice = K.P.model.Slice;
  function images(dt) {
    return Array.prototype.filter.call((dt && dt.files) || [], function (f) {
      return /^image\//.test(f.type);
    });
  }
  function insertImages(view, files, pos) {
    files.forEach(function (f) {
      var r = new FileReader();
      r.onload = function () {
        var at = pos == null ? view.state.selection.from : pos;
        view.dispatch(view.state.tr.insert(at, node(K, "image").create({ src: r.result })));
      };
      r.readAsDataURL(f);
    });
  }
  return new K.P.state.Plugin({ props: {
    handlePaste: function (view, e) {
      var dt = e.clipboardData, files = images(dt);
      if (files.length) { insertImages(view, files); return true; }
      if (!dt || dt.getData("text/html")) { return false; }
      var text = dt.getData("text/plain");
      if (!text || view.state.selection.$from.parent.type.spec.code || !LOOKS_MD.test(text)) {
        return false;
      }
      var doc = K.parse(text);
      view.dispatch(view.state.tr.replaceSelection(new Slice(doc.content, 0, 0)).scrollIntoView());
      return true;
    },
    handleDrop: function (view, e) {
      var files = images(e.dataTransfer);
      if (!files.length) { return false; }
      e.preventDefault();
      var at = view.posAtCoords({ left: e.clientX, top: e.clientY });
      insertImages(view, files, at ? at.pos : null);
      return true;
    }
  } });
}

// plugins/placeholder.cljs: the text of an empty document.
function placeholderPlugin(K, text) {
  var V = K.P.view;
  return new K.P.state.Plugin({ props: { decorations: function (state) {
    var doc = state.doc, first = doc.firstChild;
    if (doc.childCount !== 1 || !first.isTextblock || first.type.name !== "paragraph" ||
        first.childCount) { return null; }
    return V.DecorationSet.create(doc, [V.Decoration.widget(1, function () {
      var s = document.createElement("span");
      s.className = "ah-pm-placeholder";
      s.setAttribute("contenteditable", "false");
      s.setAttribute("aria-hidden", "true");
      s.textContent = text;
      return s;
    }, { key: "placeholder", side: -1 })]);
  } } });
}

// ==================================================================
// Slash menu (plugins/slash_menu.cljs + slash_menu/{view,commands}.cljs)
// ==================================================================

// The block an item stands for.
function blockFor(K, type, level) {
  var p = function () { return node(K, "paragraph").create(); };
  switch (type) {
    case "heading": return node(K, "heading").create({ level: level || 1 });
    case "code_block": return node(K, "code_block").create({ language: "plaintext" });
    case "blockquote": return node(K, "blockquote").create(null, [p()]);
    case "bullet_list":
    case "ordered_list":
      return node(K, type).create(null, [node(K, "list_item").create(null, [p()])]);
    case "task_list":
      return node(K, "task_list").create(null, [node(K, "task_item").create(null, [p()])]);
    case "table":
      var row = function (t) {
        return node(K, "table_row").create(null, [0, 1, 2].map(function () { return node(K, t).create(); }));
      };
      return node(K, "table").create(null, [row("table_header"), row("table_cell")]);
    case "horizontal_rule": return node(K, "horizontal_rule").create();
    default: return p();
  }
}

// Replace the (empty or "/") top-level block at `pos' with the item's
// block and put the cursor inside it.
function insertBlock(st, type, level, pos) {
  var K = st.K, view = st.view, P = K.P;
  var $pos = view.state.doc.resolve(Math.min(pos, view.state.doc.content.size));
  if ($pos.depth < 1) { return; }
  var from = $pos.before(1), to = $pos.after(1);
  var block;
  if (type === "image") {
    var src = window.prompt(st.labels.enter_image_url || "Image URL:");
    view.focus();
    if (!src) { return; }
    block = node(K, "paragraph").create(null, [node(K, "image").create({ src: src })]);
  } else {
    block = blockFor(K, type, level);
  }
  var tr = view.state.tr.replaceWith(from, to, block);
  if (type === "horizontal_rule") {
    tr.insert(from + block.nodeSize, node(K, "paragraph").create());
  }
  var target = type === "horizontal_rule" ? from + block.nodeSize + 1 : from + 1;
  var $t = tr.doc.resolve(target);
  tr.setSelection($t.parent.inlineContent ? P.state.TextSelection.create(tr.doc, target)
                  : P.state.Selection.near($t));
  view.dispatch(tr.scrollIntoView());
  view.focus();
}

function slashPlugin(st) {
  var K = st.K, el = st.el;
  var menu = el.querySelector(".ah-pm-slash-menu");
  if (!menu) { return null; }
  var caret = el.querySelector(".ah-md-editor-caret");
  var list = menu.querySelector(".ah-pm-slash-menu-content");
  var m = st.menu = { open: false, pos: null, float: null };

  function items() { return Array.from(menu.querySelectorAll(".ah-pm-slash-menu-item")); }
  function selected() {
    return Math.max(0, items().findIndex(function (it) { return it.classList.contains("selected"); }));
  }
  function select(i) {
    var all = items(), n = all.length;
    i = (i + n) % n;
    all.forEach(function (x) {
      x.classList.remove("selected");
      x.setAttribute("aria-selected", "false");
    });
    var it = all[i];
    it.classList.add("selected");
    it.setAttribute("aria-selected", "true");
    // scroll the list, not the page
    var top = it.offsetTop - list.offsetTop;
    if (top < list.scrollTop) { list.scrollTop = top; }
    else if (top + it.offsetHeight > list.scrollTop + list.clientHeight) {
      list.scrollTop = top + it.offsetHeight - list.clientHeight;
    }
    var g = it.closest(".ah-pm-slash-menu-group");
    var group = g ? g.getAttribute("data-group") : null;
    menu.querySelectorAll(".ah-pm-slash-menu-tab").forEach(function (tab) {
      tab.classList.toggle("active", tab.getAttribute("data-group") === group);
    });
    if (st.view) { st.view.dom.setAttribute("aria-activedescendant", it.id); }
  }
  function show(pos) {
    var view = st.view, c;
    try { c = view.coordsAtPos(pos); } catch (e) { return; }
    var content = caret.offsetParent || caret.parentNode;
    var r = content.getBoundingClientRect();
    caret.style.left = (c.left - r.left + content.scrollLeft) + "px";
    caret.style.top = (c.top - r.top + content.scrollTop) + "px";
    caret.style.height = Math.max(1, c.bottom - c.top) + "px";
    m.open = true;
    m.pos = pos;
    menu.classList.add("ah-pm-slash-menu--visible");
    list.scrollTop = 0;
    select(0);
    if (m.float) { m.float.stop(); }
    m.float = AH.float(menu, caret, { placement: "bottom", align: "start", offset: 4 });
    view.dom.setAttribute("aria-expanded", "true");
  }
  function hide() {
    if (!m.open) { return; }
    m.open = false;
    m.pos = null;
    menu.classList.remove("ah-pm-slash-menu--visible");
    if (m.float) { m.float.stop(); m.float = null; }
    if (st.view) {
      st.view.dom.setAttribute("aria-expanded", "false");
      st.view.dom.removeAttribute("aria-activedescendant");
    }
  }
  function confirm(it) {
    if (!it || m.pos == null) { return; }
    var pos = m.pos;
    hide();
    insertBlock(st, it.getAttribute("data-type"), +it.getAttribute("data-level") || null, pos);
  }
  m.show = show;
  m.hide = hide;

  return new K.P.state.Plugin({
    view: function () {
      var off = new AbortController();
      menu.addEventListener("mousedown", function (e) {
        e.preventDefault();
        var tab = e.target.closest(".ah-pm-slash-menu-tab");
        if (tab) {
          var g = Array.prototype.filter.call(menu.querySelectorAll(".ah-pm-slash-menu-group"), function (x) {
            return x.getAttribute("data-group") === tab.getAttribute("data-group");
          })[0];
          if (g) {
            list.scrollTop = g.offsetTop - list.offsetTop;
            select(items().indexOf(g.querySelector(".ah-pm-slash-menu-item")));
          }
          return;
        }
        confirm(e.target.closest(".ah-pm-slash-menu-item"));
      }, { signal: off.signal });
      document.addEventListener("mousedown", function (e) {
        if (m.open && !menu.contains(e.target)) { hide(); }
      }, { signal: off.signal });
      return {
        // "/" typed into an empty top-level paragraph opens the menu.
        // Read from the document rather than handleTextInput, which
        // ProseMirror skips when the browser rewrote the empty block.
        update: function (view, prev) {
          if (m.open || !prev || prev.doc.eq(view.state.doc)) { return; }
          var sel = view.state.selection, $f = sel.$from;
          if (!sel.empty || $f.depth !== 1 || $f.parent.type.name !== "paragraph" ||
              $f.parent.textContent !== "/" || $f.parentOffset !== 1) { return; }
          var i = $f.index(0), was = prev.doc.childCount === view.state.doc.childCount &&
              prev.doc.child(i);
          if (was && was.type.name === "paragraph" && was.childCount === 0) { show(sel.from); }
        },
        destroy: function () {
          hide();
          off.abort();
        }
      };
    },
    props: {
      handleKeyDown: function (view, e) {
        if (!m.open) { return false; }
        switch (e.key) {
          case "ArrowDown": select(selected() + 1); e.preventDefault(); return true;
          case "ArrowUp": select(selected() - 1); e.preventDefault(); return true;
          case "Enter": confirm(items()[selected()]); e.preventDefault(); return true;
          case "Escape": hide(); e.preventDefault(); return true;
          case "Shift": case "Control": case "Alt": case "Meta": return false;
          default: hide(); return false;
        }
      }
    }
  });
}

// ==================================================================
// Block handle and drag (plugins/handle.cljs, plugins/drag.cljs)
// ==================================================================

// The top-level block (a child of view.dom) holding a DOM node.
function topBlock(view, dom) {
  while (dom && dom.parentNode !== view.dom) {
    if (dom === view.dom || !view.dom.contains(dom)) { return null; }
    dom = dom.parentNode;
  }
  return dom && dom.nodeType === 1 ? dom : null;
}

function posOf(view, dom) {
  try { return view.posAtDOM(dom, 0); } catch (e) { return null; }
}

function handlePlugin(st) {
  var el = st.el, handle = el.querySelector(".ah-pm-block-handle");
  if (!handle) { return null; }
  var timer = null, block = null;
  var content = handle.parentNode;

  function cancel() { clearTimeout(timer); timer = null; }
  function show(dom) {
    cancel();
    var r = dom.getBoundingClientRect(), c = content.getBoundingClientRect();
    var lh = parseFloat(getComputedStyle(dom).lineHeight) || 24;
    var top = r.top - c.top + content.scrollTop + (Math.min(lh, r.height) - handle.offsetHeight) / 2;
    handle.style.top = Math.round(top) + "px";
    handle.classList.add("ah-pm-block-handle--visible");
    block = dom;
    st.handleBlock = dom;
  }
  function hideNow() {
    handle.classList.remove("ah-pm-block-handle--visible");
    block = st.handleBlock = null;
  }
  function hideLater() { cancel(); timer = setTimeout(hideNow, 150); }

  return new st.K.P.state.Plugin({
    view: function (view) {
      var off = new AbortController(), opts = { signal: off.signal };
      handle.addEventListener("mouseenter", function () {
        cancel();
        handle.classList.add("ah-pm-block-handle--visible");
      }, opts);
      handle.addEventListener("mouseleave", hideLater, opts);
      handle.addEventListener("mousedown", function (e) {
        if (e.target.closest(".ah-pm-block-handle-add")) { e.preventDefault(); }
      }, opts);
      handle.addEventListener("click", function (e) {
          if (!e.target.closest(".ah-pm-block-handle-add")) { return; }
          e.preventDefault();
          var pos = block && posOf(st.view, block);
          if (pos == null) { return; }
          var v = st.view, after = v.state.doc.resolve(pos).after(1);
          var tr = v.state.tr.insert(after, node(st.K, "paragraph").create());
          tr.setSelection(st.K.P.state.TextSelection.create(tr.doc, after + 1));
          v.dispatch(tr.scrollIntoView());
          v.focus();
          if (st.menu) {
            requestAnimationFrame(function () { if (st.view) { st.menu.show(after + 1); } });
          }
        }, opts);
      return { update: function () {
        if (block && !view.dom.contains(block)) { hideNow(); }
      }, destroy: function () { cancel(); hideNow(); off.abort(); } };
    },
    props: { handleDOMEvents: {
      mousemove: function (view, e) {
        if (st.drag && st.drag.block) { return false; }
        var b = e.target === view.dom ? null : topBlock(view, e.target);
        if (!b) { hideLater(); }
        else if (b !== block) { show(b); }
        return false;
      },
      mouseleave: function () { hideLater(); return false; }
    } }
  });
}

function dragPlugin(st) {
  var el = st.el, indicator = el.querySelector(".ah-pm-drag-indicator");
  if (!indicator) { return null; }
  var content = indicator.parentNode;
  var d = st.drag = { block: null, pos: null, clone: null };

  // The document position a drop at clientY lands on (a boundary between
  // top-level blocks) and the y of that boundary.
  function dropAt(view, y) {
    var kids = view.dom.children, last = null;
    for (var i = 0; i < kids.length; i++) {
      var r = kids[i].getBoundingClientRect();
      var p = posOf(view, kids[i]);
      if (p == null) { continue; }
      if (y < r.top + r.height / 2) {
        return { pos: view.state.doc.resolve(p).before(1), y: r.top };
      }
      last = { pos: view.state.doc.resolve(p).after(1), y: r.bottom };
    }
    return last;
  }
  function removeClone() { if (d.clone) { d.clone.remove(); d.clone = null; } }
  function end() {
    if (d.block) { d.block.classList.remove("ah-pm-dragging"); }
    indicator.style.display = "none";
    removeClone();
    d.block = d.pos = null;
  }

  return new st.K.P.state.Plugin({
    view: function (view) {
      var off = new AbortController(), moving = null;
      el.addEventListener("mousedown", function (e) {
        if (!e.target.closest || !e.target.closest(".ah-pm-block-handle-drag")) { return; }
        var b = st.handleBlock, p = b && posOf(view, b);
        if (p == null || e.button !== 0) { return; }
        e.preventDefault();
        d.block = b;
        d.pos = p;
        b.classList.add("ah-pm-dragging");
        var r = b.getBoundingClientRect(), c = b.cloneNode(true);
        c.classList.add("ah-pm-drag-clone");
        Object.assign(c.style, { position: "fixed", opacity: "0.6", pointerEvents: "none", zIndex: "1000",
                                 width: r.width + "px", transform: "rotate(1deg)", margin: "0",
                                 boxShadow: "0 4px 16px var(--ah-color-bg-overlay)",
                                 left: (e.clientX - 16) + "px", top: (e.clientY - 16) + "px" });
        document.body.appendChild(c);
        d.clone = c;
        if (moving) { moving.abort(); }
        moving = new AbortController();
        var mv = { signal: AbortSignal.any ? AbortSignal.any([moving.signal, off.signal]) : moving.signal };
        document.addEventListener("mousemove", function (ev) {
          var at = dropAt(view, ev.clientY), cr = content.getBoundingClientRect();
          if (at) {
            indicator.style.top = (at.y - cr.top + content.scrollTop - 1) + "px";
            indicator.style.display = "block";
          }
          if (d.clone) {
            d.clone.style.left = (ev.clientX - 16) + "px";
            d.clone.style.top = (ev.clientY - 16) + "px";
          }
        }, mv);
        document.addEventListener("mouseup", function (ev) {
          moving.abort();
          moving = null;
          var from = d.pos, at = dropAt(view, ev.clientY);
          end();
          if (from != null && at) { moveBlock(view, from, at.pos); }
        }, mv);
      }, { signal: off.signal });
      return { destroy: function () {
        end();
        if (moving) { moving.abort(); moving = null; }
        off.abort();
      } };
    }
  });
}

// Move the top-level block at `pos' to the block boundary `to'
// (drag.cljs move-block!).
function moveBlock(view, pos, to) {
  var doc = view.state.doc, $p = doc.resolve(pos);
  if ($p.depth < 1) { return; }
  var from = $p.before(1), end = $p.after(1), blk = $p.node(1);
  if (to >= from && to <= end) { return; }
  var tr = view.state.tr;
  if (to < from) { tr.delete(from, end).insert(to, blk); }
  else { tr.insert(to, blk).delete(from, end); }
  view.dispatch(tr);
}

// ==================================================================
// Stats footer (plugins/stats_footer.cljs, stats.cljs)
// ==================================================================

var CJK = /[\u4e00-\u9fff\u3400-\u4dbf\uf900-\ufaff]/g;

function stats(doc) {
  var text = doc.textBetween(0, doc.content.size, "\n");
  var blank = !/\S/.test(text);
  var paragraphs = 0;
  doc.descendants(function (n) {
    if (n.type.name === "paragraph" || n.type.name === "heading") { paragraphs++; }
  });
  return {
    chars: blank ? 0 : Array.from(text).length,
    words: blank ? 0 : (text.match(CJK) || []).length +
      text.replace(CJK, " ").split(/\s+/).filter(Boolean).length,
    paragraphs: paragraphs
  };
}

function statsPlugin(st) {
  var foot = st.el.querySelector(".ah-pm-stats");
  if (!foot) { return null; }
  var max = +st.el.getAttribute("data-ah-max-chars") || 0;
  var timer = null;
  function fill(doc) {
    var s = stats(doc);
    var put = function (name, text) {
      foot.querySelectorAll('[data-stat="' + name + '"]').forEach(function (n) { n.textContent = String(text); });
    };
    put("chars", s.chars);
    put("words", s.words);
    put("paragraphs", s.paragraphs);
    if (max) {
      var over = s.chars > max;
      put("limit", s.chars + "/" + max);
      foot.querySelectorAll('[data-stat="limit"]').forEach(function (n) {
        n.classList.toggle("ah-pm-stats__value--warning", over);
      });
      foot.querySelectorAll(".ah-pm-stats__warning").forEach(function (n) { n.hidden = !over; });
      st.el.classList.toggle("ah-md-editor-over", over);
    }
  }
  return new st.K.P.state.Plugin({ view: function (view) {
    fill(view.state.doc);
    return {
      update: function (v, prev) {
        if (prev && prev.doc.eq(v.state.doc)) { return; }
        clearTimeout(timer);
        timer = setTimeout(function () { if (st.view) { fill(st.view.state.doc); } }, 100);
      },
      destroy: function () { clearTimeout(timer); }
    };
  } });
}

// ==================================================================
// The component
// ==================================================================

function labelsOf(el) {
  try { return JSON.parse(el.getAttribute("data-ah-labels") || "{}"); } catch (e) { return {}; }
}

// Value contract (designs/04-components.md): data-ah-value, the
// textarea (the form field), `input' now, `change' when committed.
function publish(st, md) {
  st.el.setAttribute("data-ah-value", md);
  if (st.ta && st.ta.value !== md) { st.ta.value = md; }
  fire(st.el, "input");
}

function commit(st) {
  var v = st.el.getAttribute("data-ah-value") || "";
  if (v !== st.committed) {
    st.committed = v;
    fire(st.el, "change");
  }
}

// The document has the focus (the slash menu and the handle keep it
// there; a task checkbox takes it).
function focused(st) {
  return !!st.view && document.activeElement === st.view.dom;
}

function onDocChanged(st) {
  publish(st, st.K.serialize(st.view.state.doc));
  if (!focused(st)) { commit(st); }
}

function setDoc(st, md) {
  var view = st.view, P = st.K.P;
  view.updateState(P.state.EditorState.create({
    schema: st.K.schema, doc: st.K.parse(md), plugins: view.state.plugins
  }));
}

function mountEditor(st, P) {
  if (!KIT) { KIT = buildKit(P); K0 = P.state; }
  var K = st.K = KIT, el = st.el;
  var content = el.querySelector(".ah-pm-content") || el;
  var editable = !el.hasAttribute("data-ah-readonly") && el.getAttribute("aria-disabled") !== "true";
  var place = document.createElement("div");
  content.insertBefore(place, content.firstChild);
  st.place = place;

  var plugins = [];
  if (editable) {
    plugins.push(slashPlugin(st), inputRulesPlugin(K));
    plugins = plugins.concat(listPlugins(K), tablePlugins(K));
    plugins.push(clipboardPlugin(K), placeholderPlugin(K, el.getAttribute("data-ah-placeholder") || ""),
                 handlePlugin(st), dragPlugin(st));
  }
  plugins.push(statsPlugin(st));
  if (editable) {
    plugins.push(P.keymap.keymap(coreKeymap(K, st.labels)), P.keymap.keymap(P.commands.baseKeymap),
                 P.history.history(), P.dropcursor.dropCursor(), P.gapcursor.gapCursor());
  }
  plugins = plugins.filter(Boolean);

  var attrs = { role: "textbox", "aria-multiline": "true",
                "aria-label": st.labels.editor || "Markdown editor" };
  if (!editable) { attrs["aria-readonly"] = "true"; }
  if (el.getAttribute("aria-disabled") === "true") { attrs["aria-disabled"] = "true"; }
  if (st.menu || el.querySelector(".ah-pm-slash-menu")) {
    var list = el.querySelector(".ah-pm-slash-menu-content");
    attrs["aria-haspopup"] = "listbox";
    attrs["aria-expanded"] = "false";
    if (list) { attrs["aria-controls"] = list.id; }
  }
  var ph = el.getAttribute("data-ah-placeholder");
  if (ph && editable) { attrs["aria-placeholder"] = ph; }

  var view = new P.view.EditorView({ mount: place }, {
    state: P.state.EditorState.create({
      schema: K.schema, doc: K.parse(el.getAttribute("data-ah-value") || ""), plugins: plugins
    }),
    editable: function () { return editable; },
    attributes: attrs,
    nodeViews: { task_item: taskItemView },
    dispatchTransaction: function (tr) {
      var v = st.view || view;
      v.updateState(v.state.apply(tr));
      if (tr.docChanged) { onDocChanged(st); }
    }
  });
  st.view = view;
  // ProseMirror's own input/change events are not the component's
  view.dom.addEventListener("input", swallow);
  view.dom.addEventListener("change", swallow);
  if (st.ta) { st.ta.hidden = true; }
  el.classList.add("ah-md-editor-ready");
  return view;
}

AH.register("markdown-editor", class extends AH.Controller {
  setup() {
    var el = this.element;
    var st = this.st = {
      el: el, uid: ++uid, view: null, K: null, labels: labelsOf(el),
      ta: el.querySelector(".ah-md-editor-source"),
      committed: el.getAttribute("data-ah-value") || "", destroyed: false
    };
    // Until the editor is up the textarea is the editor.
    if (st.ta) {
      this.listen(st.ta, "input", function (e) {
        e.stopPropagation();
        if (!st.view) { publish(st, st.ta.value); }
      });
      this.listen(st.ta, "change", function (e) {
        e.stopPropagation();
        if (!st.view) { commit(st); }
      });
    }
    this.listen(el, "focusout", function (e) {
      if (st.view && e.target === st.view.dom) { commit(st); }
    });
    st.ready = AH.vendor("prosemirror").then(function (P) {
      if (st.destroyed) { throw new Error("aihtml: markdown editor destroyed"); }
      return mountEditor(st, P);
    });
    st.ready.catch(function (err) {
      if (!st.destroyed) { console.error("aihtml: markdown editor not loaded", err); }
    });
  }

  teardown() {
    var st = this.st, el = this.element;
    st.destroyed = true;
    if (st.view) {
      st.view.dom.removeEventListener("input", swallow);
      st.view.dom.removeEventListener("change", swallow);
      st.view.destroy();
      st.view = null;
    }
    if (st.place) { st.place.remove(); st.place = null; }
    if (st.ta) { st.ta.hidden = false; }
    el.classList.remove("ah-md-editor-ready", "ah-md-editor-over");
  }

  // methods (aihtml_action:call/4, AH.invoke)
  getValue() { return this.element.getAttribute("data-ah-value") || ""; }
  setValue(md) {
    var st = this.st;
    md = md == null ? "" : String(md);
    this.element.setAttribute("data-ah-value", md);
    st.committed = md;
    if (st.ta) { st.ta.value = md; }
    if (st.view) { setDoc(st, md); }
  }
  getHtml() {
    var st = this.st;
    if (!st.view) { return null; }
    var div = document.createElement("div");
    div.appendChild(st.K.P.model.DOMSerializer.fromSchema(st.K.schema)
                      .serializeFragment(st.view.state.doc.content));
    return div.innerHTML;
  }
  getJson() { return this.st.view ? this.st.view.state.doc.toJSON() : null; }
  focus() {
    var st = this.st;
    if (st.view) { st.view.focus(); } else if (st.ta) { st.ta.focus(); }
  }
  blur() {
    var st = this.st;
    if (st.view) { st.view.dom.blur(); } else if (st.ta) { st.ta.blur(); }
    if (st.view && !focused(st)) { commit(st); }
  }
  exec(cmd, opts) {
    var st = this.st;
    if (!st.view) { st.ready.then(function () { exec(st, cmd, opts || {}); }); return false; }
    return exec(st, cmd, opts || {});
  }
  stats() { return this.st.view ? stats(this.st.view.state.doc) : null; }
  ready() { return this.st.ready; }
  view() { return this.st.view; }
});

// exec-command! (core.cljs): marks, blocks, undo / redo.
function exec(st, cmd, opts) {
  var K = st.K, view = st.view, P = K.P, C = P.commands;
  var MARKS = { bold: "strong", italic: "em", code: "code", strikethrough: "strikethrough" };
  var run = function (c) { return c(view.state, view.dispatch, view); };
  if (MARKS[cmd]) { return run(C.toggleMark(mark(K, MARKS[cmd]))); }
  switch (cmd) {
    case "link":
      if (!opts.href) { return run(linkPrompt(K, st.labels)); }
      return run(C.toggleMark(mark(K, "link"), { href: opts.href, title: opts.title || "" }));
    case "undo": return run(P.history.undo);
    case "redo": return run(P.history.redo);
    case "heading": return run(C.setBlockType(node(K, "heading"), { level: +opts.level || 1 }));
    case "paragraph": return run(C.setBlockType(node(K, "paragraph")));
    case "code_block":
      return run(C.setBlockType(node(K, "code_block"), { language: opts.language || "plaintext" }));
    case "blockquote": return run(C.wrapIn(node(K, "blockquote")));
    case "bullet_list":
    case "ordered_list":
    case "task_list":
      return run(P.schemaList.wrapInList(node(K, cmd)));
    case "horizontal_rule":
      return run(function (state, dispatch) {
        if (dispatch) { dispatch(state.tr.replaceSelectionWith(node(K, "horizontal_rule").create())); }
        return true;
      });
    default:
      console.error("aihtml: unknown markdown editor command " + cmd);
      return false;
  }
}
