// Reference renderer for aihtml_lib_markdown's tests: renders every case
// of a Markdown fixtures file with markdown-it, configured as the
// markdown_editor configures it, and prints the HTML as a JSON array.
//
//   node scripts/render-markdown.mjs apps/aihtml/test/aihtml_lib_markdown.fixtures.txt
//
// The fixtures file: each case starts with a line "%%%% <name>"; the case
// is every line after it up to the next such line, each ending in a
// newline (a header ending in " [nonl]" drops the last one). Text before
// the first header is a comment.
//
// markdown-it as in assets/js/components/markdown_editor.js: the "default"
// preset (GFM tables and strikethrough, no typographer), html off, linkify
// off, plus the editor's task list rule (copied below; keep it in step).
// Two things are the view's own, and the Erlang renderer does the same:
//   - validateLink: only http(s), mailto and relative URLs (markdown-it
//     only refuses javascript:, vbscript:, file: and most data: URLs);
//   - task lists render as <ul class="contains-task-list"> with
//     <li class="task-list-item"> and a disabled checkbox before the text
//     (the editor draws them with ProseMirror, markdown-it has no rendering
//     for them).
// One normalisation: markdown-it escapes & < > " and the Erlang side
// escapes ' as well (&#39;, beamai_html_escape, like all aihtml output);
// markdown-it never writes a quote of its own, so every ' in its output is
// text or an attribute value and is escaped here to compare.
import { readFileSync } from "node:fs";
import MarkdownIt from "markdown-it";

const md = new MarkdownIt("default", { html: false });
md.core.ruler.push("task_lists", taskListRule);
md.core.ruler.push("task_checkboxes", taskCheckboxes);
md.validateLink = validateLink;

md.renderer.rules.task_list_open = (toks, i, opts, env, slf) => {
  toks[i].attrs = [["class", "contains-task-list"]];
  return slf.renderToken(toks, i, opts);
};
md.renderer.rules.task_item_open = (toks, i, opts, env, slf) => {
  toks[i].attrs = [["class", "task-list-item"]];
  return slf.renderToken(toks, i, opts);
};

// Only http:, https:, mailto: and URLs without a scheme (the URL is
// normalised: entities decoded, unsafe characters percent-encoded).
function validateLink(url) {
  const m = /^([a-zA-Z][a-zA-Z0-9+.-]*):/.exec(url.trim());
  return !m || ["http", "https", "mailto"].includes(m[1].toLowerCase());
}

// The checkbox of a task item, as the first inline of its first line.
function taskCheckboxes(st) {
  const toks = st.tokens;
  for (let i = 0; i < toks.length; i++) {
    if (toks[i].type !== "task_item_open") { continue; }
    for (let m = i + 1; m < toks.length; m++) {
      if (toks[m].type === "inline") {
        const t = new st.Token("html_inline", "", 0);
        t.content = '<input type="checkbox" class="task-list-item-checkbox" disabled' +
          (toks[i].attrGet("checked") === "true" ? " checked" : "") + ">";
        toks[m].children.unshift(t);
        break;
      }
      if (/_item_close$/.test(toks[m].type)) { break; }
    }
  }
}

// ---- copied from assets/js/components/markdown_editor.js ----
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
// ---- end of copy ----

export function readFixtures(text) {
  const cases = [];
  let cur = null;
  for (const line of text.split("\n")) {
    const h = /^%%%% (.*)$/.exec(line);
    if (h) {
      cur = { name: h[1].replace(/ \[nonl\]$/, ""), nonl: / \[nonl\]$/.test(h[1]), lines: [] };
      cases.push(cur);
    } else if (cur) {
      cur.lines.push(line);
    }
  }
  return cases.map((c) => {
    const src = c.lines.join("\n").replace(/\n$/, "");
    return { name: c.name, src: c.nonl ? src : src + "\n" };
  });
}

const file = process.argv[2];
const cases = readFixtures(readFileSync(file, "utf8"));
const out = cases.map((c) => ({ name: c.name, html: md.render(c.src).replace(/'/g, "&#39;") }));
process.stdout.write(JSON.stringify(out));
